#lang racket/base

;;; City growth, OpenTTD-style: roads creep outward from the existing
;;; network, buildings spring up alongside roads, and established
;;; buildings densify into taller ones over time. One call to
;;; advance-city! is one tick of the simulation.

(require racket/list
         racket/match
         "state.rkt")

(provide advance-city!)

(module+ test
  (require rackunit
           "terrain.rkt"))

(define max-building-level 3)
(define building-variant-count 4)

(define orthogonal-directions '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))

(define (random-pick lst)
  (and (pair? lst) (list-ref lst (random (length lst)))))

;; The city stops sprawling as its footprint approaches this fraction
;; of the land; growth continues vertically through upgrades. Combined
;; with urban churn this caps the city at a living equilibrium that
;; always leaves room for the landscape.
(define city-footprint-target 0.62)

(define (advance-city! st)
  (set-city-state-tick! st (add1 (city-state-tick st)))
  (parameterize ([current-pseudo-random-generator (city-state-rng st)])
    (define building-count (length (tiles-matching st building?)))
    (define road-count (length (tiles-matching st road?)))
    (define land-count
      (- (* (city-state-width st) (city-state-height st))
         (length (tiles-matching st water?))))
    ;; Demand for new land falls to zero as the footprint nears the
    ;; target fraction.
    (define footprint (/ (+ building-count road-count) (exact->inexact land-count)))
    (define demand (max 0.0 (- 1.0 (/ footprint city-footprint-target))))
    ;; Growth speeds up gently as the city matures, capped so a large
    ;; city still changes at a watchable pace.
    (define iterations (min 40 (+ 10 (quotient building-count 8))))
    (for ([_ (in-range iterations)])
      (define roll (random 100))
      (cond
        [(< roll 35) (when (< (random) demand) (try-extend-road! st))]
        [(< roll 75) (when (< (random) demand) (try-spawn-building! st))]
        [else (try-upgrade-building! st)]))
    (advance-nature! st)
    (renew-city! st)))

;; Urban churn, so the city saturates into a living equilibrium
;; instead of paving every last tile: fringe buildings occasionally
;; get demolished and roads serving nothing get reclaimed.
(define (renew-city! st)
  (define building-pos (random-pick (tiles-matching st building?)))
  (when building-pos
    (match-define (cons x y) building-pos)
    (when (and (= (building-level (tile-ref st x y)) 1)
               (< (random 100) 30))
      (tile-set! st x y 'grass)))
  (define road-pos (random-pick (tiles-matching st road?)))
  (when road-pos
    (match-define (cons x y) road-pos)
    (define serves-buildings?
      (for/or ([c (in-list (neighbor-coords st x y))])
        (building? (tile-ref st (car c) (cdr c)))))
    (when (and (not serves-buildings?) (< (random 100) 20))
      (tile-set! st x y 'grass))))

;; The landscape drifts too: forests creep outward at their edges,
;; lone saplings colonize open grass, and old trees die back. Interior
;; trees have nowhere to spread but still die, so dense woods slowly
;; thin into clearings that later refill — the forests wander instead
;; of sitting frozen.
(define (advance-nature! st)
  ;; Seed colonization: rarely, a sapling takes root on open grass.
  (when (< (random 100) 8)
    (define pos (random-pick (tiles-matching st grass?)))
    (when pos
      (tile-set! st (car pos) (cdr pos) 'tree)))
  ;; Spread: forest edges claim adjacent grass.
  (for ([_ (in-range 3)])
    (define pos (random-pick (tiles-matching st tree?)))
    (when pos
      (match-define (cons dx dy) (random-pick orthogonal-directions))
      (define nx (+ (car pos) dx))
      (define ny (+ (cdr pos) dy))
      (when (and (in-bounds? st nx ny)
                 (grass? (tile-ref st nx ny))
                 (< (random 100) 60))
        (tile-set! st nx ny 'tree))))
  ;; Dieback: crowded trees die much more readily than isolated ones,
  ;; so dense woods self-thin while lone trees and small groves
  ;; persist — forests can't spiral into extinction or takeover.
  (for ([_ (in-range 3)])
    (define pos (random-pick (tiles-matching st tree?)))
    (when pos
      (match-define (cons x y) pos)
      (define crowding
        (for/sum ([c (in-list (neighbor-coords-8 st x y))])
          (if (tree? (tile-ref st (car c) (cdr c))) 1 0)))
      (when (< (random 100) (+ 10 (* 11 crowding)))
        (tile-set! st x y 'grass)))))

;; A 2x2 block of road reads as a parking lot, and a single-tile S/Z
;; zigzag (a corner that immediately re-corners with no straight run
;; between the turns) reads as a paving glitch rather than a street.
;; Multi-tile S-bends have a straight run between their corners and
;; don't match either shape, so they're left alone. Checked against
;; every rotation and mirror of the zigzag so bends in any direction
;; are caught.
(define zigzag-road-cells '((0 . 0) (0 . 1) (1 . 1) (1 . 2)))
(define zigzag-empty-cells '((1 . 0) (0 . 2)))

(define (rotate-offset c)
  (cons (cdr c) (- (car c))))

(define (flip-offset c)
  (cons (- (car c)) (cdr c)))

(define (offset-orientations cells)
  (for*/list ([flip? (in-list '(#f #t))]
              [rotations (in-range 4)])
    (for/fold ([c (if flip? (map flip-offset cells) cells)])
              ([_ (in-range rotations)])
      (map rotate-offset c))))

(define zigzag-orientations
  (map cons (offset-orientations zigzag-road-cells)
       (offset-orientations zigzag-empty-cells)))

(define (creates-road-square? st x y)
  (for*/or ([bx (in-list (list (sub1 x) x))]
            [by (in-list (list (sub1 y) y))])
    (for*/and ([dx (in-list '(0 1))]
               [dy (in-list '(0 1))])
      (define cx (+ bx dx))
      (define cy (+ by dy))
      (or (and (= cx x) (= cy y))
          (and (in-bounds? st cx cy) (road? (tile-ref st cx cy)))))))

(define (creates-road-zigzag? st x y)
  (for/or ([orient (in-list zigzag-orientations)])
    (match-define (cons road-cells empty-cells) orient)
    (for/or ([anchor (in-list road-cells)])
      (define ox (- x (car anchor)))
      (define oy (- y (cdr anchor)))
      (and (for/and ([c (in-list road-cells)])
             (define cx (+ ox (car c)))
             (define cy (+ oy (cdr c)))
             (or (and (= cx x) (= cy y))
                 (and (in-bounds? st cx cy) (road? (tile-ref st cx cy)))))
           (for/and ([c (in-list empty-cells)])
             (define cx (+ ox (car c)))
             (define cy (+ oy (cdr c)))
             (or (not (in-bounds? st cx cy))
                 (not (road? (tile-ref st cx cy)))))))))

(define (bad-road-shape? st x y)
  (or (creates-road-square? st x y)
      (creates-road-zigzag? st x y)))

;; Extends the road network by one tile from a random existing road.
;; Requiring the new tile to touch exactly one existing road keeps
;; streets from clumping into solid pavement; the occasional two-road
;; exception lets loops form, but never into a parking-lot square or a
;; single-tile zigzag. Small buildings can be bulldozed for the road
;; to pass through — otherwise new buildings encircle the network and
;; growth deadlocks.
(define (try-extend-road! st)
  (define road-pos (random-pick (tiles-matching st road?)))
  (when road-pos
    (match-define (cons x y) road-pos)
    (define (valid-direction? d)
      (define nx (+ x (car d)))
      (define ny (+ y (cdr d)))
      (and (in-bounds? st nx ny)
           (let ([tile (tile-ref st nx ny)])
             (or (buildable? tile)
                 (and (building? tile) (= (building-level tile) 1))))
           (let ([roads (road-neighbor-count st nx ny)])
             (or (= roads 1)
                 (and (= roads 2) (< (random 100) 15))))
           (not (bad-road-shape? st nx ny))))
    ;; A direction continues a straight street when the tile on the
    ;; opposite side is already road.
    (define (straight? d)
      (define bx (- x (car d)))
      (define by (- y (cdr d)))
      (and (in-bounds? st bx by) (road? (tile-ref st bx by))))
    (define candidates (filter valid-direction? orthogonal-directions))
    (define weighted
      (append candidates
              (for*/list ([d (in-list candidates)]
                          #:when (straight? d)
                          [_ (in-range 3)])
                d)))
    (define choice (random-pick weighted))
    (when choice
      (define nx (+ x (car choice)))
      (define ny (+ y (cdr choice)))
      (define target (tile-ref st nx ny))
      ;; Bulldozing a building for the road only sometimes succeeds.
      (when (or (buildable? target) (< (random 100) 40))
        (tile-set! st nx ny 'road)))))

;; Puts a small new building on a buildable tile next to a road.
;; Wooded lots are half as attractive as open grass.
(define (try-spawn-building! st)
  (define road-pos (random-pick (tiles-matching st road?)))
  (when road-pos
    (match-define (cons x y) road-pos)
    (match-define (cons dx dy) (random-pick orthogonal-directions))
    (define nx (+ x dx))
    (define ny (+ y dy))
    (when (and (in-bounds? st nx ny)
               (buildable? (tile-ref st nx ny))
               (or (grass? (tile-ref st nx ny)) (< (random 100) 50)))
      (tile-set! st nx ny (make-building 1 (random building-variant-count))))))

;; Levels up a random building, with odds weighted by how built-up its
;; surroundings are, so density grows from the inside out.
(define (try-upgrade-building! st)
  (define pos (random-pick (tiles-matching st building?)))
  (when pos
    (match-define (cons x y) pos)
    (define tile (tile-ref st x y))
    (define density
      (for/sum ([c (in-list (neighbor-coords-8 st x y))])
        (if (building? (tile-ref st (car c) (cdr c))) 1 0)))
    (when (and (< (building-level tile) max-building-level)
               (< (random 12) density))
      (tile-set! st x y
                 (make-building (add1 (building-level tile))
                                (building-variant tile))))))

(module+ test
  (test-case "bad-road-shape?"
    (define (grid-state width height road-coords)
      (define st (city-state 0 width height
                             (make-vector (* width height) 'grass)
                             (make-pseudo-random-generator)))
      (for ([c (in-list road-coords)])
        (tile-set! st (car c) (cdr c) 'road))
      st)
    (test-case "flags a tile that would complete a 2x2 square"
      (define st (grid-state 2 2 '((0 . 0) (1 . 0) (0 . 1))))
      (check-true (creates-road-square? st 1 1))
      (check-false (creates-road-zigzag? st 1 1)))
    (test-case "does not flag an ordinary corner"
      (define st (grid-state 2 3 '((0 . 0) (0 . 1))))
      (check-false (creates-road-square? st 1 1)))
    (test-case "flags a tile that would complete a single-tile S-bend"
      (define st (grid-state 2 3 '((0 . 0) (0 . 1) (1 . 1))))
      (check-true (creates-road-zigzag? st 1 2))
      (check-false (creates-road-square? st 1 2)))
    (test-case "does not flag a multi-tile S-bend"
      (define st (grid-state 3 3 '((0 . 0) (0 . 1) (1 . 1) (2 . 1))))
      (check-false (creates-road-zigzag? st 2 2))
      (check-false (creates-road-square? st 2 2))))

  (test-case "advance-city! never produces parking-lot squares or single-tile zigzags"
    (define st (generate-world 32 32 7))
    (for ([_ (in-range 200)])
      (advance-city! st))
    (for* ([y (in-range 32)]
           [x (in-range 32)]
           #:when (road? (tile-ref st x y)))
      (check-false (creates-road-square? st x y)
                   (format "2x2 road square at ~a,~a" x y))
      (check-false (creates-road-zigzag? st x y)
                   (format "single-tile zigzag at ~a,~a" x y))))

  (test-case "advance-city!"
    (define st (generate-world 32 32 42))
    (for ([_ (in-range 50)])
      (advance-city! st))
    (test-case "advances the tick counter"
      (check-equal? (city-state-tick st) 50))
    (test-case "grows roads and buildings"
      (check-true (> (length (tiles-matching st road?)) 2))
      (check-true (pair? (tiles-matching st building?))))
    (test-case "keeps every tile well-formed"
      (for* ([y (in-range 32)]
             [x (in-range 32)])
        (define tile (tile-ref st x y))
        (check-true (or (water? tile) (grass? tile) (tree? tile)
                        (road? tile) (building? tile)))
        (when (building? tile)
          (check-true (<= 1 (building-level tile) max-building-level)))))
    (test-case "never paves over water"
      (define st2 (generate-world 32 32 42))
      (define water-before (tiles-matching st2 water?))
      (for ([_ (in-range 50)])
        (advance-city! st2))
      (check-equal? (tiles-matching st2 water?) water-before))
    (test-case "forests drift but neither vanish nor take over"
      (define st3 (generate-world 32 32 42))
      (define trees-before (tiles-matching st3 tree?))
      (define land-count
        (- (* 32 32) (length (tiles-matching st3 water?))))
      (for ([_ (in-range 100)])
        (advance-city! st3))
      (define trees-after (tiles-matching st3 tree?))
      (check-true (pair? trees-after))
      (check-true (< (length trees-after) (quotient (* land-count 2) 3)))
      (check-not-equal? trees-after trees-before))))
