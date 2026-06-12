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

(define (advance-city! st)
  (set-city-state-tick! st (add1 (city-state-tick st)))
  (parameterize ([current-pseudo-random-generator (city-state-rng st)])
    (define building-count (length (tiles-matching st building?)))
    ;; Growth speeds up gently as the city matures, capped so a large
    ;; city still changes at a watchable pace.
    (define iterations (min 40 (+ 10 (quotient building-count 8))))
    (for ([_ (in-range iterations)])
      (define roll (random 100))
      (cond
        [(< roll 35) (try-extend-road! st)]
        [(< roll 75) (try-spawn-building! st)]
        [else (try-upgrade-building! st)]))))

;; Extends the road network by one tile from a random existing road.
;; Requiring the new tile to touch exactly one existing road keeps
;; streets from clumping into solid pavement; the occasional two-road
;; exception lets loops form. Small buildings can be bulldozed for the
;; road to pass through — otherwise new buildings encircle the network
;; and growth deadlocks.
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
                 (and (= roads 2) (< (random 100) 15))))))
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
(define (try-spawn-building! st)
  (define road-pos (random-pick (tiles-matching st road?)))
  (when road-pos
    (match-define (cons x y) road-pos)
    (match-define (cons dx dy) (random-pick orthogonal-directions))
    (define nx (+ x dx))
    (define ny (+ y dy))
    (when (and (in-bounds? st nx ny)
               (buildable? (tile-ref st nx ny)))
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
      (check-equal? (tiles-matching st2 water?) water-before))))
