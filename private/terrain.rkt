#lang racket/base

;;; World generation: value-noise terrain (water, grass, forests) plus
;;; the founding road tiles the city grows from. Only runs once, on the
;;; first tick, when no saved state exists.

(require racket/list
         racket/match
         "state.rkt")

(provide generate-world)

(module+ test
  (require rackunit))

;;; Value noise

(define (make-lattice cols rows)
  (build-vector (* cols rows) (lambda (i) (random))))

(define (lattice-ref lattice cols cx cy)
  (vector-ref lattice (+ cx (* cy cols))))

(define (smoothstep t)
  (* t t (- 3.0 (* 2.0 t))))

(define (lerp a b t)
  (+ a (* t (- b a))))

;; Bilinear interpolation over a coarse lattice of random values.
(define (sample-noise lattice cols cell x y)
  (define gx (/ (exact->inexact x) cell))
  (define gy (/ (exact->inexact y) cell))
  (define ix (inexact->exact (floor gx)))
  (define iy (inexact->exact (floor gy)))
  (define fx (smoothstep (- gx ix)))
  (define fy (smoothstep (- gy iy)))
  (define v00 (lattice-ref lattice cols ix iy))
  (define v10 (lattice-ref lattice cols (add1 ix) iy))
  (define v01 (lattice-ref lattice cols ix (add1 iy)))
  (define v11 (lattice-ref lattice cols (add1 ix) (add1 iy)))
  (lerp (lerp v00 v10 fx) (lerp v01 v11 fx) fy))

;; A width*height vector of noise values in [0, 1], two octaves.
(define (noise-map width height cell)
  (define cols (+ 3 (quotient width cell)))
  (define rows (+ 3 (quotient height cell)))
  (define coarse (make-lattice cols rows))
  (define fine-cell (max 2 (quotient cell 2)))
  (define fine-cols (+ 3 (quotient width fine-cell)))
  (define fine-rows (+ 3 (quotient height fine-cell)))
  (define fine (make-lattice fine-cols fine-rows))
  (for*/vector ([y (in-range height)]
                [x (in-range width)])
    (+ (* 0.7 (sample-noise coarse cols cell x y))
       (* 0.3 (sample-noise fine fine-cols fine-cell x y)))))

;; The value below which the given fraction of all values fall.
(define (percentile values fraction)
  (define sorted (sort (vector->list values) <))
  (list-ref sorted (inexact->exact (floor (* fraction (length sorted))))))

;;; World generation

(define water-fraction 0.18)
(define forest-threshold-fraction 0.72)

(define (generate-world width height seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (define elevation (noise-map width height 16))
    (define forest (noise-map width height 8))
    (define sea-level (percentile elevation water-fraction))
    (define tree-line (percentile forest forest-threshold-fraction))
    (define grid
      (for*/vector ([y (in-range height)]
                    [x (in-range width)])
        (define i (+ x (* y width)))
        (cond
          [(< (vector-ref elevation i) sea-level) 'water]
          [(and (> (vector-ref forest i) tree-line) (< (random 100) 65)) 'tree]
          [else 'grass])))
    (define st (city-state 0 width height grid rng))
    (found-town! st)
    st))

;; Picks a town center near the middle of the map, on solid ground,
;; and lays the founding road tiles.
(define (found-town! st)
  (define cx (quotient (city-state-width st) 2))
  (define cy (quotient (city-state-height st) 2))
  (define (distance-to-center pos)
    (match-define (cons x y) pos)
    (+ (* (- x cx) (- x cx)) (* (- y cy) (- y cy))))
  (define candidates
    (sort (tiles-matching st grass?) < #:key distance-to-center))
  (define center
    (or (for/first ([pos (in-list candidates)]
                    #:when (clear-around? st pos 2))
          pos)
        (first candidates)))
  (match-define (cons x y) center)
  (tile-set! st x y 'road)
  (when (in-bounds? st (add1 x) y)
    (tile-set! st (add1 x) y 'road)))

;; True when no tile within the given Chebyshev radius is water.
(define (clear-around? st pos radius)
  (match-define (cons x y) pos)
  (for*/and ([dy (in-range (- radius) (add1 radius))]
             [dx (in-range (- radius) (add1 radius))])
    (and (in-bounds? st (+ x dx) (+ y dy))
         (not (water? (tile-ref st (+ x dx) (+ y dy)))))))

(module+ test
  (test-case "generate-world"
    (define st (generate-world 32 32 42))
    (test-case "produces a full grid"
      (check-equal? (vector-length (city-state-grid st)) (* 32 32)))
    (test-case "contains some water and some grass"
      (check-true (pair? (tiles-matching st water?)))
      (check-true (pair? (tiles-matching st grass?))))
    (test-case "founds the town with road tiles"
      (check-true (pair? (tiles-matching st road?))))
    (test-case "is deterministic for a given seed"
      (define st2 (generate-world 32 32 42))
      (check-equal? (city-state-grid st2) (city-state-grid st)))))
