#lang racket/base

;;; City state: the tile grid, tick counter, and RNG, plus serialization
;;; to a plain s-expression file so each run can pick up where the last
;;; one left off.

(require racket/list
         racket/match
         racket/vector)

(provide (struct-out city-state)
         in-bounds?
         tile-ref
         tile-set!
         water?
         grass?
         tree?
         road?
         building?
         buildable?
         make-building
         building-level
         building-variant
         tiles-matching
         neighbor-coords
         neighbor-coords-8
         road-neighbor-count
         save-state!
         load-state
         city-state->jsexpr
         jsexpr->city-state
         iso8601-now)

(module+ test
  (require rackunit
           racket/file))

;; grid is a mutable vector of tiles in row-major order. A tile is one of:
;;   'water | 'grass | 'tree | 'road | (list 'building level variant)
(struct city-state (tick width height grid rng)
  #:transparent
  #:mutable)

(define (in-bounds? st x y)
  (and (<= 0 x)
       (< x (city-state-width st))
       (<= 0 y)
       (< y (city-state-height st))))

(define (tile-index st x y)
  (+ x (* y (city-state-width st))))

(define (tile-ref st x y)
  (vector-ref (city-state-grid st) (tile-index st x y)))

(define (tile-set! st x y tile)
  (vector-set! (city-state-grid st) (tile-index st x y) tile))

(define (water? tile)
  (eq? tile 'water))

(define (grass? tile)
  (eq? tile 'grass))

(define (tree? tile)
  (eq? tile 'tree))

(define (road? tile)
  (eq? tile 'road))

(define (building? tile)
  (and (pair? tile) (eq? (first tile) 'building)))

;; Tiles that growth is allowed to pave over.
(define (buildable? tile)
  (or (grass? tile) (tree? tile)))

(define (make-building level variant)
  (list 'building level variant))

(define (building-level tile)
  (second tile))

(define (building-variant tile)
  (third tile))

;; Returns the coordinates of every tile whose contents satisfy pred,
;; as a list of (cons x y) pairs.
(define (tiles-matching st pred)
  (for*/list ([y (in-range (city-state-height st))]
              [x (in-range (city-state-width st))]
              #:when (pred (tile-ref st x y)))
    (cons x y)))

(define orthogonal-directions '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))

(define all-directions
  '((1 . 0) (-1 . 0) (0 . 1) (0 . -1) (1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))

(define (coords-toward st x y directions)
  (for/list ([d (in-list directions)]
             #:when (in-bounds? st (+ x (car d)) (+ y (cdr d))))
    (cons (+ x (car d)) (+ y (cdr d)))))

(define (neighbor-coords st x y)
  (coords-toward st x y orthogonal-directions))

(define (neighbor-coords-8 st x y)
  (coords-toward st x y all-directions))

(define (road-neighbor-count st x y)
  (for/sum ([c (in-list (neighbor-coords st x y))])
    (if (road? (tile-ref st (car c) (cdr c))) 1 0)))

;;; Serialization

(define state-format-version 1)

(define (save-state! st path)
  (define datum
    (list 'trmcity-state
          state-format-version
          (city-state-tick st)
          (city-state-width st)
          (city-state-height st)
          (city-state-grid st)
          (pseudo-random-generator->vector (city-state-rng st))))
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (write datum out)
      (newline out))))

;; Returns the saved state, or #f if no state file exists yet.
(define (load-state path)
  (and (file-exists? path)
       (match (call-with-input-file path read)
         [(list 'trmcity-state (== state-format-version) tick width height grid rng-vec)
          (city-state tick width height (vector-copy grid)
                      (vector->pseudo-random-generator rng-vec))]
         [_ (raise-arguments-error 'load-state
                                   "unrecognized state file format"
                                   "path" path)])))

;;; JSON serialization, for storing the city as an atproto record.
;;; The grid becomes one string per row, one character per tile, so the
;;; record itself reads as an ASCII map of the city.

(define city-record-type "space.notjack.trmcity.map")

(define building-char-base (char->integer #\a))

(define (tile->char tile)
  (cond
    [(water? tile) #\~]
    [(grass? tile) #\.]
    [(tree? tile) #\t]
    [(road? tile) #\#]
    [(building? tile)
     (integer->char (+ building-char-base
                       (* 4 (sub1 (building-level tile)))
                       (building-variant tile)))]
    [else (raise-argument-error 'tile->char "tile" tile)]))

(define (char->tile c)
  (cond
    [(char=? c #\~) 'water]
    [(char=? c #\.) 'grass]
    [(char=? c #\t) 'tree]
    [(char=? c #\#) 'road]
    [else
     (define index (- (char->integer c) building-char-base))
     (unless (<= 0 index 11)
       (raise-argument-error 'char->tile "tile character" c))
     (make-building (add1 (quotient index 4)) (remainder index 4))]))

(define (iso8601-now)
  (define d (seconds->date (current-seconds) #f))
  (define (pad n) (if (< n 10) (format "0~a" n) (format "~a" n)))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d) (pad (date-month d)) (pad (date-day d))
          (pad (date-hour d)) (pad (date-minute d)) (pad (date-second d))))

(define (city-state->jsexpr st #:founded-at [founded-at #f])
  (define width (city-state-width st))
  (define height (city-state-height st))
  (define base
    (hasheq '$type city-record-type
            'tick (city-state-tick st)
            'width width
            'height height
            'rows (for/list ([y (in-range height)])
                    (list->string
                     (for/list ([x (in-range width)])
                       (tile->char (tile-ref st x y)))))
            'rng (vector->list (pseudo-random-generator->vector (city-state-rng st)))
            'updatedAt (iso8601-now)))
  (if founded-at (hash-set base 'foundedAt founded-at) base))

(define (jsexpr->city-state js)
  (define width (hash-ref js 'width))
  (define height (hash-ref js 'height))
  (define rows (hash-ref js 'rows))
  (define grid (make-vector (* width height) 'grass))
  (for ([row (in-list rows)]
        [y (in-naturals)])
    (for ([c (in-string row)]
          [x (in-naturals)])
      (vector-set! grid (+ x (* y width)) (char->tile c))))
  (city-state (hash-ref js 'tick) width height grid
              (vector->pseudo-random-generator
               (list->vector (hash-ref js 'rng)))))

(module+ test
  (test-case "tile accessors"
    (define st (city-state 0 4 4 (make-vector 16 'grass) (make-pseudo-random-generator)))
    (check-true (grass? (tile-ref st 2 3)))
    (tile-set! st 2 3 'road)
    (check-true (road? (tile-ref st 2 3)))
    (check-equal? (tiles-matching st road?) (list (cons 2 3)))
    (check-equal? (road-neighbor-count st 2 2) 1)
    (check-false (in-bounds? st 4 0)))

  (test-case "save and load round-trip"
    (define st (city-state 7 2 2
                           (vector 'water 'grass 'road (make-building 2 1))
                           (make-pseudo-random-generator)))
    (define path (make-temporary-file "trmcity-test-~a.rktd"))
    (save-state! st path)
    (define loaded (load-state path))
    (delete-file path)
    (check-equal? (city-state-tick loaded) 7)
    (check-equal? (city-state-grid loaded) (city-state-grid st))
    (check-true (building? (tile-ref loaded 1 1)))
    (check-equal? (building-level (tile-ref loaded 1 1)) 2))

  (test-case "load-state returns #f for missing file"
    (check-false (load-state "no-such-state-file.rktd")))

  (test-case "jsexpr round-trip"
    (define st (city-state 9 3 2
                           (vector 'water 'grass 'tree
                                   'road (make-building 1 0) (make-building 3 2))
                           (make-pseudo-random-generator)))
    (define js (city-state->jsexpr st #:founded-at "2026-06-11T00:00:00Z"))
    (test-case "encodes rows as tile characters"
      (check-equal? (hash-ref js 'rows) (list "~.t" "#ak")))
    (test-case "carries metadata"
      (check-equal? (hash-ref js 'tick) 9)
      (check-equal? (hash-ref js '$type) "space.notjack.trmcity.map")
      (check-equal? (hash-ref js 'foundedAt) "2026-06-11T00:00:00Z")
      (check-equal? (length (hash-ref js 'rng)) 6))
    (test-case "decodes back to an identical city"
      (define loaded (jsexpr->city-state js))
      (check-equal? (city-state-tick loaded) 9)
      (check-equal? (city-state-grid loaded) (city-state-grid st))
      ;; The restored RNG must continue the same random sequence.
      (define expected
        (parameterize ([current-pseudo-random-generator (city-state-rng st)])
          (random 1000000)))
      (check-equal? (parameterize ([current-pseudo-random-generator
                                    (city-state-rng loaded)])
                      (random 1000000))
                    expected))))
