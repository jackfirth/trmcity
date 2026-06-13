#lang racket/base

;;; Isometric renderer: paints the city onto a PNG with racket/draw.
;;; Classic 2:1 diamond projection, drawn back to front so buildings
;;; correctly occlude what's behind them. All art is drawn in code —
;;; no sprite assets.

(require racket/class
         racket/draw
         racket/list
         racket/match
         "state.rkt")

(provide render-city
         render-city-eink)

(define half-tile-w 8)
(define half-tile-h 4)
(define tile-h (* 2 half-tile-h))
(define margin-x 16)
(define margin-top 48)
(define margin-bottom 24)

;; Deterministic per-tile variation, so the map doesn't shimmer
;; between renders.
(define (tile-hash x y)
  (modulo (+ (* 7919 x) (* 104729 y) (* 31 x y)) 1024))

;;; Palette

(define background-color (make-color 24 26 35))
(define water-color (make-color 52 94 168))
(define shore-color (make-color 70 114 188))
(define grass-colors
  (list (make-color 96 156 78)
        (make-color 88 148 70)
        (make-color 103 163 85)))
(define road-color (make-color 98 98 104))
(define road-line-color (make-color 138 136 118))
(define pavement-color (make-color 130 128 126))
(define trunk-color (make-color 96 70 48))
(define canopy-color (make-color 44 98 50))
(define canopy-light-color (make-color 62 120 62))
(define pine-color (make-color 38 86 54))
(define window-lit-color (make-color 255 226 130))
(define window-dark-color (make-color 38 42 58))

;; Each building variant: wall facing the light, wall in shade, roof.
(define building-palettes
  (list (list (make-color 216 198 160) (make-color 178 160 124) (make-color 152 84 66))
        (list (make-color 182 112 92) (make-color 144 86 70) (make-color 92 92 102))
        (list (make-color 134 144 164) (make-color 102 112 132) (make-color 72 78 94))
        (list (make-color 184 184 180) (make-color 148 148 146) (make-color 122 128 134))))

;;; Drawing helpers

(define (fill dc color)
  (send dc set-brush color 'solid))

(define (diamond-points sx sy)
  (list (cons sx sy)
        (cons (+ sx half-tile-w) (+ sy half-tile-h))
        (cons sx (+ sy tile-h))
        (cons (- sx half-tile-w) (+ sy half-tile-h))))

(define (draw-diamond dc sx sy color)
  (fill dc color)
  (send dc draw-polygon (diamond-points sx sy)))

;;; Tile renderers
;;; (sx, sy) is always the screen position of the tile's top corner.

(define (draw-water dc st x y sx sy)
  (define shore?
    (for/or ([c (in-list (neighbor-coords st x y))])
      (not (water? (tile-ref st (car c) (cdr c))))))
  (draw-diamond dc sx sy (if shore? shore-color water-color)))

(define (draw-grass dc x y sx sy)
  (define color (list-ref grass-colors (modulo (tile-hash x y) (length grass-colors))))
  (draw-diamond dc sx sy color))

(define (draw-tree dc x y sx sy)
  (draw-grass dc x y sx sy)
  (define cx sx)
  (define cy (+ sy half-tile-h))
  (fill dc trunk-color)
  (send dc draw-rectangle (- cx 1) (- cy 3) 2 4)
  (cond
    [(even? (tile-hash x y))
     (fill dc canopy-color)
     (send dc draw-ellipse (- cx 4) (- cy 11) 8 8)
     (fill dc canopy-light-color)
     (send dc draw-ellipse (- cx 3) (- cy 10) 5 5)]
    [else
     (fill dc pine-color)
     (send dc draw-polygon
           (list (cons cx (- cy 13))
                 (cons (+ cx 4) (- cy 3))
                 (cons (- cx 4) (- cy 3))))]))

(define (draw-road dc st x y sx sy)
  (draw-diamond dc sx sy road-color)
  ;; Faint lane markings reaching toward each connected neighbor.
  (define cx sx)
  (define cy (+ sy half-tile-h))
  (send dc set-pen road-line-color 1 'solid)
  (for ([d (in-list '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))])
    (define nx (+ x (car d)))
    (define ny (+ y (cdr d)))
    (when (and (in-bounds? st nx ny)
               (road? (tile-ref st nx ny)))
      (define edge-x (+ cx (* (- (car d) (cdr d)) half-tile-w 1/2)))
      (define edge-y (+ cy (* (+ (car d) (cdr d)) half-tile-h 1/2)))
      (send dc draw-line cx cy edge-x edge-y)))
  (send dc set-pen "black" 0 'transparent))

(define (building-height level hash)
  (match level
    [1 (+ 9 (modulo hash 3))]
    [2 (+ 16 (modulo hash 5))]
    [3 (+ 26 (modulo hash 7))]))

(define (draw-building dc tile x y sx sy)
  (draw-diamond dc sx sy pavement-color)
  (define level (building-level tile))
  (define hash (tile-hash x y))
  (define height (building-height level hash))
  (match-define (list wall-light wall-shade roof)
    (list-ref building-palettes (building-variant tile)))
  ;; Corner positions of the building footprint — inset from the tile
  ;; diamond so a strip of pavement shows and neighboring buildings
  ;; don't fuse into one mass.
  (define top-x sx)
  (define top-y (+ sy 1))
  (define right-x (+ sx half-tile-w -2))
  (define right-y (+ sy half-tile-h))
  (define bottom-x sx)
  (define bottom-y (+ sy tile-h -1))
  (define left-x (- sx half-tile-w -2))
  (define left-y (+ sy half-tile-h))
  ;; Southwest face (toward the light).
  (fill dc wall-light)
  (send dc draw-polygon
        (list (cons left-x left-y)
              (cons bottom-x bottom-y)
              (cons bottom-x (- bottom-y height))
              (cons left-x (- left-y height))))
  ;; Southeast face (in shade).
  (fill dc wall-shade)
  (send dc draw-polygon
        (list (cons bottom-x bottom-y)
              (cons right-x right-y)
              (cons right-x (- right-y height))
              (cons bottom-x (- bottom-y height))))
  ;; Roof.
  (fill dc roof)
  (send dc draw-polygon
        (list (cons top-x (- top-y height))
              (cons right-x (- right-y height))
              (cons bottom-x (- bottom-y height))
              (cons left-x (- left-y height))))
  (draw-windows dc x y height left-x left-y bottom-x bottom-y right-x right-y))

;; Small window rectangles on both visible faces, in rows every few
;; pixels of building height. Some are lit, deterministically.
(define (draw-windows dc x y height left-x left-y bottom-x bottom-y right-x right-y)
  (for ([row (in-naturals 1)]
        #:break (> (+ (* 6 row) 4) height))
    (define dy (* 6 row))
    (for ([t (in-list '(0.35 0.7))]
          [column (in-naturals)])
      ;; A point t of the way along each face's base edge, lifted by dy.
      (define lit? (even? (+ (tile-hash x y) row column (modulo x 3))))
      (fill dc (if lit? window-lit-color window-dark-color))
      (define lx (+ left-x (* t (- bottom-x left-x))))
      (define ly (+ left-y (* t (- bottom-y left-y))))
      (send dc draw-rectangle (- lx 1) (- ly dy 1) 2 3)
      (define rx (+ bottom-x (* t (- right-x bottom-x))))
      (define ry (+ bottom-y (* t (- right-y bottom-y))))
      (define lit2? (odd? (+ (tile-hash x y) row column (modulo y 3))))
      (fill dc (if lit2? window-lit-color window-dark-color))
      (send dc draw-rectangle (- rx 1) (- ry dy 1) 2 3))))

(define (draw-tile dc st x y origin-x origin-y)
  (define sx (+ origin-x (* half-tile-w (- x y))))
  (define sy (+ origin-y (* half-tile-h (+ x y))))
  (define tile (tile-ref st x y))
  (cond
    [(water? tile) (draw-water dc st x y sx sy)]
    [(grass? tile) (draw-grass dc x y sx sy)]
    [(tree? tile) (draw-tree dc x y sx sy)]
    [(road? tile) (draw-road dc st x y sx sy)]
    [(building? tile) (draw-building dc tile x y sx sy)]))

;;; Entry point

;;; E-ink renderer: an 800×480 high-contrast variant sized exactly for
;;; a TRMNL display. Drawn in a near-1-bit style — white ground, black
;;; roads and trees, outlined buildings, gray water left for the
;;; display's dithering — so it survives grayscale conversion. The tile
;;; size adapts to the grid so any map fills the panel: a 64×64 map
;;; gets 12×6 tiles, a 32×32 map gets 24×12.

(define eink-width 800)
(define eink-height 480)
(define eink-margin-top 60)

(define eink-white (make-color 255 255 255))
(define eink-black (make-color 0 0 0))
(define eink-gray (make-color 150 150 150))

;; The half-tile-width (s) that fits the map into the panel, kept even
;; so the half-height s/2 stays integral.
(define (eink-tile-scale w h)
  (max 2 (* 2 (quotient (quotient 768 (+ w h)) 2))))

(define (eink-building-height s level hash)
  (match level
    [1 (+ s (modulo hash (max 2 (quotient s 4))))]
    [2 (+ (* 2 s) -1 (modulo hash (max 2 (quotient s 3))))]
    [3 (+ (* 3 s) -2 (modulo hash (max 3 (quotient s 2))))]))

(define (render-city-eink st path)
  (define w (city-state-width st))
  (define h (city-state-height st))
  (define s (eink-tile-scale w h))
  (define bmp (make-bitmap eink-width eink-height))
  (define dc (new bitmap-dc% [bitmap bmp]))
  (send dc set-smoothing 'unsmoothed)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush eink-white 'solid)
  (send dc draw-rectangle 0 0 eink-width eink-height)
  (define origin-x (quotient eink-width 2))
  (for ([diag (in-range (+ w h -1))])
    (for ([x (in-range (max 0 (- diag (sub1 h))) (min w (add1 diag)))])
      (draw-eink-tile dc st s x (- diag x) origin-x eink-margin-top)))
  (send bmp save-file (if (path? path) path (string->path path)) 'png))

(define (draw-eink-tile dc st s x y origin-x origin-y)
  (define sx (+ origin-x (* s (- x y))))
  (define sy (+ origin-y (* (quotient s 2) (+ x y))))
  (define tile (tile-ref st x y))
  ;; Grass tiles are left as bare background: the white world reads
  ;; cleanly on e-ink, and everything else supplies the contrast.
  (cond
    [(water? tile) (draw-eink-water dc st s x y sx sy)]
    [(tree? tile) (draw-eink-tree dc s x y sx sy)]
    [(road? tile) (draw-eink-road dc s sx sy)]
    [(building? tile) (draw-eink-building dc s tile x y sx sy)]))

(define (eink-diamond-points s sx sy)
  (list (cons sx sy)
        (cons (+ sx s) (+ sy (quotient s 2)))
        (cons sx (+ sy s))
        (cons (- sx s) (+ sy (quotient s 2)))))

;; Gray fill (dithered to texture by the display), with a black
;; coastline along any edge bordering land.
(define (draw-eink-water dc st s x y sx sy)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush eink-gray 'solid)
  (send dc draw-polygon (eink-diamond-points s sx sy))
  (match-define (list top right bottom left) (eink-diamond-points s sx sy))
  (define (land? dx dy)
    (and (in-bounds? st (+ x dx) (+ y dy))
         (not (water? (tile-ref st (+ x dx) (+ y dy))))))
  (send dc set-pen eink-black 1 'solid)
  (define (edge a b)
    (send dc draw-line (car a) (cdr a) (car b) (cdr b)))
  (when (land? 1 0) (edge right bottom))
  (when (land? -1 0) (edge top left))
  (when (land? 0 1) (edge left bottom))
  (when (land? 0 -1) (edge top right))
  (send dc set-pen "black" 0 'transparent))

(define (draw-eink-tree dc s x y sx sy)
  (define cx sx)
  (define cy (+ sy (quotient s 2)))
  (define top (- cy s 1 (modulo (tile-hash x y) (max 2 (quotient s 2)))))
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush eink-black 'solid)
  (send dc draw-polygon
        (list (cons cx top)
              (cons (+ cx (quotient s 2)) (- cy 1))
              (cons (- cx (quotient s 2)) (- cy 1)))))

(define (draw-eink-road dc s sx sy)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush eink-black 'solid)
  (send dc draw-polygon (eink-diamond-points s sx sy)))

(define (draw-eink-building dc s tile x y sx sy)
  (define height (eink-building-height s (building-level tile) (tile-hash x y)))
  ;; Inset footprint corners, as in the color renderer.
  (define inset (max 1 (quotient s 6)))
  (define top-x sx)
  (define top-y (+ sy (quotient inset 2) 1))
  (define right-x (+ sx s (- inset)))
  (define right-y (+ sy (quotient s 2)))
  (define bottom-x sx)
  (define bottom-y (+ sy s (- (quotient inset 2)) -1))
  (define left-x (- sx s (- inset)))
  (define left-y (+ sy (quotient s 2)))
  (send dc set-pen eink-black 1 'solid)
  ;; Southwest face: white, outlined.
  (send dc set-brush eink-white 'solid)
  (send dc draw-polygon
        (list (cons left-x left-y)
              (cons bottom-x bottom-y)
              (cons bottom-x (- bottom-y height))
              (cons left-x (- left-y height))))
  ;; Southeast face: solid black for the 3D read.
  (send dc set-brush eink-black 'solid)
  (send dc draw-polygon
        (list (cons bottom-x bottom-y)
              (cons right-x right-y)
              (cons right-x (- right-y height))
              (cons bottom-x (- bottom-y height))))
  ;; Roof: white, outlined.
  (send dc set-brush eink-white 'solid)
  (send dc draw-polygon
        (list (cons top-x (- top-y height))
              (cons right-x (- right-y height))
              (cons bottom-x (- bottom-y height))
              (cons left-x (- left-y height))))
  (send dc set-pen "black" 0 'transparent))

(define (render-city st path)
  (define w (city-state-width st))
  (define h (city-state-height st))
  (define px-w (+ (* half-tile-w (+ w h)) (* 2 margin-x)))
  (define px-h (+ (* half-tile-h (+ w h)) margin-top margin-bottom))
  (define bmp (make-bitmap px-w px-h))
  (define dc (new bitmap-dc% [bitmap bmp]))
  (send dc set-smoothing 'unsmoothed)
  (send dc set-pen "black" 0 'transparent)
  (fill dc background-color)
  (send dc draw-rectangle 0 0 px-w px-h)
  (define origin-x (+ margin-x (* half-tile-w h)))
  ;; Painter's algorithm: sweep diagonals from the back of the scene
  ;; (x+y small) to the front.
  (for ([diag (in-range (+ w h -1))])
    (for ([x (in-range (max 0 (- diag (sub1 h))) (min w (add1 diag)))])
      (draw-tile dc st x (- diag x) origin-x margin-top)))
  (send bmp save-file (if (path? path) path (string->path path)) 'png))
