#lang racket/base

;;; Static site output: an index.html and stylesheet alongside the
;;; rendered city.png. The whole site is regenerated from scratch on
;;; every tick — it's a pure function of the city state.

(require racket/file
         racket/list
         racket/match
         "state.rkt")

(provide write-site!
         city-population)

(module+ test
  (require rackunit))

;; Rough residents per building level — towers hold a lot more people.
(define (level-population level)
  (match level
    [1 6]
    [2 25]
    [3 90]))

(define (city-population st)
  (for/sum ([pos (in-list (tiles-matching st building?))])
    (level-population (building-level (tile-ref st (car pos) (cdr pos))))))

(define (utc-timestamp)
  (define d (seconds->date (current-seconds) #f))
  (define (pad n) (if (< n 10) (format "0~a" n) (format "~a" n)))
  (format "~a-~a-~a ~a:~a"
          (date-year d) (pad (date-month d)) (pad (date-day d))
          (pad (date-hour d)) (pad (date-minute d))))

(define (index-html st)
  (define day (city-state-tick st))
  (define population (city-population st))
  (define buildings (length (tiles-matching st building?)))
  (define roads (length (tiles-matching st road?)))
  (define trees (length (tiles-matching st tree?)))
  (string-append
   "<!doctype html>\n"
   "<html lang=\"en\">\n"
   "<head>\n"
   "<meta charset=\"utf-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   (format "<title>trmcity — day ~a</title>\n" day)
   "<link rel=\"stylesheet\" href=\"style.css\">\n"
   "</head>\n"
   "<body>\n"
   "<main>\n"
   "<h1>trmcity</h1>\n"
   "<p class=\"tagline\">a tiny city, growing on its own schedule</p>\n"
   "<img class=\"city\" src=\"city.png\" "
   "alt=\"Isometric pixel-art view of a procedurally grown city\">\n"
   (format "<p class=\"stats\">day ~a · population ~a · ~a buildings · ~a roads · ~a trees</p>\n"
           day population buildings roads trees)
   (format "<footer>last grown ~a UTC · simulated &amp; drawn in Racket</footer>\n"
           (utc-timestamp))
   "</main>\n"
   "</body>\n"
   "</html>\n"))

(define stylesheet #<<CSS
:root {
  color-scheme: dark;
}

body {
  margin: 0;
  min-height: 100vh;
  display: flex;
  align-items: center;
  justify-content: center;
  background: #181a23;
  color: #d8d9e0;
  font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
}

main {
  text-align: center;
  padding: 2rem 1rem;
}

h1 {
  margin: 0;
  font-size: 1.6rem;
  letter-spacing: 0.35em;
  text-transform: uppercase;
  color: #f0f1f5;
}

.tagline {
  margin: 0.4rem 0 1.5rem;
  color: #8a8da0;
  font-size: 0.85rem;
}

.city {
  width: min(100%, 1056px);
  image-rendering: pixelated;
  border-radius: 6px;
}

.stats {
  margin: 1.2rem 0 0;
  font-size: 0.9rem;
  color: #b8bac8;
}

footer {
  margin-top: 0.6rem;
  font-size: 0.75rem;
  color: #686b7e;
}
CSS
)

(define (write-site! st site-dir)
  (make-directory* site-dir)
  (display-to-file (index-html st)
                   (build-path site-dir "index.html")
                   #:exists 'replace)
  (display-to-file stylesheet
                   (build-path site-dir "style.css")
                   #:exists 'replace))

(module+ test
  (test-case "city-population"
    (define st (city-state 0 2 2
                           (vector 'grass (make-building 1 0)
                                   (make-building 3 1) 'road)
                           (make-pseudo-random-generator)))
    (check-equal? (city-population st) 96))

  (test-case "write-site! produces the site files"
    (define dir (make-temporary-directory "trmcity-site-~a"))
    (define st (city-state 5 1 1 (vector 'grass) (make-pseudo-random-generator)))
    (write-site! st dir)
    (check-true (file-exists? (build-path dir "index.html")))
    (check-true (file-exists? (build-path dir "style.css")))
    (delete-directory/files dir)))
