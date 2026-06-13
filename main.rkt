#lang racket/base

;;; trmcity: run one (or more) ticks of the city simulation and emit
;;; the static site. Designed to be run on a schedule — each invocation
;;; loads the saved state, grows the city a little, and regenerates
;;; everything in the site directory.
;;;
;;; The city's state lives in an atproto record
;;; (at://<did>/space.notjack.trmcity.map/self) so any machine with the
;;; app password can run a tick. With --local (or when no password
;;; file is present) state falls back to a local state.rktd file,
;;; which is handy for development.

(require racket/file
         racket/string
         racket/cmdline
         "private/atproto.rkt"
         "private/growth.rkt"
         "private/render.rkt"
         "private/site.rkt"
         "private/state.rkt"
         "private/terrain.rkt")

(define world-width 32)
(define world-height 32)
(define city-collection "space.notjack.trmcity.map")
(define city-rkey "self")

;; A storage is a pair of procedures: (load-city) -> (or/c city-state #f)
;; and (save-city! st).

(define (make-file-storage state-path)
  (values
   (lambda () (load-state state-path))
   (lambda (st) (save-state! st state-path))))

(define (make-pds-storage handle password-file)
  (define did (resolve-handle handle))
  (define pds (pds-endpoint did))
  (printf "resolved ~a -> ~a at ~a\n" handle did pds)
  (define password (string-trim (file->string password-file)))
  (define session (create-session pds handle password))
  ;; Preserved across the load/save pair so the founding date survives
  ;; every subsequent tick.
  (define founded-at #f)
  (values
   (lambda ()
     (define record (get-record pds did city-collection city-rkey))
     (set! founded-at (and record (hash-ref record 'foundedAt #f)))
     (and record (jsexpr->city-state record)))
   (lambda (st)
     (define response
       (put-record session city-collection city-rkey
                   (city-state->jsexpr st #:founded-at (or founded-at (iso8601-now)))))
     (printf "state saved to at://~a/~a/~a (validation: ~a)\n"
             did city-collection city-rkey
             (hash-ref response 'validationStatus "unknown")))))

(define (run-trmcity #:load-city load-city
                     #:save-city! save-city!
                     #:site-dir site-dir
                     #:ticks ticks
                     #:seed seed)
  (define st
    (or (load-city)
        (let ([seed (or seed (random 1000000000))])
          (printf "no saved city; founding a new one (seed ~a)\n" seed)
          (generate-world world-width world-height seed))))
  (for ([_ (in-range ticks)])
    (advance-city! st))
  (save-city! st)
  (write-site! st site-dir)
  (render-city st (build-path site-dir "city.png"))
  (render-city-eink st (build-path site-dir "trmnl.png"))
  (printf "day ~a · population ~a · ~a buildings · ~a roads\n"
          (city-state-tick st)
          (city-population st)
          (length (tiles-matching st building?))
          (length (tiles-matching st road?)))
  (printf "site written to ~a\n" site-dir))

(module+ main
  (define handle (make-parameter "claude.notjack.space"))
  (define password-file (make-parameter "app_password.txt"))
  (define local-mode (make-parameter #f))
  (define state-path (make-parameter "state.rktd"))
  (define site-dir (make-parameter "site"))
  (define ticks (make-parameter 1))
  (define seed (make-parameter #f))
  (command-line
   #:program "trmcity"
   #:once-each
   [("--handle") h "ATproto handle that owns the city record"
                 (handle h)]
   [("--password-file") path "File containing the app password"
                        (password-file path)]
   [("--local") "Use a local state file instead of the PDS"
                (local-mode #t)]
   [("--state") path "Local state file path, for --local mode (default: state.rktd)"
                (state-path path)]
   [("--site") dir "Output directory for the static site (default: site)"
               (site-dir dir)]
   [("--ticks") n "Number of ticks to simulate this run (default: 1)"
                (ticks (string->number n))]
   [("--seed") s "World seed, used only when founding a new city"
               (seed (string->number s))])
  (define-values (load-city save-city!)
    (if (or (local-mode) (not (file-exists? (password-file))))
        (make-file-storage (state-path))
        (make-pds-storage (handle) (password-file))))
  (run-trmcity #:load-city load-city
               #:save-city! save-city!
               #:site-dir (site-dir)
               #:ticks (ticks)
               #:seed (seed)))
