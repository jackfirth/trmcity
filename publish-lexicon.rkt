#lang racket/base

;;; Publishes the space.notjack.trmcity lexicon schema as a
;;; com.atproto.lexicon.schema record in the city account's repo, per
;;; the atproto lexicon-resolution spec. Run after any lexicon change.

(require json
         racket/file
         racket/string
         "private/atproto.rkt")

(define lexicon-path "lexicons/space.notjack.trmcity.map.json")
(define lexicon-nsid "space.notjack.trmcity.map")

(module+ main
  (define handle (vector-ref (current-command-line-arguments) 0))
  (define password-file "app_password.txt")
  (define lexicon (call-with-input-file lexicon-path read-json))
  (define did (resolve-handle handle))
  (define pds (pds-endpoint did))
  (define session (create-session pds handle (string-trim (file->string password-file))))
  (put-record session "com.atproto.lexicon.schema" lexicon-nsid
              (hash-set lexicon '$type "com.atproto.lexicon.schema"))
  (printf "published at://~a/com.atproto.lexicon.schema/~a\n" did lexicon-nsid))
