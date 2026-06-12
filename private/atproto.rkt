#lang racket/base

;;; A minimal AT Protocol client: just enough XRPC to resolve a handle
;;; to its PDS, authenticate, and read/write repository records. Uses
;;; only the Racket standard library.

(require json
         net/uri-codec
         net/url
         racket/string)

(provide (struct-out atproto-session)
         resolve-handle
         pds-endpoint
         create-session
         get-record
         put-record
         delete-record)

(struct atproto-session (pds did access-jwt) #:transparent)

(define (read-json-response port)
  (define result (read-json port))
  (close-input-port port)
  (if (eof-object? result) (hasheq) result))

(define (check-xrpc-error who response)
  (when (and (hash? response) (hash-has-key? response 'error))
    (raise-arguments-error who "XRPC request failed"
                           "error" (hash-ref response 'error)
                           "message" (hash-ref response 'message "")))
  response)

(define (xrpc-get who base nsid params)
  (define url
    (string->url (format "~a/xrpc/~a?~a" base nsid (alist->form-urlencoded params))))
  (check-xrpc-error who (read-json-response (get-pure-port url))))

(define (xrpc-post who base nsid body #:jwt [jwt #f])
  (define headers
    (append (list "Content-Type: application/json")
            (if jwt (list (format "Authorization: Bearer ~a" jwt)) '())))
  (define url (string->url (format "~a/xrpc/~a" base nsid)))
  (check-xrpc-error who (read-json-response (post-pure-port url (jsexpr->bytes body) headers))))

;; Resolves a handle to its DID using the public Bluesky AppView.
(define (resolve-handle handle)
  (hash-ref (xrpc-get 'resolve-handle
                      "https://public.api.bsky.app"
                      "com.atproto.identity.resolveHandle"
                      (list (cons 'handle handle)))
            'did))

;; Looks up the PDS service endpoint in the DID document.
(define (pds-endpoint did)
  (define doc
    (cond
      [(string-prefix? did "did:plc:")
       (read-json-response (get-pure-port (string->url (format "https://plc.directory/~a" did))))]
      [(string-prefix? did "did:web:")
       (define domain (substring did (string-length "did:web:")))
       (read-json-response
        (get-pure-port (string->url (format "https://~a/.well-known/did.json" domain))))]
      [else (raise-argument-error 'pds-endpoint "did:plc or did:web DID" did)]))
  (define service
    (for/first ([s (in-list (hash-ref doc 'service '()))]
                #:when (equal? (hash-ref s 'id "") "#atproto_pds"))
      s))
  (unless service
    (raise-arguments-error 'pds-endpoint "DID document has no PDS service" "did" did))
  (hash-ref service 'serviceEndpoint))

(define (create-session pds identifier password)
  (define response
    (xrpc-post 'create-session pds "com.atproto.server.createSession"
               (hasheq 'identifier identifier 'password password)))
  (atproto-session pds (hash-ref response 'did) (hash-ref response 'accessJwt)))

;; Returns the record's value, or #f if no such record exists.
(define (get-record pds repo collection rkey)
  (define url
    (string->url
     (format "~a/xrpc/com.atproto.repo.getRecord?~a"
             pds
             (alist->form-urlencoded
              (list (cons 'repo repo) (cons 'collection collection) (cons 'rkey rkey))))))
  (define response (read-json-response (get-pure-port url)))
  (cond
    [(and (hash? response) (hash-has-key? response 'value)) (hash-ref response 'value)]
    [(and (hash? response) (equal? (hash-ref response 'error #f) "RecordNotFound")) #f]
    [else (check-xrpc-error 'get-record response)
          (raise-arguments-error 'get-record "malformed XRPC response")]))

(define (put-record session collection rkey record)
  (xrpc-post 'put-record
             (atproto-session-pds session)
             "com.atproto.repo.putRecord"
             (hasheq 'repo (atproto-session-did session)
                     'collection collection
                     'rkey rkey
                     'record record)
             #:jwt (atproto-session-access-jwt session)))

(define (delete-record session collection rkey)
  (xrpc-post 'delete-record
             (atproto-session-pds session)
             "com.atproto.repo.deleteRecord"
             (hasheq 'repo (atproto-session-did session)
                     'collection collection
                     'rkey rkey)
             #:jwt (atproto-session-access-jwt session)))
