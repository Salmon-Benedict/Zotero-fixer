#lang racket/base
;; Loads a Zotero "CSL JSON" export and provides field accessors over the
;; parsed entries. An entry is just the jsexpr hash for that reference
;; (hasheq with symbol keys, as produced by racket/json's read-json).

(require json racket/list racket/string)

(provide load-entries
         entry-id entry-type entry-title entry-container-title
         entry-authors entry-editors person-literal? person-name
         entry-date entry-accessed date-year date-month date-day
         entry-volume entry-issue entry-page entry-publisher
         entry-doi entry-url)

;; load-entries : path -> (listof jsexpr)
;; Accepts either a top-level JSON array of entries, or a single entry.
(define (load-entries path)
  (define data (call-with-input-file path read-json))
  (cond
    [(list? data) data]
    [(hash? data) (list data)]
    [else (error 'load-entries "unrecognized CSL JSON shape in ~a" path)]))

(define (str-ref e key)
  (define v (hash-ref e key #f))
  (and (string? v) (non-empty-string? (string-trim v)) v))

(define (entry-id e) (hash-ref e 'id #f))
(define (entry-type e) (or (hash-ref e 'type #f) "unknown"))
(define (entry-title e) (str-ref e 'title))
(define (entry-container-title e) (str-ref e 'container-title))
(define (entry-publisher e) (str-ref e 'publisher))
(define (entry-volume e) (str-ref e 'volume))
(define (entry-issue e) (str-ref e 'issue))
(define (entry-page e) (str-ref e 'page))
(define (entry-doi e) (str-ref e 'DOI))
(define (entry-url e) (str-ref e 'URL))

(define (person-list e key)
  (define v (hash-ref e key '()))
  (if (list? v) v '()))

(define (entry-authors e) (person-list e 'author))
(define (entry-editors e) (person-list e 'editor))

(define (person-literal? p) (and (hash-ref p 'literal #f) #t))

;; person-name : person-hash -> string
(define (person-name p)
  (cond
    [(hash-ref p 'literal #f) => values]
    [else
     (define family (hash-ref p 'family #f))
     (define given (hash-ref p 'given #f))
     (define given-str (and given (non-empty-string? (string-trim given)) given))
     (string-trim
      (string-append (or family "") (if given-str (string-append ", " given-str) "")))]))

;; A "date" here is either #f, or a list of 1-3 integers (year month day)
;; drawn from CSL's date-parts, or a single-element list (year) parsed out
;; of a "literal" approximate date string when date-parts is absent.
(define (parse-date-field v)
  (cond
    [(not (hash? v)) #f]
    [(hash-ref v 'date-parts #f)
     => (lambda (parts)
          (and (pair? parts) (pair? (car parts))
               (map (lambda (x) (if (number? x) (inexact->exact (truncate x)) x))
                    (car parts))))]
    [(hash-ref v 'literal #f)
     => (lambda (lit)
          (define m (regexp-match #px"[0-9]{4}" lit))
          (and m (list (string->number (car m)))))]
    [else #f]))

(define (entry-date e) (parse-date-field (hash-ref e 'issued #f)))
(define (entry-accessed e) (parse-date-field (hash-ref e 'accessed #f)))

(define (date-year d) (and d (pair? d) (list-ref d 0)))
(define (date-month d) (and d (>= (length d) 2) (list-ref d 1)))
(define (date-day d) (and d (>= (length d) 3) (list-ref d 2)))
