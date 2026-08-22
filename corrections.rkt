#lang racket/base
;; Applies only the algorithmically-safe fixes: capitalization the
;; heuristics in text-utils.rkt are confident about, and DOI formatting.
;; Never invents data for a missing field, and never touches an author
;; entry -- suspicious ones (see check-suspicious-authors) need a human,
;; and legitimate given names should stay full names in the underlying
;; data even though APA renders them as initials (that's a display-time
;; rule, not a correction to the source record).

(require racket/string
         "csl-json.rkt" "text-utils.rkt")

(provide (struct-out correction) correct-entry correct-entries)

;; field   : symbol naming the corrected CSL key ('title 'container-title 'DOI)
;; label   : human-readable name of that field, for the correction log
;; before  : original value
;; after   : corrected value
(struct correction (field label before after) #:transparent)

(define (correct-entry e)
  (define corrections '())
  (define (apply-fix! key label new-value)
    (set! corrections (cons (correction key label (hash-ref e key) new-value) corrections))
    (set! e (hash-set e key new-value)))

  (define title (entry-title e))
  (when (and title (looks-title-case? title))
    (apply-fix! 'title "title" (to-sentence-case title)))

  (define container (entry-container-title e))
  (when (and container (looks-sentence-case? container))
    (apply-fix! 'container-title "container title" (to-title-case container)))

  (define doi (entry-doi e))
  (when (and doi (not (regexp-match? #px"^https?://" doi)))
    (apply-fix! 'DOI "DOI" (string-append "https://doi.org/" doi)))

  (values e (reverse corrections)))

;; correct-entries : (listof entry) -> (values (listof entry) (listof (listof correction)))
;; The second value is parallel to the input list -- one (possibly empty)
;; list of corrections per entry, in the same order.
(define (correct-entries entries)
  (for/fold ([es '()] [cs '()] #:result (values (reverse es) (reverse cs)))
            ([e entries])
    (define-values (e* c*) (correct-entry e))
    (values (cons e* es) (cons c* cs))))
