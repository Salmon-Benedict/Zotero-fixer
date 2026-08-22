#lang racket/base
;; APA 7th-edition checks over a single CSL JSON entry (a hasheq, see
;; csl-json.rkt). Every check-* function has signature
;;   (entry natural-number -> (listof issue))
;; where the number is the entry's 1-based position in the input file.

(require racket/string racket/list
         "csl-json.rkt" "apa-types.rkt" "issue.rkt" "text-utils.rkt")

(provide run-checks
         check-required-fields check-title-case check-container-case
         check-date check-doi check-journal-fields check-author-initials
         check-suspicious-authors)

(define (make-issue e idx severity category message)
  (issue idx (entry-id e) (entry-type e) severity category message))

;; ---------------------------------------------------------------------
;; Completeness

(define (field-present? e key)
  (case key
    [(author) (pair? (entry-authors e))]
    [(editor) (pair? (entry-editors e))]
    [(date) (and (entry-date e) (date-year (entry-date e)) #t)]
    [(title) (and (entry-title e) #t)]
    [(container-title) (and (entry-container-title e) #t)]
    [(publisher) (and (entry-publisher e) #t)]
    [(volume) (and (entry-volume e) #t)]
    [(issue) (and (entry-issue e) #t)]
    [(page) (and (entry-page e) #t)]
    [(url-or-doi) (and (or (entry-url e) (entry-doi e)) #t)]
    [else #f]))

(define (check-required-fields e idx)
  (define spec (type-spec-for (entry-type e)))
  (append
   (for/list ([key (type-spec-required spec)] #:unless (field-present? e key))
     (make-issue e idx 'error 'missing-required
                 (format "Missing required element: ~a" (field-label key))))
   (for/list ([key (type-spec-recommended spec)] #:unless (field-present? e key))
     (make-issue e idx 'warning 'missing-recommended
                 (format "Missing recommended element: ~a" (field-label key))))))

;; ---------------------------------------------------------------------
;; Capitalization
;;
;; APA sentence-case titles capitalize only the first word, the first
;; word after a colon/dash, and proper nouns. APA title-case container
;; names (journals, books-as-containers) capitalize most content words.
;; looks-title-case?/looks-sentence-case? (text-utils.rkt) are heuristic
;; detectors -- good enough to flag for human review, not a silent
;; auto-fixer; see corrections.rkt for the actual rewriting.

(define (check-title-case e idx)
  (define t (entry-title e))
  (if (and t (looks-title-case? t))
      (list (make-issue e idx 'warning 'title-case
             (format "Title \"~a\" looks title-cased; APA uses sentence case (capitalize only the first word, the first word after a colon/dash, and proper nouns)." t)))
      '()))

(define (check-container-case e idx)
  (define c (entry-container-title e))
  (if (and c (looks-sentence-case? c))
      (list (make-issue e idx 'warning 'container-case
             (format "Container title \"~a\" looks sentence-cased; APA keeps periodical and book (container) titles in title case." c)))
      '()))

;; ---------------------------------------------------------------------
;; Dates

(define (check-date e idx)
  (define type (entry-type e))
  (define d (entry-date e))
  (if (and d (member type '("article-magazine" "article-newspaper" "webpage"))
           (not (date-month d)))
      (list (make-issue e idx 'warning 'date-incomplete
             "Only a year is present; APA wants (Year, Month Day) for this source type when the fuller date is available."))
      '()))

;; ---------------------------------------------------------------------
;; DOI / URL

(define (check-doi e idx)
  (define doi (entry-doi e))
  (if (and doi (not (regexp-match? #px"^https?://" doi)))
      (list (make-issue e idx 'warning 'doi-format
             (format "DOI \"~a\" should be formatted as a full URL, e.g. https://doi.org/~a" doi doi)))
      '()))

;; ---------------------------------------------------------------------
;; Journal-specific fields

(define (check-journal-fields e idx)
  (if (and (string=? (entry-type e) "article-journal")
           (entry-volume e)
           (not (entry-issue e)))
      (list (make-issue e idx 'warning 'journal-fields
             "Volume is present but issue number is missing; include both when the journal paginates by issue."))
      '()))

;; ---------------------------------------------------------------------
;; Author name note (informational, not an error in the source data)

(define initials-pattern #px"^([A-Z]\\.[- ]?)+$")
(define (looks-like-initials? given) (regexp-match? initials-pattern (string-trim given)))

(define (initialize given)
  (string-join
   (for/list ([w (split-words given)] #:when (> (string-length (strip-punct w)) 0))
     ;; Initials are always capitalized, even for a word like "for" that
     ;; check-title-case would otherwise treat as a lowercase stopword.
     (string-append (string-upcase (substring (strip-punct w) 0 1)) "."))
   " "))

;; One combined note per entry rather than one per author -- a paper with
;; a dozen co-authors would otherwise produce a dozen near-identical
;; lines, each repeating the same rule reference.
(define (check-author-initials e idx)
  (define conversions
    (for*/list ([p (entry-authors e)]
                #:unless (person-literal? p)
                [given (in-value (hash-ref p 'given #f))]
                #:when (and given (non-empty-string? (string-trim given)))
                #:unless (looks-like-initials? given))
      (format "\"~a\" -> \"~a\"" given (initialize given))))
  (if (null? conversions)
      '()
      (list (make-issue e idx 'note 'author-initials
             (format "~a author given name~a will need to be reduced to initials in the rendered APA citation: ~a."
                     (length conversions) (if (= (length conversions) 1) "" "s")
                     (string-join conversions ", "))))))

;; ---------------------------------------------------------------------
;; Suspicious authors: Zotero's web-page scraper sometimes captures
;; something that isn't a person or organization at all -- an email
;; address left in a "Contact" field, a stray page label like "Phone:",
;; or a chunk of the page title mis-split into family/given. These are
;; real data problems (not formatting nits), so they're worth a warning.

(define (looks-like-email? s) (regexp-match? #px"^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$" (string-trim s)))
(define (looks-like-label? s) (regexp-match? #px":$" (string-trim s)))
(define (too-many-words? s [max-words 3]) (> (length (split-words s)) max-words))

(define (author-suspicious? p)
  (cond
    [(person-literal? p)
     (define lit (hash-ref p 'literal ""))
     (or (looks-like-email? lit) (looks-like-label? lit))]
    [else
     (define family (hash-ref p 'family #f))
     (define given (hash-ref p 'given #f))
     (or (and family (looks-like-email? family))
         (and given (looks-like-email? given))
         (and given (non-empty-string? (string-trim given)) (too-many-words? given))
         (and family (too-many-words? family)))]))

(define (check-suspicious-authors e idx)
  (for/list ([p (entry-authors e)] #:when (author-suspicious? p))
    (make-issue e idx 'warning 'suspicious-author
     (format "Author entry \"~a\" looks like a scraping artifact (an email address, a page label, or a mis-split name) rather than a real author; verify it in Zotero."
             (person-name p)))))

;; ---------------------------------------------------------------------

(define (run-checks entries)
  (for*/list ([(e idx0) (in-indexed entries)]
              [idx (in-value (add1 idx0))]
              [check (list check-required-fields check-title-case check-container-case
                           check-date check-doi check-journal-fields check-author-initials
                           check-suspicious-authors)]
              [iss (check e idx)])
    iss))
