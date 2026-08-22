#lang racket/base
;; Issue struct: one flagged problem on one bibliography entry, plus the
;; table mapping check categories to the APA 7th-edition section that
;; governs the fix.

(provide (struct-out issue)
         rule-for
         severity-rank)

;; index      : 1-based position of the entry in the input file
;; id         : the entry's CSL "id" (or #f)
;; type       : CSL source type string, e.g. "article-journal"
;; severity   : 'error | 'warning | 'note
;; category   : symbol identifying which check produced this (see rule-table)
;; message    : human-readable description of the specific problem
(struct issue (index id type severity category message) #:transparent)

(define (severity-rank sev)
  (case sev
    [(error) 0]
    [(warning) 1]
    [(note) 2]
    [else 3]))

(define rule-table
  (hash
   'missing-required
   "APA 7th ed., Ch. 9 (Sections 9.7-9.12) -- Reference Elements: Author, Date, Title, Source"

   'missing-recommended
   "APA 7th ed., Ch. 9 (Sections 9.7-9.12) -- Reference Elements: Author, Date, Title, Source"

   'title-case
   "APA 7th ed., Section 6.17 -- Capitalization: use sentence case for a work's own title"

   'container-case
   "APA 7th ed., Section 6.17 -- Capitalization: use title case for periodical/book (container) titles"

   'date-incomplete
   "APA 7th ed., Section 9.8 -- Date element"

   'doi-format
   "APA 7th ed., Sections 9.16-9.17 -- Format DOIs as a full https://doi.org/... URL"

   'journal-fields
   "APA 7th ed., Ch. 10.1 -- Periodical reference examples (volume, issue, pages)"

   'author-initials
   "APA 7th ed., Section 9.7 -- Author element: give initials, not full given names"

   'suspicious-author
   "APA 7th ed., Section 9.7 -- Author element: use the actual creator's name, not scraped page metadata"))

(define (rule-for category)
  (hash-ref rule-table category "APA 7th ed."))
