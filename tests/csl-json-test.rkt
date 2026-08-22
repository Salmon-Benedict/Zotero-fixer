#lang racket/base
(require rackunit racket/port racket/string racket/file
         "../csl-json.rkt")

(define sample-json
  #<<JSON
[
  {
    "id": "smith2020",
    "type": "article-journal",
    "title": "the rise of the machines",
    "container-title": "Journal of Automation Studies",
    "author": [{"family": "Smith", "given": "Jane Marie"}],
    "editor": [],
    "issued": {"date-parts": [[2020, 5, 3]]},
    "volume": "12",
    "issue": "3",
    "page": "45-67",
    "DOI": "10.1000/xyz123"
  },
  {
    "id": "who2019",
    "type": "webpage",
    "title": "Climate Facts",
    "author": [{"literal": "World Health Organization"}],
    "issued": {"literal": "circa 2019"},
    "URL": "https://example.com/climate"
  }
]
JSON
  )

(define tmp (make-temporary-file "zotero-fixer-test~a.json"))
(call-with-output-file tmp #:exists 'truncate (lambda (p) (display sample-json p)))

(define entries (load-entries tmp))
(delete-file tmp)

(test-case "load-entries reads a JSON array of entries"
  (check-equal? (length entries) 2))

(define e1 (car entries))
(define e2 (cadr entries))

(test-case "basic scalar accessors"
  (check-equal? (entry-id e1) "smith2020")
  (check-equal? (entry-type e1) "article-journal")
  (check-equal? (entry-title e1) "the rise of the machines")
  (check-equal? (entry-container-title e1) "Journal of Automation Studies")
  (check-equal? (entry-volume e1) "12")
  (check-equal? (entry-issue e1) "3")
  (check-equal? (entry-page e1) "45-67")
  (check-equal? (entry-doi e1) "10.1000/xyz123"))

(test-case "author/editor lists"
  (check-equal? (length (entry-authors e1)) 1)
  (check-equal? (person-name (car (entry-authors e1))) "Smith, Jane Marie")
  (check-equal? (entry-editors e1) '())
  (check-true (person-literal? (car (entry-authors e2))))
  (check-equal? (person-name (car (entry-authors e2))) "World Health Organization"))

(test-case "date parsing from date-parts"
  (define d (entry-date e1))
  (check-equal? (date-year d) 2020)
  (check-equal? (date-month d) 5)
  (check-equal? (date-day d) 3))

(test-case "date parsing from literal approximate date"
  (define d (entry-date e2))
  (check-equal? (date-year d) 2019)
  (check-false (date-month d)))

(test-case "missing fields come back as #f, not errors"
  (check-false (entry-container-title e2))
  (check-false (entry-doi e2))
  (check-equal? (entry-url e2) "https://example.com/climate"))

(test-case "person-name: an empty-string given name doesn't leave a trailing comma"
  (check-equal? (person-name (hasheq 'family "SDCCE" 'given "")) "SDCCE"))
