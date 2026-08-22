#lang racket/base
(require rackunit racket/list "../corrections.rkt")

(define bad-journal
  (hasheq 'id "j2" 'type "article-journal"
          'title "The Rise Of The Machines In Manufacturing"
          'container-title "journal of automation studies"
          'author (list (hasheq 'family "Smith" 'given "Jane Marie"))
          'issued (hasheq 'date-parts (list (list 2020)))
          'volume "12" 'page "45-67"
          'DOI "10.1000/xyz"))

(define good-journal
  (hasheq 'id "j1" 'type "article-journal"
          'title "Sentence case titles are easy to spot"
          'container-title "Journal of Automation Studies"
          'author (list (hasheq 'family "Smith" 'given "J. M."))
          'issued (hasheq 'date-parts (list (list 2020)))
          'volume "12" 'issue "3" 'page "45-67"
          'DOI "https://doi.org/10.1000/xyz"))

(test-case "correct-entry: fixes container and DOI together"
  (define-values (fixed cs) (correct-entry bad-journal))
  (check-equal? (length cs) 2)
  (check-equal? (hash-ref fixed 'container-title) "Journal of Automation Studies")
  (check-equal? (hash-ref fixed 'DOI) "https://doi.org/10.1000/xyz"))

(test-case "correct-entry: never touches a work's own title, even when it looks title-cased"
  ;; Sentence-casing can silently lowercase real proper nouns (e.g. "San
  ;; Diego" -> "san diego") with no way to tell afterward -- too risky to
  ;; auto-apply to data that can be re-imported into Zotero. Stays a
  ;; flagged-only issue (check-title-case in checks.rkt).
  (define-values (fixed cs) (correct-entry bad-journal))
  (check-equal? (hash-ref fixed 'title) (hash-ref bad-journal 'title))
  (check-false (memf (lambda (c) (eq? (correction-field c) 'title)) cs)))

(test-case "correct-entry: an already-clean entry gets zero corrections and is unchanged"
  (define-values (fixed cs) (correct-entry good-journal))
  (check-equal? cs '())
  (check-equal? fixed good-journal))

(test-case "correct-entry: never touches author fields"
  (define-values (fixed cs) (correct-entry bad-journal))
  (check-equal? (hash-ref fixed 'author) (hash-ref bad-journal 'author)))

(test-case "correct-entry: leaves fields with no fix untouched (missing publisher stays absent)"
  (define-values (fixed cs) (correct-entry bad-journal))
  (check-false (hash-ref fixed 'publisher #f)))

(test-case "correct-entries: parallel lists, one corrections-list per entry"
  (define-values (all-fixed all-cs) (correct-entries (list good-journal bad-journal)))
  (check-equal? (length all-fixed) 2)
  (check-equal? (length all-cs) 2)
  (check-equal? (car all-cs) '())
  (check-equal? (length (cadr all-cs)) 2))
