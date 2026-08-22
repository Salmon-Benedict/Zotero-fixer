#lang racket/base
(require rackunit racket/list
         "../checks.rkt" "../issue.rkt")

;; Entries are built directly as the hasheq shape json's read-json would
;; produce, so these tests exercise checks.rkt without needing a file.

(define (author family given) (hasheq 'family family 'given given))
(define (author-org name) (hasheq 'literal name))
(define (issued . parts) (hasheq 'date-parts (list parts)))

(define good-book
  (hasheq 'id "b1" 'type "book"
          'title "The structure of scientific revolutions"
          'author (list (author "Kuhn" "T. S."))
          'issued (issued 1962)
          'publisher "University of Chicago Press"))

(define bad-book
  (hasheq 'id "b2" 'type "book"
          'title "The Structure Of Scientific Revolutions"
          'author (list (author "Kuhn" "Thomas Samuel"))
          'issued (issued 1962)))

(test-case "check-required-fields: complete book is clean"
  (check-equal? (check-required-fields good-book 1) '()))

(test-case "check-required-fields: missing publisher is an error"
  (define issues (check-required-fields bad-book 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-severity (car issues)) 'error)
  (check-eq? (issue-category (car issues)) 'missing-required))

(test-case "check-title-case: flags a title-cased title"
  (define issues (check-title-case bad-book 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'title-case))

(test-case "check-title-case: sentence-case title is clean"
  (check-equal? (check-title-case good-book 1) '()))

(define good-journal
  (hasheq 'id "j1" 'type "article-journal"
          'title "Sentence case titles are easy to spot"
          'container-title "Journal of Automation Studies"
          'author (list (author "Smith" "J. M."))
          'issued (issued 2020)
          'volume "12" 'issue "3" 'page "45-67"
          'DOI "https://doi.org/10.1000/xyz"))

(define bad-journal
  (hasheq 'id "j2" 'type "article-journal"
          'title "sentence case titles are easy to spot"
          'container-title "journal of automation studies"
          'author (list (author "Smith" "Jane Marie"))
          'issued (issued 2020)
          'volume "12" 'page "45-67"
          'DOI "10.1000/xyz"))

(test-case "check-container-case: flags a sentence-cased container"
  (define issues (check-container-case bad-journal 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'container-case))

(test-case "check-container-case: title-case container is clean"
  (check-equal? (check-container-case good-journal 1) '()))

(test-case "check-doi: flags a bare DOI"
  (define issues (check-doi bad-journal 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'doi-format))

(test-case "check-doi: full-URL DOI is clean"
  (check-equal? (check-doi good-journal 1) '()))

(test-case "check-journal-fields: flags volume without issue"
  (define issues (check-journal-fields bad-journal 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'journal-fields))

(test-case "check-journal-fields: volume+issue together is clean"
  (check-equal? (check-journal-fields good-journal 1) '()))

(test-case "check-author-initials: flags a full given name"
  (define issues (check-author-initials bad-journal 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'author-initials)
  (check-eq? (issue-severity (car issues)) 'note)
  (check-true (regexp-match? #rx"J\\. M\\." (issue-message (car issues)))))

(test-case "check-author-initials: already-initialized given name is clean"
  (check-equal? (check-author-initials good-journal 1) '()))

(define org-webpage-yearonly
  (hasheq 'id "w1" 'type "webpage"
          'title "Climate facts"
          'author (list (author-org "World Health Organization"))
          'issued (issued 2019)
          'URL "https://example.com/climate"))

(define org-webpage-fulldate
  (hasheq 'id "w2" 'type "webpage"
          'title "Climate facts"
          'author (list (author-org "World Health Organization"))
          'issued (issued 2019 6 1)
          'URL "https://example.com/climate"))

(test-case "check-date: warns when only a year is present for a webpage"
  (define issues (check-date org-webpage-yearonly 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'date-incomplete))

(test-case "check-date: full date on a webpage is clean"
  (check-equal? (check-date org-webpage-fulldate 1) '()))

(test-case "check-date: year-only is fine for a book (not a periodical/webpage)"
  (check-equal? (check-date good-book 1) '()))

(test-case "check-author-initials: literal (organization) authors are skipped"
  (check-equal? (check-author-initials org-webpage-yearonly 1) '()))

(test-case "check-author-initials: initials are always capitalized, even for lowercase-looking words"
  (define entry
    (hasheq 'id "org1" 'type "post-weblog"
            'title "Grand challenges for social work"
            'author (list (author "Work" "Grand Challenges for Social"))
            'issued (issued 2024)))
  (define issues (check-author-initials entry 1))
  (check-equal? (length issues) 1)
  (check-true (regexp-match? #rx"G\\. C\\. F\\. S\\." (issue-message (car issues)))))

(test-case "check-required-fields: webpage missing author/date is clean (APA allows n.d./title-as-author)"
  (define anon-webpage
    (hasheq 'id "w3" 'type "webpage"
            'title "Some organization"
            'container-title "Example Site"
            'URL "https://example.com"))
  (check-equal? (check-required-fields anon-webpage 1) '()))

(test-case "check-required-fields: post-weblog missing author/date is clean too"
  (define anon-post
    (hasheq 'id "p1" 'type "post-weblog"
            'title "A post with no byline"
            'container-title "Example Blog"
            'URL "https://example.com/post"))
  (check-equal? (check-required-fields anon-post 1) '()))

(test-case "check-suspicious-authors: flags an email address left in family"
  (define entry
    (hasheq 'id "s1" 'type "webpage"
            'title "Home page"
            'author (list (author "webmaster@example.gov" ""))))
  (define issues (check-suspicious-authors entry 1))
  (check-equal? (length issues) 1)
  (check-eq? (issue-category (car issues)) 'suspicious-author))

(test-case "check-suspicious-authors: flags a literal page-label artifact"
  (define entry
    (hasheq 'id "s2" 'type "webpage"
            'title "Contact us"
            'author (list (author-org "Phone:"))))
  (define issues (check-suspicious-authors entry 1))
  (check-equal? (length issues) 1))

(test-case "check-suspicious-authors: flags a given name with too many words"
  (define entry
    (hasheq 'id "s3" 'type "post-weblog"
            'title "Grand challenges for social work"
            'author (list (author "Work" "Grand Challenges for Social"))))
  (define issues (check-suspicious-authors entry 1))
  (check-equal? (length issues) 1))

(test-case "check-suspicious-authors: a normal name is clean"
  (check-equal? (check-suspicious-authors good-journal 1) '())
  (check-equal? (check-suspicious-authors org-webpage-yearonly 1) '()))

(test-case "run-checks: aggregates across all entries and all checks"
  (define issues (run-checks (list good-book bad-book good-journal bad-journal)))
  (check-true (> (length issues) 0))
  (check-true (andmap (lambda (i) (issue? i)) issues))
  ;; good-book and good-journal should contribute nothing
  (check-true (andmap (lambda (i) (and (member (issue-index i) '(2 4)) #t)) issues)))
