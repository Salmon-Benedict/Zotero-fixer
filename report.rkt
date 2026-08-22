#lang racket/base
;; Renders a list of issues (see issue.rkt) plus the parsed entries into a
;; readable plain-text report.

(require racket/string racket/list racket/port
         "csl-json.rkt" "issue.rkt")

(provide render-report)

(define (entry-label e)
  (define authors (entry-authors e))
  (define who (if (pair? authors) (person-name (car authors)) "Unknown author"))
  (define year (date-year (entry-date e)))
  (format "~a~a -- ~a [~a]"
          who
          (if year (format " (~a)" year) "")
          (or (entry-title e) "Untitled")
          (entry-type e)))

(define (severity-label sev)
  (case sev
    [(error) "ERROR  "]
    [(warning) "WARNING"]
    [(note) "NOTE   "]
    [else "?      "]))

(define (plural n singular plural-form)
  (format "~a ~a" n (if (= n 1) singular plural-form)))

(define (render-report entries issues)
  (define n (length entries))
  (define by-index (make-hash))
  (for ([iss issues])
    (hash-update! by-index (issue-index iss) (lambda (l) (cons iss l)) '()))
  (define n-err (count (lambda (i) (eq? (issue-severity i) 'error)) issues))
  (define n-warn (count (lambda (i) (eq? (issue-severity i) 'warning)) issues))
  (define n-note (count (lambda (i) (eq? (issue-severity i) 'note)) issues))
  (define n-clean (for/sum ([idx (in-range 1 (add1 n))])
                     (if (hash-has-key? by-index idx) 0 1)))
  (with-output-to-string
    (lambda ()
      (printf "Zotero-fixer APA Report\n")
      (printf "Checked ~a -- ~a, ~a, ~a, ~a\n\n"
              (plural n "entry" "entries")
              (plural n-err "error" "errors")
              (plural n-warn "warning" "warnings")
              (plural n-note "note" "notes")
              (plural n-clean "clean entry" "clean entries"))
      (for ([e entries] [idx (in-naturals 1)])
        (define here (reverse (hash-ref by-index idx '())))
        (define sorted (sort here < #:key (lambda (i) (severity-rank (issue-severity i)))))
        (printf "[~a] ~a\n" idx (entry-label e))
        (cond
          [(null? sorted) (printf "    clean\n")]
          [else
           (for ([iss sorted])
             (printf "    ~a  ~a\n" (severity-label (issue-severity iss)) (issue-message iss))
             (printf "             ~a\n" (rule-for (issue-category iss))))])
        (printf "\n")))))
