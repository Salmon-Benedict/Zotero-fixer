#lang racket/base
;; Renders a before/after correction log: what corrections.rkt actually
;; changed, per entry. Entries with nothing algorithmically fixable still
;; get a line, so the numbering lines up 1:1 with the flagged report.

(require racket/string racket/list racket/port
         "csl-json.rkt" "corrections.rkt")

(provide render-fixed-report)

(define (entry-label e)
  (define authors (entry-authors e))
  (define who (if (pair? authors) (person-name (car authors)) "Unknown author"))
  (define year (date-year (entry-date e)))
  (format "~a~a -- ~a [~a]"
          who
          (if year (format " (~a)" year) "")
          (or (entry-title e) "Untitled")
          (entry-type e)))

(define (plural n singular plural-form)
  (format "~a ~a" n (if (= n 1) singular plural-form)))

;; original-entries and corrections-per-entry must be parallel (same
;; order/length) -- as produced by correct-entries.
(define (render-fixed-report original-entries corrections-per-entry flagged-filename)
  (define n (length original-entries))
  (define n-corrections (for/sum ([cs corrections-per-entry]) (length cs)))
  (define n-touched (for/sum ([cs corrections-per-entry]) (if (null? cs) 0 1)))
  (with-output-to-string
    (lambda ()
      (printf "Zotero-fixer APA Correction Log\n")
      (printf "Applied ~a across ~a (of ~a total).\n"
              (plural n-corrections "correction" "corrections")
              (plural n-touched "entry" "entries")
              n)
      (printf "Only capitalization and DOI-URL formatting are corrected automatically.\n")
      (printf "Missing data, incomplete dates, and suspicious authors still need manual\n")
      (printf "attention -- see ~a.\n\n" flagged-filename)
      (for ([e original-entries] [cs corrections-per-entry] [idx (in-naturals 1)])
        (printf "[~a] ~a\n" idx (entry-label e))
        (cond
          [(null? cs) (printf "    no automatic corrections available\n")]
          [else
           (for ([c cs])
             (printf "    FIXED ~a:\n" (correction-label c))
             (printf "        was: ~a\n" (correction-before c))
             (printf "        now: ~a\n" (correction-after c)))])
        (printf "\n")))))
