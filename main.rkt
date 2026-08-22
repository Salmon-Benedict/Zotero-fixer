#lang racket/base
;; CLI entry point.
;;   racket main.rkt <export.json> [--out report.txt]
;; Reads a Zotero CSL JSON export, checks every entry against APA 7th
;; edition, and writes the report to stdout and to a file.

(require racket/cmdline racket/path
         "csl-json.rkt" "checks.rkt" "report.rkt")

(define out-path (make-parameter #f))

(define input-file
  (command-line
   #:program "zotero-fixer"
   #:once-each
   [("--out") path "Write the report to this file instead of the default" (out-path path)]
   #:args (input)
   input))

(define (default-out-path in)
  (define-values (base name must-be-dir?) (split-path in))
  (define name-str (path->string name))
  (define stem (if (regexp-match? #rx"\\." name-str)
                    (regexp-replace #rx"\\.[^.]*$" name-str "")
                    name-str))
  (define new-name (string-append stem "-report.txt"))
  (if (path? base) (build-path base new-name) new-name))

(module+ main
  (define entries (load-entries input-file))
  (define issues (run-checks entries))
  (define text (render-report entries issues))
  (display text)
  (define dest (or (out-path) (default-out-path input-file)))
  (call-with-output-file dest #:exists 'replace (lambda (p) (display text p)))
  (printf "\nReport written to ~a\n" (path->string (path->complete-path dest))))
