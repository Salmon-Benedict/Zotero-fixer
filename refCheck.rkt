#lang racket/base
;; Two ways to use this file:
;;
;; 1. As a script -- checks one file and writes the three result files
;;    (<stem>-flagged.txt, <stem>-fixed.txt, <stem>-zotero-import.json)
;;    into the SAME folder as the input, same as main.rkt:
;;
;;      racket refCheck.rkt "My Library-Zotero.json"
;;
;; 2. As a REPL entry point -- every function from every module (plus
;;    process-file itself) is in scope, and no argument is required:
;;
;;      racket -i -t refCheck.rkt
;;
;;    Example session:
;;      (define es (load-entries "My Library-Zotero.json"))
;;      (run-checks es)
;;      (define-values (fixed cs) (correct-entries es))
;;      (display (render-report es (run-checks es)))
;;      (process-file "My Library-Zotero.json")   ; same as the script above

(require racket/cmdline racket/path
         "csl-json.rkt"
         "apa-types.rkt"
         "issue.rkt"
         "text-utils.rkt"
         "checks.rkt"
         "corrections.rkt"
         "report.rkt"
         "fixed-report.rkt"
         "pipeline.rkt")

(provide (all-from-out "csl-json.rkt")
         (all-from-out "apa-types.rkt")
         (all-from-out "issue.rkt")
         (all-from-out "text-utils.rkt")
         (all-from-out "checks.rkt")
         (all-from-out "corrections.rkt")
         (all-from-out "report.rkt")
         (all-from-out "fixed-report.rkt")
         (all-from-out "pipeline.rkt"))

;; `racket -t refCheck.rkt` (REPL mode) also runs this submodule, so the
;; file argument has to be optional: given none, just fall through to
;; the REPL with nothing processed yet.
(module+ main
  (define files
    (command-line
     #:program "refCheck"
     #:args files
     files))
  (when (pair? files)
    (define input-file (car files))
    (define-values (flagged-text flagged-path fixed-path zotero-path) (process-file input-file))
    (display flagged-text)
    (printf "\nFlagged errors:  ~a\n" (path->string (path->complete-path flagged-path)))
    (printf "Correction log:  ~a\n" (path->string (path->complete-path fixed-path)))
    (printf "Zotero re-import: ~a\n" (path->string (path->complete-path zotero-path)))))
