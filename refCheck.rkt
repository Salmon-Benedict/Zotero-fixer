#lang racket/base
;; Two ways to use this file:
;;
;; 1. As a script -- checks one file and writes the three result files
;;    (<stem>-flagged.txt, <stem>-fixed.txt, <stem>-zotero-import.json)
;;    into the SAME folder as the input, same as main.rkt:
;;
;;      racket refCheck.rkt "My Library-Zotero.json"
;;
;;    Leave off the filename and it asks for one interactively:
;;
;;      racket refCheck.rkt
;;      Enter the Zotero export file to check (or press Enter to skip): _
;;
;; 2. As a REPL entry point -- every function from every module (plus
;;    process-file itself) is in scope. `racket -t` also runs the
;;    interactive prompt above; press Enter with nothing typed to skip
;;    straight to the REPL:
;;
;;      racket -i -t refCheck.rkt
;;
;;    Example session:
;;      (define es (load-entries "My Library-Zotero.json"))
;;      (run-checks es)
;;      (define-values (fixed cs) (correct-entries es))
;;      (display (render-report es (run-checks es)))
;;      (process-file "My Library-Zotero.json")   ; same as the script above
;;
;; Run it from the project root (or point the -t path at wherever
;; refCheck.rkt actually lives) -- Racket looks for it relative to your
;; terminal's current directory, same as any other file argument.

(require racket/cmdline racket/path racket/string
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

;; Repeatedly asks for a filename until given a real file, an empty
;; line (skip), or EOF (Ctrl+D, also treated as skip).
(define (prompt-for-file)
  (let loop ()
    (display "Enter the Zotero export file to check (or press Enter to skip): ")
    (flush-output)
    (define line (read-line (current-input-port) 'any))
    (cond
      [(eof-object? line) #f]
      [(zero? (string-length (string-trim line))) #f]
      [(file-exists? (string-trim line)) (string-trim line)]
      [else
       (printf "Can't find \"~a\" -- check the path and try again.\n" (string-trim line))
       (loop)])))

;; `racket -t refCheck.rkt` (REPL mode) also runs this submodule, so a
;; missing file argument prompts interactively rather than erroring;
;; skipping the prompt (blank line/EOF) just falls through to the REPL.
(module+ main
  (define files
    (command-line
     #:program "refCheck"
     #:args files
     files))
  (define input-file (if (pair? files) (car files) (prompt-for-file)))
  (when input-file
    (define-values (flagged-text flagged-path fixed-path zotero-path) (process-file input-file))
    (display flagged-text)
    (printf "\nFlagged errors:  ~a\n" (path->string (path->complete-path flagged-path)))
    (printf "Correction log:  ~a\n" (path->string (path->complete-path fixed-path)))
    (printf "Zotero re-import: ~a\n" (path->string (path->complete-path zotero-path)))))
