#lang racket/base
;; REPL entry point -- not the CLI (see main.rkt for that). Start a REPL
;; on this file and every function from every module is in scope:
;;
;;   racket -i -t refCheck.rkt
;;
;; or, from an already-running REPL:
;;
;;   (require "refCheck.rkt")
;;
;; Example session:
;;   (define es (load-entries "My Library-Zotero.json"))
;;   (run-checks es)
;;   (define-values (fixed cs) (correct-entries es))
;;   (display (render-report es (run-checks es)))

(require "csl-json.rkt"
         "apa-types.rkt"
         "issue.rkt"
         "text-utils.rkt"
         "checks.rkt"
         "corrections.rkt"
         "report.rkt"
         "fixed-report.rkt")

(provide (all-from-out "csl-json.rkt")
         (all-from-out "apa-types.rkt")
         (all-from-out "issue.rkt")
         (all-from-out "text-utils.rkt")
         (all-from-out "checks.rkt")
         (all-from-out "corrections.rkt")
         (all-from-out "report.rkt")
         (all-from-out "fixed-report.rkt"))
