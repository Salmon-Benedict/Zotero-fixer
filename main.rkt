#lang racket/base
;; CLI entry point.
;;   racket main.rkt <export.json>
;; Reads a Zotero CSL JSON export, checks every entry against APA 7th
;; edition, and writes three files next to the input (plus a stdout
;; summary):
;;   <stem>-flagged.txt         every problem found, with an APA rule cite
;;   <stem>-fixed.txt           a before/after log of what was auto-corrected
;;   <stem>-zotero-import.json  the corrected entries, ready to re-import

(require racket/cmdline racket/path "pipeline.rkt")

(define input-file
  (command-line
   #:program "zotero-fixer"
   #:args (input)
   input))

(module+ main
  (define-values (flagged-text flagged-path fixed-path zotero-path) (process-file input-file))
  (display flagged-text)
  (printf "\nFlagged errors:  ~a\n" (path->string (path->complete-path flagged-path)))
  (printf "Correction log:  ~a\n" (path->string (path->complete-path fixed-path)))
  (printf "Zotero re-import: ~a\n" (path->string (path->complete-path zotero-path))))
