#lang racket/base
;; CLI entry point.
;;   racket main.rkt <export.json>
;; Reads a Zotero CSL JSON export, checks every entry against APA 7th
;; edition, and writes three files next to the input (plus a stdout
;; summary):
;;   <stem>-flagged.txt       every problem found, with an APA rule cite
;;   <stem>-fixed.txt         a before/after log of what was auto-corrected
;;   <stem>-zotero-import.json  the corrected entries, ready to re-import

(require racket/cmdline racket/path json
         "csl-json.rkt" "checks.rkt" "report.rkt" "corrections.rkt" "fixed-report.rkt")

(define input-file
  (command-line
   #:program "zotero-fixer"
   #:args (input)
   input))

(define (stem-of in)
  (define-values (base name must-be-dir?) (split-path in))
  (define name-str (path->string name))
  (values base
          (if (regexp-match? #rx"\\." name-str)
              (regexp-replace #rx"\\.[^.]*$" name-str "")
              name-str)))

(define (sibling-path base stem suffix)
  (define new-name (string-append stem suffix))
  (if (path? base) (build-path base new-name) new-name))

(define (write-text-file dest text)
  (call-with-output-file dest #:exists 'replace (lambda (p) (display text p))))

(define (write-json-array dest entries)
  (call-with-output-file dest #:exists 'replace
    (lambda (out)
      (write-string "[\n" out)
      (for ([e entries] [i (in-naturals)])
        (write-string "  " out)
        (write-json e out)
        (when (< (add1 i) (length entries)) (write-string "," out))
        (write-string "\n" out))
      (write-string "]\n" out))))

(define (file-name-from-path p)
  (define-values (base name must-be-dir?) (split-path p))
  name)

(module+ main
  (define-values (base stem) (stem-of input-file))
  (define flagged-path (sibling-path base stem "-flagged.txt"))
  (define fixed-path (sibling-path base stem "-fixed.txt"))
  (define zotero-path (sibling-path base stem "-zotero-import.json"))

  (define entries (load-entries input-file))
  (define issues (run-checks entries))
  (define flagged-text (render-report entries issues))
  (write-text-file flagged-path flagged-text)

  (define-values (corrected-entries corrections-per-entry) (correct-entries entries))
  (define fixed-text (render-fixed-report entries corrections-per-entry (path->string (file-name-from-path flagged-path))))
  (write-text-file fixed-path fixed-text)
  (write-json-array zotero-path corrected-entries)

  (display flagged-text)
  (printf "\nFlagged errors:  ~a\n" (path->string (path->complete-path flagged-path)))
  (printf "Correction log:  ~a\n" (path->string (path->complete-path fixed-path)))
  (printf "Zotero re-import: ~a\n" (path->string (path->complete-path zotero-path))))
