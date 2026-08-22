#lang racket/base
;; The actual "check one export, write three files next to it" pipeline,
;; shared by main.rkt and refCheck.rkt so the CLI and the REPL entry
;; point can't drift out of sync.

(require racket/path json
         "csl-json.rkt" "checks.rkt" "report.rkt" "corrections.rkt" "fixed-report.rkt")

(provide process-file)

(define (stem-of in)
  (define-values (base name must-be-dir?) (split-path in))
  (define name-str (path->string name))
  (values base
          (if (regexp-match? #rx"\\." name-str)
              (regexp-replace #rx"\\.[^.]*$" name-str "")
              name-str)))

;; Always a sibling of the input file -- same directory, name derived
;; from its stem -- regardless of what directory the tool is run from.
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

;; process-file : path-string -> (values string path path path)
;; Reads input-file, writes <stem>-flagged.txt, <stem>-fixed.txt, and
;; <stem>-zotero-import.json into the same folder as input-file.
;; Returns the flagged report text (for printing) and the three paths.
(define (process-file input-file)
  (define-values (base stem) (stem-of input-file))
  (define flagged-path (sibling-path base stem "-flagged.txt"))
  (define fixed-path (sibling-path base stem "-fixed.txt"))
  (define zotero-path (sibling-path base stem "-zotero-import.json"))

  (define entries (load-entries input-file))
  (define issues (run-checks entries))
  (define flagged-text (render-report entries issues))
  (write-text-file flagged-path flagged-text)

  (define-values (corrected-entries corrections-per-entry) (correct-entries entries))
  (define fixed-text (render-fixed-report entries corrections-per-entry
                                           (path->string (file-name-from-path flagged-path))))
  (write-text-file fixed-path fixed-text)
  (write-json-array zotero-path corrected-entries)

  (values flagged-text flagged-path fixed-path zotero-path))
