#lang racket/base
;; Word-level helpers shared by checks.rkt (detecting wrong capitalization)
;; and corrections.rkt (actually converting it). Single source of truth so
;; the two stay in agreement about what counts as a "small word."

(require racket/string racket/list)

(provide split-words strip-punct stopword? long-word? capitalized-word?
         all-caps-word? content-words
         looks-title-case? looks-sentence-case?
         to-sentence-case to-title-case)

;; Small connector words APA leaves lowercase in title case (unless
;; first/last word or right after a colon/dash), and that check-title-case
;; ignores as "not informative" when judging capitalization ratios.
(define stopwords
  '("a" "an" "the" "of" "in" "on" "and" "or" "but" "for" "nor" "to" "as"
    "at" "by" "is" "with" "from" "vs" "vs."))

(define (split-words s) (regexp-split #px"\\s+" (string-trim s)))
(define (strip-punct w) (regexp-replace* #px"^[^A-Za-z0-9]+|[^A-Za-z0-9]+$" w ""))
(define (stopword? w) (and (member (string-downcase (strip-punct w)) stopwords) #t))
(define (long-word? w) (> (string-length (strip-punct w)) 3))

(define (capitalized-word? w)
  (define w2 (strip-punct w))
  (and (> (string-length w2) 0) (char-upper-case? (string-ref w2 0))))

;; An acronym like "TCM" or "NASA" -- two or more letters, all uppercase.
;; Preserved as-is by to-sentence-case rather than blindly lowercased.
(define (all-caps-word? w)
  (define w2 (strip-punct w))
  (and (>= (string-length w2) 2)
       (for/and ([c (in-string w2)]) (or (not (char-alphabetic? c)) (char-upper-case? c)))))

(define (content-words words)
  (for/list ([w (if (pair? words) (cdr words) '())]
             #:unless (stopword? w)
             #:when (long-word? w))
    w))

;; ---------------------------------------------------------------------
;; Detection heuristics
;;
;; True proper-noun detection needs a dictionary we don't have, so these
;; look at words after the first one and flag when a large share of
;; "long, non-stopword" tokens are capitalized (title case) vs. lowercase
;; (sentence case). Good enough to flag for human review.

(define (looks-title-case? title)
  (define words (split-words title))
  (and (>= (length words) 3)
       (let* ([candidates (content-words words)]
              [offenders (filter capitalized-word? candidates)])
         (and (>= (length candidates) 2)
              (> (length offenders) (/ (length candidates) 2))))))

(define (looks-sentence-case? title)
  (define words (split-words title))
  (and (>= (length words) 2)
       (let* ([candidates (content-words words)]
              [lowered (filter (lambda (w) (not (capitalized-word? w))) candidates)])
         (and (>= (length candidates) 2)
              (> (length lowered) (/ (length candidates) 2))))))

;; ---------------------------------------------------------------------
;; Conversion
;;
;; These actually rewrite the string, so they're more conservative than
;; the detectors above need to be: only the first word of the title, the
;; first word after a colon/dash, and ALL-CAPS acronyms keep their
;; capitals in sentence case -- every other word is lowercased even if it
;; might be a proper noun. This is the same naive algorithm citation
;; managers (Zotero/EndNote included) use when auto-converting to APA
;; sentence case; a human still needs to re-capitalize real proper nouns
;; (e.g. country or person names) afterward. Title case is the safer
;; direction: it only capitalizes words that are already there, using a
;; standard small-word exception list.

(define (word-boundary-positions s)
  (regexp-match-positions* #px"[A-Za-z][A-Za-z'’-]*" s))

(define (rebuild s positions transform)
  ;; transform : index word -> replacement-string, index is the word's
  ;; position (0-based) among matched word tokens.
  (define out (open-output-string))
  (let loop ([pos 0] [spans positions] [i 0])
    (cond
      [(null? spans)
       (write-string (substring s pos) out)]
      [else
       (define start (caar spans))
       (define end (cdar spans))
       (write-string (substring s pos start) out)
       (write-string (transform i (substring s start end)) out)
       (loop end (cdr spans) (add1 i))]))
  (get-output-string out))

;; Words immediately preceded (ignoring whitespace) by ':' or a dash
;; start a new "sentence-case unit" and keep their capital.
(define (starts-after-colon-or-dash? s start)
  (let loop ([i (sub1 start)])
    (cond
      [(< i 0) #f]
      [(char-whitespace? (string-ref s i)) (loop (sub1 i))]
      [else (memv (string-ref s i) '(#\: #\- #\– #\—))])))

;; A title where most words are individually ALL-CAPS is someone's
;; caps-lock, not a string of acronyms (a genuine acronym like "TCM" is
;; the exception inside an otherwise normal-case title). Once more than
;; half the words are shouting, stop treating ALL-CAPS as "protect this
;; word" -- otherwise the acronym exception leaves the whole thing
;; still capitalized, which is worse than doing nothing.
(define (shouting-title? words)
  (and (>= (length words) 3)
       (> (count all-caps-word? words) (/ (length words) 2))))

;; A word with an internal capital that isn't fully uppercase --
;; "ScienceInsights", "MacBook" -- is a stylized/brand spelling, not a
;; case-convention artifact. Preserved as-is everywhere, same reasoning
;; as acronym preservation below.
(define (mixed-case-word? w)
  (define w2 (strip-punct w))
  (and (> (string-length w2) 1)
       (not (all-caps-word? w2))
       (for/or ([i (in-range 1 (string-length w2))]) (char-upper-case? (string-ref w2 i)))))

(define (to-sentence-case s)
  (define positions (word-boundary-positions s))
  (define words (for/list ([p positions]) (substring s (car p) (cdr p))))
  (define shouting? (shouting-title? words))
  (rebuild s positions
           (lambda (i w)
             (define start (car (list-ref positions i)))
             (cond
               [(mixed-case-word? w) w]
               ;; All-caps check comes before the "is this the first
               ;; word" check so a leading acronym (e.g. "NASW Code of
               ;; Ethics") isn't title-cased down to "Nasw".
               [(and (not shouting?) (all-caps-word? w)) w]
               [(= i 0) (string-titlecase-first w)]
               [(starts-after-colon-or-dash? s start) (string-titlecase-first w)]
               [else (string-downcase w)]))))

(define (string-titlecase-first w)
  (if (zero? (string-length w))
      w
      (string-append (string-upcase (substring w 0 1)) (string-downcase (substring w 1)))))

(define (to-title-case s)
  (define positions (word-boundary-positions s))
  (define n (length positions))
  (rebuild s positions
           (lambda (i w)
             (cond
               [(mixed-case-word? w) w]
               [(all-caps-word? w) w]
               [(or (= i 0) (= i (sub1 n))) (string-titlecase-first w)]
               [(stopword? w) (string-downcase w)]
               [else (string-titlecase-first w)]))))
