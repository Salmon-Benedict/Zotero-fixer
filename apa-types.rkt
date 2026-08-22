#lang racket/base
;; Per-CSL-type table of which abstract APA reference elements are
;; required vs. merely recommended.
;;
;; Element keys used here (checked against csl-json.rkt accessors in
;; checks.rkt, not raw CSL field names):
;;   'author 'editor 'date 'title 'container-title
;;   'publisher 'volume 'issue 'page 'url-or-doi

(provide type-spec type-spec-required type-spec-recommended
         type-spec-for known-type? field-label)

(struct type-spec (required recommended) #:transparent)

(define default-spec (type-spec '(title) '(author date)))

(define apa-type-table
  (hash
   "book"
   ;; Print books have no URL/DOI element in APA at all, so it's not
   ;; even a "recommended" field here -- only ebooks would carry one,
   ;; and we have no reliable signal to distinguish the two.
   (type-spec '(author date title publisher) '())

   "chapter"
   (type-spec '(author date title container-title page) '(editor publisher))

   "article-journal"
   ;; Issue number is deliberately left off the recommended list here --
   ;; check-journal-fields in checks.rkt already gives the more precise
   ;; "volume present but issue missing" advice, so listing it here too
   ;; would double-flag the same problem.
   (type-spec '(author date title container-title volume page) '(url-or-doi))

   "article-magazine"
   (type-spec '(author date title container-title) '(volume issue page url-or-doi))

   "article-newspaper"
   (type-spec '(date title container-title) '(author url-or-doi))

   "webpage"
   ;; APA explicitly sanctions omitting author (move title into author
   ;; position) and date ("n.d.") for webpages -- these are normal, not
   ;; defects, so they're deliberately not even "recommended" here.
   (type-spec '(title url-or-doi) '(container-title))

   "report"
   (type-spec '(author date title publisher) '(url-or-doi))

   "paper-conference"
   (type-spec '(author date title container-title) '(url-or-doi))

   "post-weblog"
   ;; Same reasoning as webpage: a missing author/date is handled by
   ;; APA's own "n.d." / title-as-author conventions, not a defect.
   (type-spec '(title url-or-doi) '(container-title))

   "document"
   ;; CSL's generic catch-all (reports, manuscripts, fact sheets, etc.)
   ;; -- kept permissive since it covers too wide a range of real APA
   ;; reference types to enforce a strict shape.
   (type-spec '(title) '(author date publisher url-or-doi))

   "motion_picture"
   ;; Director, D. D. (Director). (Year). Title of motion picture [Film]. Production Company.
   (type-spec '(date title publisher) '(author url-or-doi))

   "entry-encyclopedia"
   ;; Author. (Year). Title of entry. In Editor (Ed.), Title of encyclopedia (pp. xx-xx). Publisher.
   (type-spec '(title container-title date) '(author editor publisher page url-or-doi))

   "graphic"
   ;; Author. (Year). Title of work [Photograph/Painting/etc.]. Source.
   (type-spec '(title) '(author date publisher url-or-doi))))

(define (known-type? type) (hash-has-key? apa-type-table type))

(define (type-spec-for type) (hash-ref apa-type-table type default-spec))

(define field-labels
  (hash 'author "author"
        'editor "editor"
        'date "publication date"
        'title "title"
        'container-title "container title (journal/site/book name)"
        'publisher "publisher"
        'volume "volume"
        'issue "issue number"
        'page "page range"
        'url-or-doi "URL or DOI"))

(define (field-label key) (hash-ref field-labels key (symbol->string key)))
