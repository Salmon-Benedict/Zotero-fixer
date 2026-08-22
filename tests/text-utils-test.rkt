#lang racket/base
(require rackunit "../text-utils.rkt")

(test-case "to-sentence-case: lowercases everything but the first word"
  (check-equal? (to-sentence-case "The Rise Of The Machines")
                "The rise of the machines"))

(test-case "to-sentence-case: keeps the word after a colon capitalized"
  (check-equal? (to-sentence-case "Traditional Chinese Medicine: An Overview")
                "Traditional chinese medicine: An overview"))

(test-case "to-sentence-case: preserves an all-caps acronym"
  (check-equal? (to-sentence-case "Traditional Chinese Medicine (TCM)")
                "Traditional chinese medicine (TCM)"))

(test-case "to-sentence-case: an entirely-shouting title is normalized, not treated as all acronyms"
  (check-equal? (to-sentence-case "ETHICAL CONFLICTS IN WORK WITH DISORDER")
                "Ethical conflicts in work with disorder"))

(test-case "to-sentence-case: a leading acronym survives (not title-cased to 'Nasw')"
  (check-equal? (to-sentence-case "NASW Code of Ethics: English")
                "NASW code of ethics: English"))

(test-case "to-sentence-case: a stylized mixed-case brand word is left untouched"
  (check-equal? (to-sentence-case "A Timeline - ScienceInsights")
                "A timeline - ScienceInsights"))

(test-case "to-title-case: a stylized mixed-case brand word is left untouched"
  (check-equal? (to-title-case "the ScienceInsights journal")
                "The ScienceInsights Journal"))

(test-case "to-title-case: capitalizes content words, lowercases small words"
  (check-equal? (to-title-case "journal of automation studies")
                "Journal of Automation Studies"))

(test-case "to-title-case: capitalizes a leading/trailing small word anyway"
  (check-equal? (to-title-case "the new england journal of medicine")
                "The New England Journal of Medicine"))

(test-case "looks-title-case?/looks-sentence-case? still work after the refactor"
  (check-true (looks-title-case? "The Rise Of The Machines In Manufacturing"))
  (check-false (looks-title-case? "The rise of the machines in manufacturing"))
  (check-true (looks-sentence-case? "journal of automation studies"))
  (check-false (looks-sentence-case? "Journal of Automation Studies")))
