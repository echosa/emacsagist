;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I run a search$"
  (lambda ()
    (emacsagist/search "phpunit")))

(Then "^I should be at the end of buffer$"
  (lambda ()
    (cl-assert (eobp))))
