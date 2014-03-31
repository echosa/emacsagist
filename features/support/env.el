(require 'f)

(defvar emacsagist-support-path
  (f-dirname load-file-name))

(defvar emacsagist-features-path
  (f-parent emacsagist-support-path))

(defvar emacsagist-root-path
  (f-parent emacsagist-features-path))

(add-to-list 'load-path emacsagist-root-path)

(require 'emacsagist)
(require 'espuds)
(require 'ert)

(defun emacsagist/search-packagist (results)
  (with-temp-buffer
    (insert-file-contents
     (concat emacsagist-root-path "/test-data/search-page"
             (number-to-string (emacsagist/packagist-search-page results))
             ".json"))
    (buffer-string)))

(defun emacsagist/get-packagist-package (package)
  (with-temp-buffer
    (insert-file-contents
     (concat emacsagist-root-path "/test-data/phpunit.json"))
    (buffer-string)))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
