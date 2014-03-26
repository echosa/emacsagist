(require 'ert)
(require 'emacsagist)

(ert-deftest make-search-url-test ()
  (should (string=
           "https://packagist.org/search.json?q=phpunit&page=1"
           (emacsagist/make-search-url "phpunit" 1))))
