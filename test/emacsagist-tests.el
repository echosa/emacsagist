(require 'ert)
(require 'emacsagist)

(ert-deftest make-search-url-with-page-string-test ()
  (should (string=
           "https://packagist.org/search.json?q=phpunit&page=1"
           (emacsagist/make-search-url
            (make-emacsagist/packagist-search :query "phpunit" :page 1)))))

(ert-deftest make-search-url-without-page-test ()
  (should (string=
           "https://packagist.org/search.json?q=phpunit"
           (emacsagist/make-search-url
            (make-emacsagist/packagist-search :query "phpunit")))))
