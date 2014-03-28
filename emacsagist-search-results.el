(require 'cl-lib)

(cl-defstruct emacsagist/packagist-search
  query page results next-url)

(cl-defstruct emacsagist/search-result
  name description url)

(provide 'emacsagist-search-results)
