(require 'url)

(defvar emacsagist/packagist-url "https://packagist.org")
(defvar emacsagist/packagist-results-buffer "*Packagist*")

(defun emacsagist/make-search-url (query)
  "Creates a search url for the query."
  (concat emacsagist/packagist-url "/search.json?q=" query))

(defun emacsagist/search-packagist (query)
  "Searches Packagist for the query, returning the resulting JSON."
  (save-current-buffer
    (switch-to-buffer (url-retrieve-synchronously
                       (emacsagist/make-search-url query)))
    (buffer-substring url-http-end-of-headers (point-max))))

(defun emacsagist/parse-results (results)
  "Parses the JSON result from the search."
  (json-read-from-string results))

(defun emacsagist/display-result (result)
  "Displays a single result entry."
  (insert (cdr (assoc 'name result))) 
  (newline))

(defun emacsagist/display-results (query results)
  "Displays the results in a user interface buffer."
  (switch-to-buffer emacsagist/packagist-results-buffer)
  (kill-region (point-min) (point-max))
  (insert (concat "Packagist results for: " query))
  (newline 2)
  (let ((matches (cdr (assoc 'results results))))
    (dotimes (index (length matches))
      (emacsagist/display-result (elt matches index)))))

(defun emacsagist/search (query)
  "Prompts the user for a search term, then searches and displays results."
  (interactive "sSearch Packagist for: ")
  (emacsagist/display-results query (emacsagist/parse-results
                                     (emacsagist/search-packagist query))))
