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

(defun emacsagist/display-header (query &optional next-url)
  "Displays a header for the search results."
  (insert (concat "Packagist results for: " query))
  (when next-url
    (newline)
    (insert next-url))
  (newline 2))

(defun emacsagist/display-footer (&optional next-url)
  "Displays a footer for the search results."
  (when next-url
    (newline)
    (insert next-url)))

(defun emacsagist/display-result (result)
  "Displays a single result entry."
  (insert (cdr (assoc 'name result))) 
  (newline))

(defun emacsagist/display-results (query results)
  "Displays the results in a user interface buffer."
  (switch-to-buffer emacsagist/packagist-results-buffer)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  (let ((matches (cdr (assoc 'results results)))
        (next-url (when (assoc 'next results)
                    (cdr (assoc 'next results)))))
    (emacsagist/display-header query next-url)
    (dotimes (index (length matches))
      (emacsagist/display-result (elt matches index)))
    (emacsagist/display-footer next-url))
  (read-only-mode 1)
  (goto-char (point-min))
  (emacsagist-mode))

(defun emacsagist/search (query)
  "Prompts the user for a search term, then searches and displays results."
  (interactive "sSearch Packagist for: ")
  (emacsagist/display-results query (emacsagist/parse-results
                                     (emacsagist/search-packagist query))))

(defun emacsagist/keypress-return ()
  (interactive)
  (message "return pressed"))

(define-derived-mode emacsagist-mode special-mode "Emacsagist"
  "Major mode for the emacsagist results buffer.
\\{emacsagist-mode-map}"
  nil)

(define-key emacsagist-mode-map [return] 'emacsagist/keypress-return)

(provide 'emacsagist)
