(require 'url)

(defvar emacsagist/packagist-url "https://packagist.org")
(defvar emacsagist/packagist-results-buffer "*Packagist*")

(defun emacsagist/make-search-url (query page)
  "Creates a search url for the query."
  (concat emacsagist/packagist-url "/search.json?q=" query
          (when page
            (concat "&page=" (if (stringp page)
                                 page
                               (number-to-string page))))))

(defun emacsagist/search-packagist (query page)
  "Searches Packagist for the query, returning the resulting JSON."
  (save-current-buffer
    (switch-to-buffer (url-retrieve-synchronously
                       (emacsagist/make-search-url query page)))
    (buffer-substring url-http-end-of-headers (point-max))))

(defun emacsagist/parse-results (results)
  "Parses the JSON result from the search."
  (json-read-from-string results))

(defun emacsagist/goto-url ()
  (interactive)
  (message (concat "going to" (get-text-property (point) 'url))))

(defun emacsagist/display-next-page-link (next-page query)
  (let ((position (point)))
    (insert "[Next Page]")
    (put-text-property position (point) 'next-page next-page)
    (let ((map (make-sparse-keymap)))
      (define-key map [return] 'emacsagist/display-next-page)
      (add-text-properties position (point) `(keymap ,map
                                              mouse-face highlight
                                              next-page ,next-page
                                              query ,query))))) 

(defun emacsagist/display-previous-page-link (previous-page query)
  (let ((position (point)))
    (insert "[Previous Page]")
    (put-text-property position (point) 'previous-page previous-page)
    (let ((map (make-sparse-keymap)))
      (define-key map [return] 'emacsagist/display-previous-page)
      (add-text-properties position (point) `(keymap ,map
                                              mouse-face highlight
                                              previous-page ,previous-page
                                              query ,query)))))

(defun emacsagist/display-page-links (page next-page query)
  (let (show-previous-link
        show-next-link
        (page (if (and page (stringp page))
                  (string-to-number page)
                page)))
    (when (> page 1)
      (setq show-previous-link t))
    (when next-page
      (setq show-next-link t))
    (when (or show-previous-link show-next-link)
      (newline))
    (when show-previous-link
      (emacsagist/display-previous-page-link (number-to-string (- page 1)) query)
      (when show-next-link
        (insert " ")))
    (when show-next-link
      (emacsagist/display-next-page-link next-page query))
    ))

(defun emacsagist/display-header (query page &optional next-page)
  "Displays a header for the search results."
  (insert (concat "Packagist results for: " query))
  (newline)
  (insert (concat "Page " (if (and page (stringp page))
                              page
                            (number-to-string page))))
  (emacsagist/display-page-links page next-page query)
  (newline 2))

(defun emacsagist/display-footer (&optional next-page)
  "Displays a footer for the search results."
  (emacsagist/display-page-links page next-page query))

(defun emacsagist/display-result (result)
  "Displays a single result entry."
  (insert (cdr (assoc 'name result))) 
  (newline))

(defun emacsagist/get-next-page-number (next-url)
  "Returns the page number in the next url."
  (when next-url
    (let ((delimiter "page="))
      (when (string-match delimiter next-url)
        (nth 1 (split-string next-url delimiter))))))

(defun emacsagist/display-results (query page results)
  "Displays the results in a user interface buffer."
  (switch-to-buffer emacsagist/packagist-results-buffer)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  (let ((matches (cdr (assoc 'results results)))
        (next-url (emacsagist/get-next-page-number
                   (when (assoc 'next results)
                     (cdr (assoc 'next results))))))
    (emacsagist/display-header query page next-url)
    (if (= 0 (length matches))
        (insert "No packages found.")
      (dotimes (index (length matches))
        (emacsagist/display-result (elt matches index))))
    (emacsagist/display-footer next-url))
  (read-only-mode 1)
  (goto-char (point-min))
  (emacsagist-mode))

(defun emacsagist/display-previous-page ()
  (interactive)
  (let ((previous-page (get-text-property (point) 'previous-page))
        (query (get-text-property (point) 'query)))
    (emacsagist/search query previous-page)))

(defun emacsagist/display-next-page ()
  (interactive)
  (let ((next-page (get-text-property (point) 'next-page))
        (query (get-text-property (point) 'query)))
    (emacsagist/search query next-page)))

(defun emacsagist/search (query &optional page)
  "Prompts the user for a search term, then searches and displays results."
  (interactive "sSearch Packagist for: ")
  (let ((page (or page 1)))
    (emacsagist/display-results query
                                page
                                (emacsagist/parse-results
                                       (emacsagist/search-packagist query page)))))

(defun emacsagist/keypress-return ()
  (interactive)
  (message "return pressed"))

(define-derived-mode emacsagist-mode special-mode "Emacsagist"
  "Major mode for the emacsagist results buffer.
\\{emacsagist-mode-map}"
  nil)

(define-key emacsagist-mode-map [return] 'emacsagist/keypress-return)

(provide 'emacsagist)
