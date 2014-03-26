;; -*- lexical-binding: t; -*-
;;; emacsagist.el --- Search Packagist.org packages without leaving Emacs

;; Version: 0.1.0

;;; Commentary:
;; 

(require 'url)
(require 'json)

;;; Code:

(defvar emacsagist/packagist-url "https://packagist.org")
(defvar emacsagist/packagist-results-buffer "*Packagist*")

(defun emacsagist/make-search-url (query page)
  "Create a search url for the QUERY.
Argument PAGE page number."
  (concat emacsagist/packagist-url "/search.json?q=" query
          (when page
            (concat "&page=" (if (stringp page)
                                 page
                               (number-to-string page))))))

(defun emacsagist/search-packagist (query page)
  "Search Packagist for the QUERY, returning the resulting JSON.
Argument PAGE page number."
  (save-current-buffer
    (let ((http-buffer (url-retrieve-synchronously
                        (emacsagist/make-search-url query page))))
      (switch-to-buffer http-buffer)
      (let ((result (buffer-substring url-http-end-of-headers (point-max))))
        (kill-buffer http-buffer)
        result))))

(defun emacsagist/parse-results (results)
  "Parse the JSON result from the search.
Argument RESULTS JSON results."
  (json-read-from-string results))

(defun emacsagist/add-page-link (start end target-page query)
  "Add a link for visiting the previous or next page of results to the region 
between START and END.
Argument TARGET-PAGE the target page number.
Argument QUERY query string to pass along for display."
  (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") 'emacsagist/display-page)
      (add-text-properties start end `(keymap ,map
                                       face underline
                                       page ,target-page
                                       query ,query))))

(defun emacsagist/display-next-page-link (next-page query)
  "Display a link to the next page of search results.
Argument NEXT-PAGE previous next number.
Argument QUERY the search string.
Argument QUERY query string to pass along for display."
  (let ((start (point)))
    (insert "[Next Page]")
    (emacsagist/add-page-link start (point) next-page query)))

(defun emacsagist/display-previous-page-link (previous-page query)
  "Display a link to the previous page of search results.
Argument PREVIOUS-PAGE previous page number.
Argument QUERY the search string.
Argument QUERY query string to pass along for display."
  (let ((start (point)))
    (insert "[Previous Page]")
    (emacsagist/add-page-link start (point) previous-page query)))

(defun emacsagist/display-page-links (page next-page query)
  "Display previous/next page links.
Argument PAGE current page
Argument NEXT-PAGE next page
Argument QUERY search string"
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
      (emacsagist/display-next-page-link next-page query)
      (insert " "))))

(defun emacsagist/display-header (query page &optional next-page)
  "Displays a header for the search results.
Argument QUERY search query.
Argument PAGE page number.
Optional argument NEXT-PAGE next page number."
  (insert (concat "Packagist results for: " query))
  (newline)
  (insert (concat "Page " (if (and page (stringp page))
                              page
                            (number-to-string page))))
  (emacsagist/display-page-links page next-page query)
  (newline 2))

(defun emacsagist/display-footer (query page &optional next-page)
  "Displays a footer for the search results.
Argument QUERY query string to pass along.
Argument PAGE current page number.
Optional argument NEXT-PAGE next page number."
  (emacsagist/display-page-links page next-page query))

(defun emacsagist/display-result (result)
  "Displays a single RESULT entry."
  (insert (cdr (assoc 'name result)))
  (newline)
  (insert (cdr (assoc 'description result)))
  (newline)
  (insert (cdr (assoc 'url result)))
  (newline 2))

(defun emacsagist/get-next-page-number (next-url)
  "Return the page number in the next url.
Argument NEXT-URL next url string."
  (when next-url
    (let ((delimiter "page="))
      (when (string-match delimiter next-url)
        (nth 1 (split-string next-url delimiter))))))

(defun emacsagist/display-results (query page results)
  "Display the results in a user interface buffer.
Argument QUERY search string.
Argument PAGE page number.
Argument RESULTS search results."
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
    (emacsagist/display-footer query page next-url))
  (read-only-mode 1)
  (goto-char (point-min))
  (emacsagist-mode))

(defun emacsagist/display-page ()
  "Display previous page."
  (interactive)
  (let ((page (get-text-property (point) 'page))
        (query (get-text-property (point) 'query)))
    (emacsagist/search query page)))

(defun emacsagist/search (query &optional page)
  "Prompt the user for a search QUERY, then search and display results for the correct PAGE."
  (interactive "sSearch Packagist for: ")
  (let ((page (or page 1)))
    (emacsagist/display-results
     query
     page
     (emacsagist/parse-results (emacsagist/search-packagist query page)))))

(define-derived-mode emacsagist-mode special-mode "Emacsagist"
  "Major mode for the emacsagist results buffer.
\\{emacsagist-mode-map}"
  (auto-fill-mode))

(provide 'emacsagist)

;;; emacsagist.el ends here
