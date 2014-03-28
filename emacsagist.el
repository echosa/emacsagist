;; -*- lexical-binding: t; -*-
;;; emacsagist.el --- Search Packagist.org packages without leaving Emacs

;; Version: 0.1.0

;;; Commentary:
;; 

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Code:

(defvar emacsagist/packagist-url "https://packagist.org")
(defvar emacsagist/packagist-results-buffer "*Packagist*")

(cl-defstruct emacsagist/packagist-search
  query page results next-page)

(cl-defstruct emacsagist/search-result
  name description url)

(defun emacsagist/make-search-url (search)
  "Create a search url for the QUERY.
Argument PAGE page number."
  (let ((page (emacsagist/packagist-search-page search)))
    (concat emacsagist/packagist-url "/search.json?q="
            (emacsagist/packagist-search-query search)
            (when page (concat "&page=" (number-to-string page))))))

(defun emacsagist/search-packagist (search)
  "Search Packagist for the QUERY, returning the resulting JSON.
Argument PAGE page number."
  (save-current-buffer
    (let ((http-buffer (url-retrieve-synchronously
                        (emacsagist/make-search-url search))))
      (switch-to-buffer http-buffer)
      (let ((result (buffer-substring url-http-end-of-headers (point-max))))
        (kill-buffer http-buffer)
        result))))

(defun emacsagist/parse-results (search results)
  "Parse the JSON result from the search.
Argument RESULTS JSON results."
  (let ((parsed-results (json-read-from-string results)))
    (setf (emacsagist/packagist-search-results search)
          (cdr (assoc 'results parsed-results)))
    (setf (emacsagist/packagist-search-next-page search)
          (let ((next-url (when (assoc 'next parsed-results)
                            (cdr (assoc 'next parsed-results)))))
            (when next-url
              (nth 1 (split-string next-url "page=")))))
    search))

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

(defun emacsagist/display-page-links (search)
  "Display previous/next page links.
Argument PAGE current page
Argument NEXT-PAGE next page
Argument QUERY search string"
  (let ((query (emacsagist/packagist-search-query search))
        (page (emacsagist/packagist-search-page search)) 
        (next-page (emacsagist/packagist-search-next-page search)))
    (newline)
    (when (> page 1)
      (emacsagist/display-previous-page-link
       (number-to-string (- page 1)) query)
      (when next-page (insert " ")))
    (when next-page
      (emacsagist/display-next-page-link next-page query))
    (insert " ")))

(defun emacsagist/display-header (search)
  "Displays a header for the search results.
Argument QUERY search query.
Argument PAGE page number.
Optional argument NEXT-PAGE next page number."
  (let ((query (emacsagist/packagist-search-query search))
        (page (emacsagist/packagist-search-page search))
        (next-page (emacsagist/packagist-search-next-page search)))
    (insert (concat "Packagist results for: " query))
    (newline)
    (insert (concat "Page " (number-to-string page)))
    (emacsagist/display-page-links search))
  (newline 2))

(defun emacsagist/goto-url-property ()
  "Open URL in a web browser."
  (interactive)
  (browse-url (get-text-property (point) 'url)))

(defun emacsagist/display-result (result)
  "Displays a single RESULT entry."
  (insert (cdr (assoc 'name result)))
  (newline)
  (let ((desc (cdr (assoc 'description result))))
    (unless (string= desc "")
      (insert desc)
      (newline)))
  (let ((url (cdr (assoc 'url result)))
        (start (point))
        (map (make-sparse-keymap)))
    (insert url)
    (define-key map (kbd "RET") 'emacsagist/goto-url-property)
    (add-text-properties start (point) `(keymap ,map
                                         face underline
                                         url ,url)))
  (insert " ")
  (newline 2))

(defun emacsagist/display-results (search)
  "Display the results in a user interface buffer.
Argument QUERY search string.
Argument PAGE page number.
Argument RESULTS search results."
  (switch-to-buffer emacsagist/packagist-results-buffer)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  (let ((matches (emacsagist/packagist-search-results search))
        (next-page (emacsagist/packagist-search-next-page search)))
    (emacsagist/display-header search)
    (if (= 0 (length matches))
        (insert "No packages found.")
      (dotimes (index (length matches))
        (emacsagist/display-result (elt matches index))))
    (emacsagist/display-header search))
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
  "Prompt the user for a search QUERY, then search and display results for the 
correct PAGE."
  (interactive "sSearch Packagist for: ")
  (let* ((page (if (stringp page) (string-to-number page) (or page 1)))
         (search (make-emacsagist/packagist-search :query query :page page)))
    (emacsagist/display-results
     (emacsagist/parse-results search (emacsagist/search-packagist search)))))

(define-derived-mode emacsagist-mode special-mode "Emacsagist"
  "Major mode for the emacsagist results buffer.
\\{emacsagist-mode-map}"
  (auto-fill-mode))

(provide 'emacsagist)

;;; emacsagist.el ends here
