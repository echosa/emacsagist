;;; emacsagist.el --- Search Packagist.org packages without leaving Emacs -*- lexical-binding: t; -*-

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
  "Generate a url for the SEARCH query."
  (let ((page (emacsagist/packagist-search-page search)))
    (concat emacsagist/packagist-url "/search.json?q="
            (emacsagist/packagist-search-query search)
            (when page (concat "&page=" (number-to-string page))))))

(defun emacsagist/search-packagist (search)
  "Search Packagist for the given SEARCH, returning the resulting JSON."
  (save-current-buffer
    (let ((http-buffer (url-retrieve-synchronously
                        (emacsagist/make-search-url search))))
      (switch-to-buffer http-buffer)
      (let ((result (buffer-substring url-http-end-of-headers (point-max))))
        (kill-buffer http-buffer)
        result))))

(defun emacsagist/parse-results (search results)
  "Parse the SEARCH struct's RESULTS."
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
  "Add link between START and END to view TARGET-PAGE of QUERY search results."
  (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") 'emacsagist/display-page)
      (add-text-properties start end `(keymap ,map
                                       face underline
                                       page ,target-page
                                       query ,query))))

(defun emacsagist/display-page-links (search)
  "Display previous/next page links for SEARCH results."
  (let ((query (emacsagist/packagist-search-query search))
        (page (emacsagist/packagist-search-page search))
        (next-page (emacsagist/packagist-search-next-page search)))
    (newline)
    (when (> page 1)
      (let ((start (point)))
        (insert "[Previous Page]")
        (emacsagist/add-page-link start (point) (- page 1) query))
      (when next-page (insert " ")))
    (when next-page
      (let ((start (point)))
        (insert "[Next Page]")
        (emacsagist/add-page-link start (point) next-page query)))
    (insert " ")))

(defun emacsagist/display-header (search)
  "Display a header for the SEARCH results."
  (let ((query (emacsagist/packagist-search-query search))
        (page (emacsagist/packagist-search-page search))
        (next-page (emacsagist/packagist-search-next-page search)))
    (insert (concat "Packagist results for: " query))
    (newline)
    (insert (concat "Page " (number-to-string page)))
    (emacsagist/display-page-links search))
  (newline 2))

(defun emacsagist/goto-url-property ()
  "Open the URL text property in a web browser."
  (interactive)
  (browse-url (get-text-property (point) 'url)))

(defun emacsagist/display-result (result)
  "Display the RESULT entry in the search results list."
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
  "Display the SEARCH results in a user interface buffer."
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
  "Display search results page stored in the page text-property."
  (interactive)
  (let ((page (get-text-property (point) 'page))
        (query (get-text-property (point) 'query)))
    (emacsagist/search query page)))

(defun emacsagist/search (query &optional page)
  "Search Packagist for QUERY, then display results for PAGE (default 1)."
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
