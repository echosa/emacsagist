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
  name description url downloads favers)

(cl-defstruct emacsagist/packagist-package
  name description maintainers versions type repository downloads favers)

(cl-defstruct emacsagist/packagist-package-version
  name description homepage version license authors type require suggest)

(defun emacsagist/parse-package-versions (data)
  "Parse the DATA into a list of structs."
  (loop for version in data
        collect
        (make-emacsagist/packagist-package-version
         :name (cdr (assoc 'name (cdr version)))
         :description (cdr (assoc 'description (cdr version)))
         :homepage (cdr (assoc 'homepage (cdr version)))
         :version (cdr (assoc 'version (cdr version)))
         :license (cdr (assoc 'license (cdr version)))
         :authors (cdr (assoc 'authors (cdr version)))
         :type (cdr (assoc 'type (cdr version)))
         :require (cdr (assoc 'require (cdr version)))
         :suggest (cdr (assoc 'suggest (cdr version))))))

(defun emacsagist/parse-package (json)
  "Parse JSON data into a struct."
  (let ((parsed-data (cdr (assoc 'package (json-read-from-string json)))))
    (make-emacsagist/packagist-package
     :name (cdr (assoc 'name parsed-data))
     :description (cdr (assoc 'description parsed-data))
     :maintainers (cdr (assoc 'maintainers parsed-data))
     :versions (emacsagist/parse-package-versions
                (cdr (assoc 'versions parsed-data)))
     :type (cdr (assoc 'type parsed-data))
     :repository (cdr (assoc 'repository parsed-data))
     :downloads (cdr (assoc 'downloads parsed-data))
     :favers (cdr (assoc 'faves parsed-data)))))

(defun emacsagist/make-package-url (package)
  "Generate a url for the PACKAGE."
  (concat emacsagist/packagist-url "/packages/" 
          (emacsagist/packagist-package-name package)
          ".json"))

(defun emacsagist/get-packagist-package (package)
  "Get the PACKAGE information from Packagist."
  (save-current-buffer
    (let ((http-buffer (url-retrieve-synchronously
                        (emacsagist/make-package-url package))))
      (switch-to-buffer http-buffer)
      (let ((result (buffer-substring url-http-end-of-headers (point-max))))
        (kill-buffer http-buffer)
        result))))

(defun emacsagist/display-package (package-name)
  (let ((package (emacsagist/parse-package
                  (emacsagist/get-packagist-package
                   (make-emacsagist/packagist-package :name package-name)))))
    (switch-to-buffer emacsagist/packagist-results-buffer)
    (read-only-mode -1)
    (kill-region (point-min) (point-max))
    (insert (emacsagist/packagist-package-name package))
    (read-only-mode 1)
    (goto-char (point-min))
    (emacsagist-mode)))

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

(defun emacsagist/parse-search (search results)
  "Parse the SEARCH struct's RESULTS."
  (let ((parsed-results (json-read-from-string results)))
    (setf (emacsagist/packagist-search-results search)
          (loop for result across (cdr (assoc 'results parsed-results))
                collect (make-emacsagist/search-result
                         :name (cdr (assoc 'name result))
                         :description (cdr (assoc 'description result))
                         :url (cdr (assoc 'url result))
                         :downloads (cdr (assoc 'downloads result))
                         :favers (cdr (assoc 'favers result)))))
    (setf (emacsagist/packagist-search-next-page search)
          (when (assoc 'next parsed-results)
            (string-to-number
             (nth 1 (split-string (cdr (assoc 'next parsed-results))
                                  "page=")))))
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
  (insert (concat "Packagist results for: "
                  (emacsagist/packagist-search-query search)))
  (newline)
  (insert (concat "Page "
                  (number-to-string (emacsagist/packagist-search-page search))))
  (newline)
  (emacsagist/display-page-links search)
  (newline 2))

(defun emacsagist/goto-url-property ()
  "Open the URL text property in a web browser."
  (interactive)
  (browse-url (get-text-property (point) 'url)))

(defun emacsagist/next-link ()
  "Move cursor to the next link in the buffer."
  (interactive)
  (let* ((next-position (next-single-property-change (point) 'keymap))
         (next-position
          (when next-position
            (if (get-text-property next-position 'keymap)
                next-position
              (unless (eobp)
                (next-single-property-change next-position 'keymap))))))
    (when next-position (goto-char next-position))))

(defun emacsagist/previous-link ()
  "Move cursor to the previous link in the buffer."
  (interactive)
  (let* ((previous-position (previous-single-property-change (point) 'keymap))
         (previous-position
          (when previous-position
            (if (get-text-property previous-position 'keymap)
                previous-position
              (unless (bobp)
                (previous-single-property-change previous-position 'keymap))))))
    (when previous-position (goto-char previous-position))))

(defun emacsagist/display-result (result)
  "Display the RESULT entry in the search results list."
  (insert (concat (emacsagist/search-result-name result) " ("
                  (number-to-string (emacsagist/search-result-downloads result))
                  " download"
                  (when (> (emacsagist/search-result-downloads result) 1) "s")
                  ", "
                  (number-to-string (emacsagist/search-result-favers result))
                  " favorite"
                  (when (> (emacsagist/search-result-favers result) 1) "s")
                  ")"))
  (newline)
  (let ((desc (emacsagist/search-result-description result)))
    (unless (string= desc "")
      (insert desc)
      (newline)))
  (let ((url (emacsagist/search-result-url result))
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
  (let ((matches (emacsagist/packagist-search-results search)))
    (emacsagist/display-header search)
    (if (= 0 (length matches))
        (insert "No packages found.")
      (dolist (result matches)
        (emacsagist/display-result result)))
    (emacsagist/display-header search))
  (read-only-mode 1)
  (goto-char (point-min))
  (emacsagist-mode))

(defun emacsagist/display-page ()
  "Display search results page stored in the page text-property."
  (interactive)
  (emacsagist/search (get-text-property (point) 'query)
                     (get-text-property (point) 'page)))

(defun emacsagist/search (query &optional page)
  "Search Packagist for QUERY, then display results for PAGE (default 1)."
  (interactive "sSearch Packagist for: ")
  (let* ((page (if (stringp page) (string-to-number page) (or page 1)))
         (search (make-emacsagist/packagist-search :query query :page page)))
    (emacsagist/display-results
     (emacsagist/parse-search search (emacsagist/search-packagist search)))))


(define-derived-mode emacsagist-mode special-mode "Emacsagist"
  "Major mode for the emacsagist results buffer.
\\{emacsagist-mode-map}"
  (auto-fill-mode))

(define-key emacsagist-mode-map (kbd "TAB") 'emacsagist/next-link)
(define-key emacsagist-mode-map [backtab] 'emacsagist/previous-link)

(provide 'emacsagist)

;;; emacsagist.el ends here
