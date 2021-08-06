;;; lbry-mode.el --- Application for LBRY  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version: 0.0.1
;; Keywords: lbry matching multimedia

;;; Commentary:

;; This package provide a major mode to interact with the LBRY data
;; network via Elfeed-like buffer.  The LBRY data network includes
;; videos, films, art, books, and many more.
;; To learn more please visit https://lbry.tech/

;;; Code:
(require 'json)
(require 'cl-lib)
(require 'seq)
;;;; Types

(cl-defstruct (lbry-entry (:constructor lbry-entry-create)
			  (:copier nil))
  "Information about a LBRY video."
  (id "" :read-only t)
  (lbry-url "" :read-only t)
  (release-time "" :read-only t)
  (file-date 0 :read-only t)
  (title     "" :read-only t)
  (stream-type "" :read-only t)
  (media-type "" :read-only t)
  (channel    "" :read-only t)
  (desc "" :read-only t)
  (tags [] :type vector :read-only t))

;;;; Faces

(defface lbry-channel
  (if (featurep 'elfeed)
      '((t :inherit elfeed-search-feed-face))
    '((((class color) (background light)) (:foreground "#003497"))
      (((class color) (background dark))  (:foreground "#92baff"))))
  "Face used for channel name in *LBRY* buffer"
  :group 'lbry)

(defface lbry-date
  (if (featurep 'elfeed)
      '((t :inherit elfeed-search-date-face))
    '((((class color) (background light)) (:foreground "#00538b"))
      (((class color) (background dark))  (:foreground "#00d3d0"))))
  "Face used for date in *LBRY* buffer"
  :group 'lbry)

(defface lbry-tags
  (if (featurep 'elfeed)
      '((t :inherit elfeed-search-tag-face))
    '((((class color) (background light)) (:foreground "#005a5f"))
      (((class color) (background dark))  (:foreground "#6ae4b9"))))
  "Face used for tags in *LBRY* buffer"
  :group 'lbry)

(defface lbry-video-title
  (if (featurep 'elfeed)
      '((t :inherit elfeed-search-unread title-face))
    '((t :weight bold)))
  "Face used for title of claims with \"video\" stream type"
  :group 'lbry)

(defface lbry-image-title
  '((t :inherit outline-5))
  "Face used for title of claims with \"image\" stream type"
  :group 'lbry)
(defface lbry-audio-title
  '((t :inherit outline-7))
  "Face used for title of claims with \"audio\" stream type")
(defface lbry-binary-title
  '((t :inherit outline-3))
  "Face used for title of claims with \"binary\" stream type"
  :group 'lbry)

(defface lbry-document-title
  '((t :inherit outline-4))
  "Face used for title of claims with \"document\" stream type"
  :group 'lbry)


;;;; Customizable variables

(defgroup lbry nil
  "A LBRY application."
  :group 'comm
  :link '(url-link "https://gitlab.com/c1-g/lbry-mode-el"))

(defcustom lbry-order-by 'name
  "Order to sort the results of the search query. Default is descending order
to do an ascending order prepend ^ to the options"
  :type 'symbol
  :options '(choice (symbol :tag "Name" name)
                    (symbol :tag "Height" height)
                    (symbol :tag "Release_Time" release_time)
                    (symbol :tag "Publish_Time" publish_time)
                    (symbol :tag "Amount" amount)
                    (symbol :tag "Effective_Amount" effective_amount)
                    (symbol :tag "Support_Amount" support_amount)
                    (symbol :tag "Trending_Group" trending_group)
                    (symbol :tag "Trending_Mixed" trending_mixed)
                    (symbol :tag "Treading_Local" trending_local)
                    (symbol :tag "Trending_Global" trending_global)
                    (symbol :tag "Activation_Height" activation_height))
  :group 'lbry)

(defcustom lbry-download-directory 'erc--download-directory
  "Directory where files will downloaded.
This should either be a directory name or a function (called with
no parameters) that returns a directory name."
  :group 'lbry
  :type '(choice directory function))

;;;; Internal variables

(defvar-local lbry-entries '(()))

(defvar lbry-api-url "http://localhost:5279"
  "Url to a LBRY api.")

(defvar lbry-instance-url "https://odysee.com"
  "Url to a LBRY host instance")

(defvar-local lbry-current-page 1
  "Current page of the current `lbry-search-term'")

(defvar-local lbry-search-term ""
  "Current search string as used by `lbry-search'")

(defvar-local isearch-mode nil)

(defvar lbry-search-message-prefix "Search terms: ")

(defvar lbry-search-message "")

(defvar lbry-title-reserved-space 65
  "Number of characters reserved for the video title in the *LBRY* buffer.")

(defvar lbry-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'quit-window)
    (define-key map "g" #'lbry-draw-buffer)
    (define-key map "h" #'desribe-mode)
    (define-key map "f" #'lbry-next-page)
    (define-key map "b" #'lbry-last-page)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "s" #'lbry-search)
    (define-key map "s" #'lbry-download)
    (define-key map [return] #'lbry--dwim)
    map))

(defvar lbry-search-mode-map
  (let ((i 0)
	(map (make-keymap)))
    ;; (or (char-table-p (nth 1 map))
    ;; 	(error "The initialization of lbry-search-mode-map must be updated"))
    ;; (set-char-table-range (nth 1 map) (cons #x100 (max-char))
    ;; 			  'lbry-search-print-char)
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'lbry-search-print-char)
      (setq i (1+ i)))
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map))
    (define-key map "\177" 'lbry-search-delete-char)
;;    (define-key map "\C-g" 'lbry-search-abort)
    (define-key map [return] 'lbry-search-exit)
    map)
  "Keymap for `lbry-search-mode'.")

;;;; Functions
;;;;; Format JSON from `lbry-sdk'

(defun lbry--API-call (method args)
  (with-temp-buffer
    (let ((exit-code (call-process "curl" nil (list (current-buffer) "/tmp/lbry-el-curl-error") nil
				   "--show-error"
				   "--silent"
				   "--data"
				   (json-encode `(("method" . ,method)
						  ("params" . ,args)))
				   lbry-api-url)))
      ;; Put error code from cURL in "/tmp/lbry-el-curl-error" file
      (unless (= exit-code 0)
	(with-temp-buffer
	  ;; Insert the error code in temporary buffer
	  (insert-file "/tmp/lbry-el-curl-error")
	  ;; Show the content of the buffer as an error
	  ;; We go out of our to do this because cURL error code is very useful
	  ;; e.g. wrong url or no protocol etc.
	  (user-error "%s" (buffer-string))))
      (goto-char (point-min))
      (ignore-errors (json-read)))))

;; Many thanks to @Malabarba for this fantastic solution
;; https://emacs.stackexchange.com/questions/3197/best-way-to-retrieve-values-in-nested-assoc-lists
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun lbry--query (string &optional page)
  "Query the LBRY blockchain via `lbry-sdk' for STRING, return Nth page of resutls."
  (let ((claims (lbry--API-call "claim_search" `(("text" . ,string)
						 ("page" . ,(or page
							       lbry-current-page))
						 ("claim_type" . "stream")
						 ("fee_amount" . 0)))))
    ;; Above is for debugging
    ;; Error handling
    (if (assoc-recursive claims 'error)
	(error "%s" (assoc-recursive claims 'error 'message))
      ;; Assigning value to `lbry-entry' struct
      (progn (dotimes (i (assoc-recursive claims 'result 'page_size))
	       (let ((stream (aref (assoc-recursive claims 'result 'items) i)))
		 (aset (assoc-recursive claims 'result 'items) i
		       (lbry-entry-create :id (assoc-recursive stream 'claim_id)
					  :lbry-url (assoc-recursive stream 'permanent_url)
					  :release-time (assoc-recursive stream 'value 'release_time)
					  :file-date (assoc-recursive stream 'timestamp)
					  :title (or (assoc-recursive stream 'value 'title)
						     "No title available.")
					  :stream-type (assoc-recursive stream 'value 'stream_type)
					  :media-type (assoc-recursive stream 'value 'source 'media_type)
					  ;; Sometimes claims was published from an anonymous source
					  :channel (or (assoc-recursive stream 'signing_channel 'name)
						       "Anonymous")
					  :tags (assoc-recursive stream 'value 'tags)
					  :desc (assoc-recursive stream 'value 'description)))))
	     (assoc-recursive claims 'result 'items)))))
;;;;; Inserting *LBRY* Buffer

(defun lbry--format-title (title &optional file-type)
  "Format a claim `TITLE' to be inserted according to `lbry-title-reserved-space'"
  (pcase file-type
    ((pred (string-match-p "binary.*")) (propertize title 'face 'lbry-binary-title))
    ((pred (string-match-p "video.*")) (propertize title 'face 'lbry-video-title))
    ((pred (string-match-p "audio.*")) (propertize title 'face 'lbry-audio-title))
    ((pred (string-match-p "image.*")) (propertize title 'face 'lbry-image-title))
    ((pred (string-match-p "document.*")) (propertize title 'face 'lbry-document-title))))

(defun lbry--format-time (timestamp)
  (let ((formatted-date (format-time-string "%Y-%m-%d" (if (stringp timestamp)
				     (string-to-number timestamp)
				     timestamp))))
    (propertize formatted-date 'face 'lbry-date)))

(defun lbry--format-duration (seconds)
  "Format `SECONDS' to \"hh:mm:ss\""
  (let ((formatted-string (concat (format-seconds "%.2h" seconds)
				  ":"
				  (format-seconds "%.2m" (mod seconds 3600))
				  ":"
				  (format-seconds "%.2s" (mod seconds 60)))))
    (propertize formatted-string 'face 'lbry-date)))

(defun lbry--format-tags (tags)
  (propertize 
  (cond ((stringp tags) tags)
	 ((arrayp tags) (mapconcat (lambda (element-in-array) (format "%s" element-in-array)) tags ",")))
  'face 'lbry-tags))

(defun lbry--format-channel (name)
  "Propertize `NAME' for *LBRY* buffer"
  (propertize name 'face 'lbry-channel))

(defun lbry--insert-entry (claim)
  "Insert `CLAIM' in the current buffer."
  (list (lbry-entry-id claim)
        (vector (if (string-match-p "video.*" (lbry-entry-media-type claim))
                    (lbry--format-time (lbry-entry-release-time claim))
                  (lbry--format-time (lbry-entry-file-date claim)))
                (lbry--format-title (lbry-entry-title claim) (lbry-entry-stream-type claim))
                (lbry--format-channel (lbry-entry-channel claim))
                (lbry--format-tags (lbry-entry-media-type claim)))))

(defun lbry-draw-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq tabulated-list-format `[("Date" 10 t)
                                  ("Title" 65 t)
                                  ("Channel" 20 t)
                                  ("Type" 15 t)])
    (setq tabulated-list-entries (mapcar #'lbry--insert-entry
                                         lbry-entry))
    (setq tabulated-list--header-string
          (concat "Search results for "
        	  lbry-search-term ", page "
        	  (number-to-string lbry-current-page)))
    (tabulated-list-init-header)
    (tabulated-list-print)))

;;;###autoload
;;;;; DWIM functions
(defun lbry-application-function (claim-url temp &optional open)
  "Function to invoke when `lbry-entry-media-type' equal \"application\"
The default: Call the LBRY api with \"get\" method on `CLAIM-URL', after 
the claim is downloaded(to /tmp/ when `TEMP' is non-nil), open it with `xdg-open' when `OPEN' is non-nil."
  ;;  Call LBRY with `get' method to download file
  (let ((file-json (lbry--API-call "get" (if temp
					      `(("uri" . ,claim-url)
						 ("download_directory" . "/tmp/"))
						`(("uri" . ,claim-url))))))
    (message "%s%s%s%s" "Downloaded "
	     (assoc-recursive file-json 'result 'file_name) " at "
	     (assoc-recursive file-json 'result 'download_directory))
    (when open
      ;;      (start-process "lbry-application-open" nil "xdg-open" (assoc-recursive file-json 'result 'download_path))
      ;; I use Emacs to open pdf files with `pdf-tools' package.
      (find-file (assoc-recursive file-json 'result 'download_path))
      (message "%s" (concat "Opening " (assoc-recursive file-json 'result 'file_name))))))
      
(defun lbry-audio-function (&optional url)
  "Function to invoke when `lbry-entry-media-type' equal \"audio\"
The default: Call the LBRY api with \"get\" method on `CLAIM-URL', after 
the claim is downloaded, open it with `mpv --no-video' when `OPEN' is non-nil."
  (start-process "lbry-play-audio" nil "mpv"
		 "--no-video" url)
  (message "Starting audio..."))

(defun lbry-image-function (claim-url temp &optional open)
  "Function to invoke when `lbry-entry-media-type' equal \"image\"
The default: Call the LBRY api with \"get\" method on `CLAIM-URL', after 
the claim is downloaded(to /tmp/ when `TEMP' is non-nil), open it with `xdg-open' when `OPEN' is non-nil."
  (let* ((file-json (lbry--API-call "get" (if temp
					      `(("uri" . ,claim-url)
						("download_directory" . "/tmp/"))
					    `(("uri" . ,claim-url))))))
    (message "%s" (concat "Downloaded "
			  (assoc-recursive file-json 'result 'file_name) " at "
			  (assoc-recursive file-json 'result 'download_directory)))
    (when open
      ;; (start-process "lbry-view-image" nil "xdg-open" (assoc-recursive file-json 'result 'download_path))
      ;; I use Emacs
      (find-file (assoc-recursive file-json 'result 'download_path))
      (message "%s" (concat "Opening " (assoc-recursive file-json 'result 'file_name))))))

(defun lbry-text-function (claim-url temp)
    "Function to invoke when `lbry-entry-media-type' equal \"text\"
The default: Call the LBRY api with \"get\" method on `CLAIM-URL', after 
the claim is downloaded(to /tmp/ when `TEMP' is non-nil), call `find-file' to claim"
  (let ((file-json (lbry--API-call "get" (if temp
					      `(("uri" . ,claim-url)
						("download_directory" . "/tmp/"))
					    `(("uri" . ,claim-url))))))
    (find-file (assoc-recursive file-json 'result 'download_path))))

(defun lbry-video-function (url)
  "Function to invoke when `lbry-entry-media-type' equal \"video\"
The default: Call the LBRY api with \"get\" method on `CLAIM-URL', after 
the claim is downloaded, open it with `mpv'"
  (start-process "lbry-play-video" nil "mpv" url)
  (message "%s%s" "Playing " url))

(defun lbry-download (&optional entry tmp)
  "Apply `lbry-*-function' depending on the media type of `ENTRY'"
  (interactive)
  (let ((entry (or entry (lbry-get-current-claim)))
        (dir (if (stringp eww-download-directory)
                 lbry-download-directory
               (funcall lbry-download-directory)))
        (file-json (lbry--API-call "get" (if temp
					     `(("uri" . ,claim-url)
					       ("download_directory" . "/tmp/"))
					   `(("uri" . ,claim-url))))))
    (message "Downloaded %s at %s"
             (assoc-recursive file-json 'result 'file_name)
	     (assoc-recursive file-json 'result 'download_directory))))

(defun lbry-open (&optional entry)
  "Apply `lbry-*-function' depending on the media type of `ENTRY'"
  (interactive)
  (let* ((entry (or entry (lbry-get-current-claim)))
	 (url (lbry-entry-lbry-url entry)))
    (pcase (lbry-entry-media-type entry)
      ((pred (string-match-p "application.*")) (lbry-application-function url t t))
      ((pred (string-match-p "audio.*")) (lbry-audio-function instance-url))
      ((pred (string-match-p "image.*")) (lbry-image-function url t t))
      ((pred (string-match-p "text.*")) (lbry-text-function url t))
      ((pred (string-match-p "video.*")) (lbry-video-function instance-url)))))

;;;;; *LBRY* buffer
;;;;;; Navigation functions
(defun lbry-next-page ()
  "Switch to the next page of the current search. Redraw the buffer."
  (interactive)
  (setf lbry-entry (lbry--query lbry-search-term (1+ lbry-current-page)))
  (setf lbry-current-page (1+ lbry-current-page))
  (lbry-draw-buffer))

(defun lbry-last-page ()
  "Switch to the last page of the current search. Redraw the buffer."
  (interactive)
  (when (> lbry-current-page 1)
    (setf lbry-entry (lbry--query lbry-search-term (1- lbry-current-page)))
    (setf lbry-current-page (1- lbry-current-page))
    (lbry-draw-buffer)))
;;;;; lbry-search minor mode

(define-minor-mode lbry-search-mode
  "Mode for searching in *LBRY* buffer."
  :init-value nil
  :lighter " Search"
  :keymap 
  (make-local-variable 'lbry-search-term)
  (setq lbry-current-page 1)
  (lbry-search-update))

(defun lbry-search () 
   (interactive)
   (lbry-search-mode 1))

(defun lbry-search-exit ()
  (interactive)
  (lbry-search-mode 0)
  (force-mode-line-update)
  (setf lbry-current-page 1)
  (setq lbry-search-term lbry-search-message)
  (setq lbry-search-message "")
  (setf lbry-entry (lbry--query lbry-search-term))
  (lbry-draw-buffer))

;;;;;; Building search message

(defun lbry-search-prefix ()
  (propertize lbry-search-message-prefix 'face 'minibuffer-prompt 'read-only 't))

(defun lbry-search-update ()
  (if (not (input-pending-p))
      (funcall #'lbry-search-message)))

(defun lbry-search-print-char (&optional char count)
  (interactive (list last-command-event
		     (prefix-numeric-value current-prefix-arg)))
  (let ((char (or char last-command-event)))
    (if (= char ?\S-\ )
	(setq char ?\s))
    (lbry-process-search-char char count)))

(defun lbry-process-search-char (char &optional count)
  (let* ((string (if (and (integerp count) (> count 1))
		     (make-string count char)
		   (char-to-string char)))
	 (message (if (>= char ?\200)
		      string
		    (mapconcat 'char-to-string string ""))))
    (lbry-process-search-string string message)))

(defun lbry-process-search-string (string message)
  (setq lbry-search-term (concat lbry-search-term string)
	lbry-search-message (concat lbry-search-message message))
  (lbry-search-update))

(defun lbry-search-message ()
  (let ((m lbry-search-message))
    (when (string-match "@.* \\| @.*$" lbry-search-message)
      (setq m (copy-sequence m))
      (add-text-properties (match-beginning 0) (match-end 0) '(face lbry-channel) m))
    (setq m (concat
	     (lbry-search-prefix)
	     m))
    (let ((message-log-max nil))
      (message "%s" m))))

(defun lbry-search-delete-char ()
  (interactive)
  (if (seq-empty-p lbry-search-term)
      (ding)
    (setf lbry-search-message (substring lbry-search-message 0 (1- (length lbry-search-message)))))
  (lbry-search-update))
    
(defun lbry-get-current-claim ()
  (aref lbry-entry (1- (line-number-at-pos))))

(define-derived-mode lbry-mode tabulated-list-mode "LBRY"
  (setq buffer-read-only t
	truncate-line t)
  (buffer-disable-undo)
  (hl-line-mode))

;;;###autoload
(defun lbry ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*LBRY*"))
  (unless (eq major-mode 'lbry-mode)
    (lbry-mode))
  (when (seq-empty-p lbry-search-term)
   (call-interactively #'lbry-search)))

(provide 'lbry-mode)
;;; lbry-mode.el ends here
