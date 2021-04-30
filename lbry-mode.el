;;; lbry-mode.el --- Application for LBRY                 -*- lexical-binding: t; -*-

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

;; This package provide a major mode to interact with the LBRY data network via Elfeed-like buffer.
;; The LBRY data network includes videos, films, art, books, and many more.
;; To learn more please visit https://lbry.tech/

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Types

(cl-defstruct (lbry-entry (:constructor lbry-entry-create)
			  (:copier nil))
  "Information about a LBRY video."
  (id "" :read-only t)
  (release-time "" :read-only t)
  (file-date 0 :read-only t)
  (title     "" :read-only t)
  (media-type "" :read-only t)
  (desc "" :read-only t)
  (channel    "" :read-only t))

;;;; Variables

(defvar lbry-entry '(()))

(defvar lbry-api-url "http://localhost:5279"
  "Url to LBRY api.")

(defvar-local lbry-current-page 1
  "Current page of the current `lbry-search-term'")

(defvar-local lbry-search-term ""
  "Current search string as used by `lbry-search'")

(defvar lbry-title-reserved-space 100
  "Number of characters reserved for the video title in the *LBRY* buffer."

(defvar lbry-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'lbry-quit)
    (define-key map "g" #'lbry--draw-buffer)
    (define-key map "h" #'desribe-mode)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "s" #'lbry-search)
    (define-key map "RET" #'lbry-info)
    map))

;;;; Custom

(defgroup lbry nil
  "A LBRY application."
  :group 'comm)

(defcustom lbry-order-by 'name
  "Order to sort the results of the search query. Default is descending order
to do an ascending order prepend ^ to the options"
  :type 'symbol
  :options '(name height release_time publish_time amount effective_amount
		  support_amount trending_group trending_mixed trending_local
		  trending_global activation_height)
  :group 'lbry)
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
	  (error "%s" (buffer-string))))
      (goto-char (point-min))
      (ignore-errors (json-read)))))

;; Many thanks to @Malabarba for this fantastic solution
;; https://emacs.stackexchange.com/questions/3197/best-way-to-retrieve-values-in-nested-assoc-lists
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun lbry--query (string)
  "Query the LBRY blockchain via `lbry-sdk' for STRING, return Nth page of resutls."
  (let ((claims (lbry--API-call "claim_search" `(("text" . ,string)
						 ("page" . ,lbry-current-page)
						 ("claim_type" . "stream")))))
    (with-current-buffer (get-buffer-create "ee")
      (emacs-lisp-mode)
      (erase-buffer)
      (insert (format "%S" claims)))
    ;; Above is for debugging
    (dotimes (i 20)
      (let* ((stream (aref (assoc-recursive claims 'result 'items) i)))
	(aset (assoc-recursive claims 'result 'items) i
	      (lbry-entry-create :id (assoc-recursive stream 'claim_id)
				 :release-time (assoc-recursive stream 'value 'release_time)
				 :file-date (assoc-recursive stream 'timestamp)
				 :title (assoc-recursive stream 'value 'title)
				 :media-type (assoc-recursive stream 'value 'source 'media_type)
				 :channel (or (assoc-recursive stream 'signing_channel 'name)
					      "Anonymous")
				 :desc (assoc-recursive stream 'value 'description)))))
    (assoc-recursive claims 'result 'items)))

;;;;; *LBRY* Buffer
(defun lbry ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*LBRY*"))
  (unless (eq major-mode 'lbry-mode)
    (lbry-mode))
  (when (seq-empty-p lbry-search-term)
    (call-interactively #'lbry-search)))

(defun lbry--format-time (timestamp)
  (format-time-string "%Y-%m-%d" (if (stringp timestamp)
				     (string-to-number timestamp)
				   timestamp)))

(defun lbry--format-duration (seconds)
  "Format `SECONDS' to \"hh:mm:ss\""
  (let ((formatted-string (concat (format-seconds "%.2h" seconds)
				  ":"
				  (format-seconds "%.2m" (mod seconds 3600))
				  ":"
				  (format-seconds "%.2s" (mod seconds 60)))))
    formatted-string))

(defun lbry--insert-entry (claims)
  "Insert `CLAIMS' in the current buffer."
  (insert
   ;; Video type of claim has a special release_type key, use it instead of file date.
   (if (string-match-p (lbry-entry-media-type claims) "video.*")
       (lbry--format-time (lbry-entry-release-time claims))
     (lbry--format-time (lbry-entry-file-date claims)))
   " "
   (lbry-entry-title claims)
   " "
   (lbry-entry-channel claims)
   " "
   (lbry-entry-media-type claims)))


(defun lbry--draw-buffer ()
  (interactive)
  (let ((inhibit-read-only t)
	(current-line (line-number-at-pos)))
    (erase-buffer)
    (setf header-line-format (concat "Search results for "
				     lbry-search-term
				     ", page "
				     (number-to-string lbry-current-page)))
      (seq-do (lambda (v)
		(lbry--insert-entry v)
		(insert "\n"))
	      lbry-entry)
      (goto-char (point-min))))

(defun lbry-search (query)
  "Search the LBRY network for `QUERY', and redraw the buffer."
  (interactive "sSearch terms: ")
  (setf lbry-current-page 1)
  (setf lbry-search-term query)
  (setf lbry-entry (lbry--query query))
  (lbry--draw-buffer))


;;;###autoload

(define-derived-mode lbry-mode text-mode "LBRY"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (make-local-variable 'lbry-entry))

(defun lbry-quit ()
  (interactive)
  (quit-window))

(defun lbry-get-current-claims ()
  (aref lbry-entry (1- (line-number-at-pos))))



(provide 'lbry-mode.el)
