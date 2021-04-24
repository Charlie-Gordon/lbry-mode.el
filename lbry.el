;;; lbry.el --- Application for LBRY                 -*- lexical-binding: t; -*-

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

;; User Variables
(defgroup lbry nil
  "A LBRY application."
  :group 'comm)

(defvar lbry-api-url "http://localhost:5279"
  "Url to LBRY api.")
;; (defun lbry-search (query)
;;   "Search LBRY blockchain for `QUERY'"
;;   (interactive "sSearch terms: ")
  
(defun lbry--jsonrpc-query-request (query)
  (mapconcat
   (lambda (key-vals)
     (let ((escaped
            (mapcar (lambda (sym)
                      (format "%s" sym)) key-vals)))
       (mapconcat (lambda (val)
                    (let ((vprint (format "%s" val))
                          (eprint (format "%s" (car escaped))))
                      (concat eprint
                              (if (or nil
                                      (and val (not (zerop (length vprint)))))
                                  ":"
                                "")
                              vprint)))
                  (or (cdr escaped) '("")) (if nil ";" "&"))))
   query ","))
  

(defun ytel--query (string n)
  "Query LBRY for STRING, return the Nth page of results."


(defun lbry--API-call (method args)
  (with-temp-buffer
    (let ((exit-code (call-process "curl" nil (list (current-buffer) "/tmp/lbry-el-curl-error") nil
				   "--show-error"
				   "--silent"
				   "--data"
				   (concat "{" (lbry--jsonrpc-query-request '((\"method\" \"claim_search\")
		    (\"text\" \"luke\"))) "}")
					   lbry-api-url)))
	  (unless (= exit-code 0)
	    (with-temp-buffer
	      (insert-file "/tmp/lbry-el-curl-error")
	      (error "%s" (buffer-string))))
	  (goto-char (point-min))
	  (ignore-errors (json-read))))))
;; "{\"method\":\"claim_search\",\"text\":\"luke\"}"
