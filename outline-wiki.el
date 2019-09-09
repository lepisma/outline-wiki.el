;;; outline-wiki.el --- Outline Wiki tools for Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.3
;; Package-Requires: ((emacs "26"))
;; URL: https://github.com/lepisma/outline-wiki.el

;;; Commentary:

;; Outline Wiki tools for Emacs
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'helm)
(require 'org)
(require 'request)

(defcustom outline-wiki-url "https://www.getoutline.com"
  "Root url for outline wiki.")

(defcustom outline-wiki-api-token nil
  "API token for outline wiki.")

(defun outline-wiki-get-token ()
  "Open webpage for token generation."
  (interactive)
  (browse-url (concat outline-wiki-url "/settings/tokens")))

(defun outline-wiki-doc-open (doc)
  "Open an outline doc in a new buffer."
  (let ((buffer (get-buffer-create (concat "*outline-wiki:" (alist-get 'title doc) "*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (alist-get 'text doc))
      (shell-command-on-region (point-min) (point-max) (format "pandoc -f markdown -t org") buffer t)
      (org-mode)
      (outline-show-all)
      (deactivate-mark)
      (goto-char (point-min))
      (read-only-mode))
    (set-buffer buffer)))

(defun outline-wiki-doc-open-in-browser (doc)
  "Open an outline doc in default web browser."
  (browse-url (concat outline-wiki-url (alist-get 'url doc))))

(defun helm-outline-wiki-doc ()
  "Actions for outline wiki documents."
  (interactive)
  (request
   (concat outline-wiki-url "/api/documents.list")
   :type "POST"
   :headers `(("authorization" . ,(concat "Bearer " outline-wiki-api-token)))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let ((documents (alist-get 'data data)))
                 (helm :sources (helm-build-sync-source "documents"
                                  :candidates (mapcar (lambda (doc) (cons (alist-get 'title doc) doc)) documents)
                                  :action `(("Open in buffer" . ,#'outline-wiki-doc-open)
                                            ("Open in browser" . ,#'outline-wiki-doc-open-in-browser)))
                       :buffer "*helm outline*"
                       :prompt "Open Doc : "))))))

(provide 'outline-wiki)

;;; outline-wiki.el ends here
