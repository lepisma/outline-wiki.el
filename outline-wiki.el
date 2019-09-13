;;; outline-wiki.el --- Outline Wiki tools for Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.4
;; Package-Requires: ((emacs "26") (helm "3.3") (request "0.3.1"))
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

(require 'cl-lib)
(require 'helm)
(require 'org)
(require 'request)

(defcustom outline-wiki-url "https://www.getoutline.com"
  "Root url for outline wiki.")

(defcustom outline-wiki-api-token nil
  "API token for outline wiki.")

(defvar-local outline-wiki-buffer-doc nil
  "Buffer local variable for keeping currently shown document.")

(defun outline-wiki-get-token ()
  "Open webpage for token generation."
  (interactive)
  (browse-url (concat outline-wiki-url "/settings/tokens")))

(defun outline-wiki-doc-open (doc)
  "Open an outline DOC in a new buffer."
  (let ((buffer (get-buffer-create (concat "*outline-wiki:" (alist-get 'title doc) "*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (alist-get 'text doc))
      (shell-command-on-region (point-min) (point-max) "pandoc -f markdown -t org" buffer t)
      (org-mode)
      (outline-show-all)
      (deactivate-mark)
      (goto-char (point-min))
      (setq outline-wiki-buffer-doc doc))
    (set-buffer buffer)))

(defun outline-wiki-doc-to-md ()
  "Convert a doc buffer to markdown."
  (let ((conv-buffer (get-buffer-create "*outline-wiki-conv*")))
    (shell-command-on-region (point-min) (point-max) "pandoc -f org -t gfm" conv-buffer)
    (with-current-buffer conv-buffer
      (prog1 (buffer-string)
        (kill-buffer conv-buffer)))))

(defun outline-wiki-doc-update ()
  "Update doc shown in current buffer."
  (request
   (concat outline-wiki-url "/api/documents.update")
   :type "POST"
   :headers `(("authorization" . ,(concat "Bearer " outline-wiki-api-token)))
   :data `(("id" . ,(alist-get 'id outline-wiki-buffer-doc))
           ("text" . ,(outline-wiki-doc-to-md)))
   :success (cl-function
             (lambda (&allow-other-keys)
               (message "Changes saved.")))))

(defun outline-wiki-doc-open-in-browser (doc)
  "Open an outline DOC in default web browser."
  (browse-url (concat outline-wiki-url (alist-get 'url doc))))

(defun outline-wiki-doc-parent (doc &optional all-docs)
  "Pick and return parent of given DOC from ALL-DOCS."
  (when-let ((pid (alist-get 'parentDocumentId doc)))
    (cl-find-if (lambda (other) (string= (alist-get 'id other) pid)) all-docs)))

(defun outline-wiki-relative-path (doc &optional all-docs)
  "Return relative `/' joined path for given doc."
  (if-let ((parent (outline-wiki-doc-parent doc all-docs)))
      (concat (outline-wiki-relative-path parent all-docs) " / " (alist-get 'title doc))
    (alist-get 'title doc)))

;;;###autoload
(defun helm-outline-wiki-doc (query-term)
  "Actions for outline wiki documents."
  (interactive "sQuery: ")
  (request
   (concat outline-wiki-url "/api/documents.search")
   :type "POST"
   :data `(("query" . ,query-term))
   :headers `(("authorization" . ,(concat "Bearer " outline-wiki-api-token)))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let ((documents (mapcar (lambda (item) (alist-get 'document item)) (alist-get 'data data))))
                 (helm :sources (helm-build-sync-source "documents"
                                  :candidates (mapcar (lambda (doc) (cons (outline-wiki-relative-path doc documents) doc)) documents)
                                  :action `(("Open in buffer" . ,#'outline-wiki-doc-open)
                                            ("Open in browser" . ,#'outline-wiki-doc-open-in-browser)))
                       :buffer "*helm outline*"
                       :prompt "Open Doc: "))))))

(provide 'outline-wiki)

;;; outline-wiki.el ends here
