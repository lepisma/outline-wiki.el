;;; outline-wiki.el --- Outline Wiki tools for Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.5
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
(require 'markdown-mode)
(require 'request)

(defcustom outline-wiki-url "https://www.getoutline.com"
  "Root url for outline wiki.")

(defcustom outline-wiki-api-token nil
  "API token for outline wiki.")

(defvar-local outline-wiki-doc nil
  "Buffer local variable for keeping currently shown document.")

(defun outline-wiki-get-token ()
  "Open webpage for token generation."
  (interactive)
  (browse-url (concat outline-wiki-url "/settings/tokens")))

(defun outline-wiki-post-request (request-url data callback)
  "Send post request to outline API."
  (request
   (concat outline-wiki-url request-url)
   :type "POST"
   :headers `(("authorization" . ,(concat "Bearer " outline-wiki-api-token)))
   :data data
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall callback data)))))

(defun outline-wiki-is-share-url (url)
  (string-match-p (concat outline-wiki-url "/share") url))

(defun outline-wiki-get-id-from-url (url)
  "Get identifier from given URL.

This id is share-id or url-id depending on the type of URL."
  (car (last (split-string url "/"))))

(defun outline-wiki-get-doc-from-url (url callback)
  "Get document for the given URL and call CALLBACK on the
document."
  (let ((data (if (outline-wiki-is-share-url url)
                  `(("shareId" . ,(outline-wiki-get-id-from-url url)))
                `(("id" . ,(outline-wiki-get-id-from-url url))))))
    (outline-wiki-post-request "/api/documents.info" data (lambda (data) (funcall callback (alist-get 'data data))))))

(defun outline-wiki-doc-open (doc)
  "Open an outline DOC in a new buffer."
  (let ((buffer (get-buffer-create (concat "*outline-wiki:" (alist-get 'title doc) "*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (alist-get 'text doc))
      (goto-char (point-min))
      (markdown-mode)
      (setq outline-wiki-doc doc)
      (outline-wiki-mode))
    (set-buffer buffer)))

(defun outline-wiki-doc-save (doc)
  "Push given DOC on the API. This unconditionally overwrites the
upstream so be careful with multiple editors."
  (message "Saving document")
  (request
   (concat outline-wiki-url "/api/documents.update")
   :type "POST"
   :headers `(("authorization" . ,(concat "Bearer " outline-wiki-api-token)))
   :data `(("id" . ,(alist-get 'id doc))
           ("text" . ,(buffer-substring-no-properties (point-min) (point-max))))
   :success (lambda (&rest _)
              (message "Document saved"))))

(defun outline-wiki-doc-open-in-browser (doc)
  "Open an outline DOC in default web browser."
  (browse-url (concat outline-wiki-url (alist-get 'url doc))))

(defun outline-wiki-doc-parent (doc &optional all-docs)
  "Pick and return parent of given DOC from ALL-DOCS."
  (when-let ((pid (alist-get 'parentDocumentId doc)))
    (cl-find-if (lambda (other) (string= (alist-get 'id other) pid)) all-docs)))

(defun outline-wiki-qualified-title (doc &optional all-docs)
  "Return fully specified title for given doc.

TODO: Show collection name also in front. That might need caching
      of some sort so not doing right now."
  (if-let ((parent (outline-wiki-doc-parent doc all-docs)))
      (concat (outline-wiki-relative-path parent all-docs) " › " (alist-get 'title doc))
    (alist-get 'title doc)))

;;;###autoload
(defun outline-wiki-doc-open-from-url (url)
  "Open doc from given URL for editing."
  (interactive "sOutline URL: ")
  (outline-wiki-get-doc-from-url url #'outline-wiki-doc-open))

(defun outline-wiki-save ()
  "Save doc showed in current buffer."
  (interactive)
  (if outline-wiki-doc
      (outline-wiki-doc-save outline-wiki-doc)
    (message "No outline document open right now")))

;;;###autoload
(define-minor-mode outline-wiki-mode
  "Minor mode for working with outline wiki documents."
  :init-value nil
  :keymap `((,(kbd "C-x C-s") . outline-wiki-save)))

;;;###autoload
(defun helm-outline-wiki-search (query-term)
  "Actions for outline wiki documents."
  (interactive "sQuery: ")
  (outline-wiki-post-request
   "/api/documents.search"
   `(("query" . ,query-term))
   (lambda (data)
     (let ((documents (mapcar (lambda (item) (alist-get 'document item)) (alist-get 'data data))))
       (helm :sources (helm-build-sync-source "documents"
                        :candidates (mapcar (lambda (doc) (cons (outline-wiki-qualified-title doc documents) doc)) documents)
                        :action `(("Open in buffer" . ,#'outline-wiki-doc-open)
                                  ("Open in browser" . ,#'outline-wiki-doc-open-in-browser)))
             :buffer "*helm outline*"
             :prompt "Open Doc: ")))))

(provide 'outline-wiki)

;;; outline-wiki.el ends here
