;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cquery-common)
(require 'cquery-tree)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defface cquery-call-tree-node-normal-face
  nil
  "."
  :group 'cquery)

(defface cquery-call-tree-node-base-face
  '((t (:foreground "orange red")))
  "."
  :group 'cquery)

(defface cquery-call-tree-node-derived-face
  '((t (:foreground "orange")))
  "."
  :group 'cquery)


;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct cquery-call-tree-node
  name
  usr
  call-type)

(defun cquery-call-tree--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (let* ((location (gethash "location" data))
         (filename (string-remove-prefix lsp--uri-file-prefix (gethash "uri" location))))
    (make-cquery-tree-node
     :location (cons filename (gethash "start" (gethash "range" location)))
     :has-children (gethash "hasCallers" data)
     :parent parent
     :expanded nil
     :children nil
     :data (make-cquery-call-tree-node
            :name (gethash "name" data)
            :usr (gethash "usr" data)
            :call-type (gethash "callType" data)))))

(defun cquery-call-tree--request-children (node)
  "."
  (let ((usr (cquery-call-tree-node-usr (cquery-tree-node-data node))))
    (when (string-match-p "^[0-9]+$" usr) ;; usr is no-usr for constructors
      (--map (cquery-call-tree--read-node it node)
             (lsp--send-request
              (lsp--make-request "$cquery/callTreeExpand"
                                 `(:usr ,usr))))
      )))

(defun cquery-call-tree--request-init ()
  "."
  (cquery--cquery-buffer-check)
  (lsp--send-request
   (lsp--make-request "$cquery/callTreeInitial"
                      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                                      :position ,(lsp--cur-position)))))

(defun cquery-call-tree--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((data (cquery-tree-node-data node)))
    (if (= depth 0)
        (cquery-call-tree-node-name data)
      (concat
       (propertize (cquery-call-tree-node-name data)
                   'face (pcase (cquery-call-tree-node-call-type data)
                           ('0 'cquery-call-tree-node-normal-face)
                           ('1 'cquery-call-tree-node-base-face)
                           ('2 'cquery-call-tree-node-derived-face)))
       (propertize (format " (%s:%s)"
                           (file-name-nondirectory (car (cquery-tree-node-location node)))
                           (gethash "line" (cdr (cquery-tree-node-location node))))
                   'face 'cquery-call-tree-mode-line-face)))))

(defun cquery-call-tree ()
  (interactive)
  (cquery--cquery-buffer-check)
  (cquery-tree--open
   (make-cquery-tree-client
    :name "call hierarchy"
    :mode-line-format (format " %s %s %s %s"
                              (propertize "Caller types:" 'face 'cquery-tree-mode-line-face)
                              (propertize "Normal" 'face 'cquery-call-tree-node-normal-face)
                              (propertize "Base" 'face 'cquery-call-tree-node-base-face)
                              (propertize "Derived" 'face 'cquery-call-tree-node-derived-face))
    :top-line-f (lambda () (propertize "Callers of" 'face '(:height 1.0 :inherit cquery-tree-mode-line-face)))
    :make-string-f 'cquery-call-tree--make-string
    :read-node-f 'cquery-call-tree--read-node
    :request-children-f 'cquery-call-tree--request-children
    :request-init-f 'cquery-call-tree--request-init)))

(provide 'cquery-call-tree)
;;; cquery-call-tree.el ends here
