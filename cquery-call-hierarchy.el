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

(defface cquery-call-hierarchy-node-normal-face
  nil
  "."
  :group 'cquery)

(defface cquery-call-hierarchy-node-base-face
  '((t (:foreground "orange red")))
  "."
  :group 'cquery)

(defface cquery-call-hierarchy-node-derived-face
  '((t (:foreground "orange")))
  "."
  :group 'cquery)

(defcustom cquery-call-hierarchy-use-detailed-name nil
  "Use detailed name for call hierarchy"
  :group 'cquery
  :type 'boolean)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct cquery-call-hierarchy-node
  id
  name
  call-type)

(defun cquery-call-hierarchy--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (-let* ((location (gethash "location" data))
          (filename (lsp--uri-to-path (gethash "uri" location)))
          ((&hash "id" id "name" name "callType" call-type) data))
    (make-cquery-tree-node
     :location (cons filename (gethash "start" (gethash "range" location)))
     :has-children (< 0 (gethash "numChildren" data))
     :parent parent
     :expanded nil
     :children nil
     :data (make-cquery-call-hierarchy-node
            :id id
            :name name
            :call-type call-type))))

(defun cquery-call-hierarchy--request-children (callee node)
  "."
  (let ((id (cquery-call-hierarchy-node-id (cquery-tree-node-data node))))
    (--map (cquery-call-hierarchy--read-node it node)
           (gethash "children"
                    (lsp--send-request
                     (lsp--make-request "$cquery/callHierarchy"
                                        `(:id ,id
                                              :callee ,callee
                                              :callType 3
                                              :levels ,cquery-tree-initial-levels
                                              :detailedName ,(if cquery-call-hierarchy-use-detailed-name t :json-false)
                                              )))))))

(defun cquery-call-hierarchy--request-init (callee)
  "."
  (cquery--cquery-buffer-check)
  (lsp--send-request
   (lsp--make-request "$cquery/callHierarchy"
                      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                                      :position ,(lsp--cur-position)
                                      :callee ,callee
                                      :callType 3
                                      :detailedName ,(if cquery-call-hierarchy-use-detailed-name t :json-false)
                                      ))))

(defun cquery-call-hierarchy--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((data (cquery-tree-node-data node)))
    (if (= depth 0)
        (cquery-call-hierarchy-node-name data)
      (concat
       (propertize (cquery-call-hierarchy-node-name data)
                   'face (pcase (cquery-call-hierarchy-node-call-type data)
                           ('0 'cquery-call-hierarchy-node-normal-face)
                           ('1 'cquery-call-hierarchy-node-base-face)
                           ('2 'cquery-call-hierarchy-node-derived-face)))
       (propertize (format " (%s:%s)"
                           (file-name-nondirectory (car (cquery-tree-node-location node)))
                           (gethash "line" (cdr (cquery-tree-node-location node))))
                   'face 'cquery-mode-line-face)))))

(defun cquery-call-hierarchy (callee)
  (interactive "P")
  (cquery--cquery-buffer-check)
  (setq callee (if callee t :json-false))
  (cquery-tree--open
   (make-cquery-tree-client
    :name "call hierarchy"
    :mode-line-format (format " %s %s %s %s"
                              (propertize (if (eq callee t) "Callee types:" "Caller types:") 'face 'cquery-tree-mode-line-face)
                              (propertize "Normal" 'face 'cquery-call-hierarchy-node-normal-face)
                              (propertize "Base" 'face 'cquery-call-hierarchy-node-base-face)
                              (propertize "Derived" 'face 'cquery-call-hierarchy-node-derived-face))
    :top-line-f (lambda () (propertize (if (eq callee t) "Callees of " "Callers of") 'face 'cquery-tree-mode-line-face))
    :make-string-f 'cquery-call-hierarchy--make-string
    :read-node-f 'cquery-call-hierarchy--read-node
    :request-children-f (apply-partially #'cquery-call-hierarchy--request-children callee)
    :request-init-f (lambda () (cquery-call-hierarchy--request-init callee)))))

(provide 'cquery-call-hierarchy)
;;; cquery-call-hierarchy.el ends here
