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

(defcustom cquery-member-hierarchy-use-detailed-name t
  "Use detailed name for member hierarchy"
  :group 'cquery
  :type 'boolean)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct cquery-member-hierarchy-node
  name
  field-name
  id)

(defun cquery-member-hierarchy--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (-let* (((&hash "location" location "numChildren" nchildren "name" name "fieldName" field-name "id" id "children" children) data)
          (filename (lsp--uri-to-path (gethash "uri" location)))
          (node
           (make-cquery-tree-node
            :location (cons filename (gethash "start" (gethash "range" location)))
            ;; With a little bit of luck, this only filters out enums
            :has-children (not (or (>= 0 nchildren)
                                   (null parent)
                                   (= id (cquery-member-hierarchy-node-id (cquery-tree-node-data parent)))))
            :parent parent
            :expanded nil
            :children nil
            :data (make-cquery-member-hierarchy-node
                   :name name
                   :field-name field-name
                   :id id))))
    (setf (cquery-tree-node-children node)
          (--map (cquery-member-hierarchy--read-node it node)
                 children))
    node))

(defun cquery-member-hierarchy--request-children (node)
  "."
  (let ((id (cquery-member-hierarchy-node-id (cquery-tree-node-data node))))
    (--map (cquery-member-hierarchy--read-node it node)
           (gethash "children" (lsp--send-request
                                (lsp--make-request "$cquery/memberHierarchy"
                                                   `(:id ,id
                                                         :levels ,cquery-tree-initial-levels
                                                         :detailedName ,(if cquery-member-hierarchy-use-detailed-name t :json-false))))))))

(defun cquery-member-hierarchy--request-init ()
  "."
  (cquery--cquery-buffer-check)
  (lsp--send-request
   (lsp--make-request "$cquery/memberHierarchy"
                      `(
                        :textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                        :position ,(lsp--cur-position)
                        :levels 1
                        :detailedName ,(if cquery-member-hierarchy-use-detailed-name t :json-false)))))

(defun cquery-member-hierarchy--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((data (cquery-tree-node-data node)))
    (cquery--render-string (if (eq depth 0)
                               (cquery-member-hierarchy-node-name data)
                             (cquery-member-hierarchy-node-field-name data)))))

(defun cquery-member-hierarchy ()
  (interactive)
  (cquery--cquery-buffer-check)
  (cquery-tree--open
   (make-cquery-tree-client
    :name "member hierarchy"
    :mode-line-format (propertize "Member hierarchy" 'face 'cquery-tree-mode-line-face)
    :top-line-f (lambda () (propertize "Members of" 'face 'cquery-tree-mode-line-face))
    :make-string-f 'cquery-member-hierarchy--make-string
    :read-node-f 'cquery-member-hierarchy--read-node
    :request-children-f 'cquery-member-hierarchy--request-children
    :request-init-f 'cquery-member-hierarchy--request-init)))

(provide 'cquery-member-hierarchy)
;;; cquery-member-hierarchy.el ends here
