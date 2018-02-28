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


(defface cquery-inheritance-hierarchy-base-face
  '((t (:foreground "orange red")))
  "."
  :group 'cquery)

(defcustom cquery-inheritance-hierarchy-use-detailed-name t
  "Use detailed name for types in inheritance hierarchy"
  :group 'cquery
  :type 'boolean)

(cl-defstruct cquery-inheritance-hierarchy-node
  id
  kind
  name)

(defun cquery-inheritance-hierarchy--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (-let* ((location (gethash "location" data '(nil . nil)))
          (filename (lsp--uri-to-path (gethash "uri" location)))
          ((&hash "id" id "kind" kind "name" name) data)
          (node
           (make-cquery-tree-node
            :location (cons filename (gethash "start" (gethash "range" location)))
            :has-children (< 0 (gethash "numChildren" data))
            :parent parent
            :expanded nil
            :children nil
            :data (make-cquery-inheritance-hierarchy-node
                   :id id
                   :kind kind
                   :name name))))
    (setf (cquery-tree-node-children node)
          (--map (cquery-inheritance-hierarchy--read-node it node)
                 (gethash "children" data)))
    node))

(defun cquery-inheritance-hierarchy--request-children (derived node)
  "."
  (let ((id (cquery-inheritance-hierarchy-node-id (cquery-tree-node-data node)))
        (kind (cquery-inheritance-hierarchy-node-kind (cquery-tree-node-data node))))
    (--map (cquery-inheritance-hierarchy--read-node it node)
           (gethash "children"
                    (lsp--send-request
                     (lsp--make-request "$cquery/inheritanceHierarchy"
                                        `(:id ,id :kind ,kind
                                              :derived ,derived
                                              :detailedName ,(if cquery-inheritance-hierarchy-use-detailed-name t :json-false)
                                              :levels ,cquery-tree-initial-levels)))))))

(defun cquery-inheritance-hierarchy--request-init (derived)
  "."
  (cquery--cquery-buffer-check)
  (lsp--send-request
   (lsp--make-request "$cquery/inheritanceHierarchy"
                      `(
                        :textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                        :position ,(lsp--cur-position)

                        :derived ,derived
                        :detailedName ,(if cquery-inheritance-hierarchy-use-detailed-name t :json-false)
                        :levels 1))))

(defun cquery-inheritance-hierarchy--make-string (node _depth)
  "Propertize the name of NODE with the correct properties"
  (let* ((data (cquery-tree-node-data node))
         (name (cquery-inheritance-hierarchy-node-name data)))
    (if (string-equal name "[[Base]]")
        (propertize "Bases" 'face 'cquery-inheritance-hierarchy-base-face)
      (cquery--render-type name))))

(defun cquery-inheritance-hierarchy (derived)
  (interactive "P")
  (cquery--cquery-buffer-check)
  (let ((json-derived (if derived t :json-false)))
    (cquery-tree--open
     (make-cquery-tree-client
      :name "inheritance hierarchy"
      :mode-line-format (propertize (if derived
                                        "Inheritance Hierarchy: Subclasses"
                                      "Inheritance Hierarchy: Bases")
                                    'face 'cquery-tree-mode-line-face)
      :top-line-f (lambda () (propertize (if derived "Derive from" "Bases of") 'face 'cquery-tree-mode-line-face))
      :make-string-f 'cquery-inheritance-hierarchy--make-string
      :read-node-f 'cquery-inheritance-hierarchy--read-node
      :request-children-f (apply-partially #'cquery-inheritance-hierarchy--request-children json-derived)
      :request-init-f (lambda () (cquery-inheritance-hierarchy--request-init json-derived))))))

(provide 'cquery-inheritance-hierarchy)
;;; cquery-inheritance-hierarchy.el ends here
