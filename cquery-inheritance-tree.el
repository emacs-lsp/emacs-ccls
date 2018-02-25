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


(defface cquery-inheritance-tree-base-face
  '((t (:foreground "orange red")))
  "."
  :group 'cquery)

(cl-defstruct cquery-inheritance-tree-node
  name)

(defun cquery-inheritance-tree--read-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (let* ((location (gethash "location" data '(nil . nil)))
         (filename (string-remove-prefix lsp--uri-file-prefix (gethash "uri" location)))
         (name (gethash "name" data))
         (node
          (make-cquery-tree-node
           :location (cons filename (gethash "start" (gethash "range" location)))
           :has-children (not (null (gethash "children" data nil)))
           :parent parent
           :expanded (string-equal name "[[Base]]")
           :children nil
           :data (make-cquery-inheritance-tree-node
                  :name name))))
    (setf (cquery-tree-node-children node)
          (--map (cquery-inheritance-tree--read-node it node)
                 (gethash "children" data)))
    node))

(defun cquery-inheritance-tree--request-children (_node)
  "."
  nil)

(defun cquery-inheritance-tree--request-init ()
  "."
  (cquery--cquery-buffer-check)
  (list
   (lsp--send-request
    (lsp--make-request "$cquery/inheritanceHierarchy"
                       `(
                         :textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                         :position ,(lsp--cur-position))))))

(defun cquery-inheritance-tree--make-string (node _depth)
  "Propertize the name of NODE with the correct properties"
  (let* ((data (cquery-tree-node-data node))
         (name (cquery-inheritance-tree-node-name data)))
    (if (string-equal name "[[Base]]")
        (propertize "Bases" 'face 'cquery-inheritance-tree-base-face)
      (cquery--render-string name))))

(defun cquery-inheritance-tree ()
  (interactive)
  (cquery--cquery-buffer-check)
  (cquery-tree--open
   (make-cquery-tree-client
    :name "inheritance hierarchy"
    :mode-line-format (propertize "Inheritance hierarchy" 'face 'cquery-tree-mode-line-face)
    :make-string-f 'cquery-inheritance-tree--make-string
    :read-node-f 'cquery-inheritance-tree--read-node
    :request-children-f 'cquery-inheritance-tree--request-children
    :request-init-f 'cquery-inheritance-tree--request-init)))

(provide 'cquery-inheritance-tree)
;;; cquery-inheritance-tree.el ends here
