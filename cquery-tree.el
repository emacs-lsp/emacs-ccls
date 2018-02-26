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

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defcustom cquery-tree-initial-levels 2
  "."
  :group 'cquery)

(defface cquery-tree-root-face
  '((t (:height 1.5 :line-height 2.0)))
  "."
  :group 'cquery)

(defface cquery-tree-mouse-face
  '((t (:background "green")))
  "."
  :group 'cquery)

(defface cquery-tree-icon-face
  '((t (:foreground "grey")))
  "."
  :group 'cquery)

(defface cquery-tree-header-line-face
  '((t (:foreground "grey" :height 0.8)))
  "."
  :group 'cquery)

(defface cquery-tree-mode-line-face
  '((t (:foreground "grey" :slant italic)))
  "."
  :group 'cquery)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct cquery-tree-node
  parent ;; parent node
  has-children ;; whether this node has children
  children ;; a list of child nodes, if already requested
  expanded ;; is this node expanded?
  location ;; (filename . (hashmap '(("line" . line) ("char" . char))). Location to go to on click
  data ;; Arbitrary, client defined data
  )

(cl-defstruct cquery-tree-client
  name ;; name of client
  mode-line-format ;; mode-line format
  header-line-format ;; header-line format
  top-line-f ;; Function to draw in front of the first line
  make-string-f ;; Function to get the string for a node. form: (node depth)
  read-node-f ;; Function to read a node from a hashmap. form: (hashmap &optional parent)
  request-children-f ;; Function to request children for a node. form: (node)
  request-init-f ;; Function to request initial nodes. form: ()
  )

(defvar-local cquery-tree--cur-client nil
  "Buffer tree client.")

(defun cquery-tree--read-node (data &optional parent)
  (funcall (cquery-tree-client-read-node-f cquery-tree--cur-client) data parent))

(defun cquery-tree--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (funcall (cquery-tree-client-make-string-f cquery-tree--cur-client) node depth))

(defun cquery-tree-node--request-children (node)
  (funcall (cquery-tree-client-request-children-f cquery-tree--cur-client) node))

(defun cquery-tree--request-init (client)
  (funcall (cquery-tree-client-request-init-f client)))

(defun cquery-tree--draw-top-line ()
  (-some-> (cquery-tree-client-top-line-f cquery-tree--cur-client)
           (funcall)
           (concat "\n")
           (insert)))

;; ---------------------------------------------------------------------
;;   Visualization
;; ---------------------------------------------------------------------

(defvar-local cquery-tree--root-nodes nil
  ".")

(defvar-local cquery-tree--visible-root nil
  ".")

(defvar-local cquery-tree--origin-win nil
  "Window that the current call tree was opened from.")

(defun cquery-tree--refresh ()
  (let ((p (point)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setf (cquery-tree-node-expanded cquery-tree--visible-root) t)
    (cquery-tree--draw-top-line)
    (cquery-tree--insert-node cquery-tree--visible-root 0 1 0)
    (goto-char p)
    (setq buffer-read-only t)))

(defun cquery-tree--insert-node (node number nchildren depth)
  (let* ((prefix (cquery-tree--make-prefix node number nchildren depth))
         (name (cquery-tree--make-string node depth)))
    (insert (if (= depth 0)
                (propertize (concat prefix name "\n")
                            'depth depth
                            'face 'cquery-tree-root-face
                            'cquery-tree-node node)
              (propertize (concat prefix name "\n")
                          'depth depth
                          'cquery-tree-node node)))
    (when (or (cquery-tree-node-expanded node) (< depth cquery-tree-initial-levels))
      (when (and (cquery-tree-node-has-children node)
                 (null (cquery-tree-node-children node)))
        (setf (cquery-tree-node-children node)
              (cquery-tree-node--request-children node)))
      (let ((children (cquery-tree-node-children node)))
        (--map-indexed (cquery-tree--insert-node it it-index (length children) (+ depth 1))
                       children)))))

(defun cquery-tree--make-prefix (node number nchildren depth)
  "."
  (let* ((padding (if (= depth 0) "" (make-string (* 2 (- depth 1)) ?\ )))
         (symbol (if (= depth 0)
                     (if (cquery-tree-node-parent node)
                         "⏴ "
                       "")
                   (if (cquery-tree-node-has-children node)
                       (if (cquery-tree-node-expanded node) "└⏷" "└⏵")
                     (if (eq number (- nchildren 1)) "└╸" "├╸")))))
    (concat padding (propertize symbol 'face 'cquery-tree-icon-face))))

(defun cquery-tree--open (client)
  "."
  (let ((lsp-ws lsp--cur-workspace)
        (root-node-data (cquery-tree--request-init client))
        (orig-buf (current-buffer))
        (bufname (format "*cquery-tree %s*" (cquery-tree-client-name client))))
    (with-current-buffer (get-buffer-create bufname)
      (cquery-tree-mode)
      (setq lsp--cur-workspace lsp-ws
            cquery-tree--cur-client client
            cquery-tree--origin-win (get-buffer-window orig-buf)
            cquery-tree--root-nodes (when root-node-data (cquery-tree--read-node root-node-data))
            cquery-tree--visible-root cquery-tree--root-nodes)
      (when (null cquery-tree--root-nodes)
        (user-error "Couldn't open tree from point"))
      (cquery-tree--refresh)
      (setq header-line-format (cquery-tree-client-header-line-format cquery-tree--cur-client))
      (setq mode-line-format (cquery-tree-client-mode-line-format cquery-tree--cur-client))
      (goto-char 1))
    (let ((win (display-buffer-in-side-window (get-buffer bufname) '((side . right)))))
      (set-window-margins win 1)
      (select-window win)
      (set-window-start win 1)
      (set-window-dedicated-p win t))))

(defun cquery-tree--node-at-point ()
  (get-text-property (point) 'cquery-tree-node))

(defun cquery-tree--depth-at-point ()
  (get-text-property (point) 'depth))

;; ---------------------------------------------------------------------
;;   Actions
;; ---------------------------------------------------------------------

(defun cquery-tree-toggle-expand ()
  "Toggle expansion of node at point"
  (interactive)
  (-when-let* ((node (cquery-tree--node-at-point)))
    (setf (cquery-tree-node-expanded node)
          (or (not (cquery-tree-node-expanded node))
              (= 0 (cquery-tree--depth-at-point))))
    (cquery-tree--refresh)))

(defun cquery-tree-select-parent ()
  "."
  (interactive)
  (let ((depth (cquery-tree--depth-at-point)))
    (if (null depth)
        (forward-line -1)
      (if (> depth 0)
          (while (and (>= (cquery-tree--depth-at-point) depth)
                      (= 0 (forward-line -1))))
        (-when-let* ((parent (cquery-tree-node-parent (cquery-tree--node-at-point))))
          (setq cquery-tree--visible-root parent)
          (cquery-tree--refresh))))))

(defun cquery-tree-set-root ()
  "Set root to current node"
  (interactive)
  (-when-let* ((node (cquery-tree--node-at-point)))
    (when (cquery-tree-node-has-children node)
      (setq cquery-tree--visible-root node)
      (setf (cquery-tree-node-expanded node) t)
      (cquery-tree--refresh))))

(defun cquery-tree-go ()
  "Go to the definition of the function at point"
  (interactive)
  (-when-let* ((node (cquery-tree--node-at-point)))
    (select-window cquery-tree--origin-win)
    (find-file (car (cquery-tree-node-location node)))
    (goto-char (lsp--position-to-point (cdr (cquery-tree-node-location node))))
    (pulse-momentary-highlight-one-line (point) 'next-error)))

(defun cquery-tree-look ()
  "Look at the definition of function at point"
  (interactive)
  (-when-let* ((node (cquery-tree--node-at-point)))
    (with-selected-window cquery-tree--origin-win
      (find-file (car (cquery-tree-node-location node)))
      (goto-char (lsp--position-to-point (cdr (cquery-tree-node-location node))))
      (recenter)
      (pulse-momentary-highlight-one-line (point) 'next-error))))

(defun cquery-tree-expand-or-set-root ()
  "If the node at point is unexpanded expand it, otherwise set it as root"
  (interactive)
  (-when-let* ((node (cquery-tree--node-at-point)))
    (when (cquery-tree-node-has-children node)
      (if (cquery-tree-node-expanded node)
          (cquery-tree-set-root)
        (cquery-tree-toggle-expand)))))

(defun cquery-tree-collapse-or-select-parent ()
  "If the node at point is expanded collapse it, otherwise select its parent"
  (interactive)
  (-when-let* ((node (cquery-tree--node-at-point)))
    (if (and (> (cquery-tree--depth-at-point) 0)
             (cquery-tree-node-expanded node))
        (cquery-tree-toggle-expand)
      (cquery-tree-select-parent))))

;; ---------------------------------------------------------------------
;;   Mode
;; ---------------------------------------------------------------------

(defvar cquery-tree-mode-map nil
  "Keymap used with ‘cquery-tree-mode’.")

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "<tab>") 'cquery-tree-toggle-expand)
  (define-key map [mouse-1] 'cquery-tree-toggle-expand )
  (define-key map (kbd "<return>") 'cquery-tree-go)
  (define-key map (kbd "C-<return>") 'cquery-tree-look)
  (define-key map (kbd "<left>") 'cquery-tree-collapse-or-select-parent)
  (define-key map (kbd "<right>") 'cquery-tree-expand-or-set-root)
  (setq cquery-tree-mode-map map))

(define-derived-mode cquery-tree-mode special-mode "cquery-tree"
  "Mode for cquery tree buffers")

(provide 'cquery-tree)
