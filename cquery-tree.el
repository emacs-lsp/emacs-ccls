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

(require 'xref)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defgroup cquery-tree nil
  "cquery tree."
  :group 'tools
  :group 'cquery)

(defcustom cquery-tree-initial-levels 2
  "."
  :group 'cquery-tree)

(defface cquery-tree-root-face
  '((t (:height 1.5 :line-height 2.0)))
  "."
  :group 'cquery-tree)

(defface cquery-tree-mouse-face
  '((t (:background "green")))
  "."
  :group 'cquery-tree)

(defface cquery-tree-icon-face
  '((t (:foreground "grey")))
  "."
  :group 'cquery-tree)

(defface cquery-tree-header-line-face
  '((t (:foreground "grey" :height 0.8)))
  "."
  :group 'cquery-tree)

(defface cquery-tree-mode-line-face
  '((t (:foreground "grey" :slant italic)))
  "."
  :group 'cquery-tree)

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

(defvar-local cquery-tree-calling t
  "When non-nil, visit the node when the selected line changes.")

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

(defvar-local cquery-tree--origin-buffer nil
  "Buffer that the tree was opened from.")
(defvar-local cquery-tree--origin-win nil
  "Win that the tree was opened from.")

(defvar-local cquery-tree--opoint 1
  "The original point of the buffer that the tree was opened from.")

(defun cquery-tree--refresh ()
  (let ((p (point))
        (inhibit-read-only t))
    (erase-buffer)
    (setf (cquery-tree-node-expanded cquery-tree--visible-root) t)
    (cquery-tree--draw-top-line)
    (cquery-tree--insert-node cquery-tree--visible-root 0 1 0)
    (goto-char p)))

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
    (when (or (cquery-tree-node-expanded node))
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

(defun cquery-tree--expand-levels (node levels)
  "Expand NODE and its children LEVELS down"
  (when (> levels 0)
    (setf (cquery-tree-node-expanded node) t)
    (--map (cquery-tree--expand-levels it (- levels 1))
           (cquery-tree-node-children node))))

(defun cquery-tree--open (client)
  "."
  (let ((opoint (point))
        (lsp-ws lsp--cur-workspace)
        (root-node-data (cquery-tree--request-init client))
        (orig-buf (current-buffer))
        (bufname (format "*cquery-tree %s*" (cquery-tree-client-name client))))
    (with-current-buffer (get-buffer-create bufname)
      (cquery-tree-mode)
      (setq lsp--cur-workspace lsp-ws
            cquery-tree--cur-client client
            cquery-tree--origin-buffer orig-buf
            cquery-tree--origin-win (get-buffer-window orig-buf)
            cquery-tree--opoint opoint
            cquery-tree--root-nodes (when root-node-data (cquery-tree--read-node root-node-data))
            cquery-tree--visible-root cquery-tree--root-nodes)
      (when (null cquery-tree--root-nodes)
        (user-error "Couldn't open tree from point"))
      (cquery-tree--refresh)
      (cquery-tree--expand-levels cquery-tree--visible-root cquery-tree-initial-levels)
      (cquery-tree--refresh)
      (setq header-line-format (cquery-tree-client-header-line-format cquery-tree--cur-client))
      (setq mode-line-format (cquery-tree-client-mode-line-format cquery-tree--cur-client))
      (goto-char 1)
      (forward-line))
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

(defun cquery-tree-toggle-calling ()
  "Toggle `cquery-tree-calling'."
  (interactive)
  (when (setq cquery-tree-calling (not cquery-tree-calling))
    (cquery-tree-press)))

(defun cquery-tree-press (&optional split-fn)
  "Jump to the location."
  (interactive)
  (-when-let* ((workspace lsp--cur-workspace)
               (node (cquery-tree--node-at-point))
               (_ (window-live-p cquery-tree--origin-win)))
    (with-selected-window cquery-tree--origin-win
      (when split-fn
        (funcall split-fn))
      (find-file (car (cquery-tree-node-location node)))
      ;; TODO Extract lsp-ui-peek.el lsp-ui-peek--goto-xref
      (unless lsp--cur-workspace
        (setq lsp--cur-workspace workspace))
      (unless lsp-mode
        (lsp-mode 1)
        (lsp-on-open))
      (goto-char (lsp--position-to-point (cdr (cquery-tree-node-location node))))
      (recenter)
      (run-hooks 'xref-after-jump-hook))))

(defun cquery-tree-press-and-switch ()
  "Switch window and jump to the location."
  (interactive)
  (cquery-tree-press)
  (when (window-live-p cquery-tree--origin-win)
    (select-window cquery-tree--origin-win)))

(defun cquery-tree-press-and-horizontal-split ()
  "Split window horizontally and jump to the location."
  (interactive)
  (cquery-tree-press #'split-window-horizontally)
  (when (window-live-p cquery-tree--origin-win)
    (select-window cquery-tree--origin-win)))

(defun cquery-tree-press-and-vertical-split ()
  "Split window vertically and jump to the location."
  (interactive)
  (cquery-tree-press #'split-window-vertically)
  (when (window-live-p cquery-tree--origin-win)
    (select-window cquery-tree--origin-win)))

(defun cquery-tree-next-line (&optional arg)
  (interactive "p")
  (forward-line arg)
  (when cquery-tree-calling
    (cquery-tree-press)))

(defun cquery-tree-prev-line (&optional arg)
  (interactive "p")
  (forward-line (- arg))
  (when cquery-tree-calling
    (cquery-tree-press)))

(defun cquery-tree-next-sibling (&optional arg)
  (interactive "p")
  (-when-let* ((depth (cquery-tree--depth-at-point)))
    (while (and (forward-line 1)
                (< depth (or (cquery-tree--depth-at-point) 0))))
    (when cquery-tree-calling
      (cquery-tree-press))))

(defun cquery-tree-prev-sibling (&optional arg)
  (interactive "p")
  (-when-let* ((depth (cquery-tree--depth-at-point)))
    (while (and (forward-line -1)
                (< depth (or (cquery-tree--depth-at-point) 0))))
    (when cquery-tree-calling
      (cquery-tree-press))))

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

(defun cquery-tree-quit ()
  (interactive)
  (-when-let* ((buf cquery-tree--origin-buffer)
               (opoint cquery-tree--opoint)
               (_ (window-live-p cquery-tree--origin-win)))
    (with-selected-window cquery-tree--origin-win
      (switch-to-buffer buf)
      (goto-char opoint)))
  (quit-window))

(defun cquery-tree-yank-path ()
  (interactive)
  (--if-let (-some-> (cquery-tree--node-at-point) (cquery-tree-node-location) (car) (kill-new))
      (message (format "Yanked path: %s" (propertize it 'face 'font-lock-string-face)))
    (user-error "There is nothing to copy here")))

;; ---------------------------------------------------------------------
;;   Mode
;; ---------------------------------------------------------------------

(defvar cquery-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'cquery-tree-toggle-expand)
    (define-key map [mouse-1] #'cquery-tree-toggle-expand)
    (define-key map (kbd "c") #'cquery-tree-toggle-calling)
    (define-key map (kbd "f") #'cquery-tree-press)
    (define-key map (kbd "h") #'cquery-tree-collapse-or-select-parent)
    (define-key map (kbd "j") #'cquery-tree-next-line)
    (define-key map (kbd "k") #'cquery-tree-prev-line)
    (define-key map (kbd "J") #'cquery-tree-next-sibling)
    (define-key map (kbd "K") #'cquery-tree-prev-sibling)
    (define-key map (kbd "l") #'cquery-tree-expand-or-set-root)
    (define-key map (kbd "oh") #'cquery-tree-press-and-horizontal-split)
    (define-key map (kbd "ov") #'cquery-tree-press-and-vertical-split)
    (define-key map (kbd "oo") #'cquery-tree-press-and-switch)
    (define-key map (kbd "q") #'cquery-tree-quit)
    (define-key map (kbd "Q") #'quit-window)
    (define-key map (kbd "yy") #'cquery-tree-yank-path)
    (define-key map (kbd "RET") #'cquery-tree-press-and-switch)
    (define-key map (kbd "<left>") #'cquery-tree-collapse-or-select-parent)
    (define-key map (kbd "<right>") #'cquery-tree-expand-or-set-root)
    map)
  "Keymap for `cquery-tree-mode'.")

(define-derived-mode cquery-tree-mode special-mode "cquery-tree"
  "Mode for cquery tree buffers")

(provide 'cquery-tree)
