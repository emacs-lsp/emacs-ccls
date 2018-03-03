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

(defgroup cquery-code-lens nil
  "cquery code lens."
  :group 'tools
  :group 'cquery)

(defface cquery-code-lens-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'cquery-code-lens)

(defface cquery-code-lens-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'cquery-code-lens)

;; ---------------------------------------------------------------------
;;   Codelens
;;
;;   Enable by calling `cquery-request-code-lens'
;;   Clear them away using `cquery-clear-code-lens'
;;
;;   TODO:
;;   - Find a better way to display them.
;;
;;   - Instead of adding multiple lenses to one symbol, append the text
;;     of the new one to the old. This will fix flickering when moving
;;     over lenses.
;;
;;   - Add a global option to request code lenses on automatically
;; ---------------------------------------------------------------------

(defun cquery--make-code-lens-string (command)
  "."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive) (cquery--execute-command (gethash "command" command) (gethash "arguments" command))))
    (propertize (gethash "title" command)
                'face 'cquery-code-lens-face
                'mouse-face 'cquery-code-lens-mouse-face
                'local-map map)))

(defun cquery--code-lens-callback (result)
  "."
  (overlay-recenter (point-max))
  (cquery-clear-code-lens)
  (let (buffers)
    (dolist (lens result)
      (let* ((range (cquery--read-range (gethash "range" lens)))
             (root (gethash "command" lens))
             ;; (title (gethash "title" root))
             ;; (command (gethash "command" root))
             (buffer (find-buffer-visiting (lsp--uri-to-path (car (gethash "arguments" root))))))
        (when buffer
          (with-current-buffer buffer
            (save-excursion
              (when (not (member buffer buffers))
                (cquery-clear-code-lens)
                (overlay-recenter (point-max))
                (setq buffers (cons buffer buffers)))
              (let ((ov (make-overlay (car range) (cdr range) buffer)))
                (overlay-put ov 'cquery-code-lens t)
                (overlay-put ov 'after-string (format " %s" (cquery--make-code-lens-string root)))))))))))

(defun cquery-request-code-lens ()
  "Request code lens from cquery."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request-async
   (lsp--make-request "textDocument/codeLens"
                      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))))
   'cquery--code-lens-callback))

(defun cquery-clear-code-lens ()
  "Clear all code lenses from this buffer."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'cquery-code-lens)
      (delete-overlay ov))))

(defun cquery-code-lens--request-when-idle ()
  (run-with-idle-timer 0.5 nil 'cquery-request-code-lens))

(define-minor-mode cquery-code-lens-mode
  "toggle code-lens overlays"
  :group 'cquery
  :global nil
  :init-value nil
  :lighter "Lens"
  (pcase cquery-code-lens-mode
    ('t
     (cquery-request-code-lens)
     (add-hook 'lsp-after-diagnostics-hook 'cquery-code-lens--request-when-idle t t))
    ('nil
     (remove-hook 'lsp-after-diagnostics-hook 'cquery-code-lens--request-when-idle t)
     (cquery-clear-code-lens))))

(provide 'cquery-code-lens)
