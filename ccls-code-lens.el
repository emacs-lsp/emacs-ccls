;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Fangrui Song

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

(require 'ccls-common)

(defgroup ccls-code-lens nil
  "ccls code lens."
  :group 'tools
  :group 'ccls)

(defface ccls-code-lens-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'ccls-code-lens)

(defface ccls-code-lens-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'ccls-code-lens)

;; ---------------------------------------------------------------------
;;   Codelens
;;
;;   Enable by calling `ccls-request-code-lens'
;;   Clear them away using `ccls-clear-code-lens'
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

(defun ccls--make-code-lens-string (command)
  "."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive) (ccls--execute-command (gethash "command" command) (gethash "arguments" command))))
    (propertize (gethash "title" command)
                'face 'ccls-code-lens-face
                'mouse-face 'ccls-code-lens-mouse-face
                'local-map map)))

(defun ccls--code-lens-callback (result)
  "."
  (overlay-recenter (point-max))
  (ccls-clear-code-lens)
  (let (buffers)
    (dolist (lens result)
      (let* ((range (ccls--read-range (gethash "range" lens)))
             (root (gethash "command" lens))
             ;; (title (gethash "title" root))
             ;; (command (gethash "command" root))
             (buffer (find-buffer-visiting (lsp--uri-to-path (gethash "uri" (gethash "arguments" root))))))
        (when buffer
          (with-current-buffer buffer
            (save-excursion
              (when (not (member buffer buffers))
                (ccls-clear-code-lens)
                (overlay-recenter (point-max))
                (setq buffers (cons buffer buffers)))
              (let ((ov (make-overlay (car range) (cdr range) buffer)))
                (overlay-put ov 'ccls-code-lens t)
                (overlay-put ov 'after-string (format " %s" (ccls--make-code-lens-string root)))))))))))

(defun ccls-request-code-lens ()
  "Request code lens from ccls."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request-async
   (lsp--make-request "textDocument/codeLens"
                      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))))
   #'ccls--code-lens-callback))

(defun ccls-clear-code-lens ()
  "Clear all code lenses from this buffer."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'ccls-code-lens)
      (delete-overlay ov))))

(defun ccls-code-lens--request-when-idle ()
  (run-with-idle-timer 0.5 nil 'ccls-request-code-lens))

(define-minor-mode ccls-code-lens-mode
  "toggle code-lens overlays"
  :group 'ccls
  :global nil
  :init-value nil
  :lighter "Lens"
  (pcase ccls-code-lens-mode
    ('t
     (ccls-request-code-lens)
     (add-hook 'lsp-after-diagnostics-hook 'ccls-code-lens--request-when-idle t t))
    ('nil
     (remove-hook 'lsp-after-diagnostics-hook 'ccls-code-lens--request-when-idle t)
     (ccls-clear-code-lens))))

(provide 'ccls-code-lens)
