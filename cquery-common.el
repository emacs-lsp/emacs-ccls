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

(require 'cc-mode)
(require 'lsp-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'dash)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defgroup cquery nil
  "Customization options for the cquery client"
  :group 'tools)

;; ---------------------------------------------------------------------
;;   Utility
;; ---------------------------------------------------------------------

(defun cquery--read-range (range)
  (cons (lsp--position-to-point (gethash "start" range))
        (lsp--position-to-point (gethash "end" range))))

(defsubst cquery--root-from-file (file)
  (-when-let (match (locate-dominating-file default-directory file))
    (expand-file-name match)))

(defsubst cquery--root-from-func (func)
  (and (fboundp func) (ignore-errors (funcall func))))

(cl-defun cquery-project-roots-matcher ()
  (cl-loop for root in cquery-project-roots do
           (when (string-prefix-p (expand-file-name root) buffer-file-name)
             (cl-return-from cquery--get-root root))))

(cl-defun cquery--get-root ()
  "Return the root directory of a cquery project."
  (cl-loop for matcher in cquery-project-root-matchers do
           (-when-let (root (cl-typecase matcher
                              (string (cquery--root-from-file matcher))
                              (function  (cquery--root-from-func matcher))))
             (cl-return-from cquery--get-root root)))
  (user-error "Could not find cquery project root"))

(defun cquery--is-cquery-buffer (&optional buffer)
  "Return non-nil if current buffer is using the cquery client"
  (with-current-buffer (or buffer (current-buffer))
    (and lsp--cur-workspace
         (eq (lsp--client-get-root (lsp--workspace-client lsp--cur-workspace)) 'cquery--get-root))))

(define-inline cquery--cquery-buffer-check ()
  (inline-quote (cl-assert (cquery--is-cquery-buffer) nil
                           "Cquery is not enabled in this buffer.")))

(defun cquery--get-renderer ()
  (thread-last lsp--cur-workspace
    lsp--workspace-client
    lsp--client-string-renderers
    (assoc-string (thread-first lsp--cur-workspace
                    lsp--workspace-client
                    lsp--client-language-id
                    (funcall (current-buffer))))
    cdr))

(defun cquery--render-string (str)
  (funcall (cquery--get-renderer) str))

(defun cquery--render-type (str)
  "Render a string as a type"
  (string-remove-suffix " a;" (cquery--render-string (format "%s a;" str))))

;; ---------------------------------------------------------------------
;;   Notification handlers
;; ---------------------------------------------------------------------

(defvar cquery--handlers
  '(("$cquery/progress" . (lambda (_w _p))))
  "List of cons-cells of (METHOD . HANDLER) pairs, where METHOD is the lsp method to handle, 
and handler is a function invoked as (handler WORKSPACE PARAMS), where WORKSPACE is the current
lsp-workspace, and PARAMS is a hashmap of the params recieved with the notification.")

;; ---------------------------------------------------------------------
;;   Commands
;; ---------------------------------------------------------------------

(defun cquery--execute-command (command &optional arguments)
  "Execute a cquery command."
  (let* ((uri (car arguments))
         (data (cdr arguments)))
    (save-current-buffer
      (find-file (lsp--uri-to-path uri))
      (pcase command
        ;; Code actions
        ('"cquery._applyFixIt"
         (dolist (edit data)
           (cquery--apply-textedit (car edit))))
        ('"cquery._autoImplement"
         (dolist (edit data)
           (cquery--apply-textedit (car edit)))
         (goto-char (lsp--position-to-point
                     (gethash "start" (gethash "range" (caar data))))))
        ('"cquery._insertInclude"
         (cquery--select-textedit data "Include: "))
        ('"cquery.showReferences" ;; Used by code lenses
         (xref--show-xrefs (lsp--locations-to-xref-items (cadr data)) nil))
        (_
         (message "unknown command: %s" command))))))

(defun cquery--select-textedit (edit-list prompt)
  "Show a list of possible textedits, and apply the selected.
  Used by cquery._insertInclude"
  (let ((name-func (lambda (edit)
                     (concat (lsp--position-to-point
                              (gethash "start" (gethash "range" edit)))
                             ": "
                             (gethash "newText" edit)))))
    (ivy-read prompt
              (mapcar (lambda (edit)
                        (funcall name-func edit))
                      edit-list)
              :require-match t
              :action (lambda (str)
                        (cl-loop
                         for edit in edit-list
                         do (when (equal (funcall name-func edit) str)
                              (cquery--apply-textedit edit)))))))

(defun cquery--apply-textedit (edit)
  (let* ((range (gethash "range" edit))
         (start (lsp--position-to-point (gethash "start" range)))
         (end (lsp--position-to-point (gethash "end" range)))
         (newText (gethash "newText" edit)))
    (when (> end start)
      (delete-region start (- end 1)))
    (goto-char start)
    (insert newText)))

(defun cquery--execute-command-locally-advice (orig-func command args)
  "Cquery currently doesn't support `workspace/executeCommand', so execute those locally.
Keep an eye on https://github.com/jacobdufault/cquery/issues/283"
  (if (cquery--is-cquery-buffer)
      (cquery--execute-command command args)
    (funcall orig-func args)))

(advice-add 'lsp--send-execute-command :around #'cquery--execute-command-locally-advice)

(provide 'cquery-common)
