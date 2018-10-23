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

(require 'cc-mode)
(require 'lsp-mode)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dash)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defgroup ccls nil
  "Customization options for the ccls client"
  :group 'tools)

;; ---------------------------------------------------------------------
;;   Utility
;; ---------------------------------------------------------------------

(defun ccls--read-range (range)
  (cons (lsp--position-to-point (gethash "start" range))
        (lsp--position-to-point (gethash "end" range))))

(cl-defun ccls--get-root ()
  "Return the root directory of a ccls project."
  (-when-let (match (locate-dominating-file default-directory ".ccls-root"))
    (expand-file-name match)))

(defun ccls--get-renderer ()
  (thread-last lsp--cur-workspace
    lsp--workspace-client
    lsp--client-string-renderers
    (assoc-string (thread-first lsp--cur-workspace
                    lsp--workspace-client
                    lsp--client-language-id
                    (funcall (current-buffer))))
    cdr))

(defun ccls--render-string (str)
  (funcall (ccls--get-renderer) str))

(defun ccls--render-type (str)
  "Render a string as a type"
  (string-remove-suffix " a;" (ccls--render-string (format "%s a;" str))))

;; ---------------------------------------------------------------------
;;   Notification handlers
;; ---------------------------------------------------------------------

(defvar ccls--handlers nil
  "List of cons-cells of (METHOD . HANDLER) pairs, where METHOD is the lsp method to handle,
and handler is a function invoked as (handler WORKSPACE PARAMS), where WORKSPACE is the current
lsp-workspace, and PARAMS is a hashmap of the params recieved with the notification.")


(provide 'ccls-common)
