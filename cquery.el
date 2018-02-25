;;; cquery.el --- cquery client for lsp-mode     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Author:  Tobias Pisani
;; Package-Version: 20180122.1
;; Version: 0.1
;; Homepage: https://github.com/jacobdufault/cquery
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (dash "0.13"))
;; Keywords: languages, lsp, c++

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
;;
;;; Commentary:

;;
;; To enable, call (lsp-cquery-enable) in your c++-mode hook.
;;

;;; Code:

(require 'cquery-common)
(require 'cquery-semantic-highlighting)
(require 'cquery-codelens)
(require 'cquery-tree)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defcustom cquery-executable
  "cquery"
  "Path of the cquery executable."
  :type 'file
  :group 'cquery)

(defcustom cquery-extra-args
  nil
  "Additional command line options passed to the cquery executable."
  :type '(repeat string)
  :group 'cquery)

(defalias 'cquery-additional-arguments 'cquery-extra-args)

(defcustom cquery-cache-dir
  ".cquery_cached_index/"
  "Directory in which cquery will store its index cache.
Relative to the project root directory."
  :type 'directory
  :group 'cquery)

(defcustom cquery-extra-init-params
  nil
  "Additional initializationOptions passed to cquery."
  :type '(repeat string)
  :group 'cquery)

(defcustom cquery-project-roots
  nil
  "A list of project roots that will be matched against the source filename first
to get the project root, before consulting `projectile' or `project'.

This is useful when your project has subprojects. Otherwise `projectile' and
`project' may think the file resides in a subproject and thus the file
does not belong to the current workspace.
"
  :type '(repeat directory)
  :group 'cquery)

;; ---------------------------------------------------------------------
;;   Other cquery-specific methods
;; ---------------------------------------------------------------------

(defun cquery-file-info ()
  (lsp--cur-workspace-check)
  (lsp--send-request
   (lsp--make-request "$cquery/fileInfo"
                      `(:textDocument ,(lsp--text-document-identifier)))))

(defun cquery-freshen-index (&optional whitelist blacklist)
  "Rebuild indexes for matched files.
`whitelist' and `blacklist' are ECMAScript regex used by std::regex_match
`regexp-quote' quotes in elisp flavored regex, so some metacharacters may fail."
  (interactive (list (list (concat "^" (regexp-quote buffer-file-name) "$")) (list ".")))
  (lsp--cur-workspace-check)
  (lsp--send-notification
   (lsp--make-notification "$cquery/freshenIndex"
                           (list :whitelist (or whitelist [])
                                 :blacklist (or blacklist [])))))

(defun cquery-xref-find-custom (method &optional display-action)
  "Find cquery-specific cross references.

Choices of METHOD include \"$cquery/base\", \"$cquery/callers\",
\"$cquery/derived\", \"$cquery/vars\".
Read document for all choices. DISPLAY-ACTION is passed to xref--show-xrefs."
  (lsp--cur-workspace-check)
  (let ((xrefs (lsp--locations-to-xref-items
                (lsp--send-request
                 (lsp--make-request method
                                    (lsp--text-document-position-params))))))
    (unless xrefs
      (user-error "No %s found" method))
    (xref--show-xrefs xrefs display-action)))


;; ---------------------------------------------------------------------
;;  Register lsp client
;; ---------------------------------------------------------------------

(defun cquery--make-renderer (mode)
  `(lambda (str)
     (with-temp-buffer
       (delay-mode-hooks (,(intern (format "%s-mode" mode))))
       (insert str)
       (font-lock-ensure)
       (buffer-string))))

(defun cquery--initialize-client (client)
  (dolist (p cquery--handlers)
    (lsp-client-on-notification client (car p) (cdr p)))
  (lsp-provide-marked-string-renderer client "c" (cquery--make-renderer "c"))
  (lsp-provide-marked-string-renderer client "cpp" (cquery--make-renderer "c++"))
  (lsp-provide-marked-string-renderer client "objectivec" (cquery--make-renderer "objc")))

(defun cquery--get-init-params (workspace)
  `(:cacheDirectory ,(file-name-as-directory
                      (expand-file-name cquery-cache-dir (lsp--workspace-root workspace)))
                    ,@cquery-extra-init-params)) ; TODO: prog reports for modeline

;;;###autoload (autoload 'lsp-cquery-enable "cquery")
(lsp-define-stdio-client
 lsp-cquery "cpp" #'cquery--get-root
 `(,cquery-executable ,@cquery-extra-args)
 :initialize #'cquery--initialize-client
 :extra-init-params #'cquery--get-init-params)

(provide 'cquery)
;;; cquery.el ends here
