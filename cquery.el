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
;;  TODO:
;;
;;  - Rainbow variables with semantic highlighting
;;  - Better config options
;;

;;; Code:

(require 'cc-mode)
(require 'lsp-mode)
(require 'cl-lib)
(require 'subr-x)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defgroup cquery nil
  "Customization options for the cquery client"
  :group 'tools)

(defcustom cquery-executable
  "cquery"
  "Path of the cquery executable."
  :type 'file
  :group 'cquery)

(defcustom cquery-extra-args
  nil
  "Additional command line options passed to the cquery executable."
  :type 'list
  :group 'cquery)

(defalias 'cquery-additional-arguments 'cquery-extra-args)

(defcustom cquery-cache-dir
  ".vscode/cquery_cached_index/"
  "Directory in which cquery will store its index cache.
Relative to the project root directory."
  :type 'string
  :group 'cquery)

(defcustom cquery-extra-init-params
  nil
  "Additional initializationOptions passed to cquery."
  :type '(repeat string)
  :group 'cquery)

(defface cquery-inactive-region-face
  '((t :inherit shadow))
  "The face used to mark inactive regions."
  :group 'cquery)

(defvar cquery-sem-face-function 'cquery-sem--default-face
  "Function used to determine the face of a symbol in semantic highlighting.")

(defface cquery-sem-type-face
  '((t :weight bold :inherit font-lock-type-face))
  "The default face in cquery-sem-type-faces."
  :group 'cquery)

(defcustom cquery-sem-type-faces [cquery-sem-type-face]
  "Faces used to mark types."
  :type '(repeat face)
  :group 'cquery)

(defface cquery-sem-member-func-face
  '((t :slant italic :inherit font-lock-function-name-face))
  "The default face in cquery-sem-member-func-faces."
  :group 'cquery)

(defcustom cquery-sem-member-func-faces [cquery-sem-member-func-face]
  "Faces used to mark member functions."
  :type '(repeat face)
  :group 'cquery)

(defface cquery-sem-free-func-face
  '((t :inherit font-lock-function-name-face))
  "The default face in cquery-sem-free-func-faces."
  :group 'cquery)

(defcustom cquery-sem-free-func-faces [cquery-sem-free-func-face]
  "Faces used to mark free functions."
  :type '(repeat face)
  :group 'cquery)

(defface cquery-sem-member-var-face
  '((t :slant italic :inherit font-lock-variable-name-face))
  "The default face in cquery-sem-member-var-faces."
  :group 'cquery)

(defcustom cquery-sem-member-var-faces [cquery-sem-member-var-face]
  "Faces used to mark member variables."
  :type '(repeat face)
  :group 'cquery)

(defface cquery-sem-free-var-face
  '((t :inherit font-lock-variable-name-face))
  "The default face in cquery-sem-free-var-faces."
  :group 'cquery)

(defcustom cquery-sem-free-var-faces [cquery-sem-free-var-face]
  "Faces used to mark free variables."
  :type '(repeat face)
  :group 'cquery)

(defcustom cquery-rainbow-sem-type-colors
  '("#e1afc3" "#d533bb" "#9b677f" "#e350b6" "#a04360"
    "#dd82bc" "#de3864" "#ad3f87" "#dd7a90" "#e0438a")
  "Type colors used in rainbow semantic highlighting."
  :type '(repeat string)
  :group 'cquery)

(defcustom cquery-rainbow-sem-func-colors
  '("#e5b124" "#927754" "#eb992c" "#e2bf8f" "#d67c17"
    "#88651e" "#e4b953" "#a36526" "#b28927" "#d69855")
  "Function colors used in rainbow semantic highlighting."
  :type '(repeat string)
  :group 'cquery)

(defcustom cquery-rainbow-sem-var-colors
  '("#587d87" "#26cdca" "#397797" "#57c2cc" "#306b72"
    "#6cbcdf" "#368896" "#3ea0d2" "#48a5af" "#7ca6b7")
  "Variable colors used in rainbow semantic highlighting."
  :type '(repeat string)
  :group 'cquery)

(defface cquery-code-lens-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'cquery)

(defface cquery-code-lens-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'cquery)

(defcustom cquery-enable-sem-highlight
  t
  "Enable semantic highlighting."
  :type 'boolean
  :group 'cquery)

(defcustom cquery-sem-highlight-method
  'overlay
  "The method used to draw semantic highlighting.
overlays are more accurate than font-lock, but slower."
  :group 'lsp-mode
  :type '(radio
          (const :tag "overlays" overlay)
          (const :tag "font-lock" font-lock)))

;; ---------------------------------------------------------------------
;;   Semantic highlighting
;; ---------------------------------------------------------------------

(defun cquery--clear-sem-highlights ()
  "."
  (pcase cquery-sem-highlight-method
    ('overlay
     (dolist (ov (overlays-in (point-min) (point-max)))
       (when (overlay-get ov 'cquery-sem-highlight)
         (delete-overlay ov))))
    ('font-lock
     (font-lock-ensure))))

(defun cquery--make-sem-highlight (region buffer face)
  "Highlight a REGION in BUFFER with FACE."
  (pcase cquery-sem-highlight-method
    ('overlay
     (let ((ov (make-overlay (car region) (cdr region) buffer)))
       (overlay-put ov 'face face)
       (overlay-put ov 'cquery-sem-highlight t)))
    ('font-lock
     (put-text-property (car region) (cdr region) 'font-lock-face face buffer))))

(defun cquery-sem--default-face (symbol)
  "Get semantic highlighting face of SYMBOL."
  (let* ((type (gethash "type" symbol))
         (kind (gethash "kind" symbol))
         (stable-id (gethash "stableId" symbol))
         (is-type-member (gethash "isTypeMember" symbol))
         (fn0 (lambda (faces lo0 hi0)
                (let* ((n (length faces))
                       (lo (/ (* lo0 n) 1000))
                       (hi (/ (* hi0 n) 1000)))
                  (elt faces
                       (if (= lo hi) (max (1- hi) 0) (+ lo (% stable-id (- hi lo))))))))
         (fn (lambda (faces) (elt faces (% stable-id (length faces))))))
    ;; cquery/src/indexer.h ClangSymbolKind
    ;; clang/Index/IndexSymbol.h clang::index::SymbolKind
    (pcase kind
      ;; var
      (4 (funcall fn0 cquery-sem-free-var-faces 600 700)) ; Macro
      (13 (funcall fn0 cquery-sem-free-var-faces 0 600)) ; Variable
      (25 (funcall fn0 cquery-sem-free-var-faces 700 1000)) ; Parameter
      (14 (funcall fn0 cquery-sem-member-var-faces 400 1000)) ; Field
      (15 (funcall fn0 cquery-sem-member-var-faces 200 400)) ; EnumConstant
      (21 (funcall fn0 cquery-sem-member-var-faces 0 200)) ; StaticProperty

      ;; func
      (12 (funcall fn0 cquery-sem-free-func-faces 0 800)) ; Function
      (18 (funcall fn0 cquery-sem-free-func-faces 800 1000)) ; StaticMethod
      (22 (funcall fn0 cquery-sem-member-func-faces 800 1000)) ; Constructor
      (23 (funcall fn0 cquery-sem-member-func-faces 1000 1000)) ; Destructor
      (24 (funcall fn0 cquery-sem-member-func-faces 1000 1000)) ; ConversionFunction
      (16 (funcall fn0 cquery-sem-member-func-faces 0 800)) ; InstanceMethod

      ;; type
      ((or 6 7) (funcall fn0 cquery-sem-type-faces 0 700)) ; Struct | Class
      (10 (funcall fn0 cquery-sem-type-faces 1000 1000)) ; Union
      (11 (funcall fn0 cquery-sem-type-faces 700 1000)) ; TypeAlias

      (_ (pcase type
           (0 (funcall fn cquery-sem-type-faces))
           (1 (if is-type-member
                  (funcall fn cquery-sem-member-func-faces)
                (funcall fn cquery-sem-free-func-faces)))
           (2 (if is-type-member
                  (funcall fn cquery-sem-member-var-faces)
                (funcall fn cquery-sem-free-var-faces))))))))

(defun cquery--publish-semantic-highlighting (_workspace params)
  "Publish semantic highlighting information according to PARAMS."
  (when cquery-enable-sem-highlight
    (let* ((file (lsp--uri-to-path (gethash "uri" params)))
           (buffer (find-buffer-visiting file))
           (symbols (gethash "symbols" params)))
      (when buffer
        (with-current-buffer buffer
          (save-excursion
           (with-silent-modifications
             (cquery--clear-sem-highlights)
             (dolist (symbol symbols)
               (-when-let (face (funcall cquery-sem-face-function symbol))
                 (dolist (range
                          (mapcar 'cquery--read-range (gethash "ranges" symbol)))
                     (cquery--make-sem-highlight range buffer face)))))))))))

(defmacro cquery-use-default-rainbow-sem-highlight ()
  "Use default rainbow semantic highlighting theme."
  (require 'dash)  ; for --map-indexed
  `(progn
     ;; type
     ,@(--map-indexed
        `(defface ,(intern (format "cquery-sem-type-face-%S" it-index))
           '((t :foreground ,it)) ".")
        cquery-rainbow-sem-type-colors)
     (setq cquery-sem-type-faces
           (apply #'vector (loop for i to 10 collect
                                 (intern (format "cquery-sem-type-face-%S" i)))))

     ;; func
     ,@(apply #'append (--map-indexed
                        `((defface ,(intern (format "cquery-sem-free-func-face-%S" it-index))
                            '((t :foreground ,it)) ".")
                          (defface ,(intern (format "cquery-sem-member-func-face-%S" it-index))
                            '((t :slant italic :foreground ,it)) "."))
                        cquery-rainbow-sem-func-colors))
     (setq cquery-sem-free-func-faces
           (apply #'vector (loop for i to 10 collect
                                 (intern (format "cquery-sem-free-func-face-%S" i)))))
     (setq cquery-sem-member-func-faces
           (apply #'vector (loop for i to 10 collect
                                 (intern (format "cquery-sem-member-func-face-%S" i)))))

     ;; var
     ,@(apply #'append (--map-indexed
                        `((defface ,(intern (format "cquery-sem-free-var-face-%S" it-index))
                            '((t :foreground ,it)) ".")
                          (defface ,(intern (format "cquery-sem-member-var-face-%S" it-index))
                            '((t :slant italic :foreground ,it)) "."))
                        cquery-rainbow-sem-var-colors))
     (setq cquery-sem-free-var-faces
           (apply #'vector (loop for i to 10 collect
                                 (intern (format "cquery-sem-free-var-face-%S" i)))))
     (setq cquery-sem-member-var-faces
           (apply #'vector (loop for i to 10 collect
                                 (intern (format "cquery-sem-member-var-face-%S" i)))))
     ))

;; ---------------------------------------------------------------------
;;   Inactive regions
;; ---------------------------------------------------------------------

(defun cquery--clear-inactive-regions ()
  "."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'cquery-inactive)
      (delete-overlay ov))))

(defun cquery--set-inactive-regions (_workspace params)
  "Put overlays on (preprocessed) inactive regions according to PARAMS."
  (let* ((file (lsp--uri-to-path (gethash "uri" params)))
         (regions (mapcar 'cquery--read-range (gethash "inactiveRegions" params)))
         (buffer (find-buffer-visiting file)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (cquery--clear-inactive-regions)
          (overlay-recenter (point-max))
          (dolist (region regions)
            (let ((ov (make-overlay (car region) (cdr region) buffer)))
              (overlay-put ov 'face 'cquery-inactive-region-face)
              (overlay-put ov 'cquery-inactive t))))))))

;; ---------------------------------------------------------------------
;;   Notification handlers
;; ---------------------------------------------------------------------

(defconst cquery--handlers
  '(("$cquery/setInactiveRegions" . (lambda (w p) (cquery--set-inactive-regions w p)))
    ("$cquery/publishSemanticHighlighting" . (lambda (w p) (cquery--publish-semantic-highlighting w p)))
    ("$cquery/progress" . (lambda (_w _p)))))

;; ---------------------------------------------------------------------
;;   Other cquery-specific methods
;; ---------------------------------------------------------------------

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

(defun cquery-request-code-lens ()
  "Request code lens from cquery."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-request-async
   (lsp--make-request "textDocument/codeLens"
                      `(:textDocument (:uri ,(lsp--path-to-uri buffer-file-name))))
   'cquery--code-lens-callback))

(defun cquery-clear-code-lens ()
  "Clear all code lenses from this buffer."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'cquery-code-lens)
      (delete-overlay ov))))

(define-minor-mode cquery-code-lens-mode
  "toggle code-lens overlays"
  :group 'cquery
  :global nil
  :init-value nil
  :lighter "Lens"
  (if cquery-code-lens-mode
      (cquery-request-code-lens)
    (cquery-clear-code-lens)))

(defun cquery--make-code-lens-string (command)
  "."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive) (cquery--execute-command command)))
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
             (title (gethash "title" root))
             (command (gethash "command" root))
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
                (overlay-put ov 'after-string (format " %s " (cquery--make-code-lens-string root)))))))))))

;; ---------------------------------------------------------------------
;;   CodeAction Commands
;; ---------------------------------------------------------------------

(defun cquery-select-codeaction ()
  "Show a list of codeactions using ivy, and pick one to apply."
  (interactive)
  (let ((name-func
         (lambda (action)
           (let ((edit (caadr (gethash "arguments" action))))
             (format "%s: %s" (lsp--position-to-point
                               (gethash "start" (gethash "range" edit)))
                     (gethash "title" action))))))
    (if (null lsp-code-actions)
        (message "No code actions avaliable")
      (ivy-read "Apply CodeAction: "
                (mapcar (lambda (action)
                          (funcall name-func action))
                        lsp-code-actions)
                :action (lambda (str)
                          (dolist (action lsp-code-actions)
                            (when (equal (funcall name-func action) str)
                              (cquery--execute-command (gethash "command" action) (gethash "arguments" action))
                              (lsp--text-document-code-action))))))))

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
         (let ((pos (lsp--position-to-point (car data))))
           (goto-char pos)
           (xref-find-references (xref-backend-identifier-at-point (xref-find-backend)))))))))

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

(defun cquery--read-range (range)
  (cons (lsp--position-to-point (gethash "start" range))
        (lsp--position-to-point (gethash "end" range))))

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

(defun cquery--get-root ()
  "Return the root directory of a cquery project."
  (expand-file-name (or (locate-dominating-file default-directory "compile_commands.json")
                        (locate-dominating-file default-directory ".cquery")
                        default-directory)))

(defun cquery--is-cquery-buffer(&optional buffer)
  "Return non-nil if current buffer is using the cquery client."
  (with-current-buffer (or buffer (current-buffer))
    (and lsp--cur-workspace
         (eq (lsp--client-get-root (lsp--workspace-client lsp--cur-workspace)) 'cquery--get-root))))

(defun cquery--execute-command-locally-advice (orig-func command args)
  "Cquery currently doesn't support `workspace/executeCommand', so execute those locally.
Keep an eye on https://github.com/jacobdufault/cquery/issues/283"
  (if (cquery--is-cquery-buffer)
      (cquery--execute-command command args)
    (orig-func args)))

(advice-add 'lsp--send-execute-command :around #'cquery--execute-command-locally-advice)

(lsp-define-stdio-client
 lsp-cquery "cpp" #'cquery--get-root
 `(,cquery-executable "--language-server" ,@cquery-extra-args)
 :initialize #'cquery--initialize-client
 :extra-init-params #'cquery--get-init-params)

;;;###autoload
(autoload 'lsp-cquery-enable "cquery")

(provide 'cquery)
;;; cquery.el ends here
