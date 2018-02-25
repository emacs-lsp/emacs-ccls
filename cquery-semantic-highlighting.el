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

(defface cquery-inactive-region-face
  '((t :inherit shadow))
  "The face used to mark inactive regions."
  :group 'cquery)

(defvar cquery-sem-face-function 'cquery-sem--default-face
  "Function used to determine the face of a symbol in semantic highlighting.")

(defface cquery-sem-member-face
  '((t :slant italic))
  "The extra face applied to member functions/variables."
  :group 'cquery)

(defface cquery-sem-static-face
  '((t :weight bold))
  "The additional face for variables with static storage."
  :group 'cquery)

(defface cquery-sem-static-field-face
  '((t :inherit cquery-sem-static-face))
  "The additional face for static member variables."
  :group 'cquery)

(defface cquery-sem-static-method-face
  '((t :inherit cquery-sem-static-face))
  "The additional face for static member functions."
  :group 'cquery)

(defcustom cquery-sem-function-faces [font-lock-function-name-face]
  "Faces for functions."
  :type '(repeat face)
  :group 'cquery)

(defcustom cquery-sem-macro-faces [font-lock-variable-name-face]
  "Faces for macros."
  :type '(repeat face)
  :group 'cquery)

(defcustom cquery-sem-namespace-faces [font-lock-constant-face]
  "Faces for namespaces."
  :type '(repeat face)
  :group 'cquery)

(defcustom cquery-sem-parameter-faces [font-lock-variable-name-face]
  "Faces for parameters."
  :type '(repeat face)
  :group 'cquery)

(defcustom cquery-sem-type-faces [font-lock-type-face]
  "Faces used to mark types."
  :type '(repeat face)
  :group 'cquery)

(defcustom cquery-sem-variable-faces [font-lock-variable-name-face]
  "Faces for variables."
  :type '(repeat face)
  :group 'cquery)

;; Default colors used by `cquery-use-default-rainbow-sem-highlight'
(defcustom cquery-sem-function-colors
  '("#e5b124" "#927754" "#eb992c" "#e2bf8f" "#d67c17"
    "#88651e" "#e4b953" "#a36526" "#b28927" "#d69855")
  "Default colors for `cquery-sem-function-faces'."
  :type '(repeat color)
  :group 'cquery)

(defcustom cquery-sem-macro-colors
  '("#e79528" "#c5373d" "#e8a272" "#d84f2b" "#a67245"
    "#e27a33" "#9b4a31" "#b66a1e" "#e27a71" "#cf6d49")
  "Default colors for `cquery-sem-macro-faces'."
  :type '(repeat color)
  :group 'cquery)

(defcustom cquery-sem-namespace-colors
  '("#429921" "#58c1a4" "#5ec648" "#36815b" "#83c65d"
    "#417b2f" "#43cc71" "#7eb769" "#58bf89" "#3e9f4a")
  "Default colors for `cquery-sem-namespace-faces'."
  :type '(repeat color)
  :group 'cquery)

(defcustom cquery-sem-parameter-colors
  '("#429921" "#58c1a4" "#5ec648" "#36815b" "#83c65d"
    "#417b2f" "#43cc71" "#7eb769" "#58bf89" "#3e9f4a")
  "Default colors for `cquery-sem-parameter-faces'."
  :type '(repeat color)
  :group 'cquery)

(defcustom cquery-sem-type-colors
  '("#e1afc3" "#d533bb" "#9b677f" "#e350b6" "#a04360"
    "#dd82bc" "#de3864" "#ad3f87" "#dd7a90" "#e0438a")
  "Default colors for `cquery-sem-type-faces'."
  :type '(repeat color)
  :group 'cquery)

(defcustom cquery-sem-variable-colors
  cquery-sem-parameter-colors
  "Default colors for `cquery-sem-variable-faces'."
  :type '(repeat color)
  :group 'cquery)

(defface cquery-code-lens-face
  '((t :inherit shadow))
  "The face used for code lens overlays."
  :group 'cquery)

(defface cquery-code-lens-mouse-face
  '((t :box t))
  "The face used for code lens overlays."
  :group 'cquery)

(defcustom cquery-enable-inactive-region
  t
  "Enable inactive region.
Regions that are disabled by preprocessors will be displayed in shadow."
  :group 'cquery
  :type 'bool)

(defcustom cquery-sem-highlight-method
  nil
  "The method used to draw semantic highlighting.
overlays are more accurate than font-lock, but slower.
If nil, disable semantic highlighting."
  :group 'cquery
  :type '(radio
          (const nil)
          (const :tag "overlay" overlay)
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
  ;; https://github.com/cquery-project/cquery/blob/master/src/symbol.h
  (-let* (((&hash "type" type "kind" kind "storage" storage
                  "parentKind" parent-kind "stableId" stable-id) symbol)
         (fn0 (lambda (faces lo0 hi0)
                (let* ((n (length faces))
                       (lo (/ (* lo0 n) 1000))
                       (hi (/ (* hi0 n) 1000))
                       (idx (max 0 (if (= lo hi) (1- hi) (+ lo (% stable-id (- hi lo)))))))
                  (elt faces idx))))
         (fn (lambda (faces) (elt faces (% stable-id (length faces))))))
    ;; cquery/src/indexer.h ClangSymbolKind
    ;; clang/Index/IndexSymbol.h clang::index::SymbolKind
    (pcase kind
      ;; Functions
      (6 `(,(funcall fn0 cquery-sem-function-faces 0 800)
           cquery-sem-member-face)) ; Method
      (9 `(,(funcall fn0 cquery-sem-function-faces 800 1000)
           cquery-sem-member-face)) ; Constructor
      (12 (funcall fn0 cquery-sem-function-faces 0 1000)) ; Function
      (254 `(,(funcall fn0 cquery-sem-function-faces 0 1000)
             cquery-sem-static-method-face)) ; StaticMethod

      ;; Types
      (3 (funcall fn0 cquery-sem-namespace-faces 0 1000)) ; Namespace
      ((or 5 23) (funcall fn0 cquery-sem-type-faces 0 800)) ; Struct, Class
      (10 (funcall fn0 cquery-sem-type-faces 800 1000)) ; Enum
      (26 (funcall fn0 cquery-sem-type-faces 0 1000)) ; TypeParameter
      (252 `(,(funcall fn0 cquery-sem-type-faces 0 1000)
            cquery-sem-member-face)) ; TypeAlias

      ;; Variables
      (13 `(,(funcall fn0 cquery-sem-variable-faces 0 1000)
            ,@(when (or (= parent-kind 1) (= storage 3))
                '(cquery-sem-static-face)))) ; Variable
      (253 (funcall fn0 cquery-sem-parameter-faces 0 1000)) ; Parameter
      (255 (funcall fn0 cquery-sem-macro-faces 0 1000)) ; Macro
      (8 `(,(funcall fn0 cquery-sem-variable-faces 0 1000)
           ,(if (= storage 3)
                'cquery-sem-static-field-face
              'cquery-sem-member-face
              ))) ; Field
      (22 `(,(funcall fn0 cquery-sem-variable-faces 0 1000)
            cquery-sem-member-face)) ; EnumMember

      (_ (pcase type
           (0 (funcall fn cquery-sem-type-faces))
           (1 (funcall fn cquery-sem-function-faces))
           (_ (funcall fn cquery-sem-variable-faces)))))))

(defun cquery--read-semantic-ranges (symbol face)
  (--map (let ((start (gethash "start" it))
               (end (gethash "end" it)))
           (list (cons (gethash "line" start)
                       (gethash "character" start))
                 (cons (gethash "line" end)
                       (gethash "character" end))
                 face))
         (gethash "ranges" symbol)))

(defun cquery--publish-semantic-highlighting (_workspace params)
  "Publish semantic highlighting information according to PARAMS."
  (when cquery-sem-highlight-method
    (let* ((file (cquery--uri-to-file (gethash "uri" params)))
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
  (require 'dash)
  `(progn
     ,@(cl-loop
        for kind in '("function" "macro" "namespace" "parameter" "type" "variable") append
        (let ((colors (intern (format "cquery-sem-%s-colors" kind))))
          (append
           (--map-indexed
            `(defface ,(intern (format "cquery-sem-%s-face-%S" kind it-index))
               '((t :foreground ,it)) "." :group 'cquery)
            (symbol-value colors))
           (list
            `(setq ,(intern (format "cquery-sem-%s-faces" kind))
                   (apply #'vector
                          (cl-loop for i below (length ,colors) collect
                                   (intern (format "cquery-sem-%s-face-%S" ,kind i)))))))))))

;; Add handler
(push '("$cquery/publishSemanticHighlighting" . (lambda (w p) (cquery--publish-semantic-highlighting w p)))
      cquery--handlers)

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
  (let* ((file (cquery--uri-to-file (gethash "uri" params)))
         (regions (mapcar 'cquery--read-range (gethash "inactiveRegions" params)))
         (buffer (find-buffer-visiting file)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (cquery--clear-inactive-regions)
          (when cquery-enable-inactive-region
            (overlay-recenter (point-max))
            (dolist (region regions)
              (let ((ov (make-overlay (car region) (cdr region) buffer)))
                (overlay-put ov 'face 'cquery-inactive-region-face)
                (overlay-put ov 'cquery-inactive t)))))))))

;; Add handler
(push '("$cquery/setInactiveRegions" . (lambda (w p) (cquery--set-inactive-regions w p)))
      cquery--handlers)

(provide 'cquery-semantic-highlighting)
