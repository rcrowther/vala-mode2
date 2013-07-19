;;; vala-mode2-faces.el - Major mode for editing vala
;;; Copyright (c) 2013 Robert Crowther
;;; For information on the License, see the LICENSE file


;; Define a face, or two

;(defface font-lock-namespace-face
;  '((t :inherit font-lock-type-face))
;  "Font Lock mode face used to highlight namespace qualifiers."
;  :group 'font-lock-faces)

;; Use:
;; M-x list-colors-display

;; Use from stock
;; comment, keyword, string, doc, builtin, preprocessor

;; (defface font-lock-doc-face
;;   '((t :inherit font-lock-comment-face))
;;   "Font Lock mode face used to highlight documentation."
;;   :group 'font-lock-faces)

;; On 8 colour terminals cyan is used.
(defface font-lock-namespace-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 88) (background dark))  :foreground "Gold")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "Gold")
    (((class color) (min-colors 8)) :foreground "cyan")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight namespaces."
  :group 'font-lock-faces)

(defvar font-lock-namespace-face 'font-lock-namespace-face
  "Face name to use for namespaces.")


;; class usage is regarded as type, thus font-lock-type-face
;; On 8 colour terminals yellow is used.
(defface font-lock-class-definition-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 88) (background dark))  :foreground "Gold")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "Gold")
    (((class color) (min-colors 8)) :foreground "yellow")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight class definitions."
  :group 'font-lock-faces)

(defvar font-lock-class-definition-face 'font-lock-class-definition-face
  "Face name to use for class definitions.")



;; On 8 colour terminals blue is used.
;; Unused, but we may have to.
(defface font-lock-method-definition-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 88) (background dark))  :foreground "Gold")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "Gold")
    (((class color) (min-colors 8)) :foreground "blue")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight method definitions."
  :group 'font-lock-faces)

(defvar font-lock-method-definition-face 'font-lock-method-definition-face
  "Face name to use for method definitions.")



;; method usage should be font-lock-function-face, but can't redefine?
;; On 8 colour terminals magenta is used.
(defface font-lock-minor-keywords-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 88) (background dark))  :foreground "Gold")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "Gold")
    (((class color) (min-colors 8)) :foreground "magenta")
    ())
  "Font Lock mode face used to highlight method references."
  :group 'font-lock-faces)

(defvar font-lock-minor-keywords-face 'font-lock-minor-keywords-face
  "Face name to use for minor keyword referencess.")

(provide 'vala-mode2-faces)
