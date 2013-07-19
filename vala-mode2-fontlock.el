;;; vala-mode2-fontlock.el - fontlock for Vala Mode2             

;; Copyright (c) 2013 Robert Crowther                         

;; Authors: 2013 Robert Crowther                        
;; Keywords: c languages  
;; Package: vala-mode2                        

;;; For information on the License, see the LICENSE file      

;;; Commentary:                    

;;; Code:

;;(require 'vala-mode2-syntax)
(require 'vala-mode2-faces)
; just for debugging?
; (load-file "./vala-mode2-syntax.el")







;; For the call, see vala-mode2.
(defun vala-font-lock:keywords-1 ()
  ;; chars, string, comments are handled according to syntax and
  ;; syntax propertize

  `(;; keywords

    (,vala-syntax:builtin-function-and-operator-keywords-opt-re
      1 font-lock-minor-keywords-face)

    ;; doesn't work - why?
    (,vala-syntax:class-tree-keywords-opt-re
      1 font-lock-namespace-face)


    ;; Annotations
    ;(, (rx (and "@" (in "a-zA-Z_.") (0+ (in "a-zA-Z0-9_."))))
    ; . font-lock-preprocessor-face)

    ;; Code Attributes
    ;; TODO: The nested bracket form foils the simple regex. Must be
    ;; converted to an off-table syntax parse.
    ;; There's big problems with code attributes.
    ;; From c#, and other languages, they muddle the index operator
    ;; with annotation bracketing. Searching for text won't help, as
    ;; all kinds of variables may be in the operator. Filtering names,
    ;; e.g. Deprecated, Compact, CCode... is foiled by vapi flexibility
    ;; in naming just about any c attribute.
    ;; We go for an initial capital letter. Hope that doesn't annoy
    ;; anyone, including us. It could be expanded to the full internal syntax.
  ;;  (, (rx (and "[" upper (+ (not-char "]" )) "]"))
    ;; . font-lock-preprocessor-face)


))



;; Syntactic call for comments.
;; The call is enabled in vala-mode2.
(defun vala-font-lock:syntactic-face-function (state)
  "Return correct face for string or comment"
  ;; integerp 4 = inside nestable (block) comment
  (if (integerp (nth 4 state))
      (if (save-excursion
            ;; 8 = start position
            (goto-char (nth 8 state))
            ;; If block comment starts with multiple asterix, it must
            ;; be followed by an empty line e.g. "/**<space>\n"
            (looking-at "/\\*\\(?:[^*]\\|\\*+\\s *$\\)")
            )
      ;; block comment start ok
      font-lock-comment-face
      ;; block comment start failed, highlight as string
      font-lock-string-face
      )
    ;; if not, this is either a string or single line comment
    ;; 3 = string
    (if (nth 3 state) font-lock-string-face font-lock-comment-face)))



(provide 'vala-mode2-fontlock)
