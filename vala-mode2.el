;;; vala-mode2.el - Major mode for editing the Vala language

;; Copyright (c) 2013 Robert Crowther
;; Authors: 2013 Robert Crowther                        

;; Created:    2013
;; Version:    0.1alpha1
;; Keywords: c languages 
;; Package: vala-mode2                        

;;; For information on the License, see the LICENSE file

;;; Commentary:
;;
;;    See http://live.gnome.org/Vala for details about Vala language.
;;
;;    This mode implements fill, indenting and font-locking for the Vala
;;    language. It inherits from fundamental-mode.
;;
;; The code was originally hacked from scala-mode2.


;;; Code:

(require 'vala-mode2-styles)
(require 'vala-mode2-syntax)
(require 'vala-mode2-paragraph)
(require 'vala-mode2-indent)
(require 'vala-mode2-fontlock)
(require 'vala-mode2-lib)

;; Tested only for emacs 24
(unless (<= 24 emacs-major-version)
  (error
   (format "This Vala mode has been tested only on Emacs version 24.2 (and not your Emacs version %s.%s)"  
           emacs-major-version  emacs-minor-version)))

(defgroup vala nil
  "Code in the Vala language"
  :group 'languages)

(defmacro vala-mode2:make-local-variables (&rest quoted-names)
  (cons 'progn (mapcar #'(lambda (quoted-name) `(make-local-variable ,quoted-name)) quoted-names)))

;(defun vala-mode2:forward-sexp-function (&optional count)
 ; (unless count (setq count 1))
  ;(if (< count 0)
   ;   (dotimes (n (abs count))
    ;    (vala-syntax:backward-sexp))
   ; (dotimes (n count)
    ;  (vala-syntax:forward-sexp))))


;;;###autoload
(define-derived-mode vala-mode2 prog-mode "Vala"
  "Major mode for editing vala code.

When started, runs `vala-mode2-hook'. 

\\{vala-mode2-map}" 
  :syntax-table vala-syntax:syntax-table
;  :group                               
;  :abbrev

  (vala-mode2:make-local-variables
   'post-self-insert-hook
   'syntax-propertize-function
  ;; 'font-lock-syntactic-face-function
   'font-lock-defaults
   'paragraph-start
   'paragraph-separate
   'fill-paragraph-function
   'adaptive-fill-function
   'adaptive-fill-first-line-regexp
   'comment-start
   'comment-end
   'comment-start-skip
   'comment-column
   'comment-multi-line
  ;; 'forward-sexp-function
   'indent-line-function
   ;; ensure this is local (sometimes it is, sometimes not)
   'indent-tabs-mode
   'vala-indentation-style)


  (add-hook 'syntax-propertize-extend-region-functions
            'vala-syntax:propertize-extend-region)

  ;; Preset some EMACS general values
  (setq vala-mode2:debug-messages       nil

        syntax-propertize-function      'vala-syntax:propertize
        parse-sexp-lookup-properties    t

        ;; TODO: font-lock
        ;; (23.6.5 Levels of Font Lock)
        font-lock-defaults              '(
                (vala-font-lock:keywords-1 ;; mode-default fontification
                )

                nil
                ;; No other font-lock levels are currently set
                 )

        ;; fontify comments and strings with the builtin syntactic
        ;; function
        font-lock-syntactic-face-function 'vala-font-lock:syntactic-face-function

        ;; TODO: beginning-of-defun-function, end-of-defun-function

        ;; comments
        paragraph-start                 vala-paragraph:paragraph-start-re
        paragraph-separate              vala-paragraph:paragraph-separate-re
        fill-paragraph-function         'vala-paragraph:fill-paragraph-function
        adaptive-fill-function          'vala-paragraph:adaptive-fill-function
        adaptive-fill-first-line-regexp vala-paragraph:fill-first-line-re
        ;; but, what are these FOR?
        comment-start                   "// "
        comment-end                     ""
        comment-start-skip              "\\(//+\\|/\\*+\\)[ \t]*"
        comment-column                  0
        comment-multi-line              t

        ;;forward-sexp-function           'vala-mode2:forward-sexp-function
        indent-line-function            'vala-indent:indent-line
        ;;indent-tabs-mode                nil
     ;;   indent-tabs-mode                t
        ;; set this to the detection of space or tab
        indent-tabs-mode                (progn
                                          (let ((ident-type (vala-lib:buffer-has-tabs-p)))
                                          (cond
                                           ((= ident-type 0)
                                            ;; undefined
                                            (message "undefined indent style")
                                            (default-value indent-tabs-mode)) 
                                           ((= ident-type 1) nil) ; space
                                           ((= ident-type 2) t) ; tabs
                                           )))
	;; tab-width
        )
;;  (message "tabs -%s-" (vala-lib:buffer-has-tabs-p))
  ;; Put a message up
  (message "Indent style detected: -%s-"
           (if (null indent-tabs-mode) "SPACE" "TAB"))
  (message "  indents: -%s-" (vala-style:print-indents))
  ;; TODO: need to change the tab internal style variables here, as the
  ;; variables were initialised long previously.

;; (use-local-map vala-mode2-map)
;;  (define-key c-mode-base-map "\C-c."     'c-set-style)
  ;; add indent functionality to some characters
;;  (vala-mode2-map:add-remove-indent-hook)
;;  (vala-mode2-map:add-self-insert-hooks)
;; 
)

;; Attach .vala files to the vala-mode2
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(vala\\|vapi\\)\\'" . vala-mode2))
  (modify-coding-system-alist 'file "\\.\\(vala\\|vapi\\)\\'" 'utf-8))

(provide 'vala-mode2)
;;; vala-mode2.el ends here
