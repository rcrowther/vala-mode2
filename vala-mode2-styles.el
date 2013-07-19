;;; scala-mode-styles.el - Style presets and actions for Vala Mode2

;; Copyright (c) 2013 Robert Crowther

;; Authors: 2013 Robert Crowther
;; Keywords: c languages
;; Package: vala-mode2

;;; For information on the License, see the LICENSE file

;;; Commentary:

;;; Code:

(defvar vala-style:presets
  '(("gnu"
        (vala-indent:nesting-definition-to-brackets . 0)
        (vala-indent:nesting-brackets-to-body . 2)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 2))

    ("1tbs"
        (vala-indent:nesting-definition-to-brackets . 0)
        (vala-indent:nesting-brackets-to-body . 2)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 8))

    ("k&r"
        (vala-indent:nesting-definition-to-brackets . 0)
        (vala-indent:nesting-brackets-to-body . 4)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 4))

    ("allman"
        (vala-indent:nesting-definition-to-brackets . 0)
        (vala-indent:nesting-brackets-to-body . 4)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 4))

    ("stroustrup"
        (vala-indent:nesting-definition-to-brackets . 0)
        (vala-indent:nesting-brackets-to-body . 0)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 4))

    ("whitesmith"
        (vala-indent:nesting-definition-to-brackets . 4)
        (vala-indent:nesting-brackets-to-body . 0)
        (vala-indent:definition-to-brackets . 8)
        (vala-indent:brackets-to-body . 0))

    ("linux"
        (vala-indent:nesting-definition-to-brackets . 4)
        (vala-indent:nesting-brackets-to-body . 4)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 8))

    ("ihb"
        (vala-indent:nesting-definition-to-brackets . 2)
        (vala-indent:nesting-brackets-to-body . 2)
        (vala-indent:definition-to-brackets . 2)
        (vala-indent:brackets-to-body . 2))
    )
  "Styles of indentation.
Elements of this alist are of the form:

  (STYLE-STRING zeroOrMore( (VARIABLE . VALUE) )

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any Emacs variable, and VALUE is the intended value
for that variable when using the selected style.")



;; Functions that manipulate styles
(defun vala-style:set-style (style-data)
""
  (mapc
   (lambda (keyval)
     (set (car keyval) (cdr keyval)))
   (cdr style-data))
  (setq vala-indentation-style (car style-data))
  )


(defvar vala-style:style-history nil)


;;
;; External
;;

;; TODO: attempts to provide defcustom variable fail to provide
;; suitable list for filtering. Conclude: without suitable
;; documentation, Customize interface does not want us to use
;; it. Unsatisfactory but logical result.

;; (defcustom vala-indentation-style-default "gnu" "The default style
;; to use for indentation. Any of the presets in the list ... is
;; valid" :type '(choice (const "gnu") (const "thumpin")) :group
;; 'vala)


(defcustom vala-indentation-style-default "gnu"
  "The default style to use for indentation. Any of the presets
in the list ... is valid"
  :type 'string
  :group 'vala)


(defvar vala-indentation-style vala-indentation-style-default
  "The current style used for indentation. It should not be altered directly, see vala-set-style.")



(defun vala-set-style (stylename)
  "Set the current buffer to use the style STYLENAME.
STYLENAME must be an existing Vala Mode style. The current list
of names can be found in `vala-style:presets'.

The variable `vala-indentation-style' will get set to STYLENAME."
  (interactive
   (list (let ((completion-ignore-case t)
	       (prompt (format "Which %s indentation style? "
			       mode-name)))
           (completing-read prompt vala-style:presets nil t nil
			    'vala-style:style-history
			    vala-indentation-style-default))))
  ;; TODO err, how?
 ;; (or c-buffer-is-cc-mode
   ;;   (error "Buffer %s is not a CC Mode buffer (vala-set-style)" (buffer-name)))
  (or (stringp stylename)
      (error "Argument to vala-set-style was not a string"))
  (message "style is:-%s-" stylename)

  (mapc 
   (lambda (elem) 
     (when (string=  stylename (car elem))
     (message "-%s-" (car elem))
   (vala-style:set-style elem)
   ))
   vala-style:presets)

  ; (message " now set to -%s-" vala-indentation-style)
  ;; not defined?
  ;; (c-keep-region-active)
  )

;;TODO: this doesn't work
;; (defun vala-set-default-style (stylename &optional print-message)
;;   "Set the default style to use the style STYLENAME.
;; STYLENAME must be an existing Vala Mode style. The current list
;; of names can be found in `vala-style:presets'.

;; Using this function will not change the state of current
;; buffers. If successful, new buffers will be opened with the style
;; supplied."
;;   (interactive "Mtype name: \np"
;;    (if (not (member vala:styles:presets stylename))
;;        (error "Argument to vala-set-style-default was not recognized")
;;     (progn 
;;       (setq vala-indentation-style-default stylename)
;;       (when print-message
;;          (message "Vala indentaion style default changed to: %s" stylename))
;;      ))))


;;
;; Tabs vs Spaces
;;

(provide 'vala-mode2-styles)
