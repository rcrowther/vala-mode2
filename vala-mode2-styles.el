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

    ;; 'as if syntax', space-munching, even handed style,
    ;; which ignores bracket sublties.
    ("ais"
        (vala-indent:nesting-definition-to-brackets . 2)
        (vala-indent:nesting-brackets-to-body . 2)
        (vala-indent:definition-to-brackets . 2)
        (vala-indent:brackets-to-body . 2))

    ("custom"
        (vala-indent:nesting-definition-to-brackets . 0)
        (vala-indent:nesting-brackets-to-body . 0)
        (vala-indent:definition-to-brackets . 0)
        (vala-indent:brackets-to-body . 0))
    )
  "Styles of indentation.
Elements of this alist are of the form:

  (STYLE-STRING zeroOrMore( (VARIABLE . VALUE) )

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any Emacs variable, and VALUE is the intended value
for that variable when using the selected style.")



;; Functions that manipulate styles
(defun vala-style:print-indents ()
  "Print the current indents.
For debugging"
  (interactive)
  (message "-%s--%s--%s--%s-"
           vala-indent:nesting-definition-to-brackets
           vala-indent:nesting-brackets-to-body
           vala-indent:definition-to-brackets
           vala-indent:brackets-to-body))


(defun vala-style:set-buffer-data-from-style (style-name)
  "Set style data for the current buffer.
STYLENAME must be an existing Vala Mode style. The current list
of names can be found in `vala-style:presets', or is displayed by
the Customize interface.
The function usually finds data in the preset table, but if the
'style-name' is 'custom', it sets the data from custom variables.
This is a dry function, not for external use."
  (message "indent style change!!! name:-%s-" style-name)
  (let (
        (style-data (assoc style-name vala-style:presets))
        (style-used style-name)
        )
    ;; ensure data
    (when (null style-data)
      (setq style-used "gnu")
      (setq style-data (assoc "gnu" vala-style:presets)))
    ;; make settings from the data in the style
    ;; filter custom
    (if (string= style-used "custom")
        (progn
          ;; gather custom settings and apply them
          (message "  custom style change")
          (setq vala-indent:nesting-definition-to-brackets
                vala-custom-nesting-definition-to-brackets)
          (setq vala-indent:nesting-brackets-to-body
                vala-custom-nesting-brackets-to-body)
          (setq vala-indent:definition-to-brackets
                vala-custom-definition-to-brackets)
          (setq vala-indent:brackets-to-body
                vala-custom-brackets-to-body)
          )
      ;; ...else, set from preset data
      (mapc
       (lambda (keyval) (set (car keyval) (cdr keyval)))
       (cdr style-data)))))


;;
;; Style variable
;;

(defun vala-style:set-style-variable-and-buffer-style (option-name value)
  "Set a style variable and the buffer style."
  ;; set the variable first
  (set option-name value)
  (vala-style:set-buffer-data-from-style value))

;; TODO: attempts to provide defcustom variable fail to provide
;; suitable list for filtering. Conclude: without suitable
;; documentation, Customize interface does not want us to use
;; it. Unsatisfactory but logical result.

(defcustom vala-indentation-style "gnu"
  "Set the style to style STYLENAME.
STYLENAME must be an existing Vala Mode style. The current list
of names can be found in `vala-style:presets', or is displayed by
the Customize interface.

This custom variable calls the function 'vala-style:set-buffer-data-from-style'
to make changes.
"
  :type   
  (cons 'radio
        (mapcar (lambda (kv) (list 'const (car kv) ) ) vala-style:presets)
        )
  :set 'vala-style:set-style-variable-and-buffer-style
  :group 'vala)



;;
;; Custom variables
;;
;;; custom variables declared after others to get initialization
;;; straight (:set-after not in right form?).
(defun vala-style:set-style-variable-and-buffer-style-custom (option-name
                                                              value)
  "Set a style variable and the buffer style.
Filters actions from this mode's custom variables. Sets the
variable first. Then, if (and only if) the style is 'custom', the
function will change the current buffer data."
  ;; set the variable first
  (set option-name value)

  (message "custom indent change!!! -%s--%s-" option-name value)
  (message " go? -%s-" vala-indentation-style)
  (message " go2? -%s-" (string= "custom" vala-indentation-style))
  ;; if 'custom', set the buffer style data
  (when (string= "custom" vala-indentation-style)
  ;;  (message "custom indent change!!!")
    (vala-style:set-buffer-data-from-style vala-indentation-style)
    )
)

(defcustom vala-custom-nesting-definition-to-brackets 0
  "Number of spaces from definition to brackets.
Used where definitions define a code block containing other
definitions, such as namespaces."
  :type 'integer
  :set 'vala-style:set-style-variable-and-buffer-style-custom
  ;; :set-after 'vala-indentation-style
  :group 'vala)

(defcustom vala-custom-nesting-brackets-to-body 0
  "Nummber of spaces from brackets to body.
Used where brackets contain other definitions, such as between
class bracketing and method definitions"
  :type 'integer
  :set 'vala-style:set-style-variable-and-buffer-style-custom
  ;; :set-after 'vala-indentation-style
  :group 'vala)

(defcustom vala-custom-definition-to-brackets 0
  "Number of spaces from definition to bracketing.
Used where definitions define a code block, such as between
method definitions and their brackets"
  :type 'integer
  :set 'vala-style:set-style-variable-and-buffer-style-custom
  ;; :set-after 'vala-indentation-style
  :group 'vala)

(defcustom vala-custom-brackets-to-body 0
  "Number of spaces between brackets and code.
Used where brackets contain a body of code, such as between
method bracketing and the code"
  :type 'integer
  :set 'vala-style:set-style-variable-and-buffer-style-custom
  ;; :set-after 'vala-indentation-style
  :group 'vala)


(provide 'vala-mode2-styles)
