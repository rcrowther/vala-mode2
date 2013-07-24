;;; vala-mode2-indent.el - indent functionality for Vala Mode2             

;; Copyright (c) 2013 Robert Crowther                         

;; Authors: 2013 Robert Crowther                        
;; Keywords: c languages  
;; Package: vala-mode2                        

;;; For information on the License, see the LICENSE file      

;;; Commentary:                    

;;; Code:

(require 'vala-mode2-syntax)
(require 'vala-mode2-lib)
; for debugging
; (load-file "./vala-mode2-syntax.el")


(defvar vala-indent:nesting-definition-to-brackets 2
  "The number of spaces from definition to brackets for
definitions which contain other definitions,
such as namespaces")

(defvar vala-indent:nesting-brackets-to-body 2
  "The number of spaces from brackets to body,
such as between class bracketing and method definitions")


(defvar vala-indent:definition-to-brackets 2
  "The number of spaces from definition to bracketing,
such as in a method")

(defvar vala-indent:brackets-to-body 2
  "The number of spaces between brackets and a body of code,
such as in a method")

(defun vala-indent:get-indent (var-name)
  "Get the indent for <VARNAME>.
This function tests for tab settings, and adjusts the indent
width accordingly, altering tab distances to tab-with if tabs
have been selected.

Indent settings should always be accessed through this function,
never directly.

<VARNAME> is one of the custom variables for indent settings.
"
  (let ((var-value (symbol-value var-name)))
    (if indent-tabs-mode 
        (if (> var-value 0) tab-width 0)
      var-value)))
;;(vala-indent:get-indent 'vala-indent:brackets-to-body)



;;;
;;; Info gathering
;;;

(defun vala-indent:get-indent-from-container (from-line-indent
                                              from-bracket-indent)
  "Find an indent by looking for an open bracket, or prefixing text.
Uses the syntax parse to find the opening bracket. If the bracket is
prefixed, on the same line, by text, the function uses the start of the
text as an anchor instead.
Returned indents are relative to the chosen anchor.
from-line-indent: extra indent to use if text is found
from-bracket-indent: indent to use if bracket only is found
return: the calculated indent if bracket match found, else 0"
  (save-excursion
    ;; Because Vala is fine with both spaces and brackets, we must be
    ;; careful here. The buffer positions are not the display
    ;; positions, for tabs, and (current-column) is needed for the
    ;; display positions.

    ;;  find the open bracket
    (let* (
           (anchor 0)
           (open-b (nth 1 (syntax-ppss (point))))
           )
      (if (null open-b)
          0
        (goto-char open-b)
        ;; point to ob's line start
        (goto-char (line-beginning-position))
        (if (and (looking-at "[\t ]*") (< (match-end 0) open-b))
            (progn
              ;;the opening of the line is earlier than the bracket
              (goto-char (match-end 0))
              (+ (current-column)
                 (+ from-line-indent from-bracket-indent)))
          ;;the line is the bracket only
          (goto-char open-b)
          (+ (current-column) from-bracket-indent)
          )))))
;  porridge((vala-indent:get-indent-from-container 3 2))


(defun vala-indent:get-body-indent-modifications-from-base ()
  "indent rules modifying body code from the container indent.
Currently:
- indents switch contents to behave as if parameterized
returns: any additional indent. Always positive numeric"
  ;; words to ignore
  ;; currently, break, case
  (if (looking-at vala-syntax:irregular-conditional-keywords-opt-re) 0
    (save-excursion
      (let ((open-parenthesis (nth 1 (syntax-ppss))))
        (if (and
             open-parenthesis
             ;; go back to the opening of the current parentheses
             (progn (goto-char open-parenthesis)
                    (skip-syntax-backward " ")
                    ;; backward skip any round brackets
                    (when (= (char-before) ?\))
                      (backward-sexp)
                      (skip-syntax-backward " "))
                    ;; back over a word, then look at it
                    (< (skip-syntax-backward "w_'") 0)))
             (cond
              ((looking-at "switch")
               (vala-indent:get-indent 'brackets-to-body)     
               )
              ;; TOCONSIDER: More indent sublties maybe here
              (t 0)
              ))
            0 ))))
;; (defun vala-syntax:looking-at-line-start (re)
;;   (save-excursion
;;     (goto-char (line-beginning-position))
;;     (looking-at re) 
;;     ))
; @duke (vala-syntax:looking-at-line-start "[ \t]*[A-Za-z*@]")


 

;;
;; calculating indents
;;

(defun vala-indent:calculate-indent-for-line ()
  "Calculate the appropriate indent for the current point or the
point 'point'. Returns the new column, or nil if the indent
cannot be determined."
  ;; compare to vala-paragraph:adaptive-fill-function
  ;; similar, but not the same.
  (save-excursion
    ;; Indent is *not* called on a narrowed buffer range.
    (beginning-of-line)
    (skip-syntax-forward " ")
    (cond
     ;; comments
     ;; TODO: This is capable of Java indentation, if anyone wants
     ;; that?
((looking-at vala-syntax:opening-grouped-comment-start-re)
      ;; The match could be beginning or end. if it exists, then the
      ;; match was not a block comment start.
      (if (match-beginning 1)
                                        ;(message "guessed in comment indent -%s-" (point))
          (vala-indent:get-indent-from-container
           (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
           (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body))
        (+ (vala-indent:get-indent-from-container
            (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
            (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body)) 1)))
     

     ;;TOCONSIDER: parameter mods woyuld probably go here?

     ;; open brackets
     ((or (= (char-after) ?\()  (= (char-after) ?\{))
      ;;TODO: If runon stategy, must check behind bracket too.
      (+ (vala-indent:get-indent-from-container
          (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
          (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body))
         (vala-indent:get-indent 'vala-indent:definition-to-brackets)))

     ;; close brackets
     ((or (= (char-after) ?\)) (= (char-after) ?\}))
      ;; forward-char gets out of the brackets into surrounding area.
      (forward-char)
      (+ (vala-indent:get-indent-from-container
          (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
          (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body))
         (vala-indent:get-indent 'vala-indent:definition-to-brackets)
         ))

     ((looking-at "throws")
      ;; special effort for the one outer multi-statement construct
      (+ (vala-indent:get-indent-from-container
          (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
          (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body))
         (vala-indent:get-indent 'vala-indent:definition-to-brackets)))

     ;;not in comments or brackets
     ((vala-syntax:syntax-class-inheritance-item-p)
      ;;(goto-char (line-beginning-position))
      (+ (vala-indent:get-indent-from-container
          (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
          (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body))
         (vala-indent:get-indent 'vala-indent:definition-to-brackets)))

     ((vala-syntax:syntax-function-or-class-declaration-p)
      ;; declarations
      (vala-indent:get-indent-from-container
       (vala-indent:get-indent 'vala-indent:nesting-definition-to-brackets)
       (vala-indent:get-indent 'vala-indent:nesting-brackets-to-body)))

     (t
     (message "guessed in body indent -%s-" 
(vala-indent:get-body-indent-modifications-from-base))
      ;; bodies
      (+ (vala-indent:get-body-indent-modifications-from-base)
         (vala-indent:get-indent-from-container
          (vala-indent:get-indent 'vala-indent:definition-to-brackets)
          (vala-indent:get-indent 'vala-indent:brackets-to-body))
         )))
    ))

(defun vala-indent:indent-line-to (column)
  "Indent the line to column and move cursor to the indent
column, if it was at the left margin."
  (when column
    (if (<= (current-column) (current-indentation))
        ;; Not-even-documented (in manual) function
        (indent-line-to column)
      (save-excursion (indent-line-to column)))))



(defun vala-indent:indent-code-line ()
  "Indent a line of code. Expects to be outside of any comments or
strings"
  (vala-indent:indent-line-to (vala-indent:calculate-indent-for-line))
  (vala-lib:delete-trailing-whitespace))


;;
;; Main call
;;
(defun vala-indent:indent-line (&optional strategy)
  "Indents the current line."
  (vala-indent:indent-code-line)
  )

(provide 'vala-mode2-indent)
