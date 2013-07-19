;;; vala-mode2-paragraph.el - paragraph fill for valadoc comments and strings for Vala Mode2             

;; Copyright (c) 2013 Robert Crowther                                           

;; Authors: 2013 Robert Crowther                             
;; Keywords: c languages                                 
;; Package: vala-mode2                                       

;;; For information on the License, see the LICENSE file      

;;; Commentary:                    

;;; Code:

(defconst vala-paragraph:paragraph-line-start-re
  (concat "\\(?:\\s *"                 ; whitespace
          "\\(?://+\\|\\*\\|/\\*+\\)"     ; comment start
          "\s*\\)"))                 ; whitespace
;;(looking-at vala-paragraph:paragraph-line-start-re) *

(defconst vala-paragraph:valadoc-list-start-re
  (concat "\\(?:\\(?:*\\|#\\)"         ; dotted/ordered lists
          "\\|[1IiAa]\\."              ; numeric/alphabetic lists
          "\\)\\s *"))

(defconst vala-paragraph:fill-first-line-re
  (concat "\\s *\\(//+\\|\\*\\||\\)?\\s *"
          "\\(?:" vala-paragraph:valadoc-list-start-re "\\)?"))

;; start or separate paragraphs
(defconst vala-paragraph:paragraph-start-re
  (concat vala-paragraph:paragraph-line-start-re
          "\\(?:$"                ; empty line
          "\\|=[^=\n]+=[ ]*$"     ; heading 1
          "\\|==[^=\n]+==[ ]*$"   ; heading 2
          "\\|===[^=\n]+===[ ]*$"   ; heading 3
          "\\|====+[^=\n]+====+[ ]*$"   ; heading 4-n
          "\\|"
          vala-paragraph:valadoc-list-start-re
          "\\|{{{"               ; code block start
          "\\|}}}"               ; code block end
          "\\|@[a-zA-Z]+\\>"      ; annotations
          "\\)"
          "\\|\\(?:\\s *\\*/\\)"  ; end of comment
          ))

(defconst vala-paragraph:paragraph-separate-re
  (concat vala-paragraph:paragraph-line-start-re
          "\\(?:$"
          "\\|=[^=\n]+=[ ]*$"     ; heading 1
          "\\|==[^=\n]+==[ ]*$"   ; heading 2
          "\\|===[^=\n]+===[ ]*$"   ; heading 3
          "\\|====+[^=\n]+====+[ ]*$"   ; heading 4-n
          "\\|@[a-zA-Z]+\\>"      ; annotations
          "\\|{{{"               ; code block start
          "\\|}}}"               ; code block end
          "\\)"
          "\\|\\(?:\\s *\\*/\\)"    ; end of comment
          ))


(defun vala-paragraph:adaptive-fill-function ()
  (let (fill)
    ;; the adaptive fill calls on a narrowed buffer range.
    (save-excursion
      (save-restriction
        (widen)
        (beginning-of-line)
        (cond 
         ;; block comments
         ((looking-at "\\s */?\\*+\\s *")
          ;; set fill to be whatever the asterixed prefix is or, if a
          ;; block comment start, replace strings to make at least 2
          ;; spaces then the asterix.
          (setq fill (replace-regexp-in-string
                      "/\\*+"
                      (lambda (str) (if (= (length str) 3) "  *" " *"))
                      (match-string-no-properties 0)))
          (goto-char (match-end 0))
          ;; make a special prefix grab on list paragraphs
          ;; if the items don't match, they'll concatenate.
          (when (looking-at vala-paragraph:valadoc-list-start-re)
            (setq fill
                  (concat fill (make-string (- (match-end 0)
                                               (match-beginning 0)) ?\s)))))

         ;; single line comments
         ;; special regex to capture the first asterix
         ((looking-at "\\(\\s *//\\)/*+")
          ;; not sure if we should do this, but make the filled area
          ;; tidy, even if the coder did not.
          (setq fill (concat (match-string 1) " "))
          (goto-char (match-end 0))
          )

         ;; anything else, return empty string as the prefix.  the
         ;; only other place fill-paragraph function is activated is
         ;; in strrings, so this will be appropriate,
         (t "")
         )
        )) fill))

(defun vala-paragraph:fill-paragraph-function (&rest args)
  ;; move to inside the comment/string
  (when (looking-at "\\s *\\(?:/\\*+\\|//\\|\"\"\"\\|\"\\)\\s *")
    (goto-char (match-end 0)))

  ;; the parse state is used in a couple of cases
  (let ((parse-state (syntax-ppss)))
    ;; TODO: remnant from scala-mode2.
    ;; whats the situation?  Can it occur?
    ;;(fill-paragraph-function
    ;; Avoid infinite recursion, set fill-paragraph-function to
    ;; nil if it is 'vala-paragraph:fill-paragraph
    ;;(unless (eq fill-paragraph-function 'vala-paragraph:fill-paragraph)
    ;;fill-paragraph-function))
    
    (cond 
     ;; inside comment
     ;; mask multi-line comments and fill
     ((nth 4 parse-state)
      (save-restriction
        ;; 8 = start position
        (narrow-to-region (nth 8 parse-state)
                          (save-excursion (goto-char (nth 8 parse-state))
                                          (if (forward-comment 1)
                                              (point)
                                            (point-max))))
        (message "fill called -%s-" (point))
        (fill-paragraph))
      t)

     ;; inside string
     ;; mask strings and fill.
     ((nth 3 parse-state)
      (save-restriction
        (narrow-to-region (nth 8 parse-state)
                          (save-excursion (goto-char (nth 8 parse-state))
                                          (or (ignore-errors
                                                (forward-sexp)
                                                (point))
                                              (point-max))))
        (fill-paragraph))
      t)

     ;; if nothing is recognised, do not fill.
     (t t))))

(provide 'vala-mode2-paragraph)
