;;; vala-mode2-lib.el - Major mode for editing vala
;;; Copyright (c) 2012 Heikki Vesalainen
;;; Copyright (c) 2013 Robert Crowther
;;; For information on the License, see the LICENSE file


(defun vala-lib:delete-trailing-whitespace ()
  (save-excursion
    (end-of-line)
    (skip-syntax-backward " ")
    (unless (bolp)
      (delete-char (- (line-end-position) (point))))))



(defun vala-lib:buffer-has-tabs-p ()
  "Is buffer indented with spaces or tabs?
return: nill if SPACE, t if TAB. Default (scan reaches buffer
end) is nil."
(interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (or
                 (eobp)
                 (eq (char-after) ?\t)
                 ;; quit scanning if a line with space and a word
                 ;; syntax character is found. This would be lines
                 ;; with declarations on them, for example, but not
                 ;; lines with comments. Hopefully this heuristic is
                 ;; good enough for Vala.
                 (looking-at "[ ]+\\sw")))
      (forward-line))
    (eq (char-after) ?\t)))
;;(vala-lib:buffer-has-tabs-p)

(provide 'vala-mode2-lib)
