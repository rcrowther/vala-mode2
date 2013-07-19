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

(provide 'vala-mode2-lib)
