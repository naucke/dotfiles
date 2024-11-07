;;; dollar-electric.el --- Fix $ for electric pair in LaTeX
;;; Commentary:
;;; from https://emacs.stackexchange.com/a/42415
;;; Code:
(defun fix-electric-pair-paired-delimiters-in-tex-mode ()
  "Fix $ for electric pair in LaTeX."
  (add-function
   :around
   (local 'electric-pair-skip-self)
   (lambda (oldfun c)
     (pcase (electric-pair-syntax-info c)
       (`(,syntax ,_ ,_ ,_)
        (if (eq syntax ?$)
            (unwind-protect
                (progn
                  (delete-char -1)
                  (texmathp))
              (insert-char c))
          (funcall oldfun c)))))
   '((name . fix-electric-pair-paired-delimiters-in-tex-mode))))
(provide 'dollar-electric)
;;; dollar-electric.el ends here
