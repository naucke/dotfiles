;;; flyspell-envs.el --- Exclude environments from flyspell
;;; Commentary:
;;; from https://tex.stackexchange.com/a/154540
;;; Code:
(put 'LaTeX-mode 'flyspell-mode-predicate 'auctex-mode-flyspell-skip)
(defun auctex-mode-flyspell-skip ()
  "Define skips."
  (save-excursion (widen)
    (let ((p (point)) (count 0))
      (not (or (and (re-search-backward "\\\\begin{\\(minted\\|verbatim\\)}" nil t) (> p (point))
                    (or (not (re-search-forward "^\\\\end{\\(minted\\|verbatim\\)}" nil t)) (< p (point))))
               (eq 1 (progn (while (re-search-backward "`" (line-beginning-position) t)
                              (setq count (1+ count)))
                            (- count (* 2 (/ count 2))))))))))
(add-hook 'LaTeX-mode-hook (lambda () (setq flyspell-generic-check-word-predicate 'auctex-mode-flyspell-skip)))
(provide 'flyspell-envs)
;;; flyspell-envs.el ends here
