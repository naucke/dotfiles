;;; functions.el --- Extra .emacs functions
;;; Commentary:
;;; extra .emacs functions
;;; Code:
(defun auctex-mode-flyspell-skip ()
  "Define AucTeX skips, see https://tex.stackexchange.com/a/154540."
  (save-excursion (widen)
    (let ((p (point)) (count 0))
      (not (or (and (re-search-backward "\\\\begin{\\(minted\\|verbatim\\)}" nil t) (> p (point))
                    (or (not (re-search-forward "^\\\\end{\\(minted\\|verbatim\\)}" nil t)) (< p (point))))
               (eq 1 (progn (while (re-search-backward "`" (line-beginning-position) t)
                              (setq count (1+ count)))
                            (- count (* 2 (/ count 2))))))))))

(defun fix-electric-pair-paired-delimiters-in-tex-mode ()
  "Fix $ for electric pair in LaTeX, see https://emacs.stackexchange.com/a/42415."
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

(defun hly/evil-open-below (count)
  "Open COUNT below, see https://github.com/haskell/haskell-mode/issues/1265."
  (interactive "p")
  (setq unread-command-events (listify-key-sequence (kbd "RET")))
  (evil-append-line count))
(defun hly/evil-open-above (count)
  "Open COUNT above, see https://github.com/haskell/haskell-mode/issues/1265."
  (interactive "p")
  (forward-line -1)
  (hly/evil-open-below count))

(defun org-archive-done-tasks ()
  "Archive done tasks in org mode, see https://stackoverflow.com/a/27043756."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'agenda))

(require 'ol)
(defun org-textedit-open (path _)
  "Open textedit link PATH with emacsclient."
  (message path)
  ; AFAICT org has really nice functionality for this... for things at point.
  (string-match "^//\\(.+\\):\\([0-9]+:[0-9]+\\):[0-9]+$" path)
  (call-process-shell-command (format "emacsclient +%s %s &" (match-string 2 path) (match-string 1 path)) nil 0))
(provide 'functions)
;;; functions.el ends here
