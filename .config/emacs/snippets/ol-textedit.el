;;; ol-textedit.el --- Handle `textedit:` links
;;; Commentary:
;;; like LilyPond ships
;;; Code:
(require 'ol)
(org-link-set-parameters "textedit" :follow #'org-textedit-open)
(defun org-textedit-open (path _)
  "Open textedit link PATH with emacsclient."
  (message path)
  ; AFAICT org has really nice functionality for this... for things at point.
  (string-match "^//\\(.+\\):\\([0-9]+:[0-9]+\\):[0-9]+$" path)
  (call-process-shell-command (format "emacsclient +%s %s &" (match-string 2 path) (match-string 1 path)) nil 0))
(provide 'ol-textedit)
;;; ol-textedit.el ends here
