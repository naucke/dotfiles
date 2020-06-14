(custom-set-variables '(package-selected-packages '(auctex auto-complete elpy evil jedi magit package)))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(load-theme 'wombat)
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))
(add-to-list 'default-frame-alist '(alpha . (85 . 75)))

(global-display-line-numbers-mode)
(setq column-number-mode t)
(setq show-paren-delay 0)
(show-paren-mode 1)

(global-set-key (kbd "M-y") 'shell-command)

(evil-mode 1)
(elpy-enable)
(global-set-key (kbd "C-x g") 'magit-status)

(ac-config-default)
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

(with-eval-after-load "tex"
  (add-to-list 'TeX-view-program-list '("mupdf" "mupdf %o"))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf")))

(with-eval-after-load "tex"
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-to-list 'TeX-view-program-list '("mupdf" ("mupdf" " %o" (mode-io-correlate " %(outpage)"))))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf")))
