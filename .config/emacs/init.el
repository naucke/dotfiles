(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'auto-complete)
(require 'elpy)
(require 'evil)
(require 'magit)
(require 'package)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(load-theme 'wombat)
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))

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
  (add-to-list 'TeX-view-program-list '("org.gnome.Evince" "org.gnome.Evince %o"))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("org.gnome.Evince")))

(with-eval-after-load "tex"
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-to-list 'TeX-view-program-list '("org.gnome.Evince" ("org.gnome.Evince" " %o" (mode-io-correlate " %(outpage)"))))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("org.gnome.Evince")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit evil elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
