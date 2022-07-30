; Packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex evil go-mode flycheck lsp-mode magit pdf-tools rust-mode)))

; Modes
(column-number-mode)
(evil-mode)
(global-display-line-numbers-mode)
(global-flycheck-mode)
(global-visual-line-mode)
(savehist-mode)
(show-paren-mode)

; Settings
(setq compilation-scroll-output t
      display-line-numbers-type 'relative
      mouse-yank-at-point t
      scroll-margin 4
      scroll-step 1
      visible-bell t)

; Graphical mode fixes (does not apply to emacs-nox)
(blink-cursor-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Styling (dto. emacs-nox)
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))
(load-theme 'wombat)
(set-cursor-color "#ffffff")

; Evil exclusions
(evil-set-initial-state 'compilation-mode 'emacs)
(evil-set-initial-state 'image-mode 'emacs)
(evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
(evil-set-initial-state 'info-mode 'emacs)
(evil-set-initial-state 'xref-mode 'emacs)

; LSP launchers
(add-hook 'c-mode-hook 'lsp)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'python-mode-hook 'lsp-deferred)

; No tabs
(defun disable-tabs () (setq indent-tabs-mode nil))
(add-hook 'rust-mode-hook 'disable-tabs)

; AucTeX
(pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook (lambda () (push '("latexmk" "latexmk" TeX-run-TeX nil t) TeX-command-list)))
(add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
