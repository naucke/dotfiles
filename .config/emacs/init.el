; Packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables '(package-selected-packages '(auctex evil go-mode lsp-mode magit pdf-tools rust-mode)))

; Modes
(column-number-mode)
(evil-mode)
(global-display-line-numbers-mode)
(global-visual-line-mode)
(savehist-mode)
(show-paren-mode)

; Settings
(setq display-line-numbers 'relative
      mouse-yank-at-point t
      scroll-margin 4
      scroll-step 1)

; Graphical mode fixes (does not apply to emacs-nox)
(blink-cursor-mode -1)
(menu-bar-mode -1)
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

; LSP launchers
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'python-mode-hook 'lsp-deferred)

; No tabs
(defun disable-tabs () (setq indent-tabs-mode nil))
(add-hook 'rust-mode-hook 'disable-tabs)

; Avoid i3/Sway clash
(global-set-key (kbd "M-c") 'shell-command)

; AucTeX
(pdf-loader-install)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook (lambda () (push '("latexmk" "latexmk -pdf" TeX-run-TeX nil t) TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
