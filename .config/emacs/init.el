;;; packages --- Summary
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex elpy evil geiser-racket go-mode flycheck lsp-metals lsp-mode magit pdf-tools scala-mode ranger rust-mode vterm)))

;;; Commentary:
; trying to keep it at least a bit minimalistic

;;; Code:
; Modes
(column-number-mode)
(evil-mode)
(global-display-line-numbers-mode)
(global-flycheck-mode)
(global-visual-line-mode)
(ranger-override-dired-mode)
(savehist-mode)
(show-paren-mode)

; Settings
(setq compilation-scroll-output t
      display-line-numbers-type 'relative
      mouse-yank-at-point t
      scroll-margin 4
      scroll-step 1
      visible-bell t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(put 'magit-clean 'disabled nil)

; Graphical mode fixes (does not apply to emacs-nox)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

; Styling (dto. emacs-nox)
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))
(load-theme 'wombat)
(set-cursor-color "#ffffff")

; Evil, line number, and window exclusions
(mapc (lambda (m) (evil-set-initial-state m 'emacs))
      '(buffer-menu-mode compilation-mode image-mode image-dired-thumbnail-mode info-mode TeX-output-mode xref-mode))
(mapc (lambda (m) (add-hook m (lambda () (display-line-numbers-mode 0))))
      '(compilation-mode-hook eshell-mode-hook gdb-mode-hook inferior-python-mode-hook
        jdb-mode-hook pdf-view-mode-hook TeX-output-mode-hook vterm-mode-hook))
(setq same-window-buffer-names '("*compilation*"))

; LSP launchers
(mapc (lambda (m) (add-hook m 'lsp-deferred))
      '(c-mode-hook c++-mode-hook go-mode-hook rust-mode-hook python-mode-hook scala-mode-hook))
(add-hook 'python-mode-hook 'elpy-enable)

; No tabs
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

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
