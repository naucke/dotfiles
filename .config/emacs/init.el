;;; packages --- Summary
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(; Languages
     auctex elpy flymake-shellcheck go-mode lean-mode rust-mode yaml-mode
     ; Workflow
     evil flycheck hl-todo lsp-mode magit org-chef pdf-tools ranger vterm)))

;;; Commentary:
; trying to keep it at least a bit minimalistic

;;; Code:
; Settings
(setq
 backup-directory-alist '(("." . "~/.cache/emacs"))
 compilation-scroll-output t
 display-line-numbers-type 'relative
 evil-want-C-i-jump nil
 initial-major-mode 'text-mode
 mouse-yank-at-point t
 org-agenda-files '("~/Documents/orgzly")
 org-agenda-file-regexp "\\`.*todo\\.org\\'"
 org-agenda-show-future-repeats 'next
 org-agenda-todo-ignore-scheduled 'future
 org-enforce-todo-dependencies t
 org-log-repeat nil
 org-tags-column 0
 scroll-conservatively 30
 scroll-margin 4
 scroll-step 1
 visible-bell t
 vc-follow-symlinks t
)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(put 'magit-clean 'disabled nil)

; Modes
(column-number-mode)
(electric-pair-mode)
(evil-mode)
(global-display-line-numbers-mode)
(global-flycheck-mode)
(global-hl-todo-mode)
(global-visual-line-mode)
(icomplete-mode)
(ranger-override-dired-mode)
(savehist-mode)
(show-paren-mode)
(tab-bar-mode)
(winner-mode)

; Graphical mode fixes (does not apply to emacs-nox)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

; Styling (dto. emacs-nox)
(dolist (p '(
	     (font . "IBM Plex Mono-10")
	     (fullscreen . maximized)
	    ))
  (add-to-list 'default-frame-alist p))
      
(load-theme 'wombat)
(set-cursor-color "#ffffff")

; Evil, line number, and window exclusions
(dolist (m '(
	     buffer-menu-mode
	     compilation-mode
             image-mode
             image-dired-thumbnail-mode
             Info-mode
             TeX-output-mode
             xref--xref-buffer-mode
	    ))
  (evil-set-initial-state m 'emacs))
(dolist (m '(
             compilation-mode-hook
             eshell-mode-hook
             gdb-mode-hook
             inferior-python-mode-hook
             jdb-mode-hook
             org-agenda-mode-hook
             pdf-view-mode-hook
             TeX-output-mode-hook
             vterm-mode-hook
	     ))
  (add-hook m (lambda () (display-line-numbers-mode 0))))
(dolist (b '(
             "*compilation*"
             "*Help*"
             "*xref*"
             "*Python*"
             "*Geiser Racket REPL*"
	    ))
  (add-to-list 'same-window-buffer-names b))
(dolist (r '(
             "magit: .+"
             ".+\.pdf"
	    ))
  (add-to-list 'same-window-regexps r))

; LSP launchers
(dolist (m '(
             c-mode-hook
             c++-mode-hook
             go-mode-hook
             rust-mode-hook
             perl-mode-hook
             python-mode-hook
             sh-mode-hook
	     ))
  (add-hook m 'lsp-deferred))
(add-hook 'lsp-mode-hook (lambda () (define-key lsp-mode-map (kbd "M-o") lsp-command-map)))
(add-hook 'lean-mode-hook (lambda () (set-input-method 'Lean)))
(add-hook 'python-mode-hook 'elpy-enable)

; No tabs
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

; AucTeX
(pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))
(dolist (f '(
             (lambda () (push '("latexmk" "latexmk" TeX-run-TeX nil t) TeX-command-list))
             turn-on-reftex
	    ))
  (add-hook 'LaTeX-mode-hook f))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
