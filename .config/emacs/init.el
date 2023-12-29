;;; packages --- Summary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961"))
 '(package-selected-packages
   '(; Imperative languages
     elpy go-mode flymake-shellcheck rust-mode
     ; Declarative languages
     haskell-mode lean-mode lsp-haskell lsp-metals scala-mode
     ; Markup languages
     auctex dockerfile-mode graphviz-dot-mode yaml-mode
     ; Org
     org-chef org-drill org-ref org-roam
     ; Styling
     auto-dark dracula-theme
     ; Non-org workflow
     evil flycheck helm hl-todo lsp-mode magit pdf-tools ranger vterm)))

;;; Commentary:
; trying to keep it at least a bit minimalistic

;;; Code:
(server-start)
(load-library "lilypond-mode")
; Settings
(setq
 auto-dark-dark-theme 'dracula
 backup-directory-alist '(("." . "~/.cache/emacs"))
 bibtex-completion-library-path '("~/Nextcloud/Studium/PDFs/")
 bibtex-completion-pdf-open-function (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))
 compilation-scroll-output t
 display-line-numbers-type 'relative
 evil-want-C-i-jump nil
 echo-keystrokes .1
 graphviz-dot-preview-extension "pdf"
 initial-major-mode 'text-mode
 markdown-command '("pandoc" "--from=markdown" "--to=html5")
 mouse-yank-at-point t
 org-agenda-files '("~/Documents/orgzly")
 org-agenda-show-future-repeats 'next
 org-agenda-span 3
 org-agenda-start-on-weekday nil
 org-agenda-todo-ignore-scheduled 'all
 org-agenda-todo-ignore-deadlines 'all
 org-cite-global-bibliography '("~/Nextcloud/roam/roam.bib")
 org-confirm-babel-evaluate nil
 org-enforce-todo-dependencies t
 org-extend-today-until 2
 org-latex-preview-ltxpng-directory "~/.cache/emacs/ltximg"
 org-log-repeat nil
 org-roam-directory (file-truename "~/Nextcloud/roam")
 org-roam-mode-sections (list 'org-roam-backlinks-section
                              'org-roam-reflinks-section
                              'org-roam-unlinked-references-section)
 org-src-fontify-natively t
 org-tags-column 0
 scroll-conservatively 30
 scroll-margin 4
 scroll-step 1
 visible-bell t
 vc-follow-symlinks t
 warning-minimum-level :emergency
 LilyPond-pdf-command "emacsclient"
)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n b") 'org-roam-buffer-toggle)
(global-set-key (kbd "M-p") 'org-meta-return)
(add-hook 'org-mode-hook (lambda () (plist-put org-format-latex-options :scale 2)))
(put 'magit-clean 'disabled nil)

; Modes
(auto-dark-mode)
(column-number-mode)
(electric-pair-mode)
(evil-mode)
(global-display-line-numbers-mode)
(global-flycheck-mode)
(global-hl-todo-mode)
(global-visual-line-mode)
(helm-mode)
(icomplete-mode)
(org-roam-db-autosync-mode)
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
      
(set-cursor-color "#ffffff")

; Evil, line number, and window exclusions
(dolist (m '(
             buffer-menu-mode
             calc-mode
             compilation-mode
             eshell-mode
             image-mode
             image-dired-thumbnail-mode
             haskell-error-mode
             haskell-interactive-mode
             Info-mode
             TeX-output-mode
             xref--xref-buffer-mode
             vterm-mode
            ))
  (evil-set-initial-state m 'emacs))
(dolist (m '(
             compilation-mode-hook
             eshell-mode-hook
             gdb-mode-hook
             inferior-python-mode-hook
             org-agenda-mode-hook
             pdb-mode-hook
             pdf-view-mode-hook
             ranger-mode-hook
             TeX-output-mode-hook
             vterm-mode-hook
             ))
  (add-hook m (lambda () (display-line-numbers-mode 0))))
(dolist (b '(
             "*compilation*"
             "*grep*"
             "*xref*"
             "*Help*"
             "*Lean Goal*"
             "*Python*"
            ))
  (add-to-list 'same-window-buffer-names b))
(dolist (r '(
             "magit: .+"
             ".+\\.pdf"
             ".+\\.el\\(\\.gz\\)?"
            ))
  (add-to-list 'same-window-regexps r))
; Org does it differently(tm)
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

; LSP & language launchers
(dolist (m '(
             c-mode-hook
             c++-mode-hook
             haskell-mode-hook
             html-mode-hook
             js-mode-hook
             rust-mode-hook
             scala-mode-hook
             python-mode-hook
             sh-mode-hook
            ))
  (add-hook m 'lsp-deferred))
(add-hook 'lsp-mode-hook (lambda () (define-key lsp-mode-map (kbd "M-o") lsp-command-map)))
(add-hook 'lsp-mode-hook (lambda () (keymap-local-set "<tab-bar> <mouse-movement>" 'ignore)))
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)
(add-hook 'lean-mode-hook (lambda () (set-input-method 'Lean)))
(add-hook 'python-mode-hook 'elpy-enable)
(load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))

; Whitespace
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
; These exist not a specific projects, but more as examples how to set things up
(add-hook 'c-mode-hook (lambda () (setq tab-width 2
                                        c-basic-offset 2)))
(add-hook 'c++-mode-hook (lambda () (setq indent-tabs-mode nil
                                          c-basic-offset 4)))
(add-hook 'org-agenda-mode-hook (lambda () (visual-line-mode -1) (setq truncate-lines 1)))

; PDF
(pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))
(dolist (f '(
             (lambda () (push '("latexmk" "latexmk" TeX-run-TeX nil t) TeX-command-list))
             turn-on-reftex
            ))
  (add-hook 'LaTeX-mode-hook f))

(load "~/.config/emacs/snippets/ol-textedit")
(require 'ol-textedit)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
