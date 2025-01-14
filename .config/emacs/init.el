;;; packages --- .emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Commentary:
; trying to keep it at least a bit minimalistic

;;; Code:
(server-start)
(load-library "lilypond-mode")
(load (concat user-emacs-directory "functions.el"))
(require 'org-archive)
; Settings
(setq
 auto-dark-dark-theme 'dracula
 backup-directory-alist '(("." . "~/.cache/emacs"))
 bibtex-completion-library-path '("~/Nextcloud/Studium/PDFs/")
 bibtex-completion-pdf-open-function (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))
 compilation-scroll-output t
 display-buffer-alist '((".*" (display-buffer-reuse-window display-buffer-same-window) (reusable-frames . t)))
 display-line-numbers-type 'relative
 echo-keystrokes .1
 evil-want-C-i-jump nil
 graphviz-dot-preview-extension "pdf"
 helm-ff-skip-boring-files t
 initial-major-mode 'text-mode
 lsp-haskell-plugin-stan-global-on nil
 markdown-command '("pandoc" "--from=markdown" "--to=html5")
 mouse-yank-at-point t
 org-agenda-custom-commands
   '(("n" "Agenda & TODOs" ((agenda) (todo)) ((org-agenda-skip-scheduled-if-done t)))
     ("r" "Review" ((agenda)) ((org-agenda-overriding-header "Done:")
                               (org-agenda-start-day "-2d")
                               (org-agenda-start-with-log-mode 'only)
                               (org-agenda-log-mode-items '(closed state))
                               (org-agenda-tag-filter-preset '("-nolog")))))
 org-agenda-files (file-expand-wildcards "~/Documents/Nextcloud2/Orgzly/*todo.org*")
 org-agenda-show-future-repeats 'next
 org-agenda-span 3
 org-agenda-start-on-weekday nil
 org-agenda-todo-ignore-scheduled 'all
 org-agenda-todo-ignore-deadlines 'all
 org-agenda-use-time-grid nil
 org-cite-global-bibliography '("~/Nextcloud/roam/roam.bib")
 org-confirm-babel-evaluate nil
 org-edit-src-content-indentation 0
 org-enforce-todo-dependencies t
 org-extend-today-until 2
 org-latex-preview-ltxpng-directory "~/.cache/emacs/ltximg"
 org-log-done 'time
 org-log-into-drawer t
 org-roam-directory (file-truename "~/Nextcloud/roam")
 org-roam-mode-sections (list 'org-roam-backlinks-section
                              'org-roam-reflinks-section
                              'org-roam-unlinked-references-section)
 org-roam-node-display-template "${title} ${tags}"
 org-startup-folded 'show3
 org-src-fontify-natively t
 org-tags-column 0
 recentf-max-saved-items nil
 scroll-conservatively 30
 scroll-margin 4
 scroll-step 1
 visible-bell t
 vc-follow-symlinks t
 LilyPond-pdf-command "emacsclient"
)
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
(org-link-set-parameters "textedit" :follow #'org-textedit-open)
(put 'magit-clean 'disabled nil)

; Keybinds
(global-set-key (kbd "M-o n") 'flymake-goto-next-error)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n b") 'org-roam-buffer-toggle)
(global-set-key (kbd "M-[") 'shell-command)
(global-set-key (kbd "M-]") 'async-shell-command)
(global-unset-key (kbd "C-x C-c"))

; Modes
(auto-dark-mode)
(column-number-mode)
(electric-pair-mode)
(evil-mode)
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(global-flycheck-mode)
(global-hl-todo-mode)
(global-visual-line-mode)
(helm-mode)
(icomplete-mode)
(org-roam-db-autosync-mode)
(recentf-mode)
(save-place-mode)
(savehist-mode)
(show-paren-mode)
(tab-bar-mode)
(winner-mode)

(customize-set-variable 'helm-boring-file-regexp-list
  (append '("\\.bcf$" "\\.fls$" "\\.nav$" "\\.lol$" "\\.out$" "\\.ptb$" "\\.snm$" "\\.vrb$" "\\.run.xml$" "\\.synctex.gz$") helm-boring-file-regexp-list))

; Graphic things
(blink-cursor-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(dolist (p '(
             (font . "IBM Plex Mono-12")
             (fullscreen . maximized)
             (width . 80) (height . 24)
            ))
  (add-to-list 'default-frame-alist p))
(set-cursor-color "#ffffff")

; Evil and line number exclusions
(dolist (m '(
             buffer-menu-mode
             calc-mode
             calc-trail-mode
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
             ediff-mode-hook
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

; LSP & language launchers
(dolist (m '(
             c-mode-hook
             c++-mode-hook
             go-mode-hook
             haskell-mode-hook
             html-mode-hook
             java-mode-hook
             js-mode-hook
             TeX-mode-hook
             rust-mode-hook
             scala-mode-hook
             python-mode-hook
             sh-mode-hook
            ))
  (add-hook m 'lsp-deferred))
(add-hook 'agda2-mode-hook (lambda () (local-set-key (kbd "M-o g g") 'agda2-goto-definition-keyboard)))
(add-hook 'bibtex-mode-hook (lambda () (progn (local-set-key (kbd "C-c C-o") 'org-ref-bibtex-hydra/org-ref-open-bibtex-pdf-and-exit)
                                              (local-set-key (kbd "C-c C-d") 'doi-utils-async-download-pdf))))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'lsp-mode-hook (lambda () (progn (define-key lsp-mode-map (kbd "M-o") lsp-command-map)
                                           (keymap-local-set "<tab-bar> <mouse-movement>" 'ignore))))
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)
(add-hook 'lean-mode-hook (lambda () (set-input-method 'Lean)))
(add-hook 'org-mode-hook (lambda () (progn (local-set-key (kbd "M-p") 'org-meta-return)
                                           (plist-put org-format-latex-options :scale 2))))
(dolist (f '(highlight-indentation-mode
             highlight-indentation-current-column-mode))
  (add-hook 'prog-mode-hook f))
(add-hook 'python-mode-hook 'elpy-enable)
(load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))

; Whitespace
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'go-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'haskell-mode-hook (lambda () (progn (define-key evil-normal-state-map "o" 'hly/evil-open-below)
                                               (define-key evil-normal-state-map "O" 'hly/evil-open-above)
                                               (setq lsp-rename-use-prepare nil))))
(add-hook 'lisp-data-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'org-agenda-mode-hook (lambda () (visual-line-mode -1) (setq truncate-lines 1)))
; These exist not for specific projects, but more as examples how to set things up
(add-hook 'c-mode-hook (lambda () (setq tab-width 2
                                        c-basic-offset 2)))
(add-hook 'c++-mode-hook (lambda () (setq indent-tabs-mode nil
                                          c-basic-offset 4)))
(add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil
                                           tab-stop-list (number-sequence 4 200 4))))

; LaTeX
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
(put 'LaTeX-mode 'flyspell-mode-predicate 'auctex-mode-flyspell-skip)
(dolist (f '((lambda () (progn (add-to-list 'TeX-command-list '("latexmk" "latexmk %t" TeX-run-TeX nil t))
                               (local-set-key (kbd "M-p") 'LaTeX-insert-item)
                               (setq
                                TeX-command-default "latexmk"
                                flyspell-generic-check-word-predicate 'auctex-mode-flyspell-skip)))
             fix-electric-pair-paired-delimiters-in-tex-mode
             flymake-mode
             flyspell-mode
             latex-electric-env-pair-mode
             turn-on-reftex
             TeX-source-correlate-mode
            ))
  (add-hook 'LaTeX-mode-hook f))

(provide 'init)
;;; init.el ends here
