;;; mz-visual --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Visual and aesthetic configuration:
;; - theme
;; - modeline
;; - text highlights

;;; Code:

(require 'mz-package-management)

(use-package all-the-icons
  :config
  (use-package all-the-icons-completion
    :after marginalia)
  :hook (emacs-startup-hook . all-the-icons-completion-mode))

(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode +1)
(global-display-line-numbers-mode +1)
(global-hl-line-mode +1)
(global-prettify-symbols-mode +1)
(global-visual-line-mode +1)
(line-number-mode +1)
(size-indication-mode +1)

(setq inhibit-startup-screen t
      initial-scratch-message "")

(use-package dashboard
  :demand t
  :functions
  (dashboard-setup-startup-hook)
  :defines
  (dashboard-footer-messages
   dashboard-item-names
   dashboard-items
   dashboard-projects-backend)
  :config
  (setq dashboard-footer-messages '("On the cutting edge of cocking about"
                                    "If I were a girl I'd be pregnant a lot"
                                    "What could possibly go wrong?"
                                    "I honestly believe that my genius, it generates gravity"
                                    "Which seat do they put in as standard?")
        dashboard-item-names '(("Projects:"     . "[p] Recent projects:")
                               ("Recent Files:" . "[r] Recent files:"))
        dashboard-items '((projects . 5)
                          (recents  . 10))
        dashboard-projects-backend 'project-el
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(bind-key "C--" #'text-scale-adjust)
(bind-key "C-=" #'text-scale-adjust)

(use-package default-text-scale
  :demand t
  :functions
  (default-text-scale-mode)
  :config
  (set-face-attribute 'default nil :height 140)
  (default-text-scale-mode +1))

(use-package ef-themes
  :demand t
  :init
  (defvar mz/ef-dark 'ef-dark)
  (defvar mz/ef-light 'ef-light)
  :functions
  (ef-themes--load-theme
   ef-themes-toggle)
  :hook (emacs-startup-hook . (lambda () (ef-themes--load-theme mz/ef-dark)))
  :bind (("C-S-t" . #'ef-themes-toggle))
  :custom
  (ef-themes-to-toggle (list mz/ef-dark mz/ef-light)))

(use-package modus-themes
  :custom
  (modus-themes-syntax '(yellow-comments)))

(use-package minions
  :hook (after-init-hook . minions-mode))

(use-feature whitespace
  :hook (prog-mode-hook . whitespace-mode)
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face
                      tab-mark
                      empty
                      trailing
                      lines-tail)))

(use-package hl-todo
  :hook
  ((prog-mode-hook org-mode-hook) . hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook web-mode-hook)

(use-package git-gutter
  :hook
  (after-init-hook . global-git-gutter-mode))

(provide 'mz-visual)
;;; mz-visual.el ends here
