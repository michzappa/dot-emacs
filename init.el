;;; init --- Michael Zappa's Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This configuration is the cumulative result of my Emacs experience
;; since December 2020. Its primary philosophy is the preservation of
;; vanilla Emacs' interface with modern interactivity. This is
;; achieved with packages like `vertico', `consult', and `ctrlf'.

;;; Code:
(require 'comp)
(require 'compile)
(require 'use-package)

(setq user-full-name "Michael Zappa"
      user-mail-address "me@michzappa.com")

;; Centralize customizations in a single file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Performance Tuning.
(setq gc-cons-threshold (* 100 1024 1024)
      native-comp-async-report-warnings-errors nil)

;; Configure `use-package'.
(setq use-package-always-defer t
      use-package-hook-name-suffix "")

;; Turn unneeded visual clutter off.
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Turn desired visual clutter on.
(column-number-mode +1)
(delete-selection-mode +1)
(global-auto-revert-mode +1)
(global-display-line-numbers-mode +1)
(global-hl-line-mode +1)
(global-prettify-symbols-mode +1)
(global-subword-mode +1)
(global-visual-line-mode +1)
(line-number-mode +1)
(size-indication-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

;; Defaults for all buffers.
(setq-default tab-width 4
              indent-tabs-mode nil
              sentence-end-double-space nil)

;; Basic settings.
(setq auto-save-default nil
      compilation-scroll-output t
      confirm-kill-processes nil
      create-lockfiles nil
      delete-by-moving-to-trash t
      dired-listing-switches "-al --group-directories-first"
      enable-recursive-minibuffers t
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t
      make-backup-files nil
      mode-require-final-newline 'visit-save
      ring-bell-function #'ignore
      save-interprogram-paste-before-kill t
      split-height-threshold nil
      vc-follow-symlinks t)

;; Maximize all frames upon creation.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'server-after-make-frame-hook
          #'(lambda () (let ((frame (selected-frame)))
                    (select-frame-set-input-focus frame)
                    (set-frame-parameter frame 'fullscreen 'maximized))))

(add-hook 'compilation-filter-hook
          #'(lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;; General mouse configuration (most applicable for GUI Emacs).
(setq mouse-wheel-progressive-speed nil
      scroll-conservatively 100000
      scroll-margin 0
      scroll-preserve-screen-position t
      scroll-step 1)

;; Mouse configuration in TUI Emacs.
(unless (display-graphic-p)
  (xterm-mouse-mode t)
  (bind-key [mouse-4] (lambda () (interactive)
                        (scroll-down 1)))
  (bind-key [mouse-5] (lambda () (interactive)
                        (scroll-up 1))))

;; Bind basic functions for easy access.
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)
(bind-key "C-c D m" #'mkdir)
(bind-key "C-x C-b" #'switch-to-buffer)
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x s" #'save-buffer)
(bind-key "M-F" #'find-file-at-point)
(bind-key "M-R" #'revert-buffer)
(unbind-key "C-x C-p")
(unbind-key "C-x C-n")

;; Open a shell in Emacs, or an external terminal emulator, in the
;; current directory.
(bind-key "C-c t" (lambda () (interactive)
                    (shell (concat "shell: " default-directory))))
(bind-key "C-c T" (lambda () (interactive)
                    (call-process (getenv "TERM_APP") nil 0 nil default-directory)))

;; Semantically-based prefix keymaps.
(bind-keys :prefix-map mz/format-keymap
           :prefix "C-c f"
           ("s" . sort-lines)
           ("u" . (lambda () (interactive)
                    (untabify (point-min) (point-max))))
           ("w" . whitespace-cleanup))
(bind-keys :prefix-map mz/input-mode-keymap
           :prefix "C-c I"
           ("c" . (lambda () (interactive)
                    (set-input-method "chinese-tonepy")))
           ("i" . (lambda () (interactive)
                    (set-input-method "ipa-praat")))
           ("l" . (lambda () (interactive)
                    (set-input-method "latin-postfix"))))

;; Misc personal and DWIM functions.
(defun mz/comment-dwim ()
  "Does what I mean.
If region, toggle region. If no region, toggle current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (forward-line))))

(bind-key "M-;" #'mz/comment-dwim)

(defun mz/kill-region-dwim ()
  "Do what I mean.
If region, kill region. If no region, kill current line."
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(bind-key "C-w" #'mz/kill-region-dwim)
(bind-key "C-S-w" #'mz/kill-region-dwim)

(defun mz/kill-ring-save-dwim ()
  "Do what I mean.
If region, save region. If no region, save current line."
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(bind-key "M-w" #'mz/kill-ring-save-dwim)

(defun mz/move-line-down ()
  "Move the current line down 1."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(bind-key "M-N" #'mz/move-line-down)

(defun mz/move-line-up ()
  "Move the current line up 1."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(bind-key "M-P" #'mz/move-line-up)

(defun mz/wiktionary-lookup ()
  "Look up a word in Wiktionary from `completing-read'."
  (interactive)
  (browse-url (concat "https://wiktionary.org/wiki/" (read-string "word: "))))

(bind-key "M-W" #'mz/wiktionary-lookup)

(defun mz/xkcd ()
  "Look up an XKCD comic number from `completing-read'."
  (interactive)
  (browse-url (concat "https://xkcd.com/" (read-string "comic: "))))

(defun mz/unfill-paragraph ()
  "Turn the multiline paragraph to one line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(bind-key "M-Q" #'mz/unfill-paragraph)

;; Bootstrap `elpaca' for external package management.
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Configure `use-package' for use with `elpaca'.
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

;; Packages already on load-path (inclluded wit Emacs or installed
;; with system package manager) do not need to be pulled from external
;; sources. Termed "built-in" in this configuration.
(defconst use-builtin-font-lock-keywords
  '(("(\\(use-builtin\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t)))
  "Identical font-locking to `use-package' for the NAME of the package.")

(font-lock-add-keywords 'emacs-lisp-mode use-builtin-font-lock-keywords)

(defmacro use-builtin (name &rest args)
  "Use `use-package' to configure NAME with ARGS."
  (declare (indent defun))
  `(use-package ,name
     ,@args
     :elpaca nil))

;; Package configurations using `use-package', both "built-in" and
;; external.

(use-builtin ada-mode
  :mode ("\\.adb\\'" "\\.ads\\'"))

(use-builtin agda2-mode
  :mode "\\.agda\\'")

(use-package all-the-icons-completion
  :init
  (use-package all-the-icons)
  :after marginalia
  :hook (emacs-startup-hook . all-the-icons-completion-mode))

(use-package apheleia
  :hook
  ((nix-mode-hook
   tuareg-mode-hook
   typescript-ts-base-mode-hook) . apheleia-mode))

(use-package atomic-chrome
  :demand t
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  :config
  (atomic-chrome-start-server))

(use-package avy
  :bind (("C-c J" . avy-goto-word-0)
         ("C-c K" . avy-goto-char-timer)
         ("C-c L" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.25)
  (avy-all-windows nil)
  (avy-orders-alist '((avy-goto-word-0 . avy-order-closest))))

(use-package company
  :hook (emacs-startup-hook . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-show-numbers t))

(use-package consult
  :bind
  (("C-S-s" . consult-line)
   ("C-x C-r" . consult-recent-file)
   ("M-i" . consult-imenu)
   ("M-s" . consult-ripgrep)
   ("M-y" . consult-yank-from-kill-ring)
   :map project-prefix-map
   ("s" . consult-ripgrep)))

(use-package clipetty
  :hook (emacs-startup-hook . global-clipetty-mode))

(use-package ctrlf
  :hook (emacs-startup-hook . ctrlf-mode)
  :custom
  (ctrlf-alternate-search-style 'regexp)
  (ctrlf-auto-recenter t)
  (ctrlf-default-search-style 'fuzzy))

(use-package dashboard
  :demand t
  :custom
  (dashboard-footer-messages '("On the cutting edge of cocking about"
                               "If I were a girl I'd be pregnant a lot"
                               "What could possibly go wrong?"
                               "I honestly believe that my genius, it generates gravity"
                               "Which seat do they put in as standard?"))
  (dashboard-init-info (lambda ()
                         (let ((time (emacs-init-time)))
                           (format "Emacs started in %s" time))))
  (dashboard-item-names '(("Projects:"     . "[p] Recent projects:")
                          ("Recent Files:" . "[r] Recent files:")))
  (dashboard-items '((projects . 5)
                     (recents  . 10)))
  (dashboard-projects-backend 'project-el)
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (initial-scratch-message "")
  :config
  (dashboard-setup-startup-hook))

(use-package default-text-scale
  :demand t
  :config
  (set-face-attribute 'default nil :height 140)
  (default-text-scale-mode +1))

(use-package direnv
  :hook (emacs-startup-hook . direnv-mode))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package ef-themes
  :demand t
  :init
  (defvar mz/ef-dark 'ef-dark "My preferred dark theme in `ef-themes'.")
  (defvar mz/ef-light 'ef-light "My prefered light theme in `ef-themes'.")
  :custom (ef-themes-to-toggle (list mz/ef-dark mz/ef-light))
  :bind (("C-S-t" . ef-themes-toggle))
  :config
  (load-theme mz/ef-dark))

(use-builtin eglot
  :hook
  (typescript-ts-base-mode-hook . eglot-ensure)
  (tuareg-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-base-mode)
                 "typescript-language-server" "--stdio")))

(use-builtin eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

(use-builtin elec-pair
  :hook ((prog-mode-hook org-mode-hook) . electric-pair-mode)
  :config
  ;; Do not automatically close <> in `org-mode'.
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-builtin erc
  :bind (("C-c u e" . erc))
  :custom
  (erc-autojoin-channels-alist
   '((Libera.chat "#emacs" "#nixos")))
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-nick "michzappa")
  (erc-port 6667)
  (erc-prompt-for-nickserv-password nil)
  (erc-prompt-for-password nil)
  (erc-server "irc.libera.chat")
  (erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))

(use-package exec-path-from-shell
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

(use-package expand-region
  :demand t
  :bind
  (("M-=" . (lambda () (interactive)
              (er/expand-region +1)
              (recenter)))
   ("M--" . (lambda () (interactive)
              (er/contract-region +1)
              (recenter))))
  :config
  ;; Due to conflicting binding at M-= in `org-mode'.
  (with-eval-after-load 'org
    (bind-key "C-c =" #'er/expand-region org-mode-map)))

(use-package git-gutter
  :hook (emacs-startup-hook . global-git-gutter-mode))

(use-package go-mode
  :mode "\\.go\\'")

(use-builtin gnus
  :bind (("C-c u g" . gnus))
  :custom
  (gnus-always-read-dribble-file t)
  (gnus-auto-select-first nil)
  (gnus-save-newsrc-file nil)
  (gnus-select-method
   '(nntp "news.gmane.io"))
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-date
     (not gnus-thread-sort-by-number)))
  (gnus-summary-display-arrow nil)
  (gnus-use-cache t))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode))

(use-package helpful
  :demand t

  :bind (("C-c C-h" . helpful-at-point)
         ("C-h F"   . helpful-function)
         ("C-h C"   . helpful-command))
  :config
  (advice-add 'describe-function :override #'helpful-callable)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-key      :override #'helpful-key))

(use-package hl-todo
  :hook ((prog-mode-hook org-mode-hook) . hl-todo-mode))

(use-package kbd-mode
  :elpaca '(kbd-mode :host github :repo "kmonad/kbd-mode")
  :mode "\\.kbd\\'")

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-builtin lean-mode
  :mode "\\.lean\\'")

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t))

(use-builtin lilypond-mode
  :mode ("\\.ly\\'" . LilyPond-mode))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              ("C-c c" . macrostep-collapse)
              ("C-c q" . macrostep-collapse-all)))

(use-package magit
  :after project
  :init
  ;; Use `magit' instead of `vc-mode' in project.el.
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (delete '(project-vc-dir "VC-Dir") project-switch-commands)
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-file-dispatch)
         :map project-prefix-map
         ("m" . magit-project-status)))

(use-package marginalia
  :hook (emacs-startup-hook . marginalia-mode))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package minions
  :hook (emacs-startup-hook . minions-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nixos-options
  :bind (("C-c N S" . mz/nixos-options))
  :config
  (defun mz/nixos-options ()
    "A NixOS option searcher based using `completing-read'."
    (interactive)
    (let* ((option-name (completing-read "NixOS Option: " nixos-options))
           (option-buffer (generate-new-buffer (concat "*nixos option: " option-name "*"))))
      (with-current-buffer option-buffer
        (insert
         (nixos-options-get-description
          (nixos-options-get-option-by-name option-name)))
        (read-only-mode)
        (view-mode)
        (setq-local view-exit-action 'kill-buffer))
      (pop-to-buffer option-buffer))))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package no-littering
  :demand t
  :init
  :config
  (startup-redirect-eln-cache (expand-file-name "eln-cache" no-littering-var-directory)))

(use-builtin ob-tangle
  :bind ("C-c C-v T" . org-babel-detangle))

(use-builtin org
  :demand t
  :custom
  (org-directory "~/org")
  (org-goto-interface 'outline-path-completion)
  (org-goto-max-level 10)
  (org-image-actual-width nil)
  (org-imenu-depth 10)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-support-shift-select t)
  (org-tags-column 0)
  :bind
  (("C-c o c" . org-capture)
   ("C-c o f" . mz/find-org-file)
   :map org-mode-map
   ("C-c C-l" . org-insert-link)
   ("C-c C-S-l" . org-store-link))
  :config
  (defun mz/find-org-file ()
    "Search for and open a file in `org-directory'."
    (interactive)
    (find-file
     (completing-read
      "Org File: "
      (directory-files-recursively org-directory ".org"))))

  (add-to-list 'org-link-frame-setup '(file . find-file)))

(use-package org-bullets
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '( "●" "◉" "○")))

(use-builtin ox-extra
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package org-drill
  :bind
  (:map org-mode-map (("C-c o d" . mz/org-drill-resume)))
  :custom
  (org-drill-save-buffers-after-drill-sessions-p nil)
  :config
  (defun mz/org-drill-resume ()
    "Resumes an org-drill session if one exists, otherwise starts anew."
    (interactive)
    (cond ((not (boundp 'org-drill-entries-pending-p)) (org-drill))
          ((org-drill-entries-pending-p org-drill-last-session) (org-drill-resume))
          (t (org-drill)))))

(use-builtin org-tempo
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("nn" . "src nix :noweb-ref")))

(use-builtin ox-latex
  :config
  (add-to-list 'org-latex-logfiles-extensions "bbl")
  (add-to-list 'org-latex-logfiles-extensions "tex"))

(use-builtin pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (use-package pdf-annot
    :commands (pdf-annot-minor-mode))
  (use-package pdf-history
    :commands (pdf-history-minor-mode))
  (use-package pdf-links
    :commands (pdf-links-minor-mode))
  (use-package pdf-occur
    :commands (pdf-occur-global-minor-mode))
  (use-package pdf-outline
    :commands (pdf-outline-minor-mode))
  (use-package pdf-sync
    :commands (pdf-sync-minor-mode))
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
  (add-hook 'pdf-view-mode-hook #'(lambda () (display-line-numbers-mode -1))))

(use-builtin project
  ;; TODO exclude the files (or more apt, anything in a submodule) in
  ;; user-emacs-directory/packages from project search?
  :config
  (defun mz/promote-project (dir)
    "If DIR has a '.project' file, treat it as a project."
    (let ((found-dir (locate-dominating-file dir ".project")))
      (if found-dir
          (cons 'transient found-dir)
        nil)))
  (setq project-find-functions '(mz/promote-project project-try-vc)))

(use-package puni
  :hook (prog-mode-hook . puni-mode)
  :bind
  (:map puni-mode-map
        ("C-c s <" . puni-wrap-angle)
        ("C-c s (" . puni-wrap-round)
        ("C-c s [" . puni-wrap-square)
        ("C-c s {" . puni-wrap-curly)
        ("C-c s u" . puni-splice)))

(use-builtin proof-general
  :mode ("\\.v\\'" . coq-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook web-mode-hook)

(use-builtin recentf
  :config
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (recentf-mode +1))

(use-builtin ruby-mode
  :mode "\\.rb\\'"
  :config
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?@ "w"))))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-builtin savehist
  :hook (emacs-startup-hook . savehist-mode))

(use-builtin saveplace
  :hook (emacs-startup-hook . save-place-mode))

(use-package scratch
  :bind (("C-c S" . scratch)))

(use-package simple-httpd)

(use-package sly)

(use-builtin tex-site
  :init
  (setq-default TeX-engine 'xetex)
  :custom
  (TeX-auto-save t)
  (TeX-PDF-mode t)
  (TeX-save-query nil))

(use-package tramp
  :elpaca nil
  :config
  (defun mz/sudo ()
    "Use TRAMP to `sudo' the current buffer."
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name))))

  (setq remote-file-name-inhibit-cache nil
        tramp-default-method "ssh"
        tramp-verbose 1))

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode))

(use-builtin typescript-ts-mode
  :mode ("\\.ts\\'" "\\.tsx"))

(use-package undo-tree
  :hook (emacs-startup-hook . global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(concat user-emacs-directory "var/undo-tree-history")))))

(use-package vertico
  :hook (emacs-startup-hook . vertico-mode))

(use-builtin vterm
  :demand t
  :after project
  :bind (("C-c T" . mz/vterm))
  :custom
  (vterm-max-scrollback 100000)
  :config
  (defun mz/vterm ()
    "Open `vterm' in another window, at project root if possible."
    (interactive)
    (let ((proj (project-current nil))
          (open-vterm (lambda () (interactive)
                        (vterm-other-window (concat "vterm: " default-directory)))))
      (if proj
          (with-temp-buffer
            (cd (project-root proj))
            (funcall open-vterm))
        (funcall open-vterm))))
  (defun mz/quit-vterm-window (_ _)
    "Wrapper around `quit-window' for a vterm exit hook."
    (quit-window))
  (defun mz/vterm-close-all-buffers ()
    "Close all vterm sessions."
    (interactive)
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'major-mode buffer) 'vterm-mode)
        (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
        (kill-buffer buffer))))
  (add-hook 'vterm-exit-functions 'mz/quit-vterm-window)
  (add-to-list 'project-switch-commands '(mz/vterm "Vterm") t)
  (bind-key "t" #'mz/vterm project-prefix-map))

(use-package web-mode
  :mode "\\.erb\\'"
  :custom
  (web-mode-auto-close-style 2)
  (web-mode-enable-auto-closing t)
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :hook (emacs-startup-hook . which-key-mode))

(use-builtin whitespace
  :hook (prog-mode-hook . whitespace-mode)
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face
                      tab-mark
                      empty
                      trailing
                      lines-tail)))

(use-builtin windmove
  :bind
  (("C-c j" . #'windmove-left)
   ("C-c l" . #'windmove-right)
   ("C-c i" . #'windmove-up)
   ("C-c k" . #'windmove-down)
   ("C-c <left>" . #'windmove-swap-states-left)
   ("C-c <right>" . #'windmove-swap-states-right)
   ("C-c <up>" . #'windmove-swap-states-up)
   ("C-c <down>" . #'windmove-swap-states-down)))

(use-package ws-butler
  :hook ((org-mode-hook prog-mode-hook) . ws-butler-mode)
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(provide 'init)
;;; init.el ends here.
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; eval: (flymake-mode +1)
;; End:
