;;; mz-enhancements --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Modifications to Emacs' basic functionality which create my desired
;; user experience, while minimizing interactive differences with
;; vanilla Emacs (use as many of the same keybinds, which more
;; powerful functions behind them).

;;; Code:

(require 'mz-package-management)
(require 'compile)

;; Always open in a maximized frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'server-after-make-frame-hook
          #'(lambda () (let ((frame (selected-frame)))
                    (select-frame-set-input-focus frame)
                    (set-frame-parameter frame 'fullscreen 'maximized))))

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode +1)
(global-auto-revert-mode +1)
(global-subword-mode +1)

(add-hook 'compilation-filter-hook
          #'(lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(setq auto-save-default nil
      compilation-scroll-output t
      confirm-kill-processes nil
      create-lockfiles nil
      delete-by-moving-to-trash t
      enable-recursive-minibuffers t
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t
      make-backup-files nil
      mode-require-final-newline 'visit-save
      ring-bell-function #'ignore
      save-interprogram-paste-before-kill t
      split-height-threshold nil
      vc-follow-symlinks t)

(bind-key "C-c D m" #'mkdir)
(bind-key "C-c d" #'dired-jump)
(bind-key "C-x C-b" #'switch-to-buffer)
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x s" #'save-buffer)
(bind-key "M-F" #'find-file-at-point)
(bind-key "M-R" #'revert-buffer)
(unbind-key "C-x C-p")
(unbind-key "C-x C-n")

(use-feature windmove
  :bind (("C-c j" . #'windmove-left)
         ("C-c l" . #'windmove-right)
         ("C-c i" . #'windmove-up)
         ("C-c k" . #'windmove-down)
         ("C-c <left>" .#'windmove-swap-states-left)
         ("C-c <right>" .#'windmove-swap-states-right)
         ("C-c <up>" .#'windmove-swap-states-up)
         ("C-c <down>" .#'windmove-swap-states-down)))

(defun mz/move-line-down ()
  "Move the current line down 1."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun mz/move-line-up ()
  "Move the current line up 1."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(bind-key "M-N" #'mz/move-line-down)
(bind-key "M-P" #'mz/move-line-up)

(defun mz/unfill-paragraph ()
  "Turn the multiline paragraph to one line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(bind-key "M-Q" #'mz/unfill-paragraph)

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

(use-package clipetty
  :hook (after-init-hook . global-clipetty-mode))

(setq mouse-wheel-progressive-speed nil
      scroll-conservatively 100000
      scroll-margin 0
      scroll-preserve-screen-position t
      scroll-step 1)

(unless (display-graphic-p)
  (xterm-mouse-mode t)
  (bind-key [mouse-4] (lambda () (interactive)
                        (scroll-down 1)))
  (bind-key [mouse-5] (lambda () (interactive)
                        (scroll-up 1))))

(use-package ctrlf
  :hook (after-init-hook . ctrlf-mode)
  :custom
  (ctrlf-alternate-search-style 'regexp)
  (ctrlf-auto-recenter t)
  (ctrlf-default-search-style 'fuzzy))

(use-package undo-tree
  :hook (after-init-hook . global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(concat user-emacs-directory "var/undo-tree-history")))))

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

(use-feature recentf
  :config
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (recentf-mode +1))

(use-package dumb-jump
  :hook xref-backend-functions)

(use-feature saveplace
  :hook
  (after-init-hook . save-place-mode))

(use-package avy
  :functions
  (avy-goto-word-0
   avy-goto-char-timer
   avy-goto-line)
  :bind (("C-c J" . #'avy-goto-word-0)
         ("C-c K" . #'avy-goto-char-timer)
         ("C-c L" . #'avy-goto-line))
  :custom
  (avy-timeout-seconds 0.25)
  (avy-all-windows nil)
  (avy-orders-alist '((avy-goto-word-0 . avy-order-closest))))

(use-package multiple-cursors
  :functions
  (mc/mark-next-like-this
   mc/mark-previous-like-this)
  :bind (("C->" . #'mc/mark-next-like-this)
         ("C-<" . #'mc/mark-previous-like-this)))

(use-package puni
  :defines
  (puni-mode-map)
  :hook (prog-mode-hook . puni-mode)
  :bind (:map puni-mode-map
              ("C-c s <" . puni-wrap-angle)
              ("C-c s (" . puni-wrap-round)
              ("C-c s [" . puni-wrap-square)
              ("C-c s {" . puni-wrap-curly)
              ("C-c s u" . puni-splice)))

(electric-pair-mode +1)
(add-hook
 'org-mode-hook
 (lambda ()
   (setq-local electric-pair-inhibit-predicate
               `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package expand-region
  :functions
  (er/expand-region
   er/contract-region)
  :bind
  (("M-=" . (lambda () (interactive)
              (er/expand-region +1)
              (recenter)))
   ("M--" . (lambda () (interactive)
              (er/contract-region +1)
              (recenter))))
  :config
  (with-eval-after-load 'org
    (bind-key "C-c =" #'er/expand-region org-mode-map)))

(use-package marginalia
  :hook (after-init-hook . marginalia-mode))

(use-package vertico
  :hook (after-init-hook . vertico-mode))

(use-package savehist
  :hook (after-init-hook . savehist-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :functions
  (consult-line
   consult-recent-file
   consult-imenu
   consult-ripgrep
   consult-yank-from-kill-ring)
  :bind
  (("C-S-s" . #'consult-line)
   ("C-x C-r" . #'consult-recent-file)
   ("M-i" . #'consult-imenu)
   ("M-s" . #'consult-ripgrep)
   ("M-y" . #'consult-yank-from-kill-ring)
   :map project-prefix-map
   ("s" . #'consult-ripgrep)))

(provide 'mz-enhancements)
;;; mz-enhancements.el ends here
