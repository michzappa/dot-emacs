;;; mz-dev-editing --- Michael Zappa's Emacs Configuration

;;; Commentary:

;; Developing programs or editing other text files.
;; - Interactive theorem provers.
;; - Eglot for LSP.
;; - Treesitter where applicable.

;;; Code:

(require 'mz-package-management)

(use-package apheleia
  :defines
  (apheleia-formatters)
  :hook
  (nix-mode-hook tuareg-mode-hook typescript-ts-base-mode-hook))

(use-package ws-butler
  :hook (org-mode-hook prog-mode-hook)
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t))

(use-package scratch
  :straight '(:type git :host nil :protocol "http" :repo "https://codeberg.org/emacs-weirdware/scratch")
  :functions (scratch)
  :bind (("C-c S" . #'scratch)))

(bind-keys :prefix-map mz/format-keymap
           :prefix "C-c f"
           ("s" . sort-lines)
           ("u" . (lambda () (interactive)
                    (untabify (point-min) (point-max))))
           ("w" . whitespace-cleanup))

(setq-default tab-width 4
              indent-tabs-mode nil
              sentence-end-double-space nil)

(use-package company
  :hook (after-init-hook . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-show-numbers t))

(use-feature agda2-mode
  :mode "\\.agda\\'")

(use-feature lean-mode
  :mode "\\.lean\\'")

(use-feature proof-general
  :mode ("\\.v\\'" . coq-mode))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              ("C-c q" . macrostep-collapse-all)))

(use-feature eglot
  :hook
  (typescript-ts-base-mode-hook . eglot-ensure)
  (tuareg-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-base-mode)
                 "typescript-language-server" "--stdio")))

(use-feature treesit)

(use-package sly)

(use-package go-mode)

(use-package haskell-mode
  :functions
  (interactive-haskell-mode
   haskell-doc-mode)
  :config
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode))

(use-package auctex
  :init
  (setq-default TeX-engine 'xetex)
  :custom
  (TeX-auto-save t)
  (TeX-PDF-mode t)
  (TeX-save-query nil))

(use-package kotlin-mode)

(use-package markdown-mode)

(use-package nix-mode)

(use-package nixos-options
  :defines
  (nixos-options)
  :functions
  (nixos-options-get-description
   nixos-options-get-option-by-name)
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

(use-package tuareg)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

(use-package racket-mode)

(use-feature ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?@ "w"))))

(use-package rust-mode)

(use-package yaml-mode)

(use-package simple-httpd)

(use-package web-mode
  :defines
  (web-mode-auto-close-style
   web-mode-enable-auto-closing
   web-mode-markup-indent-offset)
  :config
  (setq
   web-mode-auto-close-style 2
   web-mode-enable-auto-closing t
   web-mode-markup-indent-offset 2))

(use-package kbd-mode
  :straight '(:type git :host github :repo "kmonad/kbd-mode"))

(provide 'mz-dev-editing)
;;; mz-dev-editing.el ends here
