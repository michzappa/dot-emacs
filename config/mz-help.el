;;; mz-help --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages to help out the Emacs user.

;;; Code:

(require 'mz-package-management)

(use-package helpful
  :demand t
  :functions (helpful-at-point
              helpful-function
              helpful-command
              helpful-callable
              helpful-variable
              helpful-key)
  :bind (("C-c C-h" . #'helpful-at-point)
         ("C-h F"   . #'helpful-function)
         ("C-h C"   . #'helpful-command))
  :config
  (advice-add 'describe-function :override #'helpful-callable)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-key      :override #'helpful-key))

(use-package which-key
  :hook (elpaca-after-init-hook . which-key-mode))

(provide 'mz-help)
;;; mz-help.el ends here
