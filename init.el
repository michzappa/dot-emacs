;;; init --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; This generation of my Emacs configuration is designed with the
;; intent of understanding Emacs better to reflect my multi-year
;; usage.

;; The foundation of my methodology to do so is over-using Emacs Lisp
;; - macros, packaging, etc - in order to become more familiar with
;; the fundamental features that make Emacs so powerful when it is
;; used with purpose.

;;; Code:

(defmacro require-directory (dir)
  "Load every Emacs Lisp file in the given DIR (not recursive).
DIR is relative to `user-emacs-directory'."
  `(let ((dir (expand-file-name ,dir user-emacs-directory)))
     (add-to-list 'load-path dir)
     (dolist (file (directory-files dir nil "\\.el$"))
       (require (intern (file-name-sans-extension file))))))

;; Configurations for and additions to [packages, which I do not maintain].
(require-directory "config")

;; [Packages, which I maintain].
(require-directory "lisp")

;; Don't litter init.el with customizations.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Truly personal miscellaneous configuration.
(setq user-full-name "Michael Zappa"
      user-mail-address "me@michzappa.com")

(defun mz/wiktionary-lookup ()
  "Look up a word in Wiktionary from `completing-read'."
  (interactive)
  (browse-url (concat "https://wiktionary.org/wiki/" (read-string "word: "))))

(bind-key "M-W" #'mz/wiktionary-lookup)

(bind-keys :prefix-map mz/input-mode-keymap
           :prefix "C-c I"
           ("c" . (lambda () (interactive)
                    (set-input-method "chinese-tonepy")))
           ("i" . (lambda () (interactive)
                    (set-input-method "ipa-praat")))
           ("l" . (lambda () (interactive)
                    (set-input-method "latin-postfix"))))

(use-package atomic-chrome
  :demand t
  :functions
  (atomic-chrome-start-server)
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  :config
  (atomic-chrome-start-server))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t))

(provide 'init)
;;; init.el ends here
