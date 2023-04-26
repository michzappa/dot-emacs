;;; mz-package-management --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; - I use `use-package' to load and configure packages, both internal
;; and external.
;; - I generally use `straight' to install external packages.
;; - I use Nix to bundle some packages with Emacs directly,
;; typically these depend on external programs.

;;; Code:

(setq use-package-always-defer t
      use-package-hook-name-suffix "")

;; TODO make macro for custom straight.el recipes

(defmacro use-package-dependency (name &rest body)
  "Like `use-package', but `:demand'ed.
For loading dependencies in `:config'. NAME and BODY are as in
`use-package'."
  (declare (indent defun))
  `(use-package ,name
     :demand t
     ,@body))

(defmacro use-feature (name &rest body)
  "Like `use-package', but without straight.el integration.
For loading packages bundled with Emacs. NAME and BODY are as in
`use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@body))

(defmacro use-feature-dependency (name &rest body)
  "Like `use-feature', but `:demand'ed.
For loading dependencies in `:config'. NAME and BODY are as in
`use-package'."
  (declare (indent defun))
  `(use-feature ,name
     :demand t
     ,@body))

(setq straight-build-dir (concat "build_" emacs-version)
      straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Don't use the versions of these packages which come with Emacs,
;; install them from `straight'. This prevents version conflicts.
(straight-register-package 'org)
(straight-register-package 'org-contrib)

(when (not (require 'use-package nil t))
  (straight-use-package 'use-package))

(use-package no-littering
  :demand t)

(provide 'mz-package-management)
;;; mz-package-management.el ends here
