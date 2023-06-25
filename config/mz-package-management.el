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
  "Like `use-package', but without `elpaca' integration.
For loading packages built-into Emacs. NAME and BODY are as in
`use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@body))

(defmacro use-feature-dependency (name &rest body)
  "Like `use-feature', but `:demand'ed.
For loading dependencies in `:config'. NAME and BODY are as in
`use-package'."
  (declare (indent defun))
  `(use-feature ,name
     :demand t
     ,@body))

;; Bootstrap third-party package-manager.
(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
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
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; `use-package' support for `elpaca'.
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Don't use the versions of these packages which come with Emacs,
;; install them with `elpaca'. This prevents version conflicts.
(elpaca org)
(elpaca org-contrib)

(when (not (require 'use-package nil t))
  (elpaca 'use-package)
  (elpaca-wait))

(use-package no-littering
  :demand t)

(provide 'mz-package-management)
;;; mz-package-management.el ends here
