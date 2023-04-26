;;; mz-tuning --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Adjustments to Emacs in order to give better performance -
;; including, but not limited to:
;; - Compilation of Emacs Lisp (native, byte-code, etc.)
;; - Garbage Collection

;;; Code:

(require 'comp)
(require 'mz-package-management)

(setq gc-cons-threshold (* 100 1024 1024)
      native-comp-async-report-warnings-errors nil)

(use-package auto-compile
  :hook (after-init-hook . auto-compile-on-load-mode))

(provide 'mz-tuning)
;;; mz-tuning.el ends here
