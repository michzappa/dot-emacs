;;; mz-project-vc --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; I use project.el to manage my projects. Version control is usually
;; with Git, so Magit is the obvious choice. I have Forge installed
;; but haven't really found where it fits in my workflow.

;;; Code:

(require 'mz-package-management)

(defun mz/promote-project (dir)
  "If DIR has a '.project' file, treat it as a project."
  (let ((found-dir (locate-dominating-file dir ".project")))
    (if found-dir
        (cons 'transient found-dir)
      nil)))

(use-feature project
  :config
  (setq project-find-functions '(mz/promote-project project-try-vc)))

(use-package magit
  :after project
  :init
  ;; Use Magit instead of vc-mode in project.el
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (delete '(project-vc-dir "VC-Dir") project-switch-commands)
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-file-dispatch)
         :map project-prefix-map
         ("m" . magit-project-status)))

(use-package forge
  :after magit)

(provide 'mz-project-vc)
;;; mz-project-vc.el ends here
