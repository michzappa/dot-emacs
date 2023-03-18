;;; mz-org --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Scratches on the surface of the venerable org-mode.
;; - Hugely underutilized org-roam.
;; - Tangling and exporting.

;;; Code:

(require 'mz-package-management)

(use-package org
  :bind (("C-c o c" . #'org-capture)
         :map org-mode-map
         ("C-c C-l" . #'org-insert-link)
         ("C-c C-S-l" . #'org-store-link))
  :custom
  (org-directory "~/org")
  (org-image-actual-width nil)
  (org-imenu-depth 10)
  (org-refile-targets nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-support-shift-select t)
  (org-tags-column 0)
  :config
  (add-to-list 'org-link-frame-setup '(file . find-file))

  (use-feature-dependency ob-tangle
    :functions
    (org-babel-detangle)
    :bind ("C-c C-v T" . #'org-babel-detangle))

  (use-feature-dependency org-capture)

  (use-package-dependency org-drill
    :after org-capture
    :functions
    (org-drill
     org-drill-resume
     org-drill-entries-pending-p)
    :defines
    (org-drill-last-session)
    :bind (:map org-mode-map (("C-c o d" . mz/org-drill-resume)))
    :custom
    (org-drill-save-buffers-after-drill-sessions-p nil)
    :config
    (add-to-list 'org-capture-templates
     `("f" "French Item" entry
        (file ,(expand-file-name "french.org" org-directory))
        "* %^{Part of Speech|Nom|Adjectif|Verbe|Adverbes|Conjunction|Préposition|Pronom|Expression|Rugby} :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n** French\n%^{French}\n** English\n%^{English}"))
    (defun mz/org-drill-resume ()
      "Resumes an org-drill session if one exists, otherwise starts anew."
      (interactive)
      (cond ((not (boundp 'org-drill-entries-pending-p)) (org-drill))
            ((org-drill-entries-pending-p org-drill-last-session) (org-drill-resume))
            (t (org-drill)))))

  (use-package-dependency org-contrib
    :config
    (use-feature-dependency ox-extra
      :functions
      (ox-extras-activate)
      :config
      (ox-extras-activate '(ignore-headlines))))

    (use-feature-dependency org-tempo
    :config
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("nn" . "src nix :noweb-ref")))

  (use-feature-dependency ox-latex
    :config
    (add-to-list 'org-latex-logfiles-extensions "bbl")
    (add-to-list 'org-latex-logfiles-extensions "tex"))

  (use-package-dependency ox-hugo
    :after (ox org-roam)
    :config
    (remove-hook 'before-save-hook 'org-encrypt-entries t))

  (use-package-dependency org-bullets
    :hook org-mode-hook))

(use-package org-roam
  :defines
  (org-roam-db-location
   org-roam-directory)
  :functions
  (org-roam-setup
   org-roam-db-autosync-mode)
  :bind (("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-node-find)
         :map org-mode-map
         (("C-c n o" . org-id-get-create)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n E" . org-roam-extract-subtree)))
  :custom
  (org-roam-capture-templates '())
  (org-roam-completion-everywhere t)
  (org-roam-directory org-directory)
  (org-roam-extract-new-file-path "${slug}.org")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:30}" 'face 'org-tag)))
  ;; Exclude every org-drill vocab entry and files which have been
  ;; copied to the nix store.
  (org-roam-db-node-include-function
   #'(lambda () (and (not (file-in-directory-p (buffer-file-name) "/nix/store/"))
                (not (member "drill" (org-get-tags))))))
  :config
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (use-package org-roam-ui
    :functions
    (org-roam-ui-mode)
    :bind (:map org-mode-map (("C-c n V" . (lambda () (interactive)
                                             (org-roam-ui-mode +1)))))))
(provide 'mz-org)
;;; mz-org.el ends here
