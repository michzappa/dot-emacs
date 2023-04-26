;;; mz-org --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Scratches on the surface of the venerable org-mode.
;; - Hugely underutilized org-roam.
;; - Tangling and exporting.

;;; Code:

(require 'mz-package-management)

(use-package org
  :demand t
  :commands (mz/find-org-file)
  :bind (("C-c o c" . #'org-capture)
         ("C-c o f" . #'mz/find-org-file)
         :map org-mode-map
         ("C-c C-l" . #'org-insert-link)
         ("C-c C-S-l" . #'org-store-link))
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
  :config
  (defun mz/find-org-file ()
    "Search for and open a file in `org-directory'."
    (interactive)
    (find-file
     (completing-read
      "Org File: "
      (directory-files-recursively org-directory ".org"))))

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

(provide 'mz-org)
;;; mz-org.el ends here
