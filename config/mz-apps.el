;;; mz-apps --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs packages which more closely resemble proper applications.

;;; Code:

(require 'mz-package-management)

;; TODO this should not be necessary
(defvar native-comp-deferred-compilation-deny-list nil)

(use-package elfeed
  :defines
  (elfeed-search-filter
   elfeed-search-print-entry-function
   elfeed-search-title-max-width
   elfeed-search-title-min-width
   elfeed-search-trailing-width
   elfeed-sort-order
   elfeed-search-mode-map)
  :functions
  (elfeed-clamp
   elfeed-db-get-all-tags
   elfeed-entry-date
   elfeed-entry-feed
   elfeed-entry-tags
   elfeed-entry-title
   elfeed-feed-title
   elfeed-format-column
   elfeed-make-tagger
   elfeed-meta
   elfeed-search--faces
   elfeed-search-format-date
   elfeed-search-update--force
   elfeed-update)
  :init
  (defvar mz/base-elfeed-search-filter "@3-days-ago +unread")
  (defvar mz/home-elfeed-search-filter (concat mz/base-elfeed-search-filter " +home"))
  (setq browse-url-browser-function #'browse-url-firefox)
  (run-with-timer 0 (* 4 60 60) 'elfeed-update) ; Update every 4 hours
  :bind (("C-c u f" . elfeed))
  :custom
  (elfeed-search-filter mz/home-elfeed-search-filter)
  (elfeed-search-title-max-width 80)
  (elfeed-sort-order 'ascending)
  :config
  (use-package-dependency elfeed-org
    :defines (rmh-elfeed-org-files)
    :functions (elfeed-org)
    :config
    (setq rmh-elfeed-org-files
          `(,(expand-file-name "elfeed.org" org-directory)))
    (elfeed-org))

  (defun mz/elfeed-filter-tags (reset exclude)
    "Prompt the user for tags known to elfeed. If EXCLUDE, hide all
  entries with any of those tags. Else, show only entries with all
  those tags. If RESET, only the generated filter will be in
  action. If not, it will be added to the existing filter."
    (interactive)
    (let* ((tags (completing-read-multiple "Tags: " (elfeed-db-get-all-tags)))
           (filter (string-join (mapcar (lambda (tag) (format "%s%s" (if exclude "-" "+") tag))
                                        tags)
                                " ")))
      (setq elfeed-search-filter (format "%s %s"
                                         (if reset mz/base-elfeed-search-filter elfeed-search-filter)
                                         filter)))
    (elfeed-search-update--force))

  (defun mz/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer, no tags."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))))

  (defun mz/elfeed-toggle-sort-direction ()
    "Toggle the `elfeed-sort-order' between ascending and descending
  chronologically."
    (interactive)
    (setq elfeed-sort-order
          (if (eq elfeed-sort-order 'ascending) 'descending 'ascending))
    (elfeed-search-update--force))

  (bind-keys :map elfeed-search-mode-map
             ("e" . (lambda () (interactive)
                      (mz/elfeed-filter-tags nil t)))
             ("i" . (lambda () (interactive)
                      (mz/elfeed-filter-tags nil nil)))
             ("l" . recenter-top-bottom)
             ("E" . (lambda () (interactive)
                      (mz/elfeed-filter-tags t t)))
             ("H" . (lambda () (interactive)
                      (setq elfeed-search-filter mz/home-elfeed-search-filter)
                      (elfeed-search-update--force)))
             ("I" . (lambda () (interactive)
                      (mz/elfeed-filter-tags t nil)))
             ("R" . (lambda () (interactive)
                      (setq elfeed-search-filter mz/base-elfeed-search-filter)
                      (elfeed-search-update--force)))
             ("T" . mz/elfeed-toggle-sort-direction)
             ("U" . elfeed-update))

  (setq elfeed-search-print-entry-function 'mz/elfeed-search-print-entry)

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "reddit\\.com"
                                :add '(reddit)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(youtube))))

(use-feature erc
  :bind (("C-c u e" . erc))
  :custom
  (erc-autojoin-channels-alist
   '((Libera.chat "#emacs" "#nixos")))
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-nick "michzappa")
  (erc-port 6667)
  (erc-prompt-for-nickserv-password nil)
  (erc-prompt-for-password nil)
  (erc-server "irc.libera.chat")
  (erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))

(use-feature gnus
  :defines
  (gnus-always-read-dribble-file
   gnus-auto-select-first
   gnus-save-newsrc-file
   gnus-thread-sort-functions
   gnus-summary-display-arrow)
  :bind (("C-c u g" . gnus))
  :config
  (setq
   gnus-always-read-dribble-file t
   gnus-auto-select-first nil
   gnus-save-newsrc-file nil
   gnus-select-method
   '(nntp "news.gmane.io")
   gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-date
     (not gnus-thread-sort-by-number))
   gnus-summary-display-arrow nil
   gnus-use-cache t))

(use-feature tramp
  :config
  (defun mz/sudo ()
    "Use TRAMP to `sudo' the current buffer."
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name))))

  (setq remote-file-name-inhibit-cache nil
        tramp-default-method "ssh"
        tramp-verbose 1))

(use-feature pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :functions
  (pdf-tools-install
   pdf-view-midnight-minor-mode)
  :config
  (use-feature-dependency pdf-annot
    :commands (pdf-annot-minor-mode))
  (use-feature-dependency pdf-history
    :commands (pdf-history-minor-mode))
  (use-feature-dependency pdf-links
    :commands (pdf-links-minor-mode))
  (use-feature-dependency pdf-occur
    :commands (pdf-occur-global-minor-mode))
  (use-feature-dependency pdf-outline
    :commands (pdf-outline-minor-mode))
  (use-feature-dependency pdf-sync
    :commands (pdf-sync-minor-mode))
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))

(provide 'mz-apps)
;;; mz-apps.el ends here
