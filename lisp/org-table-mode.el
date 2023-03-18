;;; org-table-mode --- Minor Mode for Org Tables -*- lexical-binding: t; -*-

;;; Commentary:

;; When working with org mode tables I wanted a more close-to-hand way
;; to copy around the contents of cells.

;;; Code:

(require 'org-table)

(define-minor-mode mz/org-table
  "Minor mode for helpful keybindings to work with `org-mode' tables."
  :init-value
  nil
  :lighter
  "mz/org-table"
  :keymap
  '())

(defun mz/org-table-select-cell ()
  "Select the table cell under point."
  (when (not (looking-back "|[[:blank:]]?" nil))
    (org-table-beginning-of-field 1))
  (set-mark-command nil)
  (org-table-end-of-field 1))

(defun mz/org-table-save-cell ()
  "Save the table field under point."
  (interactive)
  (mz/org-table-select-cell)
  (copy-region-as-kill 0 0 t)
  (org-table-align))

(defun mz/org-table-kill-cell ()
  "Kill the table field under point."
  (interactive)
  (mz/org-table-select-cell)
  (kill-region 0 0 t)
  (org-table-align))

(bind-key "M-S-SPC" #'mz/org-table-kill-cell mz/org-table-map)
(bind-key "S-SPC" #'mz/org-table-save-cell mz/org-table-map)

(provide 'org-table-mode)
;;; org-table-mode.el ends here
