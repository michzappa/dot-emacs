;;; mz-terminal-shell --- Michael Zappa's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; - `vterm' is my preferred terminal emulator in Emacs.
;; - I use Emacs on machines with non-Posix shells.

;;; Code:

(require 'mz-package-management)

(use-package exec-path-from-shell
  :hook
  (elpaca-after-init-hook . exec-path-from-shell-initialize))

(use-package direnv
  :functions (direnv-mode)
  :config
  (direnv-mode +1))

(bind-key "C-c t" (lambda () (interactive)
                    (shell (concat "shell: " default-directory))))

(bind-key "C-c T" (lambda () (interactive)
                    (call-process (getenv "TERM_APP") nil 0 nil default-directory)))

(use-feature vterm
  :demand t
  :after project
  :defines (vterm-max-scrollback project-switch-commands)
  :functions (vterm-other-window mz/vterm project-root)
  :bind (("M-T" . mz/vterm))
  :custom
  (vterm-max-scrollback 100000)
  :config
  (defun mz/vterm ()
    "Open `vterm' in another window, at project root if possible."
    (interactive)
    (let ((proj (project-current nil))
          (open-vterm (lambda () (interactive)
                        (vterm-other-window (concat "vterm: " default-directory)))))
      (if proj
          (with-temp-buffer
            (cd (project-root proj))
            (funcall open-vterm))
        (funcall open-vterm))))
  (defun mz/quit-vterm-window (_ _)
    "Wrapper around `quit-window' for a vterm exit hook."
    (quit-window))
  (defun mz/vterm-close-all-buffers ()
    "Close all vterm sessions."
    (interactive)
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'major-mode buffer) 'vterm-mode)
        (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
        (kill-buffer buffer))))
  (add-hook 'vterm-exit-functions 'mz/quit-vterm-window)
  ;; FIXME: this opens Vterm in the current project, not the one being
  ;; switched to
  (add-to-list 'project-switch-commands '(mz/vterm "Vterm") t)
  (bind-key "t" #'mz/vterm project-prefix-map))

(provide 'mz-terminal)
;;; mz-terminal.el ends here
