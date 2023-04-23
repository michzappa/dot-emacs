;;; snake-mode --- Major mode for the snake languages

;;; Commentary:

;; TODO Goals:
;; - Indentation
;; - Imenu support for def and let(rec)

;;; Code:

(defconst snake-constants '("true" "false"))
(defconst snake-keywords '("let" "lambda" "rec" "in" "if" "else" "def" "begin" "end"))
(defconst snake-runtime-functions '("print" "input" "equal" "printStack"))

(defconst snake-constants-regexp (regexp-opt snake-constants 'words))
(defconst snake-keywords-regexp (regexp-opt snake-keywords 'words))
(defconst snake-runtime-functions-regexp (regexp-opt snake-runtime-functions 'words))

(defconst snake-font-lock-keywords
      `((,snake-constants-regexp . font-lock-constant-face)
        (,snake-keywords-regexp . font-lock-keyword-face)
        (,snake-runtime-functions-regexp . font-lock-function-name-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adder\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.boa\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.cobra\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.diamond\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.egg\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.fdl\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.garter\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.racer\\'" . snake-mode))
(add-to-list 'auto-mode-alist '("\\.snake\\'" . snake-mode))

;;;###autoload
(define-derived-mode snake-mode fundamental-mode
  "snake mode"
  "Major mode for snake languages."
  (setq font-lock-defaults '((snake-font-lock-keywords)))
  (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face)))
  (setq comment-start "# ")
  (setq comment-end ""))

(provide 'snake-mode)
;;; snake-mode.el ends here
