;; Jump to chars
(use-package avy
  :ensure t
  :config (global-set-key (kbd "C-a") 'avy-goto-char-2))

;; Goto line with help
(global-set-key (kbd "C-b") 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
  (display-line-numbers-mode 1)
  (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

;; Folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(my-key "hide-all" "C-v C-o" '(hs-hide-all))
(my-key "show-all" "C-v C-ü" '(hs-show-all))
(my-key "hide-block" "C-v C-p" '(hs-hide-block))
(my-key "show-block" "C-v C-ö" '(hs-show-block))
(my-key "hide-level" "C-v C-l" '(hs-hide-block))
(my-key "show-next-level" "C-v C-ä" '(hs-show-block) '(hs-hide-level 0))
