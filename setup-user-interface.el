;; Don't beep, flash instead
(setq ring-bell-function (lambda ()
	 (invert-face 'mode-line)
	 (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Better tab-completion
(ido-mode t)
(fido-mode t)

;; Show folder for files with same names
(setq uniquify-buffer-name-style 'forward)

;; Auto-complete
(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t))
