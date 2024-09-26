;; Minimal frame title
(setq frame-title-format '("emacs"))

;; Change exit key
(global-set-key (kbd "C-x q") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c"))

;; Change help key
(global-set-key (kbd "C-x h") help-map)

;; Change macro keys
(global-set-key (kbd "C-x m") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c m") 'kmacro-end-or-call-macro)

;; Key binding for ibuffer
(global-set-key (kbd "C-c b") 'ibuffer)

;; Key binding for saving files
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-c s") 'save-some-buffers)

;; Don't beep, flash instead
(setq ring-bell-function (lambda ()
	 (invert-face 'mode-line)
	 (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Better tab-completion
(use-package flx-ido
	:ensure t
	:defer 2
	:bind (("M-j" . icomplete-fido-exit))
	:config
	(ido-mode 1)
	(fido-mode t)
	(ido-everywhere 1)
	(flx-ido-mode 1)

	;; disable ido faces to see flx highlights.
	(setq ido-enable-flex-matching t)
	(setq ido-use-faces nil))

;; Make URLs clickable (and open with `C-c RET`)
(global-goto-address-mode t)

;; Echo unfinished commands immediately
(setq echo-keystrokes 0.02)

;; Show folder for files with same names
(setq uniquify-buffer-name-style 'forward)

;; Auto-complete
(use-package auto-complete
	:ensure t
	:defer 2
	:config (global-auto-complete-mode t))

;; Cleaner mode line
(use-package mood-line
	:ensure t
	:defer 1
	:config (mood-line-mode))

;; Help with key bindings
(use-package which-key
	:ensure t
	:defer 3
	:config (which-key-mode))

;; Makes sure windows are balanced and enlarged if needed
(use-package zoom
	:ensure t
	:defer 3
	:config (zoom-mode t))
