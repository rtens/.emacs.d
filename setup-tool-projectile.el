;; Install and configure projectile
(use-package projectile
	:ensure t
	:defer 2
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map))
	:config
	(projectile-mode +1)

	;; Add project name to frame title
	(setq frame-title-format '("emacs [" (:eval (projectile-project-name)) "]"))

	;; Add project name to mode line
	(defun mood-line-segment-buffer-name ()
		"Displays the name of the project and current buffer in the mode-line."
		(format "[%s] %s" (projectile-project-name) (buffer-name))))

;; Function to load proiect init files
(defun my-load-project-inits ()
	"Load .emacs-project(-user).el if existing when entering a project"
	(message (concat "Loading project: " (projectile-project-root)))
	(dolist (init-file (list
											".emacs/init.el"
											".emacs/init-user.el"))
		(let ((file (expand-file-name
								 init-file (projectile-project-root))))
			(if (file-exists-p file) (load-file file)))))

;; Load project init files after a short delay to make sure project root is available
(defun my-delayed-project-inits ()
	(run-with-timer 1 nil 'my-load-project-inits))
(add-hook 'desktop-after-read-hook 'my-delayed-project-inits)

;; Load project init files every time the project is switched to
(add-hook 'projectile-after-switch-project-hook #'my-load-project-inits)
