;; Install the package
(use-package yasnippet
	:ensure t
	:config (yas-global-mode 1))

;; Load project snippets
(defun my-load-project-snippets ()
	"Load snippets from project directory"
	(let ((dir (expand-file-name ".emacs/snippets" (projectile-project-root))))
		(when (file-exists-p dir)
			(yas-load-directory dir)
			(message "Project snippets loaded"))))
(add-hook 'projectile-after-switch-project-hook #'my-load-project-snippets)
(defun my-delayed-load-project-snippets ()
	(run-with-timer 1 nil 'my-load-project-snippets))
(add-hook 'desktop-after-read-hook 'my-delayed-load-project-snippets)
