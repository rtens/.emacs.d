;; Install and configure
(use-package neotree
	:ensure t
	:config
	(setq neo-theme 'ascii)
	(setq neo-window-width 40)
	(setq neo-window-fixed-size t)
	(setq neo-window-position 'right)
	(setq neo-autorefresh t)
	(global-set-key (kbd "C-c d") 'neotree-project-dir))

;; Use projectile project root
(defun neotree-project-dir ()
	"Open NeoTree using the project root."
	(interactive)
	(let ((project-dir (projectile-project-root))
				(file-name (buffer-file-name)))
		(neotree-toggle)
		(if project-dir
				(if (neo-global--window-exists-p)
						(progn
							(neotree-dir project-dir)
							(neotree-find file-name)))
			(message "Could not find project root."))))

;; Hide tree when open a file
(defun neo-open-file-hide (full-path &optional arg)
	"Open a file node and hides tree."
	(neo-global--select-mru-window arg)
	(find-file full-path)
	(neotree-hide))
(defun neotree-enter-hide (&optional arg)
	"Enters file and hides neotree directly"
	(interactive "P")
	(neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
(add-hook
 'neotree-mode-hook
 (lambda ()
	 (define-key neotree-mode-map (kbd "RET") 'neotree-enter-hide)))
