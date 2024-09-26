;; Author information
(setq user-full-name "Nikolas Martens")
(setq user-mail-address "Nikolas.M@rtens.org")

;; Function for loading other init files
(defun setup (file)
	"Import a file from the emacs directory."
	(load-file (expand-file-name
							(concat "setup/" file ".el")
							user-emacs-directory)))

;; Turn off the lights
(set-background-color "black")

;; Set and load customization file
(setq custom-file (expand-file-name "custom-user.el" user-emacs-directory))
(if (file-exists-p custom-file) (load custom-file))

;; Load all init files
(setup "archives")
(setup "behavior")
(setup "user-interface")
(setup "my-mode")
(setup "editor")
(setup "navigation")

(setup "tool-magit")
(setup "tool-neotree")
(setup "tool-projectile")
(setup "tool-yasnippet")

(setup "lang-js")

(setup "appearance")

;; Load system-specific init files
(if (eq system-type 'darwin)
		(setup "system-mac"))
(if (eq system-type 'windows-nt)
		(setup "system-windows"))

;; Load user init files if existing
(let ((init-user (expand-file-name "init-user.el" user-emacs-directory)))
	(if (file-exists-p init-user)
			(load-file init-user)))

;; Load custom file again to overwrite other settings
(if (file-exists-p custom-file) (load custom-file))
