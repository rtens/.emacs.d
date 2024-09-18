;; Author information
(setq user-full-name "Nikolas Martens")
(setq user-mail-address "Nikolas.M@rtens.org")

;; Function for loading other init files
(defun setup (file)
  "Import a file from the emacs directory."
  (load-file (expand-file-name
	      (concat "setup-" file ".el")
	      user-emacs-directory)))

(setup "archives")
(setup "behavior")
(setup "user-interface")
(setup "editor")
(setup "appearance")
