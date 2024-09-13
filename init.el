;; ========== basics ===============
(setq inhibit-splash-screen t)
(setq root-directory default-directory)

;; ======== desktop mode ===========
(desktop-save-mode 1)
(setq desktop-path '("."))

;; ======== auto saves =============
(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;; ======== backup files ==========
(let ((my-backup-dir (locate-user-emacs-file "backups")))
  (setq backup-directory-alist `(("." . ,my-backup-dir)))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir)))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ===== customization files ======
(setq custom-file "~/.emacs.d/custom-local.el")

;; ====== init files cascade ======
(dolist (init-file (list "~/.emacs.d/custom.el"
			 "~/.emacs.d/custom-local.el"
			 "~/.emacs.d/init-local.el"
			 "./.emacs-project.el"
			 "./.emacs-local.el"))
  (if (file-exists-p init-file)
      (load-file init-file)))
