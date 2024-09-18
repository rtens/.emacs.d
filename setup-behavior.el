;; Re-open buffers
(desktop-save-mode 1)
(setq desktop-restore-eager 2)

;; Split window to the right by default
(setq split-width-threshold 100)

;; Answer yes/no question with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Keep buffers up-to-date
(global-auto-revert-mode 1)

;; Don't crate lock files (eg .#foo#)
(setq create-lockfiles nil)

;; Keep auto-save files in central folder
(let ((my-auto-save-dir (expand-file-name "auto-save" user-emacs-directory)))
  (setq auto-save-file-name-transforms
  `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;; Keep backup files in central folder
(let ((my-backup-dir (expand-file-name "backups" user-emacs-directory)))
  (setq backup-directory-alist `(("." . ,my-backup-dir)))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir)))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
