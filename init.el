;; ========== basics ===============
(setq inhibit-splash-screen t)
(setq root-directory default-directory)
(winner-mode)
(electric-pair-mode)

;; ========== javascrpt ============
(setq js-indent-level 2)
;; (add-hook 'js-mode-hook #'display-line-numbers-mode)

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

;; ========== packages ============
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(use-package move-dup :ensure t)
(global-move-dup-mode)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(flycheck-add-mode 'javascript-eslint 'js-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package auto-complete :ensure t)
(global-auto-complete-mode t)

(use-package highlight-symbol
  :ensure t
  :config (add-hook 'js-mode-hook #'highlight-symbol-mode))

(use-package multiple-cursors :ensure t)
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c f") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-c b") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c a") 'mc/mark-all-symbols-like-this)

(use-package magit :ensure t)

(provide 'init)
;;; init.el ends here
