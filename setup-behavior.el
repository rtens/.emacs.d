;; Re-open buffers
(desktop-save-mode 1)
(setq desktop-restore-eager 2)

;; Split window to the right by default
(setq split-width-threshold 120)

;; Answer yes/no question with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Keep buffers up-to-date
(global-auto-revert-mode 1)

;; Keep working directory clean
(setq create-lockfiles nil)
