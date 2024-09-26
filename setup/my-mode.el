;; Minor mode to collect own key bindings

;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar my-mode-map (make-sparse-keymap)
	"Keymap for `my-mode'.")

;;;###autoload
(define-minor-mode my-mode
	"A minor mode so that my key settings override annoying major modes."
	;; If init-value is not set to t, this mode does not get enabled in
	;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
	;; More info: http://emacs.stackexchange.com/q/16693/115
	:init-value t
	:lighter " my-mode"
	:keymap my-mode-map)

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer and magit
(defun turn-off-my-mode ()
	(interactive)
	(my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)
(add-hook 'magit-mode-hook #'turn-off-my-mode)

;; Shorthand for defining key binding with single command
(defun my-key-one (key command)
	(define-key my-mode-map (kbd key) command))

;; Shorthand for defining key binding with multiple commands
(defun my-key (name key &rest commands)
	(defalias (intern (concat "my-" name))
		(append '(lambda () (interactive)) commands)
		name)
	(define-key my-mode-map (kbd key) (intern (concat "my-" name))))

;; Define a repeatable key binding (escape with C-i)
(defun my-key-dir (keys &rest commands)
	(let ((name-prefix (string-join (butlast (mapcar 'symbol-name keys)) "-")))
		(apply 'my-key
					 (list
						(string-join (mapcar 'symbol-name keys) "-")
						(eval (append '(concat) keys))
						(append '(ignore-errors) (list (append '(progn) commands)))
						(list 'setq 'my-key-name-prefix name-prefix)
						'(set-transient-map
							(let ((xkmap (make-sparse-keymap)))
								(define-key xkmap (kbd "C-i") ())
								(dolist (dir '(up down right left begin end))
									(let ((fn (intern (concat "my-" my-key-name-prefix "-" (symbol-name dir)))))
										(when (fboundp fn)
											(define-key xkmap (kbd (eval dir)) fn))))
								xkmap))
						))))
