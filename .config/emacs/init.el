;; Since it can't reach it's function at runtime
;; Configure package.el to include MELPA.
(setq EMACS_DIR "~/.config/emacs/")
(setq user-init-file "~/.config/emacs/init.el")

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
	(package-refresh-contents)
	(package-install 'use-package))

;; (setq treesit-extra-load-path (concat (file-name-as-directory EMACS_DIR) "tree-sitter-module/dist/"))
;; load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-functions)
(require 'org-settings)
(require 'init-evil)
;; (require 'lsp-stuff)
;; (require 'eglot-stuff)

(setq byte-compile-warnings '(cl-functions))
;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Don't clutter my folders
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; stop creating those #auto-save# files
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

;; Some global settings
(defalias 'yes-or-no-p 'y-or-n-p)
;; confirm quiting or not
;; (setq confirm-kill-emacs 'yes-or-no-p)
;; (setq confirm-kill-processes nil)

;; enable image mode by default
(setq image-mode 1)
(setq default-input-method "arabic")
(setq-default image-mode nil)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)
(setq ibuffer-expert t)
;; warn when o
(setq large-file-warning-threshold 100000000)
;; Always load newest byte code
(setq load-prefer-newer t)
(save-place-mode 1)
(setq-default diff-update-on-the-fly nil)
;; recentf
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)
;; bookmarks default file
(setq bookmark-default-file (concat (file-name-as-directory EMACS_DIR) "/bookmarks"))
(tooltip-mode -1)
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
	(interactive (list my-term-shell)))
(ad-activate 'ansi-term)
;; Automatically save bookmarks upon any modification to them
(setq bookmark-save-flag 1)


;; Startup performance
;; reduce
;; each 50MB of allocated data (the default is on every 0.76MB)
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

;; keybind/unbind
(use-package emacs
	:ensure nil
	:defer nil
	:bind (
				 ("C-x u"   . undo-only)
				 ("C-z"     . nil)
				 ("C-x C-z"     . nil)
				 ("C-x C-u" . undo-redo)
				 ("C-x k" . kill-current-buffer)
				 ("C-?"     . undo-redo)
				 ("C-/"     . undo-only)
				 ("<f5>" . recompile)
				 ;; ("C-c s" . flyspell-mode)
				 ("M-o"   . other-window)
				 ;; ("C-x s"   . shell)
				 ;; ("C-;" . toggle-input-method)
				 ))
;; Look and feel
;; (setq inhibit-x-resources 1)
;; (set-foreground-color "white")
;; (set-background-color "black")
;; (set-cursor-color "white")
;; Set up the visible bell
;; (setq visible-bell 1)
;; (global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(use-package gruvbox-theme
	:ensure t
	:config 
	(load-theme 'gruvbox-dark-hard t))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; startup messages
(setq inhibit-startup-message t)
;; vim-like scrolling
(setq scroll-conservatively 100)
;; (setq ring-bell-function 'ignore)

(blink-cursor-mode -1)

(setq dictionary-server "dict.org")

;; Font settings
(set-face-attribute 'default nil :font "monospace" :height 115)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Sans" :height 115 :weight 'regular)
(set-fontset-font t 'arabic "DejaVu Sans Mono")
(set-fontset-font t nil "monospace" nil 'append)
(setq my/font-change-increment 1.1)


;; bidi settings
;; (setq-default bidi-display-reordering nil)
;; (defun bidi-reordering-toggle ()
;; 	"Toggle bidirectional display reordering."
;; 	(interactive)
;; 	(setq bidi-display-reordering (not bidi-display-reordering))
;; 	(message "bidi reordering is %s" bidi-display-reordering))
;; (defun bidi-display-reordering-on ()
;; 	"Sets bidi-display-reordering-on"
;; 	(setq-local bidi-display-reordering t))
;; (add-hook 'text-mode-hook 'bidi-display-reordering-on)
(defun bidi-direction-toggle ()
	"Will switch the explicit direction of text for current
 buffer. This will set BIDI-DISPLAY-REORDERING to T"
	(interactive "")
	(setq bidi-display-reordering t)
	(if (equal bidi-paragraph-direction 'right-to-left)
			(setq bidi-paragraph-direction 'left-to-right)
		(setq bidi-paragraph-direction 'right-to-left)
		)
	(message "%s" bidi-paragraph-direction))

(use-package rainbow-mode
	:ensure t
	:hook
	(prog-mode))

(use-package yasnippet
	:ensure t
	:config
	(setq yas-indent-line 'auto) ;; do not always indent
	(yas-global-mode 1)
	(use-package yasnippet-snippets
		:ensure t))


(use-package popup-kill-ring
	:ensure t
	:bind ("M-y" . popup-kill-ring))

;; Enable vertico
(use-package vertico
	:ensure t
	:init
	(vertico-mode)
	:config
	(use-package orderless
		:ensure t
		:init
		;; Configure a custom style dispatcher (see the Consult wiki)
		;; (setq orderless-style-dispatchers '(+orderless-dispatch)
		;;       orderless-component-separator #'orderless-escapable-split-on-space)
		(setq completion-styles '(orderless basic)
					completion-category-defaults nil
					completion-category-overrides '((file (styles partial-completion))))))

;; A few more useful configurations...
(use-package emacs
	:init
	;; Add prompt indicator to `completing-read-multiple'.
	;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
	(defun crm-indicator (args)
		(cons (format "[CRM%s] %s"
									(replace-regexp-in-string
									 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
									 crm-separator)
									(car args))
					(cdr args)))
	(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

	;; Do not allow the cursor in the minibuffer prompt
	(setq minibuffer-prompt-properties
				'(read-only t cursor-intangible t face minibuffer-prompt))
	(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; (use-package consult
;;	:ensure t
;;	:bind (
;;				 :map minibuffer-local-map
;;				 ("M-r" . consult-history))
;;	:custom
;;	(completion-in-region-function #'consult-completion-in-region)
;;	:config
;;	(add-hook 'completion-setup-hook #'hl-line-mode))

;; hippie expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
																				 try-expand-dabbrev-all-buffers
																				 try-expand-dabbrev-from-kill
																				 try-complete-file-name-partially
																				 try-complete-file-name
																				 try-expand-all-abbrevs
																				 try-expand-list
																				 try-expand-line
																				 try-complete-lisp-symbol-partially
																				 try-complete-lisp-symbol))
(global-set-key (kbd "M-/")  #'hippie-expand)

(use-package magit
	:ensure t)

(use-package web-mode
	:ensure t
	:config
	(setq web-mode-markup-indent-offset 2
				web-mode-css-indent-offset 2
				web-mode-code-indent-offset 2
				web-mode-indent-style 2)
	(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode)))

(use-package emmet-mode
	:ensure t
	:config
	(add-hook 'web-mode-hook 'emmet-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:init
	(savehist-mode))

(use-package saveplace
	:ensure t
	:config
	(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
	;; activate it for all buffers
	(setq-default save-place t))

;; leetcode
(use-package leetcode
	:ensure t
	:hook
	(leetcode-solution-mode-hook .
															 (lambda() (flycheck-mode -1)))
	:config
	(setq leetcode-prefer-language "java")
	(setq leetcode-prefer-sql "mysql")
	(setq leetcode-save-solutions t)
	(setq leetcode-directory "~/code/exercise/leetcode"))

(use-package flymake
	:hook
	(prog-mode))

(use-package undo-tree
	:ensure t
	:config
	;; autosave the undo-tree history
	(setq undo-tree-history-directory-alist
				`((".*" . ,temporary-file-directory)))
	(setq undo-tree-auto-save-history t)
	(global-undo-tree-mode +1)

	(when (package-installed-p 'evil)
		(evil-set-undo-system 'undo-tree))
	)

;; (use-package which-key
;; 	:ensure t
;; 	:config
;; 	(which-key-mode 1))

;; Latex
;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :hook (LaTeX-mode . (lambda ()
;;                         (push (list 'output-pdf "Zathura")
;;                               TeX-view-program-selection))))

;; System notifications
;; (setq compilation-finish-functions
;;			(append compilation-finish-functions
;;							'(fmq-compilation-finish)))

;; (defun fmq-compilation-finish (buffer status)
;;	(call-process "notify-send" nil nil nil
;;								"-t" "0"
;;								"-i" "emacs"
;;								"Compilation finished in Emacs"
;;								status))

;; ;; Javascript
;; (use-package js2-mode
;;   :ensure t)
;; ;; set as the default mode for javascript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (use-package js2-refactor
;;   :ensure t)

;; (setq-default indent-tabs-mode nil)

;; ;; treesitter
;; (add-hook 'java-mode-hook 'java-ts-mode)


;; (use-package exwm
;;   :ensure t
;;	:config
;;   (require 'exwm-config)
;;   (exwm-config-default)
;;   ;; (global-set-key (kbd "s-k") 'exwm-workspace-delete)
;;   ;; (global-set-key (kbd "s-w") 'exwm-workspace-swap)
;;   ;; Set the default number of workspaces
;;   (setq exwm-workspace-number 5)

;;   ;; When window "class" updates, use it to set the buffer name
;;   (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

;;   ;; Rebind CapsLock to Ctrl
;;   ;; (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

;;   ;; Set the screen resolution (update this to be the correct resolution for your screen!)
;;   (require 'exwm-randr)
;;   (exwm-randr-enable)
;;   ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

;;   ;; Load the system tray before exwm-init
;;   (require 'exwm-systemtray)
;;   (exwm-systemtray-enable)

;;   ;; These keys should always pass through to Emacs
;;   (setq exwm-input-prefix-keys
;;				'(?\C-x
;;					?\C-u
;;					?\C-h
;;					?\M-x
;;					?\M-`
;;					?\M-&
;;					?\M-:
;;					?\C-\M-j  ;; Buffer list
;;					?\C-\ ))  ;; Ctrl+Space

;;   ;; Ctrl+Q will enable the next key to be sent directly
;;   (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;;   ;; Set up global key bindings.  These always work, no matter the input state!
;;   ;; Keep in mind that changing this list after EXWM initializes has no effect.
;;   (setq exwm-input-global-keys
;;         `(
;;           ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
;;           ([?\s-r] . exwm-reset)

;;           ;; Move between windows
;;           ([s-left] . windmove-left)
;;           ([s-right] . windmove-right)
;;           ([s-up] . windmove-up)
;;           ([s-down] . windmove-down)

;;           ;; Launch applications via shell command
;;           ([?\s-&] . (lambda (command)
;;                        (interactive (list (read-shell-command "$ ")))
;;                        (start-process-shell-command command nil command)))

;;           ;; Switch workspace
;;           ([?\s-w] . exwm-workspace-switch)
;;           ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

;;           ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
;;           ,@(mapcar (lambda (i)
;;                       `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                           (interactive)
;;                           (exwm-workspace-switch-create ,i))))
;;                     (number-sequence 0 9))))
;;   (exwm-enable)))
