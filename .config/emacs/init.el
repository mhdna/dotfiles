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
(setq use-package-always-ensure t)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq byte-compile-warnings '(cl-functions))

;; load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-functions)
(require 'init-tex)
(require 'init-dev)
(require 'init-org)
(require 'init-evil)
(require 'init-lsp)
;; (require 'init-eglot)

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
(setq confirm-kill-emacs 'yes-or-no-p)
;; (setq confirm-kill-processes nil)

;; save a list of open files in user-emacs-dir/.emacs.desktop
;; (setq desktop-path (list user-emacs-directory)
;;			desktop-auto-save-timeout 600)
;; ;; disable frame sizing so it doesn't conflict with auto maximization
;; (setq desktop-restore-frames nil)
;; (desktop-save-mode 1)

(setq use-dialog-box nil) ;; Don't show gui dialog boxs, use minibuffer instead
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t) ;; maximize on startup
;; (setq split-width-threshold 0) ;; default splits to vertical
;; (setq inhibit-x-res
(blink-cursor-mode -1) ;; disable blinking cursor
;; Set up the visible bell
;; (setq visible-bell 1)
(global-display-line-numbers-mode 1)
;; (global-visual-line-mode 1)
;; (global-hl-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'manoj-dark t)
;; startup messages
(setq inhibit-startup-message t)
;; vim-like scrolling
(setq scroll-conservatively 100)
(setq scroll-margin 8) ;; scroll offset
;; (setq ring-bell-function 'ignore)
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
(setq-default diff-update-on-the-fly nil)
;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; more recentf amount
(run-at-time nil (* 5 60) 'recentf-save-list)
;; bookmarks default file
(setq bookmark-default-file (concat (file-name-as-directory EMACS_DIR) "/bookmarks"))
(tooltip-mode -1)

;; (defvar my-term-shell "/bin/zsh")
;; (defadvice ansi-term (before force-zsh)
;;	(interactive (list my-term-shell)))
;; (ad-activate 'ansi-term)
;; (use-package vterm
;;   :commands vterm
;;   :config
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
;;   ;;(setq vterm-shell "zsh")
;;   (setq vterm-max-scrollback 10000))
;; Configure eshell
;; https://github.com/daviwil/emacs-from-scratch/blob/f4918aadf6970b098999d28bdbc212942aa62b80/show-notes/Emacs-09.org#eshell
(defun my/configure-eshell ()
	;; Save command history when commands are entered
	(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

	;; Truncate buffer for performance
	(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

	;; Bind some useful keys for evil-mode
	(evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
	(evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
	(evil-normalize-keymaps)

	(setq eshell-history-size         10000
				eshell-buffer-maximum-lines 10000
				eshell-hist-ignoredups t
				eshell-scroll-to-bottom-on-input t))

(use-package eshell
	:hook (eshell-first-time-mode . my/configure-eshell))
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

(setq dictionary-server "dict.org")

;; Font settings
;; (setq my/font-change-increment 1.1)
;; (set-face-attribute 'default nil :font (font-spec :family "monospace" :height 110 :weight 'regular))
;; ;; Makes commented text and keywords italics. This is working in emacsclient but not emacs. Your font must have an italic face available.
;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
;; ;; adjust line spacing
;; ;; (setq-default line-spacing 0.12)
;; ;; Arabic font
;; (set-fontset-font t 'arabic "DejaVu Sans Mono")
;; ;; Set the fixed pitch face
;; (set-face-attribute 'fixed-pitch nil :font (font-spec :family "monospace" :size 15 :weight 'regular))
;; ;; Set the variable pitch face which is the same for mac and linux
;; (set-face-attribute 'variable-pitch nil :font (font-spec :family "Sans" :size 14 :weight 'regular))

(defun my/setup-font-faces ()
	"Setup all gui font faces."
	(when (display-graphic-p)
		;; set default font
		(set-face-attribute 'default nil :font (font-spec :family "monospace" :size 16 :weight 'regular))
		;; Arabic font
		(set-fontset-font t 'arabic "DejaVu Sans Mono")
		;; Set the fixed pitch face
		(set-face-attribute 'fixed-pitch nil :font (font-spec :family "monospace" :size 16 :weight 'regular))
		;; Set the variable pitch face which is the same for mac and linux
		(set-face-attribute 'variable-pitch nil :font (font-spec :family "Sans" :size 15 :weight 'regular))
		;; (set-frame-parameter (selected-frame) 'alpha '(100 100))
		;; (add-to-list 'default-frame-alist '(alpha 100 100))
		;; set current frame width and height characters
		;; (set-frame-width (frame-focus) 75)
		;; (set-frame-height (frame-focus) 30)
		)
	)
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'my/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'my/setup-font-faces)


;; bidi settings
;; (setq-default bidi-display-reordering nil)
;; (defun bidi-reordering-toggle ()
;;	"Toggle bidirectional display reordering."
;;	(interactive)
;;	(setq bidi-display-reordering (not bidi-display-reordering))
;;	(message "bidi reordering is %s" bidi-display-reordering))
;; (defun bidi-display-reordering-on ()
;;	"Sets bidi-display-reordering-on"
;;	(setq-local bidi-display-reordering t))
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

;; (use-package yasnippet
;;	:config
;;	(setq yas-indent-line 'auto) ;; do not always indent
;;	(use-package yasnippet-snippets)
;;	:init
;;	(yas-global-mode 1))


;; (use-package popup-kill-ring
;;	:bind ("M-y" . popup-kill-ring))

;; Enable vertico
(use-package vertico
	:init
	(vertico-mode)
	:config
	(use-package orderless
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

(use-package consult
	:bind (
				 :map minibuffer-local-map
				 ("M-r" . consult-history))
	:custom
	(completion-in-region-function #'consult-completion-in-region)
	:config
	(add-hook 'completion-setup-hook #'hl-line-mode))

;; hippie expand
;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;																				 try-expand-dabbrev-all-buffers
;;																				 try-expand-dabbrev-from-kill
;;																				 try-complete-file-name-partially
;;																				 try-complete-file-name
;;																				 try-expand-all-abbrevs
;;																				 try-expand-list
;;																				 try-expand-line
;;																				 try-complete-lisp-symbol-partially
;;																				 try-complete-lisp-symbol))
;; (global-set-key (kbd "M-/")  #'hippie-expand)

(use-package magit)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:init
	(savehist-mode))

(use-package saveplace
	:init
	(save-place-mode 1)
	:config
	(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
	;; activate it for all buffers
	(setq-default save-place t))

;; leetcode
;; (use-package leetcode
;;	:hook
;;	(leetcode-solution-mode-hook .
;;															 (lambda() (flycheck-mode -1)))
;;	:config
;;	(setq leetcode-prefer-language "java")
;;	(setq leetcode-prefer-sql "mysql")
;;	(setq leetcode-save-solutions t)
;;	(setq leetcode-directory "~/code/exercise/leetcode"))

(use-package undo-tree
	:config
	;; autosave the undo-tree history
	(setq undo-tree-history-directory-alist
				`((".*" . ,temporary-file-directory)))
	(setq undo-tree-auto-save-history t)
	(global-undo-tree-mode +1))

(use-package whitespace
	:ensure nil
	:hook (before-save . whitespace-cleanup))

;; (use-package minions
;;	:demand t

;;	:custom
;;	(minions-mode-line-delimiters (cons "" ""))

;;	:config
;;	(defun +set-minions-mode-line-lighter ()
;;		(setq minions-mode-line-lighter
;;					(if (display-graphic-p) "" )))

;;	(add-hook 'server-after-make-frame-hook '+set-minions-mode-line-lighter)

;;	(minions-mode 1))

;; (use-package which-key
;;	:config
;;	(which-key-mode 1))


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


(use-package rainbow-mode
	:config
	;; Rainbow mode doesn't work globaly by default, so I'll define my own global mode
	(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
		(lambda () (rainbow-mode 1)))
	(my-global-rainbow-mode 1))


;; (use-package avy
;;	:bind*
;;	("M-s" . evil-avy-goto-char))

;; (setq-default indent-tabs-mode nil)

;; ;; treesitter
;; (add-hook 'java-mode-hook 'java-ts-mode)


;; (use-package exwm
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


;;; EXTRA UI
;; (use-package hl-todo :hook prog-mode)

;; (use-package winum
;;	:config
;;	(global-set-key (kbd "M-0") 'treemacs-select-window)
;;	(global-set-key (kbd "M-1") 'winum-select-window-1)
;;	(global-set-key (kbd "M-2") 'winum-select-window-2)
;;	(global-set-key (kbd "M-3") 'winum-select-window-3)
;;	(global-set-key (kbd "M-4") 'winum-select-window-4)
;;	(global-set-key (kbd "M-5") 'winum-select-window-5)
;;	(global-set-key (kbd "M-6") 'winum-select-window-6)
;;	(global-set-key (kbd "M-7") 'winum-select-window-7)
;;	(global-set-key (kbd "M-8") 'winum-select-window-8)
;;	(winum-mode))

;; (use-package powerline
;;	:config
;;	(powerline-default-theme))

(use-package imenu
	:ensure nil
	:custom
	(imenu-auto-rescan t)
	(imenu-max-items nil))

(use-package treemacs-icons-dired
	:hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package chatgpt-shell
	:ensure t
	:custom
	((chatgpt-shell-openai-key
		(lambda ()
			(auth-source-pass-get 'secret "openai-key")))))
;; if you are using the "pass" password manager
(setq chatgpt-shell-openai-key
			(lambda ()
				;; (auth-source-pass-get 'secret "openai-key") ; alternative using pass support in auth-sources
				(nth 0 (process-lines "pass" "show" "gpt"))))
