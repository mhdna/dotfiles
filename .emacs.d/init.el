;; Since it can't reach it's function at runtime
;; Configure package.el to include MELPA.
(setq EMACS_DIR "~/.emacs.d/")
;; (setq user-init-file "~/.emacs.d/init.el")
(setq user-init-file "~/.emacs.d/init.el")

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
;; auto-save-mode doesn't create the path automatically! (#something#)
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
			auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
;; Files created by packages
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
			lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

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
;; warn when opening files bigger than 100MB
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
(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-zsh)
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
				 ("M-2" . 'my/split-window-below-and-switch)
				 ("M-3" . 'my/split-window-right-and-switch)
				 ("C-x u"   . undo-only)
				 ("C-z"     . nil)
				 ("C-x C-z"     . nil)
				 ("C-x C-u" . undo-redo)
				 ("C-x k" . kill-current-buffer)
				 ("C-c R" . revert-buffer)
				 ("C-?"     . undo-redo)
				 ("C-/"     . undo-only)
				 ("<f5>" . recompile)
				 ("C-c s" . flyspell-mode)
				 ("M-o"   . other-window)
				 ("C-x s"   . shell)
				 ("C-;" . toggle-input-method)
				 ("C-c ! l" . 'my/switch-to-flycheck-list-errors)

         ("M-u"     . universal-argument)
         ("M-u"     . universal-argument)
         ("M-S-u"     . negative-argument)
				 ))

;; Look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; startup messages
(setq inhibit-startup-message t)
;; vim-like scrolling
(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)

(blink-cursor-mode -1)

;; System notifications
;; (setq compilation-finish-functions
;; 			(append compilation-finish-functions
;; 							'(fmq-compilation-finish)))

;; (defun fmq-compilation-finish (buffer status)
;; 	(call-process "notify-send" nil nil nil
;; 								"-t" "0"
;; 								"-i" "emacs"
;; 								"Compilation finished in Emacs"
;; 								status))

;; Font settings
(set-face-attribute 'default nil :font "monospace" :height 115)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "monospace" :height 115)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Sans" :height 115 :weight 'regular)
(set-fontset-font "fontset-default" 'arabic (font-spec :family "DejaVu Sans Mono"))
(setq my/font-change-increment 1.1)


;; bidi settings
(setq-default bidi-display-reordering nil)
(defun bidi-reordering-toggle ()
	"Toggle bidirectional display reordering."
	(interactive)
	(setq bidi-display-reordering (not bidi-display-reordering))
	(message "bidi reordering is %s" bidi-display-reordering))
(defun bidi-display-reordering-on ()
	"Sets bidi-display-reordering-on"
	(setq-local bidi-display-reordering t))
(add-hook 'text-mode-hook 'bidi-display-reordering-on)
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

;; electric paris for automatically closing brackets
;; (setq electric-pair-pairs '(
;;                             (?\( . ?\))
;;                             (?\[ . ?\])
;;                             ))
;; (electric-pair-mode t)
;; (electric-indent-mode +1)

;; Latex
;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :hook (LaTeX-mode . (lambda ()
;;                         (push (list 'output-pdf "Zathura")
;;                               TeX-view-program-selection))))

(use-package rainbow-mode
	:ensure t
	:hook
	(prog-mode)
	)

(use-package yasnippet
	:ensure t
	:config
	(use-package yasnippet-snippets
		:ensure t)
	(use-package java-snippets
		:ensure t)
	)

(add-hook 'css-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'yas-minor-mode)

(use-package popup-kill-ring
	:ensure t
	:bind ("M-y" . popup-kill-ring))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

	;; (use-package consult
	;; 	:ensure t
	;; 	:bind (
	;; 				 ("C-x b"       . consult-buffer)
	;; 				 ("C-c r"       . consult-recent-file)
	;; 				 ("C-x C-k C-k" . consult-kmacro)
	;; 				 ("M-y"         . consult-yank-pop)
	;; 				 ("M-g g"       . consult-goto-line)
	;; 				 ("M-g M-g"     . consult-goto-line)
	;; 				 ("M-g f"       . consult-flymake)
	;; 				 ("M-i"       . consult-imenu)
	;; 				 ("M-s l"       . consult-line)
	;; 				 ("M-s L"       . consult-line-multi)
	;; 				 ("M-s u"       . consult-focus-lines)
	;; 				 ("M-s g"       . consult-ripgrep)
	;; 				 ("C-x C-SPC"   . consult-global-mark)
	;; 				 ("C-x M-:"     . consult-complex-command)
	;; 				 ("C-c n"       . consult-org-agenda)
	;; 				 ("C-c m"     . my/notegrep)
	;; 				 :map dired-mode-map
	;; 				 ("O" . consult-file-externally)
	;; 				 :map help-map
	;; 				 ("a" . consult-apropos)
	;; 				 :map minibuffer-local-map
	;; 				 ("M-r" . consult-history))
	;; 	:custom
	;; 	(completion-in-region-function #'consult-completion-in-region)
	;; 	:config
	;; 	(add-hook 'completion-setup-hook #'hl-line-mode))

(use-package magit
	:ensure t)

(use-package emmet-mode
	:ensure t
	:config
	(add-hook 'web-mode-hook 'emmet-mode)
	;; (add-hook 'js2-mode-hook 'emmet-mode)
	)

(use-package flycheck
	:ensure t
	:hook
	(prog-mode)
	)

(use-package web-mode
	:ensure t
	:config
	(setq web-mode-markup-indent-offset 2
				web-mode-css-indent-offset 2
				web-mode-code-indent-offset 2
				web-mode-indent-style 2))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))

;; ;; Javascript
;; (use-package js2-mode
;;   :ensure t)
;; ;; set as the default mode for javascript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (use-package js2-refactor
;;   :ensure t)

;; (setq-default indent-tabs-mode nil)

;; (defconst my/savefile-dir (expand-file-name "savefile" user-emacs-directory))
(use-package saveplace
:ensure t
:config
(setq save-place-file (expand-file-name "saveplace" my/savefile-dir))
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
	(setq leetcode-directory "~/code/exercise/leetcode")
	)

(use-package undo-tree
	:ensure t
	:config
	;; autosave the undo-tree history
	(setq undo-tree-history-directory-alist
				`((".*" . ,temporary-file-directory)))
	(setq undo-tree-auto-save-history t)
	(global-undo-tree-mode +1)
	(evil-set-undo-system 'undo-tree)
)

;; ;; treesitter
;; (add-hook 'java-mode-hook 'java-ts-mode)
