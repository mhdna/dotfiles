;; Since it can't reach it's function at runtime
;; Configure package.el to include MELPA.
(setq EMACS_DIR "~/.config/emacs/")
(setq EMACS_CACHE_DIR "~/.cache/emacs/")
(setq user-init-file (expand-file-name "init.el" EMACS_DIR))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq byte-compile-warnings '(cl-functions))

;; load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-functions)
;; (require 'init-tex)
(require 'init-org)
(require 'init-evil)

;; Do not clutter my Emacs directory
(setq user-emacs-directory (expand-file-name EMACS_CACHE_DIR))
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; stop creating those #auto-save# files
;; (setq auto-save-default nil)
;; (setq create-lockfiles nil)

;; native compilation stuff
(when (boundp 'native-comp-eln-load-path)
  ;; ;; ;; Don't store eln files in ~/.emacs.d/eln-cache (where they can easily be deleted).
  ;; ;; ;; REVIEW Use `startup-redirect-eln-cache' when 28 support is dropped
  ;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" EMACS_CACHE_DIR))
  ;; ;;
  ;; UX: Suppress compiler warnings and don't inundate users with their popups.
  ;; ;; ;;   They are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug))

;; Startup performance
;; reduce each 50MB of allocated data (the default is on every 0.76MB)
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

;; ;; Some global settings
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

;; Always indent with 2 spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(setq use-dialog-box nil) ;; Do not show gui dialog boxs, instead, use the minibuffer
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t) ;; maximize on startup
(setq split-width-threshold 0) ;; default splits to vertical
(blink-cursor-mode -1) ;; disable blinking cursor
;; Set up the visible bell
;; (setq visible-bell 1)

(setq column-number-mode t)
(global-visual-line-mode) ;; works better with bidi mode and arabic input method in general

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
;; count the number of lines to use for line number width
(setq-default display-line-numbers-width-start t)

;; (global-hl-line-mode 1)

;; set transparency
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

;; (setq inhibit-x-resources t)
;; dark mode
;; (set-background-color "black")
;; (set-foreground-color "white")
;; (set-cursor-color "white")
;; (set-variable 'frame-background-mode 'dark)
(load-theme 'manoj-dark t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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

;; install xclip (for copying in tui)
;; (use-package xclip
;;   :config
;;   (xclip-mode 1))

(setq dired-dwim-target t)
(setq ibuffer-expert t)
;; warn when o
(setq large-file-warning-threshold 100000000)
;; Always load newest byte code
(setq load-prefer-newer t)
(setq-default diff-update-on-the-fly nil)
;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 500) ;; more recentf amount
(run-at-time nil (* 5 60) 'recentf-save-list)
;; bookmarks default file
(setq bookmark-default-file (concat (file-name-as-directory EMACS_DIR) "/bookmarks"))
(tooltip-mode -1)

;; Enable clipboard integration in TUI mode
(unless (display-graphic-p)
  (setq select-enable-clipboard t
        select-enable-primary t))

;; (defvar my-term-shell "/bin/zsh")
;; (defadvice ansi-term (before force-zsh)
;;	(interactive (list my-term-shell)))
;; (ad-activate 'ansi-term)

;; keybind/unbind
(use-package emacs
  :ensure nil
  :defer nil
  :bind (
         ("C-x u"   . undo-only)
         ("C-z"     . nil)
         ("C-x C-z"     . nil)
         ("C-x C-u" . undo-redo)
         ("C-x k" . my/close-current-buffer)
         ("C-?"     . undo-redo)
         ("C-/"     . undo-only)
         ("M-o"   . other-window)
         ("M-SPC"	.					 nil) ;; so it doesn't interfere with my remaps key binding
         ("C-SPC".						 'toggle-input-method)
         ("C-x s"   . eshell))
  ;; useful completion configurations (for vertico)...
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

(define-key emacs-lisp-mode-map (kbd "<f9>") 'eval-buffer)
(global-set-key (kbd "<f8>")						 'my/indent-buffer)

(setq dictionary-server "dict.org")

;; (setq ispell-program-name "aspell")
;; ;; Set the default dictionary to use for spell-checking
;; (setq ispell-dictionary "ar") ; Replace "en" with the desired language code
;; ;; Optionally, set the default language for new buffers
;; (setq-default ispell-dictionary "ar")

(defun my/setup-font-faces ()
  "Setup all gui font faces."
  (when (display-graphic-p)
    (setq my/font-change-increment 1.1)
    ;; set default font
    (set-face-attribute 'default nil :font (font-spec :family "monospace" :size 15 :weight 'regular))
    ;; Arabic font
    (set-fontset-font t 'arabic "DejaVu Sans Mono")
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "monospace" :size 16 :weight 'regular))
    ;; Set the variable pitch face which is the same for mac and linux
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Sans" :size 15 :weight 'regular))
    ;; Makes commented text and keywords italics. This is working in emacsclient but not emacs. Your font must have an italic face available.
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Sans" :size 15 :weight 'regular))
    ))
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'my/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'my/setup-font-faces)

;; bidi settings
(setq-default bidi-paragraph-direction 'left-to-right)
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
    (setq bidi-paragraph-direction 'right-to-left))
  (message "%s" bidi-paragraph-direction))

(use-package yasnippet
  :config
  (setq yas-indent-line 'auto) ;; do not always indent
  (use-package yasnippet-snippets)
  :init
  (yas-global-mode 1))

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

(use-package consult
  :bind (
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (add-hook 'completion-setup-hook #'hl-line-mode))

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

(use-package rainbow-mode
  :config
  ;; Rainbow mode doesn't work globaly by default, so I'll define my own global mode
  (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode 1)))
  (my-global-rainbow-mode 1))

(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            ))
;; Do not pair quotes
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
(electric-pair-mode t)

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items nil))

;; (use-package chatgpt-shell
;;   :custom
;;   ((chatgpt-shell-openai-key
;;     (lambda ()
;;       (auth-source-pass-get 'secret "openai-key")))))
;; ;; if you are using the "pass" password manager
;; (setq chatgpt-shell-openai-key
;;       (lambda ()
;;         ;; (auth-source-pass-get 'secret "openai-key") ; alternative using pass support in auth-sources
;;         (nth 0 (process-lines "pass" "show" "gpt"))))
