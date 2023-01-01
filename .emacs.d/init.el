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

;; load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-functions)
(require 'org-settings)
;; (require 'eglot-stuff)

(setq byte-compile-warnings '(cl-functions))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Don't clutter my folders
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode (#something) doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))


;; Some global settings
(defalias 'yes-or-no-p 'y-or-n-p)
;; confirm quiting or not
(setq confirm-kill-emacs 'yes-or-no-p)

;; (column-number-mode 1)
(global-subword-mode 1)
(setq default-input-method "arabic")
(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(setq-default image-mode nil)
(setq split-width-threshold 0) ;; vertical split by default
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)
(setq ibuffer-expert t)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; Always load newest byte code
(setq load-prefer-newer t)
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
(setq gc-cons-threshold (* 100 1000 1000))

;; Look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; startup messages
(setq inhibit-startup-message t)
;; vim-like scrolling
(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)
;; (set-background-color "black")
;; (set-foreground-color "white")
;; (set-cursor-color "white")
(blink-cursor-mode -1)
;; Do not load xresources
;; (setq-default inhibit-x-resources 1)

;; Font settings
(set-face-attribute 'default nil :font "monospace" :height 135)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "monospace" :height 135)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Sans" :height 135 :weight 'regular)
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

;; Key bindings/unbindings
(use-package emacs
  :ensure nil
  :defer nil
  :bind (
				 ("C-x u"   . undo-only)
				 ("C-?"     . undo-redo)
         ("C-/"     . undo-only)
         ("C-z"     . undo-only)
         ("C-S-z"   . undo-redo)
         ("C-x C-u" . undo-redo)
				 ("<f5>" . recompile)
				 ("C-c s" . flyspell-mode)
         ("C-c w"   . fixup-whitespace)
         ("M-o"   . other-window)
         ("C-x S"   . shell)
         ("M-1" . delete-other-windows)
         ("C-;" . comment-line)
				 ("C-'" . toggle-input-method)
         ("C-x C-;" . comment-box)
				 ))
(global-set-key (kbd "M-0") 'my/delete-window-and-rebalance)
(global-set-key (kbd "M-2") 'my/split-window-right-and-switch)
(global-set-key (kbd "M-3") 'my/split-window-below-and-switch)


;; Latex settings

(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . (lambda ()
                        (push (list 'output-pdf "Zathura")
                              TeX-view-program-selection))))

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
  (yas-reload-all)
	)
(add-hook 'css-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'yas-minor-mode)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

;;; Aligning Text
(use-package align
  :ensure nil
  :defer t
  :bind ("C-x a a" . align-regexp)
  :config
  ;; Align using spaces
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

(use-package vertico
  :ensure t
	:init
;;;; Out Of Order Compleiton
  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))

;; ;;;; Extra Completion Functions
(use-package consult
	:ensure t
	:bind (
				 ("C-x b"       . consult-buffer)
				 ("C-x C-k C-k" . consult-kmacro)
				 ("M-y"         . consult-yank-pop)
				 ("M-g g"       . consult-goto-line)
				 ("M-g M-g"     . consult-goto-line)
				 ("M-g f"       . consult-flymake)
				 ("M-i"       . consult-imenu)
				 ("M-s l"       . consult-line)
				 ("M-s L"       . consult-line-multi)
				 ("M-s u"       . consult-focus-lines)
				 ("M-s g"       . consult-ripgrep)
				 ("C-x C-SPC"   . consult-global-mark)
				 ("C-x M-:"     . consult-complex-command)
				 ("C-c n"       . consult-org-agenda)
				 ("C-c m"     . my/notegrep)
				 :map dired-mode-map
				 ("O" . consult-file-externally)
				 :map help-map
				 ("a" . consult-apropos)
				 :map minibuffer-local-map
				 ("M-r" . consult-history))
	:custom
	(completion-in-region-function #'consult-completion-in-region)
	:config
	(add-hook 'completion-setup-hook #'hl-line-mode))

  (use-package marginalia
    :ensure t
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode t)
  :config
  ;; Used for the vertico-directory extension
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setu
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)))

(use-package magit
  :ensure t)

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

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/")  #'hippie-expand)

(use-package corfu
	:ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-echo-documentation 0.25) ; Enable documentation for completions
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first t)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("S-<return>" . nil)
              ("RET"     . nil) ;; leave my enter alone!
              )
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
	(setq tab-always-indent 'complete)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))))
;; fuzzy completion for corfu
(use-package orderless
  :ensure t
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

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
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defconst my/savefile-dir (expand-file-name "savefile" user-emacs-directory))
(use-package saveplace
	:ensure t
	:config
	(setq save-place-file (expand-file-name "saveplace" my/savefile-dir))
	;; activate it for all buffers
	(setq-default save-place t))

(use-package subword
  :ensure t
  :config (global-subword-mode 1))


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
  (setq leetcode-directory "~/exercise/code/leetcode")
	)

;; ;; treesitter
(add-hook 'java-mode-hook 'java-ts-mode)

