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
(require 'init-evil)
(require 'org-settings)
(require 'lsp-stuff)
;; (require 'eglot-stuff)

(setq byte-compile-warnings '(cl-functions))
;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Don't clutter my folders
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save files (#something#)
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
;; Files created by packages
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

;; Some global settings
(defalias 'yes-or-no-p 'y-or-n-p)
;; confirm quiting or not
(setq confirm-kill-emacs 'yes-or-no-p)
;; (setq confirm-kill-processes nil)

;; (column-number-mode 1)
(global-subword-mode 1)
(setq default-input-method "arabic")
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(setq-default image-mode nil)
(setq split-width-threshold 0) ;; vertical split by default
;; (setq split-height-threshold nil) ;; horizontal split by default
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
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;; The default is 800 kilobytes.  Measured in bytes.
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

;; Line numbers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (global-display-line-numbers-mode 1)
(blink-cursor-mode -1)
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)
;; Do not load xresources
;; (setq-default inhibit-x-resources 1)

;; count the number of lines to use for line number width
;; (setq-default display-line-numbers-width-start t)
;; Highlight line (gui)
;; (when window-system (global-hl-line-mode nil))

;; highlight for gui and cli
;; (global-hl-line-mode nil)

;; System notifications
(setq compilation-finish-functions
      (append compilation-finish-functions
              '(fmq-compilation-finish)))

(defun fmq-compilation-finish (buffer status)
  (call-process "notify-send" nil nil nil
                "-t" "0"
                "-i" "emacs"
                "Compilation finished in Emacs"
                status))

;; (use-package notifications
;; :config (notifications-notify
;; :title "Notifications"
;; :body "Notifications enabled"
;; :timeout 3000))
;; (defun notify-after-compile (comp-buffer exit-string)
;; (notifications-notify :title "compile"
;; :body (concat (buffer-name comp-buffer)) exit-string
;; :timeout 5000
;; )
;; )
;; (add-hook 'compilation-finish-functions 'notify-after-compile)


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
         ;; ("M-S-u"     . negative-argument)
         ;; ("M-u"     . universal-argument)
         ("M-1" . delete-other-windows)
         ;; ("C-;" . comment-line)
				 ("C-;" . toggle-input-method)
         ("C-x C-;" . comment-box)
				 ))


(global-set-key (kbd "M-2") 'my/split-window-right-and-switch)
(global-set-key (kbd "M-3") 'my/split-window-below-and-switch)
;; Disable ESC (C-g) that closes other splits

(define-key input-decode-map "\e[1;2A" [S-up])

;; electric paris for automatically closing brackets
;; (setq electric-pair-pairs '(
;;                             (?\( . ?\))
;;                             (?\[ . ?\])
;;                             ))
;; (electric-pair-mode t)
;; (electric-indent-mode +1)


;; (auto-revert-mode t)

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
  ;; (yas-reload-all)
	) ;needed so you don't always refresh when adding your own
;; ;
(add-hook 'css-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'yas-minor-mode)
;;                                         ; use yas-describe-tables to see what's available

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

;; ;;; COMPLETION
;; ;;; Aligning Text
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

(use-package diff-hl
  :ensure t
  ;; :unless my/is-termux
  :defer 5
  :init (global-diff-hl-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode))

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

(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
				;; ("RET" . nil)
        ;; ("<escape>" . company-abort)
        )
  ;; (:map lsp-mode-map
  ;; ("<tab>" . company-indent-or-complete-common))
  :hook
  (prog-mode)
	;; (add-hook 'after-init-hook 'global-company-mode) ;not only for programming moed
  :config
  ;; exit in evil normal mode
  ;; (add-hook 'company-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'evil-normal-state-entry-hook
  ;;                       (lambda ()
  ;;                         (company-abort)))))
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; (company-keymap--unbind-quick-access company-active-map) ;; disable using M-number to select items
  ;; (company-tng-configure-default) ;; don't change the default tab behaviour
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  ;; (global-company-mode)
  ;; (with-eval-after-load 'company
  ;;   (define-key company-active-map (kbd "<return>") #'company-complete-selection))
  ;;  :config
  ;;  (setq lsp-completion-provider :capf))
 )

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
  :config
  (evil-define-key 'normal flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (evil-define-key 'normal flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; ;; Javascript
;; (use-package js2-mode
;;   :ensure t)
;; ;; set as the default mode for javascript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (use-package js2-refactor
;;   :ensure t)

;; (setq-default indent-tabs-mode nil)

;; (defconst my/savefile-dir (expand-file-name "savefile" user-emacs-directory))
;; (use-package saveplace
;; :ensure t
;; :config
;; (setq save-place-file (expand-file-name "saveplace" my/savefile-dir))
;; activate it for all buffers
;; (setq-default save-place t))

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

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; (global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "M-0") 'my/delete-window-and-rebalance)
;; (global-set-key (kbd "M-n") 'flycheck-next-error)
;; (global-set-key (kbd "M-p") 'flycheck-previous-error)

(defun switch-to-flycheck-list-errors ()
  (interactive)
  (flycheck-list-errors)
  (pop-to-buffer "*Flycheck errors*"))
(global-set-key (kbd "C-c l") 'switch-to-flycheck-list-errors)


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

 (use-package treemacs
   :ensure t
   :defer t
   :init
   (with-eval-after-load 'winum
     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
   :config
   (progn
     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
           treemacs-deferred-git-apply-delay        0.5
           treemacs-directory-name-transformer      #'identity
           treemacs-display-in-side-window          t
           treemacs-eldoc-display                   'simple
           treemacs-file-event-delay                2000
           treemacs-file-extension-regex            treemacs-last-period-regex-value
           treemacs-file-follow-delay               0.2
           treemacs-file-name-transformer           #'identity
           treemacs-follow-after-init               t
           treemacs-expand-after-init               t
           treemacs-find-workspace-method           'find-for-file-or-pick-first
           treemacs-git-command-pipe                ""
           treemacs-goto-tag-strategy               'refetch-index
           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
           treemacs-hide-dot-git-directory          t
           treemacs-indentation                     2
           treemacs-indentation-string              " "
           treemacs-is-never-other-window           t
           treemacs-max-git-entries                 5000
           treemacs-missing-project-action          'ask
           treemacs-move-forward-on-expand          nil
           treemacs-no-png-images                   nil
           treemacs-no-delete-other-windows         t
           treemacs-project-follow-cleanup          nil
           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
           treemacs-position                        'left
           treemacs-read-string-input               'from-child-frame
           treemacs-recenter-distance               0.1
           treemacs-recenter-after-file-follow      nil
           treemacs-recenter-after-tag-follow       nil
           treemacs-recenter-after-project-jump     'always
           treemacs-recenter-after-project-expand   'on-distance
           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
           treemacs-show-cursor                     nil
           treemacs-show-hidden-files               t
           treemacs-silent-filewatch                nil
           treemacs-silent-refresh                  nil
           treemacs-sorting                         'alphabetic-asc
           treemacs-select-when-already-in-treemacs 'move-back
           treemacs-space-between-root-nodes        t
           treemacs-tag-follow-cleanup              t
           treemacs-tag-follow-delay                1.5
           treemacs-text-scale                      nil
           treemacs-user-mode-line-format           nil
           treemacs-user-header-line-format         nil
           treemacs-wide-toggle-width               70
           treemacs-width                           35
           treemacs-width-increment                 1
           treemacs-width-is-initially-locked       t
           treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 22)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; ;; treesitter
(add-hook 'java-mode-hook 'java-ts-mode)
