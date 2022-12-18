;; Since it can't reach it's function at runtime
;; Configure package.el to include MELPA.
(setq EMACS_DIR "~/.emacs.d/")
;; (setq user-init-file "~/.emacs.d/init.el")
(setq user-init-file "~/.emacs.d/init.el")

;; (native-compile-async "~/.emacs.d/elpa/" 4 t)

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


;; maximize the initial frame automatically
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))



;; (org-babel-load-file "~/.emacs.d/config.org")
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; (setq treesit-extra-load-path (concat (file-name-as-directory EMACS_DIR) "tree-sitter-module/dist/"))
;; load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'my-functions)
;; (require 'init-evil)
(require 'org-settings)
;; (require 'lsp-stuff)
;; (require 'eglot-stuff)

;; (setq native-comp-eln-load-path "/home/mahdi/.emacs.d/eln-cache/" "/usr/local/lib/emacs/29.0.60/native-lisp/")
;; (setq package-native-compile t)
;; PACKAGING configuration
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)
;; (use-package auto-compile
;;   :ensure t
;;   :config (auto-compile-on-load-mode))
;; (setq load-prefer-newer t)
(setq byte-compile-warnings '(cl-functions))
;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t))
;; (load-file "~/.emacs.d/sensible-defaults.el")
;; (sensible-defaults/use-all-settings)
;; (sensible-defaults/use-all-keybindings)
;; (sensible-defaults/backup-to-temp-directory)

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
;; (setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
;;       lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

;; ;; (require 'xah-fly-keys)
;; ;; specify a layout
;; (xah-fly-keys-set-layout "qwerty")
;; ;; possible values
;; ;; adnw , azerty , azerty-be , beopy , bepo , carpalx-qfmlwy , carpalx-qgmlwb , carpalx-qgmlwy , colemak , colemak-dhm , colemak-dhm-angle , colemak-dhk , dvorak , koy , neo2 , norman , programer-dvorak , pt-nativo , qwerty , qwerty-abnt , qwerty-no (qwerty Norwegian) , qwertz , workman
;; (xah-fly-keys 1)


;; Some global settings
(defalias 'yes-or-no-p 'y-or-n-p)
;; confirm quiting or not
(setq confirm-kill-emacs 'yes-or-no-p)
;; (setq confirm-kill-processes nil)
(line-number-mode 1)
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
;; perfomance stuff
;; (setq read-process-output-max (* 1024 1024))
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))
;; (defun my/display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;;            (format "%.2f seconds"
;;                    (float-time
;;                     (time-subtract after-init-time before-init-time)))
;;            gcs-done))
;; (add-hook 'emacs-startup-hook #'my/display-startup-time)


;; Look and feel

;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)
;; startup messages
(setq inhibit-startup-message t)
;; vim-like scrolling
(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)
;; (load-file "~/.emacs.d/organic-green-theme.el")
;; (load-theme 'organic-green t)
(load-theme 'manoj-dark)
;; (set-background-color "white")
;; (set-foreground-color "black")
;; (set-cursor-color "black")
;; (global-display-line-numbers-mode 1)
;; Change mark region color
;; (set-face-attribute 'region nil :background "#ffff00")
;; (set-face-background 'minibuffer-prompt "#770000")
;; (set-face-foreground 'minibuffer-prompt "white")
(set-window-scroll-bars (minibuffer-window) nil nil)
(blink-cursor-mode -1)
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)
;; Do not load xresources
(setq-default inhibit-x-resources 1)
;; Line numbers
;;   (setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-display-line-numbers-mode 1)
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
(set-face-attribute 'default nil :font "Liberation Mono" :height 105)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Liberation Mono" :height 105)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Liberation Mono" :height 105 :weight 'regular)
(set-fontset-font "fontset-default" 'arabic (font-spec :family "Dejavu Sans Mono"))
(setq my/font-change-increment 1.1)
(custom-set-faces
 '(default ((t (:inherit nil :height 110 :family "Liberation Mono")))))


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
         ("<f1>" . toggle-input-method)
				 ("<f5>" . recompile)
				 ("C-c s" . flyspell-mode)
         ("C-c w"   . fixup-whitespace)
         ("M-o"   . other-window)
         ("C-x S"   . shell)
         ("M-S-u"     . negative-argument)
         ("M-u"     . universal-argument)
         ("M-1" . delete-other-windows)
         ;; ("C-;" . comment-line)
         ("C-x C-;" . comment-box)
         ;; ("C-x C-;" . eval-buffer)
         ;; ("M-;" . eval-last-sexp)
         ))

(defun back-window ()
  (interactive)
  (other-window -1))
;; (global-set-key (kbd "C-/") 'comment-line)
;; (global-set-key (kbd "C-S-/") 'comment-box)
;; (global-set-key (kbd "M-/") 'comment-dwim)
(global-set-key (kbd "M-2") 'my/split-window-right-and-switch)
(global-set-key (kbd "M-3") 'my/split-window-below-and-switch)
;; Disable ESC (C-g) that closes other splits

;;(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;;(setq ido-create-new-buffer 'always)
(define-key input-decode-map "\e[1;2A" [S-up])
;; unbind C-x C-x for closing, and C-x C-z for suspending
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))

;; Useful functions
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(ido-mode t)

;; electric paris for automatically closing brackets
;; (setq electric-pair-pairs '(
;;                             (?\( . ?\))
;;                             (?\[ . ?\])
;;                             ))
;; (electric-pair-mode t)
;; ;; (auto-revert-mode t)
;; ;; for automatically indenting new lines
;; (electric-indent-mode +1)

;; Latex settings

;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :hook (LaTeX-mode . (lambda ()
;;                         (push (list 'output-pdf "Zathura")
;;                               TeX-view-program-selection))))

;; (use-package which-key
;;   :ensure t
;;   :config (which-key-mode))
;;

;; (use-package rainbow-mode
;;   :ensure t
;;   ;; :init (add-hook 'prog-mode-hook 'rainbow-mode)
;;   )
;; (add-hook 'after-init-hook #'rainbow-mode)
;; (add-hook 'prog-mode #'rainbow-mode)


;; (use-package rainbow-delimiters
;;   :ensure t)


;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (use-package yasnippet-snippets
;;     :ensure t)
;;   (use-package java-snippets
;;     :ensure t)
;;   (yas-reload-all)) ;needed so you don't always refresh when adding your own
;; ;; ;
;; (yas-global-mode 1)
;; ;; (add-hook 'lua-mode-hook 'yas-minor-mode)
;; ;; (add-hook 'java-mode-hook 'yas-minor-mode)
;; ;; (add-hook 'c-mode-hook 'yas-minor-mode)
;; ;; (add-hook 'python-mode-hook 'yas-minor-mode)
;; ;; (add-hook 'elisp-mode-hook 'yas-minor-mode)
;; ;; (add-hook 'org-mode-hook 'yas-minor-mode)
;; ;; (add-hook 'c++-mode-hook 'yas-minor-mode)
;;                                         ; use yas-describe-tables to see what's available

;; ;; (use-package popup-kill-ring
;; ;;   :ensure t
;; ;;   :bind ("M-y" . popup-kill-ring))

;; ;; (use-package expand-region
;; ;;   :ensure t)
;; ;; ;; :bind ("C-q" . er/expand-region))

;; ;;; COMPLETION
;; ;;; Aligning Text
;; (use-package align
;;   :ensure nil
;;   :defer t
;;   :bind ("C-x a a" . align-regexp)
;;   :config
;;   ;; Align using spaces
;;   (defadvice align-regexp (around align-regexp-with-spaces activate)
;;     (let ((indent-tabs-mode nil))
;;       ad-do-it)))

;; ;;; COMPLETION
;; (use-package vertico
;;   :ensure t
;;   :init
;; ;;;; Out Of Order Compleiton
;;   (use-package orderless
;;     :commands (orderless)
;;     :custom (completion-styles '(orderless flex)))

;; ;;;; Extra Completion Functions
;; 	(use-package consult
;; 		:ensure t
;; 		:bind (
;; 					 ;; ("C-x b"       . consult-buffer)
;; 					 ("C-x C-k C-k" . consult-kmacro)
;; 					 ("M-y"         . consult-yank-pop)
;; 					 ("M-g g"       . consult-goto-line)
;; 					 ("M-g M-g"     . consult-goto-line)
;; 					 ("M-g f"       . consult-flymake)
;; 					 ;; ("M-i"       . consult-imenu)
;; 					 ;; ("M-s l"       . consult-line)
;; 					 ;; ("M-s L"       . consult-line-multi)
;; 					 ("M-s u"       . consult-focus-lines)
;; 					 ("M-s g"       . consult-ripgrep)
;; 					 ("C-x C-SPC"   . consult-global-mark)
;; 					 ("C-x M-:"     . consult-complex-command)
;; 					 ("C-c n"       . consult-org-agenda)
;; 					 ("C-c m"     . my/notegrep)
;; 					 :map dired-mode-map
;; 					 ("O" . consult-file-externally)
;; 					 :map help-map
;; 					 ("a" . consult-apropos)
;; 					 :map minibuffer-local-map
;; 					 ("M-r" . consult-history))
;; 		:custom
;; 		(completion-in-region-function #'consult-completion-in-region)
;; 		:config
;; 		(defun my/notegrep ()
;; 			"Use interactive grepping to search my notes"
;; 			(interactive)
;; 			(consult-ripgrep org-directory))
;; 		(add-hook 'completion-setup-hook #'hl-line-mode)
;; 		(recentf-mode t))

;;   ;; (load (concat user-emacs-directory
;;   ;;               "lisp/affe-config.el"))

;;   (use-package marginalia
;;     :ensure t
;;     :custom
;;     (marginalia-annotators
;;      '(marginalia-annotators-heavy marginalia-annotators-light nil))
;;     :init
;;     (marginalia-mode))
;;   ;; Enable vertico using the vertico-flat-mode
;;   (require 'vertico-directory)
;;   (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
;;   (vertico-mode t)
;;   :config
;;   ;; Used for the vertico-directory extension
;;   (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setu
;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t)))

;; (use-package magit
;;   :ensure t)

;; (use-package ediff
;;   :after (magit vc)
;;   :init
;;   ;; multiframe just doesn't make sense to me
;;   (with-eval-after-load 'winner
;;     (add-hook 'ediff-quit-hook 'winner-undo))
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; (use-package diff-hl
;;   :ensure t
;;   ;; :unless my/is-termux
;;   :defer 5
;;   :init (global-diff-hl-mode)
;;   :config
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   (diff-hl-flydiff-mode))


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
;; (global-set-key (kbd "M-/")  #'hippie-expand)


;; for explicitly completing using <tab>
;; (setq tab-always-indent 'complete)

;; (use-package corfu
;; 	:ensure t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 1)
;;   (corfu-auto-delay 0.0)
;;   (corfu-echo-documentation 0.25) ; Enable documentation for completions
;;   (corfu-preview-current 'insert) ; Do not preview current candidate
;;   (corfu-preselect-first t)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;;               ("M-SPC" . corfu-insert-separator)
;;               ("TAB"     . corfu-next)
;;               ([tab]     . corfu-next)
;;               ("S-TAB"   . corfu-previous)
;;               ([backtab] . corfu-previous)
;;               ("S-<return>" . nil)
;;               ("RET"     . corfu-insert) ;; leave my enter alone!
;; 							;; ("<escape>" . (lambda () (interactive) (corfu-quit) (evil-normal-state)))
;;               )
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   :config
;; 	;; exit with evil-escape
;; 	(advice-add 'evil-escape-func :after 'corfu-quit)
;;   (setq tab-always-indent 'complete)
;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;;               (corfu-mode))))
;; ;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
;; ;; fuzzy completion for corfu
;; (use-package orderless
;;   :ensure t
;;   :init
;;   ;; Tune the global completion style settings to your liking!
;;   ;; This affects the minibuffer and non-lsp completion at point.
;;   (setq completion-styles '(orderless partial-completion basic)
;;         completion-category-defaults nil
;;         completion-category-overrides nil))

;; (use-package company
;;   :ensure t
;;   :bind
;;   (:map company-active-map
;;         ;; ("<tab>" . company-complete-selection)
;; 				("RET" . nil)
;;                     ;; ("<escape>" . company-abort)
;;         )
;;   ;; (:map lsp-mode-map
;;   ;; ("<tab>" . company-indent-or-complete-common))
;;   :hook
;;   (prog-mode)
;;   :config
;;   ;; exit in evil normal mode
;;   (add-hook 'company-mode-hook
;;             (lambda ()
;;               (add-hook 'evil-normal-state-entry-hook
;;                         (lambda ()
;;                           (company-abort)))))
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous)
;;   ;; (company-keymap--unbind-quick-access company-active-map) ;; disable using M-number to select items
;;   ;; (company-tng-configure-default) ;; don't change the default tab behaviour
;;   (setq company-idle-delay 0.1)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   ;; invert the navigation direction if the the completion popup-isearch-match
;;   ;; is displayed on top (happens near the bottom of windows)
;;   (setq company-tooltip-flip-when-above t)
;;   ;; (global-company-mode)
;;   (with-eval-after-load 'company
;;     (define-key company-active-map (kbd "<return>") #'company-complete-selection))
;;   ;;        (add-hook 'after-init-hook 'global-company-mode) ;not only for programming mode
;;   ;;  :config
;;   ;;  (setq lsp-completion-provider :capf))
;;   )


;; (use-package company-web
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-web-html))

;; (use-package emmet-mode
;;   :ensure t
;;   :config
;;   (add-hook 'web-mode-hook 'emmet-mode)
;;   (add-hook 'js2-mode-hook 'emmet-mode))

(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; (add-hook 'flycheck-mode-hook
  ;; (lambda ()
  ;; (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
  ;; (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'lisp-mode 'flycheck-mode)
  (add-hook 'java-mode 'flycheck-mode)
  ;; (add-hook 'go-mode-hook 'flycheck-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
  )

;; (use-package web-mode
;;   :ensure t
;;   :config
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-code-indent-offset 2
;;         web-mode-indent-style 2))

;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

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

;; (use-package subword
;;   :ensure t
;;   :config (global-subword-mode 1))


;; needed packages
;; pip install ‘python-language-server[all]’
;; go get golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/cmd/goimports@latest

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
  (setq leetcode-directory "~/stuff/code/leetcode")
	)

;; git clone git@github.com:ginqi7/leetcode-emacs.git ~/.emacs.d/lisp/leetcode
;; npm install -g leetcode-cli

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; (global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "M-0") 'my/delete-window-and-rebalance)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(defun switch-to-flycheck-list-errors ()
  (interactive)
  (flycheck-list-errors)
  (pop-to-buffer "*Flycheck errors*"))
;; (global-set-key (kbd "C-c l") 'switch-to-flycheck-list-errors)
;; (global-set-key (kbd "M-j") 'next-buffer)
;; (global-set-key (kbd "M-k") 'previous-buffer)


;; (use-package undo-tree
;; 	:ensure t
;; 	:config
;; 	;; autosave the undo-tree history
;; 	(setq undo-tree-history-directory-alist
;; 				`((".*" . ,temporary-file-directory)))
;; 	(setq undo-tree-auto-save-history t)
;; 	(global-undo-tree-mode +1)
;; 	(evil-set-undo-system 'undo-tree)
;; 	)

;; ;; treesitter
;; (add-hook 'c++-mode-hook 'c++-ts-mode)
(add-hook 'java-mode-hook 'java-ts-mode)
;; (add-hook 'c-mode-hook 'c-ts-mode)
;; (add-hook 'python-mode-hook 'python-ts-mode)
