;; Configure evil config to have evil-leader
;; Since it can't reach it's function at runtime
;; Configure package.el to include MELPA.
(setq EMACS_DIR "~/.emacs.d/")
(setq user-init-file "~/.emacs.d/init.el")
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
                                        ;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
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


;; load path
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-evil)
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
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; Look and feel

(tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
;; startup messages
(setq inhibit-startup-message t)
(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)
;; (load-file "~/.emacs.d/organic-green-theme.el")
;; (load-theme 'organic-green t)
;; (set-background-color "white")
;; (set-foreground-color "black")
;; (set-cursor-color "black")
;; (use-package parchment-theme)
;; (global-display-line-numbers-mode 1)
;; Change mark region color
(set-face-attribute 'region nil :background "#ffff00")
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
(setq-default display-line-numbers-width-start t)
;; Highlight line (gui)
(when window-system (global-hl-line-mode nil))
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
(set-face-attribute 'default nil :font "Liberation Mono" :height 110)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Liberation Mono" :height 110)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Liberation Serif" :height 110 :weight 'regular)
(set-fontset-font "fontset-default" 'arabic (font-spec :family "Dejavu Sans Mono"))
(setq mhd-font-change-increment 1.1)


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
;; (global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-o") 'other-window)
(defun back-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "S-<return>") 'back-window)
(global-set-key (kbd "C-;") 'comment-line)
;; Disable ESC (C-g) that closes other splits

;; (define-key evil-normal-state-map (kbd "bracketLeft") 'other-window)
;; (define-key evil-normal-state-map (kbd "bracketLeft") 'other-window)
;; (define-key evil-normal-state-map (kbd "BackBra") 'back-window)
;; Leader key bindings



;; (define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
;; (define-key evil-normal-state-map (kbd "SPC S-w") 'save-buffer 'kill-current-buffer)
;; (defun mhd-save-buffer-and-close)
;; (global-set-key (kbd "SPC S-w") 'save-buffer)
;; (defun mhd-comment-at-the-end()
;; (comment-dwim)
;; (evil-insert)
;; )
;; (define-key global-map (kbd "M-;") 'mhd-comment-at-the-end)
(global-set-key (kbd "C-x C-;") 'comment-box)
;;(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;;(setq ido-create-new-buffer 'always)
(define-key input-decode-map "\e[1;2A" [S-up])
;; unbind C-x C-x for closing
(global-set-key (kbd "C-x C-c") nil)
;; (global-unset-key (kbd "C-x C-b"))

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
(defun mhd-split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(defun mhd-split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(defun mhd-delete-window-and-rebalance ()
  "Delete the current window, then rebalance the remaining windows."
  (interactive)
  (delete-window)
  (balance-windows))
(global-set-key (kbd "C-x 2") 'mhd-split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'mhd-split-window-right-and-switch)
(global-set-key (kbd "C-x 0") 'mhd-delete-window-and-rebalance)
(defun mhd-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the current or marked files.
If a buffer is not file and not dired, copy value of `default-directory'.
URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))
(defun mhd-window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun mhd-html-open-link-in-firefox (&optional @fullpath)
  "open url under cursor in Firefox browser.
Work in Windows, macOS. 2019-11-09 linux not yet.
Version 2019-11-09"
  (interactive)
  (let ($path)
    (if @fullpath
        (progn (setq $path @fullpath))
      (let (($inputStr
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let ($p0 $p1 $p2
                         ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                 (setq $p0 (point))
                 (skip-chars-backward $pathStops)
                 (setq $p1 (point))
                 (goto-char $p0)
                 (skip-chars-forward $pathStops)
                 (setq $p2 (point))
                 (goto-char $p0)
                 (buffer-substring-no-properties $p1 $p2)))))
        (setq $path (replace-regexp-in-string
                     "^file:///" "/"
                     (replace-regexp-in-string
                      ":\\'" "" $inputStr)))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a 'Firefox.app' \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "firefox" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "firefox \"%s\"" $path))))))

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; (defun config-reload ()
;; (interactive)
;; (eval-buffer (expand-file-name "~/.emacs.d/init.el")))
;; package-autoremove cleanup

;; electric paris for automatically closing brackets
(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            ))
(electric-pair-mode t)

;; Latex settings

;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :hook (LaTeX-mode . (lambda ()
;;                         (push (list 'output-pdf "Zathura")
;;                               TeX-view-program-selection))))

;; (use-package which-key
;;   :config (which-key-mode))

;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda () (evil-org-set-key-theme)))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))


;; Ido
;; (setq ido-enable-flex-matching nil)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (use-package ido-vertical-mode
;;   :ensure t
;;   :init
;;   (ido-vertical-mode 1))
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;; (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)


;; (use-package smex
;;   :ensure t
;;   :init (smex-initialize)
;;   :bind
;;   ("M-x" . smex))


;; (use-package avy
;;   :ensure t
;;   :bind*
;;   ("M-s" . evil-avy-goto-char-2))

(use-package rainbow-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

;; (use-package rainbow-delimiters
;;   :ensure t)


(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t))
;;   (yas-reload-all)) ;needed so you don't always refresh when adding your own
;; ;
(yas-global-mode 1)
;; (add-hook 'lua-mode-hook 'yas-minor-mode)
;; (add-hook 'c-mode-hook 'yas-minor-mode)
;; (add-hook 'python-mode-hook 'yas-minor-mode)
;; (add-hook 'elisp-mode-hook 'yas-minor-mode)
;; (add-hook 'org-mode-hook 'yas-minor-mode)
;; (add-hook 'c++-mode-hook 'yas-minor-mode)
                                        ; use yas-describe-tables to see what's available

;; (use-package popup-kill-ring
;;   :ensure t
;;   :bind ("M-y" . popup-kill-ring))

(use-package expand-region
  :ensure t)
;; :bind ("C-q" . er/expand-region))

(use-package ivy
  ;; :diminish
  :ensure t
  :bind (
         ;; ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-height 17)
  )
(ivy-mode)

(use-package counsel
  :ensure t
  :config
  ;; ("M-x" . 'counsel-M-x))

  ;; (define-key org-mode-map (kbd "C-c i") 'counsel-org-goto)
  ;; (global-set-key (kbd "C-c i") 'counsel-imenu))
  )



;; ;; ;; ;; temporarily highlight changes from yanking, etc
;; (use-package evil-goggles
;;   :ensure t
;;   :config
;;   (evil-goggles-mode)
;;   ;; optionally use diff-mode's faces; as a result, deleted text
;;   ;; will be highlighed with `diff-removed` face which is typically
;;   ;; some red color (as defined by the color theme)
;;   ;; other faces such as `diff-added` will be used for other actions
;;   ;; (setq evil-goggles-duration 0.100) ;; default is 0.200
;;   ;; Override evil googles colors
;;   ;; (evil-goggles-use-magit-faces)
;;   (custom-set-faces
;;    '(evil-goggles-default-face ((t (:inherit 'highlight))))) ;; default is to inherit 'region
;;   )

;; (use-package volatile-highlights
;;   :ensure t
;;   :config
;;   (volatile-highlights-mode t)
;;   ;; Supporting evil-mode.
;;   (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
;;                         'evil-paste-pop 'evil-move)
;;   (vhl/install-extension 'evil)
;;   ;; Supporting undo-tree.
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree)
;;   )


;; (use-package magit
;; :ensure t)

;; (use-package projectile
;;   ;; :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (when (file-directory-p "~/stuff/code")
;;     (setq projectile-project-search-path '("~/stuff/code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile
;;   :after projectile
;;   :config (counsel-projectile-mode))


(use-package ripgrep
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
(global-set-key (kbd "<f1>") 'toggle-input-method)

;; (use-package lsp-mode
;;   :ensure t
;;   :hook(
;;         (lsp-mode . lsp-enable-which-key-integration)
;;         (python-mode . #'lsp-defferred)
;;         (javascript-mode . #'lsp-defferred))
;;   :init
;;   (setq
;;    ;; lsp-keymap-prefix "C-c l"
;;    lsp-enable-file-watchers nil
;;    read-process-output-max (* 1024 1024)  ; 1 mb
;;    lsp-completion-provider :capf
;;    lsp-idle-delay 0.500
;;    )
;;   :config
;;   (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
;;   (setq lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; Automatically shutdown lsp server
;;   (setq lsp-keep-workspace-alive nil)
;;   (with-eval-after-load 'lsp-intelephense
;;     (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
;;   (setq lsp-eldoc-enable-hover t)
;;   (setq lsp-ui-doc-enable nil)
;;   :bind(:map lsp-mode-map
;;              ("C-c d" . lsp-describe-thing-at-point)
;;              ("C-c a" . lsp-execute-code-action))
;;   )

;; ;; java lsp
;; (use-package lsp-java
;;   :ensure t
;;   :config
;;   (add-hook 'java-mode-hook 'lsp)
;;   (use-package dap-java :ensure nil)
;;   (use-package dap-mode
;;     :after lsp-mode
;;     :config (dap-auto-configure-mode)))
;; ;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol);; if you are ivy user

;; (use-package lsp-ui
;;   :ensure t
;;   :after (lsp-mode)
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references))
;;   :init (setq lsp-ui-doc-delay 1.5
;;               lsp-ui-doc-position 'bottom
;;               lsp-ui-doc-max-width 100
;;               ))
;; ;; golang
;; (use-package go-mode
;;   :ensure t
;;   :hook ((go-mode . lsp))
;;   ;; (go-mode . company-mode)
;;   :bind (:map go-mode-map
;;               ("<f6>" . gofmt)
;;               ("C-c 6" . gofmt))
;;   :config
;;   (require 'lsp-go)
;;   ;; setting up some analyses
;;   ;; fieldalignment is good for alignment
;;   (setq lsp-go-analyses
;;         '((fieldalignment . t)
;;           (nilness . t)
;;           (unusedwrite . t)
;;           (unusedparams . t)))
;;   ;; ;; ;; GOPATH/go
;;   ;; (add-to-list 'exec-path "~/.local/share/go/bin")
;;   (setenv "GOPATH" (concat (getenv "HOME") "/.local/share/go/bin"))
;;   (setq gofmt-command "goimports"))
;; (global-set-key (kbd "<f5>") #'recompile)
;; (global-set-key (kbd "C-c s") 'flyspell-mode)
;; ;; python
;; (use-package python-mode
;;   :ensure t
;;   :hook (python-mode . lsp-deferred)
;;   :custom
;;   NOTE: Set these if Python 3 is called "python3" on your system!
;;   (python-shell-interpreter "python3")
;;   (dap-python-executable "python3")
;;   (dap-python-debugger 'debugpy)
;;   :config
;;   (require 'dap-python))

;; eglot
;; (use-package eglot
;;   :bind (:map eglot-mode-map
;;               ("<tab>" . company-complete)
;;               ("C-c l n" . flymake-goto-next-error)
;;               ("C-c l p" . flymake-goto-prev-error)
;;               ("C-c l r" . eglot-rename)
;;               ("C-c l f" . eglot-format)
;;               ("C-c l F" . eglot-format-buffer)
;;               ("C-c l a" . eglot-code-actions))
;;   :hook
;;   ((python-mode . eglot-ensure)
;;   ;;  ;; (c-mode . eglot-ensure)
;;   ;;  (LaTeX-mode . eglot-ensure)
;;   ;;  (c++-mode . eglot-ensure)
;;    (java-mode . eglot-ensure))
;;     ;; :config
;;     ;; (setcdr (assq 'java-mode eglot-server-programs)
;;     ;;         `("jdtls" "-data" "/home/user/.cache/emacs/workspace/"
;;     ;;           "-javaagent:/home/user/work/src/lombok.jar"
;;     ;;           "-Xbootclasspath/a:/home/user/work/src/lombok.jar"
;;     ;;           "--jvm-arg=-XX:+UseG1GC"
;;     ;;           "--jvm-arg=-XX:+UseStringDeduplication"
;;     ;;           "-Djava.format.settings.url=file:///home/user/code-format.xml"
;;     ;;           "-Djava.format.settings.profile=myown"))
;;     :custom
;;     ((eglot-autoshutdown t)))
;;   (use-package eglot-java)
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               (setq-local eglot-workspace-configuration
;;                           '((:java . ("org.eclipse.jdt.core.formatter.lineSplit"
;;                                       "40"))))))

(use-package quickrun
  :ensure t
  :bind ("C-c r" . quickrun))

;; (use-package company
;;   :ensure t
;;   :bind (:map company-active-map
;;               ("<tab>" . company-complete-selection)
;; 	      ;; ("<escape>" . company-abort)
;; 	      )
;;   ;; (:map lsp-mode-map
;;   ;; ("<tab>" . company-indent-or-complete-common))
;;   :config
;;   ;; exit in evil normal mode
;;   (add-hook 'company-mode-hook
;;    (lambda ()
;;      (add-hook 'evil-normal-state-entry-hook
;;                (lambda ()
;;                  (company-abort)))))
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous)
;;   (company-keymap--unbind-quick-access company-active-map) ;; disable using M-number to select items
;;   ;; (company-tng-configure-default) ;; don't change the default tab behaviour
;;   (setq company-idle-delay 0)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   ;; invert the navigation direction if the the completion popup-isearch-match
;;   ;; is displayed on top (happens near the bottom of windows)
;;   (setq company-tooltip-flip-when-above t)
;;   ;; (global-company-mode)
;;   (with-eval-after-load 'company
;;     (define-key company-active-map (kbd "<return>") #'company-complete-selection))
;;   (add-hook 'prog-mode-hook 'company-mode)
;;   ;;        (add-hook 'after-init-hook 'global-company-mode) ;not only for programming mode
;;   ;;  :config
;;   ;;  (setq lsp-completion-provider :capf)
;;   (diminish 'company-mode))
;; (use-package corfu
;;   ;; Optional customizations
;;   ;; :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :init
;;   (global-corfu-mode))

;; (use-package company-web
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-web-html))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'js2-mode-hook 'emmet-mode))

;; (use-package winum
;;   :ensure t
;;   :config
;;   (setq winum-keymap
;;         (let ((map (make-sparse-keymap)))
;;           ;; (define-key map (kbd "C-`") 'winum-select-window-by-number)
;;           ;; (define-key map (kbd "C-²") 'winum-select-window-by-number)
;;           (global-set-key (kbd "M-0") 'treemacs-select-window)
;;           (define-key map (kbd "M-1") 'winum-select-window-1)
;;           (define-key map (kbd "M-2") 'winum-select-window-2)
;;           (define-key map (kbd "M-3") 'winum-select-window-3)
;;           (define-key map (kbd "M-4") 'winum-select-window-4)
;;           (define-key map (kbd "M-5") 'winum-select-window-5)
;;           (define-key map (kbd "M-6") 'winum-select-window-6)
;;           (define-key map (kbd "M-7") 'winum-select-window-7)
;;           (define-key map (kbd "M-8") 'winum-select-window-8)
;;           map))
;; (defun winum-assign-6-to-calculator-8-to-flycheck-errors ()
;;   (cond
;;    ((equal (buffer-name) "*Calculator*") 9)
;;    ((equal (buffer-name) "*Flycheck errors*") 6)))
;; (add-to-list 'winum-assign-functions #'winum-assign-6-to-calculator-8-to-flycheck-errors)
;;   (winum-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
              (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))
  ;; (add-hook 'python-mode-hook 'flycheck-mode)
  ;; (add-hook 'go-mode-hook 'flycheck-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
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
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(use-package rainbow-mode
  :ensure t
  :hook web-mode)


;; Javascript
(use-package js2-mode
  :ensure t)
;; set as the default mode for javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(use-package js2-refactor
  :ensure t)

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

(setq-default indent-tabs-mode nil)

;; (defconst mhd-savefile-dir (expand-file-name "savefile" user-emacs-directory))
;; (use-package saveplace
;; :ensure t
;; :config
;; (setq save-place-file (expand-file-name "saveplace" mhd-savefile-dir))
;; activate it for all buffers
;; (setq-default save-place t))

(use-package subword
  :ensure t
  :config (global-subword-mode 1))

;; (fset 'mhd-star
;;       (kmacro-lambda-form [?A ?  ?\\ ?* escape] 0 "%X"))

;; Org settings
(setq org-export-with-sub-superscripts nil)
(setq org-pretty-entities nil)
(setq-default tab-width 2)
(setq org-export-coding-system 'utf-8)
;; (add-hook 'org-mode-hook 'org-indent-mode)
(setq org-adapt-indentation nil)
(setq coding-system-for-read 'utf-8 ) ; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(define-key global-map "\C-cl" 'org-store-link)
;; Insert mode whenever inserting a new heading (especiall useful with todo M-S-<return>)
(add-hook 'org-insert-heading-hook (apply-partially #'evil-insert 1))
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(use-package org
  :config
  (require 'org-tempo))
;; (setq initial-major-mode 'org-mode)
;; (setq org-hide-emphasis-markers t)
;; (use-package org-appear
;;   :hook (org-mode . org-appear-mode))
(setq org-pretty-entities t)
(setq org-startup-with-inline-images t
      org-image-actual-width '(600))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(add-to-list 'org-structure-template-alist
             '("el" . "src emacs-lisp"))
(setq org-adapt-indentation nil)
(setq org-directory "~/stuff/org")
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))
;; (setq org-inbox-file "~/sync/Dropbox/inbox.org")
(setq org-index-file (org-file-path "inbox.org"))
(setq org-archive-location
      (concat
       (org-file-path (format "archive/archive-%s.org" (format-time-string "%Y")))
       "::* From %s"))
(setq org-refile-targets `((,org-index-file :level . 1)
                           ;; (,(org-file-path "environment.org") :level . 1)
                           (,(org-file-path "inbox.org") :level . 1)
                           (,(org-file-path "goals.org") :level . 1)
                           (,(org-file-path "links.org") :level . 1)
                           (,(org-file-path "media.org") :level . 1)))
;; (,(org-file-path "someday-maybe.org") :level . 1)
;; (,(org-file-path "work.org") :level . 1)))
(setq org-agenda-files (list org-index-file
                             ;; (org-file-path "calendars")
                             (org-file-path "goals.org")
                             ;; (org-file-path "tasks.org")
                             (org-file-path "habits.org")
                             (org-file-path "news.org")
                             ;; (org-file-path "recurring-tasks.org")
                             ;; (org-file-path "work.org")
                             ))
(defun mhd-mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE, archive it, and
save the Org buffers."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree)
  (org-save-all-org-buffers))
(define-key org-mode-map (kbd "C-c C-x C-a") 'mhd-mark-done-and-archive)
(setq org-log-done 'time)
;; (setq org-log-done 'note)
(setq org-enforce-todo-dependencies t)
(defun mhd-org-add-tag (new-tag)
  (org-set-tags (cons new-tag
                      (seq-remove (lambda (tag)
                                    (get-text-property 0 'inherited tag))
                                  (org-get-tags)))))
(defun mhd-schedule-today ()
  "Tag this item with `daily'."
  (interactive)
  (mhd-org-add-tag "daily")
  (save-buffer))
(setq org-agenda-start-on-weekday nil)
;; (setq org-deadline-warning-days 0)
(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                 (todo . " %i ")
                                 (tags . " %i ")
                                 (search . " %i ")))
;; (setq org-agenda-dim-blocked-tasks 'invisible)
(defun org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
A block is identified as empty if there are fewer than 2
non-empty lines in the block (excluding the line with
`org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))
(add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)
(setq org-agenda-custom-commands '())
(add-to-list 'org-agenda-custom-commands
             '("p" "Personal agenda"
               ((tags-todo "plan"
                           ((org-agenda-overriding-header "Inbox")
                            (org-agenda-hide-tags-regexp "plan\\|daily\\|weekly\\|yearly")))
                (tags-todo "daily"
                           ((org-agenda-overriding-header "Today")
                            (org-agenda-hide-tags-regexp "habit\\|daily\\|weekly\\|yearly")))
                (tags-todo "habit-daily-weekly"
                           ((org-agenda-overriding-header "Habits")
                            (org-agenda-hide-tags-regexp "habit")))
                (tags-todo "weekly-daily"
                           ((org-agenda-overriding-header "This Week")
                            (org-agenda-hide-tags-regexp "weekly\\|yearly")))
                (agenda ""
                        (
                         ;; (org-agenda-overriding-header "Calendar")
                         (org-agenda-tag-filter-preset '("-daily"
                                                         "-habit"
                                                         "-weekly"
                                                         "-yearly"))
                         (org-agenda-hide-tags-regexp "daily\\|weekly\\|yearly")))
                (tags-todo "yearly-daily-weekly"
                           ((org-agenda-overriding-header "Annual Goals")
                            (org-agenda-hide-tags-regexp "yearly"))))
               ((org-agenda-skip-deadline-if-done t)
                (org-agenda-skip-scheduled-if-done t)
                (org-agenda-skip-timestamp-if-done t)
                (org-agenda-tag-filter-preset '("-duplicate" "-news" "-writing")))))
(defun mhd-org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
A block is identified as empty if there are fewer than 2
non-empty lines in the block (excluding the line with
`org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))
(add-hook 'org-agenda-finalize-hook #'mhd-org-agenda-delete-empty-blocks)
(defun mhd-dashboard ()
  (interactive)
  (call-process-shell-command "daily-checklist")
  (delete-other-windows)
  (find-file org-index-file)
  (org-agenda nil "p"))
;; (global-set-key (kbd "C-c d") 'mhd-dashboard)
(defadvice org-agenda-set-mode-name (after truncate-org-agenda-mode-name activate)
  (setq mode-name '("Org-agenda")))
(add-to-list 'org-agenda-custom-commands
             '("n" "News from this week"
               ((agenda ""))
               ((org-agenda-overriding-header "News from this week")
                (org-agenda-start-day "-6d")
                (org-agenda-span 14)
                (org-agenda-files '("~/stuff/org/news.org"
                                    ;; "~/stuff/org/recurring-events.org"
                                    "~/stuff/org/books-read.org"
                                    ;;                                         "~/stuff/org/papers-read.org"
                                    )))))
(add-to-list 'org-agenda-custom-commands
             '("w" "Writing prompts"
               ((tags "+writing"))
               ((org-agenda-overriding-header "Writing prompts")
                (org-agenda-sorting-strategy '((agenda ts-down))))))
(defvar org-capture-templates '())
(add-to-list 'org-capture-templates
             '("p" "Project idea"
               entry
               (file "~/stuff/org/project-idea.org")
               "* %?\n"))
(add-to-list 'org-capture-templates
             '("c" "Contact"
               entry
               (file "~/stuff/contacts.org")
               "* %(org-contacts-template-name"))
;; (add-to-list 'org-capture-templates
;;              '("d" "Delivery"
;;                entry
;;                (file+headline "~/stuff/org/deliveries.org" "Deliveries")
;;                "** %?\n   SCHEDULED: %t\n"))
;; (add-to-list 'org-capture-templates
;;              '("e" "Email"
;;                entry
;;                (file+headline org-index-file "Inbox")
;;                "* TODO %?\n%a\n"))
(add-to-list 'org-capture-templates
             '("b" "Books finished"
               entry
               (file+headline "~/stuff/org/books-read.org" "Books")
               "* %^{Title} -- %^{Author}\n** Summary\n%^{Summary}\n%t\n"))
;; (add-to-list 'org-capture-templates
;;              '("k" "Kookaburra ingest"
;;                entry
;;                (file+headline "~/stuff/org/kookaburra-ingest.org" "Queue")
;;                "* TODO %?\n"))
(add-to-list 'org-capture-templates
             '("m" "Media queue"
               item
               (file+headline "~/stuff/org/media.org" "Inbox")
               "- [ ] %?\n"))
(add-to-list 'org-capture-templates
             '("n" "News item"
               entry
               (file "~/stuff/org/news.org")
               "* %?\n%t\n"))
;; (add-to-list 'org-capture-templates
;;              '("p" "Finished paper"
;;                entry
;;                (file+headline "~/stuff/org/papers-read.org" "Papers")
;;                "* %^{Title} -- %^{Author}\n%t\n"))
(add-to-list 'org-capture-templates
             '("w" "Writing prompt"
               entry
               (file "~/stuff/org/journal.org")
               "* %?\n   %t\n"))
(add-to-list 'org-capture-templates
             '("s" "Subscribe to an RSS feed"
               plain
               (file "~/stuff/rss-feeds.org")
               "*** [[%^{Feed URL}][%^{Feed add}]]"))
(add-to-list 'org-capture-templates
             '("t" "Task"
               entry
               (file+headline org-index-file "Inbox")
               "* TODO %?\n"))
(add-to-list 'org-capture-templates
             '("Q" "Quote"
               entry
               (file "~/stuff/org/quotes.org")
               "* %?\n"))
;; (add-to-list 'org-capture-templates
;;              '("w" "Work task"
;;                entry
;;                (file+headline "~/stuff/org/work.org" "Tasks")
;;                "* TODO %?\n"))
;; Languages templates
(add-to-list 'org-capture-templates
             '("e" "English word"
               plain
               (file+headline "~/stuff/org/language.org" "English Words")
               "- %^{Word}: %^{Meaning}"))
(add-to-list 'org-capture-templates
             '("E" "English phrase"
               plain
               (file+headline "~/stuff/org/language.org" "English Phrases")
               "- %^{Phrase}: %^{Meaning}"))
(add-to-list 'org-capture-templates
             '("I" "Idioms"
               plain
               (file+headline "~/stuff/org/language.org" "Idioms")
               "- %^{Idiom}: %^{Meaning}"))
(add-to-list 'org-capture-templates
             '("f" "Farsi word"
               plain
               (file+headline "~/stuff/org/language.org" "Farsi Words")
               "- %^{Word}: %^{Meaning}"))
(add-to-list 'org-capture-templates
             '("F" "Farsi phrase"
               plain
               (file+headline "~/stuff/org/language.org" "Farsi Phrases")
               "- %^{Phrase}: %^{Meaning}"))
(add-to-list 'org-capture-templates
             '("a" "Arabic word"
               item
               (file+headline "~/stuff/org/language.org" "Arabic Words")
               "- %^{Word}"))
(add-to-list 'org-capture-templates
             '("A" "Arabic phrase"
               plain
               (file+headline "~/stuff/org/language.org" "Arabic Phrases")
               "- %^{Phrase}"))
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(defun mhd-index-file-open ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(setq mhd-diary-file (org-file-path "/diary/days.org"))
(defun mhd-diary-file-open()
  (interactive)
  ;; (setq filename (concat "~/stuff/org/diary/" (format-time-string "%Y-%m-%d-%H-%M) " ".org"))
  (find-file mhd-diary-file)
  (end-of-buffer)
  (insert (concat "* " (format-time-string "%Y.%m.%d %H:%M %P ") "\n"))
  (evil-insert-state)
  (flycheck-mode -1))

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))
(global-set-key (kbd "M-S-i") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-S-i") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-S-i") 'org-capture-todo)))
;; (defun mhd-open-work-file ()
;;   "Open the work TODO list."
;;   (interactive)
;;   (find-file (org-file-path "work.org"))
;;   (flycheck-mode -1)
;;   (end-of-buffer))
;; (global-set-key (kbd "C-c w") 'mhd-open-work-file)

(defun mhd-org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content))
           (message clipboard-url))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))
(define-key org-mode-map (kbd "C-c C-l") 'mhd-org-insert-link-dwim)

;; (require 'ox-md)
;; (require 'ox-beamer)
;; (use-package gnuplot)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((ditaa . t)
;;    (dot . t)
;;    (emacs-lisp . t)
;;    (gnuplot . t)
;;    (ruby . t)
;;    (shell . t)))
(setq org-confirm-babel-evaluate nil)
;; (use-package graphviz-dot-mode)
;; (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(setq org-export-with-smart-quotes t)
;; html syntax highlighting on export
;; (use-package htmlize)
(setq org-html-postamble nil)
;;tex in org
;; (setq org-latex-pdf-process
;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)
;; (add-to-list 'org-latex-logfiles-extensions "tex")
;; (add-to-list 'org-latex-classes
;;              '("book-noparts"
;;                "\\documentclass{book}"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; (setq TeX-parse-self t)
;; ;; (setq TeX-PDF-mode t)
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (LaTeX-math-mode)
;;             (setq TeX-master t)))
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (define-key LaTeX-mode-map (kbd "<f5>")
;;               (lambda ()
;;                 (interactive)
;;                 (compile "make")))))



;; :config
;; (use-package flx)

;; needed packages
;; pip install ‘python-language-server[all]’
;; go get golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/cmd/goimports@latest


;; speed-type for keyboard type testing
;; (use-package speed-type
;; :ensure t)


;; Treemacs
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                2000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           t
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     (treemacs-resize-icons 22)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (when treemacs-python-executable
;;       (treemacs-git-commit-diff-mode t))

;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))

;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

;; leetcode
;; (use-package leetcode
;;   :ensure t
;;   :hook
;;   (leetcode-solution-mode-hook .
;;                                (lambda() (flycheck-mode -1)))
;;   :config
;;   (setq leetcode-prefer-language "java")
;;   (setq leetcode-prefer-sql "mysql")
;;   (setq leetcode-save-solutions t)
;;   (setq leetcode-directory "~/stuff/code/leetcode")
;;   )

;; git clone git@github.com:ginqi7/leetcode-emacs.git ~/.emacs.d/lisp/leetcode
;; npm install -g leetcode-cli

;;; init-evil.el -- My evil mode configuration.
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; (defun mhd-split-ansi-term()
;;   (interactive)
;;   (split-window-vertically)
;;   (other-window 1)
;;   (ansi-term)
;;   (shrink-window 10))

;; (global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "M-0") 'mhd-delete-window-and-rebalance)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(defun switch-to-flycheck-list-errors ()
  (interactive)
  (flycheck-list-errors)
  (pop-to-buffer "*Flycheck errors*"))
(global-set-key (kbd "C-c l") 'switch-to-flycheck-list-errors)
(global-set-key (kbd "M-j") 'next-buffer)
(global-set-key (kbd "M-k") 'previous-buffer)

;; this function relies on the expand-region package
(defun mhd-org-empahsize ()
  (interactive)
  (if (not (use-region-p))
      (er/mark-word))
  (org-emphasize))

(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-last-sexp)
  (evil-leader/set-key-for-mode 'org-mode "i" 'counsel-org-goto)
  (evil-leader/set-key-for-mode 'org-mode "e" 'mhd-org-empahsize)
  (evil-leader/set-key-for-mode 'org-mode "A" 'mhd-mark-done-and-archive)
  (evil-leader/set-key-for-mode 'org-mode "/" 'org-sparse-tree)
  (evil-leader/set-key-for-mode 'html-mode "o" 'browse-url-of-buffer)
  (evil-leader/set-key
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    ;; "B"  'magit-blame-toggle
    ;; "f"  'helm-imenu            ;; Jump to function in buffer
    ;; "g"  'magit-status
    ;; "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'switch-to-flycheck-list-errors
    "L"  'whitespace-mode       ;; Show invisible characters
    ;; "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    ;; "nw" 'air-org-narrow-to-prose-dwim
    ;; "r"  'chrome-reload
    "E" 'eval-expression
    "d" 'dired-jump
    "S"  'delete-trailing-whitespace
    "u" 'undo-tree-visualize
    "w"  'save-buffer
    "mf" 'make-frame
    "W"  'write-file
    ;; "s"  'mhd-split-ansi-term
    "F" 'indent-buffer
    "f" 'find-file
    "v" 'find-alternate-file
    "b" 'switch-to-buffer
    "r" 'counsel-recentf
    "j" 'bookmark-jump
    "J" 'bookmark-set
    "k" 'kill-current-buffer
    "K" 'kill-buffer
    "P" 'project-switch-project
    "p" 'project-find-file
    "0" 'mhd-delete-window-and-rebalance
    "c" 'org-capture
    "i" 'counsel-imenu
    "D" 'mhd-diary-file-open
    "t" 'org-capture-todo
    "T" 'mhd-dashboard
    "H" 'mark-whole-buffer
    ;; "R" 'load-file user-init-file
    ))

;;   (defun magit-blame-toggle ()
;;     "Toggle magit-blame-mode on and off interactively."
;;     (interactive)
;;     (if (and (boundp 'magit-blame-mode) magit-blame-mode)
;;         (magit-blame-quit)
;;       (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(calendar-mode
                  ;; ag-mode
                  ;; custom-mode
                  ;; custom-new-theme-mode
                  dired-mode
                  eshell-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  ;; hugo-mode
                  ;; hugo-server-mode
                  ;; hugo-process-mode
                  ;; octopress-mode
                  ;; octopress-server-mode
                  ;; octopress-process-mode
                  org-capture-mode
                  ;; sunshine-mode
                  term-mode
                  ;; deadgrep-mode
                  ))
    (evil-set-initial-state mode 'emacs))

  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(twittering-edit-mode
                  magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'isearch-forward-regexp
    (kbd "?")       'isearch-backward-regexp
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    )

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "<backspace>") #'execute-extended-command)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key 'normal global-map (kbd "C-u")  'evil-scroll-up)
  (evil-define-key 'normal global-map (kbd "C-p")  'duplicate-line-or-region)
  (evil-define-key 'insert global-map (kbd "C-n")  'next-line)
  (evil-define-key 'insert global-map (kbd "C-p")  'previous-line)
  (evil-define-key 'insert global-map (kbd "C-e")     'end-of-line)
  (evil-define-key 'insert global-map (kbd "C-a")  'beginning-of-line)
  ;; (evil-define-key 'normal global-map (kbd "C-S-<backspace>")  'backward-kill-line)
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  ;; (evil-define-key 'normal global-map (kbd "-") 'helm-find-files)
  ;; (evil-define-key 'normal global-map (kbd "J")       'forward-paragraph)
  ;; (evil-define-key 'normal global-map (kbd "K")       'backward-paragraph)
  ;; (evil-define-key 'normal global-map (kbd "[")       'evil-backward-paragraph)
  ;; (evil-define-key 'normal global-map (kbd "[")       'evil-scroll-up)
  ;; (evil-define-key 'normal global-map (kbd "]")       'evil-scroll-down)
  ;; (evil-define-key 'normal global-map (kbd "C--")     'air-dired-buffer-dir-or-home)
  ;; (evil-define-key 'normal global-map (kbd "C-`")     (lambda ()
  ;;                                                       (interactive)
  ;;                                                       (dired (expand-file-name "~"))))
  (evil-define-key 'normal global-map (kbd "C-]")     'gtags-find-tag-from-here)
  (evil-define-key 'normal global-map (kbd "g/")      'occur-last-search)
  (evil-define-key 'normal global-map (kbd "[i")      'show-first-occurrence)
  ;; (evil-define-key 'normal global-map (kbd "S-SPC")   'air-pop-to-org-agenda-default)

  (evil-define-key 'normal global-map (kbd "1")     'delete-other-windows)
  (evil-define-key 'normal global-map (kbd "2")     'mhd-split-window-below-and-switch)
  (evil-define-key 'normal global-map (kbd "3")     'mhd-split-window-right-and-switch)
  (evil-define-key 'normal global-map (kbd "6")     'evil-buffer)
  (evil-define-key 'normal global-map (kbd "8")     'er/expand-region)
  ;; (evil-define-key 'normal global-map (kbd "9")     'xah-select-text-in-quote)
  ;; ("6" . xah-select-block)
  ;; ("7" . xah-select-line)
  ;; ("9" . xah-select-text-in-quote)
  (evil-define-key 'normal global-map (kbd "z d")     'dictionary-lookup-definition)
  ;; (evil-define-key 'normal global-map (kbd "\\ \\")   'tiny-menu)
  ;; (evil-define-key 'normal global-map (kbd "\\ a")    (tiny-menu-run-item "org-agendas"))
  ;; (evil-define-key 'normal global-map (kbd "\\ f")    (tiny-menu-run-item "org-files"))
  ;; (evil-define-key 'normal global-map (kbd "\\ t")    (tiny-menu-run-item "org-things"))
  ;; (evil-define-key 'normal global-map (kbd "\\ c")    (tiny-menu-run-item "org-captures"))
  ;; (evil-define-key 'normal global-map (kbd "\\ h")    (tiny-menu-run-item "org-personal-captures"))
  ;; (evil-define-key 'normal global-map (kbd "\\ l")    (tiny-menu-run-item "org-links"))
  ;; (evil-define-key 'normal global-map (kbd "C-=")     'sanityinc/increase-default-font-height)
  ;; (evil-define-key 'normal global-map (kbd "C--")     'sanityinc/decrease-default-font-height)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  ;; (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; My own Ex commands.
  ;; (evil-ex-define-cmd "hugo" 'hugo-status)
  ;; (evil-ex-define-cmd "om" 'octopress-status)
  )

(defun air--apply-evil-other-package-configs ()
  "Apply evil-dependent settings specific to other packages."

  ;; (defun next-conflict-marker ()
  ;;   (interactive)
  ;;   (evil-next-visual-line)
  ;;   (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
  ;;       (evil-previous-visual-line))
  ;;   (move-beginning-of-line nil))

  ;; (defun previous-conflict-marker ()
  ;;   (interactive)
  ;;   (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
  ;;   (move-beginning-of-line nil))

  ;; PHP
  ;; (evil-define-key 'normal php-mode-map (kbd "]n") 'next-conflict-marker)
  ;; (evil-define-key 'normal php-mode-map (kbd "[n") 'previous-conflict-marker)
  ;; (evil-define-key 'visual php-mode-map (kbd "]n") 'next-conflict-marker)
  ;; (evil-define-key 'visual php-mode-map (kbd "[n") 'previous-conflict-marker)

  ;; Dired
  (evil-define-key 'normal dired-mode-map (kbd "C-e") 'dired-toggle-read-only))

(defmacro define-evil-or-global-key (key def &optional state)
  "Define a key KEY with DEF in an Evil map, or in the global map.
If the Evil map for STATE is defined (or `normal' if STATE is not
provided) the key will be defined in that map.  Failing that, it will
be defined globally.
Note that STATE should be provided as an unquoted symbol.
This macro provides a way to override Evil mappings in the appropriate
Evil map in a manner that is compatible with environments where Evil
is not used."
  (let* ((evil-map-name (if state
                            (concat "evil-" (symbol-name state) "-state-map")
                          "evil-normal-state-map"))
         (map (if (boundp (intern evil-map-name))
                  (intern evil-map-name)
                global-map)))
    `(define-key ,map ,key ,def)))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1)
  ;; (diminish 'undo-tree-mode)
  )

;; It should be before evil to work in buffers like scratch and others
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (air--config-evil-leader))


(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :init
  ;; (setq evil-want-C-i-jump nil) ;; preserves org <tab> cycling
  ;; for evil-collection
  ;; (setq evil-want-C-u-scroll t)
  ;; (setq evil-want-Y-yank-to-eol t)
  (evil-set-undo-system 'undo-tree)
  ;; (setq evil-want-keybinding nil)
  :config
  (setq evil-disable-insert-state-bindings t)
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1))

;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda () (evil-org-set-key-theme)))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))


;; (use-package undo-fu
;;   :ensure t)


(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(air--apply-evil-other-package-configs)

;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :config
;;   (setq evil-collection-mode-list
;;         '(
;;           ;; deadgrep
;;           dired
;;           ibuffer
;;           magit
;;           ;; elfeed
;;           ;; mu4e
;;           ;; which-key)
;;           ))
;;   (evil-collection-init))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))
(defun mhd-kids-log-enter ()
  (interactive)
  (append-to-file (format-time-string "Entered: %Y-%m-%d-%H:%M:%S\n") nil "~/stuff/kids-log"))
(defun mhd-kids-log-leave ()
  (interactive)
  (append-to-file (format-time-string "Left: %Y-%m-%d-%H:%M:%S\n") nil "~/stuff/kids-log"))
