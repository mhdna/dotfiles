(defun set-keys ()

  ;; https://github.com/emacs-evil/evil-collection#key-translation

  (defvar my-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my-intercept-mode)

  (dolist (state											 '(normal visual insert))
    (evil-make-intercept-map
     ;; NOTE: This requires an evil version from 2018-03-20 or later
     (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
     state))
  (evil-define-key										 '(normal visual) lisp-mode-map
    (kbd "SPC E")											 'eval-expression)

  (evil-define-key										 '(insert) my-intercept-mode-map
    (kbd "M-SPC")												 'nil
    (kbd "C-u")												 'kill-start-of-line)

  (evil-define-key										 '(normal visual) my-intercept-mode-map
    (kbd "M-u")												 'universal-argument
    (kbd "M-S-u")												 'negative-argument
    ;; (kbd "C-]")										 'gtags-find-tag-from-here
    ;; (kbd "C-p")												 'duplicate-line-or-region
    (kbd "z d")												 'dictionary-lookup-definition
    ;; fix gj, gk when using evil-org-mode
    (kbd "gj")												 'evil-next-visual-line
    (kbd "gk")												 'evil-previous-visual-line
    (kbd "C-SPC")												 'toggle-input-method

    ;; Leader keybindings
    ;; I use SPC instead of setting up <leader> because otherwise my-intercept-mode-map won't work, and things like <leader>al won't either
    (kbd "SPC al")										 'align-regexp
    (kbd "SPC a=")										 'my-align-single-equals
    (kbd "SPC aa")										 'my/agenda
    (kbd "SPC B")											 'magit-blame-toggle
    (kbd "SPC g")											 'magit-status
    (kbd "SPC l")											 'consult-flymake
    (kbd "SPC y")											 'consult-yank-pop
    (kbd "SPC y")											 'consult-yank-pop
    (kbd "gs")											 'consult-ripgrep
    (kbd "SPC R")											 'find-file-root
    (kbd "M-o")                        'dired-jump
    (kbd "SPC S")											 'delete-trailing-whitespace
    (kbd "SPC u")											 'undo-tree-visualize
    (kbd "SPC M")											 'make-frame
    (kbd "SPC W")											 'write-file
    (kbd "SPC O")											 'browse-url-xdg-open
    ;; (kbd "SPC s")											 'my/split-term
    (kbd "SPC s")											 'my/chatgpt-switch-or-open
    (kbd "SPC f")                        'find-file
    (kbd "SPC v")											 'find-alternate-file
    (kbd "SPC b")											 'switch-to-buffer
    (kbd "SPC r")											 'consult-recent-file
    (kbd "SPC j")											 'bookmark-jump
    (kbd "SPC J")											 'my/bookmark-set
    ;; (kbd "SPC K")                        'kill-current-buffer
    (kbd "SPC k")											 'my/close-current-buffer
    (kbd "C-S-T")											 'my/open-last-closed
    (kbd "SPC P")											 'project-switch-project
    (kbd "SPC p")											 'project-find-file
    (kbd "SPC c")											 'org-capture
    (kbd "SPC C")											 'calc
    (kbd "SPC i")											 'consult-imenu
    (kbd "SPC d")											 'my/diary-file-open
    (kbd "SPC D")											 'my/journal-file-open
    (kbd "SPC t")											 'org-capture-todo
    (kbd "SPC n")											 'my/notes-open
    (kbd "SPC R")                      '(lambda () (interactive) (load-file user-init-file))
    (kbd "SPC H")											 'mark-whole-buffer)
    ;; (kbd "/")                          'isearch-forward
    ;; (kbd "?")                           'isearch-backward)


  (evil-define-key										 '(normal visual) org-mode-map
    (kbd "SPC /")											 'org-sparse-tree
    (kbd "SPC e")											 'my/org-empahsize
    (kbd "SPC A")											 'my/mark-done-and-archive
    (kbd "SPC i")											 'my/org-goto
    (kbd "SPC E")											 'org-export-dispatch
    (kbd "SPC xi")										 'org-clock-in
    (kbd "SPC xo")										 'org-clock-out
    (kbd "SPC xx")										 'org-clock-display)

  (evil-define-key										 '(normal visual) web-mode-map
    (kbd "SPC o")											 'browse-url-of-buffer)

  (evil-define-key										 '(normal visual) emacs-lisp-mode-map
    (kbd "SPC e")											 'eval-last-sexp
    (kbd "(")																 'evil-previous-open-paren
    (kbd ")")																 'evil-next-close-paren))

;; Global Bindings
;; (global-set-key (kbd "M-o")						 'evil-window-next)
(global-set-key (kbd "C-w")						 'backward-kill-word)
;; M-[0-4] are especially annoying if they close windows or open new splits if envoked with universal-argument prefix
(global-unset-key (kbd "M-0"))
(global-unset-key (kbd "M-1"))
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(global-unset-key (kbd "M-4"))
(global-unset-key (kbd "M-5"))
(global-unset-key (kbd "M-6"))
(global-unset-key (kbd "M-7"))
(global-unset-key (kbd "M-8"))
(global-unset-key (kbd "M-9"))

;; quit everything with escape whenever possible.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key minibuffer-local-map [escape]							 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-SPC")							 'toggle-input-method)
(define-key minibuffer-local-ns-map [escape]					 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape]	 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape]	 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape]			 'minibuffer-keyboard-quit)


(defun config-evil ()

  ;; quit corfu in normal mode
  ;; (add-hook 'evil-normal-state-entry-hook 'corfu-quit)

  "Configure evil mode."
  ;; Use Emacs state in these additional modes.
  (dolist (mode '(
                  ;; calendar-mode
                  ;; ag-mode
                  ;; dired-mode
                  eshell-mode
                  git-rebase-mode
                  image-mode
                  ;; hugo-mode
                  ;; hugo-server-mode
                  ;; hugo-process-mode
                  ;; octopress-mode
                  ;; octopress-server-mode
                  ;; octopress-process-mode
                  ;; org-capture-mode
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

  (add-to-list 'evil-buffer-regexps '("\\*Flymake ")))

(use-package evil
  :init
  ;; (setq evil-want-C-i-jump nil) ;; preserves org <tab> cycling
  ;; for evil-collection
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
    ;; (setq evil-search-module 'isearch)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  ;; (setq evil-disable-insert-state-bindings t)
  ;; (setq evil-undo-system 'undo-redo)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  :config
  (add-hook 'evil-mode-hook 'set-keys)
  (add-hook 'evil-mode-hook 'config-evil)
  (evil-mode 1)
  :config
  ;; (evil-set-leader 'normal " ")
  (when (package-installed-p 'corfu)
    ;; corfu exit with evil-escape
    (advice-add 'evil-escape-func :after 'corfu-quit)
    (evil-make-overriding-map corfu-map)
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))
  :bind
  (:map evil-normal-state-map
              ;; vim vinigar style
              ("-"  . (lambda () (interactive)
                        (dired ".")))
              ;; ("C-s" . consult-line)
              ;; ("<leader>/" . evil-ex-nohighlight)
              ;; ("C-n" . evil-next-line)
              ;; ("C-p" . evil-previous-line))
  ))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        '(
          calendar
          calc
          dired
          info
          help
          ibuffer
          magit
          corfu
          doc-view
          eshell
          ripgrep
          rg
          flymake
          flycheck
          dictionary
          compile
          eldoc
          image
          ibuffer
          man
          pdf
          ))
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys t))

;; Automatic insert-mode for certain hooks
(add-hook 'org-insert-heading-hook (apply-partially #'evil-insert 1))
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq tab-always-indent 'complete)

;; set default undo-tree system if installed
(when (package-installed-p 'undo-tree)
  (evil-set-undo-system 'undo-tree))

(provide 'init-evil)
