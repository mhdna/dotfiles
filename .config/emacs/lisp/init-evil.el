(defun set-keys ()
	;; (setq leader "SPC")
	
	;; (evil-define-key '(insert) global-map
		;; (kbd "C-n")							 'next-line
		;; (kbd "C-p")							 'previous-line
		;; (kbd "C-e")							 'end-of-line
		;; (kbd "C-a")							 'beginning-of-line
		;; (kbd "<down>")												 'evil-next-visual-line
		;; (kbd "<up>")													 'evil-previous-visual-line
		;; )

	(evil-define-key '(normal visual) global-map
		(kbd "C-]")					 'gtags-find-tag-from-here
		(kbd "C-p")					 'duplicate-line-or-region
		(kbd "z d")					 'dictionary-lookup-definition

		;; Leader keybinds
		(kbd "SPC aa")			 'align-regexp
		(kbd "SPC a=")			 'my-align-single-equals
		(kbd "SPC B")				 'magit-blame-toggle
		(kbd "SPC g")				 'magit-status
		;; (kbd "SPC h")		 'fontify-and-browse    ;; HTML-ize the buffer and browse the result
		(kbd "SPC l")				 'consult-flymake
		(kbd "SPC L")				 'whitespace-mode       ;; Show invisible characters
		(kbd "SPC E")				 'eval-expression
		(kbd "SPC y")				 'consult-yank-pop
		(kbd "SPC y")				 'consult-yank-pop
		(kbd "SPC G")				 'consult-ripgrep
		(kbd "SPC R")				 'find-file-root
		(kbd "SPC d")				 'dired-jump
		(kbd "SPC S")				 'delete-trailing-whitespace
		(kbd "SPC s")				 'shell
		(kbd "SPC u")				 'undo-tree-visualize
		;; (kbd "SPC w")		 'save-buffer
		(kbd "SPC M")				 'make-frame
		;; (kbd "SPC W")		 'write-file
		(kbd "SPC O")				 'browse-url-xdg-open
		;; (kbd "SPC s")		 'my/split-ansi-term
		(kbd "SPC F")				 'my/indent-buffer
		(kbd "SPC f")				 'find-file
		(kbd "SPC v")				 'find-alternate-file
		(kbd "SPC b")				 'switch-to-buffer
		(kbd "SPC r")				 'consult-recent-file
		(kbd "SPC j")				 'bookmark-jump
		(kbd "SPC J")				 'bookmark-set
		(kbd "SPC k")				 'kill-current-buffer
		(kbd "SPC K")				 'kill-buffer
		(kbd "SPC P")				 'project-switch-project
		(kbd "SPC p")				 'project-find-file
		(kbd "SPC 0")				 'my/delete-window-and-rebalance
		(kbd "SPC c")				 'org-capture
		(kbd "SPC i")				 'consult-imenu
		(kbd "SPC D")				 'my/diary-file-open
		(kbd "SPC t")				 'org-capture-todo
		(kbd "SPC T")				 'my/agenda
    (kbd "SPC H")				 'mark-whole-buffer
		;; (kbd "SPC R")		 'load-file user-init-file
)
	
	(evil-define-key '(normal visual) org-mode-map
		(kbd "SPC /") 'org-sparse-tree
		(kbd "SPC e") 'my/org-empahsize
		(kbd "SPC A") 'my/mark-done-and-archive
		(kbd "SPC i")				 'my/org-goto
		)

	(evil-define-key '(normal visual) web-mode-map
		(kbd "SPC o") 'browse-url-of-buffer)

	(evil-define-key '(normal visual) emacs-lisp-mode-map
		(kbd "SPC e") 'eval-last-sexp
		)

	(evil-define-key 'normal dired-mode-map
		(kbd "C-e") 'dired-toggle-read-only)

	(evil-define-key 'normal flycheck-mode-map
		(kbd "M-n") 'flycheck-next-error
		(kbd "M-p") 'flycheck-previous-error)
	)

;; Global Bindings
(define-key global-map (kbd "M-u")										 'universal-argument)
(define-key global-map (kbd "M-S-u")									 'negative-argument)
(define-key global-map (kbd "M-0")										 'my/delete-window-and-rebalance)
(define-key global-map (kbd "M-1")										 'delete-other-windows)
(define-key global-map (kbd "M-2")										 'my/split-window-below-and-switch)
(define-key global-map (kbd "M-3")										 'my/split-window-right-and-switch)

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
(define-key minibuffer-local-ns-map [escape]					 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape]	 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape]	 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape]			 'minibuffer-keyboard-quit)


(defun config-evil ()

	;; quit corfu in normal mode
	;; (add-hook 'evil-normal-state-entry-hook 'corfu-quit)

	"Configure evil mode."
	;; Use Emacs state in these additional modes.
	(dolist (mode '(calendar-mode
									;; ag-mode
									;; dired-mode
									eshell-mode
									flycheck-error-list-mode
									git-rebase-mode
									image-mode
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
	)

(use-package evil
	:ensure t
	:init
	;; (setq evil-want-C-i-jump nil) ;; preserves org <tab> cycling
	;; for evil-collection
	(setq evil-want-C-u-scroll t)
	(setq evil-want-Y-yank-to-eol t)
	(setq evil-want-keybinding nil)
	:config
	(setq evil-disable-insert-state-bindings t)
	(add-hook 'evil-mode-hook 'set-keys)
	(add-hook 'evil-mode-hook 'config-evil)
	(evil-mode 1)
	)

(use-package evil-org
	:ensure t
	:after org
	:config
	(add-hook 'org-mode-hook 'evil-org-mode)
	(add-hook 'evil-org-mode-hook
						(lambda () (evil-org-set-key-theme)))
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))


(use-package evil-surround
	:ensure t
	:config
	(global-evil-surround-mode))

(use-package evil-collection
	:ensure t
	:after evil
	:config
	(setq evil-collection-mode-list
				'(
					dired
					ibuffer
					magit
					;; elfeed
					;; mu4e
					;; which-key
					))
	(evil-collection-init))

(use-package evil-nerd-commenter
	:ensure t
	:after evil
	:config
	(evilnc-default-hotkeys t)
	)

(add-hook 'org-insert-heading-hook (apply-partially #'evil-insert 1))
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; corfu exit with evil-escape
;; (advice-add 'evil-escape-func :after 'corfu-quit)
;; (setq tab-always-indent 'complete)

(provide 'init-evil)
