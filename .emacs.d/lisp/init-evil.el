;; this function relies on the expand-region package
(defun my/org-empahsize ()
  (interactive)
  (if (not (use-region-p))
      (er/mark-word))
  (org-emphasize))

(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-last-sexp)
  ;;(evil-leader/set-key-for-mode 'org-mode "i" 'counsel-org-goto)
  (evil-leader/set-key-for-mode 'org-mode "e" 'my/org-empahsize)
  (evil-leader/set-key-for-mode 'org-mode "A" 'my/mark-done-and-archive)
  (evil-leader/set-key-for-mode 'org-mode "/" 'org-sparse-tree)
  (evil-leader/set-key-for-mode 'web-mode "o" 'browse-url-of-buffer)
  (evil-leader/set-key
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    ;; "B"  'magit-blame-toggle
    ;; "f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    ;; "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'consult-flymake
    "L"  'switch-to-flycheck-list-errors
    ;; "L"  'whitespace-mode       ;; Show invisible characters
    ;; "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    ;; "nw" 'air-org-narrow-to-prose-dwim
    ;; "r"  'chrome-reload
    "E" 'eval-expression
    "y" 'consult-yank-pop
    "y" 'consult-yank-pop
		"G" 'consult-ripgrep
		"R" 'find-file-root
    "d" 'dired-jump
    "S"  'delete-trailing-whitespace
		"s" 'shell
    "u" 'undo-tree-visualize
    ;; "w"  'save-buffer
    "M" 'make-frame
    ;; "W"  'write-file
    "O"  'browse-url-xdg-open
    ;; "s"  'my/split-ansi-term
    "F" 'indent-buffer
    "f" 'find-file
    "v" 'find-alternate-file
    "b" 'switch-to-buffer
    "r" 'consult-recent-file
    "j" 'bookmark-jump
    "J" 'bookmark-set
    "k" 'kill-current-buffer
    "K" 'kill-buffer
    "P" 'project-switch-project
    "p" 'project-find-file
    "0" 'my/delete-window-and-rebalance
    "c" 'org-capture
    "i" 'consult-imenu
    "D" 'my/diary-file-open
    "t" 'org-capture-todo
    "T" 'my/dashboard
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

	;; quit corfu in normal mode
	;; (add-hook 'evil-normal-state-entry-hook 'corfu-quit)

  "Configure evil mode."
  ;; Use Emacs state in these additional modes.
  (dolist (mode '(calendar-mode
                  ;; ag-mode
                  ;; custom-mode
                  ;; custom-new-theme-mode
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
  ;; (evil-define-key 'insert global-map (kbd "C-n")  'next-line)
  ;; (evil-define-key 'insert global-map (kbd "C-p")  'previous-line)
  ;; (evil-define-key 'insert global-map (kbd "C-e")     'end-of-line)
  ;; (evil-define-key 'insert global-map (kbd "C-a")  'beginning-of-line)
  ;; (evil-define-key 'normal global-map (kbd "C-S-<backspace>")  'backward-kill-line)
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "C-]")     'gtags-find-tag-from-here)
  (evil-define-key 'normal global-map (kbd "g/")      'occur-last-search)
  (evil-define-key 'normal global-map (kbd "[i")      'show-first-occurrence)
  (evil-define-key 'normal global-map (kbd "z d")     'dictionary-lookup-definition)

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
  ;; (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  )

(defun air--apply-evil-other-package-configs ()
  "Apply evil-dependent settings specific to other packages."

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

;; It should be before evil to work in buffers like scratch and others
;; we should place evil variables in this function since it fires up evil
(use-package evil-leader
  :ensure t
  :init
  ;; for evil-collection
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
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
  ;; (setq evil-want-keybinding nil)
  :config
  (setq evil-disable-insert-state-bindings t)
  (add-hook 'evil-mode-hook 'air--config-evil)
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

(air--apply-evil-other-package-configs)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list
        '(
          ;; deadgrep
          dired
          ibuffer
          magit
          ;; elfeed
          ;; mu4e
          which-key
          ))
  (evil-collection-init))

(use-package evil-nerd-commenter
	:ensure t
	:after evil
	:config
	(evilnc-default-hotkeys t)
	)

;; unbind C-x C-x for closing, and C-x C-z for suspending
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))

(add-hook 'org-insert-heading-hook (apply-partially #'evil-insert 1))
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; corfu exit with evil-escape
(advice-add 'evil-escape-func :after 'corfu-quit)
(setq tab-always-indent 'complete)

(provide 'init-evil)
