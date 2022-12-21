(use-package lsp-mode
  :ensure t
  ;; corfu stuff
  ;; :custom
  ;; (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook(
        (lsp-mode . lsp-enable-which-key-integration)
        (c++-mode . #'lsp-defferred)
        ;; (javascript-mode . #'lsp-defferred))
        ;; for corfu
        ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
	      )
  :init
  (setq
   ;; lsp-keymap-prefix "C-c l"
   lsp-enable-file-watchers nil
   read-process-output-max (* 1024 1024)  ; 1 mb
   lsp-completion-provider :capf
   lsp-idle-delay 0.500
   )
  :config
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  ;; Automatically shutdown lsp server
  (setq lsp-keep-workspace-alive nil)
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-ui-doc-enable nil)
  :bind(:map lsp-mode-map
             ("C-c d" . lsp-describe-thing-at-point)
             ("C-c a" . lsp-execute-code-action))
  )

;; java lsp
;; (use-package lsp-java
;; 	:ensure t
;; 	:hook (java-mode . lsp-deferred)
;; 	:config
;; 	(use-package dap-java :ensure nil)
;; 	(use-package dap-mode
;; 	  :after lsp-mode
;; 	  :config (dap-auto-configure-mode)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
              lsp-ui-doc-position 'bottom
              lsp-ui-doc-max-width 100
              ))
;; golang
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

;; needed packages
;; pip install ‘python-language-server[all]’
;; go get golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/cmd/goimports@latest

;; ;; python
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(provide 'lsp-stuff)
