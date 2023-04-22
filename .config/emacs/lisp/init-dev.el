;; (use-package company
;;	:ensure t
;;	:config
;;	(add-hook 'after-init-hook 'global-company-mode)
;;	;; (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
;;	;; disable non-smart completion
;;	(delete 'company-dabbrev company-backends)
;;	(setq company-idle-delay 0
;;				company-require-match nil
;;				company-tooltip-limit 10
;;				company-tooltip-align-annotations t
;;				company-dabbrev-downcase nil
;;				company-show-quick-access t)
;;	(eval-after-load 'company
;;		'(progn
;;			 (define-key company-active-map (kbd "TAB") 'company-complete-selection)
;;			 (define-key company-active-map [tab] 'company-complete-selection)
;;			 (unbind-key "RET" company-active-map)
;;			 (unbind-key "<return>" company-active-map))))

;; ;; popup documentation for quick help for company
;; (use-package company-quickhelp
;;	:ensure t
;;	:config
;;	(company-quickhelp-mode))

;; (use-package tree-sitter
;;	:ensure t
;;   :config
;;   (global-tree-sitter-mode 1))
;; (use-package tree-sitter-langs)

;; web stuff
(use-package web-mode
	:ensure t
	:config
	(setq web-mode-markup-indent-offset 2
				web-mode-css-indent-offset 2
				web-mode-code-indent-offset 2
				web-mode-indent-style 2)
	(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode)))

(use-package emmet-mode
	:ensure t
	:config
	(add-hook 'web-mode-hook 'emmet-mode))

(provide 'init-dev)
