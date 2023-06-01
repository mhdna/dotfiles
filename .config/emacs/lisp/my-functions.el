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

(defun my/split-window-below-and-switch ()
	"Split the window horizontally, then switch to the new pane."
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))

(defun my/split-window-right-and-switch ()
	"Split the window vertically, then switch to the new pane."
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))

(defun my/delete-window-and-rebalance ()
	"Delete the current window, then rebalance the remaining windows."
	(interactive)
	(delete-window)
	(balance-windows))

(defun my/indent-buffer ()
	(interactive)
	(save-excursion
		(indent-region (point-min) (point-max) nil)))

(defun my/copy-file-path (&optional DirPathOnlyQ)
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
(defun my/window-split-toggle ()
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

(defun my/html-open-link-in-firefox (&optional @fullpath)
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
	(add-hook 'kill-buffer-query-functions 'my/kill-scratch-buffer))

(defun my/kill-scratch-buffer ()
	;; The next line is just in case someone calls this manually
	(set-buffer (get-buffer-create "*scratch*"))
	;; Kill the current (*scratch*) buffer
	(remove-hook 'kill-buffer-query-functions 'my/kill-scratch-buffer)
	(kill-buffer (current-buffer))
	;; Make a brand new *scratch* buffer
	(set-buffer (get-buffer-create "*scratch*"))
	(lisp-interaction-mode)
	(make-local-variable 'kill-buffer-query-functions)
	(add-hook 'kill-buffer-query-functions 'my/kill-scratch-buffer)
	;; Since we killed it, don't let caller do that.
	nil)

;; (defun my/config-reload ()
;; (interactive)
;; (eval-buffer (expand-file-name "~/.emacs.d/init.el")))

(defun my/split-term()
	(interactive)
	(split-window-vertically)
	(other-window 1)
	(eshell)
	;; (shrink-window 10)
	)

(defun my/kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer
				(delq (current-buffer)
							(remove-if-not 'buffer-file-name (buffer-list)))))

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
	"*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
	"History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
	"Normal hook for functions to run after finding a \"root\" file.")

(defun my/find-file-root ()
	"*Open a file as the root user.
	 Prepends `find-file-root-prefix' to the selected file name so that it
	 maybe accessed via the corresponding tramp method."

	(interactive)
	(require 'tramp)
	(let* ( ;; We bind the variable `file-name-history' locally so we can
				 ;; use a separate history list for "root" files.
				 (file-name-history find-file-root-history)
				 (name (or buffer-file-name default-directory))
				 (tramp (and (tramp-tramp-file-p name)
										 (tramp-dissect-file-name name)))
				 path dir file)

		;; If called from a "root" file, we need to fix up the path.
		(when tramp
			(setq path (tramp-file-name-localname tramp)
						dir (file-name-directory path)))

		(when (setq file (read-file-name "Find file (UID = 0): " dir path))
			(find-file (concat find-file-root-prefix file))
			;; If this all succeeded save our new history list.
			(setq find-file-root-history file-name-history)
			;; allow some user customization
			(run-hooks 'find-file-root-hook))))

(defun my/which (filename)
	(find-file (executable-find filename)))

(provide 'my-functions)
