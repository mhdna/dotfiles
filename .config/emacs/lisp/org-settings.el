;; Org settings
(add-hook 'org-mode-hook 'flyspell-mode)
(setq org-export-with-sub-superscripts nil)
(setq org-pretty-entities nil)
(setq-default tab-width 2)
(setq org-export-coding-system 'utf-8)
;; set default unprioritized tasks to #C instead of #B
(setq org-default-priority ?C)
;; (add-hook 'org-mode-hook 'org-indent-mode)
(setq org-adapt-indentation nil)
(setq coding-system-for-read 'utf-8 ) ; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
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
(setq org-directory "~/personal/notes/")
(defun org-file-path (filename)
	"Return the absolute address of an org file, given its relative name."
	(concat (file-name-as-directory org-directory) filename))
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
			(concat
			 (org-file-path (format "archive/archive-%s.org" (format-time-string "%Y")))
			 "::* From %s"))
(setq org-refile-targets `((,org-index-file :level . 1)
													 ;; (,(org-file-path "environment.org") :level . 1)
													 (,(org-file-path "index.org") :level . 1)
													 ;; (,(org-file-path "goals.org") :level . 1)
													 (,(org-file-path "links.org") :level . 1)
													 (,(org-file-path "media.org") :level . 1)))
;; (,(org-file-path "someday-maybe.org") :level . 1)
;; (,(org-file-path "work.org") :level . 1)))
(setq org-agenda-files (list org-index-file
														 ;; (org-file-path "calendars")
														 ;; (org-file-path "goals.org")
														 ;; (org-file-path "tasks.org")
														 (org-file-path "habits.org")
														 (org-file-path "news.org")
														 ;; (org-file-path "recurring-tasks.org")
														 ;; (org-file-path "work.org")
														 ))
(defun my/mark-done-and-archive ()
	"Mark the state of an org-mode item as DONE, archive it, and
save the Org buffers."
	(interactive)
	(org-todo 'done)
	(org-archive-subtree)
	(org-save-all-org-buffers))
(setq org-log-done 'time)
;; (setq org-log-done 'note)
(setq org-enforce-todo-dependencies t)
(defun my/org-add-tag (new-tag)
	(org-set-tags (cons new-tag
											(seq-remove (lambda (tag)
																		(get-text-property 0 'inherited tag))
																	(org-get-tags)))))
(defun my/schedule-today ()
	"Tag this item with `daily'."
	(interactive)
	(my/org-add-tag "daily")
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
													 ((org-agenda-overriding-header "Index")
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
(defun my/org-agenda-delete-empty-blocks ()
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
(add-hook 'org-agenda-finalize-hook #'my/org-agenda-delete-empty-blocks)
(defun my/agenda ()
	(interactive)
	(call-process-shell-command "daily-checklist")
	(delete-other-windows)
	;; (find-file org-index-file)
	;; (beginning-of-buffer)
	(org-agenda nil "t")
	;; (pop-to-buffer "index.org") ;; focus on org index file
	;; (beginning-of-buffer)
	;; (enlarge-window 15)
	;; (org-link-open-from-string "[[*Today]]")
	;; (goto-char (org-find-exact-headline-in-buffer "Today"))
	)
(defun my/org-goto()
	(interactive)
	(let ((org-goto-interface 'outline-path-completion)) (org-goto))
	)

(defadvice org-agenda-set-mode-name (after truncate-org-agenda-mode-name activate)
	(setq mode-name '("Org-agenda")))
(add-to-list 'org-agenda-custom-commands
						 '("n" "News from this week"
							 ((agenda ""))
							 ((org-agenda-overriding-header "News from this week")
								(org-agenda-start-day "-6d")
								(org-agenda-span 14)
								(org-agenda-files '(org-file-path "news.org"
																									;; "recurring-events.org"
																									"books-read.org"
																									;;                                         "papers-read.org"
																									)))))
(defun my/org-empahsize ()
	(interactive)
	(if (not (use-region-p))
			(execute-kbd-macro (kbd "vaW")))
	(org-emphasize))

(add-to-list 'org-agenda-custom-commands
						 '("w" "Writing prompts"
							 ((tags "+writing"))
							 ((org-agenda-overriding-header "Writing prompts")
								(org-agenda-sorting-strategy '((agenda ts-down))))))
(defvar org-capture-templates '())
(add-to-list 'org-capture-templates
						 '("p" "Project idea"
							 entry
							 (file "project-idea.org")
							 "* %?\n"))
(add-to-list 'org-capture-templates
						 '("c" "Contact"
							 entry
							 (file "~/documents/contacts.org")
							 "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{123 Fake St., City, ST 12345}
:PHONE: %^{555-555-5555}
:EMAIL: %^{email}
:NOTE: %^{note}
:END:"))
;; (add-to-list 'org-capture-templates
;;              '("d" "Delivery"
;;                entry
;;                (file+headline "deliveries.org" "Deliveries")
;;                "** %?\n   SCHEDULED: %t\n"))
;; (add-to-list 'org-capture-templates
;;              '("e" "Email"
;;                entry
;;                (file+headline org-index-file "Index")
;;                "* TODO %?\n%a\n"))

;; (add-to-list 'org-capture-templates
;;              '("k" "Kookaburra ingest"
;;                entry
;;                (file+headline "kookaburra-ingest.org" "Queue")
;;                "* TODO %?\n"))
(add-to-list 'org-capture-templates
						 '("m" "Media queue"
							 item
							 (file+headline "media.org" "Index")
							 "- [ ] %?\n"))
(add-to-list 'org-capture-templates
						 '("n" "News item"
							 entry
							 (file "news.org")
							 "* %?\n%t\n"))
;; (add-to-list 'org-capture-templates
;;              '("p" "Finished paper"
;;                entry
;;                (file+headline "papers-read.org" "Papers")
;;                "* %^{Title} -- %^{Author}\n%t\n"))
(add-to-list 'org-capture-templates
						 '("w" "Writing prompt"
							 entry
							 (file "journal.org")
							 "* %?\n   %t\n"))
(add-to-list 'org-capture-templates
						 '("s" "Subscribe to an RSS feed"
							 plain
							 (file "rss-feeds.org")
							 "*** [[%^{Feed URL}][%^{Feed add}]]"))
(add-to-list 'org-capture-templates
						 '("t" "Task"
							 entry
							 (file+headline org-index-file "Index")
							 "* TODO [#%^{Priority (A-B-C)}] %?\n"))
(add-to-list 'org-capture-templates
						 '("Q" "Quote"
							 entry
							 (file "quotes.org")
							 "- %?\n"))
;; (add-to-list 'org-capture-templates
;;              '("w" "Work task"
;;                entry
;;                (file+headline "work.org" "Tasks")
;;                "* TODO %?\n"))
;; Languages templates
(add-to-list 'org-capture-templates
						 '("r" "Readlater"
							 plain
							 (file+headline org-index-file "ReadLater")
							 "** %^{Title or link to readlater}"))
(add-to-list 'org-capture-templates
						 '("q" "Question"
							 plain
							 (file+headline org-index-file "Questions")
							 "** %^{Question}"))
(add-to-list 'org-capture-templates
						 '("D" "Download"
							 plain
							 (file+headline org-index-file "To Download")
							 "- %^{Titel or Link}"))
(add-to-list 'org-capture-templates
						 '("b" "Book to read"
							 plain
							 (file+headline org-index-file "Books")
							 "** %^{Book Title}"))
(add-to-list 'org-capture-templates
						 '("B" "Red Book"
							 entry
							 (file+headline "red_books.org" "Red Books")
							 "* %^{Title} -- %^{Author}\n%t\n** Review\n%^{Review}\n** Summary\n%^{Summary}"))
(add-to-list 'org-capture-templates
						 '("M" "Movie"
							 plain
							 (file "movies.org")
							 "* %^{Title} -- %t\n** Review\n%?\n** Quotes\n"))
(add-to-list 'org-capture-templates
						 '("d" "Advice"
							 plain
							 (file "advice.org")
							 "* %?"))
(add-to-list 'org-capture-templates
						 '("e" "English word"
							 plain
							 (file+headline "language.org" "English Words")
							 "- %^{Word}: %^{Meaning}"))
(add-to-list 'org-capture-templates
						 '("E" "English phrase"
							 plain
							 (file+headline "language.org" "English Phrases")
							 "- %^{Phrase}: %^{Meaning}"))
(add-to-list 'org-capture-templates
						 '("I" "Idiom"
							 plain
							 (file+headline "language.org" "Idioms")
							 "- %^{Idiom}: %^{Meaning}"))
(add-to-list 'org-capture-templates
						 '("f" "Farsi word"
							 plain
							 (file+headline "language.org" "Farsi Words")
							 "- %^{Word}: %^{Meaning}"))
(add-to-list 'org-capture-templates
						 '("F" "Farsi phrase"
							 plain
							 (file+headline "language.org" "Farsi Phrases")
							 "- %^{Phrase}: %^{Meaning}"))
(add-to-list 'org-capture-templates
						 '("a" "Arabic word"
							 item
							 (file+headline "language.org" "Arabic Words")
							 "- %^{Word}"))
(add-to-list 'org-capture-templates
						 '("A" "Arabic phrase"
							 plain
							 (file+headline "language.org" "Arabic Phrases")
							 "- %^{Phrase}"))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(defun my/index-file-open ()
	"Open the master org TODO list."
	(interactive)
	(find-file org-index-file)
	(flycheck-mode -1)
	(end-of-buffer))

(setq my/diary-file (org-file-path "/diary/days.org"))
(defun my/diary-file-open()
	(interactive)
	;; (setq filename (concat "diary/" (format-time-string "%Y-%m-%d-%H-%M) " ".org"))
	(find-file my/diary-file)
	(end-of-buffer)
	(insert (concat "** " (format-time-string "%Y.%m.%d %H:%M %P ") "\n"))
	(evil-insert-state)
	(flycheck-mode -1))

(defun my/english-reading-notes-open ()
	(interactive)
	(find-file (org-file-path "/language.org"))
	(end-of-buffer))

(defun org-capture-todo ()
	(interactive)
	(org-capture :keys "t"))
;; (defun my/open-work-file ()
;;   "Open the work TODO list."
;;   (interactive)
;;   (find-file (org-file-path "work.org"))
;;   (flycheck-mode -1)
;;   (end-of-buffer))

(defun my/org-insert-link-dwim ()
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

(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-html-postamble nil)

;; org notifications
(use-package org-alert
	:ensure t
	:config
	(setq alert-default-style 'libnotify)
	(setq org-alert-interval 300
				org-alert-notify-cutoff 10
				org-alert-notification-title "Upcoming Event"
				org-alert-notify-after-event-cutoff 10)
	(org-alert-enable))


;; org reveal for presentations
(use-package ox-reveal
	:ensure t
	:config
	(setq org-reveal-root "file:///home/mahdi/.config/emacs/reveal.js"))
;; (use-package org-wild-notifier
;;	:ensure t
;;   :after org
;;   :config
;;   ;; Make sure we receive notifications for non-TODO events
;;   ;; like those synced from Google Calendar
;;   (setq org-wild-notifier-keyword-whitelist nil)
;;   (setq org-wild-notifier-notification-title "Agenda Reminder")
;;   (setq org-wild-notifier-alert-time '(1 10 30))
;;   (org-wild-notifier-mode))

;; publishing

;; (require 'ox-publish)

;; (setq org-publish-project-alist
;;       `(("pages"
;;          :base-directory "~/mhdna.io/org/"
;;          :base-extension "org"
;;          :recursive t
;;          :publishing-directory "~/mhdna.io/html/"
;;          :publishing-function org-html-publish-to-html
;;				 )
;;         ("static"
;;          :base-directory "~/mhdna.io/org/"
;;          :base-extension "css\\|txt\\|jpg\\|gif\\|png"
;;          :recursive t
;;          :publishing-directory  "~/mhdna.io/html/"
;;          :publishing-function org-publish-attachment
;;					 ;; Exporting template
;;				 :html-doctype "html5"
;;				 :html-html5-fancy t
;;																				; Disable some Org's HTML defaults
;;				 :html-head-include-scripts nil
;;				 :html-head-include-default-style nil
;;				 :html-head "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>"
;;				 :html-preamble "<nav>
;;   <a href=\"/\">&lt; Home</a>
;; </nav>
;; <div id=\"updated\">Updated: %C</div>"

;;				 :html-postamble "<hr/>
;; <footer>
;;   <div class=\"copyright-container\">
;;     <div class=\"copyright\">
;;       Copyright &copy; 2017-2020 Thomas Ingram some rights reserved<br/>
;;       Content is available under
;;       <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
;;         CC-BY-SA 4.0
;;       </a> unless otherwise noted
;;     </div>
;;     <div class=\"cc-badge\">
;;       <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
;;         <img alt=\"Creative Commons License\"
;;              src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\" />
;;       </a>
;;     </div>
;;   </div>

;;   <div class=\"generated\">
;;     Created with %c on <a href=\"https://www.gnu.org\">GNU</a>/<a href=\"https://www.kernel.org/\">Linux</a>
;;   </div>
;; </footer>"
;;				 :auto-sitemap t
;;				 :sitemap-filename "sitemap.org"

;;				 )

;;         ("mhdna.io" :components ("pages" "static"))))


(define-key org-mode-map (kbd "C-c C-l") 'my/org-insert-link-dwim)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key org-mode-map (kbd "C-c C-x C-a") 'my/mark-done-and-archive)
(global-set-key (kbd "C-c d") 'my/agenda)
(global-set-key (kbd "C-c t") 'org-capture-todo)
(global-set-key (kbd "C-c D") 'my/diary-file-open)
;; (global-set-key (kbd "C-c w") 'my/open-work-file)

(provide 'org-settings)
