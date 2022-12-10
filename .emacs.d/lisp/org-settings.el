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

(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-html-postamble nil)

(provide 'org-settings)
