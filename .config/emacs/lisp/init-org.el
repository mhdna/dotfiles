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
(setq initial-major-mode 'org-mode)
(setq org-hide-emphasis-markers t)
(use-package org-appear
  :hook (org-mode . org-appear-mode))
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

(defun my/notes-open()
  (interactive)
  (find-file org-directory))

(defun my/org-goto()
  (interactive)
  (let ((org-goto-interface 'outline-path-completion)) (org-goto)))

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
(defun select-current-word ()
  "Select the current word."
  (let (bounds)
    (skip-chars-backward "A-Za-z0-9_")
    (setq bounds (point))
    (skip-chars-forward "A-Za-z0-9_")
    (set-mark bounds)))
                                                  )))))
(defun my-org-emphasize ()
  (interactive)
  (if (use-region-p)
      (org-emphasize (region-beginning) (region-end))
    (progn
      (select-current-word)
      (org-emphasize (point) (mark)))))

;; (defvar org-capture-templates '())
(setq org-capture-templates
      (quote (("s" "Schedule"
               plain
               (file+headline org-index-file "Index")
               "** %?\n   SCHEDULED: %t\n")
              ("t" "todo"
               entry
               (file+headline org-index-file "Index")
               "* TODO [#%^{Priority}] %?\t%^g\n")
              ("T" "todo code (File)"
               entry
               (file+headline org-index-file "Code")
               "* TODO %?\n%i%a\n")
              ;; http://doc.norang.ca/org-mode.html#CaptureTemplates
              ;; ("T" "todo+refile" entry (file+headline org-index-file "Index")
              ;;  "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Media queue"
               item
               (file+headline "media.org" "Index")
               "- [ ] %?\n")
              ("n" "News item"
               entry
               (file "news.org")
               "* %?\n%U\n")
              ("p" "Project idea"
               entry
               (file "project-idea.org")
               "* %?\t%^g\n")
              ;; ("j" "Journal prompt"
              ;;  entry
              ;;  (file "journal.org")
              ;;  "* %?\t%^g\n   %t\n")
              ("S" "Shopping item"
               plain
               (file+headline org-index-file "Shopping List")
               "** %? \t%^g\n")
              ("Q" "Quote"
               plain
               (file "quotes.org")
               "* %? (%^{Quotee})\t%^g\n")
              ;; Languages templates
              ("r" "Readlater"
               plain
               (file+headline org-index-file "ReadLater")
               "** %^{Title or link to readlater}")
              ("q" "Question"
               plain
               (file+headline org-index-file "Questions")
               "** %^{Question}? \t%^g\n")
              ;; ("d" "Advice"
              ;;  plain
              ;;  (file "advice.org")
              ("D" "Download"
               plain
               (file+headline org-index-file "To Download")
               "- %^{Titel or Link}")
              ("b" "Book to read"
               plain
               (file+headline org-index-file "Books")
               "** %^{Book Title}\t%^g")
              ("B" "Red Book"
               entry
               (file+headline "red_books.org" "Red Books")
               "* %^{Title} -- %^{Author}\n%t\n** Review\n%^{Review}\n** Summary\n%^{Summary}\t%^g")
              ("M" "Movie"
               plain
               (file "movies.org")
               "* %^{Title} -- %t   :%^g:\n** Stars: %^{Stars (out of 10)}/10\n** Degeneracy: %^{Degeneracy (out of 10)}/10\n** Review\n%?\n** Quotes\n")
              ("e" "English Words"
               plain
               (file+headline "language.org" "English Words")
               "*** %^{Word}\t%^g\n\t: %^{Meaning}")
              ("E" "English Phrases"
               plain
               (file+headline "language.org" "English Phrases")
               "*** %^{Phrase}\t%^g\n")
              ;; ("f" "Farsi Words"
              ;;  plain
              ;;  (file+headline "language.org" "Farsi Words")
              ;;  "- %^{Word}\n\t%^g\t: %^{Meaning}")
              ;; ("F" "Farsi Phrases"
              ;;  plain
              ;;  (file+headline "language.org" "Farsi Phrases")
              ;;  "*** %^{Phrase}\t%^g\n\t: %^{Meaning}")
              ("a" "Arabic Words"
               item
               (file+headline "language.org" "Arabic Words")
               "*** %^{Word}\t%^g")
              ("A" "Arabic Phrases"
               plain
               (file+headline "language.org" "Arabic Phrases")
               "*** %^{Phrase}\t%^g")
              ("c" "Color"
               plain
               (file+headline "language.org" "Colors")
               "** %^{Color and explanation -if any-} %^{Hex Code}")
              )))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(defun my/index-file-open ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1))

(setq my/diary-file (org-file-path "/diary/days.org"))
(defun my/diary-file-open()
  (interactive)
  ;; (setq filename (concat "diary/" (format-time-string "%Y-%m-%d-%H-%M) " ".org"))
  (find-file my/diary-file)
  (end-of-buffer)
  ;; (insert (concat "** " (format-time-string "%Y.%m.%d %H:%M %P ") "\n"))
  (let ((title (read-string "Enter Title: ")))
  (insert (concat "** " title (format-time-string " %Y.%m.%d %H:%M %P ") "\n")))
  (org-set-tags-command)
  (evil-insert-state)
  (flycheck-mode -1))

(setq my/journal-file (org-file-path "/journal.org"))
(defun my/journal-file-open()
  (interactive)
  (find-file my/journal-file)
  (end-of-buffer)
  (let ((title (read-string "Enter Title: ")))
  (insert (concat "** " title (format-time-string "\n<%Y-%m-%d %a>") "\n")))
  (org-set-tags-command)
  (evil-insert-state)
  (flycheck-mode -1))

(defun my/english-reading-notes-open ()
  (interactive)
  (find-file (org-file-path "/language.org"))
  (goto-char (org-find-exact-headline-in-buffer "English Reading Notes"))
  (org-shifttab)
  (org-cycle))

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

;; ;; org notifications
(use-package org-alert
  :config
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notification-title "Upcoming Event"
        org-alert-notify-after-event-cutoff 10)
  (org-alert-enable))


;; anki-editor
(use-package anki-editor)

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))


(define-key org-mode-map (kbd "C-c C-l") 'my/org-insert-link-dwim)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key org-mode-map (kbd "C-c C-x C-a") 'my/mark-done-and-archive)
(global-set-key (kbd "C-c d") 'my/agenda)
(global-set-key (kbd "C-c t") 'org-capture-todo)
(global-set-key (kbd "C-c D") 'my/diary-file-open)
;; (global-set-key (kbd "C-c w") 'my/open-work-file)

(provide 'init-org)
