;;; ../dotfiles/doom/.doom.d/agenda-config.el -*- lexical-binding: t; -*-

(defun chloe/org-agenda-effort-string ()
  (let ((effort (org-entry-get (point) "Effort")))
    (if (> (length effort) 0)
        (concat "[" effort "]")
      "")))

(defun chloe/is-not-agenda-calendar-file ()
  "Returns true if this file is not in the directory agenda/calendar."
  (if (string-match-p "agenda/calendar/" (buffer-file-name))
      nil
    t))

(after! org

  (add-to-list 'org-modules 'org-checklist)
  (add-to-list 'org-modules 'org-habit)

  ;; logging
  (setq org-log-done-with-time t
        org-log-reschedule 'time
        org-log-into-drawer t)

  ;; archive into date tree
  (setq org-archive-location "%s_archive::datetree/")

  ;; don't clutter refile targets with gcal calendar events
  (setq org-refile-target-verify-function #'chloe/is-not-agenda-calendar-file)

  ;; using two %(...)'s only shows the first one, for some reason
  (let ((prefix " %(chloe/org-agenda-effort-string)  "))
    (setq org-agenda-prefix-format `((agenda . ,(concat " %i %-12:c%?-12t% s" prefix))
                                     (todo . ,(concat "%i %-12:c" prefix))
                                     (tags . ,(concat "%i %-12:c" prefix))
                                     (search . ,(concat "%i %-12:c" prefix)))))

  (setq org-agenda-files (list chloe/org-agenda-directory (concat chloe/org-agenda-directory "calendar"))
        org-todo-keywords '((sequence "TODO(t)" "INPROG(i)" "|" "DONE(d!)")
                            (sequence "WAIT(w)" "|" "KILL(k)"))
        org-lowest-priority ?D

        ;; start today
        org-agenda-start-on-weekday nil
        org-agenda-start-day "0d"

        org-agenda-skip-deadline-prewarning-if-scheduled t

        org-deadline-warning-days 14
        org-agenda-span 7)

  (setq org-agenda-custom-commands
        `((" " "Overview"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-deadline-warning-days 365)))

            (tags-todo
             (concat "SCHEDULED=\"\"" "CATEGORY<>\"someday\"" "CATEGORY<>\"inbox\"" "/!")
             ((org-agenda-overriding-header "Next actions (no deadline, unscheduled)")
              (org-agenda-skip-function #'(org-agenda-skip-entry-if 'deadline))))

            (tags-todo "CATEGORY=\"inbox\"|+inbox"
                       ((org-agenda-overriding-header "Inbox")))

            ))
          ("d" "Someday"
           ((tags-todo "CATEGORY=\"someday\"")))
          ))

  (let ((created-property-string ":Created: %U\n"))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,(concat chloe/org-agenda-directory "inbox.org"))
             ,(concat "* TODO %?\n:PROPERTIES:\n" created-property-string ":END:"))))))

;; HACK Fix "Capture template ‘t’: Error in a Doom startup hook: doom-switch-buffer-hook, +org--restart-mode-h,"
(when (eq system-type 'darwin)
  (advice-add #'org-capture :around
              (lambda (fun &rest args)
                (letf! ((#'+org--restart-mode-h #'ignore))
                  (apply fun args)))))
