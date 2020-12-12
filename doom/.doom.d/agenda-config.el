;;; ../dotfiles/doom/.doom.d/agenda-config.el -*- lexical-binding: t; -*-

(add-to-list 'org-modules 'org-checklist)
(add-to-list 'org-modules 'org-habit)

(defun chloe/org-agenda-intensity-string ()
  (let ((intensity (org-entry-get (point) "Intensity")))
    (if (> (length intensity) 0)
        (concat "[" intensity "]")
      "")))

(defun chloe/org-agenda-effort-string ()
  (let ((effort (org-entry-get (point) "Effort")))
    (if (> (length effort) 0)
        (concat "[" effort "]")
      "")))

;; using two %(...)'s only shows the first one, for some reason
(let ((prefix " %(concat (chloe/org-agenda-effort-string) (chloe/org-agenda-intensity-string)) "))
  (setq org-agenda-prefix-format `((agenda . ,(concat " %i %-12:c%?-12t% s" prefix))
                                   (todo . ,(concat "%i %-12:c" prefix))
                                   (tags . ,(concat "%i %-12:c" prefix))
                                   (search . ,(concat "%i %-12:c" prefix)))))

(setq org-agenda-files (list chloe/org-agenda-directory)
      org-todo-keywords '((sequence "TODO(t)" "INPROG(i)" "PROJ(p)" "|" "DONE(d!)")
                          (sequence "WAIT(w)" "|" "KILL(k)"))

      ;; start today
      org-agenda-start-on-weekday nil
      org-agenda-start-day "0d"

      org-deadline-warning-days 0
      org-agenda-span 14)

(setq org-agenda-custom-commands
      `((" " "Overview"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-deadline-warning-days 365)))

          (tags "CATEGORY=\"inbox\""
                ((org-agenda-overriding-header "Inbox")))

          (tags-todo
           (concat
            "SCHEDULED=\"\"&PRIORITY=\"A\""
            "CATEGORY<>\"inbox\"-inbox"
            "|"
            "SCHEDULED=\"\"&PRIORITY=\"B\""
            "CATEGORY<>\"inbox\"-inbox"
            "/!")
           ((org-agenda-overriding-header "Next actions")))

          (tags-todo
           "PRIORITY<>\"A\"PRIORITY<>\"B\"SCHEDULED=\"\"CATEGORY<>\"inbox\"-inbox/!-PROJ"
           ((org-agenda-overriding-header "Someday")))
          ))))

(let ((created-property-string ":Created: %U\n"))
  (setq org-capture-templates
        `(("t" "todo" entry (file ,(concat chloe/org-agenda-directory "inbox.org"))
           ,(concat "* TODO %?\n:PROPERTIES:\n" created-property-string ":END:")))))
