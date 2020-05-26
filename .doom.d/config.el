;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(toggle-frame-maximized)

;; Theme
(setq doom-theme 'doom-solarized-light)
(setq doom-peacock-brighter-comments t)

;; Font
(setq doom-font (font-spec :family "RobotoMono Nerd Font" :size 16)
      doom-unicode-font (font-spec :family "RobotoMono Nerd Font" :size 16))

;; Enable line wrapping
(global-visual-line-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Org
(after! org
  ;; org ref
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (require 'org-ref)

  ;; org directory
  (setq org-files-directory "~/Documents/Org")
  (setq org-agenda-files (list org-files-directory))

  (setq org-use-property-inheritance t)

  ;; To Do setup
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w)" "HOLD(h)" "|" "KILL(k)")))

  ;; agenda
  ;; make org agenda start on today
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "0d")

  ;; don't show done items in agenda view
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; (setq org-columns-default-format
  ;;       "%TODO %3PRIORITY %25ITEM %DEADLINE %TAGS")
  ;; (setq org-agenda-view-columns-initially t)
  ;; Agenda items format
  ;; (setq org-agenda-prefix-format '((agenda . " %i %-12:c %?-12t %s")
  ;;                                  (todo . " %i %-12:c")
  ;;                                  (tags . " %i %-12:c")
  ;;                                  (search . " %i %-12:c")
  ;;                                  ))

  (setq org-agenda-custom-commands
        '(
          (" " "Default agenda"
           ;; Things today
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-deadline-warning-days 31)
                     (org-agenda-overriding-header "Deadlines for the next month")
                     ))

            ;; NEXT items
            (tags "/NEXT"
                  ((org-agenda-overriding-header "Next")))

            ;; TODO items
            (tags "CATEGORY!=\"refile\"-SOMEDAY/TODO"
                  ((org-agenda-overriding-header "To Do")))

            ;; Projects
            (tags "CATEGORY!=\"refile\"-SOMEDAY/PROJ"
                  ((org-agenda-overriding-header "Projects")))

            ;; WAIT items
            (tags "CATEGORY!=\"refile\"-SOMEDAY/WAIT"
                  ((org-agenda-overriding-header "Waiting")))

            ;; HOLD items
            (tags "CATEGORY!=\"refile\"=SOMEDAY/HOLD"
                  ((org-agenda-overriding-header "Undecided")))

            ;; Refile items
            (tags "CATEGORY=\"refile\"/"
                  ((org-agenda-overriding-header "Notes and To Dos to Refile")))

            ))

          ("c" "Calendar"
           ((agenda ""
                    ((org-agenda-span 14)
                     (org-deadline-warning-days 0) ;; don't clutter with future deadlines
                     ))))

          ("s" "Someday"
           (
            ;; Projects
            (tags "+SOMEDAY/PROJ"
                  ((org-agenda-overriding-header "Projects")))

            ;; NEXT items
            (tags "+SOMEDAY/NEXT"
                  ((org-agenda-overriding-header "Next")))

            ;; TODO items
            (tags "+SOMEDAY/TODO"
                  ((org-agenda-overriding-header "To Do")))

            ;; WAIT items
            (tags "+SOMEDAY/WAIT"
                  ((org-agenda-overriding-header "Waiting")))

            ;; HOLD items
            (tags "+SOMEDAY/HOLD"
                  ((org-agenda-overriding-header "Undecided")))
            ))
          ))

  ;; so projects aren't dimmed
  (setq org-agenda-dim-blocked-tasks nil)

  ;; org capture
  (setq chloe/org-refile "~/Documents/Org/refile.org")
  (setq org-capture-templates
        '(("t" "todo" entry (file chloe/org-refile) "* TODO %?")
          ("n" "note" entry (file chloe/org-refile) "* %?")))
  )

;; Pretty symbols
(global-prettify-symbols-mode 1)
(defvar base-prettify-symbols-alist '(("<=" . ?≤)
                                      (">=" . ?≥)
                                      ("!=" . ?≠)
                                      ("lambda" . ?λ)
                                      ("None"   . ?∅)
                                      ("null"   . ?∅)
                                      ("in"     . ?∈)
                                      ("not in" . ?∉)))

