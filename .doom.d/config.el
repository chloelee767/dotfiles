;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; (toggle-frame-maximized)

;; Theme
(setq doom-theme 'doom-solarized-light)
(setq doom-peacock-brighter-comments t)

;; Font
(setq doom-font (font-spec :family "Iosevka" :size 16))

;; Enable line wrapping
(global-visual-line-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Org
(after! org
  ;; directories and files
  (setq org-files-directory "~/Documents/org/"
        org-roam-directory (concat org-files-directory "notes/")
        deft-directory org-roam-directory
        deft-recursive t
        cl/org-agenda-directory (concat org-files-directory "todos/")
        ;; (setq org-roam-index-file "index.org") ;; access the index file using org-roam-jump-to-index
        org-agenda-files (list cl/org-agenda-directory)
        cl/org-refile-file (concat cl/org-agenda-directory "refile.org"))

   ;; (setq org-use-property-inheritance t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w)" "|" "KILL(k)")))

   ;; org ref
   (setq org-ref-completion-library 'org-ref-ivy-cite)
   (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
   (require 'org-ref)

  ;; org roam templates
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n"
             :unnarrowed t)
         ;; resources go to res/ folder
         ("r" "resource" plain (function org-roam--capture-get-point)
             :file-name "res/%<%Y%m%d%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n"
             :unnarrowed t)))

   ;; org capture
   (setq org-capture-templates '(("t" "todo" entry (file cl/org-refile-file) "* TODO %?")))

   ;; agenda
   ;; make org agenda start on today
   (setq org-agenda-start-on-weekday nil)
   (setq org-agenda-start-day "0d")

   ;; don't show done items in agenda view
   (setq org-agenda-skip-deadline-if-done t)
   (setq org-agenda-skip-scheduled-if-done t)

   (setq org-agenda-custom-commands
         '(
           (" " "Default agenda"
            ;; Things today
            ((agenda ""
                     ((org-agenda-span 1)
                      (org-deadline-warning-days 31)
                      (org-agenda-overriding-header "Deadlines for the next month")
                      ))
             (tags "/NEXT" ((org-agenda-overriding-header "Next")))
             (tags "CATEGORY<>\"refile\"-SOMEDAY/TODO" ((org-agenda-overriding-header "To Do")))
             (tags "CATEGORY<>\"refile\"-SOMEDAY/PROJ" ((org-agenda-overriding-header "Projects")))
             (tags "CATEGORY<>\"refile\"-SOMEDAY/WAIT" ((org-agenda-overriding-header "Waiting")))
             (tags "CATEGORY=\"refile\"/" ((org-agenda-overriding-header "Refile")))
             ))

           ("c" "Calendar"
            ((agenda "" ((org-agenda-span 14) (org-deadline-warning-days 0)))))

           ("s" "Someday"
            ((tags "+SOMEDAY/PROJ" ((org-agenda-overriding-header "Projects")))
             (tags "+SOMEDAY/NEXT" ((org-agenda-overriding-header "Next")))
             (tags "+SOMEDAY/TODO" ((org-agenda-overriding-header "To Do")))
             (tags "+SOMEDAY/WAIT" ((org-agenda-overriding-header "Waiting")))
             ))

           ("d" "Deadlines overview"
            ((agenda "" ((org-agenda-span 1) (org-deadline-warning-days 365)))))
           ))
   (setq org-agenda-dim-blocked-tasks nil) ;; so projects aren't dimmed
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

