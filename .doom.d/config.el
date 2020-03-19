;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Theme
(setq doom-theme 'doom-peacock)
;; Other themes which are not bad:
;; oceanic next
;; doom vibrant/doom one
;; spacegrey
;; gruvbox
;; opera
;; wilmersdorf
;; tomorrow night

;; Font
(set-frame-font "Fira Mono for Powerline" nil t)

;; Enable line wrapping
(global-visual-line-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Org Todo and Agenda
(after! org
  ;; org directory
  (setq org-files-directory "~/Documents/Org")
  (setq org-agenda-files (list org-files-directory))

  (setq org-use-property-inheritance t)

  ;; agenda
  ;; make org agenda start on today
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "0d")

  (setq org-deadline-warning-days 0) ;; don't clutter today with future deadlines
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t) ;; don't show scheduled and deadline on same day

  ;; don't show done items in agenda view
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; TODO is there a way to truncate the breadcrumbs? Or ideally display properties
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c %?-12t %s %-20b")
                                   (todo . " %i %-12:c %-20b")
                                   (tags . " %i %-12:c %-20b")
                                   (search . " %i %-12:c %-20b")))

  (setq org-agenda-custom-commands '(("q" "Agenda with unscheduled items"
                                      ((agenda "" ((org-agenda-span 12)))
                                       (tags "-longterm+DEADLINE=\"\"+SCHEDULED=\"\"/!")
                                       ))
                                     ("u" "Unscheduled items"
                                      ((tags "-longterm+DEADLINE=\"\"+SCHEDULED=\"\"/!"))
                                      )
                                     ("l" "Long term"
                                      ((tags "+longterm/!")) ;; /! : match not done TODOs
                                      )))

  ;; (setq org-agenda-overriding-columns-format "%CATEGORY %LABEL %TODO %3PRIORITY %ITEM %DEADLINE %SCHEDULED %TAGS")
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
