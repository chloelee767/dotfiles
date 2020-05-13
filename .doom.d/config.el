;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(toggle-frame-maximized)

;; Theme
(setq doom-theme 'doom-peacock)
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
  (setq line-spacing 0.2)

  ;; org ref
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (require 'org-ref)

  ;; org directory
  (setq org-files-directory "~/Documents/Org")
  (setq org-agenda-files (list org-files-directory))

  (setq org-use-property-inheritance t)

  ;; agenda
  ;; make org agenda start on today
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "0d")

  ;; don't show done items in agenda view
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Show headings in agenda
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c %?-12t %s %-12b")
                                   (todo . " %i %-12:c %-12b")
                                   (tags . " %i %-12:c %-12b")
                                   (search . " %i %-12:c %-12b")))

  (setq org-agenda-custom-commands '(
    ("s" "2 week agenda"
     ((agenda "" (
                  (org-agenda-span 14)
                  (org-deadline-warning-days 0) ;; don't clutter with future deadlines
                  ))))
    ("q" "Daily agenda"
     ((agenda "" (
                  (org-agenda-span 1)
                  (org-deadline-warning-days 31) ;; up to 1 month warning
                  ))
      (tags-todo "-longterm+DEADLINE=\"\"+SCHEDULED=\"\"") ;; unscheduled TOODs
      ))
    ("u" "Unscheduled TODOs"
      ((tags-todo "-longterm+DEADLINE=\"\"+SCHEDULED=\"\"")))
    ("l" "Long term, unscheduled TODOs"
      ((tags-todo "+longterm+DEADLINE=\"\"+SCHEDULED=\"\"")))
                                     ))
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

