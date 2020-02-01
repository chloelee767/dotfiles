;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; enable line wrapping
(global-visual-line-mode 1)

;; turn off auto-fill mode
;; (auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "■")
                                   (?B . "■")
                                   (?C . "■")))
)

;; Org Todo and Agenda
(after! org
  ;; org directory
  (setq org-files-directory "~/Documents/Org")
  (setq org-agenda-files (list org-files-directory))

  (setq org-use-property-inheritance t)

  ;; agenda
  (setq org-agenda-start-on-weekday 1) ;; start on monday
  (setq org-deadline-warning-days 0) ;; don't clutter today with future deadlines
  ;; TODO is there a way to truncate the breadcrumbs? Or ideally display properties
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c %?-12t %s %-20b")
                                   (todo . " %i %-12:c %-20b")
                                   (tags . " %i %-12:c %-20b")
                                   (search . " %i %-12:c %-20b")))

  (setq org-agenda-custom-commands '(("q" "Agenda with unscheduled items"
                                      ((agenda "" ((org-agenda-span 7)))
                                       (tags "+DEADLINE=\"\"+SCHEDULED=\"\"/!")
                                       ))
                                     ("u" "Unscheduled items"
                                      ((tags "+DEADLINE=\"\"+SCHEDULED=\"\"/!"))
                                      )))

  ;; (setq org-agenda-overriding-columns-format "%CATEGORY %LABEL %TODO %3PRIORITY %ITEM %DEADLINE %SCHEDULED %TAGS")
)

;; pretty symbols
(global-prettify-symbols-mode 1)
(defvar base-prettify-symbols-alist '(("<=" . ?≤)
                                      (">=" . ?≥)
                                      ("!=" . ?≠)
                                      ("lambda" . ?λ)
                                      ("None"   . ?∅)
                                      ("null"   . ?∅)
                                      ("in"     . ?∈)
                                      ("not in" . ?∉)))

(set-frame-font "Meslo LG S DZ for Powerline" nil t)
