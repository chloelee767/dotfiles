;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;; Appearance ;;;;
;; TODO change font comment font colours
(setq doom-theme 'doom-peacock)
(setq doom-peacock-brighter-comments t)
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16))

(global-prettify-symbols-mode 1)
;; (toggle-frame-maximized)

;; Enable line wrapping
(global-visual-line-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;;;; Org ;;;;
(after! org
  ;; directories and files
  (setq org-files-directory "~/Documents/Org/"
        org-roam-directory (concat org-files-directory "notes/")
        deft-directory org-roam-directory
        deft-recursive t
        cl/org-agenda-directory (concat org-files-directory "todos/")
        org-roam-index-file (concat org-roam-directory "index.org") ;; access the index file using org-roam-jump-to-index
        org-agenda-files (list cl/org-agenda-directory)
        cl/org-refile-file (concat cl/org-agenda-directory "refile.org"))

  (setq org-list-allow-alphabetical t)
  (setq org-superstar-headline-bullets-list '("▶"))
  ;; (setq org-use-property-inheritance t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w)" "|" "KILL(k)")))

  ;; org ref
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (require 'org-ref)

  ;; org roam
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
  (setq org-roam-buffer-width 0.2)

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
            (tags "CATEGORY<>\"refile\"-SOMEDAY/TODO" ((org-agenda-overriding-header "To Do (now)")))
            (tags "CATEGORY<>\"refile\"-SOMEDAY/WAIT" ((org-agenda-overriding-header "Waiting")))
            (tags "CATEGORY=\"refile\"/" ((org-agenda-overriding-header "Refile")))
            (tags "CATEGORY<>\"refile\"+SOMEDAY/!" ((org-agenda-overriding-header "To Do (someday)")))
            ))

          ("c" "Calendar"
           ((agenda "" ((org-agenda-span 14) (org-deadline-warning-days 0)))))

          ("d" "Deadlines overview"
           ((agenda "" ((org-agenda-span 1) (org-deadline-warning-days 365)))))
          ))
  (setq org-agenda-dim-blocked-tasks nil) ;; so projects aren't dimmed
  )

;;;; LSP ;;;;
;; use microsoft python lsp
(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

;; fix lsp showing outdated errors: https://github.com/hlissner/doom-emacs/issues/3267
(setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))

;;;; Spellcheck ;;;;
(setq ispell-dictionary "en_GB")

;;;; Keybindings ;;;;
(map! :n "C-!" #'rotate-text-backward)

(map!
 :nv "j" #'evil-next-visual-line
 :nv "k" #'evil-previous-visual-line
 :nv "0" #'evil-beginning-of-visual-line
 :nv "^" #'evil-first-non-blank-of-visual-line
 :nv "$" #'evil-end-of-visual-line

 :nv "gj" #'evil-next-line
 :nv "gk" #'evil-previous-line
 :nv "g0" #'evil-beginning-of-line
 :nv "g^" #'evil-first-non-blank-of-line
 :nv "g$" #'evil-end-of-line
 )

;; looks interesting: https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org#create-a-paste-transient-state-to-cycle-through-kill-ring-on-paste


(load! "private.el")
