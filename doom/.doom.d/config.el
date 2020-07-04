;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;; Appearance ;;;;
;; TODO set treemacs font to be the same as doom-font
(setq doom-theme 'doom-peacock-v2)
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16))

(global-prettify-symbols-mode 1)

;; Enable line wrapping
(global-visual-line-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;;;; Org ;;;;
(setq org-files-directory "~/Documents/Org/"
      org-roam-directory (concat org-files-directory "notes/")
      deft-directory org-roam-directory
      deft-recursive t
      cl/org-agenda-directory (concat org-roam-directory "agenda/")
      org-agenda-files (list cl/org-agenda-directory)
      cl/org-refile-file (concat cl/org-agenda-directory "refile.org"))

(after! org
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree))
  ;; directories and files
  (setq org-list-allow-alphabetical t)
  (setq org-superstar-headline-bullets-list '("▶"))
  ;; (setq org-use-property-inheritance t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w)" "|" "KILL(k)")))

  ;; org capture
  (setq org-capture-templates '(("t" "todo" entry (file cl/org-refile-file) "* TODO %?")))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
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

(use-package! org-ref
  :after-call org-mode-hook
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  )

(use-package! org-roam
  :init
  ;; keybindings
  (map! :leader (:prefix "n" (:prefix "r"
                              :desc "Open index file" "e" #'org-roam-jump-to-index
                              :desc "org-mark-ring-goto" "m" #'org-mark-ring-goto
                              )))
  (map! :map org-mode-map
        :localleader
        (:prefix "m"
         "e" #'org-roam-jump-to-index
         "h" #'org-mark-ring-goto
         ))
  :config
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)

          ("b" "book" plain (function org-roam--capture-get-point)
           :file-name "res/books/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)

          ("p" "paper" plain (function org-roam--capture-get-point)
           :file-name "res/papers/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)

          ("w" "website" plain (function org-roam--capture-get-point)
           :file-name "res/websites/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)

          ("r" "other resource" plain (function org-roam--capture-get-point)
           :file-name "res/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)

          ("e" "person" plain (function org-roam--capture-get-point)
           :file-name "people/${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          )
        )
  )

;;;; LSP ;;;;
;; use microsoft python lsp
(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

;;;; Spellcheck ;;;;
(setq ispell-dictionary "british")
;; ispell-word and flyspell-correct-at-point are both bound to z =
;; HACK binding directly to the ispell-word function causes flyspell-correct-at-point to be called for some reason
(map! :nv "z s" (lambda () (interactive)(ispell-word)))

;;;; Misc ;;;;
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

(map! :nv "C-w" #'ace-window)

(setq delete-by-moving-to-trash t)

(load! "private.el")
