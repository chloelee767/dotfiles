;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;; General ;;;;
(setq doom-theme 'doom-monokai-v2
      doom-font (font-spec :family "Iosevka Nerd Font" :size 12.0)
      ;; doom-variable-pitch-font (font-spec :family "iA Writer Quattro S" :size 12.0) ;; why does this appear bold? Looks fine in other programs
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 11.0)

      delete-by-moving-to-trash t
      x-select-enable-clipboard-manager nil ;; don't save to x clipboard manager on quit since it takes a long time
      )

(global-prettify-symbols-mode 1)
(global-visual-line-mode 1) ;; Enable line wrapping

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; editor keybindings
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

 :n "C-!" #'rotate-text-backward

 :nv "C-w" #'ace-window
 )
;; looks interesting: https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org#create-a-paste-transient-state-to-cycle-through-kill-ring-on-paste

(setq org-files-directory "~/Dropbox/Org/"
      org-roam-directory (concat org-files-directory "notes/")
      chloe/org-agenda-directory (concat org-files-directory "agenda/")
      chloe/org-refile-file (concat chloe/org-agenda-directory "refile.org")
      chloe/documents-directory "~/Documents/"
      chloe/nus-directory (concat chloe/documents-directory "NUS/")
      chloe/nus-current-sem-directory (concat chloe/nus-directory "Y3S1/")

      ;; tool specific
      org-agenda-files (list chloe/org-agenda-directory)
      org-roam-db-location (concat doom-private-dir "org-roam.db") ;; avoid syncing org-roam.db file to dropbox
      deft-directory org-roam-directory
      deft-recursive t)

;; shortcuts to useful folders
;; SPC f f is already bound counsel-find-file, but I don't mind since find file is bound to SPC . as well
(map! :leader (:prefix "f" "f" nil))
(map! :leader (:prefix "f" (:prefix ("f" . "favourites")
               :desc "Home" "h" (lambda () (interactive)(doom-project-browse "~/"))
               :desc "Documents" "d" (lambda () (interactive)(doom-project-browse chloe/documents-directory))
               :desc "Code" "c" (lambda () (interactive)(doom-project-browse "~/Code/"))
               :desc "Dotfiles" "f" (lambda () (interactive)(doom-project-find-file "~/dotfiles/"))
               :desc "Agenda folder" "a" (lambda () (interactive)(doom-project-browse chloe/org-agenda-directory))
               :desc "Org folder" "o" (lambda () (interactive)(doom-project-browse org-files-directory))

               :desc "NUS" "n" (lambda () (interactive)(doom-project-browse chloe/nus-directory))
               :desc "Current semester" "s" (lambda () (interactive)(doom-project-browse chloe/nus-current-sem-directory))
               :desc "CS1010S Teaching" "t" (lambda () (interactive)(doom-project-browse (concat chloe/nus-directory "CS1010S-Teaching/")))
               :desc "cs1010sx" "x" (lambda () (interactive)(doom-project-browse (concat chloe/nus-directory "CS1010S-Teaching/cs1010sx/")))
               )))

(load! "private.el")

;;;; Org ;;;;
;; (use-package org-variable-pitch :load-path doom-private-dir
;;   :config
;;   (setq org-variable-pitch-fixed-font "Iosevka")
;;   )
;; (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)
;; (add-hook 'org-mode-hook #'variable-pitch-mode)

(use-package! org
  :config
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree))
  (setq org-list-allow-alphabetical t)
  (setq org-superstar-headline-bullets-list '("▶"))
  ;; (setq org-use-property-inheritance t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w)" "|" "KILL(k)")))

  ;; org capture
  (setq org-capture-templates '(("t" "todo" entry (file chloe/org-refile-file) "* TODO %?")))

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

          ("t" "talk" plain (function org-roam--capture-get-point)
           :file-name "res/talks/%<%Y%m%d%H%M%S>-${slug}"
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
