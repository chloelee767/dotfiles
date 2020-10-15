;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;; General ;;;;
(setq! doom-theme (if (member (string-to-number (substring (current-time-string) 11 13)) (number-sequence 7 17)) 'doom-simple-light 'doom-old-hope) ;; set theme based on time
       doom-font (font-spec :family "Iosevka Nerd Font" :size 12.0)
       ;; doom-variable-pitch-font (font-spec :family "iA Writer Quattro S" :size 12.0) ;; why does this appear bold? Looks fine in other programs
       doom-variable-pitch-font (font-spec :family "Roboto" :size 12.0)

       delete-by-moving-to-trash t
       x-select-enable-clipboard-manager nil ;; don't save to x clipboard manager on quit since it takes a long time
       enable-dir-local-variables t
       uniquify-buffer-name-style 'forward
       evil-want-fine-undo t)
(global-prettify-symbols-mode 1)
(global-visual-line-mode 1)
(global-subword-mode 1)
;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)
;; keybindings
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
 :i "C-c c" #'doom/leader)
;; looks interesting: https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org#create-a-paste-transient-state-to-cycle-through-kill-ring-on-paste
(map! :leader (:prefix "s" "o" nil))
(map! :leader (:prefix "s" "O" nil))
(map! :leader (:prefix "s"
               :desc "org-mark-ring-goto" "o" #'org-mark-ring-goto
               :desc "org-mark-ring-push" "O" #'org-mark-ring-push))

;; Modeline appearance
(custom-set-faces!
  '(doom-modeline-buffer-file :weight normal)
  '(doom-modeline-buffer-path :weight normal))
(setq! doom-modeline-buffer-modification-icon nil
       doom-modeline-buffer-state-icon nil
       doom-modeline-modal-icon nil)
;; https://tecosaur.github.io/emacs-config/config.html#window-title
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;;;; Directories ;;;;
(setq! org-files-directory "~/Dropbox/Org/"
       org-roam-directory (concat org-files-directory "notes/")
       chloe/org-agenda-directory (concat org-files-directory "agenda/")
       chloe/org-refile-file (concat chloe/org-agenda-directory "refile.org")
       chloe/documents-directory "~/Documents/"
       chloe/nus-directory (concat chloe/documents-directory "NUS/")
       chloe/nus-current-sem-directory (concat chloe/nus-directory "Y3S1/")
       chloe/default-bibliography-file (concat org-roam-directory "zotero_references.bib")

       org-agenda-files (list chloe/org-agenda-directory)
       org-roam-db-location (concat doom-private-dir "org-roam.db") ;; avoid syncing org-roam.db file to dropbox
       deft-directory org-roam-directory
       deft-recursive t
       ;; reftex-default-bibliography (list chloe/default-bibliography-file)
       ;; experimental biblio module
       +biblio-pdf-library-dir (concat chloe/documents-directory "orb-pdfs/") ;; not used
       +biblio-default-bibliography-files (list chloe/default-bibliography-file)
       +biblio-notes-path org-roam-directory)

;; shortcuts to useful folders
;; SPC f f is already bound counsel-find-file, but I don't mind since find file is bound to SPC . as well
(map! :leader (:prefix "f" "f" nil))
(map! :leader (:prefix "f" (:prefix ("f" . "favourites")
                            :desc "Home" "h" (lambda () (interactive)(doom-project-browse "~/"))
                            :desc "Documents" "d" (lambda () (interactive)(doom-project-browse chloe/documents-directory))
                            :desc "Code" "c" (lambda () (interactive)(doom-project-browse "~/Code/"))
                            :desc "Dotfiles" "D" (lambda () (interactive)(doom-project-find-file "~/dotfiles/"))
                            :desc "Agenda folder" "a" (lambda () (interactive)(doom-project-browse chloe/org-agenda-directory))
                            :desc "Org folder" "o" (lambda () (interactive)(doom-project-browse org-files-directory))

                            :desc "NUS" "n" (lambda () (interactive)(doom-project-browse chloe/nus-directory))
                            :desc "Current semester" "s" (lambda () (interactive)(doom-project-browse chloe/nus-current-sem-directory))
                            :desc "CS1010S Teaching" "t" (lambda () (interactive)(doom-project-browse (concat chloe/nus-directory "CS1010S-Teaching/")))
                            :desc "cs1010sx" "x" (lambda () (interactive)(doom-project-browse (concat chloe/nus-directory "CS1010S-Teaching/cs1010sx/"))))))

(map! "<f2>" #'org-agenda
      "<f1>" #'org-roam-jump-to-index
      "<f3>" #'(lambda () (interactive)(doom-project-browse chloe/org-agenda-directory))
      "<f4>" #'(lambda () (interactive)(browse-url "https://calendar.google.com")))

(load! "private.el")

;;;; Org ;;;;

;; org files
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
(use-package! org
  :config
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree))
  (setq! org-list-allow-alphabetical t
         org-ellipsis " ▾ "
         org-id-link-to-org-use-id  t)
  (custom-set-faces! '(org-document-title :height 1.5)))

;; (after! org-superstar
;;   org-superstar-headline-bullets-list '("✱");;'("☰" "☷" "☵" "☲"  "☳" "☴"  "☶"  "☱") ;;'("⯈")
;;   org-superstar-prettify-item-bullets nil
;;   ;; org-superstar-item-bullet-alist '((?- . ?–))
;;   )

(after! org (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

;; FIXME breaks themes that don't use org-level-N
;; function to toggle org heading size
;; (defun chloe/toggle-org-heading-size ()
;;   (interactive)
;;   (progn
;;    (setq chloe/toggle-org-heading-size--curr-size (if (equal chloe/toggle-org-heading-size--curr-size 1.0) 1.1 1.0))
;;    (custom-set-faces!
;;      `(org-level-1 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-2 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-3 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-4 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-5 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-6 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-7 :height ,chloe/toggle-org-heading-size--curr-size)
;;      `(org-level-8 :height ,chloe/toggle-org-heading-size--curr-size))))
;; ;; turn on larger headings by default
;; (setq chloe/toggle-org-heading-size--curr-size 1.0)
;; (chloe/toggle-org-heading-size)

(map! :map org-mode-map
      :leader (:prefix "t" :desc "Toggle heading size" "h" #'chloe/toggle-org-heading-size))

  ;; org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")

;; org agenda
(setq! org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "IN-PROG(i)" "|" "DONE(d!)")
                           (sequence "WAIT(w)" "|" "KILL(k)"))
       org-agenda-start-on-weekday nil ;; make org agenda start on today
       org-agenda-start-day "0d"
       org-agenda-span 7

       org-agenda-skip-deadline-if-done t
       org-agenda-skip-scheduled-if-done t

       org-agenda-dim-blocked-tasks nil
       org-agenda-prefix-format " %i %-12:c%?-12t% s %e "

       org-agenda-custom-commands '((" " "Default agenda"
                                     ;; Things today
                                     (
                                      ;; inbox
                                      (tags "CATEGORY=\"refile\"")

                                      ;; agenda calendar
                                      (agenda ""
                                              ((org-agenda-span 1)
                                               (org-deadline-warning-days 14)))

                                      ;; unscheduled todos, not on hold and excluding catchup
                                      (tags "CATEGORY<>\"refile\"DEADLINE=\"\"SCHEDULED=\"\"-catchup/!-WAIT"
                                            ((org-agenda-overriding-header "Unscheduled To Dos")))

                                      (tags "DEADLINE=\"\"SCHEDULED=\"\"+catchup/!-WAIT"
                                            ((org-agenda-overriding-header "Unscheduled Catchup")))

                                      ;; on hold
                                      (tags "/WAIT"))))

       org-capture-templates '(("t" "todo" entry (file chloe/org-refile-file) "* TODO %?")))

;; (use-package! org-ref
;;   ;; keybinds
;;   ;; C-] : insert ref from bib (same as auctex)
;;   :after-call org-mode-hook ;; load org-ref
;;   :config
;;   (setq org-ref-completion-library 'org-ref-ivy-cite
;;         org-ref-default-bibliography (list chloe/default-bibliography-file)))
(after! bibtex-completion
  (setq bibtex-completion-pdf-open-function (lambda (fpath) (call-process "okular" nil 0 nil fpath))
        bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:36}"))))

(use-package! org-roam
  :init
  (map! :map org-mode-map

        :i "C-c i" #'org-roam-insert
        :i "C-c I" #'org-roam-insert-immediate
        :i "C-c r" #'org-roam

        :localleader (:prefix "m"
                      "t" #'(lambda () (interactive)(setq org-roam-buffer-position (if (equal org-roam-buffer-position 'right) 'bottom 'right)))
                      "e" #'org-roam-jump-to-index
                      "m" #'org-mark-ring-goto))
  (map!
   :leader (:prefix "n" (:prefix "r"
                         :desc "Toggle buffer position" "t" #'(lambda () (interactive)(setq org-roam-buffer-position (if (equal org-roam-buffer-position 'right) 'bottom 'right)))
                         :desc "Open index file" "e" #'org-roam-jump-to-index
                         :desc "org-mark-ring-goto" "m" #'org-mark-ring-goto)))
  :config
  (setq! +org-roam-open-buffer-on-find-file nil)
  (setq! org-roam-tag-sources '(prop all-directories)))
;; improve appearance of org roam modeline
(defadvice! doom-modeline--reformat-roam (orig-fun)
  :around #'doom-modeline-buffer-file-name
  (message "Reformat?")
  (message (buffer-file-name))
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔 (\\1-\\2-\\3) "
       (funcall orig-fun))
    (funcall orig-fun)))

;;;; Python ;;;;
(use-package! lsp-pyright
  ;; don't use :ensure t because of doom
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; HACK https://github.com/hlissner/doom-emacs/issues/3267
;; (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

;;;; Latex ;;;;
(setq! +latex-viewers '(pdf-tools okular))
