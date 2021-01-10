;;; ../dotfiles/doom/.doom.d/org-config.el -*- lexical-binding: t; -*-
(after! org
  ;; hooks
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'evil-tex-mode)

  ;; keybinds
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree))

  (setq org-list-allow-alphabetical t
        org-ellipsis " ▾"
        org-id-link-to-org-use-id  'create-if-interactive-and-no-custom-id
        ;; org-hide-emphasis-markers t
        )
  (custom-set-faces! '(org-document-title :height 1.5)))

;; FIXME this causes org-latex-preview to give an "invalid face" error when trying to generate previews for an entire section
;; ensure latex preview colour matches foreground and background
;; (setq org-format-latex-options
;;       '(:foreground auto
;;         :background auto
;;         :scale 1.5
;;         :html-foreground auto
;;         :html-background auto
;;         :html-scale 1.0
;;         :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(use-package! writeroom-mode
  :init
  (setq +zen-text-scale 0)
  :config
  (setq writeroom-extra-line-spacing 0.2
        writeroom-width 120
        writeroom-mode-line t))

;; (use-package! org-superstar
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-prettify-item-bullets nil))

;;
;;; Exporting
(after! org
  ;; latex export
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(use-package! ox-gfm
  :after org)

;;
;;; Utils
(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-clipboard
  org-download-dnd-base64
  :init
  ;; HACK We add these manually so that org-download is truly lazy-loaded
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-timestamp "%Y%m%d_%H%M%S"
        org-download-screenshot-method "flameshot gui --raw > %s")
  ;; TODO how to override "SPC m P" and other existing keybindings? setting it to nil doesn't work:
  ;; nested keybindings eg. "SPC m P a" seem to get bound afterwards
  (map! :map 'org-mode-map
        :localleader
        :prefix ("D" . "org-download")
        (:desc "paste from clipboard" "p" #'org-download-clipboard
         :desc "set file local download dir" "f" #'chloe/set-file-local-org-download-dir)))

;;
;;; Note taking

(defvar +notes-bibtex-files (list chloe/default-bibliography-file)
  "A list of the default bibtex files, which is needed by org-ref, reftex and bibtex-completion.")

(defvar +notes-ref-files-directory "~/Documents/References/"
  "Path to the directory containing reference files, used for the custom ref: link type. Must end with a slash.")

(setq +notes--biblio-directory "ref/")

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :hook (org-roam-buffer-prepare-hook . hide-mode-line-mode)
  :commands
  org-roam-find-file-function
  org-roam-buffer-toggle-display
  org-roam-switch-to-buffer
  org-roam-jump-to-index
  :config
  (map! :map org-mode-map
        :i "C-c i" #'org-roam-insert
        :i "C-c I" #'org-roam-insert-immediate)
  (map! :after org ;; ensure default keybinds get overwritten
        :leader
        :prefix ("n" . "notes")

        "f" nil
        "r" nil
        "t" nil
        "m" nil
        "d" nil
        "n" nil
        "I" nil

        "*" nil
        "a" nil
        "F" nil
        "s" nil
        "S" nil
        "v" nil

        :desc "Find roam file" "f" #'org-roam-find-file
        :desc "Switch to roam buffer" "," #'org-roam-switch-to-buffer
        :desc "Org roam buffer" "r" #'org-roam-buffer-toggle-display
        :desc "Toggle roam buffer position" "t" #'chloe/toggle-org-roam-buffer-position
        :desc "Index file" "I" #'org-roam-jump-to-index
        :desc "org-mark-ring-goto" "m" #'org-mark-ring-goto
        (:prefix ("d" . "daily notes")
         :desc "open date" "d" #'org-roam-dailies-find-date
         :desc "open today" "t" #'org-roam-dailies-find-today
         :desc "open yesterday" "m" #'org-roam-dailies-find-yesterday
         :desc "open tomorrow" "r" #'org-roam-dailies-find-tomorrow
         :desc "open previous" "j" #'org-roam-dailies-find-previous-note
         :desc "open next" "k" #'org-roam-dailies-find-next-note))
  (setq org-roam-db-location (concat doom-private-dir "local/org-roam.db")
        org-roam-tag-sources '(prop all-directories)
        org-roam-verbose nil
        ;; Make org-roam buffer sticky; i.e. don't replace it when opening a
        ;; file with an *-other-window command.
        org-roam-buffer-window-parameters '((no-delete-other-windows . t))
        ;; org-roam-completion-everywhere t
        org-roam-completion-system 'ivy)
  (setq org-roam-capture-templates
        `(("d" "default" plain #'org-roam-capture--get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("m" "temp" plain #'org-roam-capture--get-point
           "%?"
           :file-name "temp/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("p" "project" plain #'org-roam-capture--get-point
           "%?"
           :file-name "project/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("t" "tool" plain #'org-roam-capture--get-point
           "%?"
           :file-name "ref/tool/${slug}"
           :head "#+title: ${title}\n- documentation :: "
           :unnarrowed t)
          ("r" "ref" plain #'org-roam-capture--get-point
           "%?"
           :file-name ,(concat +notes--biblio-directory "%<%Y%m%d%H%M%S>-${slug}")
           :head "#+title: ${title}
- year ::
- authors ::
- doi ::
- url ::
- file ::
- tags ::
* Notes\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain #'org-roam-capture--get-point
           "%?"
           :file-name ,(concat +notes--biblio-directory "%<%Y%m%d%H%M%S>-${slug}")
           :head "#+title: ${title}
- year ::
- authors ::
- doi ::
- url ::
- file ::
- tags ::
* Notes\n"
           :unnarrowed t)))

  ;; improve appearance of org roam modeline
  (defadvice! doom-modeline--reformat-roam (orig-fun)
    :around #'doom-modeline-buffer-file-name
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔 (\\1-\\2-\\3) "
         (funcall orig-fun))
      (funcall orig-fun))))

(setq reftex-default-bibliography +notes-bibtex-files)

(after! org
  (+org-define-basic-link "rf" '+notes-ref-files-directory))

(use-package! org-ref
  :after org
  :preface
  ;; This need to be set before the package is loaded, because org-ref will
  ;; automatically `require' an associated package during its loading.
  (setq org-ref-completion-library #'org-ref-helm-bibtex)
  :config
  (setq org-ref-notes-directory (concat org-roam-directory +notes--biblio-directory)
        org-ref-default-bibliography +notes-bibtex-files)
  ;; Although the name is helm-bibtex, it is actually a bibtex-completion function
  ;; it is the legacy naming of the project helm-bibtex that causes confusion.
  ;; (setq org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex)
  ;; orb will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (setq org-ref-notes-function #'org-ref-notes-function-many-files
        org-ref-notes-directory (concat org-roam-directory +notes--biblio-directory)))

(use-package! bibtex-completion
  :defer t
  :config
  (map! :after org
        :leader
        :prefix ("n" . "notes")
        :desc "Bibliographic entries" "b" #'ivy-bibtex)
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography +notes-bibtex-files
        bibtex-completion-pdf-open-function (lambda (fpath) (call-process "okular" nil 0 nil fpath))
        bibtex-completion-display-formats
        '((t . "${author:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:10}"))
        bibtex-completion-notes-path (concat org-roam-directory +notes--biblio-directory)
        bibtex-completion-notes-template-multiple-files "${=key=}: ${title}
#+ROAM_KEY: cite:${=key=}
- year :: ${year}
- authors :: ${author}
- doi :: ${doi}
- url :: ${url}
- file ::
- tags ::
* Notes\n"))

;; only use ORB for orb-note-actions and orb-insert-non-ref
;; edit notes doesn't work consistently
(use-package! org-roam-bibtex
  :after org-roam
  ;; :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (map! :leader
        :prefix "n"
        :desc "Find roam file (non-ref)" "n" #'orb-find-non-ref-file)
  (map! :map org-mode-map
        :i "C-c n" #'orb-insert-non-ref)
  (map! :map 'org-mode-map
        :localleader
        "k" #'orb-note-actions))

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)

(setq org-journal-dir (concat org-directory "journal/"))
(after! org-journal
  (setq org-journal-file-type 'daily
        org-journal-find-file #'find-file)) ; don't split window when opening journal
