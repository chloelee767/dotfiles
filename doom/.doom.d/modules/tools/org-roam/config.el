;;; tools/org-roam/config.el -*- lexical-binding: t; -*-

(defvar +org-roam-bibtex-files nil
  "A list of the default bibtex files, which is needed by org-ref, reftex and bibtex-completion.")

;; global keybindings
(map! :after org ;; after ensures default keybinds get overwritten
      :leader
      :prefix ("n" . "notes")
      "f" nil
      "r" nil
      "t" nil
      "m" nil
      "d" nil
      "n" nil

      "*" nil
      "a" nil
      "F" nil
      "s" nil
      "S" nil
      "v" nil

      :desc "Find roam file" "f" #'org-roam-find-file
      :desc "Find roam file (non-ref)" "n" #'orb-find-non-ref-file
      :desc "Switch to roam buffer" "," #'org-roam-switch-to-buffer
      :desc "Org roam buffer" "r" #'org-roam-buffer-toggle-display
      :desc "Toggle roam buffer position" "t" #'+toggle-org-roam-buffer-position
      :desc "org-mark-ring-goto" "m" #'org-mark-ring-goto
      :desc "Bibliography entries" "b" #'ivy-bibtex
      (:prefix ("d" . "daily notes")
       :desc "open date" "d" #'org-roam-dailies-find-date
       :desc "open today" "t" #'org-roam-dailies-find-today
       :desc "open yesterday" "m" #'org-roam-dailies-find-yesterday
       :desc "open tomorrow" "r" #'org-roam-dailies-find-tomorrow
       :desc "open previous" "j" #'org-roam-dailies-find-previous-note
       :desc "open next" "k" #'org-roam-dailies-find-next-note))

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :config
  (map! :map org-mode-map
        :i "C-c i" #'org-roam-insert
        :i "C-c I" #'org-roam-insert-immediate)
  (setq org-roam-db-location (concat doom-private-dir "local/org-roam.db"))
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain #'org-roam-capture--get-point
           "%?"
           :file-name "concept/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("h" "helpsheet" plain #'org-roam-capture--get-point
           "%?"
           :file-name "helpsheet/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("p" "project" plain #'org-roam-capture--get-point
           "%?"
           :file-name "project/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("r" "ref" plain #'org-roam-capture--get-point
           "%?"
           :file-name "ref/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n- source :: "
           :unnarrowed t)
          ("t" "ref/tool" plain #'org-roam-capture--get-point
           "%?"
           :file-name "ref/tool/${slug}"
           :head "#+title: ${title}\n- documentation :: "
           :unnarrowed t)

          ("z" "ref/tool/zb" plain #'org-roam-capture--get-point
           "%?"
           :file-name "ref/tool/zb/${slug}"
           :head "#+title: ${title}\n- documentation :: "
           :unnarrowed t)))

  (setq org-roam-verbose nil
        ;; Make org-roam buffer sticky; i.e. don't replace it when opening a
        ;; file with an *-other-window command.
        org-roam-buffer-window-parameters '((no-delete-other-windows . t))
        org-roam-completion-everywhere t
        org-roam-completion-system
        (cond ((featurep! :completion helm) 'helm)
              ((featurep! :completion ivy) 'ivy)
              ((featurep! :completion ido) 'ido)
              ('default)))
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)
  ;; improve appearance of org roam modeline
  (defadvice! doom-modeline--reformat-roam (orig-fun)
    :around #'doom-modeline-buffer-file-name
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔 (\\1-\\2-\\3) "
         (funcall orig-fun))
      (funcall orig-fun)))
  )

(setq +org-roam--biblio-directory "ref/paper/")

(setq reftex-default-bibliography +org-roam-bibtex-files)

(use-package! org-ref
  :after org
  :preface
  ;; This need to be set before the package is loaded, because org-ref will
  ;; automatically `require' an associated package during its loading.
  (setq org-ref-completion-library #'org-ref-helm-bibtex)
  :config
  (setq org-ref-notes-directory (concat org-roam-directory +org-roam--biblio-directory)
        org-ref-default-bibliography +org-roam-bibtex-files)
  ;; Although the name is helm-bibtex, it is actually a bibtex-completion function
  ;; it is the legacy naming of the project helm-bibtex that causes confusion.
  ;; (setq org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex)
  ;; orb will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (setq org-ref-notes-function #'org-ref-notes-function-many-files))

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-notes-path (concat org-roam-directory +org-roam--biblio-directory))
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography +org-roam-bibtex-files
        bibtex-completion-pdf-open-function (lambda (fpath) (call-process "okular" nil 0 nil fpath))
        bibtex-completion-display-formats
        '((t . "${author:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:10}"))))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (map! :map org-mode-map
        :i "C-c n" #'orb-insert-non-ref)
  (setq orb-preformat-keywords
        '("citekey" "date" "type" "pdf?" "note?" "author" "editor" "file"
          "author-abbrev" "editor-abbrev" "author-or-editor-abbrev" "doi" "url")
        orb-templates
        `(("d" "default (paper)" plain (function org-roam-capture--get-point)
           "%?"
           :file-name ,(concat +org-roam--biblio-directory "${citekey}")
           :head "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}
- date :: ${date}
- authors :: ${author}
- doi :: ${doi}
- url :: ${url}
- tags ::

* Notes\n"
           :unnarrowed t)))
  (map! :map 'org-mode-map
        :localleader
        "j" #'orb-note-actions))

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)
