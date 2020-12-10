;;; ../dotfiles/doom/.doom.d/biblio-config.el -*- lexical-binding: t; -*-

(setq reftex-default-bibliography (list chloe/default-bibliography-file))

(use-package! org-ref
  :after org
  :preface
  ;; This need to be set before the package is loaded, because org-ref will
  ;; automatically `require' an associated package during its loading.
  (setq org-ref-completion-library #'org-ref-helm-bibtex)
  :config
  (setq org-ref-notes-directory (concat org-roam-directory "ref/paper/")
        org-ref-default-bibliography (list chloe/default-bibliography-file))
  ;; Although the name is helm-bibtex, it is actually a bibtex-completion function
  ;; it is the legacy naming of the project helm-bibtex that causes confusion.
  ;; (setq org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex)
  ;; orb will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (setq org-ref-notes-function #'org-ref-notes-function-many-files))

(setq chloe/org-biblio-note-template
      "- year :: ${year}
- authors :: ${author}
- doi :: ${doi}
- url :: ${url}
- tags ::

* Notes\n")

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-notes-path org-roam-directory)

  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
        (concat "${=key=}: ${title}\n#+ROAM_KEY: cite:${=key=}\n" chloe/org-biblio-note-template)
        bibtex-completion-bibliography (list chloe/default-bibliography-file))

  (setq bibtex-completion-pdf-open-function (lambda (fpath) (call-process "okular" nil 0 nil fpath))
        bibtex-completion-display-formats
        '((t . "${author:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:10}")))

  (map! :leader (:prefix "n"
                 :desc "Bibliographic entries" "b" #'ivy-bibtex)))

(use-package! org-roam-bibtex
  :init
  (add-hook! 'org-mode-hook #'org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor" "keywords")
        orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "$ref/paper/{slug}"
           :head (concat "#+TITLE: ${ref}: ${title}\n#+ROAM_KEY: ${ref}\n" chloe/org-biblio-note-template)
           :unnarrowed t)))
  (map! :map 'org-mode-map
        :localleader (:prefix "m"
                      "n" #'orb-note-actions))
  (map! :leader (:prefix "n" (:prefix "r"
                              "n" #'orb-note-actions)))
  )
