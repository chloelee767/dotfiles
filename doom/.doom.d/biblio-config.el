;;; ../dotfiles/doom/.doom.d/biblio-config.el -*- lexical-binding: t; -*-

(setq chloe/paper-notes-directory "ref/paper/")
(setq reftex-default-bibliography (list chloe/default-bibliography-file))

(use-package! org-ref
  :after org
  :preface
  ;; This need to be set before the package is loaded, because org-ref will
  ;; automatically `require' an associated package during its loading.
  (setq org-ref-completion-library #'org-ref-helm-bibtex)
  :config
  (setq org-ref-notes-directory (concat org-roam-directory chloe/paper-notes-directory)
        org-ref-default-bibliography (list chloe/default-bibliography-file))
  ;; Although the name is helm-bibtex, it is actually a bibtex-completion function
  ;; it is the legacy naming of the project helm-bibtex that causes confusion.
  ;; (setq org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex)
  ;; orb will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (setq org-ref-notes-function #'org-ref-notes-function-many-files))

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-notes-path (concat org-roam-directory chloe/paper-notes-directory))

  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography (list chloe/default-bibliography-file)
        bibtex-completion-pdf-open-function (lambda (fpath) (call-process "okular" nil 0 nil fpath))
        bibtex-completion-display-formats
        '((t . "${author:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:10}")))

  (map! :leader (:prefix "n"
                 :desc "Bibliographic entries" "b" #'ivy-bibtex)))

(setq chloe/org-biblio-note-template "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}
- date :: ${date}
- authors :: ${author}
- doi :: ${doi}
- url :: ${url}
- tags ::

* Notes\n")

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("citekey" "date" "type" "pdf?" "note?" "author" "editor" "file" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev" "doi" "url")
        orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name ,(concat chloe/paper-notes-directory "${citekey}")
           :head ,chloe/org-biblio-note-template
           :unnarrowed t)))
  (map! :map 'org-mode-map
        :localleader (:prefix "m"
                      "n" #'orb-note-actions))
  (map! :leader (:prefix "n" (:prefix "r"
                              "n" #'orb-note-actions))))

;; TODO: add orb-insert, orb-insert-non-ref, orb-find-non-ref
