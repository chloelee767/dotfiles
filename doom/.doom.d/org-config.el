;;; ../dotfiles/doom/.doom.d/org-config.el -*- lexical-binding: t; -*-
;;
;;; Visuals
(after! org
  (setq org-list-allow-alphabetical t
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-id-link-to-org-use-id  'create-if-interactive-and-no-custom-id)
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

(use-package! org-superstar
  :hook (writeroom-mode . org-superstar-mode))

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

(after! org
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'evil-tex-mode)
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree)))

(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-clipboard
  org-download-dnd-base64
  :init
  ;; HACK From doom's org +drag-and-drop module
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
;; org-download-image-dir is safe as long as it is a string
(put 'org-download-image-dir 'safe-local-variable #'stringp)

;;
;;; Note taking

(defun chloe/org-roam-file-slug (input)
  "My version of `org-roam-node-slug'
- Replace spaces with -
- Downcase
- Ensures no trailing or sequential underscores"
  ;; Adapted from `org-roam-node-slug'
  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:].]" . "-")  ;; convert anything not alphanumeric or .
                      ("--*" . "-")  ;; remove sequential dashes
                      ("^-" . "")  ;; remove starting dash
                      ("-$" . ""))) ;; remove ending dash
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks input) pairs)))
        (downcase slug)))))

(defun chloe/org-roam-title (slug)
  (file-name-extension slug))

(use-package! org-roam
  :after org
  :init
  (map! :after org
        :leader
        :prefix ("n" . "notes")
        :desc "Org roam find" "f" #'org-roam-node-find
        :desc "Org roam buffer" "r" #'org-roam-buffer-toggle)
  :config
  ;;(setq org-roam-mode-sections
  ;;      (list #'org-roam-backlinks-insert-section
  ;;            #'org-roam-reflinks-insert-section
  ;;            #'org-roam-unlinked-references-insert-section))
  (org-roam-setup)
  (map! :map org-mode-map
        :i "C-c i" #'org-roam-node-insert)
  (setq org-roam-db-location "~/org-roam.db"
        org-roam-node-display-template "${file:*}${olp:*}"
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%(chloe/org-roam-file-slug \"${title}\").org"
                              "#+title: %(chloe/org-roam-title (chloe/org-roam-file-slug \"${title}\"))")
           :immediate-finish t
           :unnarrowed t))))

;; (use-package! org-roam
;;   :hook (org-load . org-roam-mode)
;;   :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
;;   :hook (org-roam-buffer-prepare-hook . hide-mode-line-mode)
;;   :commands
;;   org-roam-find-file-function
;;   org-roam-buffer-toggle-display
;;   org-roam-switch-to-buffer
;;   org-roam-jump-to-index
;;   :config

;;   ;; keybindings
;;   (map! :map org-mode-map
;;         :i "C-c i" #'org-roam-insert
;;         :i "C-c I" #'org-roam-insert-immediate)
;;   (map! :after org ;; ensure default keybinds get overwritten
;;         :leader
;;         :prefix ("n" . "notes")

;;         "f" nil
;;         "r" nil
;;         "t" nil
;;         "m" nil
;;         "d" nil
;;         "n" nil
;;         "I" nil

;;         "*" nil
;;         "a" nil
;;         "F" nil
;;         "s" nil
;;         "S" nil
;;         "v" nil

;;         :desc "Find roam file" "f" #'org-roam-find-file
;;         :desc "Switch to roam buffer" "," #'org-roam-switch-to-buffer
;;         :desc "Org roam buffer" "r" #'org-roam-buffer-toggle-display
;;         :desc "Toggle roam buffer position" "t" #'chloe/toggle-org-roam-buffer-position
;;         :desc "Index file" "I" #'org-roam-jump-to-index
;;         :desc "org-mark-ring-goto" "m" #'org-mark-ring-goto
;;         (:prefix ("d" . "daily notes")
;;          :desc "open date" "d" #'org-roam-dailies-find-date
;;          :desc "open today" "t" #'org-roam-dailies-find-today
;;          :desc "open yesterday" "m" #'org-roam-dailies-find-yesterday
;;          :desc "open tomorrow" "r" #'org-roam-dailies-find-tomorrow
;;          :desc "open previous" "j" #'org-roam-dailies-find-previous-note
;;          :desc "open next" "k" #'org-roam-dailies-find-next-note))

;;   ;; templates
;;   (setq org-roam-capture-templates
;;         `(("d" "default" plain #'org-roam-capture--get-point
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+title: ${title}\n"
;;            :unnarrowed t)
;;           ("r" "ref" plain #'org-roam-capture--get-point
;;            "%?"
;;            :file-name "ref/${slug}"
;;            :head "#+title: ${title}"
;;            :unnarrowed t)
;;           ("t" "temp" plain #'org-roam-capture--get-point
;;            "%?"
;;            :file-name "temp/${slug}"
;;            :head "#+title: ${title}\n"
;;            :unnarrowed t)
;;           ("p" "project" plain #'org-roam-capture--get-point
;;            "%?"
;;            :file-name "project/${slug}"
;;            :head "#+title: ${title}\n"
;;            :unnarrowed t)
;;           ("e" "personal" plain #'org-roam-capture--get-point
;;            "%?"
;;            :file-name "personal/${slug}"
;;            :head "#+title: ${title}"
;;            :unnarrowed t)))

;;   ;; other configs
;;   (setq org-roam-db-location (concat doom-private-dir "local/org-roam.db")
;;         org-roam-tag-sources '(prop all-directories)
;;         org-roam-verbose nil
;;         org-roam-db-update-method 'immediate
;;         ;; Make org-roam buffer sticky; i.e. don't replace it when opening a
;;         ;; file with an *-other-window command.
;;         org-roam-buffer-window-parameters '((no-delete-other-windows . t))
;;         ;; org-roam-completion-everywhere t
;;         org-roam-completion-system 'ivy
;;         org-roam-link-title-format "§%s"))

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)
