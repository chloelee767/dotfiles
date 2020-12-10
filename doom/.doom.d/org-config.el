;;; ../dotfiles/doom/.doom.d/org-config.el -*- lexical-binding: t; -*-

(after! org
  ;; hooks
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'evil-tex-mode)
  (add-hook 'org-mode-hook 'writeroom-mode)

  ;; keybinds
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree))

  ;; latex export
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  )

;;;;; Visuals ;;;;;
(after! org
  (setq! org-list-allow-alphabetical t
         org-ellipsis " ▾ "
         org-id-link-to-org-use-id  'create-if-interactive-and-no-custom-id
         org-hide-emphasis-markers t)
  (custom-set-faces! '(org-document-title :height 1.5)))

(use-package! writeroom-mode
  :init
  (setq +zen-text-scale 0)
  :config
  (setq writeroom-extra-line-spacing 0.2
        writeroom-width 100
        writeroom-mode-line t))

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))

;;;;; Org Roam ;;;;;
(defun chloe/toggle-org-roam-buffer-position ()
  (interactive)
  (setq org-roam-buffer-position (if (equal org-roam-buffer-position 'right) 'bottom 'right)))

(use-package! org-roam
  :config
  (map! :map org-mode-map
        :i "C-c i" #'org-roam-insert
        :i "C-c I" #'org-roam-insert-immediate

        :localleader (:prefix "m"
                      "t" #'chloe/toggle-org-roam-buffer-position
                      "e" #'org-roam-jump-to-index
                      "m" #'org-mark-ring-goto))
  (map!
   :leader (:prefix "n" (:prefix "r"
                         :desc "Toggle buffer position" "t" #'chloe/toggle-org-roam-buffer-position
                         :desc "Open index file" "e" #'org-roam-jump-to-index
                         :desc "org-mark-ring-goto" "m" #'org-mark-ring-goto)))
  (setq! org-roam-db-location (concat doom-private-dir "org-roam.db") ;; avoid syncing org-roam.db file to dropbox
         +org-roam-open-buffer-on-find-file nil
         org-roam-tag-sources '(prop all-directories))
  (add-to-list 'org-roam-capture-templates '("z" "ref/tool/zb" plain #'org-roam-capture--get-point
                                             "%?"
                                             :file-name "ref/tool/zb/${slug}"
                                             :head "#+title: ${title}\n"
                                             :unnarrowed t))
  (add-to-list 'org-roam-capture-templates '("t" "ref/tool" plain #'org-roam-capture--get-point
                                             "%?"
                                             :file-name "ref/tool/${slug}"
                                             :head "#+title: ${title}\n"
                                             :unnarrowed t))
  )

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

(after! deft
  deft-directory org-roam-directory
  deft-recursive t)

;;;;; Org Journal ;;;;;
(after! org-journal
  (setq org-journal-file-type 'monthly
        org-journal-find-file #'find-file)) ; don't split window when opening journal

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

;;;;; Org download ;;;;;
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
  (after! org
    (setq org-download-method 'directory
          org-download-image-dir "./images"
          org-download-timestamp "%Y%m%d_%H%M%S"
          org-download-screenshot-method "flameshot gui --raw > %s")
    (map! :map 'org-mode-map
          :leader (:prefix "n"
                   "p" #'org-download-clipboard
                   "P" #'org-download-yank))))
