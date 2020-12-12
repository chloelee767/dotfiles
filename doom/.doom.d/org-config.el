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

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

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
    ;; TODO how to override "SPC m P" and other existing keybindings? setting it to nil doesn't work:
    ;; nested keybindings eg. "SPC m P a" seem to get bound afterwards
    (map! :map 'org-mode-map
          :localleader
          "C" #'org-download-clipboard)))
