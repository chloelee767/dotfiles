;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;;; Defaults ;;;;;

(setq! delete-by-moving-to-trash t
       ;; don't save to x clipboard manager on quit since it takes a long time
       x-select-enable-clipboard-manager nil
       enable-dir-local-variables t
       evil-want-fine-undo t
       uniquify-buffer-name-style 'forward)

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

 :nvm "C-w" #'ace-window
 :i "C-c c" #'doom/leader)

;;;;; Visuals ;;;;;

(setq! doom-font (font-spec :family "Iosevka Nerd Font" :size 12.0)
       doom-variable-pitch-font (font-spec :family "Roboto" :size 12.0))

(global-prettify-symbols-mode 1)

(defun chloe/set-org-latex-fragment-colour (text-colour)
  (interactive "sText colour: ")
  (setq org-format-latex-options
        `(:foreground ,text-colour
          :background "Transparent"
          :scale 1.5
          :html-foreground ,text-colour
          :html-background "Transparent"
          :html-scale 1.0
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

;; set theme based on time
(if (member (string-to-number (substring (current-time-string) 11 13)) (number-sequence 7 17))
    (progn
      (setq! doom-theme 'doom-opera-light)
      (chloe/set-org-latex-fragment-colour "Black"))
  (progn
    (setq! doom-theme 'doom-gruvbox)
    (chloe/set-org-latex-fragment-colour "White")))

;; modeline appearance
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

;; popups
;; don't close help and info until I really want to
(set-popup-rule! "^\\*info.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)
(set-popup-rule! "^\\*[Hh]elp.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)

;;;;; Important files/folders ;;;;;
(setq! org-files-directory "~/Dropbox/Org/"
       org-roam-directory (concat org-files-directory "notes/")
       chloe/org-agenda-directory (concat org-files-directory "agenda/")
       chloe/documents-directory "~/Documents/"
       chloe/nus-directory (concat chloe/documents-directory "NUS/")
       chloe/nus-current-sem-directory (concat chloe/nus-directory "Y3S1/")
       chloe/default-bibliography-file (concat org-roam-directory "zotero_references.bib")
       org-journal-dir (concat org-files-directory "journal/"))

;;;;; Everything else ;;;;;

(setq! +latex-viewers '(pdf-tools okular))

(load! "utils.el")
(load! "org-config.el")
(load! "agenda-config.el")
(load! "biblio-config.el")
(load! "private.el")
