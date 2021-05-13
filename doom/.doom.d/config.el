;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;;; Defaults ;;;;;

(setq delete-by-moving-to-trash t
      ;; don't save to x clipboard manager on quit since it takes a long time
      x-select-enable-clipboard-manager nil
      enable-dir-local-variables t
      enable-local-variables t
      evil-want-fine-undo t
      evil-ex-substitute-global t
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

;; additional evil text objectts
;; https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

(after! evil
  (define-and-bind-quoted-text-object "slash" "/" "/" "/")
  (define-and-bind-quoted-text-object "asterisk" "*" "*" "*")
  (define-and-bind-quoted-text-object "underscore" "l" "_" "_") ;; underLine ;; _ is already taken by _{ }

  (define-and-bind-quoted-text-object "tidle" "~" "~" "~")
  (define-and-bind-quoted-text-object "equals" "=" "=" "=")
  (define-and-bind-quoted-text-object "plus" "+" "+" "+"))
;; FIXME not sure why it isn't working
(after! evil-surround (push '(?l . ("_" . "_")) evil-surround-pairs-alist))

;;;;; Visuals ;;;;;

(setq! doom-font (font-spec :family "Iosevka" :size 14)
       doom-variable-pitch-font (font-spec :family "Noto Sans Display")
       doom-serif-font (font-spec :family "Noto Serif"))

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

(setq doom-theme 'doom-oceanic-next)
(chloe/set-org-latex-fragment-colour "White")

;; modeline appearance
(setq! doom-modeline-buffer-modification-icon t
       doom-modeline-buffer-state-icon t
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
;; (set-popup-rule! "^\\*info.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)
;; (set-popup-rule! "^\\*[Hh]elp.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)

;;;;; Important files/folders ;;;;;

(setq! org-directory "~/Dropbox/Org/"
       org-roam-directory (concat org-directory "notes/")
       chloe/org-agenda-directory (concat org-directory "agenda/")
       chloe/documents-directory "~/Documents/"
       chloe/nus-directory (concat chloe/documents-directory "NUS/")
       chloe/nus-current-sem-directory (concat chloe/nus-directory "Y3S2/")
       chloe/urops-directory (concat chloe/nus-directory "UROPS/")
       chloe/default-bibliography-file (concat org-roam-directory "zotero_references.bib"))

;;;;; Everything else ;;;;;

(setq! +latex-viewers '(pdf-tools okular))
(load! "org-config.el")
(load! "agenda-config.el")
(load! "shortcuts.el")

(load! "nextflow-mode.el")

;;
;;; Markdown

;; similar to org
(map! :map 'markdown-mode-map
      "C-RET" #'markdown-insert-list-item
      "<C-return>" #'markdown-insert-list-item)

;;
;;; Prolog

;; associate .pl files as prolog files instead of perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;
;;; Coq

(map! :after coq-mode
      :map coq-mode-map
      :localleader
      "j"  #'proof-assert-next-command-interactive
      "k"  #'proof-undo-last-successful-command)
(map! :after coq-mode
      :map coq-mode-map
      "C-c j"  #'proof-assert-next-command-interactive
      "C-c k"  #'proof-undo-last-successful-command)

;;
;;; R

(setq-hook! 'R-mode-hook +format-with :none)
