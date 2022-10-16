;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;
;;; Defaults

(setq delete-by-moving-to-trash t
      select-enable-clipboard nil ;; don't clutter OS clipboard with evil delete etc. Use + register for OS clipboard.
      enable-dir-local-variables t
      enable-local-variables t
      evil-ex-substitute-global t
      company-idle-delay 3.0
      +latex-viewers '(pdf-tools okular)
      display-line-numbers-type 'relative
      ;; TODO renenable when compatible with workspaces module
      ;; uniquify-buffer-name-style 'forward
      ;; uniquify-after-kill-buffer-p t
      )

(global-visual-line-mode 1)
(global-subword-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

(map! :nvm "C-w" #'ace-window
      :g "C-c SPC" #'doom/leader
      :g "C-S-c" #'clipboard-kill-ring-save
      :g "C-S-v" #'clipboard-yank
      (:when IS-MAC
       :g "s-c" #'clipboard-kill-ring-save
       :g "s-v" #'clipboard-yank))
(map! :leader
      :prefix ("y" . "yank")
      :desc "Copy last kill to clipboard" "c" #'chloe/copy-last-kill-to-clipboard)
;; disable tmm=menubar
 (map! :g "`" nil
      :g "M-`" nil)

;; be similar to org
(map! :map 'markdown-mode-map
      "C-RET" #'markdown-insert-list-item
      "<C-return>" #'markdown-insert-list-item)

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

;;
;;; Visuals

(setq doom-theme (if (member (string-to-number (substring (current-time-string) 11 13)) (number-sequence 7 17)) 'doom-tomorrow-day 'doom-tomorrow-night) ;; set theme based on time
      doom-font (font-spec :family "Iosevka SS14" :size (if IS-MAC 13.0 11.0))
      doom-variable-pitch-font doom-font
      doom-serif-font (font-spec :family "Noto Serif")
      )

(global-prettify-symbols-mode 1)

(custom-theme-set-faces! 'tsdh-light
  ;; '(hl-line :background nil) ;; sometimes breaks dired
  '(company-tooltip-selection :background "LightSteelBlue")
  '(org-block :background "#f0f0f1")
  '(org-code :background "#f0f0f1" :foreground nil)
  '(org-verbatim :background "#f0f0f1" :foreground "#50a14f")
  '(org-quote :inherit org-block :weight bold :slant italic)
  '(org-verse :inherit org-block :slant italic)
  '(org-table :foreground nil))

;; modeline appearance
(setq doom-modeline-buffer-modification-icon t
      doom-modeline-buffer-state-icon t
      doom-modeline-modal-icon nil)

;; popups
;; don't close help and info until I really want to
;; (set-popup-rule! "^\\*info.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)
;; (set-popup-rule! "^\\*[Hh]elp.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)

;;
;;; Useful files and directories

(setq org-directory "~/Dropbox/Org/"
      org-roam-directory (concat org-directory "notes/")
      chloe/org-agenda-directory (concat org-directory "agenda/")
      chloe/documents-directory "~/Documents/"
      chloe/nus-directory (concat chloe/documents-directory "NUS/")
      chloe/gosrc-directory "~/go/src/"
      chloe/carousell-gocode-directory (concat chloe/gosrc-directory "github.com/carousell/")
      chloe/dropbox-directory "~/Dropbox/"
      chloe/work-dropbox (concat chloe/dropbox-directory "Work/")
      chloe/current-tickets-dropbox (concat chloe/work-dropbox "tickets/current/")
      )

;;
;;; Shortcuts and utils

(map! :leader (:prefix "f" "f" nil)) ;; SPC f f is already bound counsel-find-file, but I don't mind since find file is bound to SPC . as well

(map! :leader
      (:prefix "f"

       ;; shortcuts to useful folders
       (:prefix ("f" . "favourites")
        :desc "Home" "h" (cmd! (doom-project-browse "~/"))
        :desc "Documents" "d" (cmd! (doom-project-browse chloe/documents-directory))
        :desc "Dropbox" "r" (cmd! (doom-project-browse chloe/dropbox-directory))
        :desc "Code" "c" (cmd! (doom-project-browse "~/Code/"))
        :desc "Go (Carousell)" "g" (cmd! (doom-project-browse chloe/carousell-gocode-directory))
        :desc "Go src" "G" (cmd! (doom-project-browse chloe/gosrc-directory))
        :desc "Dotfiles" "t" (cmd! (doom-project-browse "~/dotfiles/"))
        :desc "Dotfiles (find in project)" "T" (cmd! (doom-project-find-file "~/dotfiles/"))
        :desc "Agenda folder" "a" (cmd! (doom-project-browse chloe/org-agenda-directory))
        :desc "Org folder" "o" (cmd! (doom-project-browse org-directory))
        :desc "NUS" "n" (cmd! (doom-project-browse chloe/nus-directory))
        :desc "Work" "w" (cmd! (doom-project-browse chloe/work-dropbox))
        :desc "Current tickets" "i" (cmd! (doom-project-browse chloe/current-tickets-dropbox)))

       ;; file utils
       :desc "Yank filename only" "C-y" #'chloe/yank-buffer-filename-only
       :desc "chmod" "x" #'chloe/chmod-current-file
       :desc "make executable" "X" #'chloe/make-current-file-executable)

      ;; more utils
      (:prefix "o" :desc "Google calendar" "c" #'(cmd! (browse-url "https://calendar.google.com"))))

;;
;;; Programming languages

;; associate .pl files as prolog files instead of perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(map! :after coq-mode
      :map coq-mode-map
      :localleader
      "j"  #'proof-assert-next-command-interactive
      "k"  #'proof-undo-last-successful-command)
(map! :after coq-mode
      :map coq-mode-map
      "C-c j"  #'proof-assert-next-command-interactive
      "C-c k"  #'proof-undo-last-successful-command)

(after! ess
  (setq-hook! 'R-mode-hook +format-with :none))

(use-package! protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package! nextflow-mode
  :mode ("\\.nf\\'" . nextflow-mode)
  :mode ("nextflow\\.config\\'" . nextflow-mode))

(after! go-mode
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))


(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(add-hook 'c++-mode #'format-all-mode)
(use-package! clang-format
  :after cc-mode
  :init
  (setq-hook! 'c++-mode-hook +format-with-lsp nil)
  :config
  (set-formatter! 'clang-format #'clang-format-buffer :modes '(c++-mode))
  (map! ; :mode c++-mode-map
   :g "<f10>" #'lsp-clangd-find-other-file)
  ;; does nothing if .clang-format is absent or clang format fallback not set
  ;; (defun clang-format-before-save ()
  ;;   (interactive)
  ;;   (when (eq major-mode 'c++-mode)
  ;;     (clang-format-buffer)))
  ;; (add-hook 'before-save-hook 'clang-format-before-save)

  ;; (map! :map c++-mode-map
  ;;       :localleader
  ;;       :prefix ("F" . "clang-format")
  ;;       :desc "format region" "r" #'clang-format-region
  ;;       :desc "format buffer" "f" #'clang-format-buffer)
  )

;;
;;; Org mode

;; visuals
(after! org
  (setq org-list-allow-alphabetical t
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-startup-folded 'showall ;; respect heading specific visibility
        ;; FIXME this causes org-latex-preview to give an "invalid face" error when trying to generate previews for an entire section
        ;; org-format-latex-options '(:foreground auto
        ;;                            :background auto
        ;;                            :scale 1.5
        ;;                            :html-foreground "Black"
        ;;                            :html-background "Transparent"
        ;;                            :html-scale 1.0
        ;;                            :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        )

  (custom-set-faces! '(org-document-title :height 1.5)))

(use-package! writeroom-mode
  :init
  (setq +zen-text-scale 0)
  :config
  (setq writeroom-extra-line-spacing 0.2
        writeroom-width 120
        writeroom-mode-line t))

;; (use-package! org-superstar
;;   :hook (writeroom-mode . org-superstar-mode))

(after! org
  (setq org-id-link-to-org-use-id  'create-if-interactive-and-no-custom-id)
  (add-hook! 'org-mode-hook #'turn-on-org-cdlatex #'evil-tex-mode)
  (map! :map org-mode-map :localleader (:prefix "s" "y" #'org-copy-subtree))

  ;; latex export
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(use-package! ox-gfm
  :after org)

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
        org-download-screenshot-method (cond (IS-MAC "screencapture -i %s")
                                             (IS-LINUX "flameshot gui --raw > %s")))
  (map! :map 'org-mode-map
        :localleader
        :prefix ("D" . "org-download")
        (:desc "paste from clipboard" "p" #'org-download-clipboard
         :desc "set file local download dir" "f" #'chloe/set-file-local-org-download-dir))

  ;; org-download-image-dir is safe as long as it is a string
  (put 'org-download-image-dir 'safe-local-variable #'stringp))

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
  (if (string-match "\\." slug) (file-name-extension slug) slug))

(use-package! org-roam
  :after org
  :init
  (map! :after org
        :leader
        :prefix ("n" . "notes")
        :desc "Org roam find" "f" #'org-roam-node-find
        :desc "Org roam buffer" "r" #'org-roam-buffer-toggle
        :desc "Org roam buffer" "R" #'org-roam-buffer-display-dedicated)
  :config
  (org-roam-db-autosync-mode)
  (map! :map org-mode-map
        :i "C-c i" #'org-roam-node-insert)

  (cl-defmethod org-roam-node-my-title ((node org-roam-node))
    (file-name-sans-extension (file-relative-name (org-roam-node-file node) org-roam-directory)))
  (setq org-roam-node-display-template "${my-title:*}${olp:*}")
  (setq org-roam-db-location "~/org-roam.db"
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%(chloe/org-roam-file-slug \"${title}\").org"
                              "#+title: %(chloe/org-roam-title (chloe/org-roam-file-slug \"${title}\"))")
           :immediate-finish t
           :unnarrowed t))
        org-roam-mode-sections (list #'org-roam-backlinks-insert-section
                                     #'org-roam-reflinks-insert-section
                                     ;; #'org-roam-unlinked-references-insert-section
                                     ))
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .3 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .3 :height .5 :ttl nil :modeline nil :quit nil :slot 2))))

;; (custom-set-faces! '(org-transclusion :background "#b9c9b9")
;;                 '(org-transclusion-source :background "#ebf6fa"))

;;
;;; Agenda

(defun chloe/org-agenda-effort-string ()
  (let ((effort (org-entry-get (point) "Effort")))
    (if (> (length effort) 0)
        (concat "[" effort "]")
      "")))

(defun chloe/is-not-agenda-calendar-file ()
  "Returns true if this file is NOT in the directory `agenda/calendar'."
  (if (string-match-p "agenda/calendar/" (buffer-file-name))
      nil
    t))

(after! org
  (add-to-list 'org-modules 'org-checklist)
  (add-to-list 'org-modules 'org-habit)

  (setq org-log-done-with-time t
        org-log-reschedule 'time
        org-log-into-drawer t
        org-archive-location "%s_archive::datetree/"

        ;; start today
        org-agenda-start-on-weekday nil
        org-agenda-start-day "0d"

        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-refile-target-verify-function #'chloe/is-not-agenda-calendar-file ;; don't clutter refile targets with gcal calendar events
        org-deadline-warning-days 14
        org-agenda-span 7

        org-agenda-files (list chloe/org-agenda-directory (concat chloe/org-agenda-directory "calendar"))
        org-todo-keywords '((sequence "TODO(t)" "INPROG(i)" "|" "DONE(d!)")
                            (sequence "WAIT(w)" "|" "KILL(k)"))
        org-lowest-priority ?D

        org-agenda-custom-commands `((" " "Overview"
                                      ((agenda "" ((org-agenda-span 1)
                                                   (org-deadline-warning-days 365)))
                                       (tags-todo (concat "SCHEDULED=\"\"" "CATEGORY<>\"inbox\"" "/!")
                                                  ((org-agenda-overriding-header "Next actions (no deadline, unscheduled)")
                                                   (org-agenda-skip-function #'(org-agenda-skip-entry-if 'deadline))))
                                       (tags "CATEGORY=\"inbox\"|+inbox"
                                             ((org-agenda-overriding-header "Inbox"))))))
        org-agenda-sorting-strategy '((agenda habit-down deadline-up time-up category-keep priority-down)
                                      (todo category-keep priority-down)
                                      (tags category-keep priority-down)
                                      (search category-keep priority-down)))

  ;; note: using two %(...)'s only shows the first one, for some reason
  (let ((prefix " %(chloe/org-agenda-effort-string)  "))
    (setq org-agenda-prefix-format `((agenda . ,(concat " %i %-12:c%?-12t% s" prefix))
                                     (todo . ,(concat "%i %-12:c" prefix))
                                     (tags . ,(concat "%i %-12:c" prefix))
                                     (search . ,(concat "%i %-12:c" prefix)))))

  (let* ((created-string ":Created: %U\n")
         (property-string (concat ":PROPERTIES:\n" created-string ":END:")))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,(concat chloe/org-agenda-directory "inbox.org"))
             ,(concat "* TODO %?\n" property-string))
            ("i" "idea" entry (file ,(concat chloe/org-agenda-directory "inbox.org"))
             ,(concat "* TODO [#C] Idea: %?\n" property-string))
            ("n" "note" entry (file ,(concat chloe/org-agenda-directory "inbox.org"))
             ,(concat "* NOTE %?\n" property-string))))))

;; HACK Fix "Capture template ‘t’: Error in a Doom startup hook: doom-switch-buffer-hook, +org--restart-mode-h,"
(when IS-MAC
  (advice-add #'org-capture :around
              (lambda (fun &rest args)
                (letf! ((#'+org--restart-mode-h #'ignore))
                  (apply fun args)))))
