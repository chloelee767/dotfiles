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
      frame-title-format  '("[" (:eval (safe-persp-name (get-current-persp))) "] %b - Emacs")
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
      :g "C-S-n" #'restart-emacs-start-new-emacs ;; does not quit current emacs, just spawns new instance
      (:when IS-MAC
        :g "s-c" #'clipboard-kill-ring-save
        :g "s-v" #'clipboard-yank
        :g "s-n" #'restart-emacs-start-new-emacs))
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
;;; note: a lot of custom functions work under the assumption that directory paths end with /

(setq org-directory "~/Dropbox/Org/"
      org-roam-directory (concat org-directory "notes/")
      org-roam-dailies-directory "daily/"
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
  (map! :leader
        (:prefix "t"
         :desc "Global zen mode" "Z" #'global-writeroom-mode))
  (setq +zen-text-scale 0)
  :config
  (setq writeroom-extra-line-spacing 0.4
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

(defun string-remove-surrounding (toremove str)
  (string-remove-suffix toremove (string-remove-prefix toremove str)))

(defun chloe/org-roam-note-exists-p (notename)
  ;; notename: filename sans extension
  ;; returns true iff the note file itself exists
  ;; will return false even if children exists but note file itself does not exist
  (file-exists-p (concat org-roam-directory notename ".org")))

(defun chloe/org-roam-note-children (notename)
  ;; notename : filename sans extension
  ;; returns absolute file paths
  ;; Eg. ("/Users/chloelee/Dropbox/Org/notes/cs.databases.database-apps.org" "/Users/chloelee/Dropbox/Org/notes/cs.databases.mysql.org" "/Users/chloelee/Dropbox/Org/notes/cs.databases.postgres.org")
  (let ((likequery (concat "\"" (expand-file-name org-roam-directory) notename "._%.org" "\"")))
    (mapcar #'car
            ;; db query returns list of lists
            (org-roam-db-query [:select file :from files :where (like file $r1)] likequery))))


(defun chloe/org-roam-rename-note--rename (oldname newname notename)
  ;; Returns renamed note name
  (if (string-prefix-p (concat oldname ".") notename)
      (concat newname (string-remove-prefix oldname notename))
    ;; should not happen
    (error (concat "Notename does not have correct prefix. " "Notename:" notename " Prefix:" oldname ))))
;; (chloe/org-roam-rename-note--rename "cs" "compsci" "cs.databases.mysql")

(defun string-seq-to-hashset (seq)
  (let ((hashset (make-hash-table :test 'equal :size (length seq))))
    (progn
      (mapc (lambda (x) (puthash x 't hashset)) seq)
      hashset)))
(defun hashset-contains-p (value hashset)
  (gethash value hashset nil))

(defun chloe/org-roam-rename-note--do-rename (old-notename new-notename)
  ;; rename a single file
  (let ((new-fullpath (concat org-roam-directory new-notename ".org"))
        (old-fullpath (concat org-roam-directory old-notename ".org")))
    (rename-file old-fullpath new-fullpath)
    (doom-files--update-refs old-fullpath new-fullpath)))
;; (chloe/org-roam-rename-note--do-rename "test" "test2" "test.a.foo")

(defun chloe/org-roam-note-parent (notename)
  ;; Returns name of parent note, or nil if this is a root-level note
  (if (file-name-extension notename)
      (file-name-sans-extension notename)
    nil))
;; (chloe/org-roam-note-parent "test")

(defun chloe/org-roam-rename-note--check-collisions (oldname newname oldname-children)
  (cl-dolist (notename oldname-children)
    (let ((new-notename (chloe/org-roam-rename-note--rename oldname newname notename)))
      (when (chloe/org-roam-note-exists-p new-notename)
        (cl-return notename)))))

;; Rename file
;; renames all children
;; If collision, error
(defun chloe/org-roam-rename-note (oldname newname)
  ;; oldname and newname should be notenames
  (interactive "Mfrom: \nMto: ")

  ;; make sure at most 1 of the files exist
  (if (and (chloe/org-roam-note-exists-p oldname) (chloe/org-roam-note-exists-p newname))
      (error (s-format "Both $0 and $1 exist, please manually combine files & delete one first"
                       'elt (vector oldname newname)))

    (let* ((oldname-children (mapcar (lambda (p) (file-name-sans-extension (file-name-nondirectory p))) (chloe/org-roam-note-children oldname)))
           (colliding-notename (chloe/org-roam-rename-note--check-collisions oldname newname oldname-children)))
      (if colliding-notename
          (error (s-format "Cannot rename $0 , $1 already exists"
                           'elt (vector colliding-notename (chloe/org-roam-rename-note--rename oldname newname colliding-notename))))
        ;; perform rename
        (progn
          ;; rename file itself
          (if (chloe/org-roam-note-exists-p oldname)
              (chloe/org-roam-rename-note--do-rename oldname newname))

          ;; rename children
          (mapc (lambda (notename) (chloe/org-roam-rename-note--do-rename notename (chloe/org-roam-rename-note--rename oldname newname notename)))
                oldname-children))))))

(defun chloe/org-roam-concat-note-parts (parent nextpart)
  (if (equal parent "") nextpart (concat parent "." nextpart)))

;; (chloe/org-roam-concat-note-parts "" "abc.def") ;; => "abc.def"
;; (chloe/org-roam-concat-note-parts "xyz" "abc.def") ;; => "xyz.abc.def"
;; (apply #'chloe/org-roam-concat-note-parts (list "xyz" "abc.def"))

(defun chloe/org-roam-child-hierarchies (notename &optional prepend-prefix)
  (let* ((children-filepaths-list (if (equal notename "") (org-roam-list-files) (chloe/org-roam-note-children notename)))
         (children-notenames (mapcar (lambda (x) (f-filename x)) children-filepaths-list)))
    (chloe/org-roam-child-hierarchies--internal notename children-notenames prepend-prefix)))

;; (chloe/org-roam-child-hierarchies "carousell")

(defun chloe/org-roam-child-hierarchies--internal (notename children-notenames prepend-prefix)
  (-uniq (mapcar
          (lambda (x)
            (let ((nextpart (chloe/org-roam-notename-nextpart x notename)))
              (if (and prepend-prefix (not (equal notename "")))
                  (concat notename "." nextpart)
                nextpart)))
          children-notenames)))

;; (chloe/org-roam-child-hierarchies--internal "abc" '("abc.def.123" "abc.hij.123" "abc.def.456" "abc.def.123.456") nil) ;; => '("def" "hij")
;; (chloe/org-roam-child-hierarchies--internal "abc" '("abc.def.123" "abc.hij.123" "abc.def.456" "abc.def.123.456") 't) ;; => '("abc.def" "abc.hij")
;; (chloe/org-roam-child-hierarchies--internal "" '("abc.def.123" "abc.hij.123" "abc.def.456" "abc.def.123.456" "xyz") 't) ;; => '("abc" "xyz")

(defun chloe/org-roam-notename-nextpart (notename prefix)
  (if (or (equal prefix "") (s-prefix-p (concat prefix ".") notename))
      (let* ((num-chars-to-remove (if (equal prefix "") 0 (+ 1 (length prefix)))) ;; prefix + "."
             (notename-prefix-removed (substring notename num-chars-to-remove))
             (first-dot-index (s-index-of "." notename-prefix-removed)))
        (if first-dot-index (substring notename-prefix-removed 0 first-dot-index) notename-prefix-removed))
    (error (concat "Note " notename " is not a child of " prefix))))

;; (chloe/org-roam-notename-nextpart "abc.def.123.456" "") ;; => "abc"
;; (chloe/org-roam-notename-nextpart "abc.def.123.456" "abc") ;; => "def"
;; (chloe/org-roam-notename-nextpart "abc.def.123.456" "abc.def") ;; => "123"
;; (chloe/org-roam-notename-nextpart "abc.def.123.456" "a") ;; => ERROR

(use-package! org-roam
  :after org
  :init
  (map! :after org
        :leader (:prefix "n" "d" nil))
  (map! :after org
        :leader
        (:prefix ("n" . "Notes")
         :desc "Find" "f" #'org-roam-node-find
         :desc "Find Parent" "h" #'chloe/org-roam-node-find-parent
         :desc "Find Siblings" "j" #'chloe/org-roam-node-find-siblings
         :desc "Find Children" "l" #'chloe/org-roam-node-find-children
         :desc "Backlinks buffer" "b" #'org-roam-buffer-toggle
         :desc "Dedicated backlinks buffer" "B" #'org-roam-buffer-display-dedicated
         :desc "Rename notes" "r" #'chloe/org-roam-rename-note
         (:prefix ("d" . "Daily")
          :desc "Today" "d" #'org-roam-dailies-goto-today
          :desc "Previous" "p" #'org-roam-dailies-goto-previous-note
          :desc "Next" "n" #'org-roam-dailies-goto-next-note)))

  :config
  (org-roam-db-autosync-mode)
  (map! :map org-mode-map
        :i "C-c i" #'org-roam-node-insert)

  (setq org-roam-db-location "~/org-roam.db"
        org-roam-node-display-template "${my-title:*}${olp:*}" ;; OLP = outline path
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%(chloe/org-roam-input-title-to-file-name \"${title}\").org"
                              "%(chloe/org-roam-input-title-to-file-title \"${title}\")")
           :immediate-finish t
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d-%a>.org"
                              "#+title: %<%Y-%m-%d %a>\n")))
        org-roam-mode-sections (list #'org-roam-backlinks-section
                                     #'org-roam-reflinks-section
                                     ;; #'org-roam-unlinked-references-section
                                     ))
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .2 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .2 :height .5 :ttl nil :modeline nil :quit nil :slot 2))))


;; (custom-set-faces! '(org-transclusion :background "#b9c9b9")
;;                 '(org-transclusion-source :background "#ebf6fa"))

(defun chloe/org-get-file-id (path)
  (with-temp-buffer
    (unwind-protect
        (progn
          (insert-file-contents path)
          (org-id-get (point-min) nil nil))
      (org-element-cache-reset))))

;; (chloe/org-get-file-id (concat org-roam-directory "index.org"))

(use-package! treemacs-treelib
  :after treemacs
  :config

  (defun notes-hierarchy-RET-note-action (&optional use-other-window)
    (let* ((note-info (treemacs-button-get (treemacs-current-button) :note-info))
           (filepath (concat org-roam-directory
                             (apply #'chloe/org-roam-concat-note-parts note-info)
                             ".org")))
      (if (file-exists-p filepath)
          (if-let ((org-id (chloe/org-get-file-id filepath)))
              (progn
                (other-window 1)
                (org-roam-node-visit (org-roam-populate (org-roam-node-create :id org-id)) use-other-window))
            (error "Org roam file does not have id"))
        (message "Note does not exist")))) ;; TODO option to create?


  (treemacs-define-entry-node-type
      chloe-notes
    :label (propertize "Notes" 'face 'font-lock-keyword-face)
    :key 'chloe-notes
    :open-icon "+"
    :closed-icon "-"
    :children (mapcar (lambda (x) (list "" x)) (chloe/org-roam-child-hierarchies ""))
    :child-type 'chloe-notes--note)

  (treemacs-define-expandable-node-type
      chloe-notes--note
    :closed-icon "+"
    :open-icon "-"
    :label (propertize (nth 1 item) 'face 'font-lock-variable-name-face)
    :key (nth 1 item)
    :children (mapcar (lambda (x) (list (chloe/org-roam-concat-note-parts (nth 0 item) (nth 1 item)) x))(chloe/org-roam-child-hierarchies (chloe/org-roam-concat-note-parts (nth 0 item) (nth 1 item))))
    :child-type 'chloe-notes--note
    :ret-action #'notes-hierarchy-RET-note-action
    :more-properties `(:note-info ,item)) ;;  list of (parent-notename nextpart)

  (treemacs-enable-top-level-extension
   :extension 'chloe-notes
   :position 'top
   :predicate (lambda (_) 't)))

;; HACK org-element-cache: Unregistred buffer modifications caused by org-roam
(setq warning-suppress-types (append warning-suppress-types '((org-element-cache) (defvaralias))))

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
