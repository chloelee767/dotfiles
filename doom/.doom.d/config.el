;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;
;;; Defaults

(setq delete-by-moving-to-trash t
      select-enable-clipboard nil ;; don't clutter OS clipboard with evil delete etc. Use + register for OS clipboard.
      enable-dir-local-variables t
      enable-local-variables t
      evil-ex-substitute-global t
      +latex-viewers '(pdf-tools okular)
      display-line-numbers-type 'relative
      frame-title-format  '("[" (:eval (projectile-project-name)) "] %b - Emacs")
      ;; note: uniquify is not compatible with persp-mode (:ui workspaces module)
      uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t)

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
      :g "C-S-n" #'restart-emacs-start-new-emacs ;; does not quit current emacs, st spawns new instance ;; FIXME macOS treats these as separate application types
      ;; :g "C-S-n" (lambda () (interactive) (start-process "open-emacs" nil "open" "-na" "Emacs.app")) ;; doesn't work either
      (:when IS-MAC
        :g "s-c" #'clipboard-kill-ring-save
        :g "s-v" #'clipboard-yank
        :g "s-n" #'restart-emacs-start-new-emacs))
(map! :leader
      :prefix ("y" . "yank")
      :desc "Copy last kill to clipboard" "c" #'chloe/copy-last-kill-to-clipboard
      :desc "Copy clipboard to emacs kill ring" "e" #'chloe/copy-clipboard-to-kill-ring)

(map! :map 'vterm-mode-map
      :g "C-S-v" #'chloe/vterm-yank-clipboard
      (:when IS-MAC
        :g "s-v" #'chloe/vterm-yank-clipboard))

;; project/directory search symbol at point using <leader>-s-P / <leader>-s-D, similar to <leader>-s-S
(map! :leader
      (:prefix "s"
       :desc "Search project for thing at point" "P" #'chloe/project-search-symbol-at-point
       :desc "Search other project" "C-p" #'+default/search-other-project
       :desc "Search current directory for thing at point" "D" #'chloe/cwd-search-symbol-at-point
       :desc "Search other directory" "C-d" #'+default/search-other-cwd))

;; disable tmm=menubar
;; (map! :g "`" nil
;;       :g "M-`" nil)

;; be similar to org
(map! :map 'markdown-mode-map
      "C-RET" #'markdown-insert-list-item
      "<C-return>" #'markdown-insert-list-item)

;; Unbind TAB for summoning corfu completion.
;; Completion automatically appears early enough.
;; (map! :map 'global-map
;;       :i "TAB" #'indent-for-tab-command
;;       :i "<tab>" #'indent-for-tab-command)

(setq tab-first-completion 'eol)

;; Fix yasnippet tab bindings.
;; This way yasnippet will take predence over corfu when snippet expansion is ongoing.
;; Note: with :i, sometimes yasnippet can still get overriden by corfu.
(map! :map 'yas-keymap
      :g "TAB" #'yas-next-field-or-maybe-expand
      :g "<tab>" #'yas-next-field-or-maybe-expand
      :g "S-TAB" #'yas-prev-field
      :g "<backtab>" #'yas-prev-field)

;; Fix buffer switching when :ui workspaces is disabled
(after! consult
  (defvar chloe/consult--source-file-buffer
    `(:name     "File Buffer"
      :narrow   ?f
      :category buffer
      :face     consult-buffer
      :history  buffer-name-history
      :state    ,#'consult--buffer-state
      :default  t
      :items
      ,(lambda () (consult--buffer-query :sort 'visibility
                                    :as #'consult--buffer-pair
                                    :predicate
                                    (lambda (buf)
                                      (buffer-file-name buf)))))
    "Source for `consult-buffer' consisting of buffers associated with a file")

  (defvar chloe/consult--source-non-file-buffer
    `(:name     "Non-File Buffer"
      :narrow   ?n
      :category buffer
      :face     consult-buffer
      :history  buffer-name-history
      :state    ,#'consult--buffer-state
      :items
      ,(lambda () (consult--buffer-query :sort 'visibility
                                    :as #'consult--buffer-pair
                                    :predicate
                                    (lambda (buf)
                                      (not (buffer-file-name buf))))))
    "Source for `consult-buffer' consisting of non-file buffers")

  (map! :leader
        :g "," (cmd! (consult-buffer '(chloe/consult--source-file-buffer)))
        :g "<" (cmd! (consult-buffer '(chloe/consult--source-file-buffer chloe/consult--source-non-file-buffer)))))
;;
;;; Visuals

(setq
 ;; doom-theme (if (member (string-to-number (substring (current-time-string) 11 13)) (number-sequence 7 17)) 'doom-tomorrow-day 'doom-tomorrow-night) ;; set theme based on time
 doom-theme 'doom-gruvbox
 ;; doom-theme 'doom-tomorrow-day
 ;; doom-theme 'doom-solarized-light
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

;; spelling
(after! spell-fu
  (setq spell-fu-word-delimit-camel-case t))

;; popups
;; don't close help and info until I really want to
;; (set-popup-rule! "^\\*info.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)
;; (set-popup-rule! "^\\*[Hh]elp.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)

;;
;;; Useful files and directories
;;; note: a lot of custom functions work under the assumption that directory paths end with /

(setq org-directory (if IS-MAC "~/Library/CloudStorage/Dropbox/Org/" "~/Dropbox/Org/")
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
;;; Programming

(after! prog-mode
  (which-function-mode 1))

(use-package! lsp-headerline
  :after lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-segments '(symbols)
        which-func-functions (list (lambda () (if lsp-mode (s-trim (substring-no-properties (lsp-headerline--build-symbol-string))) nil)))))

(defun chloe/toggle-which-function-position ()
  (interactive)
  (if (and lsp-mode lsp-headerline-breadcrumb-mode)
      (progn (which-function-mode 1)
             (lsp-headerline-breadcrumb-mode -1))
    (progn (which-function-mode -1)
           (lsp-headerline-breadcrumb-mode 1))))

;;
;;; Github copilot


(defvar chloe/no-copilot-modes '(shell-mode
                                 inferior-python-mode
                                 eshell-mode
                                 term-mode
                                 vterm-mode
                                 comint-mode
                                 compilation-mode
                                 debugger-mode
                                 dired-mode-hook
                                 compilation-mode-hook
                                 flutter-mode-hook
                                 minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun chloe/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (member major-mode chloe/no-copilot-modes))

(defvar chloe/go-indent 4)
(defvar chloe/elisp-indent 2)

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :hook (yaml-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-TAB" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-S-TAB" . 'copilot-accept-completion-by-word)
              ("C-S-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-disable-predicates #'chloe/copilot-disable-predicate)
  ;; copilot-indentatin-alist requires values to be symbols (ie. return true for symbolp)
  (add-to-list 'copilot-indentation-alist '(go-mode chloe/go-indent))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode chloe/elisp-indent))
  (map! :map copilot-mode-map
        :leader
        (:prefix ("l" . "copilot")
         :desc "next completion" "n" #'copilot-next-completion
         :desc "previous completion" "p" #'copilot-previous-completion
         :desc "accept word" "w" #'copilot-accept-completion-by-word
         :desc "accept line" "l" #'copilot-accept-completion-by-line
         :desc "accept all" "j" #'copilot-accept-completion)))

;;
;;; Magit

(after! magit
  (map! :map magit-mode-map
        :leader
        :prefix "g" "F" #'magit-blob-visit-file
        :prefix "gf" "F" #'magit-blob-visit-file)
  (map! :map magit-blob-mode-map
        :g "RET" #'magit-blob-visit-file
        :g "<return>" #'magit-blob-visit-file))

;;
;;; Programming languages

;; zmk
(add-to-list 'auto-mode-alist '("\\.keymap\\'" . c++-mode))

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
  (setq gofmt-command "goimports"
        lsp-go-goimports-local "github.com/carousell"
        gofmt-args (append gofmt-args '("-local" "github.com/carousell")))
  (add-hook 'before-save-hook 'gofmt-before-save)

  (map! :map go-mode-map
        :localleader
        (:prefix "ri" "c" #'chloe/go-cleanup-imports))

  ;; TODO open PR for this?
  (map! :map go-mode-map
        :localleader
        (:prefix ("g" . "generate")
                 "f" #'+go/generate-file
                 "l" #'+go/generate-line ;; remove?
                 "d" #'+go/generate-dir
                 "p" #'+go/generate-project)))


;; (setq lsp-clients-clangd-args '("-j=3"
;;                                 "--background-index"
;;                                 "--clang-tidy"
;;                                 "--completion-style=detailed"
;;                                 "--header-insertion=never"
;;                                 "--header-insertion-decorators=0"))
;; (after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; (add-hook 'c++-mode #'format-all-mode)
;; (use-package! clang-format
;;   :after cc-mode
;;   :init
;;   (setq-hook! 'c++-mode-hook +format-with-lsp nil)
;;   :config
;;   (set-formatter! 'clang-format #'clang-format-buffer :modes '(c++-mode))
;;   (map! ; :mode c++-mode-map
;;    :g "<f10>" #'lsp-clangd-find-other-file)
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
  ;; )

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


;; (custom-set-faces! '(org-transclusion :background "#b9c9b9")
;;                 '(org-transclusion-source :background "#ebf6fa"))

;; HACK org-element-cache: Unregistred buffer modifications caused by org-roam
;; (setq warning-suppress-types (append warning-suppress-types '((org-element-cache) (defvaralias))))

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
