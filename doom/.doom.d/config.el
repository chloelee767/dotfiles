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
      frame-title-format  '((:eval (doom-project-name)) " | %b - Emacs")
      ;; note: uniquify is not compatible with persp-mode (:ui workspaces module)
      uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      ;; Focus new window after splitting
      evil-split-window-below t
      evil-vsplit-window-right t
      company-show-quick-access t)

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

(map! :leader
      :prefix "b" "j" #'consult-bookmark)

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

;; Fix buffer switching when :ui workspaces is disabled.
;; Don't use after!, otherwise it will still use the default consult-buffer
;; the first time.
(use-package! consult
  :config
  (defun chloe/consult-file-buffer-pair (buffer)
    "Custom version of `consult--buffer-pair' that uses the full path of the file
relative to the project."
    (let ((filename (buffer-file-name buffer))
          (project-root (doom-project-root)))
      (if filename
          (cons
           (if (and project-root (file-in-directory-p filename project-root))
               (file-relative-name filename project-root)
             filename)
           buffer)
        (consult--buffer-pair buffer))))

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
                                    :as #'chloe/consult-file-buffer-pair
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

;; copied from :ui hl-todo module, except without XXX
(after! hl-todo
  (setq hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold))))


(after! lsp-mode
  (setq lsp-idle-delay 0.25))

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
              ("C-S-<tab>" . 'copilot-accept-completion-by-word)
              ("C-M-TAB" . 'copilot-accept-completion-by-line)
              ("C-M-<tab>" . 'copilot-accept-completion-by-line))
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

(setq transient-values '((magit-pull "--rebase" )))

(after! magit
  (map! :map magit-mode-map
        :leader
        :prefix "g" "F" #'magit-blob-visit-file
        :prefix "gf" "F" #'magit-blob-visit-file)
  (map! :map magit-blob-mode-map
        :g "RET" #'magit-blob-visit-file
        :g "<return>" #'magit-blob-visit-file))

(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil)
  (map! :leader
        :prefix "p" "t" #'magit-todos-list))

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
(add-hook 'protobuf-mode-hook (lambda () (push '("Rpc" "^[[:space:]]*rpc[[:space:]]+\\([[:alnum:]]+\\)" 1) imenu-generic-expression)))

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

;; this doesn't seem to be used
(use-package! lsp-mode
  :config
  (setq lsp-golangci-lint-fast t))

;; golangci-lint uses too much memory and cpu sometimes
(use-package! flycheck-golangci-lint
  :config
  (setq flycheck-golangci-lint-fast t))


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

