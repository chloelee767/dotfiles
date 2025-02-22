;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;
;;; Defaults

(setq delete-by-moving-to-trash t
      select-enable-clipboard nil ; don't clutter OS clipboard with evil delete etc. Use + register for OS clipboard.
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
      evil-vsplit-window-right t)

(global-visual-line-mode 1)
(global-subword-mode 1)

;; Turn off auto-fill mode
(auto-fill-mode -1)
(add-hook 'org-mode-hook #'turn-off-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; unbind `evil-emacs-state'
(map! :im "C-z" nil)
(map! :map magit-mode-map :n "C-z" nil)

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
      :desc "Copy clipboard to emacs kill ring" "e" #'chloe/copy-clipboard-to-kill-ring
      ; also bound to SPC i y
      :desc "yank pop" "p" #'+default/yank-pop)

(map! :map 'vterm-mode-map
      :g "C-S-v" #'chloe/vterm-yank-clipboard
      (:when IS-MAC
        :g "s-v" #'chloe/vterm-yank-clipboard))

(map! :leader
      (:prefix "s"
       :desc "Search buffer for thing at point" "S" #'chloe/search-symbol-at-point

        ;; project/directory search symbol at point using <leader>-s-P / <leader>-s-D, similar to <leader>-s-S
       :desc "Search project for thing at point" "P" #'chloe/project-search-symbol-at-point
       :desc "Search other project" "C-p" #'+default/search-other-project
       :desc "Search directory for thing at point" "D" #'chloe/cwd-search-symbol-at-point
       :desc "Search other directory" "C-d" #'+default/search-other-cwd))

;; disable tmm=menubar
;; (map! :g "`" nil
;;       :g "M-`" nil)

;; be similar to org
(map! :map 'markdown-mode-map
      "C-RET" #'markdown-insert-list-item
      "<C-return>" #'markdown-insert-list-item)

;; (setq tab-first-completion 'eol)
(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets t
        +corfu-want-tab-prefer-navigating-snippets nil
        +corfu-want-tab-prefer-navigating-org-tables t))

;; don't reorder completions from the language server
(setq-hook! 'lsp-mode-hook corfu-sort-function nil)

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
             (abbreviate-file-name filename))
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

;; in `consult-buffer', don't annotate the file name again next to a file buffer
(after! marginalia
  (advice-add #'marginalia--buffer-file
              :around (lambda (oldfun buffer) (if (buffer-file-name buffer) "" (funcall oldfun buffer)))))

;; TODO learn how to use dirvish?
(use-package! treemacs
  :config
  (map! :leader (:prefix "o" "p" #'+treemacs/toggle)))


;;
;;; Visuals

(setq
 ;; doom-theme (if (member (string-to-number (substring (current-time-string) 11 13)) (number-sequence 7 17)) 'doom-tomorrow-day 'doom-tomorrow-night) ;; set theme based on time
 doom-theme 'doom-tomorrow-night
 ;; doom-theme 'doom-solarized-light
 ;; doom-font (font-spec :family "JetBrainsMono Nerd Font" :size (if IS-MAC 13.0 11.0))
 ;; line-spacing 0.2
 ;; doom-themes-enable-bold nil
 doom-font (font-spec :family "Iosevka SS14" :size (if IS-MAC 13.0 11.0))
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
;; don't show file size in modeline
(setq size-indication-mode nil)
(remove-hook 'doom-modeline-mode-hook #'size-indication-mode)

;; spelling
(after! spell-fu
  (setq spell-fu-word-delimit-camel-case t))

;; popups
;; don't close these kinds of popups until I really want to
;; (set-popup-rule! "^\\*info.*" :size 80 :side 'right :ttl nil :select nil :quit 'current)
(set-popup-rule! "^\\*Embark Export.*" :ttl nil :select t :quit 'current)
(set-popup-rule! "^\\*[Hh]elp.*" :ttl 600 :select nil :quit 'current) ;; 10 mins ttl

;;
;;; Useful files and directories
;;; note: a lot of custom functions work under the assumption that directory paths end with /

(setq org-directory (if IS-MAC "~/Library/CloudStorage/Dropbox/Org/" "~/Dropbox/Org/")
      chloe/documents-directory "~/Documents/"
      chloe/gosrc-directory "~/go/src/"
      chloe/carousell-gocode-directory (concat chloe/gosrc-directory "github.com/carousell/")
      chloe/dropbox-directory "~/Dropbox/")

;;
;;; Shortcuts and utils

(map! :leader (:prefix "f" "f" nil)) ;; SPC f f is already bound counsel-find-file, but I don't mind since find file is bound to SPC . as well

(map! :leader
      (:prefix "f"
               ;; shortcuts to useful folders
               (:prefix ("f" . "favourites")
                :desc "Home" "h" (cmd! (doom-project-browse "~/"))
                :desc "Documents" "d" (cmd! (doom-project-browse chloe/documents-directory))
                :desc "Code" "c" (cmd! (doom-project-browse "~/Code/"))
                :desc "Go (Carousell)" "g" (cmd! (doom-project-browse chloe/carousell-gocode-directory))
                :desc "Go src" "G" (cmd! (doom-project-browse chloe/gosrc-directory))
                :desc "Dotfiles" "t" (cmd! (doom-project-browse "~/dotfiles/"))
                :desc "Dotfiles (find in project)" "T" (cmd! (doom-project-find-file "~/dotfiles/"))
                :desc "/" "/" (cmd! (doom-project-browse "/"))
                :desc "Org folder" "o" (cmd! (doom-project-browse org-directory)))

               ;; file utils
               :desc "Yank file path" "y" #'chloe/yank-buffer-path
               :desc "Yank file path from project" "Y" #'chloe/yank-buffer-path-relative-to-project
               :desc "Yank filename only" "C-y" #'chloe/yank-buffer-filename-only
               :desc "chmod" "x" #'chloe/chmod-current-file
               :desc "make executable" "X" #'chloe/make-current-file-executable)

      ;; more utils
      (:prefix "o" :desc "Google calendar" "c" #'(cmd! (browse-url "https://calendar.google.com"))))

;;
;;; Programming

(after! lsp-mode
  ;; go overlay hints
  ;; see:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4357
  ;; https://github.com/golang/tools/blob/master/gopls/doc/inlayHints.md
  (lsp-register-custom-settings
   '(("gopls.hints" ((parameterNames . t)))))

  (setq lsp-idle-delay 0.25
        lsp-inlay-hint-enable nil ; turn on as needed
        lsp-modeline-code-actions-enable nil ; flycheck already shows this in the modeline
        ))

(which-function-mode 1)

(defun chloe/which-func-current-truncated (len)
  (s-truncate len
              (string-replace "%" "%%"
                              (or (gethash (selected-window) which-func-table)
                                  which-func-unknown))))
(setq ;; which-func-display 'header ; Note: only available from emacs 30.1
 which-func-unknown ""
 ;; same as which-func-format, except that the function name is truncated beyond a certain point
 which-func-format `("["
                     (:propertize which-func-current
                                  (:eval (chloe/which-func-current-truncated 40)) local-map
                                  ,which-func-keymap
                                  face which-func
                                  mouse-face mode-line-highlight
                                  help-echo ,(concat
                                              "Current function\n"
                                              "mouse-1: go to beginning\n"
                                              "mouse-2: toggle rest visibility\n"
                                              "mouse-3: go to end"))
                     "]"))


;; Use lsp-headerline instead of which-func if available
(use-package! lsp-headerline
  :after lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-enable t
        ;; Use lsp to implement which-func
        ;; which-func-functions (list (lambda () (if lsp-mode (s-replace-regexp "[[:space:]]+" " " (s-trim (substring-no-properties (lsp-headerline--build-symbol-string)))) nil)))
        lsp-headerline-breadcrumb-segments '(symbols)))
(add-hook 'lsp-headerline-breadcrumb-mode-hook
          (lambda () (setq-local which-function-mode (if lsp-headerline-breadcrumb-mode nil t))))

;;
;;; Github copilot

(defvar chloe/no-copilot-modes '(shell-mode
                                 emacs-lisp-mode
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

(setq transient-values '((magit-pull "--rebase")))

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
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?" ; make colon optional
        magit-todos-exclude-globs (append magit-todos-exclude-globs '("*.pb.go" "vendor/")))
  (define-key magit-todos-section-map "j" nil)
  (map! :leader
        :prefix "p" "t" #'magit-todos-list))

;; (load! "magit-forge-config.el" doom-user-dir t)

;;
;;; Programming languages

;; shell
(add-to-list 'auto-mode-alist '("\\.zshrc\\..+" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.profile\\..+" . sh-mode))

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
        (:prefix "ri" "c" #'chloe/go-cleanup-imports)))

;; this doesn't seem to be used
;; (use-package! lsp-mode
;;   :config
;;   (setq lsp-golangci-lint-fast t))

;; For projects with large dependencies, golangci-lint is slow and extremely
;; resource-intensive, even when the project itself is small.
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

;;
;;; Org mode

(after! org
  ;; edit src blocks in current window
  (setq org-src-window-setup 'current-window)
  (set-popup-rule! "^\\*Org Src" :ignore t))

;;
;;; Helpful mode

;; helpful buffers history
;; copied from https://github.com/Wilfred/helpful/issues/250

(defvar +helpful-buffer-ring-size 20
    "How many buffers are stored for use with `+helpful-next'.")

(defvar +helpful--buffer-ring (make-ring +helpful-buffer-ring-size)
  "Ring that stores the current Helpful buffer history.")

(after! helpful
  (defadvice! +helpful--new-buffer-a (help-buf)
    "Update the buffer ring according to the current buffer and HELP-BUF."
    :filter-return #'helpful--buffer
    (let ((buf-ring +helpful--buffer-ring))
      (let ((newer-buffers (or (+helpful--buffer-index) 0)))
        (dotimes (_ newer-buffers) (ring-remove buf-ring 0)))
      (when (/= (ring-size buf-ring) +helpful-buffer-ring-size)
        (ring-resize buf-ring +helpful-buffer-ring-size))
      (ring-insert buf-ring help-buf))))

(map! :map helpful-mode-map
      :n "[ b" #'+helpful-previous
      :n "] b" #'+helpful-next)
