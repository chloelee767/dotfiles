;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(after! go-mode
  (set-docsets! 'go-mode "Go")
  (set-repl-handler! 'go-mode #'gorepl-run)

  (if (featurep! +lsp)
      (add-hook 'go-mode-local-vars-hook #'lsp!)
    (add-hook 'go-mode-hook #'go-eldoc-setup))

  (map! :map go-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "e" #'+go/play-buffer-or-region
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("ri" . "imports")
                 "a" #'go-import-add
                 "r" #'go-remove-unused-imports)
        (:prefix ("b" . "build")
         :desc "go run ." "r" (cmd! (compile "go run ."))
         :desc "go build" "b" (cmd! (compile "go build"))
         :desc "go clean" "c" (cmd! (compile "go clean")))
        (:prefix ("t" . "test")
                 "t" #'+go/test-rerun
                 "a" #'+go/test-all
                 "s" #'+go/test-single
                 "n" #'+go/test-nested
                 "g" #'go-gen-test-dwim
                 "G" #'go-gen-test-all
                 "e" #'go-gen-test-exported
                 (:prefix ("b" . "bench")
                          "s" #'+go/bench-single
                          "a" #'+go/bench-all))
        (:prefix ("g" . "generate")
                 "f" #'+go/generate-file
                 "l" #'+go/generate-line
                 "d" #'+go/generate-dir
                 "p" #'+go/generate-project)
        ))


(use-package! gorepl-mode
  :commands gorepl-run-load-current-file)


(use-package! company-go
  :when (featurep! :completion company)
  :unless (featurep! +lsp)
  :after go-mode
  :config
  (set-company-backend! 'go-mode 'company-go)
  (setq company-go-show-annotation t))

;; (use-package! flycheck-golangci-lint
;;   :when (and (modulep! :checkers syntax)
;;              (not (modulep! :checkers syntax +flymake)))
;;   :hook (go-mode . flycheck-golangci-lint-setup))

;; golangci-lint server
;; (after! lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(go-mode . "golangci-lint"))
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection "golangci-server")
;;                     ;; :major-modes '(go-mode)
;;                     :activation-fn (lsp-activate-on "golangci-lint")
;;                     :server-id 'golangci-lint
;;                     ;; :language-id "go"
;;                     :priority 0
;;                     :add-on? t
;;                     :library-folders-fn #'lsp-go--library-default-directories)))
