;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom install'
;; will do this for you). The `doom!' block below controls what modules are
;; enabled and in what order they will be loaded. Remember to run 'doom refresh'
;; after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;; (company           ; the ultimate code completion backend
       ;;  +childframe)

       (corfu
        +orderless
        +icons)

       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; ivy               ; a search engine for love and life
       (vertico
        +icons)

       :ui
       ;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        ;; +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; pretty-code       ; replace bits of code with pretty symbols
       ;; tabs              ; an tab bar for Emacs
       (treemacs          ; a project drawer, like neotree but cooler
        +lsp)
       ;; unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       ;; vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       ;;workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format
        ;; +onsave
        )  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;; word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired             ; making dired pretty [functional]
        +dirvish)
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;; shell             ; a terminal REPL for Emacs
       ;; term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;; (spell             ; tasing you for misspelling mispelling
       ;;  +enchant
       ;;  )
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;; biblio
       ;; debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       eval                ; run code, run (also, repls)
       ;;(eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (lsp
        ;; Disable lsp peek so that I can use consult + embark
        ;; +peek
        )
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       gh-copilot
       llm

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       ;;(cc                ; C/C++/Obj-C madness
       ;; +lsp
       ;;)
       ;; clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;(ess               ; emacs speaks statistics
       ;; +lsp
       ;; +lintr
       ;;)
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       (go                ; the hipster dialect
        +lsp
       )
       ;; (haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       json              ; At least it ain't XML
       ;(java
        ;+lsp) 
       (javascript        ; all(hope(abandon(ye(who(enter(here))))))
        +lsp
       )
       ;; julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex             ; writing papers in Emacs has never been so fun
        +latexmk
        +cdlatex
        )
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       (lua +lsp)               ; one-based indices? one-based indices
       (markdown          ; writing docs for people to ignore
        +grip)
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        ;; +dragndrop       ; drag & drop files/images into org buffers
        ;;+pretty          ; enables org-superstar and org-fancy-priorities
        ;;+hugo            ; use Emacs for hugo blogging
        ;; +ipython         ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        ;; +noter
        ;;+pomodoro        ; be fruitful with the tomato technique
        ;; +present         ; using org-mode for presentations
        ;; +journal
        ;; +roam
        )
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;; plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python            ; beautiful is better than ugly
        +lsp
        +pyright
        +pyenv
       )
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;(rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; +lsp
       ;;)
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       (sh                ; she sells {ba,z,fi}sh shells on the C xor
        +lsp
       )
       ;;solidity          ; do you need a blockchain? No.
       (swift ; who asked for emoji variables?
        +lsp)
       ;;terra             ; Earth and Moon in alignment for performance.
       (web               ; the tubes
        +lsp)
       yaml

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
