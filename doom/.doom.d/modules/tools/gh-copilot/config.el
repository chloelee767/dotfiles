;;; tools/gh-copilot/config.el -*- lexical-binding: t; -*-

;; TODO fail more gracefully if copilot binary is missing

(defvar +gh-copilot-disable-modes '(shell-mode
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

(defun +gh-copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (member major-mode +gh-copilot-disable-modes))

(defun +gh-copilot-maybe-enable ()
  "Turn on `copilot-mode' only in buffers visiting a real file.

Copilot starts its language server eagerly the first time `copilot-mode' is
enabled in any buffer.  We hook it onto `prog-mode', but the daemon's initial
`*scratch*' buffer uses `lisp-interaction-mode', which derives from `prog-mode',
so a plain `prog-mode' hook fires there and starts the server at every daemon
launch (defeating lazy startup).  Gating on `buffer-file-name' keeps copilot --
and its server -- off for `*scratch*' and other non-file prog-mode buffers, so
the server only starts when we actually open code."
  (when buffer-file-name
    (copilot-mode 1)))

(use-package! copilot
  :hook (prog-mode . +gh-copilot-maybe-enable)
  :hook (yaml-mode . +gh-copilot-maybe-enable)
  :bind (:map copilot-completion-map
              ("C-TAB" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-S-TAB" . 'copilot-accept-completion-by-word)
              ("C-S-<tab>" . 'copilot-accept-completion-by-word)
              ("C-M-TAB" . 'copilot-accept-completion-by-line)
              ("C-M-<tab>" . 'copilot-accept-completion-by-line))
  :config
  (add-to-list 'copilot-disable-predicates #'+gh-copilot-disable-predicate)
  ;; copilot-indentatin-alist requires values to be symbols (ie. return true for symbolp)
  ;; TODO don't hardcode indentation
  (add-to-list 'copilot-indentation-alist '(go-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  ;; TODO rethink, these keybinds are difficult to use
  (map! :map copilot-mode-map
        :leader
        (:prefix ("l" . "copilot")
         :desc "next completion" "n" #'copilot-next-completion
         :desc "previous completion" "p" #'copilot-previous-completion
         :desc "accept word" "w" #'copilot-accept-completion-by-word
         :desc "accept line" "l" #'copilot-accept-completion-by-line
         :desc "accept all" "j" #'copilot-accept-completion)))
