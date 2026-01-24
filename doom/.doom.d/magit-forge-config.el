;;; ../dotfiles/doom/.doom.d/magit-forge-config.el -*- lexical-binding: t; -*-

(setq auth-sources '("~/.authinfo"))
(use-package! forge
  :config
  ; don't show any forge topics the status buffer
  (setq forge-status-buffer-default-topic-filters (forge--topics-spec :type nil))
  ;; (setq forge-status-buffer-default-topic-filters
  ;;       (forge--topics-spec
  ;;       :type 'pullreq
  ;;       :active nil
  ;;       :state 'open
  ;;       :order 'newest))
  )

;; (defun chloe/forge-new-prs ()
;;   (forge-insert-pullreqs
;;    (forge--topics-spec :type 'pullreq :active nil :state 'open :status 'inbox :order 'newest)
;;    "Open PRs"))

;; (after! magit
;;   (setq magit-status-sections-hook
;;         '(magit-insert-status-headers
;;           magit-insert-merge-log
;;           magit-insert-rebase-sequence
;;           magit-insert-am-sequence
;;           magit-insert-sequencer-sequence
;;           magit-insert-bisect-output
;;           magit-insert-bisect-rest
;;           magit-insert-bisect-log
;;           magit-insert-untracked-files
;;           magit-insert-unstaged-changes
;;           magit-insert-staged-changes
;;           magit-insert-stashes
;;           magit-insert-unpushed-to-pushremote
;;           magit-insert-unpushed-to-upstream-or-recent
;;           magit-insert-unpulled-from-pushremote
;;           magit-insert-unpulled-from-upstream
;;           ;; forge
;;           forge-insert-pullreqs)))
