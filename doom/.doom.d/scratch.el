;;; ../dotfiles/doom/.doom.d/scratch.el -*- lexical-binding: t; -*-


;; FIXME the variable is being set, but the keyword isn't being highlighted
(defun chloe/go-add-deprecated-keyword ()
  (make-local-variable 'hl-todo-keyword-faces)
  (if (and hl-todo-mode (eq major-mode #'go-mode))
      (add-to-list 'hl-todo-keyword-faces
                  '("Deprecated" font-lock-doc-face bold)
                  t
                  (lambda (a b) (string-equal (car a) (car b))))))

(add-hook 'go-mode-hook #'chloe/go-add-deprecated-keyword)
(add-hook 'hl-todo-mode-hook #'chloe/go-add-deprecated-keyword)


(defun chloe/vertico-embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

(defun embark-consult-export-xref (items)
  "Create an xref buffer listing ITEMS."
  (cl-flet ((xref-items (items)
              (mapcar (lambda (item)
                        (let ((p (get-text-property 0 'consult-xref item)))
                          (pp p)
                          (message "xref-match-item-location:")
                          (pp (xref-match-item-location p))
                          p))
                      items)))
    (let ((fetcher consult-xref--fetcher)
          (input (minibuffer-contents)))
      (set-buffer
       (chloe/xref--show-xref-buffer
        (lambda ()
          (let ((candidates (funcall fetcher)))
            (if (null (cdr candidates))
                candidates
              (catch 'xref-items
                (minibuffer-with-setup-hook
                    (lambda ()
                      (insert input)
                      (add-hook
                       'minibuffer-exit-hook
                       (lambda ()
                         (throw 'xref-items
                                (xref-items
                                 (or
                                  (let ((res (plist-get
                                   (embark--maybe-transform-candidates)
                                   :candidates)))
                                    (message "plist-get result:")
                                    (pp res)
                                    res)
                                  (user-error "No candidates for export")))))
                       nil t))
                  (consult-xref fetcher))))))
        `((fetched-xrefs . ,(xref-items items))
          (window . ,(embark--target-window))
          (auto-jump . ,xref-auto-jump-to-first-xref)
          (display-action)))))))

(defun chloe/xref--show-xref-buffer (fetcher alist)
  (cl-assert (functionp fetcher))
  (let* ((xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher)))
         (xref-alist (xref--analyze xrefs))
         (dd default-directory)
         buf)
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (xref--ensure-default-directory dd (current-buffer))
      (xref--xref-buffer-mode)
      (chloe/xref--show-common-initialize xref-alist fetcher alist)
      (setq mode-line-process (list xref-mode-line-matches))
      (pop-to-buffer (current-buffer))
      (setq buf (current-buffer)))
    (xref--auto-jump-first buf (assoc-default 'auto-jump alist))
    buf))

(defun chloe/xref--show-common-initialize (xref-alist fetcher alist)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (inhibit-modification-hooks t))
    (erase-buffer)
    (setq overlay-arrow-position nil)
    (chloe/xref--insert-xrefs xref-alist)
    (add-hook 'post-command-hook #'xref--apply-truncation nil t)
    (goto-char (point-min))
    (setq xref--original-window (assoc-default 'window alist)
          xref--original-window-intent (assoc-default 'display-action alist))
    (setq xref--fetcher fetcher)))


(defun chloe/xref--insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...), where
GROUP is a string for decoration purposes and XREF is an
`xref-item' object."
  (require 'compile) ; For the compilation faces.
  (setq xref-num-matches-found 0)
  (cl-loop for (group . xrefs) in xref-alist
           for max-line = (cl-loop for xref in xrefs
                                   maximize (xref-location-line
                                             (xref-item-location xref)))
           for line-format = (and max-line
                                  (format
                                   #("%%%dd:" 0 4 (face xref-line-number) 5 6 (face shadow))
                                   (1+ (floor (log max-line 10)))))
           with item-text-props = (list 'mouse-face 'highlight
                                        'keymap xref--button-map
                                        'help-echo
                                        (concat "mouse-2: display in another window, "
                                                "RET or mouse-1: follow reference"))
           with prev-group = nil
           with prev-line = nil
           do
           ;; (xref--insert-propertized '(face xref-file-header xref-group t)
           ;;                           group "\n")
           (dolist (xref xrefs)
             (cl-incf xref-num-matches-found)
             (pcase-let (((cl-struct xref-item summary location) xref))
               (let* ((line (xref-location-line location))
                      (prefix
                       (cond
                        ((not line) "  ")
                        ((and (equal line prev-line)
                              (equal prev-group group))
                         "")
                        (t (format line-format line)))))
                 ;; Render multiple matches on the same line, together.
                 (when (and (equal prev-group group)
                            (or (null line)
                                (not (equal prev-line line))))
                   (insert "\n"))
                 (xref--insert-propertized (nconc (list 'xref-item xref)
                                                  item-text-props)
                                           prefix summary)
                 (setq prev-line line
                       prev-group group))))
           (insert "\n"))
  (add-to-invisibility-spec '(ellipsis . t))
  (save-excursion
    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (xref--apply-truncation)))
  (run-hooks 'xref-after-update-hook))
