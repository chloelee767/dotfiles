;;; ../dotfiles/doom/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun chloe/set-file-local-org-download-dir ()
  "Still need to manually refresh the file local variables afterwards."
  (interactive)
  (if-let (filename (buffer-file-name))
      (progn
        (add-file-local-variable 'org-download-image-dir
                                 (concat (file-name-sans-extension (file-name-nondirectory filename)) "_images"))
        (message "Refresh file local variables now."))
    (error "Couldn't get filename of current buffer")))

;;;###autoload
(defun chloe/yank-buffer-filename-only ()
  "Copy the current buffer's filename, exlucluding directories, to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't get filename of current buffer")))

;;;###autoload
(defun chloe/chmod-current-file ()
  "Change the file mode bits of the current file, similar to chmod. Interactively prompts for the file modes (eg. \"+x\")."
  (interactive)
  (chmod (buffer-file-name) (read-file-modes nil (buffer-file-name))))

;;;###autoload
(defun chloe/make-current-file-executable ()
  (interactive)
  (chmod (buffer-file-name) (file-modes-symbolic-to-number "+x" (file-modes (buffer-file-name)))))

;;;###autoload
(defun chloe/copy-last-kill-to-clipboard ()
  "Copy the last killed text to the system clipboard."
  (interactive)
  (with-temp-buffer
    (yank)
    (clipboard-kill-region (point-min) (point-max))))

;;;###autoload
(defun chloe/copy-clipboard-to-kill-ring ()
  "Copy clipboard contents to emacs kill ring."
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (kill-region (point-min) (point-max))))

;;;###autoload
(defun chloe/vterm-yank-clipboard ()
  "Paste clipboard contents into vterm"
  (interactive)
  (progn (chloe/copy-clipboard-to-kill-ring) (vterm-yank)))

;;;###autoload
(defun chloe/project-search-symbol-at-point (&optional arg)
  (interactive "P")
  (+vertico/project-search arg (thing-at-point 'symbol)))

;;;###autoload
(defun chloe/go-cleanup-imports ()
    (interactive)
    (save-excursion
      (go-goto-imports)
      ;; remove blank lines before gofmt
      (evil-command-window-ex-execute
       (concat "1,"
               (int-to-string (line-number-at-pos (point) t))
               "s/\n\n+/\n"))
      (gofmt)
      (save-buffer)))

;;
;;; go generate

(defun +go--spawn (cmd)
  (save-selected-window
    (compile cmd)))

(defun +go--generate (dir args)
  (let ((cd-cmd (concat "cd " dir))
        (generate-cmd (concat "go generate " args)))
    (+go--spawn (concat cd-cmd " && " generate-cmd))))

;;;###autoload
(defun +go/generate-file ()
  "Run go generate for the current file only."
  (interactive)
  (if buffer-file-name
      (+go--generate default-directory (file-name-nondirectory buffer-file-name))
    (error "Couldn't get filename of curent buffer")))

;;;###autoload
(defun +go/generate-dir ()
  "Run go generate for the current directory recursively."
  (interactive)
  (+go--generate default-directory "./..."))

;;;###autoload
(defun +go/generate-project ()
  "Run go generate for the entire project."
  (interactive)
  ;; interactively choose project root if we cannot find it
  (if-let ((project (project-current t nil)))
      (+go--generate (file-truename (project-root project)) "./..."))
  (error "Couldn't get current project."))

;;;###autoload
(defun +go/generate-line ()
  "Run go generate for just the line at the current point."
  (interactive)
  (let ((curr-line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (regexp (rx line-start "//go:generate")))
    (cond ((string-match "\"" curr-line-string)
           (error "go generate cannot handle quotes (I think)"))

          ((string-match regexp curr-line-string)
           (let ((run-opt (concat "-run=\"" curr-line-string "\""))
                 (filename (file-name-nondirectory buffer-file-name)))
             (+go--generate default-directory (concat run-opt " " filename))))

          (t (error "Line does not begin with //go:generate")))))
