;;; ../dotfiles/doom/.doom.d/autoload.el -*- lexical-binding: t; -*-

(defun chloe/get-selected-lines-str ()
  (if (doom-region-active-p)
      (let ((beg (line-number-at-pos (doom-region-beginning)))
            (end (line-number-at-pos (- (doom-region-end) 1))))
        (if (= beg end) (number-to-string beg) (format "%d-%d" beg end)))))

(defun chloe--yank-path (fn)
  "Calls fn, a function which yanks the current path to the clipboard, except
that it includes the line numbers if a region is active."
  (funcall fn)
  (when-let* ((lines (chloe/get-selected-lines-str))
              (path (pop kill-ring))
              (final-path (if lines (s-concat path ":" lines) path)))
    (kill-new final-path)
    (message "Copied path: %s" final-path)))

(defun chloe--yank-buffer-filename-only ()
  "Copy the current buffer's filename, exlucluding directories, to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't get filename of current buffer")))

;;;###autoload
(defun chloe/yank-buffer-path (&optional arg)
  "`+default/yank-buffer-path' except that it includes the line numbers if a
region is active."
  (interactive "P")
  (chloe--yank-path (lambda () (+default/yank-buffer-path arg))))

;;;###autoload
(defun chloe/yank-buffer-path-relative-to-project (&optional arg)
  "`+default/yank-buffer-path-relative-to-project' except that it includes the
line numbers if a region is active."
  (interactive "P")
  (chloe--yank-path (lambda () (+default/yank-buffer-path-relative-to-project arg))))

;;;###autoload
(defun chloe/yank-buffer-filename-only ()
  "Copy the current buffer's filename to the kill ring. If a region is active,
the line numbers are included."
  (interactive)
  (chloe--yank-path #'chloe--yank-buffer-filename-only))

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
(defun chloe/search-symbol-at-point ()
  "Performs a search in the current buffer for thing at point or selected
region."
  (interactive)
  (consult-line (doom-thing-at-point-or-region)))

;;;###autoload
(defun chloe/project-search-symbol-at-point (&optional arg)
  (interactive "P")
  (+vertico/project-search arg (doom-thing-at-point-or-region)))

;;;###autoload
(defun chloe/cwd-search-symbol-at-point (&optional arg)
  (interactive "P")
  (+vertico/project-search-from-cwd arg (doom-thing-at-point-or-region)))

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

;;;###autoload
(defun chloe/string-inflection-yank ()
  (interactive)
  (if-let* ((thing (doom-thing-at-point-or-region))
            (options '(("snake case" . string-inflection-underscore-function)
                       ("kebab case" . string-inflection-kebab-case-function)
                       ("camel case" . string-inflection-camelcase-function)
                       ("pascal case" . string-inflection-pascal-case-function)
                       ("constant case (upper underscore)" . string-inflection-upcase-function)))
            (choice (completing-read "Case: "
                                     (mapcar #'car options)
                                     nil t))
            (action (cdr (assoc choice options)))
            (result (funcall action thing)))
      (progn
        (kill-new result)
        (message result))
    (error "nothing found")))

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
      (+go--generate (file-truename (project-root project)) "./...")
    (error "Couldn't get current project.")))
