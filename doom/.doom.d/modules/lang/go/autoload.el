;;; lang/go/autoload.el -*- lexical-binding: t; -*-

;;
;; Tests

(defvar +go-test-last nil
  "The last test run.")

(defun +go--spawn (cmd)
  (save-selected-window
    (compile cmd)))

(defun +go--run-tests (args)
  (let ((cmd (concat "go test " args)))
    (setq +go-test-last (concat "cd " default-directory ";" cmd))
    (+go--spawn cmd)))

(defun +go--function-name ()
  (save-excursion
    (go-goto-function 1) ;; skip anonymous functions
    (substring-no-properties (go--function-name))))

(defun +go--method-receiver ()
  "Returns method receiver or nil if not in a method."
  (save-excursion
    (go-goto-function 1) ;; skip anonymous functions
    (let ((line (substring-no-properties (thing-at-point 'line)))
          (regex (rx "func" (one-or-more space)
                     "("
                     (zero-or-more space)
                     ;; <varname>
                     (? (one-or-more (or alnum "_")) (one-or-more space))
                     ;; receiver type
                     (group (? "*") (one-or-more (or alnum "_")))
                     (zero-or-more space)
                     ")")))
      (string-match regex line)
      (match-string 1 line))))

(defun +go--type-name (type-string)
  (if (string-prefix-p "*" type-string)
      (substring type-string 1) ;; remove * from pointer type
    type-string))

;;;###autoload
(defun +go/test-rerun ()
  (interactive)
  (if +go-test-last
      (+go--spawn +go-test-last)
    (+go/test-all)))

;;;###autoload
(defun +go/test-all ()
  (interactive)
  (+go--run-tests ""))

;;;###autoload
(defun +go/test-nested ()
  (interactive)
  (+go--run-tests "./..."))

;;;###autoload
(defun +go/test-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-run" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/bench-all ()
  (interactive)
  (+go--run-tests "-test.run=NONE -test.bench=\".*\""))

;;;###autoload
(defun +go/bench-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Benchmark[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-test.run=NONE -test.bench" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

;; TODO testify suite

;; (defun +go/testify-suite-all ())
;; (defun +go/testify-suite-method ())

;;
;;; Run

;;;###autoload
(defun +go/play-buffer-or-region (&optional beg end)
  "TODO"
  (interactive "r")
  (if (use-region-p)
      (go-play-region beg end)
    (go-play-buffer)))

;;
;;; go generate

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


;; doesn't exactly behave like go generate (go generate CLI doesn't always escape quotes properly)
;;;###autoload
(defun +go/generate-line-simple ()
  "Run go generate for just the line at the current point."
  (interactive)
  (let* ((curr-line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (cmd (string-trim-left curr-line-string "//go:generate ")))
    (if (not (eq curr-line-string cmd))
        (+go--spawn cmd)
      (error "Line does not begin with //go:generate"))))
