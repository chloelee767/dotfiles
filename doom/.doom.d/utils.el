;;; ../dotfiles/doom/.doom.d/utils.el -*- lexical-binding: t; -*-

;; shortcuts to useful folders
;; SPC f f is already bound counsel-find-file, but I don't mind since find file is bound to SPC . as well
(map! :leader (:prefix "f" "f" nil))
(map! :leader (:prefix "f" (:prefix ("f" . "favourites")
                            :desc "Home" "h" (lambda () (interactive)(doom-project-browse "~/"))
                            :desc "Documents" "d" (lambda () (interactive)(doom-project-browse chloe/documents-directory))
                            :desc "Code" "c" (lambda () (interactive)(doom-project-browse "~/Code/"))
                            :desc "Dotfiles" "D" (lambda () (interactive)(doom-project-find-file "~/dotfiles/"))
                            :desc "Agenda folder" "a" (lambda () (interactive)(doom-project-browse chloe/org-agenda-directory))
                            :desc "Org folder" "o" (lambda () (interactive)(doom-project-browse org-files-directory))

                            :desc "NUS" "n" (lambda () (interactive)(doom-project-browse chloe/nus-directory))
                            :desc "Current semester" "s" (lambda () (interactive)(doom-project-browse chloe/nus-current-sem-directory))
                            :desc "CS1010S Teaching" "t" (lambda () (interactive)(doom-project-browse (concat chloe/nus-directory "CS1010S-Teaching/")))
                            :desc "cs1010sx" "x" (lambda () (interactive)(doom-project-browse (concat chloe/nus-directory "CS1010S-Teaching/cs1010sx/"))))))

(map! "<f2>" #'org-agenda
      "<f1>" #'org-roam-jump-to-index
      "<f3>" #'(lambda () (interactive)(doom-project-browse chloe/org-agenda-directory)))

(map! :leader (:prefix "o"
               :desc "Google calendar" "c" #'(lambda () (interactive)(browse-url "https://calendar.google.com"))))
