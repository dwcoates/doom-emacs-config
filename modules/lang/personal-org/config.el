;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package! org
  :config
  (+org-pretty-mode)

  (set-popup-rule! "CAPTURE-.*" :quit 'other)
  (set-popup-rule! "\*Org Note\*" :quit 'other)

  (defun my-org-hook ()
    (abbrev-mode t))
  (add-hook! 'org-mode-hook 'my-org-hook)

  (map!
   :map org-mode-map
   :nvieomrg
   "M-b" #'backward-word)

  (defun my/org-clock-query-out ()
    "Ask the user before clocking out.
This is a useful function for adding to `kill-emacs-query-functions'."
    (if (and
         (featurep 'org-clock)
         (funcall 'org-clocking-p)
         (y-or-n-p "You are currently clocking time, clock out? "))
        (org-clock-out)
      t)) ;; only fails on keyboard quit or error

  ;; timeclock.el puts this on the wrong hook!
  (add-hook 'kill-emacs-query-functions 'my/org-clock-query-out)

  (defun goto-branch-entry-create-maybe ()
    "Jump to the current Git branch section in the Branch header of the project org todo file.

Create it if necessary."
    (interactive)
    (if (not (magit-get-current-branch))
        (error "Not in a Git repository...")
      (set-buffer (org-capture-target-buffer (+org-capture-project-todo-file)))
      (org-goto-marker-or-bmk (org-find-exact-headline-in-buffer "Branch"))
      (org-narrow-to-subtree)
      (let ((loc (org-find-exact-headline-in-buffer (magit-get-current-branch))))
        ;; Goto branch heading, creating it if necessary
        (if loc
            (org-goto-marker-or-bmk loc)
          (org-insert-subheading nil)
          (insert (magit-get-current-branch))))
      ;; Create new spot for entry
      (widen)))

  ;;; TODO: customize captures?
  (setq org-agenda-files '("~/workspace/ChessCom/explanation-engine/tasks.org")
        org-default-notes-file (expand-file-name +org-capture-notes-file org-directory)
        org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* [ ] %?\n%i\n%a" :prepend t)
                                ("l" "Personal to learn" entry
                                 (file+headline +org-capture-notes-file "Learn")
                                 "* %u %?\n%i\n%a" :prepend t)
                                ("n" "Personal notes" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* %u %?\n%i\n%a" :prepend t)
                                ("j" "Journal" entry
                                 (file+olp+datetree +org-capture-journal-file "Inbox")
                                 "* %U %?\n%i\n%a" :prepend t)
                                ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
                                ;; {todo,notes,changelog}.org file is found in a parent directory.
                                ;; Uses the basename from `+org-capture-todo-file',
                                ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
;;;
                                ("p" "Templates for projects")
                                ("pt" "Project-local todo") ; {project-root}/todo.org
                                ("ptt" "Today's todos" entry
                                 (file+headline +org-capture-project-todo-file "Today")
                                 "* TODO %?\n %i\n %a"
                                 :clock-in t :prepend t)
                                ("ptT" "TODOs for the current git branch" entry
                                 (function goto-branch-entry-create-maybe)
                                 "* TODO %?\n %i\n %a")
                                ("ptf" "Future features" entry
                                 (file+headline +org-capture-project-todo-file "Feature"))
                                ("ptr" "Future refactors" entry
                                 (file+headline +org-capture-project-todo-file "Refactor"))
                                ("ptb" "Bugs to fix" entry
                                 (file+headline +org-capture-project-todo-file "Bugs to fix")
                                 "* [ ] %?\t%t\n %i\n %a" :prepend t)
                                ("ptd" "Design changes" entry
                                 (file+headline +org-capture-project-todo-file "Design")
                                 "* [ ] %?\t%t\n %i\n %a" :prepend t)
                                ("pn" "Project-local notes" entry ; {project-root}/notes.org
                                 (file+headline +org-capture-project-notes-file "Inbox"))
                                ("pc" "Project-local changelog" entry ; {project-root}/changelog.org
                                 (file+headline +org-capture-project-changelog-file "Unreleased")
                                 "* %U %?\n%i\n%a" :prepend t)
                                ;; Will use {org-directory}/{+org-capture-projects-file} and store
                                ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
                                ;; support `:parents' to specify what headings to put them under, e.g.
                                ;; :parents ("Projects")
;;;
                                ("o" "Centralized templates for projects")
                                ("ot" "Project todo" entry
                                 (function +org-capture-central-project-todo-file)
                                 "* TODO %?\n %i\n %a"
                                 :heading "Tasks"
                                 :prepend nil)
                                ("on" "Project notes" entry
                                 (function +org-capture-central-project-notes-file)
                                 "* %U %?\n %i\n %a"
                                 :heading "Notes"
                                 :prepend t)
                                ("oc" "Project changelog" entry
                                 (function +org-capture-central-project-changelog-file)
                                 "* %U %?\n %i\n %a"
                                 :heading "Changelog"
                                 :prepend t)))

  ;;;  Fix the issue of escaping code markup (tildes)
  (defun dwc-org-cycle (&optional arg)
    (interactive "p")
    "Wrapper around org-cycle that escapes code markup if point is just before a tilde."
    (cond ((looking-at-p "~$")
           (progn (when (search-forward-regexp "~$" (line-end-position) t 1)
                    (replace-match "~ "))))
          ((looking-at-p "~")
           (forward-char))
          (t
           (org-cycle arg))))
  (evil-define-key 'insert evil-org-mode-map (kbd "<tab>") 'dwc-org-cycle)

 ;; (define-abbrev-table 'org-mode-abbrev-table
;;    (mapcar
;;     (lambda (char-string)
;;       (let ((character-property-elements
;;              (split-string (get-char-code-property (encode-char (string-to-char char-string) 'unicode) 'name) " ")))
;;         (list
;;          (concat
;;           (if (member "CAPITAL" character-property-elements)
;;               (capitalize (-last-item character-property-elements))
;;             (downcase (-last-item character-property-elements)))
;;           "x")
;;          char-string)))
;;     '("α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"
;;       "Α" "Β" "Γ" "Δ" "Ε" "Ζ" "Η" "Θ" "Ι" "Κ" "Λ" "Μ" "Ν" "Ξ" "Ο" "Π" "Ρ" "Σ" "Τ" "Υ" "Φ" "Χ" "Ψ" "Ω")))
  )
