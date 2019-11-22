;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package! org
  :config
  (+org-pretty-mode)

  (defun my-org-hook ()
    (abbrev-mode t))
  (add-hook! 'org-mode-hook 'my-org-hook)

  ;;; TODO: customize captures?
  (setq org-default-notes-file (expand-file-name +org-capture-notes-file org-directory)
        org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* [ ] %?\n%i\n%a" :prepend t)
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
                                 :clock-in t)
                                ("ptf" "Future features" entry
                                 (file+headline +org-capture-project-todo-file "Features"))
                                ("ptr" "Future refactors" entry
                                 (file+headline +org-capture-project-todo-file "Refactor"))
                                ("ptb" "Bugs to fix" entry
                                 (file+headline +org-capture-project-todo-file "Bugs")
                                 "* [ ] %?\t%t\n %i\n %a")
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

  (define-abbrev-table 'org-mode-abbrev-table
    (mapcar
     (lambda (char-string)
       (let ((character-property-elements
              (split-string (get-char-code-property (encode-char (string-to-char char-string) 'unicode) 'name) " ")))
         (list
          (concat
           (if (member "CAPITAL" character-property-elements)
               (capitalize (-last-item character-property-elements))
             (downcase (-last-item character-property-elements)))
           "x")
          char-string)))
     '("α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"
       "Α" "Β" "Γ" "Δ" "Ε" "Ζ" "Η" "Θ" "Ι" "Κ" "Λ" "Μ" "Ν" "Ξ" "Ο" "Π" "Ρ" "Σ" "Τ" "Υ" "Φ" "Χ" "Ψ" "Ω"))))
