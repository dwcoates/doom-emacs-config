;;; lang/org/config.el -*- lexical-binding: t; -*-

(message "hello")

(after! org
  (setq org-agenda-files '("~/org/"
                           "~/workspace/ChessCom/org/"))

  (setq org-todo-keywords `((sequence "TODO(t!)" "IN PROGRESS(p!)" "BLOCKED(b@/!)" "HOLD(h)" "IDEA(i)" "QUESTION(q)" "|" "DONE(d!/@)" "KILLED(k@)")))

  (setq org-todo-keyword-faces `(("IN PROGRESS" . +org-todo-active)
                                 ("BLOCKED" . +org-todo-cancel)
                                 ("HOLD" . +org-todo-onhold)
                                 ("QUESTION" . +org-todo-cancel)
                                 ("KILLED" . +org-todo-cancel)))

  ;; (setq org-agenda-files '("~/workspace/ChessCom/org/todo.org" "~/workspace/ChessCom/org/notes.org" "~/workspace/ChessCom/org/devs.org" "~/org/notes.org" "~/org/todo.org"))

  (defun +dwc/org-agenda-show-todos ()
    "Show all TODO items in org-agenda."
    (interactive)
    (org-call-with-arg 'org-todo-list "TODO"))

  (map! :leader
        :desc "Show all TODOs in Agenda" "o a t" #'+dwc/org-agenda-show-todos
        :desc "Show all open items in Agenda" "o a T" #'org-agenda-todos
        )
  )
