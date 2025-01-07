;;; lang/org/config.el -*- lexical-binding: t; -*-

(message "hello")

(after! org
  (setq org-agenda-files '("~/org/"
                           "~/workspace/ChessCom/org/"))

  (setq org-todo-keywords `((sequence "TODO(t!)" "IN PROGRESS(p!)" "BLOCKED(b@/!)" "HOLD(h)" "IDEA(i)" "QUESTION(q)" "NOTE(n)" "|" "DONE(d!/@)" "KILLED(k@)")))

  (setq org-todo-keyword-faces `(("IN PROGRESS" . +org-todo-active)
                                 ("BLOCKED" . +org-todo-cancel)
                                 ("HOLD" . +org-todo-onhold)
                                 ("QUESTION" . +org-todo-cancel)
                                 ("NOTE" . +org-todo-cancel)
                                 ("KILLED" . +org-todo-cancel)))

  ;; (setq org-agenda-files '("~/workspace/ChessCom/org/todo.org" "~/workspace/ChessCom/org/notes.org" "~/workspace/ChessCom/org/devs.org" "~/org/notes.org" "~/org/todo.org"))

  (defun +dwc/org-agenda-show-todos ()
    "Show all TODO items in org-agenda."
    (interactive)
    (org-call-with-arg 'org-todo-list "TODO"))

  (defun +dwc/open-org-file-as-popup (file-path)
    "Create a new buffer that visits the file at FILE-PATH."
    (let ((new-buffer (find-file-noselect file-path)))
      (set-popup-rule! (buffer-name new-buffer) :width 120 :quit t :side 'right :select t)
      (+popup--init (get-buffer-window (pop-to-buffer new-buffer t nil)))
      (+popup-buffer-mode 1)
      (solaire-mode)
      ))

  (map! :leader
        :desc "Show all TODOs in Agenda" "o a t" #'+dwc/org-agenda-show-todos
        :desc "Show all open items in Agenda" "o a T" #'org-todo-list
        :desc "Open ChessCom ToDo org file"
        "p t" #'(lambda () (interactive) (+dwc/open-org-file-as-popup "~/workspace/ChessCom/org/todo.org"))
        :desc "Open ChessCom Notes org file"
        "p n" #'(lambda () (interactive) (+dwc/open-org-file-as-popup "~/workspace/ChessCom/org/notes.org"))
        )
  )
