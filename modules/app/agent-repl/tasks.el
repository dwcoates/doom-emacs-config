;;; tasks.el --- User-defined tasks: model, persistence, org notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Backs the sidebar's "Task" view (sidebar.el): a user-defined todo
;; list whose entries group workspaces.  A task is a small plist
;; (`:id :title :done :created-at') held in an id-keyed hash and
;; persisted to `~/.claude-emacs/tasks.el' — user data, so unlike the
;; in-memory repo-fold set it must survive restarts.
;;
;; Each task owns an org notes file under `~/.claude-emacs/tasks/'
;; (`agent-repl--task-org-file'); the sidebar opens it in a right-side
;; popup (sidebar.el) so the user can persist notes about the task.
;;
;; A workspace joins a task through the `:task-id' plist key
;; (`agent-repl--task-assign-workspace').  Membership is INHERITED down
;; the family: a workspace with no `:task-id' of its own belongs to the
;; task of the nearest `:source-ws-dir' ancestor that has one
;; (`agent-repl--ws-effective-task-id'), because a child worktree is
;; implicitly part of its parent's encompassing task.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--global-state-file "core" (relative))
(declare-function agent-repl--log "core" (ws format-string &rest args))
(declare-function agent-repl--log-verbose "core" (ws format-string &rest args))
(declare-function agent-repl--read-sexp-file-if-exists "history" (file))
(declare-function agent-repl--write-sexp-file "history" (file data))
(declare-function agent-repl--ws-get "workspace" (name key))
(declare-function agent-repl--ws-put "workspace" (name key value))
(declare-function agent-repl--save-buffer-if-modified "autosave" (buf &optional ws))
(declare-function agent-repl--ws-name-for-dir "worktree" (dir))

(defvar agent-repl-workspace-id-length)

;;;; ---- State -----------------------------------------------------------

(defvar agent-repl--tasks (make-hash-table :test 'equal)
  "Hash of task id (string) -> task plist.
Plist keys: `:id' `:title' `:done' `:created-at' (epoch-seconds float).
Hydrated lazily from disk on first read (`agent-repl--tasks-ensure-loaded').")

(defvar agent-repl--tasks-loaded nil
  "Non-nil once `agent-repl--tasks' has been hydrated from disk this session.")

;;;; ---- On-disk locations -----------------------------------------------

(defun agent-repl--tasks-state-file ()
  "Absolute path of the file persisting the task list."
  (agent-repl--global-state-file "tasks.el"))

(defun agent-repl--tasks-org-dir ()
  "Absolute path (trailing slash) of the per-task org notes directory."
  (file-name-as-directory (agent-repl--global-state-file "tasks")))

(defun agent-repl--task-org-file (id)
  "Absolute path of task ID's org notes file.
Named with a `task-notes-' prefix so a single popup rule (sidebar.el)
can match every task notes buffer by name without matching unrelated
`.org' files the user opens."
  (expand-file-name (format "task-notes-%s.org" id) (agent-repl--tasks-org-dir)))

;;;; ---- Persistence -----------------------------------------------------

(defun agent-repl--tasks-load ()
  "Hydrate `agent-repl--tasks' from disk, replacing any in-memory state.
A task plist missing an `:id' is skipped with a log line rather than
silently, since a well-formed save always records one."
  (let ((file (agent-repl--tasks-state-file))
        (loaded 0)
        (skipped 0))
    (agent-repl--log nil "tasks-load: begin file=%s" file)
    (clrhash agent-repl--tasks)
    (dolist (task (agent-repl--read-sexp-file-if-exists file))
      (let ((id (plist-get task :id)))
        (if id
            (progn
              (puthash id task agent-repl--tasks)
              (setq loaded (1+ loaded)))
          (setq skipped (1+ skipped))
          (agent-repl--log nil
                            "tasks-load: skipping malformed entry index=%d missing-id=t"
                            (+ loaded skipped)))))
    (setq agent-repl--tasks-loaded t)
    (agent-repl--log nil
                      "tasks-load: complete file=%s loaded=%d skipped=%d"
                      file loaded skipped)))

(defun agent-repl--tasks-ensure-loaded ()
  "Hydrate the task list from disk once per session."
  (unless agent-repl--tasks-loaded
    (agent-repl--log nil "tasks-ensure-loaded: hydration-required=t")
    (agent-repl--tasks-load)))

(defun agent-repl--tasks-sorted ()
  "Return every task plist ordered by creation time, then id.
Creation order is the list's chronology; the id tiebreak keeps undated
tasks deterministic."
  (agent-repl--tasks-ensure-loaded)
  (let* ((ties 0)
         (tasks (sort (hash-table-values agent-repl--tasks)
                      (lambda (a b)
                        (let ((ta (or (plist-get a :created-at) 0))
                              (tb (or (plist-get b :created-at) 0)))
                          (if (= ta tb)
                              (progn
                                (setq ties (1+ ties))
                                (string< (or (plist-get a :id) "")
                                         (or (plist-get b :id) "")))
                            (< ta tb)))))))
    (agent-repl--log-verbose nil "tasks-sorted: count=%d creation-time-ties=%d"
                              (length tasks) ties)
    tasks))

(defun agent-repl--tasks-save ()
  "Write the task list to disk, newest-last, as a list of plists."
  (let* ((file (agent-repl--tasks-state-file))
         (tasks (agent-repl--tasks-sorted)))
    (agent-repl--log nil "tasks-save: begin file=%s count=%d" file (length tasks))
    (agent-repl--write-sexp-file file tasks)
    (agent-repl--log nil "tasks-save: complete file=%s count=%d" file (length tasks))))

;;;; ---- Accessors -------------------------------------------------------

(defun agent-repl--task-get (id)
  "Return the task plist for ID, or nil when unknown."
  (agent-repl--tasks-ensure-loaded)
  (let ((task (gethash id agent-repl--tasks)))
    (agent-repl--log-verbose nil "task-get: id=%s found=%s" id (and task t))
    task))

(defun agent-repl--task-new-id ()
  "Return a fresh random task id, `agent-repl-workspace-id-length' hex chars."
  (let ((id (substring (md5 (format "%s-%s" (float-time)
                                     (random most-positive-fixnum)))
                       0 agent-repl-workspace-id-length)))
    (agent-repl--log nil "task-new-id: id=%s" id)
    id))

;;;; ---- Mutations -------------------------------------------------------

(defun agent-repl--task-create (title)
  "Create a task titled TITLE, persist it, ensure its org file, return its id.
Signals on an empty (or whitespace-only) TITLE — a task with no title
has nothing to render or address."
  (agent-repl--tasks-ensure-loaded)
  (let ((clean (string-trim (or title ""))))
    (when (string-empty-p clean)
      (agent-repl--log nil "task-create: rejected empty-title=t")
      (error "agent-repl--task-create: empty title"))
    (agent-repl--log nil "task-create: begin title-length=%d" (length clean))
    (let ((id (agent-repl--task-new-id)))
      (while (gethash id agent-repl--tasks)
        (agent-repl--log nil "task-create: id-collision id=%s" id)
        (setq id (agent-repl--task-new-id)))
      (puthash id (list :id id :title clean :done nil :created-at (float-time))
               agent-repl--tasks)
      (agent-repl--task-org-ensure id clean)
      (agent-repl--tasks-save)
      (agent-repl--log nil "task-create: complete id=%s title-length=%d" id (length clean))
      id)))

(defun agent-repl--task-toggle-done (id)
  "Flip task ID's done flag, persist, and return the new done state.
Signals on an unknown ID: a toggle for a task that does not exist means
the click and the roster disagree, a contract violation to surface."
  (let ((task (agent-repl--task-get id)))
    (unless task
      (agent-repl--log nil "task-toggle-done: rejected unknown-id=%s" id)
      (error "agent-repl--task-toggle-done: unknown task %s" id))
    (let ((updated (plist-put (copy-sequence task) :done
                              (not (plist-get task :done)))))
      (puthash id updated agent-repl--tasks)
      (agent-repl--tasks-save)
      (agent-repl--log nil "task-toggle-done: id=%s done=%s"
                        id (plist-get updated :done))
      (plist-get updated :done))))

(defun agent-repl--task-org-ensure (id title)
  "Ensure task ID's org notes file exists, seeded with a TITLE header.
Returns the file path.  Idempotent: an existing file is left untouched."
  (let ((file (agent-repl--task-org-file id))
        (created nil))
    (unless (file-exists-p file)
      (agent-repl--log nil "task-org-ensure: create id=%s file=%s" id file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (format "#+TITLE: %s\n#+CREATED: %s\n\n"
                        title (format-time-string "%Y-%m-%d %H:%M"))))
      (setq created t))
    (agent-repl--log nil "task-org-ensure: ready id=%s file=%s outcome=%s"
                      id file (if created 'created 'existing))
    file))

;;;; ---- Opening the org notes popup -------------------------------------

(defun agent-repl--task-open (id)
  "Open task ID's org notes file in a popup, saving it on exit.
Signals on an unknown ID: an open request for a task that does not exist
means the click and the roster disagree.  `pop-to-buffer' routes the
file through the task-notes popup rule (config.el) — a right-side split
that leaves the agent-repl panels the left two thirds of the frame.  The
buffer gets a buffer-local `kill-buffer-hook' that saves it when
modified, so dismissing the popup persists the user's notes regardless
of how it closes."
  (let ((task (agent-repl--task-get id)))
    (unless task
      (agent-repl--log nil "task-open: rejected unknown-id=%s" id)
      (error "agent-repl--task-open: unknown task %s" id))
    (agent-repl--log nil "task-open: begin id=%s" id)
    (let* ((file (agent-repl--task-org-ensure id (plist-get task :title)))
           (buf (find-file-noselect file)))
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (agent-repl--save-buffer-if-modified (current-buffer)))
                  nil t))
      (agent-repl--log nil "task-open: id=%s file=%s" id file)
      (pop-to-buffer buf)
      buf)))

;;;; ---- Workspace membership --------------------------------------------

(defun agent-repl--ws-effective-task-id (name)
  "Return the id of the task workspace NAME belongs to, or nil.
A direct `:task-id' pointing at an existing task wins.  Otherwise the
`:source-ws-dir' ancestry is walked (cycle-guarded) and the first
ancestor whose `:task-id' names an existing task is inherited — a child
worktree is implicitly part of its parent's encompassing task.  A
`:task-id' naming a task that no longer exists is ignored rather than
stranding the workspace under a dangling section."
  (agent-repl--tasks-ensure-loaded)
  (let ((seen (make-hash-table :test 'equal))
        (cur name)
        (result nil)
        (matched-ws nil)
        (steps 0)
        (dangling-ids 0))
    (while (and cur (not result) (not (gethash cur seen)))
      (puthash cur t seen)
      (setq steps (1+ steps))
      (let ((tid (agent-repl--ws-get cur :task-id)))
        (if (and tid (gethash tid agent-repl--tasks))
            (setq result tid
                  matched-ws cur)
          (when tid
            (setq dangling-ids (1+ dangling-ids)))
          (setq cur (agent-repl--ws-name-for-dir
                     (agent-repl--ws-get cur :source-ws-dir))))))
    (agent-repl--log-verbose
     name
     "ws-effective-task-id: ws=%s result=%s matched-ws=%s steps=%d dangling-ids=%d stopped=%s"
     name result matched-ws steps dangling-ids
     (cond (result 'matched) ((and cur (gethash cur seen)) 'cycle) (t 'no-task)))
    result))

(defun agent-repl--task-assign-workspace (name id)
  "Assign live workspace NAME to task ID via its `:task-id' key.
Signals on an unknown ID for the same reason `agent-repl--task-toggle-done'
does."
  (unless (agent-repl--task-get id)
    (agent-repl--log name "task-assign-workspace: rejected ws=%s unknown-task=%s" name id)
    (error "agent-repl--task-assign-workspace: unknown task %s" id))
  (agent-repl--ws-put name :task-id id)
  (agent-repl--log name "task-assign-workspace: ws=%s task=%s outcome=assigned" name id))

;;;; ---- Change signature ------------------------------------------------

(defun agent-repl--tasks-signature ()
  "Return a cheap value that moves whenever the task list would render anew.
Feeds `agent-repl--sidebar-signature' (sidebar.el) so a task create,
rename, or done-toggle wakes the 1Hz roster push."
  (let ((signature (mapcar (lambda (task)
                             (list (plist-get task :id)
                                   (plist-get task :title)
                                   (and (plist-get task :done) t)))
                           (agent-repl--tasks-sorted))))
    (agent-repl--log-verbose nil "tasks-signature: count=%d" (length signature))
    signature))

(provide 'agent-repl-tasks)
;;; tasks.el ends here
