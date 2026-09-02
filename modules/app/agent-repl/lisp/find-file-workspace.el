;;; find-file-workspace.el --- Route a visited file into its workspace -*- lexical-binding: t; -*-

;;; Commentary:

;; Visiting a file should land it in the agent-repl workspace that OWNS it:
;; the workspace whose project dir is the file's git root.  `SPC .', `SPC f f',
;; `SPC p f' and every other entry that ends in `find-file-noselect' followed by
;; a same-window display all funnel through the same two display primitives, so
;; this module hooks THE DISPLAY STEP rather than replacing `find-file':
;;
;;   - `pop-to-buffer-same-window' — the primitive `find-file' itself uses.
;;   - `switch-to-buffer'          — the other same-window display funnel.
;;
;; The advice is :around because routing REPLACES the display: once the file has
;; been shown in the owning workspace's window, letting the primitive display it
;; again in the previous window would undo the routing.  When routing does not
;; apply — routing disabled, a non-file buffer, no git root, a root outside the
;; user's home, or a workspace the daemon has not opened yet — the advice falls
;; through to the original function unchanged, so a file is NEVER lost.
;;
;; Root detection uses EXISTING git facilities, in preference order, and never a
;; hand-rolled ancestor walk: `magit-toplevel' when Magit is loaded, then
;; `vc-git-root', then `git rev-parse --show-toplevel' run in the file's own
;; directory.  All three treat a linked worktree (a `.git' FILE) exactly as they
;; treat a `.git' directory, which is why none of them is replaced here.  The
;; whole detection is INJECTED through
;; `agent-repl-find-file-workspace-root-function' so tests never run git.
;;
;; The four cases the routing answers:
;;
;;   1. The workspace exists and its tab is open — switch to it and place.
;;   2. The workspace exists but is closed  — open it, then place.
;;   3. No workspace owns the root          — create it, open it, then place.
;;   4. The file is already current IN its own workspace — do nothing at all.
;;
;; Placement puts the file in the LARGEST non-agent-repl window of the target
;; tab.  When the tab has nothing but agent-repl panels, the frame's main window
;; is split so the panels keep the RIGHT half and the file takes the LEFT half.

;;; Code:

(require 'cl-lib)

;; Cross-module functions this one calls.  Each is defined by the module named
;; in its declaration, all of which `config.el' loads before this file.
(declare-function agent-repl--git-string-quiet "core" (&rest args))
(declare-function agent-repl--path-canonical "core" (path))
(declare-function agent-repl--agent-panel-buffer-p "core" (&optional buf))
(declare-function agent-repl--log "core" (ws format &rest args))
(declare-function agent-repl--warn "core" (ws format &rest args))
(declare-function agent-repl-window--side-window-p "window" (win &optional ws))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--ws-known-p "workspace" (ws))
(declare-function agent-repl--ws-open-p "workspace" (ws))
(declare-function agent-repl--ws-switch "workspace" (ws &rest args))
(declare-function agent-repl--ws-name-for-dir "worktree" (dir))
(declare-function agent-repl-verb-open "verbs" (ref))
(declare-function agent-repl-verbs--ref "verbs" (ws))
(declare-function agent-repl-add-project-workspace "commands" (&optional dir))

(defcustom agent-repl-find-file-workspace-enabled t
  "When non-nil, route a visited file into the workspace owning its git root.
Set to nil to disable the routing entirely; every advised display
primitive then behaves exactly as it does without this module."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-find-file-workspace-display-fns
  '(switch-to-buffer
    pop-to-buffer-same-window)
  "Display primitives advised with `agent-repl--ffw-display-advice'.
These are the DISPLAY step of every ordinary file visit: `find-file'
reaches `pop-to-buffer-same-window' with the buffer `find-file-noselect'
returned, and the buffer pickers reach `switch-to-buffer'.  Advising them
covers every entry that ends in a same-window display without replacing
`find-file' itself."
  :type '(repeat symbol)
  :group 'agent-repl)

(defvar agent-repl--ffw-routing nil
  "Non-nil while routing is placing a buffer.
The placement itself displays a buffer and so re-enters the advised
display primitives; this guard makes the routing non-recursive.")

;;;; ---- Root detection ----------------------------------------------------

(defun agent-repl--ffw-git-root (dir)
  "Return the git root of DIR using EXISTING git facilities, or nil.
Prefers `magit-toplevel' when Magit is loaded, then `vc-git-root', then
`git rev-parse --show-toplevel' run in DIR.  Every one of the three
resolves a linked worktree (whose `.git' is a FILE) as readily as an
ordinary checkout, which is why no ancestor walk is written here."
  (when (and dir (file-directory-p dir))
    (let ((root
           (cond
            ((fboundp 'magit-toplevel) (funcall 'magit-toplevel dir))
            (t nil))))
      (unless root
        (setq root (and (fboundp 'vc-git-root) (funcall 'vc-git-root dir))))
      (unless root
        (let ((out (agent-repl--git-string-quiet
                    "-C" dir "rev-parse" "--show-toplevel")))
          (when (and (stringp out)
                     (not (string-empty-p out))
                     (not (string-prefix-p "fatal" out)))
            (setq root out))))
      (when (and root (stringp root) (not (string-empty-p root)))
        root))))

(defvar agent-repl-find-file-workspace-root-function
  #'agent-repl--ffw-git-root
  "Function returning the git root of a directory, or nil.
THE INJECTION POINT for root detection: tests bind this to a fixture so
no test ever runs real git.  Production leaves it at
`agent-repl--ffw-git-root'.")

(defun agent-repl--ffw-under-home-p (root)
  "Return non-nil when canonical ROOT is at or below the user's home directory."
  (and root
       (let ((home (agent-repl--path-canonical "~"))
             (canon (agent-repl--path-canonical root)))
         (or (string= home canon)
             (string-prefix-p (file-name-as-directory home)
                              (file-name-as-directory canon))))))

(defun agent-repl--ffw-root-for-file (file)
  "Return the canonical git root owning FILE, or nil when it must not route.
Nil means \"open normally\": FILE has no git root, or its root lies
outside the user's home directory."
  (when-let* ((file file)
              (dir (file-name-directory (expand-file-name file)))
              (raw (funcall agent-repl-find-file-workspace-root-function dir))
              (root (agent-repl--path-canonical raw)))
    (if (agent-repl--ffw-under-home-p root)
        root
      (agent-repl--log nil "find-file-workspace: root outside home file=%s root=%s"
                       file root)
      nil)))

;;;; ---- Window placement --------------------------------------------------

(defun agent-repl--ffw-candidate-windows ()
  "Return the windows of the selected frame a file may be displayed in.
Excludes agent-repl panel windows (the file must not evict the agent) and
side windows (a popup is not where a file belongs)."
  (cl-remove-if
   (lambda (win)
     (or (agent-repl-window--side-window-p win)
         (agent-repl--agent-panel-buffer-p (window-buffer win))))
   (window-list nil 'no-minibuffer)))

(defun agent-repl--ffw-largest-window (windows)
  "Return the largest window in WINDOWS by total area, or nil when empty.
Ties keep the first window in WINDOWS, so the answer is deterministic."
  (car (sort (copy-sequence windows)
             (lambda (a b)
               (> (* (window-total-width a) (window-total-height a))
                  (* (window-total-width b) (window-total-height b)))))))

(defun agent-repl--ffw-split-left ()
  "Split the frame's main window so a new window takes its LEFT half.
The agent-repl panels stay in the surviving right half.  Splits the MAIN
window rather than the frame root so frame-level side windows are left
alone; `window-main-window' is the accessor for that, with the root
window as the fallback where it does not exist."
  (let ((base (if (fboundp 'window-main-window)
                  (funcall 'window-main-window)
                (frame-root-window))))
    (split-window base nil 'left)))

(defun agent-repl--ffw-place (buffer)
  "Display BUFFER in the current tab's largest non-agent-repl window.
Splits the main window left-of-the-panels when the tab has no such
window.  Returns the window BUFFER was placed in, or nil when no window
could be obtained."
  (let* ((candidates (agent-repl--ffw-candidate-windows))
         (win (if candidates
                  (agent-repl--ffw-largest-window candidates)
                (agent-repl--ffw-split-left))))
    (when (window-live-p win)
      (set-window-buffer win buffer)
      (select-window win)
      win)))

;;;; ---- Workspace acquisition ---------------------------------------------

(defun agent-repl--ffw-open-workspace (ws)
  "Open the closed workspace WS and its agent-repl panel.
The daemon owns the reopen, so this asks for it and nothing more; the
caller places the file only if the tab is actually open afterwards."
  (agent-repl-verb-open (agent-repl-verbs--ref ws))
  (agent-repl--ffw-open-panel))

(defun agent-repl--ffw-create-workspace (root)
  "Create a workspace for git root ROOT through the existing register verb.
Routes through `agent-repl-add-project-workspace' — Emacs's own
create/register path — rather than minting a second onboarding verb."
  (agent-repl-add-project-workspace (file-name-as-directory root))
  (agent-repl--ffw-open-panel))

(defun agent-repl--ffw-open-panel ()
  "Open the agent-repl panel in the current workspace when that is possible."
  (when (fboundp 'agent-repl-frontend-open-panel)
    (funcall 'agent-repl-frontend-open-panel)))

(defvar agent-repl-find-file-workspace-open-function
  #'agent-repl--ffw-open-workspace
  "Function opening the existing but closed workspace named by its argument.
Injected so tests can observe the request without reaching the daemon.")

(defvar agent-repl-find-file-workspace-create-function
  #'agent-repl--ffw-create-workspace
  "Function creating a workspace for the git root passed as its argument.
Injected so tests can observe the request without reaching the daemon.")

(defun agent-repl--ffw-already-here-p (ws buffer)
  "Return non-nil when BUFFER is already the current buffer inside WS.
This is the never-fight-the-user case: there is nothing to route."
  (and (equal ws (agent-repl--ws-current-name))
       (eq buffer (current-buffer))))

(defun agent-repl--ffw-switch-and-place (ws buffer)
  "Switch to WS and place BUFFER in it, when WS's tab is open.
Returns the window BUFFER was placed in, or nil when WS is not open —
which is the signal the caller falls back to ordinary display."
  (if (and (agent-repl--ws-known-p ws) (agent-repl--ws-open-p ws))
      (progn
        (agent-repl--ws-switch ws)
        (agent-repl--ffw-place buffer))
    (agent-repl--log ws "find-file-workspace: ws not open, no placement ws=%s" ws)
    nil))

(defvar agent-repl--ffw-pending (make-hash-table :test 'equal)
  "Pending one-shot placements, keyed by canonical git root.
Each value is the buffer to place the moment that root's tab appears.
PER ROOT AND LATEST WINS: a second visit into a root still waiting for
its tab replaces the first, because the user asked for the newer file.
An entry is cleared when it fires and when its verb is refused, so
nothing here outlives the arrival it is waiting for.")

(defun agent-repl--ffw-pending-register (root buffer)
  "Record BUFFER as the one-shot placement waiting on ROOT's tab."
  (agent-repl--log nil "find-file-workspace: pending placement root=%s buffer=%s"
                   root (buffer-name buffer))
  (puthash root buffer agent-repl--ffw-pending))

(defun agent-repl--ffw-pending-drop (root reason)
  "Forget ROOT's pending placement, recording REASON."
  (when (gethash root agent-repl--ffw-pending)
    (agent-repl--log nil "find-file-workspace: pending dropped root=%s reason=%s"
                     root reason)
    (remhash root agent-repl--ffw-pending)))

(defun agent-repl--ffw-pending-fire (&rest _)
  "Place every pending buffer whose workspace tab has now appeared.
Installed on `agent-repl-roster-update-functions', the hook the roster
runs after it has reconciled a push into tabs — so the ARRIVAL of the tab
is what fires a placement.  Nothing here polls, waits or schedules: a tab
that never arrives simply leaves its entry pending.

An entry whose buffer died is dropped rather than placed, and an entry
fires exactly ONCE because it is removed before the placement runs."
  (maphash
   (lambda (root buffer)
     (let ((ws (agent-repl--ws-name-for-dir root)))
       (cond
        ((not (buffer-live-p buffer))
         (agent-repl--ffw-pending-drop root "buffer-died"))
        ((and ws (agent-repl--ws-known-p ws) (agent-repl--ws-open-p ws))
         (remhash root agent-repl--ffw-pending)
         (agent-repl--log ws "find-file-workspace: pending fired root=%s ws=%s"
                          root ws)
         (agent-repl--ws-switch ws)
         (agent-repl--ffw-place buffer)))))
   (copy-hash-table agent-repl--ffw-pending)))

(add-hook 'agent-repl-roster-update-functions #'agent-repl--ffw-pending-fire)

(defun agent-repl--ffw-acquire (root buffer thunk)
  "Register BUFFER as ROOT's pending placement, then run THUNK to acquire it.
THUNK is the injected open or create verb.  Returns non-nil when the
display has been HANDLED — either placed immediately, because the tab was
already there when the verb returned, or held as the pending placement
that ROOT's tab arrival will fire.  A REFUSAL — the verb signalling —
drops the pending entry, is logged and messaged, and answers nil so the
caller falls back to ordinary display."
  (agent-repl--ffw-pending-register root buffer)
  (condition-case err
      (progn
        (funcall thunk)
        (or (when-let* ((ws (agent-repl--ws-name-for-dir root)))
              (when (agent-repl--ffw-switch-and-place ws buffer)
                (agent-repl--ffw-pending-drop root "placed-immediately")
                t))
            'pending))
    (error
     (agent-repl--ffw-pending-drop root "verb-refused")
     (agent-repl--warn nil "find-file-workspace: verb refused root=%s error=%S"
                       root err)
     (message "agent-repl: could not open the workspace for %s; opening the file here"
              root)
     nil)))

(defun agent-repl--ffw-route (buffer)
  "Route BUFFER into the workspace owning its file's git root.
Returns non-nil when the routing HANDLED the display — the file was
placed, or it is held as the pending placement its workspace's tab
arrival will fire — and nil when the caller must display BUFFER itself.
A held file is deliberately not shown anywhere in the meantime: the user
asked for it in its workspace, and a transient placement elsewhere is
the thing this routing exists to stop."
  (when-let* ((file (buffer-file-name buffer))
              (root (agent-repl--ffw-root-for-file file)))
    (let ((ws (agent-repl--ws-name-for-dir root)))
      (cond
       ((and ws (agent-repl--ffw-already-here-p ws buffer))
        (agent-repl--log ws "find-file-workspace: already current ws=%s file=%s"
                         ws file)
        nil)
       ((and ws (agent-repl--ws-known-p ws) (agent-repl--ws-open-p ws))
        (agent-repl--ffw-switch-and-place ws buffer))
       (ws
        (agent-repl--ffw-acquire
         root buffer
         (lambda () (funcall agent-repl-find-file-workspace-open-function ws))))
       (t
        (agent-repl--ffw-acquire
         root buffer
         (lambda () (funcall agent-repl-find-file-workspace-create-function root))))))))

;;;; ---- The display-step advice -------------------------------------------

(defun agent-repl--ffw-target-buffer (arg)
  "Return the file-visiting buffer ARG names, or nil.
ARG is the first argument of an advised display primitive: a buffer
object, or the name of an existing buffer.  Anything that does not
resolve to a live buffer visiting a file is not routable."
  (let ((buf (cond ((bufferp arg) arg)
                   ((stringp arg) (get-buffer arg)))))
    (and buf (buffer-live-p buf) (buffer-file-name buf) buf)))

(defun agent-repl--ffw-display-advice (orig &rest args)
  "Route a file display into its owning workspace, else call ORIG on ARGS.
Installed as :around advice on `agent-repl-find-file-workspace-display-fns'.
Returns the routed buffer when the routing displayed it — matching the
advised primitives, which answer with the buffer they displayed."
  (let ((buf (and agent-repl-find-file-workspace-enabled
                  (not agent-repl--ffw-routing)
                  (agent-repl--ffw-target-buffer (car args)))))
    (if (and buf
             (let ((agent-repl--ffw-routing t))
               (agent-repl--ffw-route buf)))
        buf
      (apply orig args))))

(dolist (fn agent-repl-find-file-workspace-display-fns)
  (advice-add fn :around #'agent-repl--ffw-display-advice))

(provide 'find-file-workspace)

;;; find-file-workspace.el ends here
