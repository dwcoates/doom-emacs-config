;;; webview-recovery.el --- every workspace's page, made ahead of the look -*- lexical-binding: t; -*-

;;; Commentary:

;; PAGES ARE MADE AHEAD OF THE LOOK.  Every eligible workspace gets its
;; webview slot and its page in the host web view (webhost.el) the moment it
;; is buildable -- on the link-up, on every roster push, when its tab is
;; created -- so a switch to it shows a page that is already live and drawn.
;; A slot is a buffer and a page is one call to the host, so every owed page
;; is made in one immediate pass; nothing is staggered or queued.
;;
;; ONLY THE HOST'S CREATION WAITS, AND ONLY FOR ONE STATE.  The host is the
;; one native view Emacs makes, and making a native view while Emacs is on
;; screen but not focused can bring Emacs to the front, taking the desktop
;; from whatever the user is looking at.  So while there is no host and Emacs
;; is visible but unfocused, the pass is PARKED and runs on the focus edge.
;; A hidden Emacs (an `open -gj' launch) and a focused one both proceed.  The
;; tabs do not wait for a parked pass (startup.el).
;;
;; THE STALE-WEBVIEW SWEEP IS GONE, and so is every script it drove.  A
;; rebuilt webapp reaches every page through the daemon's own
;; `reload_webapp' push (frontend.el reloads the host), and a daemon handover
;; re-points a page as a side effect.

;;; Code:


(require 'cl-lib)

(declare-function agent-repl--panels-on-view-created "panels" (ws))
(declare-function agent-repl--with-deferred-quit "agent-repl-core")
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--frontend-precreate-webview "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-precreate-refusal "agent-repl-frontend" (ws))
(declare-function agent-repl--emacs-focused-p "agent-repl-notifications" (&optional ws))
(declare-function agent-repl-webhost-xwidget "webhost" ())

(defvar agent-repl-link-up-functions)
(defvar agent-repl-roster-update-functions)

(defconst agent-repl--webview-precreate-central
  '(:agent-repl-central "webview recovery spans workspaces")
  "The log scope of a pre-creation record.")

(defvar agent-repl--webview-precreate-parked nil
  "Non-nil while a pass waits for Emacs to have desktop focus before it
creates the host.  Nothing is dropped: the next pass makes every page owed.")

(defun agent-repl--emacs-visible-p ()
  "Return nil ONLY when we KNOW Emacs shows nothing on the desktop.

The one question this answers for the park guard is whether creating a
native view could raise a frame the user can see.  It cannot when the app
is HIDDEN: a hidden macOS app (an `open -gj' launch, or a `Cmd-H') has no
on-screen window, and instantiating a WKWebView inside it neither unhides
nor activates it -- hidden is a stronger state than merely unfocused.

`visible-frame-list' is the signal: it is empty exactly when no frame is
mapped on screen, which on ns is the hidden case.  We return nil ONLY on
that positive \"nothing is visible\" answer; anything else -- a mapped
frame, or a state we cannot read (no window system, `display-graphic-p'
nil) -- returns non-nil so the guard treats it as possibly-visible and
errs toward parking, matching the conservative \"suppress when possibly
foregrounding\" stance the focus gate already takes.  Under
`noninteractive' the whole hold short-circuits before this is reached, so
its batch answer here is moot."
  (cond
   ((not (display-graphic-p)) t)        ; cannot tell -> treat as visible
   ((visible-frame-list) t)
   (t nil)))                            ; positively hidden

(defun agent-repl--emacs-can-foreground-p ()
  "Return non-nil when creating a native view could bring Emacs to the front.

That is exactly the VISIBLE-BUT-UNFOCUSED state: a frame is on screen but
Emacs is not the focused app, so instantiating a WKWebView could raise the
frame and steal the desktop from whatever the user is looking at.  When
Emacs is HIDDEN (no visible frame) creating a view cannot foreground it,
and when Emacs is already FOCUSED there is no focus to steal -- both are
safe, so both return nil.  An unreadable visibility state counts as
visible (see `agent-repl--emacs-visible-p'), so it parks rather than
gambles; an `unknown' focus state counts as focused
\(`agent-repl--emacs-focused-p'), so it proceeds rather than parking a
frame that may already be frontmost."
  (and (agent-repl--emacs-visible-p)
       (fboundp 'agent-repl--emacs-focused-p)
       (not (agent-repl--emacs-focused-p))))

(defun agent-repl--webview-precreate-hold-p ()
  "Return non-nil while a pre-creation must not mount a webview yet.

MOUNTING A WEBVIEW CAN TAKE THE DESKTOP, but only from ONE state.
Creating a WKWebView instantiates a native view, and macOS activates the
owning process when doing so would bring a window forward -- a thing
`open -g' cannot suppress, because it is not the app launch doing it.  A
link-up pre-creates every eligible workspace's page seconds into every
cold start, so an Emacs that is on screen but not focused stole the
desktop from whatever the user was looking at.

That raise can happen only when a frame is ALREADY ON SCREEN and Emacs is
NOT the focused app: the visible-but-unfocused state.  A HIDDEN Emacs (an
`open -gj' launch) creating a view stays hidden -- hidden is a stronger
state than merely unfocused, and macOS does not unhide an app to mount a
native view inside it -- and a FOCUSED Emacs has no focus to steal.  So
both of those proceed and paint; only visible-but-unfocused holds.
Parking a hidden launch is exactly the collision realtest 1 caught: focus
was preserved but no panel painted.

Only the HOST's creation holds (`agent-repl--webview-precreate-pass'):
once the host exists, making a page creates no native view.  The hold is
released by `agent-repl--webview-precreate-on-focus-change' the moment
Emacs is frontmost, which is at or before the user's first look.

Under `noninteractive' nothing holds -- a batch process has no
application to activate, and the suites drive this drain directly.  The
guard itself is `agent-repl--emacs-can-foreground-p'."
  (and (not noninteractive)
       (agent-repl--emacs-can-foreground-p)))

(defun agent-repl--webview-precreate-allow-reason ()
  "Name WHY the drain is allowed to mount right now, for the log line.

Mirrors `agent-repl--webview-precreate-hold-p': the hold is only the
visible-unfocused state, so a mount that actually happens is one of
created-while-hidden, created-while-focused, or (in batch) driven
directly by the suites.  Reported as a `reason=' tag on the created log."
  (cond
   (noninteractive "batch")
   ((not (agent-repl--emacs-visible-p)) "hidden")
   ((and (fboundp 'agent-repl--emacs-focused-p)
         (agent-repl--emacs-focused-p))
    "focused")
   (t "allowed")))

(defun agent-repl--webview-precreate-park ()
  "Hold the pass until creating the host can no longer foreground Emacs."
  (unless agent-repl--webview-precreate-parked
    (setq agent-repl--webview-precreate-parked t)
    (agent-repl--info agent-repl--webview-precreate-central
                      "elisp.webview-recovery.precreate-parked owed=%d reason=visible-unfocused"
                      (length (agent-repl--webview-precreate-missing))))
  t)

(defun agent-repl--webview-precreate-needed-p (ws)
  "Return non-nil when WS is owed a webview slot and page and has none.
Answered by `agent-repl--frontend-precreate-refusal' -- the SAME
eligibility the mount itself applies -- so a pass can never make a page the
mount would refuse, nor skip one it would accept."
  (null (agent-repl--frontend-precreate-refusal ws)))

(defun agent-repl--webview-precreate-missing ()
  "Return every live workspace owed a webview slot and page it does not have."
  (cl-remove-if-not #'agent-repl--webview-precreate-needed-p
                    (agent-repl--live-ws-names)))

(defun agent-repl--webview-precreate-pass ()
  "Make every owed workspace's slot and page now, or park for focus.
The body of `agent-repl-webview-precreate-all'.  A workspace whose mount
signals is warned about and the pass goes on: one workspace's failure must
not cost the rest their pages."
  (let ((owed (agent-repl--webview-precreate-missing)))
    (cond
     ((null owed) 0)
     ((and (null (agent-repl-webhost-xwidget))
           (agent-repl--webview-precreate-hold-p))
      (agent-repl--webview-precreate-park)
      0)
     (t
      (setq agent-repl--webview-precreate-parked nil)
      (let ((made 0))
        (dolist (ws owed)
          (condition-case err
              (progn
                (agent-repl--frontend-precreate-webview ws)
                (setq made (1+ made))
                (agent-repl--info agent-repl--webview-precreate-central
                                  "elisp.webview-recovery.precreate-created ws=%s reason=%s"
                                  ws (agent-repl--webview-precreate-allow-reason))
                ;; The workspace the user stands on may have declined its
                ;; panel restore for want of exactly this view.
                (agent-repl--panels-on-view-created ws))
            (error (agent-repl--warn ws "webview-precreate: ws=%s outcome=failed err=%S"
                                     ws err))))
        made)))))

(defun agent-repl-webview-precreate-all ()
  "Make every live workspace's owed slot and page, returning how many were made.

A PASS IS A CRITICAL SECTION FOR QUITS.  A pass runs on the desktop FOCUS
EDGE, which is the same instant a keypress arrives; a `C-g' landing here
found Emacs BUSY rather than waiting in `read_char', so it could only arm
`quit-flag', which a checkpoint inside the pass then took inside a standing
minibuffer's own recursive edit.  So the pass runs under
`agent-repl--with-deferred-quit', which holds the quit off and LEAVES IT
ARMED for Emacs's own input wait to turn into the `C-g' the read aborts on."
  (let ((made (agent-repl--with-deferred-quit "webview-precreate-pass"
                (agent-repl--webview-precreate-pass))))
    (when (> made 0)
      (agent-repl--info agent-repl--webview-precreate-central
                        "elisp.webview-recovery.precreate-all: made=%d" made))
    made))

(defun agent-repl--webview-precreate-on-focus-change ()
  "Run a parked pass once Emacs actually holds desktop focus.
Registered on `after-focus-change-function'.  A no-op when nothing is parked."
  (when (and agent-repl--webview-precreate-parked
             (not (agent-repl--webview-precreate-hold-p)))
    (agent-repl--info agent-repl--webview-precreate-central
                      "elisp.webview-recovery.precreate-resumed-on-focus")
    (agent-repl-webview-precreate-all)))

(defun agent-repl--webview-precreate-on-link-up (&optional _conn)
  "Make the pages the link-up made buildable.
Registered on `agent-repl-link-up-functions': before the link is up there
is no daemon address to build a URL from, and this edge is the first
moment every open workspace is buildable."
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-link-up-functions #'agent-repl--webview-precreate-on-link-up)

(defun agent-repl--webview-precreate-on-roster-update (&optional _roster)
  "Make the pages a roster push made buildable.
Registered on `agent-repl-roster-update-functions'.  A COLD start has no
workspaces registered at link-up -- their `WorkspaceRef's arrive on the
first roster push, and until then every workspace refuses with `:no-ref'
-- so this edge is the first moment a cold workspace is buildable.  A
workspace that already has its page is not owed one, so running on both
hooks makes nothing twice."
  (agent-repl-webview-precreate-all))

(add-hook 'agent-repl-roster-update-functions #'agent-repl--webview-precreate-on-roster-update)
(add-function :after after-focus-change-function
              #'agent-repl--webview-precreate-on-focus-change)

(provide 'webview-recovery)
;;; webview-recovery.el ends here
