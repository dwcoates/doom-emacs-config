;;; page-unload.el --- a visible workspace holds its page while its session runs or it is shown -*- lexical-binding: t; -*-
;;; Commentary:
;; THE WEB PROCESS IS AS SLOW AS IT IS BIG.  Every workspace's page lives in
;; the one host web view (webhost.el), so in one web content process with one
;; JS heap, and every full collection marks every live page.  So a page is
;; loaded only for a workspace the user can see.
;;
;; THE DAEMON DECIDES WHICH (owner ruling, 2026-11-21, which supersedes the
;; status, hold, grace and working-cap rules of 2026-10-29, 2026-10-30 and
;; 2026-11-18).  Its persisted visibility (frontend.v1.RosterRowVisibility,
;; `wsm.Workspace.Visibility') is the one fact every per-workspace allocation
;; is gated on: this editor holds a perspective, a composer, a host stream
;; and a page for exactly the workspaces it calls visible or selected
;; \(`agent-repl-roster-row-visible-p'), and tears the rest down whole, page
;; included.  So nothing here weighs a status or a clock: a workspace held
;; here has its page loaded.
;;
;; A DORMANT WORKSPACE HOLDS NO PAGE UNLESS IT IS SHOWN (owner rulings,
;; 2026-11-23, which narrow the 2026-11-21 one: visibility makes a page
;; ELIGIBLE, it does not hold it).  A session is dormant while it runs no
;; work: parked, or up only for a keep-alive (frontend.v1.RosterRowDormant,
;; the daemon's, so nothing here weighs a clock).  A page is unloaded when
;; its workspace goes dormant while the host does not show it, or when the
;; user switches away from a dormant workspace.
;;
;; A PAGE IS LOADED AT EXACTLY THREE MOMENTS, all through
;; `agent-repl-page-unload-load', the one caller of
;; `agent-repl-webhost-ensure-page' (test-page-unload.el holds it so): the
;; user shows the workspace (or clicks a gate's load button, or answers the
;; gate it shows); its session comes up for work while it is visible; or its
;; slot is made while it is wanted.  A keep-alive's restore is not work, so
;; the minute a beat holds a session up loads nothing, and no shim is ever
;; started to load a page: a prompt alone starts one.
;;
;; A COLD-GATED PAGE IS HELD BACK UNTIL THE USER ASKS FOR IT (owner request,
;; 2026-10-30).  That hold concerns the cold read, not visibility.  The gate
;; is the webhost's own (webapp/src/host/gate.ts), so it stands without the
;; page, and a user switching rapidly past cold-gated workspaces would
;; otherwise load a whole page for each.  So a cold-gated workspace's slot is
;; made with no page, its show loads nothing: the host draws a load button
;; where the page would be (`agent-repl-webhost-hold-back'), and its click
;; loads the page (`agent-repl-page-unload-load-requested').  A gate answered
;; while its page is held back on screen loads the page with no click.
;;
;; A PAGE DROPPED WHILE ITS WORKSPACE STAYS OPEN IS ANNOUNCED FIRST (owner
;; ruling, 2026-10-20).  A hide and a closed panel drop a page their
;; workspace keeps; its web stream ends with the page, and the daemon would
;; draw that end as a broken link, blue.  So the daemon is told first
;; \(`agent-repl-page-drop-announced'), and the page goes only once it has
;; answered.  A workspace that stops being visible needs no announcement:
;; the daemon decided it, and reads its streams' close as no severed hop.
;;
;; A DROPPED PAGE KEEPS ITS SLOT.  The slot buffer is free, a display of the
;; host's one xwidget, and its windows are the workspace's panels.  So a page
;; is UNLOADED exactly when its workspace holds a live slot the host holds no
;; page for (`agent-repl-page-unloaded-p'), a fact read off the two, never a
;; flag beside them; and showing that slot puts its page back
;; \(`agent-repl-page-unload-note-shown').
;;
;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key value))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl-webhost-ensure-page "webhost" (ws why))
(declare-function agent-repl-webhost-has-page-p "webhost" (ws))
(declare-function agent-repl-webhost-hold-back "webhost" (ws))
(declare-function agent-repl-webhost-held-back-p "webhost" (ws))
(declare-function agent-repl-host-report-page-unloading "host" (ws on-reported on-not-reported))
(declare-function agent-repl-roster-status-for-ws "roster" (ws))
(declare-function agent-repl-roster-dormant-p "roster" (ws))
(declare-function agent-repl-webhost-remove "webhost" (ws &optional unloading))
(declare-function agent-repl-page-report-loaded "page-report" (ws why))
(declare-function agent-repl-page-report-check-load-reason "page-report" (ws why))
(declare-function agent-repl-page-report-unloaded "page-report" (ws why))
(defvar agent-repl-roster-update-functions)

(defvar agent-repl-page-unload--shown nil
  "The workspace whose page the host shows, or nil.")

(defun agent-repl-page-unload--gated-p (ws)
  "Non-nil when WS stands at its cold gate, so its page is held back."
  (eq (agent-repl-roster-status-for-ws ws) :cold-gate))

(defun agent-repl-page-unload-wanted-p (ws)
  "Non-nil when WS's page should be loaded now, before anyone shows it.
Every workspace this editor holds is visible (frontend.v1.RosterRowVisibility),
which makes its page eligible.  It is wanted unless WS stands at its cold
gate, whose page is held back until the user asks for it, or WS's session
is dormant (frontend.v1.RosterRowDormant), whose page waits for its show.  A
slot made for a workspace that is not wanted is made with no page
\(`agent-repl--frontend-ensure-webview-buffer'); its show loads the page, or
holds it back at a gate (`agent-repl-page-unload-note-shown')."
  (not (or (agent-repl-page-unload--gated-p ws)
           (agent-repl-roster-dormant-p ws))))

(defun agent-repl-page-unload-load (ws why)
  "Load WS\='s page in the host, for WHY, a symbol the record names.
THE ONE PATH THAT LOADS A PAGE (see the commentary): every load is a
show, a gate the user answered or asked past, a session come up for
work, or a slot made while its page is wanted."
  (agent-repl-page-report-check-load-reason ws why)
  (agent-repl--info ws "elisp.page-unload.loaded ws={ws} reason={why}" :ws ws :why why)
  (prog1 (agent-repl-webhost-ensure-page ws why)
    (agent-repl-page-report-loaded ws why)))

(defun agent-repl-page-unload-note-shown (ws)
  "Record that the host now shows WS's page.
An unloaded page is loaded first (`agent-repl-page-unload-load'),
unless WS is cold-gated: its page is then held back, and the host draws a
load button in its place (`agent-repl-webhost-hold-back').  The workspace
shown before WS has its page unloaded when its session is dormant
\(`agent-repl-page-unload--release')."
  (when (agent-repl-page-unloaded-p ws)
    (if (agent-repl-page-unload--gated-p ws)
        (agent-repl-page-unload--hold-back ws)
      (agent-repl-page-unload-load ws 'shown)))
  (let ((was agent-repl-page-unload--shown))
    (setq agent-repl-page-unload--shown ws)
    (when (and was (not (equal was ws))
               (agent-repl-page-unload--releasable-p was))
      (agent-repl-page-unload--release was))))

(defun agent-repl-page-unload--releasable-p (ws)
  "Non-nil when WS\='s page is loaded and goes, its session dormant off screen.
WS is not the workspace the host shows, and no drop of its page is in
flight."
  (and (agent-repl-webhost-has-page-p ws)
       (not (equal ws agent-repl-page-unload--shown))
       (not (agent-repl--ws-get ws :page-unloading))
       (agent-repl-roster-dormant-p ws)))

(defun agent-repl-page-unload--release (ws)
  "Unload WS\='s page, its session dormant while the host does not show it.
WS stays open, so the daemon is told first (`agent-repl-page-drop-announced'),
and the page goes only if it is still releasable then; its slot stays, so
showing WS loads the page again (`agent-repl-page-unload-note-shown')."
  (agent-repl--info ws "elisp.page-unload.releasing ws={ws}" :ws ws)
  (agent-repl-page-drop-announced
   ws
   (lambda () (agent-repl-page-unload--releasable-p ws))
   (lambda ()
     (agent-repl--info ws "elisp.page-unload.released ws={ws}" :ws ws)
     (when (agent-repl-webhost-remove ws t)
       (agent-repl-page-report-unloaded ws 'dormant)))
   (lambda (why)
     (agent-repl--info ws "elisp.page-unload.release-kept ws={ws} why={why}" :ws ws :why why))))

(defun agent-repl-page-unloaded-p (ws)
  "Non-nil when WS holds a live slot whose page the host does not hold.
That is what a dropped page leaves (`agent-repl-page-drop-announced'), and
what a slot made with no page is, read off the slot and the host rather
than kept as a flag beside them."
  (and (buffer-live-p (agent-repl--ws-get ws :frontend-buffer))
       (not (agent-repl-webhost-has-page-p ws))))

(defun agent-repl-page-unload--hold-back (ws)
  "Hold back WS's unloaded page, WS being shown at its cold gate."
  (agent-repl--info ws "elisp.page-unload.held-back ws={ws}" :ws ws)
  (agent-repl-webhost-hold-back ws))

(defun agent-repl-page-unload-load-requested (ws)
  "Load WS's held-back page: the user clicked the host's load button.
A page already loaded is left as it is: a second click, or one that
crossed a load the gate's answer made, has nothing left to load."
  (if (agent-repl-page-unloaded-p ws)
      (progn
        (agent-repl--info ws "elisp.page-unload.load-requested ws={ws} outcome=loaded" :ws ws)
        (agent-repl-page-unload-load ws 'load-requested))
    (agent-repl--info ws "elisp.page-unload.load-requested ws={ws} outcome=already-loaded" :ws ws)))

(defun agent-repl-page-unload--load-ungated (ws)
  "Load WS's page when it is held back on screen but no gate holds it now.
The user answered the gate, or it was retracted, while the load button
stood; nothing holds the page back any more.  Non-nil if loaded."
  (when (and (agent-repl-page-unloaded-p ws)
             (equal ws agent-repl-page-unload--shown)
             (agent-repl-webhost-held-back-p ws)
             (not (agent-repl-page-unload--gated-p ws)))
    (agent-repl-page-unload-load ws 'ungated)
    t))

(defun agent-repl-page-unload--load-awake (ws)
  "Load WS\='s unloaded page when its session came up for work.
Its slot stands, its page is wanted (no gate stands, and it is no longer
dormant), and no drop of it is in flight.  Non-nil if loaded."
  (when (and (agent-repl-page-unloaded-p ws)
             (not (agent-repl--ws-get ws :page-unloading))
             (agent-repl-page-unload-wanted-p ws))
    (agent-repl-page-unload-load ws 'awake)
    t))

(defun agent-repl-page-drop-announced (ws still-p drop on-kept)
  "Drop open workspace WS\='s page by calling DROP, once the daemon is told.
EVERY PATH THAT DROPS THE PAGE OF A WORKSPACE THAT STAYS OPEN COMES
THROUGH HERE: a hide, closing the panel.  The page\='s web stream ends with
the page, and a stream that ends unannounced is drawn as a broken link,
blue (2026-10-25: a fold of a repository section drew a busy workspace
severed).  So the daemon is told first
\(`agent-repl-host-report-page-unloading'), and DROP runs only once it has
answered success AND STILL-P, called with no argument, still answers
non-nil: what asked for the drop may have changed while the report was in
flight.  Otherwise ON-KEPT is called with `changed' or `not-reported', and
the page stays loaded.  The caller records each outcome.

WS\='s `:page-unloading' is set while the report is in flight.

A workspace that is ENDING (closed, killed, merged, no longer visible)
drops its page unannounced (`agent-repl--frontend-release-workspace-webview'):
the daemon ended it first, so its stream\='s end draws nothing."
  (agent-repl--ws-put ws :page-unloading t)
  (agent-repl-host-report-page-unloading
   ws
   (lambda ()
     (agent-repl--ws-put ws :page-unloading nil)
     (if (funcall still-p)
         (funcall drop)
       (funcall on-kept 'changed)))
   (lambda ()
     (agent-repl--ws-put ws :page-unloading nil)
     (funcall on-kept 'not-reported))))

(defun agent-repl-page-unload-on-roster-update (&optional _roster)
  "Load every page held back on screen whose gate a roster push answered,
and every page whose session the push brought up for work, and unload
every page whose session it left dormant off screen
\(`agent-repl-page-unload--release').  Answers how many were loaded."
  (let ((loaded 0))
    (dolist (ws (agent-repl--live-ws-names))
      (when (or (agent-repl-page-unload--load-ungated ws)
                (agent-repl-page-unload--load-awake ws))
        (setq loaded (1+ loaded)))
      (when (agent-repl-page-unload--releasable-p ws)
        (agent-repl-page-unload--release ws)))
    loaded))

(add-hook 'agent-repl-roster-update-functions #'agent-repl-page-unload-on-roster-update)

(provide 'page-unload)
;;; page-unload.el ends here
