;;; page-unload.el --- a page is loaded exactly while its workspace is visible -*- lexical-binding: t; -*-
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
(declare-function agent-repl-webhost-ensure-page "webhost" (ws))
(declare-function agent-repl-webhost-has-page-p "webhost" (ws))
(declare-function agent-repl-webhost-hold-back "webhost" (ws))
(declare-function agent-repl-webhost-held-back-p "webhost" (ws))
(declare-function agent-repl-host-report-page-unloading "host" (ws on-reported on-not-reported))
(declare-function agent-repl-roster-status-for-ws "roster" (ws))
(defvar agent-repl-roster-update-functions)

(defvar agent-repl-page-unload--shown nil
  "The workspace whose page the host shows, or nil.")

(defun agent-repl-page-unload--gated-p (ws)
  "Non-nil when WS stands at its cold gate, so its page is held back."
  (eq (agent-repl-roster-status-for-ws ws) :cold-gate))

(defun agent-repl-page-unload-wanted-p (ws)
  "Non-nil when WS's page should be loaded now, before anyone shows it.
Every workspace this editor holds is visible (frontend.v1.RosterRowVisibility),
so its page is wanted, unless WS stands at its cold gate: that page is held
back until the user asks for it.  A slot made for a workspace that is not
wanted is made with no page (`agent-repl--frontend-ensure-webview-buffer');
its show holds it back (`agent-repl-page-unload-note-shown')."
  (not (agent-repl-page-unload--gated-p ws)))

(defun agent-repl-page-unload-note-shown (ws)
  "Record that the host now shows WS's page.
An unloaded page is put back first (`agent-repl-page-unload--return'),
unless WS is cold-gated: its page is then held back, and the host draws a
load button in its place (`agent-repl-webhost-hold-back')."
  (when (agent-repl-page-unloaded-p ws)
    (if (agent-repl-page-unload--gated-p ws)
        (agent-repl-page-unload--hold-back ws)
      (agent-repl-page-unload--return ws)))
  (setq agent-repl-page-unload--shown ws))

(defun agent-repl-page-unloaded-p (ws)
  "Non-nil when WS holds a live slot whose page the host does not hold.
That is what a dropped page leaves (`agent-repl-page-drop-announced'), and
what a slot made with no page is, read off the slot and the host rather
than kept as a flag beside them."
  (and (buffer-live-p (agent-repl--ws-get ws :frontend-buffer))
       (not (agent-repl-webhost-has-page-p ws))))

(defun agent-repl-page-unload--return (ws)
  "Put WS's unloaded page back in the host, its slot being shown.
The slot and its windows stood throughout; only the page comes back."
  (agent-repl--info ws "elisp.page-unload.returned ws={ws}" :ws ws)
  (agent-repl-webhost-ensure-page ws))

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
        (agent-repl-page-unload--return ws))
    (agent-repl--info ws "elisp.page-unload.load-requested ws={ws} outcome=already-loaded" :ws ws)))

(defun agent-repl-page-unload--load-ungated (ws)
  "Load WS's page when it is held back on screen but no gate holds it now.
The user answered the gate, or it was retracted, while the load button
stood; nothing holds the page back any more.  Non-nil if loaded."
  (when (and (agent-repl-page-unloaded-p ws)
             (equal ws agent-repl-page-unload--shown)
             (agent-repl-webhost-held-back-p ws)
             (not (agent-repl-page-unload--gated-p ws)))
    (agent-repl--info ws "elisp.page-unload.loaded ws={ws} reason=ungated" :ws ws)
    (agent-repl-page-unload--return ws)
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
  "Load every page held back on screen whose gate a roster push answered.
Answers how many were loaded."
  (let ((loaded 0))
    (dolist (ws (agent-repl--live-ws-names))
      (when (agent-repl-page-unload--load-ungated ws)
        (setq loaded (1+ loaded))))
    loaded))

(add-hook 'agent-repl-roster-update-functions #'agent-repl-page-unload-on-roster-update)

(provide 'page-unload)
;;; page-unload.el ends here
