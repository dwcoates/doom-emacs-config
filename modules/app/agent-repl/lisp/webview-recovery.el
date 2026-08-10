;;; webview-recovery.el --- host-driven webview reattach -*- lexical-binding: t; -*-

;;; Commentary:

;; THE DEFECT THIS ENDS: after a daemon bounce, a HIDDEN workspace webview
;; stayed on "lost the connection to the daemon" until the user switched to
;; it.  webapp/src/background-recovery.ts was written to end exactly that,
;; on a `window.setInterval' heartbeat, under the belief stated in its own
;; header that "timers keep running in a hidden page".  THAT BELIEF IS
;; WRONG FOR THIS EMBEDDER: Emacs/WebKit suspends a hidden xwidget
;; webview's timers — the mechanism ws.ts documents above `ensureConnected'
;; — so the heartbeat never ticks in the very page it exists to repair.
;; What is left in a hidden page is only `visibilitychange' and `focus',
;; both of which are a LOOK, which is the gate the module set out to remove.
;;
;; SO THE REPAIR IS DRIVEN FROM OUTSIDE THE PAGE, by the one clock the
;; suspension does not reach: Emacs's own.  Emacs already holds a channel
;; into a mounted webview that is not a page timer —
;; `agent-repl--frontend-webview-execute-script' evaluates JavaScript
;; against the live document via WebKit's `evaluateJavaScript', which is
;; delivered to the page whether or not it is displayed (it is what
;; `agent-repl--frontend-snap-webview-to-tail' and the sidebar push already
;; rely on for webviews the user is not looking at).  Timer SUSPENSION and
;; script DELIVERY are different things: the suspension stops the page
;; scheduling its own work, not the host handing it work to run.
;;
;; The sweep therefore calls the webapp's recovery hook in every live
;; workspace webview at the moment the frontend's UDS link comes back — the
;; snapshot-applied edge, the first instant the daemon's state of the world
;; has landed and a repair is answerable.  The page side is a verbatim call
;; into machinery that already exists (`catchUpOnVisible' in main.ts:
;; ConnectResync's `retryNow' then `BackgroundRecovery.recover'), so a
;; host-driven repair and a user-arrives repair run the identical code.
;;
;; A DEPLOY IS THE SAME PROBLEM ONE LAYER DOWN, and so it is the same
;; sweep.  bin/deploy-all.sh replaces the webapp bundle under every
;; mounted page; a page's own skew guard (webapp/src/version-skew.ts)
;; reloads it, but only on ADOPTION, which a hidden webview cannot reach
;; until it is focused — so the first look at a backgrounded workspace
;; after a deploy shows pre-deploy behavior for a beat.  Emacs settles
;; that without the page: the deployed bundle's identity is on disk
;; (`webapp/dist/.build-id') and the identity a page is running is in the
;; URL it was addressed at, so the sweep compares the two and either
;; drives the recovery hook (same bundle, possibly stale connection) or
;; re-navigates the page to the deployed address (different bundle, or a
;; bundle so old the URL carries no identity and the hook may not exist).
;; `agent-repl-refresh-webviews' — what deploy-all calls — is a thin call
;; into this one sweep, so the deploy edge and the link-up edge cannot
;; drift apart into two different ideas of what a stale page is.
;;
;; STARTUP IS THE SAME EDGE.  Webviews restored from a saved session can
;; predate the running daemon entirely, and the first snapshot apply after
;; Emacs starts runs this hook like any other — so the startup sweep needs
;; no separate arming, and gets none.

;;; Code:

(require 'cl-lib)
(require 'url-util)

(declare-function agent-repl--log"agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--frontend-webview-execute-script "agent-repl-frontend" (buf script))
(declare-function agent-repl--frontend-webview-live-widget "agent-repl-frontend" (buf))
(declare-function agent-repl--frontend-webview-uri "agent-repl-frontend" (xwidget))
(declare-function agent-repl--frontend-webview-navigate-widget "agent-repl-frontend" (xwidget uri))
(declare-function agent-repl--frontend-webview-workspace "agent-repl-frontend" (buf))
(declare-function agent-repl--frontend-live-webview-buffers "agent-repl-frontend" ())
(declare-function agent-repl--frontend-build-id "agent-repl-frontend-client" ())
(declare-function agent-repl--frontend-precreate-webview "agent-repl-frontend" (ws))
(declare-function agent-repl--frontend-precreate-refusal "agent-repl-frontend" (ws))
(declare-function agent-repl--ws-live-p "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-gui-frontend-p "agent-repl-frontends" (ws))
(declare-function agent-repl--open-fence-active-p "agent-repl-open-fence" (ws))

(defvar agent-repl-uds-snapshot-applied-functions)

(defconst agent-repl-frontend-recover-hook "agentReplRecoverNow"
  "Name of the webapp global that repairs the page's daemon connection.
The webapp plants it on `window' at boot (`RECOVER_HOOK' in
webapp/src/host.ts) — the two names are one contract and MUST match.  It
takes the reason string the page logs the repair under.")

(defcustom agent-repl-webview-recovery-debounce-seconds 2.0
  "Minimum seconds between two host-driven webview recovery sweeps.
A daemon bounce can flap the UDS link several times in quick succession,
and each reconnect lands its own snapshot-applied edge.  Without this
floor every flap would fire a full sweep across every live webview, so a
bounce that took five attempts to settle would issue five times the
scripts for one outage.  The page's own repair is idempotent, so the
sweeps a flap suppresses cost nothing: the last edge of the flap is the
one that matters, and it is the one that runs."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--webview-recovery-last-sweep nil
  "`float-time' of the last sweep that ran, or nil when none has.")

(defcustom agent-repl-webview-precreate-stagger-seconds 0.02
  "Seconds between two paced webview pre-creations.
Each pre-creation spawns a WebKit content process, and a startup sweep
can be owed ten of them at once.  The timer chain exists so eligibility
is re-checked between mounts and the command loop is handed back after
each one; the interval is deliberately near-zero because a full burst is
brief (each mount itself is milliseconds -- the heavy work happens in
WebKit's own processes) and a fast rollout is preferred over a stretched
one.  Raise this if a burst ever produces a visible hitch."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl--webview-precreate-queue nil
  "Workspaces still owed a paced pre-creation, in creation order.")

(defvar agent-repl--webview-precreate-timer nil
  "The timer draining `agent-repl--webview-precreate-queue', or nil.")

(defun agent-repl--webview-precreate-needed-p (ws)
  "Return non-nil when WS is owed a webview and has none.

Answered by `agent-repl--frontend-precreate-refusal' — the SAME
eligibility the mount itself applies — so the queue can never hold a
workspace the mount would then refuse, nor skip one it would accept.
It is re-asked here as well as at the mount because a workspace can be
killed, nuked, merged or fenced during the seconds a paced queue takes
to drain, and a page for it must not appear afterwards."
  (null (agent-repl--frontend-precreate-refusal ws)))

(defun agent-repl--webview-precreate-drain ()
  "Pre-create the queue's next workspace, then re-arm for the one after.
One mount per tick.  Eligibility is RE-CHECKED at the tick rather than
trusted from when the workspace was queued, so a workspace closed mid
drain gets no page.  A mount that signals is warned about and the drain
continues: one workspace's failure must not strand the rest of the queue."
  (setq agent-repl--webview-precreate-timer nil)
  (let ((ws (pop agent-repl--webview-precreate-queue)))
    (when ws
      (condition-case err
          (when (agent-repl--webview-precreate-needed-p ws)
            (agent-repl--frontend-precreate-webview ws))
        (error (agent-repl--warn ws "webview-precreate: ws=%s outcome=failed err=%S"
                                 ws err))))
    (when agent-repl--webview-precreate-queue
      (setq agent-repl--webview-precreate-timer
            (run-at-time agent-repl-webview-precreate-stagger-seconds nil
                         #'agent-repl--webview-precreate-drain)))))

(defun agent-repl--webview-precreate-schedule (workspaces)
  "Queue WORKSPACES for paced pre-creation, returning how many were queued.
Appends rather than replaces, so a sweep landing while an earlier queue
is draining cannot drop the workspaces that queue still owes.  A
workspace already queued is not queued twice."
  (let ((added 0))
    (dolist (ws workspaces)
      (unless (member ws agent-repl--webview-precreate-queue)
        (setq agent-repl--webview-precreate-queue
              (append agent-repl--webview-precreate-queue (list ws)))
        (setq added (1+ added))))
    (when (and agent-repl--webview-precreate-queue
               (null agent-repl--webview-precreate-timer))
      (setq agent-repl--webview-precreate-timer
            (run-at-time agent-repl-webview-precreate-stagger-seconds nil
                         #'agent-repl--webview-precreate-drain)))
    added))

(defun agent-repl--webview-precreate-missing ()
  "Return every live workspace owed a webview it does not have."
  (cl-remove-if-not #'agent-repl--webview-precreate-needed-p
                    (agent-repl--live-ws-names)))

(defun agent-repl--webview-recovery-script (reason)
  "Return the JS that drives the webapp's recovery hook, naming REASON.
Guarded on the hook's existence: a webview mid-boot or mid-navigation
has not planted it yet, which is an expected state rather than a
violated invariant — a page that has not finished booting has no stale
connection to repair, and its boot opens a fresh socket anyway."
  (format "window.%s && window.%s(%S);"
          agent-repl-frontend-recover-hook
          agent-repl-frontend-recover-hook
          reason))

(defun agent-repl--webview-recovery-buffers ()
  "Return every webview buffer a sweep can reach, deduplicated.

TWO SOURCES, because neither alone is the whole set:

  - `agent-repl--frontend-live-webview-buffers' walks `buffer-list', so
    it finds a webview whose workspace record has already been dropped;
  - each live workspace's recorded `:frontend-buffer' finds a webview
    whose buffer the buffer-name predicate would not claim.

A HIBERNATED workspace is a live registry entry, so its webview buffer
is in this set exactly when the buffer still exists — which is the point
of the union.  A workspace with NO webview buffer contributes nothing
HERE — there is no page to drive or reload.  It is not ignored, though:
the sweep hands it to `agent-repl--webview-precreate-schedule', which
builds the missing page so the NEXT sweep has something to repair."
  (let ((bufs (agent-repl--frontend-live-webview-buffers)))
    (dolist (ws (agent-repl--live-ws-names))
      (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
        (when (and (buffer-live-p buf) (not (memq buf bufs)))
          (setq bufs (append bufs (list buf))))))
    bufs))

(defconst agent-repl--webview-recovery-build-param "build"
  "Query parameter the webview URL carries the webapp's build identity in.
Written by `agent-repl--frontend-workspace-url' (frontend-client.el) from
`agent-repl--frontend-build-id', which reads `webapp/dist/.build-id' —
the same stamp bin/deploy-all.sh's revision gate is written beside.  The
URL is therefore the host-readable statement of WHICH BUNDLE a mounted
page is running, with no round trip into the page at all.")

(defun agent-repl--webview-recovery-uri-build (uri)
  "Return the build identity URI addresses, or nil when it carries none.
nil is a real answer, not a failure: a page addressed without the
parameter predates the build-stamped URL entirely, so it is running a
bundle no deploy can be sure of and the sweep treats it as stale."
  (when (and uri (string-match
                  (format "[?&]%s=\\([^&]*\\)" agent-repl--webview-recovery-build-param)
                  uri))
    (let ((raw (match-string 1 uri)))
      (unless (string-empty-p raw)
        (url-unhex-string raw)))))

(defun agent-repl--webview-recovery-fresh-uri (uri build)
  "Return URI re-addressed at BUILD, replacing or appending the build param.
The address is what selects the bundle a reload fetches, so a stale page
must be sent to a DIFFERENT one — re-fetching its own URI is what a
cache can, and does, answer with the superseded bundle it already holds."
  (let ((param agent-repl--webview-recovery-build-param)
        (value (url-hexify-string build)))
    (if (string-match (format "\\([?&]%s=\\)[^&]*" param) uri)
        (replace-match (concat (match-string 1 uri) value) t t uri)
      (concat uri (if (string-match-p "\\?" uri) "&" "?") param "=" value))))

(defun agent-repl--webview-recovery-sweep (reason &optional force)
  "Bring every reachable webview onto the deployed bundle, naming REASON.
Returns how many webviews the sweep ACTED on (driven plus reloaded).

WHY STALENESS IS DECIDED HOST-SIDE.  The page repairs itself on skew
only when it is ADOPTED (webapp/src/version-skew.ts), and a hidden
xwidget webview cannot reach adoption until it is focused — so after a
deploy the user's first look at a backgrounded workspace lands on the
PRE-DEPLOY bundle for a beat.  Emacs can settle it without the page's
help: `agent-repl--frontend-build-id' reads the deployed bundle's
identity off disk, and the webview's own URL carries the identity it was
addressed at, so the comparison needs nothing from a suspended page.

  - identities MATCH  -> the page is already on the deployed bundle and
                         only its daemon connection can be stale, so it
                         is DRIVEN through the existing recovery hook;
  - identities DIFFER, or the URL carries no identity at all (a bundle
    predating the stamped URL, which is also a bundle predating the
    recovery hook) -> the page is RELOADED at the deployed address,
                         because driving a hook a stale bundle may not
                         even define would repair nothing.

Debounced by `agent-repl-webview-recovery-debounce-seconds': a sweep
inside that window of the previous one is skipped and returns nil, which
is how a flapping link cannot stack sweeps.

FORCE non-nil BYPASSES that debounce.  The debounce assumes the sweep it
suppresses is redundant with one that already ran — true for a flapping
link, and FALSE for the recovery SLO's forced path (lisp/recovery-slo.el),
which sweeps precisely because a workspace has been MEASURED still broken
after the earlier sweep.  Suppressing that one would leave the budget
breach unrepaired and then re-verify the same failure.

A webview the sweep fails on is WARNED about by name and the sweep
continues to the rest: one page's broken widget is not a reason to leave
every other page stale, and the failure is still said out loud rather
than swallowed."
  (let ((now (float-time)))
    (if (and (not force)
             agent-repl--webview-recovery-last-sweep
             (< (- now agent-repl--webview-recovery-last-sweep)
                agent-repl-webview-recovery-debounce-seconds))
        (progn
          (agent-repl--log-verbose
           nil "webview-recovery: sweep skipped=debounced reason=%s since=%.3fs"
           reason (- now agent-repl--webview-recovery-last-sweep))
          nil)
      (setq agent-repl--webview-recovery-last-sweep now)
      (let ((script (agent-repl--webview-recovery-script reason))
            (deployed (agent-repl--frontend-build-id))
            (buffers (agent-repl--webview-recovery-buffers))
            (driven 0)
            (reloaded 0)
            (absent 0)
            (failed 0))
        (dolist (buf buffers)
          (let ((ws (agent-repl--frontend-webview-workspace buf)))
            (condition-case err
                (let ((xw (agent-repl--frontend-webview-live-widget buf)))
                  (if (null xw)
                      (progn
                        (setq absent (1+ absent))
                        (agent-repl--warn
                         ws "webview-recovery: buffer=%s outcome=dead-webview"
                         (buffer-name buf)))
                    (let* ((uri (agent-repl--frontend-webview-uri xw))
                           (build (agent-repl--webview-recovery-uri-build uri)))
                      (if (equal build deployed)
                          (progn
                            (agent-repl--frontend-webview-execute-script buf script)
                            (setq driven (1+ driven))
                            (agent-repl--log
                             ws "webview-recovery: buffer=%s outcome=driven build=%s"
                             (buffer-name buf) deployed))
                        (let ((fresh (agent-repl--webview-recovery-fresh-uri uri deployed)))
                          (agent-repl--frontend-webview-navigate-widget xw fresh)
                          (setq reloaded (1+ reloaded))
                          (agent-repl--log
                           ws "webview-recovery: buffer=%s outcome=reloaded was=%s now=%s url=%s"
                           (buffer-name buf) (or build "none") deployed fresh))))))
              (error
               (setq failed (1+ failed))
               (agent-repl--warn ws "webview-recovery: buffer=%s outcome=failed err=%S"
                                 (buffer-name buf) err)))))
        ;; SWEEP FIRST, CREATE SECOND.  The pages that already exist are the
        ;; ones a user may be looking at, so they are repaired before any
        ;; WebKit process is spawned for a workspace that has none.
        (let ((created (agent-repl--webview-precreate-schedule
                        (agent-repl--webview-precreate-missing))))
          (agent-repl--log nil
                           (concat "webview-recovery: sweep reason=%s webviews=%d driven=%d "
                                   "reloaded=%d absent=%d failed=%d created=%d")
                           reason (length buffers) driven reloaded absent failed created)
          (+ driven reloaded))))))

(defun agent-repl--webview-recovery-on-link-up ()
  "Sweep every live webview when the daemon link comes back.
Subscriber for `agent-repl-uds-snapshot-applied-functions' — the
link-up edge, and equally the first snapshot apply after Emacs starts,
which is what covers webviews restored from a session that predate the
running daemon."
  (agent-repl--webview-recovery-sweep "host_link_up"))

(add-hook 'agent-repl-uds-snapshot-applied-functions
          #'agent-repl--webview-recovery-on-link-up)

(provide 'webview-recovery)
;;; webview-recovery.el ends here
