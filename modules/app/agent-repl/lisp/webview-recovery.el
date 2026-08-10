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
;; STARTUP IS THE SAME EDGE.  Webviews restored from a saved session can
;; predate the running daemon entirely, and the first snapshot apply after
;; Emacs starts runs this hook like any other — so the startup sweep needs
;; no separate arming, and gets none.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--frontend-webview-execute-script "agent-repl-frontend" (buf script))

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

(defun agent-repl--webview-recovery-sweep (reason)
  "Drive the recovery hook in every live workspace webview, naming REASON.
Returns the number of webviews the script was delivered to.

Debounced by `agent-repl-webview-recovery-debounce-seconds': a sweep
inside that window of the previous one is skipped and returns nil, which
is how a flapping link cannot stack sweeps.

A webview whose script evaluation fails is WARNED about by name and the
sweep continues to the rest: one page's broken widget is not a reason to
leave every other page disconnected, and the failure is still said out
loud rather than swallowed."
  (let ((now (float-time)))
    (if (and agent-repl--webview-recovery-last-sweep
             (< (- now agent-repl--webview-recovery-last-sweep)
                agent-repl-webview-recovery-debounce-seconds))
        (progn
          (agent-repl--log-verbose
           nil "webview-recovery: sweep skipped=debounced reason=%s since=%.3fs"
           reason (- now agent-repl--webview-recovery-last-sweep))
          nil)
      (setq agent-repl--webview-recovery-last-sweep now)
      (let ((script (agent-repl--webview-recovery-script reason))
            (workspaces (agent-repl--live-ws-names))
            (driven 0)
            (skipped 0)
            (failed 0))
        (dolist (ws workspaces)
          (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
            (if (not (buffer-live-p buf))
                (setq skipped (1+ skipped))
              (condition-case err
                  (progn
                    (agent-repl--frontend-webview-execute-script buf script)
                    (setq driven (1+ driven)))
                (error
                 (setq failed (1+ failed))
                 (agent-repl--warn ws "webview-recovery: buffer=%s outcome=script-failed err=%S"
                                   (buffer-name buf) err))))))
        (agent-repl--log nil
                         "webview-recovery: sweep reason=%s workspaces=%d driven=%d no-webview=%d failed=%d"
                         reason (length workspaces) driven skipped failed)
        driven))))

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
