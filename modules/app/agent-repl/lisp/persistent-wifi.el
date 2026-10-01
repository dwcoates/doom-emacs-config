;;; persistent-wifi.el --- the machine's persistent wifi mode -*- lexical-binding: t; -*-

;;; Commentary:

;; PERSISTENT WIFI MODE is the laptop's lid-closed operation: system sleep
;; disabled, so closing the lid neither sleeps the machine nor drops its
;; network or the phone hotspot it is on.  The DAEMON owns both the reading
;; and the change (internal/persistentwifi); this file is Emacs's half:
;;
;;   - the standing, as the daemon pushes it on WatchDaemon's
;;     `persistent_wifi' arm (`agent-repl-persistent-wifi-handle'), echoed
;;     whenever it changes;
;;   - `agent-repl-persistent-wifi-mode-toggle' (`SPC j w'), which sends
;;     UpdatePersistentWifiMode{toggle} over the daemon-level connection and
;;     reports what the change left.
;;
;; The toggle is resolved by the DAEMON from the mode it reads at that
;; moment, never from the standing this file last heard, so two presses in
;; quick succession turn the mode over twice.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl-verbs--send "verbs")
(declare-function agent-repl-verbs--conn "verbs")
(declare-function agent-repl-rpc-update-persistent-wifi-mode "rpc")
(declare-function agent-repl--info "core")
(declare-function agent-repl--log "core")

(defcustom agent-repl-persistent-wifi-timeout-seconds 60
  "Seconds `agent-repl-persistent-wifi-mode-toggle' waits for the answer.
Turning the mode on joins the phone hotspot first, and a Wi-Fi join can
take far longer than an ordinary verb's deadline."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl-persistent-wifi-state nil
  "The machine's persistent-wifi standing as the daemon last stated it.
A decoded `PersistentWifiState' plist `(:wifi ONEOF :mode ONEOF)', each
ONEOF nil when the daemon could not read that fact; nil before the
daemon has stated any.")

(defconst agent-repl-persistent-wifi--log-scope
  '(:agent-repl-central "the persistent-wifi standing is the machine's, not a workspace's")
  "The log scope every record of this file carries.")

;;;; ---- Wording ---------------------------------------------------------

(defun agent-repl-persistent-wifi--wifi-text (wifi)
  "Return the words for the decoded WIFI oneof, nil being unread."
  (pcase (plist-get wifi :arm)
    ('nil "Wi-Fi could not be read")
    (:joined
     (let ((name (plist-get (plist-get wifi :value) :network-name)))
       (if name
           (format "Wi-Fi joined to %s" name)
         "Wi-Fi joined (network name withheld by macOS)")))
    (:not-joined "no Wi-Fi")
    (arm (error "agent-repl-persistent-wifi: wifi arm %S has no wording" arm))))

(defun agent-repl-persistent-wifi--mode-text (mode)
  "Return the words for the decoded MODE oneof, nil being unread."
  (pcase (plist-get mode :arm)
    ('nil "persistent wifi could not be read")
    (:on "persistent wifi on: the lid can close")
    (:off "persistent wifi off: closing the lid sleeps")
    (arm (error "agent-repl-persistent-wifi: mode arm %S has no wording" arm))))

(defun agent-repl-persistent-wifi-describe (state)
  "Return one line describing the decoded persistent-wifi STATE."
  (format "%s · %s"
          (agent-repl-persistent-wifi--mode-text (plist-get state :mode))
          (agent-repl-persistent-wifi--wifi-text (plist-get state :wifi))))

(defun agent-repl-persistent-wifi--hotspot-text (hotspot)
  "Return the words for the decoded HOTSPOT step outcome."
  (let ((v (plist-get hotspot :value)))
    (pcase (plist-get hotspot :arm)
      (:joined (format "hotspot: joined %s" (plist-get v :network-name)))
      (:already-joined (format "hotspot: already on %s" (plist-get v :network-name)))
      (:left (format "hotspot: left %s" (plist-get v :network-name)))
      (:not-on-hotspot "hotspot: not on it")
      (:network-unreadable "hotspot: left nothing (network name withheld by macOS)")
      (:no-wifi-interface "hotspot: no Wi-Fi interface")
      (:failed (format "hotspot: %s did not take (%s)"
                       (plist-get v :network-name) (plist-get v :detail)))
      (arm (error "agent-repl-persistent-wifi: hotspot arm %S has no wording" arm)))))

(defun agent-repl-persistent-wifi--display-text (display)
  "Return the words for the decoded DISPLAY step outcome."
  (let ((v (plist-get display :value)))
    (pcase (plist-get display :arm)
      (:dimmed "display: dimmed")
      (:restored "display: restored")
      (:tool-missing (format "display: %s is not installed" (plist-get v :tool-path)))
      (:failed (format "display: failed (%s)" (plist-get v :detail)))
      (arm (error "agent-repl-persistent-wifi: display arm %S has no wording" arm)))))

;;;; ---- The standing ----------------------------------------------------

(defun agent-repl-persistent-wifi-handle (state)
  "Take the decoded persistent-wifi STATE the daemon pushed.
A CHANGE is echoed in the minibuffer; the first standing a connection is
told, and a resubscribe's replay of the same one, are only recorded."
  (let ((previous agent-repl-persistent-wifi-state))
    (setq agent-repl-persistent-wifi-state state)
    (cond
     ((equal previous state)
      (agent-repl--log agent-repl-persistent-wifi--log-scope
                       "elisp.persistent-wifi.unchanged standing=%S" state))
     ((null previous)
      (agent-repl--info agent-repl-persistent-wifi--log-scope
                        "elisp.persistent-wifi.standing standing=%S" state))
     (t
      (agent-repl--info agent-repl-persistent-wifi--log-scope
                        "elisp.persistent-wifi.changed was=%S standing=%S" previous state)
      (message "agent-repl: %s" (agent-repl-persistent-wifi-describe state))))))

;;;; ---- The toggle ------------------------------------------------------

(defun agent-repl-persistent-wifi--on-success (value)
  "Report the decoded UpdatePersistentWifiModeSuccess VALUE.
The standing it carries is ADOPTED without a second echo: the daemon's
push of the same standing then reads as unchanged."
  (let ((state (plist-get value :state)))
    (setq agent-repl-persistent-wifi-state state)
    (agent-repl--info agent-repl-persistent-wifi--log-scope
                      "elisp.persistent-wifi.toggled standing=%S hotspot=%S display=%S"
                      state (plist-get value :hotspot) (plist-get value :display))
    (message "agent-repl: %s · %s · %s"
             (agent-repl-persistent-wifi-describe state)
             (agent-repl-persistent-wifi--hotspot-text (plist-get value :hotspot))
             (agent-repl-persistent-wifi--display-text (plist-get value :display)))))

(defun agent-repl-persistent-wifi-mode-toggle ()
  "Turn the machine's persistent wifi mode (lid-closed operation) over.
On: join the phone hotspot, disable system sleep, dim the display.  Off:
leave the hotspot, enable sleep, restore the display.  The daemon reads
the mode and turns it; a refusal (no sudo grant for pmset) is reported as
the daemon's refusal."
  (interactive)
  (agent-repl-verbs--send
   #'agent-repl-rpc-update-persistent-wifi-mode (agent-repl-verbs--conn)
   (list :action (list :arm :toggle))
   :op "persistent-wifi-mode"
   :timeout agent-repl-persistent-wifi-timeout-seconds
   :on-success #'agent-repl-persistent-wifi--on-success))

(provide 'agent-repl-persistent-wifi)

;;; persistent-wifi.el ends here
