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
;; While the mode is on, a closed LID (the daemon hears it) puts the
;; machine in the LID-CLOSED QUIET STATE: the display sleeps, the
;; configured applications are frozen, and nothing draws for a screen
;; nobody sees.  This file's half of that is Emacs going idle
;; (`agent-repl-persistent-wifi--follow-quiet'): the repaint and stall
;; timers pause, and `agent-repl-persistent-wifi-quiet-functions' tell the
;; web views and the page-unload pass.  Opening the lid undoes it, and the
;; daemon resyncs every view in one step.
;;
;; The toggle is resolved by the DAEMON from the mode it reads at that
;; moment, never from the standing this file last heard, so two presses in
;; quick succession turn the mode over twice.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-verbs--send "verbs")
(declare-function agent-repl-verbs--conn "verbs")
(declare-function agent-repl-rpc-update-persistent-wifi-mode "rpc")
(declare-function agent-repl--info "core")
(declare-function agent-repl--log "core")
(declare-function agent-repl--cancel-timer-key "core" (key))
(defvar agent-repl--required-timer-keys)
(defvar agent-repl--quiet-paused-timer-keys)

(defcustom agent-repl-persistent-wifi-timeout-seconds 60
  "Seconds `agent-repl-persistent-wifi-mode-toggle' waits for the answer.
Turning the mode on joins the phone hotspot first, and a Wi-Fi join can
take far longer than an ordinary verb's deadline."
  :type 'number
  :group 'agent-repl)

(defvar agent-repl-persistent-wifi-state nil
  "The machine's persistent-wifi standing as the daemon last stated it.
A decoded `PersistentWifiState' plist
`(:wifi ONEOF :mode ONEOF :lid ONEOF :quiet QUIET)', each ONEOF nil when
the daemon could not read (or, for the lid, has not yet heard) that fact,
QUIET nil while the lid-closed quiet state does not stand; nil before the
daemon has stated any.")

(defconst agent-repl-persistent-wifi-quiet-timer-keys '(:state-poll :stall-sampler)
  "The keyed timers the lid-closed quiet state pauses: the tab-bar repaint
heartbeat and the command loop's stall sampler, both drawing or measuring
for a screen nobody sees.  Each is a key of `agent-repl--required-timer-keys',
whose arm function resumes it.")

(defvar agent-repl-persistent-wifi-quiet-functions nil
  "Functions told when the lid-closed quiet state begins or ends.
Each is called with one argument, non-nil when the state begins.  The web
views (webhost.el) and the page-unload pass (page-unload.el) follow it.")

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

(defun agent-repl-persistent-wifi--quiet-text (quiet)
  "Return the words for the decoded lid-closed QUIET state, nil when none."
  (when quiet
    (let ((apps (mapcar (lambda (app) (plist-get app :name)) (plist-get quiet :frozen-apps)))
          (display (plist-get quiet :display)))
      (format "quiet: %s, %s"
              (if apps (format "froze %s" (string-join apps ", ")) "froze nothing")
              (pcase (plist-get display :arm)
                (:display-asleep "display asleep")
                (:display-awake (format "display awake (%s)"
                                        (plist-get (plist-get display :value) :detail)))
                (arm (error "agent-repl-persistent-wifi: quiet display arm %S has no wording" arm)))))))

(defun agent-repl-persistent-wifi--lid-text (lid quiet)
  "Return the words for the decoded LID oneof and the lid-closed QUIET state.
Nil when the lid is not yet heard and no quiet state stands."
  (let ((lid-words
         (pcase (plist-get lid :arm)
           ('nil nil)
           (:lid-open "lid open")
           (:lid-closed "lid closed")
           (:no-lid "no lid")
           (:lid-unwatched (format "lid not watched (%s)"
                                   (plist-get (plist-get lid :value) :detail)))
           (arm (error "agent-repl-persistent-wifi: lid arm %S has no wording" arm))))
        (quiet-words (agent-repl-persistent-wifi--quiet-text quiet)))
    (when (or lid-words quiet-words)
      (string-join (delq nil (list lid-words quiet-words)) ", "))))

(defun agent-repl-persistent-wifi-describe (state)
  "Return one line describing the decoded persistent-wifi STATE."
  (string-join
   (delq nil (list (agent-repl-persistent-wifi--mode-text (plist-get state :mode))
                   (agent-repl-persistent-wifi--wifi-text (plist-get state :wifi))
                   (agent-repl-persistent-wifi--lid-text (plist-get state :lid)
                                                         (plist-get state :quiet))))
   " · "))

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

(defun agent-repl-persistent-wifi--energy-mode-text (energy-mode)
  "Return the words for the decoded ENERGY-MODE step outcome."
  (pcase (plist-get energy-mode :arm)
    (:low-power "energy: low power")
    (:high-power "energy: high power")
    (:failed (format "energy: failed (%s)"
                     (plist-get (plist-get energy-mode :value) :detail)))
    (arm (error "agent-repl-persistent-wifi: energy mode arm %S has no wording" arm))))

(defun agent-repl-persistent-wifi--display-text (display)
  "Return the words for the decoded DISPLAY step outcome."
  (let ((v (plist-get display :value)))
    (pcase (plist-get display :arm)
      (:slept "display: asleep for the closed lid")
      (:woke "display: woken")
      (:unchanged "display: unchanged")
      (:failed (format "display: failed (%s)" (plist-get v :detail)))
      (arm (error "agent-repl-persistent-wifi: display arm %S has no wording" arm)))))

;;;; ---- The standing ----------------------------------------------------

(defun agent-repl-persistent-wifi-handle (state)
  "Take the decoded persistent-wifi STATE the daemon pushed.
A CHANGE is echoed in the minibuffer; the first standing a connection is
told, and a resubscribe's replay of the same one, are only recorded."
  (let ((previous agent-repl-persistent-wifi-state))
    (setq agent-repl-persistent-wifi-state state)
    (agent-repl-persistent-wifi--follow-quiet previous state)
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

;;;; ---- The lid-closed quiet state ---------------------------------------

(defun agent-repl-persistent-wifi-quiet-p ()
  "Non-nil while the daemon says the lid-closed quiet state stands."
  (and (plist-get agent-repl-persistent-wifi-state :quiet) t))

(defun agent-repl-persistent-wifi--follow-quiet (previous state)
  "Idle Emacs when STATE begins the quiet state PREVIOUS did not hold, or wake it.
Entering pauses `agent-repl-persistent-wifi-quiet-timer-keys'; leaving
re-arms each paused one through its arm function.  Both then run
`agent-repl-persistent-wifi-quiet-functions'."
  (let ((was (and (plist-get previous :quiet) t))
        (now (and (plist-get state :quiet) t)))
    (unless (eq was now)
      (if now
          (dolist (key agent-repl-persistent-wifi-quiet-timer-keys)
            (when (agent-repl--cancel-timer-key key)
              (push key agent-repl--quiet-paused-timer-keys)))
        (dolist (key agent-repl--quiet-paused-timer-keys)
          (funcall (alist-get key agent-repl--required-timer-keys)))
        (setq agent-repl--quiet-paused-timer-keys nil))
      (agent-repl--info agent-repl-persistent-wifi--log-scope
                        "elisp.persistent-wifi.quiet quiet=%s paused=%S"
                        (if now "entered" "left") agent-repl--quiet-paused-timer-keys)
      (run-hook-with-args 'agent-repl-persistent-wifi-quiet-functions now))))

;;;; ---- The toggle ------------------------------------------------------

(defun agent-repl-persistent-wifi--on-success (value)
  "Report the decoded UpdatePersistentWifiModeSuccess VALUE.
The standing it carries is ADOPTED without a second echo: the daemon's
push of the same standing then reads as unchanged."
  (let ((state (plist-get value :state)))
    (setq agent-repl-persistent-wifi-state state)
    (agent-repl--info agent-repl-persistent-wifi--log-scope
                      "elisp.persistent-wifi.toggled standing=%S hotspot=%S energy-mode=%S display=%S"
                      state (plist-get value :hotspot) (plist-get value :energy-mode)
                      (plist-get value :display))
    (message "agent-repl: %s · %s · %s · %s"
             (agent-repl-persistent-wifi-describe state)
             (agent-repl-persistent-wifi--hotspot-text (plist-get value :hotspot))
             (agent-repl-persistent-wifi--energy-mode-text (plist-get value :energy-mode))
             (agent-repl-persistent-wifi--display-text (plist-get value :display)))))

(defun agent-repl-persistent-wifi-mode-toggle ()
  "Turn the machine's persistent wifi mode (lid-closed operation) over.
On: join the phone hotspot, disable system sleep, switch to Low Power
Mode (High Power Mode while the lid is closed on AC power, and back to Low
Power Mode when the lid opens or the charger is unplugged); while on, the daemon rejoins the hotspot whenever Wi-Fi is lost,
and a closed lid puts the machine in the lid-closed quiet state (the
display sleeps, the configured applications freeze, nothing draws) until
it opens.  Off: leave the hotspot, enable sleep, switch to High Power
Mode, and leave the quiet state if it stands.  The daemon reads
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
