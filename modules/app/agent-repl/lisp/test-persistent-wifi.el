;;; test-persistent-wifi.el --- ERT tests for persistent-wifi.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-persistent-wifi.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures --------------------------------------------------------

(defun agent-repl-test-pwifi--state (wifi mode &optional name)
  "A decoded standing: WIFI and MODE arm keywords (nil = unread), joined NAME."
  (list :wifi (and wifi (list :arm wifi :value (and (eq wifi :joined) (list :network-name name))))
        :mode (and mode (list :arm mode :value nil))))

(defmacro agent-repl-test-pwifi--capturing (&rest body)
  "Run BODY with a fresh standing, capturing echoes and log records.
Binds `echoed' and `logged' (each oldest first after BODY) for assertions
written inside BODY after the act."
  (declare (indent 0))
  `(let ((agent-repl-persistent-wifi-state nil)
         (echoed-rev nil)
         (logged-rev nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) echoed-rev)))
               ((symbol-function 'agent-repl--info)
                (agent-repl-test--record-leveled-into logged-rev 'info))
               ((symbol-function 'agent-repl--log)
                (agent-repl-test--record-leveled-into logged-rev 'debug)))
       (cl-flet ((echoed () (reverse echoed-rev))
                 (logged () (reverse logged-rev)))
         ,@body))))

;;;; ---- Wording ---------------------------------------------------------

(ert-deftest agent-repl-test-pwifi-describe-names-both-facts ()
  "Each combination of the two facts reads as the mode, then the Wi-Fi."
  (dolist (case `((,(agent-repl-test-pwifi--state :joined :on "Home")
                   . "persistent wifi on: the lid can close · Wi-Fi joined to Home")
                  (,(agent-repl-test-pwifi--state :joined :off nil)
                   . "persistent wifi off: closing the lid sleeps · Wi-Fi joined (network name withheld by macOS)")
                  (,(agent-repl-test-pwifi--state :not-joined :on)
                   . "persistent wifi on: the lid can close · no Wi-Fi")
                  (,(agent-repl-test-pwifi--state nil nil)
                   . "persistent wifi could not be read · Wi-Fi could not be read")))
    (should (equal (agent-repl-persistent-wifi-describe (car case)) (cdr case)))))

(ert-deftest agent-repl-test-pwifi-describe-names-the-lid-and-the-quiet-state ()
  "The lid and a lid-closed quiet state follow the mode and the Wi-Fi."
  (dolist (case '(((:arm :lid-open :value nil) nil . "lid open")
                  ((:arm :lid-closed :value nil)
                   (:frozen-apps ((:name "Slack" :processes 4) (:name "Spotify" :processes 2))
                    :display (:arm :display-asleep :value nil))
                   . "lid closed, quiet: froze Slack, Spotify, display asleep")
                  ((:arm :lid-closed :value nil)
                   (:frozen-apps nil :display (:arm :display-awake :value (:detail "refused")))
                   . "lid closed, quiet: froze nothing, display awake (refused)")
                  ((:arm :no-lid :value nil) nil . "no lid")
                  ((:arm :lid-unwatched :value (:detail "IOPMrootDomain was not found")) nil
                   . "lid not watched (IOPMrootDomain was not found)")))
    (let ((state (append (agent-repl-test-pwifi--state :not-joined :on)
                         (list :lid (nth 0 case) :quiet (nth 1 case)))))
      (should (equal (agent-repl-persistent-wifi-describe state)
                     (concat "persistent wifi on: the lid can close · no Wi-Fi · "
                             (nthcdr 2 case)))))))

(ert-deftest agent-repl-test-pwifi-describe-refuses-an-unknown-quiet-display-arm ()
  "A quiet display arm the wording does not know fails hard."
  (should-error (agent-repl-persistent-wifi-describe
                 (append (agent-repl-test-pwifi--state :joined :on)
                         '(:quiet (:frozen-apps nil :display (:arm :dimmed)))))))

(ert-deftest agent-repl-test-pwifi-describe-refuses-an-unknown-lid-arm ()
  "A lid arm the wording does not know fails hard."
  (should-error (agent-repl-persistent-wifi-describe
                 (append (agent-repl-test-pwifi--state :joined :on) '(:lid (:arm :ajar))))))

(ert-deftest agent-repl-test-pwifi-describe-refuses-an-unknown-arm ()
  "An arm the wording does not know fails hard rather than reading as unread."
  (should-error (agent-repl-persistent-wifi-describe '(:wifi (:arm :radio-off) :mode nil))))

(ert-deftest agent-repl-test-pwifi-hotspot-text-words-every-arm ()
  "Every hotspot outcome arm has its own words."
  (dolist (case '(((:arm :joined :value (:network-name "P")) . "hotspot: joined P")
                  ((:arm :already-joined :value (:network-name "P")) . "hotspot: already on P")
                  ((:arm :left :value (:network-name "P")) . "hotspot: left P")
                  ((:arm :not-on-hotspot :value nil) . "hotspot: not on it")
                  ((:arm :network-unreadable :value nil)
                   . "hotspot: left nothing (network name withheld by macOS)")
                  ((:arm :no-wifi-interface :value nil) . "hotspot: no Wi-Fi interface")
                  ((:arm :failed :value (:network-name "P" :detail "gone"))
                   . "hotspot: P did not take (gone)")))
    (should (equal (agent-repl-persistent-wifi--hotspot-text (car case)) (cdr case)))))

(ert-deftest agent-repl-test-pwifi-energy-mode-text-words-every-arm ()
  "Every energy mode outcome arm has its own words."
  (dolist (case '(((:arm :low-power :value nil) . "energy: low power")
                  ((:arm :high-power :value nil) . "energy: high power")
                  ((:arm :failed :value (:detail "refused")) . "energy: failed (refused)")))
    (should (equal (agent-repl-persistent-wifi--energy-mode-text (car case)) (cdr case)))))

(ert-deftest agent-repl-test-pwifi-energy-mode-text-refuses-an-unworded-arm ()
  "An energy mode arm with no wording is a bug, signalled."
  (should-error (agent-repl-persistent-wifi--energy-mode-text '(:arm :turbo :value nil))))

(ert-deftest agent-repl-test-pwifi-display-text-words-every-arm ()
  "Every display outcome arm has its own words."
  (dolist (case '(((:arm :slept :value nil) . "display: asleep for the closed lid")
                  ((:arm :woke :value nil) . "display: woken")
                  ((:arm :unchanged :value nil) . "display: unchanged")
                  ((:arm :failed :value (:detail "no panel")) . "display: failed (no panel)")))
    (should (equal (agent-repl-persistent-wifi--display-text (car case)) (cdr case)))))

;;;; ---- The standing ----------------------------------------------------

(ert-deftest agent-repl-test-pwifi-the-first-standing-is-kept-not-echoed ()
  "The first standing a connection is told is recorded at INFO, not echoed."
  (agent-repl-test-pwifi--capturing
    ;; Act
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :off))
    ;; Assert
    (should (equal agent-repl-persistent-wifi-state (agent-repl-test-pwifi--state :joined :off)))
    (should (null (echoed)))
    (should (string-prefix-p "elisp.persistent-wifi.standing" (cdr (car (logged)))))))

(ert-deftest agent-repl-test-pwifi-a-change-is-echoed ()
  "A standing that differs from the last one is echoed in the minibuffer."
  (agent-repl-test-pwifi--capturing
    ;; Arrange
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :off))
    ;; Act
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :not-joined :off))
    ;; Assert
    (should (equal (echoed) '("agent-repl: persistent wifi off: closing the lid sleeps · no Wi-Fi")))
    (should (assoc 'info (logged)))))

(ert-deftest agent-repl-test-pwifi-a-replayed-standing-is-silent ()
  "A resubscribe's replay of the same standing is recorded at debug only."
  (agent-repl-test-pwifi--capturing
    ;; Arrange
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :on))
    ;; Act
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :on))
    ;; Assert
    (should (null (echoed)))
    (should (eq (car (car (last (logged)))) 'debug))))

;;;; ---- The lid-closed quiet state ---------------------------------------

(defconst agent-repl-test-pwifi--quiet
  '(:frozen-apps ((:name "Slack" :processes 4)) :display (:arm :display-asleep :value nil))
  "A decoded lid-closed quiet state.")

(defmacro agent-repl-test-pwifi--following-quiet (&rest body)
  "Run BODY with fake keyed timers and a recording quiet hook.
Binds `cancelled', `armed' and `told' (oldest first) for BODY's assertions.
Each of `agent-repl-persistent-wifi-quiet-timer-keys' is armed until
cancelled, and re-arming one records it."
  (declare (indent 0))
  `(let ((agent-repl--quiet-paused-timer-keys nil)
         (agent-repl--required-timer-keys
          '((:state-poll . agent-repl-test-pwifi--arm-state-poll)
            (:stall-sampler . agent-repl-test-pwifi--arm-stall)))
         (agent-repl-persistent-wifi-quiet-functions nil)
         (cancelled-rev nil) (armed-rev nil) (told-rev nil))
     (add-hook 'agent-repl-persistent-wifi-quiet-functions (lambda (q) (push q told-rev)))
     (cl-letf (((symbol-function 'agent-repl--cancel-timer-key)
                (lambda (key) (push key cancelled-rev) t))
               ((symbol-function 'agent-repl-test-pwifi--arm-state-poll)
                (lambda () (push :state-poll armed-rev)))
               ((symbol-function 'agent-repl-test-pwifi--arm-stall)
                (lambda () (push :stall-sampler armed-rev))))
       (cl-flet ((cancelled () (reverse cancelled-rev))
                 (armed () (reverse armed-rev))
                 (told () (reverse told-rev)))
         ,@body))))

(ert-deftest agent-repl-test-pwifi-entering-the-quiet-state-pauses-the-timers-and-tells-the-hook ()
  "A standing that begins the quiet state pauses the repaint and stall timers."
  (agent-repl-test-pwifi--capturing
    (agent-repl-test-pwifi--following-quiet
      ;; Act
      (agent-repl-persistent-wifi-handle
       (append (agent-repl-test-pwifi--state :joined :on) (list :quiet agent-repl-test-pwifi--quiet)))
      ;; Assert
      (should (equal (cancelled) '(:state-poll :stall-sampler)))
      (should (equal (sort (copy-sequence agent-repl--quiet-paused-timer-keys) #'string<)
                     '(:stall-sampler :state-poll)))
      (should (equal (told) '(t)))
      (should (agent-repl-persistent-wifi-quiet-p)))))

(ert-deftest agent-repl-test-pwifi-leaving-the-quiet-state-re-arms-what-it-paused ()
  "A standing that ends the quiet state re-arms each paused timer once."
  (agent-repl-test-pwifi--capturing
    (agent-repl-test-pwifi--following-quiet
      ;; Arrange
      (agent-repl-persistent-wifi-handle
       (append (agent-repl-test-pwifi--state :joined :on) (list :quiet agent-repl-test-pwifi--quiet)))
      ;; Act
      (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :on))
      ;; Assert
      (should (equal (sort (armed) #'string<) '(:stall-sampler :state-poll)))
      (should (null agent-repl--quiet-paused-timer-keys))
      (should (equal (told) '(t nil)))
      (should-not (agent-repl-persistent-wifi-quiet-p)))))

(ert-deftest agent-repl-test-pwifi-a-standing-that-keeps-the-quiet-state-moves-nothing ()
  "Two quiet standings in a row pause once and tell the hook once."
  (agent-repl-test-pwifi--capturing
    (agent-repl-test-pwifi--following-quiet
      ;; Act
      (dolist (name '("Home" "Office"))
        (agent-repl-persistent-wifi-handle
         (append (agent-repl-test-pwifi--state :joined :on name) (list :quiet agent-repl-test-pwifi--quiet))))
      ;; Assert
      (should (equal (cancelled) '(:state-poll :stall-sampler)))
      (should (equal (told) '(t))))))

(ert-deftest agent-repl-test-pwifi-the-quiet-state-is-recorded ()
  "Entering the quiet state is an INFO record naming what was paused."
  (agent-repl-test-pwifi--capturing
    (agent-repl-test-pwifi--following-quiet
      ;; Act
      (agent-repl-persistent-wifi-handle
       (append (agent-repl-test-pwifi--state :joined :on) (list :quiet agent-repl-test-pwifi--quiet)))
      ;; Assert
      (should (cl-some (lambda (r) (and (eq (car r) 'info)
                                        (string-prefix-p "elisp.persistent-wifi.quiet quiet=entered" (cdr r))))
                       (logged))))))

;;;; ---- The toggle ------------------------------------------------------

(ert-deftest agent-repl-test-pwifi-toggle-sends-the-toggle-arm ()
  "The command sends UpdatePersistentWifiMode{toggle} with its own deadline."
  ;; Arrange
  (let (sent)
    (cl-letf (((symbol-function 'agent-repl-verbs--conn) (lambda (&optional _) 'conn))
              ((symbol-function 'agent-repl-verbs--send)
               (lambda (rpc conn request &rest keys) (setq sent (list rpc conn request keys)))))
      ;; Act
      (agent-repl-persistent-wifi-mode-toggle))
    ;; Assert
    (should (eq (nth 0 sent) #'agent-repl-rpc-update-persistent-wifi-mode))
    (should (eq (nth 1 sent) 'conn))
    (should (equal (nth 2 sent) '(:action (:arm :toggle))))
    (should (equal (plist-get (nth 3 sent) :timeout) agent-repl-persistent-wifi-timeout-seconds))
    (should (eq (plist-get (nth 3 sent) :on-success) #'agent-repl-persistent-wifi--on-success))))

(ert-deftest agent-repl-test-pwifi-toggle-is-interactive ()
  "The toggle is a command, so `SPC j w' can run it."
  (should (commandp #'agent-repl-persistent-wifi-mode-toggle)))

(ert-deftest agent-repl-test-pwifi-a-success-is-echoed-whole ()
  "A success echoes the standing and every step in one line."
  (agent-repl-test-pwifi--capturing
    ;; Act
    (agent-repl-persistent-wifi--on-success
     (list :state (agent-repl-test-pwifi--state :joined :on "Phone")
           :hotspot '(:arm :joined :value (:network-name "Phone"))
           :energy-mode '(:arm :low-power :value nil)
           :display '(:arm :unchanged :value nil)))
    ;; Assert
    (should (equal (echoed)
                   '("agent-repl: persistent wifi on: the lid can close · Wi-Fi joined to Phone · hotspot: joined Phone · energy: low power · display: unchanged")))))

(ert-deftest agent-repl-test-pwifi-a-success-is-not-echoed-again-by-its-push ()
  "The success adopts its standing, so the daemon's push of it is silent."
  (agent-repl-test-pwifi--capturing
    ;; Arrange
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :off))
    (agent-repl-persistent-wifi--on-success
     (list :state (agent-repl-test-pwifi--state :joined :on)
           :hotspot '(:arm :not-on-hotspot :value nil)
           :energy-mode '(:arm :low-power :value nil)
           :display '(:arm :slept :value nil)))
    ;; Act
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :on))
    ;; Assert
    (should (= (length (echoed)) 1))))

(provide 'test-persistent-wifi)

;;; test-persistent-wifi.el ends here
