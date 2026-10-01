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
                (lambda (_ws fmt &rest args) (push (cons 'info (apply #'format fmt args)) logged-rev)))
               ((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args) (push (cons 'debug (apply #'format fmt args)) logged-rev))))
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

(ert-deftest agent-repl-test-pwifi-display-text-words-every-arm ()
  "Every display outcome arm has its own words."
  (dolist (case '(((:arm :dimmed :value nil) . "display: dimmed")
                  ((:arm :restored :value nil) . "display: restored")
                  ((:arm :tool-missing :value (:tool-path "/b/x")) . "display: /b/x is not installed")
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
  "A success echoes the standing and both steps in one line."
  (agent-repl-test-pwifi--capturing
    ;; Act
    (agent-repl-persistent-wifi--on-success
     (list :state (agent-repl-test-pwifi--state :joined :on "Phone")
           :hotspot '(:arm :joined :value (:network-name "Phone"))
           :display '(:arm :dimmed :value nil)))
    ;; Assert
    (should (equal (echoed)
                   '("agent-repl: persistent wifi on: the lid can close · Wi-Fi joined to Phone · hotspot: joined Phone · display: dimmed")))))

(ert-deftest agent-repl-test-pwifi-a-success-is-not-echoed-again-by-its-push ()
  "The success adopts its standing, so the daemon's push of it is silent."
  (agent-repl-test-pwifi--capturing
    ;; Arrange
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :off))
    (agent-repl-persistent-wifi--on-success
     (list :state (agent-repl-test-pwifi--state :joined :on)
           :hotspot '(:arm :not-on-hotspot :value nil)
           :display '(:arm :dimmed :value nil)))
    ;; Act
    (agent-repl-persistent-wifi-handle (agent-repl-test-pwifi--state :joined :on))
    ;; Assert
    (should (= (length (echoed)) 1))))

(provide 'test-persistent-wifi)

;;; test-persistent-wifi.el ends here
