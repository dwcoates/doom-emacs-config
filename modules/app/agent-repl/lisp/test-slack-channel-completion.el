;;; test-slack-channel-completion.el --- ERT tests for slack-channel-completion.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that a word starting with `#' in the composer completes
;; against exactly the Slack channels the daemon answered, matched at a name
;; part's start; that the composer asks again each time a `#' is typed;
;; that pending, a failed read and a transport failure each leave the
;; channels in hand as they stood and are logged; and that a known channel
;; is marked purple.  `ListSlackChannels' is stubbed and answers
;; synchronously, so every arm is exercised with no daemon anywhere.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fixtures ----

(defvar agent-repl-test-slack-channel-completion--requests nil
  "`ListSlackChannels' requests made, newest first.")

(defconst agent-repl-test-slack-channel-completion--channels
  '((:id "C1" :name "general" :visibility (:arm :public-channel :value nil)
     :purpose "Company-wide announcements")
    (:id "C2" :name "team-backend" :visibility (:arm :private-channel :value nil) :purpose nil)
    (:id "C3" :name "random" :visibility (:arm :public-channel :value nil) :purpose nil))
  "Decoded channels: a public one with a purpose, a private one, a bare one.")

(defun agent-repl-test-slack-channel-completion--listed (channels)
  "A `ListSlackChannels' success answer listing CHANNELS."
  (list :arm :success :value (list :arm :listed :value (list :channels channels))))

(defmacro agent-repl-test-slack-channel-completion--with (answer &rest body)
  "Run BODY in a composer owned by \"ws-1\" whose `ListSlackChannels' answers ANSWER.
ANSWER is `(:response PLIST)', `(:failure DETAIL)', or nil to answer
nothing.  The channels in hand start empty, the composer is the only
one, and a taken channel reaches the fake composer selections daemon
\(`agent-repl-test--with-composer-selections')."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local agent-repl--owning-workspace "ws-1")
     (let ((agent-repl-test-slack-channel-completion--requests nil)
           (agent-repl--slack-channel-completion-channels nil)
           (composer (current-buffer))
           (answer ,answer))
       (cl-letf (((symbol-function 'agent-repl-host-conn-or-primary)
                  (lambda (_ws) 'conn))
                 ((symbol-function 'agent-repl--input-composers)
                  (lambda () (list composer)))
                 ((symbol-function 'agent-repl-rpc-list-slack-channels)
                  (lambda (_conn request &rest keys)
                    (push request agent-repl-test-slack-channel-completion--requests)
                    (pcase (car answer)
                      (:response (funcall (plist-get keys :on-response) (cadr answer)))
                      (:failure (funcall (plist-get keys :on-failure) (cadr answer)))))))
         (agent-repl-test--with-composer-selections nil
           ,@body)))))

(defun agent-repl-test-slack-channel-completion--offered (text)
  "Return the candidates the capf offers with TEXT typed, from the fixture channels."
  (setq agent-repl--slack-channel-completion-channels
        agent-repl-test-slack-channel-completion--channels)
  (insert text)
  (when-let* ((answer (agent-repl-slack-channel-completion-at-point)))
    (let* ((table (nth 2 answer))
           (typed (buffer-substring-no-properties (nth 0 answer) (point)))
           (all (completion-all-completions typed table nil (length typed)
                                            (completion-metadata typed table nil))))
      (when (consp all) (setcdr (last all) nil))
      all)))

;;;; ---- Candidates ----

(ert-deftest agent-repl-test-slack-channel-completion-offers-a-prefix ()
  "`#gen' offers `general'."
  (agent-repl-test-slack-channel-completion--with nil
    (should (equal (agent-repl-test-slack-channel-completion--offered "#gen") '("general")))))

(ert-deftest agent-repl-test-slack-channel-completion-offers-a-later-part ()
  "`#back' offers `team-backend', whose second part it begins."
  (agent-repl-test-slack-channel-completion--with nil
    (should (equal (agent-repl-test-slack-channel-completion--offered "#back") '("team-backend")))))

(ert-deftest agent-repl-test-slack-channel-completion-falls-through-mid-part ()
  "`#ener', inside a part but at no part's start, offers nothing."
  (agent-repl-test-slack-channel-completion--with nil
    (should-not (agent-repl-test-slack-channel-completion--offered "#ener"))))

(ert-deftest agent-repl-test-slack-channel-completion-falls-through-a-double-hash ()
  "A markdown `##' heading names no channel."
  (agent-repl-test-slack-channel-completion--with nil
    (should-not (agent-repl-test-slack-channel-completion--offered "##"))))

(ert-deftest agent-repl-test-slack-channel-completion-is-nil-outside-a-hash-word ()
  "Outside a hash word the capf declines and asks nothing."
  (agent-repl-test-slack-channel-completion--with nil
    (should-not (agent-repl-test-slack-channel-completion--offered "general"))
    (should-not agent-repl-test-slack-channel-completion--requests)))

(ert-deftest agent-repl-test-slack-channel-completion-offers-nothing-without-channels ()
  "No channels in hand offers nothing."
  (agent-repl-test-slack-channel-completion--with nil
    (insert "#gen")
    (should-not (agent-repl-slack-channel-completion-at-point))))

(ert-deftest agent-repl-test-slack-channel-completion-inserts-one-hash ()
  "`completion-at-point' on a unique hash word completes it with one `#'."
  (agent-repl-test-slack-channel-completion--with
      (list :response (agent-repl-test-slack-channel-completion--listed
                       agent-repl-test-slack-channel-completion--channels))
    ;; Arrange
    (setq-local completion-at-point-functions '(agent-repl-slack-channel-completion-at-point))
    (insert "ask in #")
    (agent-repl-slack-channel-completion-at-point)
    (insert "ran")
    ;; Act
    (completion-at-point)
    ;; Assert
    (should (equal (buffer-string) "ask in #random"))))

(ert-deftest agent-repl-test-slack-channel-completion-offers-a-taken-channel-first ()
  "A channel taken before is offered first."
  (agent-repl-test-slack-channel-completion--with nil
    (agent-repl-test--with-composer-selections '(:channels ("eng-b"))
      ;; Arrange
      (setq agent-repl--slack-channel-completion-channels
            '((:id "C1" :name "eng-a" :visibility (:arm :public-channel :value nil) :purpose nil)
              (:id "C2" :name "eng-b" :visibility (:arm :public-channel :value nil) :purpose nil)))
      (insert "#eng")
      ;; Act
      (let* ((answer (agent-repl-slack-channel-completion-at-point))
             (sort (completion-metadata-get (completion-metadata "#eng" (nth 2 answer) nil)
                                            'display-sort-function)))
        ;; Assert
        (should (equal (funcall sort (list "eng-a" "eng-b")) '("eng-b" "eng-a")))))))

;;;; ---- Annotations ----

(ert-deftest agent-repl-test-slack-channel-completion-annotates-privacy-and-purpose ()
  "A private channel says so; a public one shows its purpose; a bare one nothing."
  (let ((annotate (agent-repl--slack-channel-completion-annotation
                   agent-repl-test-slack-channel-completion--channels
                   '("#general" "#team-backend" "#random"))))
    (should (equal (substring-no-properties (funcall annotate "#team-backend")) "  private"))
    (should (equal (substring-no-properties (funcall annotate "#general"))
                   "       Company-wide announcements"))
    (should (equal (funcall annotate "#random") ""))))

(ert-deftest agent-repl-test-slack-channel-completion-annotation-is-cut-to-width ()
  "An annotation longer than the width is cut, ending in an ellipsis."
  (let* ((agent-repl-slack-channel-completion-annotation-width 10)
         (annotate (agent-repl--slack-channel-completion-annotation
                    '((:id "C" :name "x" :visibility (:arm :public-channel :value nil)
                       :purpose "abcdefghijklmnop"))
                    '("#x"))))
    (should (equal (substring-no-properties (funcall annotate "#x")) "  abcdefghi…"))))

(ert-deftest agent-repl-test-slack-channel-completion-docsig-is-the-purpose ()
  "The doc signature is the channel's full purpose, nil when it has none."
  (should (equal (agent-repl--slack-channel-completion-purpose
                  agent-repl-test-slack-channel-completion--channels "#general")
                 "Company-wide announcements"))
  (should-not (agent-repl--slack-channel-completion-purpose
               agent-repl-test-slack-channel-completion--channels "#random")))

;;;; ---- Asking the daemon ----

(ert-deftest agent-repl-test-slack-channel-completion-asks-on-a-typed-hash ()
  "Typing a bare `#' asks the daemon once, naming nothing."
  (agent-repl-test-slack-channel-completion--with nil
    ;; Act
    (insert "#")
    (agent-repl-slack-channel-completion-at-point)
    (agent-repl-slack-channel-completion-at-point)
    ;; Assert
    (should (equal agent-repl-test-slack-channel-completion--requests '(nil)))))

(ert-deftest agent-repl-test-slack-channel-completion-asks-again-at-the-next-hash ()
  "Each newly typed `#' asks again."
  (agent-repl-test-slack-channel-completion--with nil
    ;; Act
    (insert "#")
    (agent-repl-slack-channel-completion-at-point)
    (insert " #")
    (agent-repl-slack-channel-completion-at-point)
    ;; Assert
    (should (= (length agent-repl-test-slack-channel-completion--requests) 2))))

(ert-deftest agent-repl-test-slack-channel-completion-longer-word-asks-only-without-channels ()
  "A pasted hash word asks only while no channels are in hand."
  (agent-repl-test-slack-channel-completion--with nil
    ;; Arrange
    (insert "#gen")
    (agent-repl-slack-channel-completion-at-point)
    (setq agent-repl--slack-channel-completion-channels
          agent-repl-test-slack-channel-completion--channels)
    (insert " #ran")
    ;; Act
    (agent-repl-slack-channel-completion-at-point)
    ;; Assert
    (should (= (length agent-repl-test-slack-channel-completion--requests) 1))))

(ert-deftest agent-repl-test-slack-channel-completion-listed-installs-the-channels ()
  "A listed answer becomes every composer's channels, and is logged."
  (agent-repl-test-slack-channel-completion--with
      (list :response (agent-repl-test-slack-channel-completion--listed
                       agent-repl-test-slack-channel-completion--channels))
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--emit-log) (agent-repl-test--record-into logged)))
        ;; Act
        (agent-repl-slack-channel-completion-refresh "ws-1"))
      ;; Assert
      (should (equal agent-repl--slack-channel-completion-channels
                     agent-repl-test-slack-channel-completion--channels))
      (should (cl-some (lambda (m) (string-match-p "slack-channel-completion.answered.*outcome=listed channels=3" m))
                       logged)))))

(ert-deftest agent-repl-test-slack-channel-completion-pending-keeps-the-channels ()
  "A pending answer leaves the channels in hand, and is logged."
  (agent-repl-test-slack-channel-completion--with
      '(:response (:arm :success :value (:arm :pending :value nil)))
    ;; Arrange
    (setq agent-repl--slack-channel-completion-channels
          agent-repl-test-slack-channel-completion--channels)
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--emit-log) (agent-repl-test--record-into logged)))
        ;; Act
        (agent-repl-slack-channel-completion-refresh "ws-1"))
      ;; Assert
      (should (equal agent-repl--slack-channel-completion-channels
                     agent-repl-test-slack-channel-completion--channels))
      (should (cl-some (lambda (m) (string-match-p "slack-channel-completion.answered.*outcome=pending" m))
                       logged)))))

(ert-deftest agent-repl-test-slack-channel-completion-read-failure-is-warned ()
  "A failed read is logged at WARN with its detail and leaves the channels."
  (agent-repl-test-slack-channel-completion--with
      '(:response (:arm :error :value (:cause (:arm :read-failed :value (:detail "gns exited 1")))))
    ;; Arrange
    (setq agent-repl--slack-channel-completion-channels
          agent-repl-test-slack-channel-completion--channels)
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--emit-warn) (agent-repl-test--record-into warned)))
        ;; Act
        (agent-repl-slack-channel-completion-refresh "ws-1"))
      ;; Assert
      (should (equal agent-repl--slack-channel-completion-channels
                     agent-repl-test-slack-channel-completion--channels))
      (should (cl-some (lambda (m) (string-match-p "slack-channel-completion.read-failed ws=ws-1 detail=\"gns exited 1\"" m))
                       warned)))))

(ert-deftest agent-repl-test-slack-channel-completion-transport-failure-is-warned ()
  "A daemon that cannot be reached is logged at WARN and leaves the channels."
  (agent-repl-test-slack-channel-completion--with '(:failure (:reason timeout))
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--emit-warn) (agent-repl-test--record-into warned)))
        ;; Act
        (agent-repl-slack-channel-completion-refresh "ws-1"))
      ;; Assert
      (should-not agent-repl--slack-channel-completion-channels)
      (should (cl-some (lambda (m) (string-match-p "slack-channel-completion.transport-failure ws=ws-1" m))
                       warned)))))

(ert-deftest agent-repl-test-slack-channel-completion-no-connection-asks-nothing ()
  "With no connection nothing is asked, and the skip is logged."
  (agent-repl-test-slack-channel-completion--with nil
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl-host-conn-or-primary) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--emit-log) (agent-repl-test--record-into logged)))
        ;; Act
        (agent-repl-slack-channel-completion-refresh "ws-1"))
      ;; Assert
      (should-not agent-repl-test-slack-channel-completion--requests)
      (should (cl-some (lambda (m) (string-match-p "refresh-skipped ws=ws-1 reason=no-connection" m))
                       logged)))))

(ert-deftest agent-repl-test-slack-channel-completion-unknown-arm-is-an-error ()
  "A response arm this file does not know is logged at ERROR."
  (let ((errors nil))
    (cl-letf (((symbol-function 'agent-repl--emit-error) (agent-repl-test--record-into errors)))
      ;; Act
      (agent-repl--slack-channel-completion-answered "ws-1" '(:arm :surprise :value nil)))
    ;; Assert
    (should (cl-some (lambda (m) (string-match-p "unknown-response-arm ws=ws-1 arm=:surprise" m))
                     errors))))

;;;; ---- Marking known channels ----

(defun agent-repl-test-slack-channel-completion--marked ()
  "Return the texts of the composer's known-channel marks, in order."
  (mapcar (lambda (ov) (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
          (sort (cl-remove-if-not (lambda (ov) (overlay-get ov 'agent-repl-slack-channel-completion-known))
                                  (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest agent-repl-test-slack-channel-completion-marks-a-known-channel ()
  "A typed hash word naming a known channel is marked; an unknown one is not."
  (agent-repl-test-slack-channel-completion--with nil
    ;; Arrange
    (setq agent-repl--slack-channel-completion-channels
          agent-repl-test-slack-channel-completion--channels)
    (add-hook 'after-change-functions #'agent-repl--slack-channel-completion-on-change nil t)
    ;; Act
    (insert "post in #general and #nowhere, or (#random).")
    ;; Assert
    (should (equal (agent-repl-test-slack-channel-completion--marked) '("#general" "#random")))))

(ert-deftest agent-repl-test-slack-channel-completion-new-channels-re-mark-the-composer ()
  "Channels arriving after the text was typed mark it."
  (agent-repl-test-slack-channel-completion--with
      (list :response (agent-repl-test-slack-channel-completion--listed
                       agent-repl-test-slack-channel-completion--channels))
    ;; Arrange
    (insert "post in #general")
    ;; Act
    (agent-repl-slack-channel-completion-refresh "ws-1")
    ;; Assert
    (should (equal (agent-repl-test-slack-channel-completion--marked) '("#general")))))

(provide 'test-slack-channel-completion)
;;; test-slack-channel-completion.el ends here
