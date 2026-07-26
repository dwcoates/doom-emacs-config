;;; test-failure.el --- Tests for failure.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The classified-failure vocabulary (F4): the namespace partition, the
;; closed local type set, wire adoption, and the single surfacing point.
;; One edge case per test (AAA).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

;;;; ---- Namespace partition ---------------------------------------------

(ert-deftest agent-repl-test-failure-local-types-carry-the-client-prefix ()
  "Every LOCAL failure type is in the frontend's reserved namespace.
A bare type minted here would mean Emacs re-classified something the
daemon already decided."
  ;; Act / Assert
  (dolist (type agent-repl-failure-local-types)
    (should (agent-repl-failure-client-type-p type))))

(ert-deftest agent-repl-test-failure-daemon-type-is-not-a-client-type ()
  "A daemon-owned type is recognized as NOT the frontend's.
The other direction of the partition: a `client.'-prefixed type on the
wire would mean a frontend's failure was laundered through the daemon."
  ;; Act / Assert
  (should-not (agent-repl-failure-client-type-p "shim.rejected")))

(ert-deftest agent-repl-test-failure-client-type-p-rejects-a-non-string ()
  "A non-string is not a client type (and does not error)."
  ;; Act / Assert
  (should-not (agent-repl-failure-client-type-p nil)))

;;;; ---- Class decoding --------------------------------------------------

(ert-deftest agent-repl-test-failure-class-internal ()
  "ERROR_CLASS_INTERNAL decodes to `:internal'."
  ;; Act / Assert
  (should (eq (agent-repl-failure-class "ERROR_CLASS_INTERNAL") :internal)))

(ert-deftest agent-repl-test-failure-class-api ()
  "ERROR_CLASS_API decodes to `:api'."
  ;; Act / Assert
  (should (eq (agent-repl-failure-class "ERROR_CLASS_API") :api)))

(ert-deftest agent-repl-test-failure-class-unspecified-errors ()
  "An UNSPECIFIED class fails loudly rather than defaulting.
The class decides the failure's color, so a default would paint a failure
some arbitrary color — quietly, and possibly contradicting the workspace
colored beside it."
  ;; Act / Assert
  (should-error (agent-repl-failure-class "ERROR_CLASS_UNSPECIFIED")))

(ert-deftest agent-repl-test-failure-class-unknown-errors ()
  "A class outside the vocabulary fails loudly."
  ;; Act / Assert
  (should-error (agent-repl-failure-class "ERROR_CLASS_SOMETHING_NEW")))

(ert-deftest agent-repl-test-failure-every-class-decodes ()
  "Every class keyword has a wire name that decodes back to it."
  ;; Act / Assert
  (dolist (class agent-repl-failure-classes)
    (should (rassq class agent-repl-failure-class-wire))))

;;;; ---- Wire adoption ---------------------------------------------------

(ert-deftest agent-repl-test-failure-from-wire-adopts-the-class ()
  "The daemon's class is adopted, not re-derived."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:errorClass "ERROR_CLASS_API" :errorType "api.rate_limit"
              :message "rate limited"))))
    ;; Assert
    (should (eq (plist-get f :class) :api))))

(ert-deftest agent-repl-test-failure-from-wire-adopts-the-type ()
  "The daemon's type rides through unchanged."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:errorClass "ERROR_CLASS_API" :errorType "api.rate_limit"
              :message "rate limited"))))
    ;; Assert
    (should (equal (plist-get f :type) "api.rate_limit"))))

(ert-deftest agent-repl-test-failure-from-wire-defaults-absent-fields ()
  "Absent optional fields become empty rather than nil.
protojson omits a zero/empty field, so an absent source_detail is the
common case, not a malformed frame."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:errorClass "ERROR_CLASS_INTERNAL" :errorType "shim.rejected"
              :message "rejected"))))
    ;; Assert
    (should (equal (plist-get f :detail) ""))
    (should (equal (plist-get f :resolved-at) 0))))

(ert-deftest agent-repl-test-failure-from-wire-carries-the-resolution ()
  "A resolved window's stamp rides through."
  ;; Arrange / Act
  (let ((f (agent-repl-failure-from-wire
            '(:errorClass "ERROR_CLASS_INTERNAL" :errorType "shim.degraded"
              :message "no traffic" :resolvedAtMs 1700000000000))))
    ;; Assert
    (should (equal (plist-get f :resolved-at) 1700000000000))))

;;;; ---- Local construction ----------------------------------------------

(ert-deftest agent-repl-test-failure-local-is-internal-class ()
  "A locally-classified failure is always INTERNAL.
Nothing Emacs can observe implicates the account, so an `:api' local
failure would be Emacs guessing at something only the daemon can see."
  ;; Act
  (let ((f (agent-repl-failure-local "client.daemon_unreachable" "gone")))
    ;; Assert
    (should (eq (plist-get f :class) :internal))))

(ert-deftest agent-repl-test-failure-local-rejects-an-unlisted-type ()
  "A type outside the closed local set fails loudly.
That is what keeps the local vocabulary closed rather than accumulating a
type per call site."
  ;; Act / Assert
  (should-error (agent-repl-failure-local "client.something_new" "x")))

(ert-deftest agent-repl-test-failure-local-rejects-a-daemon-type ()
  "A DAEMON-owned type cannot be minted locally."
  ;; Act / Assert
  (should-error (agent-repl-failure-local "shim.rejected" "x")))

;;;; ---- Surfacing -------------------------------------------------------

(ert-deftest agent-repl-test-failure-text-leads-with-the-prose ()
  "The human sentence leads and the raw account follows."
  ;; Arrange / Act
  (let ((text (agent-repl-failure-text
               '(:message "the agent process rejected the request"
                 :detail "shimclient: request nacked"))))
    ;; Assert
    (should (string-prefix-p "the agent process rejected the request" text))))

(ert-deftest agent-repl-test-failure-text-keeps-the-detail-as-evidence ()
  "The raw account survives beside the prose rather than replacing it."
  ;; Arrange / Act
  (let ((text (agent-repl-failure-text
               '(:message "rejected" :detail "shimclient: request nacked"))))
    ;; Assert
    (should (string-match-p "shimclient: request nacked" text))))

(ert-deftest agent-repl-test-failure-text-omits-an-empty-detail ()
  "A failure with no raw account reads as the sentence alone."
  ;; Arrange / Act
  (let ((text (agent-repl-failure-text '(:message "rejected" :detail ""))))
    ;; Assert
    (should (equal text "rejected"))))

(ert-deftest agent-repl-test-failure-surface-echoes-an-open-failure ()
  "An unresolved failure reaches the echo area."
  ;; Arrange
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (agent-repl-failure-surface nil '(:class :internal :type "shim.rejected"
                                        :message "rejected" :detail "" :resolved-at 0))
      ;; Assert
      (should (string-match-p "rejected" echoed)))))

(ert-deftest agent-repl-test-failure-surface-does-not-echo-a-resolution ()
  "A RESOLVED failure is logged and not echoed.
The echo area is for what is happening now; announcing the end of
something the user may never have seen the start of is noise."
  ;; Arrange
  (let (echoed)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
      ;; Act
      (agent-repl-failure-surface nil '(:class :internal :type "shim.degraded"
                                        :message "no traffic" :detail ""
                                        :resolved-at 1700000000000))
      ;; Assert
      (should (null echoed)))))

(ert-deftest agent-repl-test-failure-surface-returns-the-echoed-text ()
  "Surfacing answers what it echoed, so a caller can assert on it."
  ;; Arrange
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    ;; Act / Assert
    (should (equal (agent-repl-failure-surface
                    nil '(:class :internal :type "shim.rejected"
                          :message "rejected" :detail "" :resolved-at 0))
                   "rejected"))))

(provide 'test-failure)
;;; test-failure.el ends here
