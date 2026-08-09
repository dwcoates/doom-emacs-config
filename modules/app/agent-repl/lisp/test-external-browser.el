;;; test-external-browser.el --- ERT tests for external-browser.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for routing every hyperlink into the pinned Chrome profile.  The
;; single external boundary (`agent-repl--external-browser-call-process',
;; which spawns Chrome and `osascript') is stubbed with `cl-letf' per
;; test, so nothing here launches a browser or steals focus.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-external-browser.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)
(require 'browse-url)

;;;; ---- Fixtures -------------------------------------------------------------

(defvar agent-repl-test--browser-calls nil
  "Recorded (PROGRAM . ARGS) invocations of the external-browser boundary.
Left populated after `agent-repl-test--with-browser-exits' returns so
assertions read it once the stub is gone.")

(defvar agent-repl-test--browser-exits nil
  "Remaining stubbed exit codes for `agent-repl-test--with-browser-exits'.")

(defmacro agent-repl-test--with-browser-exits (exits &rest body)
  "Run BODY with the external-browser boundary stubbed.
EXITS is a list of exit codes handed out one per invocation, in order; a
call past the end of the list gets 0.  Every invocation is recorded, in
call order, in `agent-repl-test--browser-calls' as (PROGRAM . ARGS)."
  (declare (indent 1))
  `(progn
     (setq agent-repl-test--browser-calls nil
           agent-repl-test--browser-exits ,exits)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--external-browser-call-process)
                    (lambda (program &rest args)
                      (push (cons program args) agent-repl-test--browser-calls)
                      (or (pop agent-repl-test--browser-exits) 0))))
           ,@body)
       (setq agent-repl-test--browser-calls
             (nreverse agent-repl-test--browser-calls)))))

;;;; ---- Boundary registration ------------------------------------------------

(ert-deftest agent-repl-test-external-browser-call-process-is-registered-boundary ()
  "The spawn wrapper must be in the external-boundary registry so the
harness guards it (else a test could launch a real browser)."
  (should (memq 'agent-repl--external-browser-call-process
                agent-repl--external-boundary-functions)))

;;;; ---- Command construction -------------------------------------------------

(ert-deftest agent-repl-test-external-browser-launch-args-pin-the-profile ()
  "The Chrome argv names the configured profile directory, then the URL."
  ;; Arrange.
  (let ((agent-repl-external-browser-profile "Profile 6"))
    ;; Act.
    (let ((args (agent-repl--external-browser-launch-args "https://example.com/")))
      ;; Assert.
      (should (equal args '("--profile-directory=Profile 6"
                            "https://example.com/"))))))

(ert-deftest agent-repl-test-external-browser-launch-args-follow-the-profile-setting ()
  "Retargeting the profile setting retargets the argv."
  ;; Arrange.
  (let ((agent-repl-external-browser-profile "Profile 7"))
    ;; Act.
    (let ((args (agent-repl--external-browser-launch-args "https://example.com/")))
      ;; Assert.
      (should (equal (car args) "--profile-directory=Profile 7")))))

(ert-deftest agent-repl-test-external-browser-activate-args-name-the-app ()
  "The focus argv is an AppleScript activating the configured app."
  ;; Arrange.
  (let ((agent-repl-external-browser-app "Google Chrome"))
    ;; Act.
    (let ((args (agent-repl--external-browser-activate-args)))
      ;; Assert.
      (should (equal args
                     '("-e" "tell application \"Google Chrome\" to activate"))))))

;;;; ---- agent-repl-open-external-url -----------------------------------------

(ert-deftest agent-repl-test-open-external-url-launches-chrome-with-the-url ()
  "The happy path spawns the configured Chrome binary with the profile argv."
  ;; Arrange.
  (let ((agent-repl-external-browser-binary "/chrome")
        (agent-repl-external-browser-profile "Profile 6"))
    ;; Act.
    (agent-repl-test--with-browser-exits nil
      (agent-repl-open-external-url "https://example.com/x"))
    ;; Assert.
    (should (equal (length agent-repl-test--browser-calls) 2))
    (should (equal (nth 1 agent-repl-test--browser-calls)
                   '("/chrome" "--profile-directory=Profile 6"
                     "https://example.com/x")))))

(ert-deftest agent-repl-test-open-external-url-activates-before-launching ()
  "Focus is taken FIRST, which is what lands it on the right window.
Chrome raises the profile window it puts the tab in but never fronts
itself, so an activation that came second would restore whichever window
was frontmost before -- routinely the other profile's."
  ;; Arrange.
  (let ((agent-repl-external-browser-binary "/chrome")
        (agent-repl-external-browser-app "Google Chrome"))
    ;; Act.
    (agent-repl-test--with-browser-exits nil
      (agent-repl-open-external-url "https://example.com/x"))
    ;; Assert.
    (should (equal (car agent-repl-test--browser-calls)
                   '("osascript" "-e"
                     "tell application \"Google Chrome\" to activate")))))

(ert-deftest agent-repl-test-open-external-url-returns-the-url ()
  "The opened URL is returned so callers can chain on it."
  ;; Arrange/Act.
  (let (result)
    (agent-repl-test--with-browser-exits nil
      (setq result (agent-repl-open-external-url "https://example.com/")))
    ;; Assert.
    (should (equal result "https://example.com/"))))

(ert-deftest agent-repl-test-open-external-url-accepts-plain-http ()
  "Plain http is a browser destination too, not only https."
  ;; Arrange/Act.
  (agent-repl-test--with-browser-exits nil
    (agent-repl-open-external-url "http://example.com/"))
  ;; Assert.
  (should (equal (length agent-repl-test--browser-calls) 2)))

;;;; ---- Rejected input -------------------------------------------------------

(ert-deftest agent-repl-test-open-external-url-rejects-a-non-http-scheme ()
  "A non-http scheme is refused rather than handed to a browser command line."
  ;; Arrange/Act/Assert.
  (agent-repl-test--with-browser-exits nil
    (should-error (agent-repl-open-external-url "file:///etc/passwd"))))

(ert-deftest agent-repl-test-open-external-url-spawns-nothing-when-rejected ()
  "A refused URL must not reach the external boundary at all."
  ;; Arrange/Act.
  (agent-repl-test--with-browser-exits nil
    (should-error (agent-repl-open-external-url "file:///etc/passwd")))
  ;; Assert.
  (should (null agent-repl-test--browser-calls)))

(ert-deftest agent-repl-test-open-external-url-rejects-a-non-string ()
  "A non-string URL is a caller bug and fails loudly."
  ;; Arrange/Act/Assert.
  (agent-repl-test--with-browser-exits nil
    (should-error (agent-repl-open-external-url nil))))

(ert-deftest agent-repl-test-open-external-url-rejects-an-empty-string ()
  "An empty URL names no page and is refused."
  ;; Arrange/Act/Assert.
  (agent-repl-test--with-browser-exits nil
    (should-error (agent-repl-open-external-url ""))))

;;;; ---- Failure paths --------------------------------------------------------

(ert-deftest agent-repl-test-open-external-url-signals-on-a-failed-launch ()
  "A non-zero Chrome exit is surfaced, never swallowed into a dead click."
  ;; Arrange/Act/Assert.
  (agent-repl-test--with-browser-exits '(0 1)
    (should-error (agent-repl-open-external-url "https://example.com/"))))

(ert-deftest agent-repl-test-open-external-url-signals-on-a-failed-activate ()
  "Losing focus is a failure of the contract, so it is surfaced too."
  ;; Arrange/Act/Assert.
  (agent-repl-test--with-browser-exits '(1)
    (should-error (agent-repl-open-external-url "https://example.com/"))))

(ert-deftest agent-repl-test-open-external-url-does-not-launch-after-a-failed-activate ()
  "No URL is handed over when the browser could not be raised."
  ;; Arrange/Act.
  (agent-repl-test--with-browser-exits '(1)
    (should-error (agent-repl-open-external-url "https://example.com/")))
  ;; Assert.
  (should (equal (length agent-repl-test--browser-calls) 1)))

;;;; ---- browse-url entry point -----------------------------------------------

(ert-deftest agent-repl-test-browse-url-external-opens-the-url ()
  "The `browse-url' entry point routes to the external opener."
  ;; Arrange.
  (let ((agent-repl-external-browser-binary "/chrome"))
    ;; Act.
    (agent-repl-test--with-browser-exits nil
      (agent-repl-browse-url-external "https://example.com/"))
    ;; Assert.
    (should (equal (car (nth 1 agent-repl-test--browser-calls)) "/chrome"))))

(ert-deftest agent-repl-test-browse-url-external-ignores-browse-url-extra-args ()
  "`browse-url' passes a NEW-WINDOW argument that has no meaning here."
  ;; Arrange/Act.
  (agent-repl-test--with-browser-exits nil
    (agent-repl-browse-url-external "https://example.com/" t))
  ;; Assert.
  (should (equal (length agent-repl-test--browser-calls) 2)))

;;;; ---- Installation ---------------------------------------------------------

(ert-deftest agent-repl-test-external-browser-install-pins-browse-url ()
  "Loading the module leaves `browse-url' pointed at the external opener."
  (should (eq browse-url-browser-function #'agent-repl-browse-url-external)))

(ert-deftest agent-repl-test-external-browser-install-drops-the-emacs-handler ()
  "The stock non-HTML rule that visits URLs inside Emacs is removed."
  (should-not (rassq 'browse-url-emacs browse-url-default-handlers)))

(ert-deftest agent-repl-test-external-browser-strip-keeps-non-browser-handlers ()
  "`mailto:' dispatches to a mail client, so its handler is left alone."
  ;; Arrange.
  (let ((handlers '(("\\`mailto:" . browse-url--mailto)
                    (browse-url--non-html-file-url-p . browse-url-emacs))))
    ;; Act.
    (let ((kept (agent-repl--external-browser-strip-emacs-handlers handlers)))
      ;; Assert.
      (should (equal kept '(("\\`mailto:" . browse-url--mailto)))))))

(provide 'test-external-browser)
;;; test-external-browser.el ends here
