;;; test-startup.el --- ERT tests for agent-repl startup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-startup.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The startup's run is driven with DECODED `DaemonStartupEvent' plists, the
;; shape wire-host.el produces.  The roster and the workspace registry are
;; stubbed at their boundaries: which workspace a ref id names, whether its
;; page can load, the tab order's refresh and the selection's re-application
;; are the observations.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defun agent-repl-test-startup--ref (id)
  "Return a decoded WorkspaceRef for ID."
  (list :id id :dir (concat "/w/" id)))

(defun agent-repl-test-startup--event (arm value)
  "Return a decoded startup event with ARM and VALUE."
  (list :at-ms 1 :event (list :arm arm :value value)))

(defun agent-repl-test-startup--opening (n)
  "The run's opening event for N workspaces."
  (agent-repl-test-startup--event :opening (list :workspaces n)))

(defun agent-repl-test-startup--go-ahead (id)
  "The go-ahead event for workspace ID."
  (agent-repl-test-startup--event :workspace-open (list :workspace (agent-repl-test-startup--ref id))))

(defun agent-repl-test-startup--step (id arm &optional value)
  "A workspace step event for ID: ARM with VALUE."
  (agent-repl-test-startup--event
   :workspace-step (list :workspace (agent-repl-test-startup--ref id)
                         :step (list :arm arm :value value))))

(defun agent-repl-test-startup--finished (ready total &rest failed)
  "The run's finished event: READY of TOTAL, FAILED workspace names."
  (agent-repl-test-startup--event
   :finished (list :ready ready :total total
                   :failed (mapcar (lambda (name)
                                     (list :workspace (agent-repl-test-startup--ref name) :name name))
                                   failed))))

;;;; ---- Harness ----

(defvar agent-repl-test-startup--lines nil "The startup lines said, in order.")
(defvar agent-repl-test-startup--refreshes 0 "How often the tab order was re-derived.")
(defvar agent-repl-test-startup--applied 0 "How often the selection was re-applied.")
(defvar agent-repl-test-startup--known nil "Ref ids the registry holds a workspace for.")
(defvar agent-repl-test-startup--no-page nil "Workspaces that can have no page.")

(defmacro agent-repl-test-startup--with-run (&rest body)
  "Run BODY with a fresh startup in its process-start state, boundaries stubbed.
A ref id names the workspace of the same name once it is in
`agent-repl-test-startup--known'; every workspace can have a page unless it
is in `agent-repl-test-startup--no-page'."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-startup--phase 'expected)
           (agent-repl-test-startup--lines nil)
           (agent-repl-test-startup--refreshes 0)
           (agent-repl-test-startup--applied 0)
           (agent-repl-test-startup--known nil)
           (agent-repl-test-startup--no-page nil))
       (cl-letf (((symbol-function 'agent-repl--phase-echo)
                  (lambda (_ws fmt &rest args)
                    (setq agent-repl-test-startup--lines
                          (append agent-repl-test-startup--lines
                                  (list (apply #'format fmt args))))))
                 ((symbol-function 'agent-repl--ws-by-ref-id)
                  (lambda (id) (and (member id agent-repl-test-startup--known) id)))
                 ((symbol-function 'agent-repl--frontend-precreate-refusal)
                  (lambda (ws) (and (member ws agent-repl-test-startup--no-page) :no-xwidget)))
                 ((symbol-function 'agent-repl-roster-refresh-order)
                  (lambda () (cl-incf agent-repl-test-startup--refreshes)))
                 ((symbol-function 'agent-repl-roster-apply-current)
                  (lambda () (cl-incf agent-repl-test-startup--applied) nil)))
         ,@body))))

(defun agent-repl-test-startup--opened ()
  "The ref ids whose tab the startup opened, sorted."
  (let (out)
    (maphash (lambda (k _v) (push k out)) agent-repl-startup--released)
    (sort out #'string<)))

;;;; ---- Holding ----

(ert-deftest agent-repl-test-startup-holds-every-tab-from-process-start ()
  "Before the run says anything, every tab is held: a roster push that
reaches Emacs first cannot open a tab ahead of its go-ahead."
  (agent-repl-test-startup--with-run
    ;; Act / Assert
    (should (agent-repl-startup-holds-p "a"))))

(ert-deftest agent-repl-test-startup-holds-nothing-once-done ()
  "Once the startup is over, no tab is held."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-startup--phase 'done)
    ;; Act / Assert
    (should-not (agent-repl-startup-holds-p "a"))))

;;;; ---- Opening in order ----

(ert-deftest agent-repl-test-startup-opens-a-tab-once-its-go-ahead-and-page-are-in ()
  "Go-ahead and page both in: the tab opens and says it is ready."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup--page-drawn "a")
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Assert
    (should (equal (agent-repl-test-startup--opened) '("a")))
    (should (member "a: ready." agent-repl-test-startup--lines))
    (should (= 1 agent-repl-test-startup--refreshes))))

(ert-deftest agent-repl-test-startup-a-go-ahead-waits-for-its-page ()
  "A go-ahead whose page has not drawn opens nothing and says it is loading."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Assert
    (should (null (agent-repl-test-startup--opened)))
    (should (member "a: loading the page…" agent-repl-test-startup--lines))))

(ert-deftest agent-repl-test-startup-the-page-loading-opens-the-waiting-tab ()
  "The page drawing after the go-ahead is what opens the tab."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Act
    (agent-repl-startup--page-drawn "a")
    ;; Assert
    (should (equal (agent-repl-test-startup--opened) '("a")))))

(ert-deftest agent-repl-test-startup-a-page-without-its-go-ahead-opens-nothing ()
  "A page that drew before its go-ahead does not open the tab on its own."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    ;; Act
    (agent-repl-startup--page-drawn "a")
    ;; Assert
    (should (null (agent-repl-test-startup--opened)))))

(ert-deftest agent-repl-test-startup-tab-two-waits-for-tab-one-to-draw ()
  "Tab 2 is ready (go-ahead and page) but tab 1's page has not drawn: tab 2
waits, and opens right after tab 1 does."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("one" "two"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "one"))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "two"))
    (agent-repl-startup--page-drawn "two")
    (should (null (agent-repl-test-startup--opened)))
    ;; Act
    (agent-repl-startup--page-drawn "one")
    ;; Assert
    (should (equal (agent-repl-test-startup--opened) '("one" "two")))
    (should (equal (seq-filter (lambda (l) (string-suffix-p ": ready." l))
                               agent-repl-test-startup--lines)
                   '("one: ready." "two: ready.")))))

(ert-deftest agent-repl-test-startup-a-go-ahead-before-the-roster-waits-for-it ()
  "A go-ahead for a workspace the roster has not delivered waits for the push."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    (agent-repl-startup--page-drawn "a")
    (should (null (agent-repl-test-startup--opened)))
    ;; Act
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup--on-roster-update)
    ;; Assert
    (should (equal (agent-repl-test-startup--opened) '("a")))))

(ert-deftest agent-repl-test-startup-a-workspace-with-no-page-needs-none ()
  "An Emacs with no webview support opens a tab on its go-ahead alone."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a")
          agent-repl-test-startup--no-page '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Assert
    (should (equal (agent-repl-test-startup--opened) '("a")))))

(ert-deftest agent-repl-test-startup-an-opened-tab-re-applies-the-selection ()
  "The roster's current may name the tab that just opened."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a")
          agent-repl-test-startup--no-page '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Assert
    (should (= 1 agent-repl-test-startup--applied))))

;;;; ---- Finishing ----

(ert-deftest agent-repl-test-startup-finishes-once-every-go-ahead-opened ()
  "The finish says all are ready and ends the hold."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a")
          agent-repl-test-startup--no-page '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--finished 1 1))
    ;; Assert
    (should (eq agent-repl-startup--phase 'done))
    (should (equal (car (last agent-repl-test-startup--lines)) "all 1 workspaces ready."))))

(ert-deftest agent-repl-test-startup-a-finish-waits-for-the-last-page ()
  "A finish that arrives while a page still loads is said after its tab opens."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    (agent-repl-startup-handle (agent-repl-test-startup--finished 1 1))
    (should (eq agent-repl-startup--phase 'running))
    ;; Act
    (agent-repl-startup--page-drawn "a")
    ;; Assert
    (should (eq agent-repl-startup--phase 'done))
    (should (equal (last agent-repl-test-startup--lines 2)
                   '("a: ready." "all 1 workspaces ready.")))))

(ert-deftest agent-repl-test-startup-a-finish-names-the-failed-workspaces ()
  "A failed workspace still opened; the last line names it."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a" "b")
          agent-repl-test-startup--no-page '("a" "b"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "b"))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--finished 1 2 "b"))
    ;; Assert
    (should (equal (agent-repl-test-startup--opened) '("a" "b")))
    (should (equal (car (last agent-repl-test-startup--lines))
                   "1 of 2 workspaces ready; b failed to start."))))

(ert-deftest agent-repl-test-startup-a-stream-that-ends-releases-every-tab ()
  "Events are never replayed: a stream ending mid-startup ends the hold."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    ;; Act
    (agent-repl-startup--on-link-down)
    ;; Assert
    (should-not (agent-repl-startup-holds-p "a"))
    (should (= 1 agent-repl-test-startup--refreshes))))

(ert-deftest agent-repl-test-startup-a-link-down-after-the-startup-changes-nothing ()
  "A later link loss is not a startup's end."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-startup--phase 'done)
    ;; Act
    (agent-repl-startup--on-link-down)
    ;; Assert
    (should (= 0 agent-repl-test-startup--refreshes))))

(ert-deftest agent-repl-test-startup-a-promotion-releases-every-tab ()
  "A handover's promotion ends the startup; its handler takes the hook's two arguments."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    ;; Act
    (agent-repl-startup--on-link-promote 'old-conn 'new-conn)
    ;; Assert
    (should-not (agent-repl-startup-holds-p "a"))
    (should (= 1 agent-repl-test-startup--refreshes))))

(ert-deftest agent-repl-test-startup-the-promote-hook-runs-the-two-argument-handler ()
  "The promote hook carries the handler that accepts OLD and NEW."
  (should (memq #'agent-repl-startup--on-link-promote agent-repl-link-promote-functions))
  (should-not (memq #'agent-repl-startup--on-link-down agent-repl-link-promote-functions)))

;;;; ---- Lines ----

(ert-deftest agent-repl-test-startup-says-each-step-as-the-design-lists-it ()
  "Every daemon step is one line, worded exactly as the design record."
  (dolist (case '((:starting-session nil "a: starting session…")
                  (:waking nil "a: waking from sleep…")
                  (:resuming nil "a: resuming the conversation…")
                  (:vendor-retrying (:attempt 3) "a: Claude did not start, retrying (attempt 3)…")
                  (:vendor-rejected (:cause "bad key") "a: Claude refused to start: bad key.")
                  (:vendor-failed nil "a: Claude failed to start after 10 minutes.")
                  (:cold-gate nil "a: needs your answer to resume (large context).")
                  (:offline nil "a: offline, waiting for the network…")
                  (:failed (:reason "spawn refused") "a: session failed to start: spawn refused.")))
    (agent-repl-test-startup--with-run
      ;; Arrange
      (setq agent-repl-test-startup--known '("a"))
      ;; Act
      (agent-repl-startup-handle (agent-repl-test-startup--step "a" (nth 0 case) (nth 1 case)))
      ;; Assert
      (should (equal agent-repl-test-startup--lines (list (nth 2 case)))))))

(ert-deftest agent-repl-test-startup-says-whom-a-ready-workspace-waits-on ()
  "The waiting step names the workspace ahead."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("one" "three"))
    ;; Act
    (agent-repl-startup-handle
     (agent-repl-test-startup--step "three" :waiting-for (list :ahead (agent-repl-test-startup--ref "one"))))
    ;; Assert
    (should (equal agent-repl-test-startup--lines '("three: ready, waiting for one to open first…")))))

(ert-deftest agent-repl-test-startup-says-how-many-it-opens ()
  "The opening says how many workspaces are being brought up."
  (agent-repl-test-startup--with-run
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--opening 4))
    ;; Assert
    (should (equal agent-repl-test-startup--lines '("opening 4 workspaces…")))
    (should (eq agent-repl-startup--phase 'running))))

(ert-deftest agent-repl-test-startup-names-a-workspace-the-roster-has-not-reached-by-its-directory ()
  "Before the registry holds it, a workspace is named by its directory."
  (agent-repl-test-startup--with-run
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--step "far" :waking))
    ;; Assert
    (should (equal agent-repl-test-startup--lines '("far: waking from sleep…")))))

(ert-deftest agent-repl-test-startup-an-unknown-event-is-an-error ()
  "An event arm this file has no case for is recorded at ERROR."
  (agent-repl-test-startup--with-run
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-startup-handle (agent-repl-test-startup--event :invented nil)))
      ;; Assert
      (should (string-match-p "elisp.startup.unknown-event" (car errors))))))

;;;; ---- The page drew its conversation ----

(defmacro agent-repl-test-startup--with-page (reply &rest body)
  "Run BODY with every page read answering REPLY (a string, or nil for no page).
`reads' collects the scripts asked; `timers' the re-asks scheduled."
  (declare (indent 1))
  `(let ((reads nil) (timers nil))
     (cl-letf (((symbol-function 'agent-repl--ws-get)
                (lambda (_ws key) (and (eq key :frontend-buffer) (current-buffer))))
               ((symbol-function 'agent-repl--frontend-webview-read-script)
                (lambda (_buf script callback)
                  (push script reads)
                  (when ,reply (funcall callback ,reply))
                  (and ,reply t)))
               ((symbol-function 'run-at-time)
                (lambda (_secs _repeat fn &rest args) (push (cons fn args) timers) nil)))
       ,@body)))

(ert-deftest agent-repl-test-startup-a-loaded-page-is-asked-whether-it-drew ()
  "The HTML loading is not the conversation drawn: the page is asked."
  (agent-repl-test-startup--with-run
    (agent-repl-test-startup--with-page nil
      ;; Act
      (agent-repl-startup-note-page-loaded "a")
      ;; Assert
      (should (string-match-p "data-conversation-drawn" (car reads))))))

(ert-deftest agent-repl-test-startup-a-page-not-yet-drawn-is-asked-again ()
  "A page whose conversation has not drawn opens nothing and is asked again."
  (agent-repl-test-startup--with-run
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    (agent-repl-test-startup--with-page "{\"ws\":\"a\",\"drawn\":false}"
      ;; Act
      (agent-repl-startup-note-page-loaded "a")
      ;; Assert
      (should (null (agent-repl-test-startup--opened)))
      (should (equal timers '((agent-repl-startup--probe "a")))))))

(ert-deftest agent-repl-test-startup-a-drawn-page-opens-its-due-tab ()
  "The page answering drawn is what opens a tab whose go-ahead is in."
  (agent-repl-test-startup--with-run
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    (agent-repl-test-startup--with-page "{\"ws\":\"a\",\"drawn\":true}"
      ;; Act
      (agent-repl-startup-note-page-loaded "a")
      ;; Assert
      (should (equal (agent-repl-test-startup--opened) '("a"))))))

(ert-deftest agent-repl-test-startup-no-page-is-asked-once-the-startup-is-over ()
  "Outside the startup a load asks the page nothing."
  (agent-repl-test-startup--with-run
    (setq agent-repl-startup--phase 'done)
    (agent-repl-test-startup--with-page nil
      ;; Act
      (agent-repl-startup-note-page-loaded "a")
      ;; Assert
      (should (null reads)))))

(ert-deftest agent-repl-test-startup-an-unreadable-reply-is-an-error ()
  "A reply that is not the probe's JSON is recorded at ERROR."
  (agent-repl-test-startup--with-run
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-startup--on-probe "not json"))
      ;; Assert
      (should (string-match-p "elisp.startup.probe-unreadable" (car errors))))))

;;;; ---- Pre-creation ----

(ert-deftest agent-repl-test-startup-pre-creates-the-input-buffer-while-held ()
  "A held workspace's input buffer exists before its tab opens."
  (agent-repl-test-startup--with-run
    (let ((ensured nil))
      (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                 (lambda (ws) (push ws ensured))))
        ;; Act
        (agent-repl-startup-precreate "a"))
      ;; Assert
      (should (equal ensured '("a"))))))

(ert-deftest agent-repl-test-startup-pre-creates-nothing-once-done ()
  "Outside the startup nothing is pre-created here."
  (agent-repl-test-startup--with-run
    (setq agent-repl-startup--phase 'done)
    (let ((ensured nil))
      (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                 (lambda (ws) (push ws ensured))))
        ;; Act
        (agent-repl-startup-precreate "a"))
      ;; Assert
      (should (null ensured)))))

(provide 'test-startup)
;;; test-startup.el ends here
