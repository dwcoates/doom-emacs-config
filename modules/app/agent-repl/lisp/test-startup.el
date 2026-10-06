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
(defvar agent-repl-test-startup--switches nil "Every switch the startup asked of the daemon, as (WS . TRIGGER).")
(defvar agent-repl-test-startup--hidden nil "Workspaces whose repository is held collapsed: opened, never drawn.")
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
           (agent-repl-test-startup--no-page nil)
           (agent-repl-test-startup--switches nil)
           (agent-repl-test-startup--hidden nil)
           (agent-repl-startup--selected nil))
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
                  (lambda () (cl-incf agent-repl-test-startup--applied) nil))
                 ((symbol-function 'agent-repl-roster-drawn-tab-order)
                  (lambda () (cl-remove-if (lambda (n) (member n agent-repl-test-startup--hidden))
                                           agent-repl-test-startup--known)))
                 ((symbol-function 'agent-repl-host-request-switch)
                  (lambda (ws trigger)
                    (setq agent-repl-test-startup--switches
                          (append agent-repl-test-startup--switches (list (cons ws trigger))))
                    t)))
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

(ert-deftest agent-repl-test-startup-selects-the-first-tab-it-opens ()
  "The first tab opened is asked of the daemon as the selection at once."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a" "b"))
    (agent-repl-startup--page-drawn "a")
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Assert
    (should (equal agent-repl-test-startup--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-startup-keeps-the-first-tab-selected-as-the-rest-open ()
  "A later tab opening asks for no selection: the first stays selected."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a" "b"))
    (agent-repl-startup--page-drawn "a")
    (agent-repl-startup--page-drawn "b")
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "b"))
    ;; Assert
    (should (equal agent-repl-test-startup--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-startup-never-selects-a-tab-it-does-not-draw ()
  "A first tab of a collapsed repository is opened, but the first DRAWN one is selected."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a" "b")
          agent-repl-test-startup--hidden '("a"))
    (agent-repl-startup--page-drawn "a")
    (agent-repl-startup--page-drawn "b")
    (agent-repl-startup-handle (agent-repl-test-startup--opening 2))
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "b"))
    ;; Assert
    (should (equal agent-repl-test-startup--switches '(("b" . startup))))))

(ert-deftest agent-repl-test-startup-is-choosing-until-it-selects ()
  "Running with no selection is choosing; a drawn tab's opening ends the choice."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (agent-repl-startup--page-drawn "a")
    (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
    (should (agent-repl-startup-choosing-p))
    ;; Act
    (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
    ;; Assert
    (should-not (agent-repl-startup-choosing-p))))

(ert-deftest agent-repl-test-startup-opens-a-tab-while-page-creation-is-parked ()
  "A visible but unfocused Emacs parks page creation; the tab opens anyway."
  (agent-repl-test-startup--with-run
    ;; Arrange
    (setq agent-repl-test-startup--known '("a"))
    (let ((agent-repl--webview-precreate-parked t))
      (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
      ;; Act
      (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
      ;; Assert
      (should (equal (agent-repl-test-startup--opened) '("a"))))))

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

;;;; ---- Doom's one-time setup at idle ----

(defvar agent-repl-test-startup--file-hook nil "Stands in for `doom-first-file-hook'.")
(defvar agent-repl-test-startup--buffer-hook nil "Stands in for `doom-first-buffer-hook'.")
(defvar agent-repl-test-startup--hook-runs nil "Every hook the stubbed `doom-run-hooks' ran, in order.")
(defvar agent-repl-test-startup--armed nil "Every timer armed, as (KIND SECS FUNCTION).")

(defun agent-repl-test-startup--first-hooks-timer-p (fn)
  "Return non-nil when FN is one of the idle run's own timer functions."
  (memq fn '(agent-repl-startup--first-hooks-run
             agent-repl-startup--first-hooks-arm-or-run)))

(defmacro agent-repl-test-startup--with-first-hooks (idle &rest body)
  "Run BODY with stand-in first hooks, a stubbed Doom runner, and timers recorded.
IDLE is what `current-idle-time' answers, in seconds, or nil for not idle.
The stand-in hooks start empty; nothing is really scheduled, and only the
idle run's own timers are recorded (Emacs arms others, such as undo's)."
  (declare (indent 1))
  `(let ((agent-repl-startup--first-hooks '(agent-repl-test-startup--file-hook
                                            agent-repl-test-startup--buffer-hook))
         (agent-repl-startup--first-hooks-timer nil)
         (agent-repl-test-startup--file-hook nil)
         (agent-repl-test-startup--buffer-hook nil)
         (agent-repl-test-startup--hook-runs nil)
         (agent-repl-test-startup--armed nil))
     (cl-letf (((symbol-function 'doom-run-hooks)
                (lambda (&rest hooks)
                  (dolist (hook hooks)
                    (setq agent-repl-test-startup--hook-runs
                          (append agent-repl-test-startup--hook-runs (list hook)))
                    (run-hooks hook))))
               ((symbol-function 'current-idle-time)
                (lambda () (and ,idle (seconds-to-time ,idle))))
               ((symbol-function 'run-at-time)
                (lambda (secs _repeat fn &rest _)
                  (when (agent-repl-test-startup--first-hooks-timer-p fn)
                    (push (list 'clock secs fn) agent-repl-test-startup--armed)
                    'clock-timer)))
               ((symbol-function 'run-with-idle-timer)
                (lambda (secs _repeat fn &rest _)
                  (when (agent-repl-test-startup--first-hooks-timer-p fn)
                    (push (list 'idle secs fn) agent-repl-test-startup--armed)
                    'idle-timer))))
       ,@body)))

(ert-deftest agent-repl-test-startup-first-hooks-run-a-pending-hook-once ()
  "A pending hook is run through Doom's runner exactly once."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (let ((calls 0))
      (setq agent-repl-test-startup--buffer-hook (list (lambda () (cl-incf calls))))
      ;; Act
      (agent-repl-startup--first-hooks-run)
      (agent-repl-startup--first-hooks-run)
      ;; Assert
      (should (= calls 1)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-clears-the-hook ()
  "A hook the idle run ran is set to nil, as Doom clears it."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-run)
    ;; Assert
    (should (null agent-repl-test-startup--buffer-hook))))

(ert-deftest agent-repl-test-startup-first-hooks-run-leaves-a-run-hook-alone ()
  "A hook already run (nil) is never handed to Doom's runner."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-run)
    ;; Assert
    (should (equal agent-repl-test-startup--hook-runs
                   '(agent-repl-test-startup--buffer-hook)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-file-before-buffer ()
  "Both pending hooks run file first, the order Doom runs them at init."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--file-hook (list #'ignore)
          agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-run)
    ;; Assert
    (should (equal agent-repl-test-startup--hook-runs
                   '(agent-repl-test-startup--file-hook
                     agent-repl-test-startup--buffer-hook)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-is-a-no-op-after-a-switch ()
  "A switch that ran the hook first (Doom cleared it) leaves the timer nothing to do."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange: Doom's own switch path runs the hook and clears it.
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    (agent-repl-startup--first-hooks-arm)
    (setq agent-repl-test-startup--buffer-hook nil)
    ;; Act: the armed timer fires.
    (funcall (nth 2 (car agent-repl-test-startup--armed)))
    ;; Assert
    (should (null agent-repl-test-startup--hook-runs))))

(ert-deftest agent-repl-test-startup-first-hooks-run-clears-a-hook-that-signals ()
  "A hook that signals is still cleared, so no half-run hook is left behind."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list (lambda () (error "Boom"))))
    (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
      ;; Act
      (ignore-errors (agent-repl-startup--first-hooks-run)))
    ;; Assert
    (should (null agent-repl-test-startup--buffer-hook))))

(ert-deftest agent-repl-test-startup-first-hooks-run-resignals-a-hook-failure ()
  "A hook's failure is re-signalled, never swallowed."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list (lambda () (error "Boom"))))
    (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
      ;; Act / Assert
      (should-error (agent-repl-startup--first-hooks-run)))))

(ert-deftest agent-repl-test-startup-first-hooks-run-records-a-hook-failure ()
  "A hook's failure is recorded at the error level, naming the hook."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list (lambda () (error "Boom"))))
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (ignore-errors (agent-repl-startup--first-hooks-run)))
      ;; Assert
      (should (string-match-p "hook=agent-repl-test-startup--buffer-hook failed"
                              (car errors))))))

(ert-deftest agent-repl-test-startup-first-hooks-run-logs-one-info-record ()
  "A run records one INFO line naming the hooks it ran and the milliseconds."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    (let ((infos nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Act
        (agent-repl-startup--first-hooks-run))
      ;; Assert
      (should (= (length infos) 1))
      (should (string-match-p
               "\\`elisp.startup.first-hooks-idle ran=agent-repl-test-startup--buffer-hook ms=[0-9]+\\'"
               (car infos))))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-uses-an-idle-timer-when-busy ()
  "Armed while Emacs is not idle, the run waits on an idle timer for the delay."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-arm)
    ;; Assert
    (should (equal agent-repl-test-startup--armed
                   (list (list 'idle agent-repl-startup--first-hooks-idle-delay
                               #'agent-repl-startup--first-hooks-run))))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-uses-the-clock-deep-into-idle ()
  "Armed after Emacs sat idle past the delay, the run waits the delay on the clock.
An idle timer armed then would wait for the stretch after the next key."
  (agent-repl-test-startup--with-first-hooks 30
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-arm)
    ;; Assert
    (should (equal agent-repl-test-startup--armed
                   (list (list 'clock agent-repl-startup--first-hooks-idle-delay
                               #'agent-repl-startup--first-hooks-arm-or-run))))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-arms-nothing-with-nothing-pending ()
  "With every hook already run, nothing is armed."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Act
    (agent-repl-startup--first-hooks-arm)
    ;; Assert
    (should (null agent-repl-test-startup--armed))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-arms-once ()
  "A second arm while one timer is armed arms nothing more."
  (agent-repl-test-startup--with-first-hooks nil
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    (agent-repl-startup--first-hooks-arm)
    ;; Act
    (agent-repl-startup--first-hooks-arm)
    ;; Assert
    (should (= (length agent-repl-test-startup--armed) 1))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-or-run-runs-when-idle-long-enough ()
  "The clock timer, firing while Emacs still sits idle past the delay, runs the hooks."
  (agent-repl-test-startup--with-first-hooks 30
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-arm-or-run)
    ;; Assert
    (should (equal agent-repl-test-startup--hook-runs
                   '(agent-repl-test-startup--buffer-hook)))))

(ert-deftest agent-repl-test-startup-first-hooks-arm-or-run-waits-after-input ()
  "The clock timer, firing after input broke the idle stretch, arms an idle timer."
  (agent-repl-test-startup--with-first-hooks 0.1
    ;; Arrange
    (setq agent-repl-test-startup--buffer-hook (list #'ignore))
    ;; Act
    (agent-repl-startup--first-hooks-arm-or-run)
    ;; Assert
    (should (null agent-repl-test-startup--hook-runs))
    (should (equal (car (car agent-repl-test-startup--armed)) 'idle))))

(ert-deftest agent-repl-test-startup-first-hooks-not-armed-while-the-startup-runs ()
  "No idle run is armed before the startup completes."
  (agent-repl-test-startup--with-run
    (agent-repl-test-startup--with-first-hooks nil
      ;; Arrange
      (setq agent-repl-test-startup--buffer-hook (list #'ignore)
            agent-repl-test-startup--known '("a")
            agent-repl-test-startup--no-page '("a"))
      ;; Act
      (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
      (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
      ;; Assert
      (should (null agent-repl-test-startup--armed)))))

(ert-deftest agent-repl-test-startup-first-hooks-armed-once-the-startup-completes ()
  "The startup's finish arms the idle run."
  (agent-repl-test-startup--with-run
    (agent-repl-test-startup--with-first-hooks nil
      ;; Arrange
      (setq agent-repl-test-startup--buffer-hook (list #'ignore)
            agent-repl-test-startup--known '("a")
            agent-repl-test-startup--no-page '("a"))
      (agent-repl-startup-handle (agent-repl-test-startup--opening 1))
      (agent-repl-startup-handle (agent-repl-test-startup--go-ahead "a"))
      ;; Act
      (agent-repl-startup-handle (agent-repl-test-startup--finished 1 1))
      ;; Assert
      (should (= (length agent-repl-test-startup--armed) 1)))))

(provide 'test-startup)
;;; test-startup.el ends here
