;;; test-sidebar-view.el --- ERT tests for sidebar-view.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The sidebar's own web view: its creation, the calls Emacs makes on its
;; page and their answers, the desired state replayed on every load, its
;; reload beside the host's, and its full-height side window.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-sidebar-view.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Harness ----

(defvar agent-repl-test-sv--calls nil
  "Scripts the sidebar was sent, as (SCRIPT . CALLBACK), newest first.")

(defvar agent-repl-test-sv--navigated nil
  "The URLs the sidebar was navigated to, newest first.")

(defvar agent-repl-test-sv--xw 'sidebar-xwidget
  "What the stubbed sidebar lookup answers: the xwidget, or nil.")

(defmacro agent-repl-test-sv--with-sidebar (&rest body)
  "Run BODY with a sidebar present, its scripts and navigations recorded.
Binds `records' to every central log record, as (LEVEL . TEXT), newest first."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-sv--calls nil)
           (agent-repl-test-sv--navigated nil)
           (agent-repl-test-sv--xw 'sidebar-xwidget)
           (agent-repl--sidebar-view-url "http://host.localhost:1/?sidebar=1")
           (agent-repl--webhost-log-level nil)
           (records nil))
       (cl-letf (((symbol-function 'agent-repl-sidebar-view-xwidget)
                  (lambda () agent-repl-test-sv--xw))
                 ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
                  (lambda (_xw script callback) (push (cons script callback) agent-repl-test-sv--calls)))
                 ((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                  (lambda (_xw url) (push url agent-repl-test-sv--navigated)))
                 ((symbol-function 'agent-repl--info)
                  (lambda (_ws fmt &rest args) (push (cons 'info (apply #'format fmt args)) records)))
                 ((symbol-function 'agent-repl--log)
                  (lambda (_ws fmt &rest args) (push (cons 'log (apply #'format fmt args)) records)))
                 ((symbol-function 'agent-repl--warn)
                  (lambda (_ws fmt &rest args) (push (cons 'warn (apply #'format fmt args)) records)))
                 ((symbol-function 'agent-repl--error)
                  (lambda (_ws fmt &rest args) (push (cons 'error (apply #'format fmt args)) records))))
         ,@body))))

(defun agent-repl-test-sv--conn (address)
  "A REAL connection at ADDRESS (struct accessors are inlined at load time)."
  (agent-repl-connect-connection-create :address address))

(defun agent-repl-test-sv--sent (method)
  "The scripts sent calling METHOD, oldest first."
  (cl-remove-if-not (lambda (script) (string-search (format "h.%s(" method) script))
                    (reverse (mapcar #'car agent-repl-test-sv--calls))))

(defun agent-repl-test-sv--recorded (level fragment records)
  "Non-nil when RECORDS hold a LEVEL record containing FRAGMENT."
  (cl-some (lambda (r) (and (eq (car r) level) (string-search fragment (cdr r)))) records))

;;;; ---- The URL and the script ----

(ert-deftest agent-repl-test-sv-the-url-is-the-sidebar-page-on-the-host-origin ()
  "The sidebar loads the webapp's `?sidebar=1' page on the host's origin."
  (should (equal (agent-repl-sidebar-view-url (agent-repl-test-sv--conn "127.0.0.1:4321"))
                 "http://host.localhost:4321/?sidebar=1")))

(ert-deftest agent-repl-test-sv-a-script-calls-the-sidebar-page ()
  "A sidebar call is the shared view script on the sidebar's own globals."
  (should (equal (agent-repl--sidebar-view-script "markShown" "id-a")
                 (agent-repl--webview-script "agentReplSidebar" "__agentReplSidebarPending" "markShown" '("id-a")))))

;;;; ---- Calls ----

(ert-deftest agent-repl-test-sv-a-call-is-sent-to-the-sidebar ()
  "A call with a sidebar present is sent, answered by the sidebar's handler."
  (agent-repl-test-sv--with-sidebar
    (should (agent-repl--sidebar-view-call "markShown" "id-a"))
    (should (eq (cdar agent-repl-test-sv--calls) 'agent-repl--sidebar-view-answered))))

(ert-deftest agent-repl-test-sv-a-call-without-a-sidebar-sends-nothing ()
  "A call with no sidebar sends nothing, and says so in the log."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl-test-sv--xw nil)
    (should-not (agent-repl--sidebar-view-call "markShown" "id-a"))
    (should-not agent-repl-test-sv--calls)
    (should (agent-repl-test-sv--recorded 'log "call-skipped method=markShown reason=no-sidebar" records))))

(ert-deftest agent-repl-test-sv-marking-a-row-shown-calls-the-page ()
  "Marking a workspace's row shown calls the page's markShown with its id."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-sidebar-view-mark-shown "id-a")
    (should (equal (length (agent-repl-test-sv--sent "markShown")) 1))
    (should (equal agent-repl--sidebar-view-shown "id-a"))))

(ert-deftest agent-repl-test-sv-marking-the-row-already-shown-sends-nothing ()
  "Marking the row already shown sends nothing."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-sidebar-view-mark-shown "id-a")
    (agent-repl-sidebar-view-mark-shown "id-a")
    (should (equal (length (agent-repl-test-sv--sent "markShown")) 1))))

(ert-deftest agent-repl-test-sv-following-a-daemon-points-the-page-at-it ()
  "Following a daemon calls the page's setDaemon and keeps the origin."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-sidebar-view-follow-daemon "http://host.localhost:2")
    (should (equal (length (agent-repl-test-sv--sent "setDaemon")) 1))
    (should (equal agent-repl--sidebar-view-daemon-origin "http://host.localhost:2"))))

(ert-deftest agent-repl-test-sv-following-the-same-daemon-sends-nothing ()
  "Following the daemon already followed sends nothing."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-sidebar-view-follow-daemon "http://host.localhost:2")
    (agent-repl-sidebar-view-follow-daemon "http://host.localhost:2")
    (should (equal (length (agent-repl-test-sv--sent "setDaemon")) 1))))

(ert-deftest agent-repl-test-sv-a-log-level-reaches-the-page ()
  "A log level window is handed to the page with its end."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-sidebar-view-set-log-level 'debug 1700000060000)
    (should (string-search "h.setLogLevel(\"debug\",1700000060000)" (car (agent-repl-test-sv--sent "setLogLevel"))))))

(ert-deftest agent-repl-test-sv-a-focus-move-reaches-the-page ()
  "Emacs moving its focus is told to the page."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-sidebar-view-focus-moved)
    (should (equal (length (agent-repl-test-sv--sent "focusMoved")) 1))))

(ert-deftest agent-repl-test-sv-a-focus-move-without-a-sidebar-sends-nothing ()
  "With no sidebar there is no page to tell of a focus move."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl-test-sv--xw nil)
    (agent-repl-sidebar-view-focus-moved)
    (should-not agent-repl-test-sv--calls)))

;;;; ---- Answers ----

(ert-deftest agent-repl-test-sv-an-answer-is-recorded ()
  "Every answer is recorded with its call and outcome."
  (agent-repl-test-sv--with-sidebar
    (agent-repl--sidebar-view-answered "{\"op\":\"markShown\",\"id\":\"id-a\",\"outcome\":\"marked\",\"notes\":[]}")
    (should (agent-repl-test-sv--recorded 'log "answer op=markShown id=id-a outcome=marked" records))))

(ert-deftest agent-repl-test-sv-an-answer-s-warn-note-is-recorded-at-warn ()
  "A note the page attached at warn is recorded at warn."
  (agent-repl-test-sv--with-sidebar
    (agent-repl--sidebar-view-answered
     "{\"op\":\"markShown\",\"id\":null,\"outcome\":\"refused\",\"notes\":[{\"level\":\"warn\",\"text\":\"no id\"}]}")
    (should (agent-repl-test-sv--recorded 'warn "note op=markShown note=no id" records))))

(ert-deftest agent-repl-test-sv-an-unreadable-answer-is-recorded-at-error ()
  "An answer that is not JSON is recorded at error, never dropped."
  (agent-repl-test-sv--with-sidebar
    (agent-repl--sidebar-view-answered "not json")
    (should (agent-repl-test-sv--recorded 'error "answer-unreadable raw=\"not json\"" records))))

(ert-deftest agent-repl-test-sv-a-page-that-lacks-a-call-is-reloaded-on-its-url ()
  "A page from an older build than this elisp is reloaded once on its own URL."
  (agent-repl-test-sv--with-sidebar
    (agent-repl--sidebar-view-answered "{\"op\":\"focusMoved\",\"id\":null,\"outcome\":\"unknown-call\",\"notes\":[]}")
    (should (equal agent-repl-test-sv--navigated '("http://host.localhost:1/?sidebar=1")))))

(ert-deftest agent-repl-test-sv-a-page-that-still-lacks-a-call-is-an-error ()
  "A page still lacking the call after its reload is an error no reload heals."
  (agent-repl-test-sv--with-sidebar
    (dotimes (_ 2)
      (agent-repl--sidebar-view-answered "{\"op\":\"focusMoved\",\"id\":null,\"outcome\":\"unknown-call\",\"notes\":[]}"))
    (should (equal (length agent-repl-test-sv--navigated) 1))
    (should (agent-repl-test-sv--recorded 'error "predates-call op=focusMoved action=none reason=served-webapp-predates-elisp" records))))

;;;; ---- Loads ----

(defun agent-repl-test-sv--load (status uri)
  "Run the sidebar's load handler for a load at STATUS showing URI."
  (cl-letf (((symbol-function 'xwidget-webkit-uri) (lambda (_xw) uri)))
    (let ((last-input-event (list 'xwidget-event 'load-changed 'sidebar-xwidget status)))
      (agent-repl--sidebar-view-on-load 'load-changed))))

(ert-deftest agent-repl-test-sv-a-committed-load-replays-the-shown-row ()
  "A load committed on the sidebar's own page is handed the row shown."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--sidebar-view-shown "id-a")
    (agent-repl-test-sv--load "load-committed" "http://host.localhost:1/?sidebar=1")
    (should (equal (length (agent-repl-test-sv--sent "markShown")) 1))))

(ert-deftest agent-repl-test-sv-a-committed-load-replays-the-daemon-followed ()
  "A load committed after a handover is pointed at the daemon followed."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--sidebar-view-daemon-origin "http://host.localhost:2")
    (agent-repl-test-sv--load "load-committed" "http://host.localhost:1/?sidebar=1")
    (should (equal (length (agent-repl-test-sv--sent "setDaemon")) 1))))

(ert-deftest agent-repl-test-sv-a-committed-load-replays-an-open-log-level-window ()
  "A load committed while a log level window stands is raised to it."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--webhost-log-level (cons 'debug (+ (agent-repl--now-unix-ms) 60000)))
    (agent-repl-test-sv--load "load-committed" "http://host.localhost:1/?sidebar=1")
    (should (equal (length (agent-repl-test-sv--sent "setLogLevel")) 1))))

(ert-deftest agent-repl-test-sv-a-committed-load-replays-no-ended-log-level-window ()
  "A log level window that has ended is not replayed."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--webhost-log-level (cons 'debug 1))
    (agent-repl-test-sv--load "load-committed" "http://host.localhost:1/?sidebar=1")
    (should-not (agent-repl-test-sv--sent "setLogLevel"))))

(ert-deftest agent-repl-test-sv-a-load-stage-before-commit-replays-nothing ()
  "Only the commit replays: one replay per document."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--sidebar-view-shown "id-a")
    (agent-repl-test-sv--load "load-started" "http://host.localhost:1/?sidebar=1")
    (should-not agent-repl-test-sv--calls)))

(ert-deftest agent-repl-test-sv-a-finished-load-reads-the-sidebars-boot ()
  "At the sidebar's load-finished its boot is asked for, answered by symbol."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-test-sv--load "load-finished" "http://host.localhost:1/?sidebar=1")
    (should (equal agent-repl-test-sv--calls
                   (list (cons (agent-repl--webview-boot-script "agentReplSidebar")
                               'agent-repl--sidebar-view-on-boot))))))

(ert-deftest agent-repl-test-sv-a-strayed-finish-reads-no-boot ()
  "A site the sidebar strayed to is not its page, so it has no boot to read."
  (agent-repl-test-sv--with-sidebar
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-navigate-widget) #'ignore))
      (agent-repl-test-sv--load "load-finished" "https://example.com/"))
    (should-not agent-repl-test-sv--calls)))

(ert-deftest agent-repl-test-sv-a-boot-that-threw-is-recorded-at-error-as-the-sidebars ()
  "The sidebar's boot errors are recorded at ERROR, naming the sidebar's view."
  (agent-repl-test-sv--with-sidebar
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--error) (agent-repl-test--record-into errors)))
        (agent-repl--sidebar-view-on-boot "{\"started\":false,\"hooked\":true,\"errors\":[\"error: boom\"]}"))
      (should (equal errors '("elisp.webview.boot-errors view=sidebar started=nil errors=(\"error: boom\")"))))))

(ert-deftest agent-repl-test-sv-the-blank-document-replays-nothing ()
  "WebKit's about:blank ahead of the first navigation is not the page."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--sidebar-view-shown "id-a")
    (agent-repl-test-sv--load "load-committed" "about:blank")
    (should-not agent-repl-test-sv--calls)))

(ert-deftest agent-repl-test-sv-a-sidebar-that-strays-is-taken-back ()
  "A load committed off the sidebar's origin is navigated back to its page."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-test-sv--load "load-committed" "https://example.com/")
    (should (equal agent-repl-test-sv--navigated '("http://host.localhost:1/?sidebar=1")))))

(ert-deftest agent-repl-test-sv-a-sidebar-that-strays-is-recorded-at-error ()
  "A sidebar that strayed is a page defect, recorded at error."
  (agent-repl-test-sv--with-sidebar
    (agent-repl-test-sv--load "load-committed" "https://example.com/")
    (should (agent-repl-test-sv--recorded 'error "navigated-away uri=https://example.com/" records))))

(ert-deftest agent-repl-test-sv-a-sidebar-that-strays-replays-nothing-into-the-site ()
  "Nothing is handed to the site the sidebar strayed to."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--sidebar-view-shown "id-a")
    (agent-repl-test-sv--load "load-committed" "https://example.com/")
    (should-not agent-repl-test-sv--calls)))

(ert-deftest agent-repl-test-sv-navigating-loads-the-page-on-the-daemon-named ()
  "A reload loads the sidebar's page on the daemon named, forgetting the one followed."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl--sidebar-view-daemon-origin "http://host.localhost:2")
    (agent-repl-sidebar-view-navigate (agent-repl-test-sv--conn "127.0.0.1:3"))
    (should (equal agent-repl-test-sv--navigated '("http://host.localhost:3/?sidebar=1")))
    (should-not agent-repl--sidebar-view-daemon-origin)))

(ert-deftest agent-repl-test-sv-navigating-without-a-sidebar-loads-nothing ()
  "With no sidebar a reload has nothing to load, and says so."
  (agent-repl-test-sv--with-sidebar
    (setq agent-repl-test-sv--xw nil)
    (agent-repl-sidebar-view-navigate (agent-repl-test-sv--conn "127.0.0.1:3"))
    (should-not agent-repl-test-sv--navigated)
    (should (agent-repl-test-sv--recorded 'log "reload-skipped reason=no-sidebar" records))))

;;;; ---- Creation and the kill guard ----

(defmacro agent-repl-test-sv--with-created (&rest body)
  "Run BODY with web view creation stubbed to a fresh buffer with an xwidget.
Binds `made' to that buffer; it is killed afterwards."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((made (generate-new-buffer " *test-sidebar-view*")))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer) (lambda (_url) made))
                     ((symbol-function 'agent-repl--frontend-webview-live-widget)
                      (lambda (buf) (and (buffer-live-p buf) 'sidebar-xwidget)))
                     ((symbol-function 'xwidget-get) (lambda (_xw _prop) nil))
                     ((symbol-function 'xwidget-put) (lambda (_xw _prop _value) nil)))
             ,@body)
         (when (buffer-live-p made)
           (with-current-buffer made (setq kill-buffer-query-functions nil))
           (kill-buffer made))))))

(ert-deftest agent-repl-test-sv-ensuring-creates-the-sidebar-at-its-url ()
  "Ensuring a sidebar creates its web view at the sidebar page's URL."
  (agent-repl-test-sv--with-created
    (should (eq (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5")) 'sidebar-xwidget))
    (should (eq (agent-repl-sidebar-view-buffer) made))
    (should (equal agent-repl--sidebar-view-url "http://host.localhost:5/?sidebar=1"))))

(ert-deftest agent-repl-test-sv-ensuring-again-creates-nothing ()
  "A sidebar already standing is answered, never made twice."
  (agent-repl-test-sv--with-created
    (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5"))
    (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
               (lambda (_url) (error "made twice"))))
      (should (eq (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5")) 'sidebar-xwidget)))))

(ert-deftest agent-repl-test-sv-the-sidebar-has-no-mode-line ()
  "The sidebar's buffer draws no mode line, as the feed and composer draw none."
  (agent-repl-test-sv--with-created
    (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5"))
    (should-not (buffer-local-value 'mode-line-format made))))

(ert-deftest agent-repl-test-sv-the-sidebar-buffer-refuses-a-foreign-kill ()
  "Only this module kills the sidebar buffer."
  (agent-repl-test-sv--with-created
    (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5"))
    (cl-letf (((symbol-function 'agent-repl--warn) #'ignore))
      (should-not (with-current-buffer made (agent-repl--sidebar-view-refuse-kill))))))

(ert-deftest agent-repl-test-sv-a-refused-kill-is-recorded ()
  "A refused kill is recorded at warn."
  (agent-repl-test-sv--with-created
    (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5"))
    (let ((warned nil))
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (agent-repl-test--record-into warned)))
        (with-current-buffer made (agent-repl--sidebar-view-refuse-kill)))
      (should (string-search "kill-refused" (car warned))))))

(ert-deftest agent-repl-test-sv-the-sidebar-buffer-allows-its-own-kill ()
  "This module's own kill passes the guard."
  (agent-repl-test-sv--with-created
    (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5"))
    (let ((agent-repl--sidebar-view-killing t))
      (should (with-current-buffer made (agent-repl--sidebar-view-refuse-kill))))))

(ert-deftest agent-repl-test-sv-killing-the-sidebar-forgets-it ()
  "Killing the sidebar kills its buffer and forgets its URL."
  (agent-repl-test-sv--with-created
    (agent-repl-sidebar-view-ensure (agent-repl-test-sv--conn "127.0.0.1:5"))
    (cl-letf (((symbol-function 'agent-repl--frontend-kill-webview) #'kill-buffer))
      (agent-repl-sidebar-view-kill))
    (should-not (agent-repl-sidebar-view-buffer))
    (should-not agent-repl--sidebar-view-url)))

;;;; ---- The window ----

(defmacro agent-repl-test-sv--with-frame (&rest body)
  "Run BODY on the selected frame cut down to one main window, restored after."
  (declare (indent 0))
  `(let ((wconf (current-window-configuration)))
     (unwind-protect
         (progn (delete-other-windows) ,@body)
       (set-window-configuration wconf))))

(ert-deftest agent-repl-test-sv-showing-puts-the-sidebar-at-the-left-edge ()
  "The sidebar shows in a left side window spanning the frame's height."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let ((main (selected-window)))
        (split-window main nil 'below)
        (let ((win (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))))
          (should (eq (window-parameter win 'window-side) 'left))
          (should (= (window-total-height win) (window-total-height (frame-root-window)))))))))

(ert-deftest agent-repl-test-sv-showing-draws-no-mode-line-in-the-window ()
  "The sidebar's window draws no mode line."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let ((win (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))))
        (should (eq (window-parameter win 'mode-line-format) 'none))))))

(ert-deftest agent-repl-test-sv-showing-opens-at-the-configured-width ()
  "The sidebar opens at its fraction of the frame's width."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let ((agent-repl-sidebar-view-width-fraction 0.25))
        (let ((win (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))))
          (should (= (window-total-width win) (round (* 0.25 (frame-width))))))))))

(ert-deftest agent-repl-test-sv-showing-again-keeps-the-window-standing ()
  "Showing a sidebar already on screen answers its window and makes no other."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let* ((conn (agent-repl-test-sv--conn "127.0.0.1:5"))
             (first (agent-repl-sidebar-view-show conn)))
        (should (eq (agent-repl-sidebar-view-show conn) first))
        (should (= (length (get-buffer-window-list made nil t)) 1))))))

(ert-deftest agent-repl-test-sv-the-window-survives-delete-other-windows ()
  "The sidebar's window stands through `delete-other-windows' in the main area."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let ((main (selected-window))
            (win (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))))
        (split-window main nil 'below)
        (select-window main)
        (delete-other-windows)
        (should (window-live-p win))))))

(ert-deftest agent-repl-test-sv-the-window-is-the-sidebar-s ()
  "The sidebar's window answers as the sidebar's; another window does not."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let ((win (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))))
        (should (agent-repl-sidebar-view-window-p win))
        (should-not (agent-repl-sidebar-view-window-p (window-main-window)))))))

(ert-deftest agent-repl-test-sv-hiding-takes-the-window-off-the-frame ()
  "Hiding the sidebar deletes its window and keeps its web view."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))
      (should (agent-repl-sidebar-view-hide))
      (should-not (agent-repl-sidebar-view-window))
      (should (buffer-live-p made)))))

(ert-deftest agent-repl-test-sv-hiding-with-no-window-does-nothing ()
  "Hiding a sidebar not on screen answers nil."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (should-not (agent-repl-sidebar-view-hide)))))

;;;; ---- The offset the host places panels by ----

(ert-deftest agent-repl-test-sv-the-offset-is-the-gap-between-the-windows-tops ()
  "The offset is how far below the view's content top the sidebar's starts."
  (cl-letf (((symbol-function 'window-inside-pixel-edges)
             (lambda (win) (if (eq win 'sidebar) '(0 30 200 900) '(200 18 1200 700)))))
    (should (= (agent-repl--sidebar-view-offset 'sidebar 'view) 12))))

(ert-deftest agent-repl-test-sv-telling-the-offset-measures-both-windows ()
  "With the sidebar and the current workspace's view on the frame, their gap is told."
  (let ((told nil))
    (cl-letf (((symbol-function 'agent-repl-sidebar-view-window) (lambda (&optional _frame) 'sidebar))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha"))
              ((symbol-function 'agent-repl-window--panel-window) (lambda (_kind _ws &optional _frame) 'view))
              ((symbol-function 'window-live-p) (lambda (win) (memq win '(sidebar view))))
              ((symbol-function 'agent-repl--sidebar-view-offset) (lambda (_s _v) 5))
              ((symbol-function 'agent-repl-webhost-set-sidebar-offset) (lambda (px) (push px told))))
      (agent-repl-sidebar-view-tell-offset)
      (should (equal told '(5))))))

(ert-deftest agent-repl-test-sv-telling-the-offset-without-a-view-tells-nothing ()
  "With no view window on the frame there is nothing to measure against."
  (let ((told nil))
    (cl-letf (((symbol-function 'agent-repl-sidebar-view-window) (lambda (&optional _frame) 'sidebar))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha"))
              ((symbol-function 'agent-repl-window--panel-window) (lambda (_kind _ws &optional _frame) nil))
              ((symbol-function 'agent-repl-webhost-set-sidebar-offset) (lambda (px) (push px told))))
      (agent-repl-sidebar-view-tell-offset)
      (should-not told))))

(ert-deftest agent-repl-test-sv-telling-the-offset-runs-on-every-window-resize ()
  "A layout change is told at once, from the window size hook."
  (should (memq #'agent-repl-sidebar-view-tell-offset (default-value 'window-size-change-functions))))

(ert-deftest agent-repl-test-sv-showing-tells-the-offset ()
  "A sidebar shown tells the host where it starts."
  (agent-repl-test-sv--with-created
    (agent-repl-test-sv--with-frame
      (let ((told 0))
        (cl-letf (((symbol-function 'agent-repl-sidebar-view-tell-offset) (lambda (&rest _) (cl-incf told))))
          (agent-repl-sidebar-view-show (agent-repl-test-sv--conn "127.0.0.1:5"))
          (should (= told 1)))))))

(provide 'test-sidebar-view)

;;; test-sidebar-view.el ends here
