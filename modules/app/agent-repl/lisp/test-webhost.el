;;; test-webhost.el --- ERT tests for webhost.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The one web view that holds every workspace's page: the call scripts and
;; their queueing, the answers and their notes, the desired state Emacs keeps
;; and replays, the coalesced reload, and the guard on the host's buffer.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-webhost.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Harness ----

(defvar agent-repl-test-wh--calls nil
  "Scripts the host was sent, as (SCRIPT . CALLBACK), newest first.")

(defvar agent-repl-test-wh--host 'host-xwidget
  "What the stubbed host lookup answers: the host, or nil.")

(defmacro agent-repl-test-wh--with-host (&rest body)
  "Run BODY with a host present, its scripts recorded, and refs for two workspaces."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-wh--calls nil)
           (agent-repl-test-wh--host 'host-xwidget)
           (agent-repl--webhost-url nil)
           (agent-repl--webhost-daemon-origin nil)
           (agent-repl--webhost-follow-conn nil)
           (agent-repl--webhost-reload-started-ms nil))
       (cl-letf (((symbol-function 'agent-repl-webhost-xwidget)
                  (lambda () agent-repl-test-wh--host))
                 ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
                  (lambda (_xw script callback) (push (cons script callback) agent-repl-test-wh--calls)))
                 ((symbol-function 'agent-repl-host-ref)
                  (lambda (ws) (list :id (concat "id-" ws) :dir (concat "/tmp/" ws))))
                 ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'conn))
                 ((symbol-function 'agent-repl-webhost-ensure) (lambda (_conn) 'host-xwidget))
                 ((symbol-function 'agent-repl-frontend-webview-url)
                  (lambda (ws) (concat "http://ws-" ws ".localhost:1/"))))
         ,@body))))

(defun agent-repl-test-wh--conn (address)
  "A REAL connection at ADDRESS: struct accessors are inlined at load time,
so stubbing the accessor would not reach the code under test."
  (agent-repl-connect-connection-create :address address))

(defun agent-repl-test-wh--sent (method)
  "The scripts sent that call the host's METHOD, oldest first."
  (seq-filter (lambda (script) (string-search (format "h.%s(" method) script))
              (reverse (mapcar #'car agent-repl-test-wh--calls))))

;;;; ---- The call script ----

(ert-deftest agent-repl-test-wh-a-script-calls-the-method-with-encoded-arguments ()
  "Arguments are JSON-encoded, so nothing is spliced into the script unquoted."
  ;; Act
  (let ((script (agent-repl--webhost-script "ensure" "id-\"a\"" "http://x/")))
    ;; Assert
    (should (string-search "h.ensure(\"id-\\\"a\\\"\",\"http://x/\")" script))))

(ert-deftest agent-repl-test-wh-a-script-answers-unknown-call-for-a-method-the-host-lacks ()
  "A host from an older build answers `unknown-call' rather than throwing."
  ;; Act
  (let ((script (agent-repl--webhost-script "setDaemon" "http://host.localhost:2")))
    ;; Assert
    (should (string-search "typeof h[\"setDaemon\"]===\"function\"?h.setDaemon(" script))
    (should (string-search "outcome:\"unknown-call\"" script))))

(defmacro agent-repl-test-wh--with-predates (link &rest body)
  "Run BODY with LINK as the live link, host reloads and records captured.
Binds `reloads' and `records', newest first."
  (declare (indent 1))
  `(let ((reloads nil) (records nil)
         (agent-repl--webhost-follow-conn nil)
         (agent-repl--webhost-reloaded-for-call nil)
         (agent-repl--webhost-predates-reload-pending nil))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () ,link))
               ((symbol-function 'agent-repl-webhost-reload) (lambda (conn) (push conn reloads)))
               ((symbol-function 'agent-repl--log) #'ignore)
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args) (push (cons 'info (apply #'format fmt args)) records)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args) (push (cons 'warn (apply #'format fmt args)) records)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args) (push (cons 'error (apply #'format fmt args)) records))))
       ,@body)))

(ert-deftest agent-repl-test-wh-a-host-that-lacks-a-call-is-reloaded-on-the-live-daemon ()
  "An `unknown-call' answer reloads the host where Emacs is served now, at INFO."
  (let ((live (agent-repl-test-wh--conn "127.0.0.1:2")))
    (agent-repl-test-wh--with-predates live
      ;; Act
      (agent-repl--webhost-answered "{\"op\":\"setDaemon\",\"id\":null,\"outcome\":\"unknown-call\",\"notes\":[]}")
      ;; Assert
      (should (equal reloads (list live)))
      (should (assoc 'info records))
      (should (string-match-p "elisp.webhost.predates-call op=setDaemon action=reload url=http://host.localhost:2/\\?host=1"
                              (cdr (assoc 'info records)))))))

(ert-deftest agent-repl-test-wh-a-stale-host-is-reloaded-on-the-daemon-it-follows ()
  "Mid-handover the link can still name the daemon being left; the host is reloaded where it was pointed."
  (let ((leaving (agent-repl-test-wh--conn "127.0.0.1:1"))
        (followed (agent-repl-test-wh--conn "127.0.0.1:2")))
    (agent-repl-test-wh--with-predates leaving
      ;; Arrange
      (setq agent-repl--webhost-follow-conn followed)
      ;; Act
      (agent-repl--webhost-predates-call "setDaemon")
      ;; Assert
      (should (equal reloads (list followed))))))

(ert-deftest agent-repl-test-wh-following-a-daemon-records-its-connection ()
  "The host's calls moving to a daemon make that daemon the one it belongs on."
  (agent-repl-test-wh--with-host
    (setq agent-repl--webhost-url "http://host.localhost:1/?host=1")
    (let ((successor (agent-repl-test-wh--conn "127.0.0.1:2")))
      ;; Act
      (agent-repl-webhost-follow-daemon "alpha" successor)
      ;; Assert
      (should (eq agent-repl--webhost-follow-conn successor)))))

(ert-deftest agent-repl-test-wh-an-answer-from-the-document-being-left-asks-for-nothing ()
  "An unknown call answered before the reloaded host loads is the old document's: INFO, no second reload."
  (let ((live (agent-repl-test-wh--conn "127.0.0.1:2")))
    (agent-repl-test-wh--with-predates live
      ;; Arrange
      (agent-repl--webhost-predates-call "setDaemon")
      ;; Act
      (agent-repl--webhost-predates-call "setDaemon")
      ;; Assert
      (should (= 1 (length reloads)))
      (should-not (assoc 'error records))
      (should (string-match-p "reason=reload-pending" (cdr (assoc 'info records)))))))

(ert-deftest agent-repl-test-wh-a-host-load-ends-the-pending-reload ()
  "The reloaded host's load committing means later answers are the new document's."
  (agent-repl-test-wh--with-host
    ;; Arrange
    (let ((agent-repl--webhost-predates-reload-pending t))
      (cl-letf (((symbol-function 'agent-repl--webhost-replay) #'ignore))
        ;; Act
        (agent-repl-test-wh--load "load-committed"))
      ;; Assert
      (should-not agent-repl--webhost-predates-reload-pending))))

(ert-deftest agent-repl-test-wh-a-reloaded-host-that-still-lacks-the-call-is-an-error ()
  "A host freshly reloaded on the live daemon that still lacks a call is not stale: ERROR, no second reload."
  (let ((live (agent-repl-test-wh--conn "127.0.0.1:2")))
    (agent-repl-test-wh--with-predates live
      ;; Arrange
      (agent-repl--webhost-predates-call "setDaemon")
      (setq agent-repl--webhost-predates-reload-pending nil)
      ;; Act
      (agent-repl--webhost-predates-call "setDaemon")
      ;; Assert
      (should (= 1 (length reloads)))
      (should (string-match-p "reason=served-webapp-predates-elisp" (cdr (assoc 'error records)))))))

(ert-deftest agent-repl-test-wh-a-host-that-lacks-a-call-with-no-link-waits-at-warn ()
  "With no live daemon there is nowhere to reload the host: WARN, nothing reloaded."
  (agent-repl-test-wh--with-predates nil
    ;; Act
    (agent-repl--webhost-predates-call "setDaemon")
    ;; Assert
    (should-not reloads)
    (should (string-match-p "reason=no-link" (cdr (assoc 'warn records))))))

(ert-deftest agent-repl-test-wh-a-script-queues-itself-when-the-host-has-not-started ()
  "A call made before the host's code started is queued on the page."
  ;; Act
  (let ((script (agent-repl--webhost-script "show" "id-a")))
    ;; Assert
    (should (string-search "window.__agentReplHostPending" script))))

(ert-deftest agent-repl-test-wh-a-queued-answer-names-the-workspace ()
  "The queued answer names the id its first argument carries, so it can be asked again."
  ;; Act
  (let ((script (agent-repl--webhost-script "drawn" "id-a")))
    ;; Assert
    (should (string-search "id:\"id-a\",outcome:\"queued\"" script))))

(ert-deftest agent-repl-test-wh-a-null-argument-is-encoded-as-null ()
  "A gate given no background sends JSON null, not a string."
  ;; Act
  (let ((script (agent-repl--webhost-script "setGateDock" "id-a" 90 nil)))
    ;; Assert
    (should (string-search "h.setGateDock(\"id-a\",90,null)" script))))

;;;; ---- Answers ----

(ert-deftest agent-repl-test-wh-an-answer-is-recorded ()
  "Every answer is logged with its call and outcome."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl--webhost-answered "{\"op\":\"show\",\"id\":null,\"outcome\":\"shown\",\"notes\":[]}"))
      ;; Assert
      (should (equal logged '("elisp.webhost.answer op=show id=nil outcome=shown"))))))

(defun agent-repl-test-wh--note-levels (raw)
  "Record the host's RAW answer and return (LEVEL . LINE) for every note logged."
  (let (logged)
    (cl-letf (((symbol-function 'agent-repl--log) #'ignore)
              ((symbol-function 'agent-repl--info)
               (lambda (_ws fmt &rest args) (push (cons 'info (apply #'format fmt args)) logged)))
              ((symbol-function 'agent-repl--warn)
               (lambda (_ws fmt &rest args) (push (cons 'warn (apply #'format fmt args)) logged)))
              ((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest args) (push (cons 'error (apply #'format fmt args)) logged))))
      (agent-repl--webhost-answered raw))
    (nreverse logged)))

(ert-deftest agent-repl-test-wh-an-info-note-is-recorded-at-info ()
  "What a working host does (a page cooling, a page mid-rollout) is INFO."
  (agent-repl-test--with-clean-state
    ;; Act
    (let ((logged (agent-repl-test-wh--note-levels
                   "{\"op\":\"ids\",\"id\":null,\"outcome\":\"listed\",\"notes\":[{\"level\":\"info\",\"text\":\"b cold\"}]}")))
      ;; Assert
      (should (equal logged '((info . "elisp.webhost.note op=ids note=b cold")))))))

(ert-deftest agent-repl-test-wh-a-warn-note-is-recorded-at-warn ()
  "What a host should never see reaches the log at WARN, never dropped."
  (agent-repl-test--with-clean-state
    ;; Act
    (let ((logged (agent-repl-test-wh--note-levels
                   "{\"op\":\"ids\",\"id\":null,\"outcome\":\"listed\",\"notes\":[{\"level\":\"warn\",\"text\":\"stray\"}]}")))
      ;; Assert
      (should (equal logged '((warn . "elisp.webhost.note op=ids note=stray")))))))

(ert-deftest agent-repl-test-wh-a-bare-note-from-an-older-host-is-recorded-at-warn ()
  "A host from before notes carried a level sends strings, recorded as it recorded them."
  (agent-repl-test--with-clean-state
    ;; Act
    (let ((logged (agent-repl-test-wh--note-levels
                   "{\"op\":\"ids\",\"id\":null,\"outcome\":\"listed\",\"notes\":[\"unreadable\"]}")))
      ;; Assert
      (should (equal logged '((warn . "elisp.webhost.note op=ids note=unreadable")))))))

(ert-deftest agent-repl-test-wh-a-note-of-an-unknown-level-is-an-error ()
  "A level this build does not know is an ERROR naming the note, never dropped."
  (agent-repl-test--with-clean-state
    ;; Act
    (let ((logged (agent-repl-test-wh--note-levels
                   "{\"op\":\"ids\",\"id\":null,\"outcome\":\"listed\",\"notes\":[{\"level\":\"loud\",\"text\":\"x\"}]}")))
      ;; Assert
      (should (eq 'error (car (car logged))))
      (should (string-prefix-p "elisp.webhost.note-unknown-level op=ids" (cdr (car logged)))))))

(ert-deftest agent-repl-test-wh-an-unreadable-answer-is-an-error ()
  "An answer that is not the host's JSON is recorded at ERROR."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl--webhost-answered "not json"))
      ;; Assert
      (should (string-prefix-p "elisp.webhost.answer-unreadable" (car errors))))))

;;;; ---- Calls without a host ----

(ert-deftest agent-repl-test-wh-a-call-without-a-host-sends-nothing ()
  "With no host there is nowhere to send; the desired state is replayed later."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (setq agent-repl-test-wh--host nil)
    ;; Act
    (let ((sent (agent-repl--webhost-call "show" "id-a")))
      ;; Assert
      (should-not sent)
      (should (null agent-repl-test-wh--calls)))))

(ert-deftest agent-repl-test-wh-a-read-without-a-host-answers-nil ()
  "A question with no host to ask is not asked."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (setq agent-repl-test-wh--host nil)
    ;; Act / Assert
    (should-not (agent-repl-webhost-read "drawn" '("id-a") 'ignore))))

(ert-deftest agent-repl-test-wh-a-read-hands-the-answer-to-its-callback ()
  "A question is sent with the caller's callback symbol."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-read "drawn" '("id-a") 'my-callback)
    ;; Assert
    (should (eq (cdar agent-repl-test-wh--calls) 'my-callback))))

;;;; ---- Pages ----

(ert-deftest agent-repl-test-wh-ensuring-a-page-records-its-url ()
  "The host is meant to hold the page at the workspace's URL."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-ensure-page "alpha")
    ;; Assert
    (should (equal (gethash "id-alpha" agent-repl--webhost-pages) "http://ws-alpha.localhost:1/"))))

(ert-deftest agent-repl-test-wh-ensuring-a-page-asks-the-host ()
  "The host is told to hold the page."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-ensure-page "alpha")
    ;; Assert
    (should (= 1 (length (agent-repl-test-wh--sent "ensure"))))))

(ert-deftest agent-repl-test-wh-ensuring-a-page-twice-records-it-once ()
  "A page already held at its URL is recorded at INFO only when it changes."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let (logged)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-webhost-ensure-page "alpha")
        (agent-repl-webhost-ensure-page "alpha"))
      ;; Assert
      (should (= 1 (seq-count (lambda (text) (string-search "page-ensured" text)) logged))))))

(ert-deftest agent-repl-test-wh-showing-a-page-asks-the-host ()
  "The host is told which page to show."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-show "alpha")
    ;; Assert
    (should (equal agent-repl--webhost-shown "id-alpha"))
    (should (= 1 (length (agent-repl-test-wh--sent "show"))))))

(ert-deftest agent-repl-test-wh-showing-a-page-tells-page-unload ()
  "Every show is noted to page-unload.el, which judges looks by it."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl-page-unload-note-shown)
                 (lambda (ws) (push ws noted))))
        ;; Act
        (agent-repl-webhost-show "alpha")
        ;; Assert
        (should (equal noted '("alpha")))))))

(ert-deftest agent-repl-test-wh-showing-the-shown-page-sends-nothing ()
  "Every window change asks; only a change of page reaches the host."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-show "alpha")
    ;; Act
    (agent-repl-webhost-show "alpha")
    ;; Assert
    (should (= 1 (length (agent-repl-test-wh--sent "show"))))))

;;;; ---- A switch's instant rides its show ----

(ert-deftest agent-repl-test-wh-a-noted-switch-rides-its-show ()
  "The switch's wall-clock instant, in Unix ms, is the show's second argument."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-switch-started nil))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 1700000000.123)))
        (agent-repl-webhost-note-switch "alpha"))
      ;; Act
      (agent-repl-webhost-show "alpha")
      ;; Assert
      (should (string-search "h.show(\"id-alpha\",1700000000123)"
                             (car (agent-repl-test-wh--sent "show")))))))

(ert-deftest agent-repl-test-wh-a-show-no-switch-made-carries-no-instant ()
  "A show nothing noted (a follow, a startup landing) sends null."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-switch-started nil))
      ;; Act
      (agent-repl-webhost-show "alpha")
      ;; Assert
      (should (string-search "h.show(\"id-alpha\",null)"
                             (car (agent-repl-test-wh--sent "show")))))))

(ert-deftest agent-repl-test-wh-a-switch-is-never-charged-to-another-workspace-s-show ()
  "A show of another workspace takes the noted switch and drops it."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-switch-started nil))
      (agent-repl-webhost-note-switch "beta")
      ;; Act
      (agent-repl-webhost-show "alpha")
      (agent-repl-webhost-show "beta")
      ;; Assert
      (should (equal (mapcar (lambda (script) (and (string-search ",null)" script) t))
                             (agent-repl-test-wh--sent "show"))
                     '(t t))))))

(ert-deftest agent-repl-test-wh-a-switch-to-the-shown-page-is-forgotten ()
  "A switch to the page already shown sends nothing and leaves nothing noted."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-switch-started nil))
      (agent-repl-webhost-show "alpha")
      (agent-repl-webhost-note-switch "alpha")
      ;; Act
      (agent-repl-webhost-show "alpha")
      ;; Assert
      (should-not agent-repl--webhost-switch-started)
      (should (= 1 (length (agent-repl-test-wh--sent "show")))))))

(ert-deftest agent-repl-test-wh-removing-a-page-forgets-it ()
  "A removed page is no longer held, nor its state kept."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (agent-repl-webhost-set-input-background "alpha" "#000")
    ;; Act
    (agent-repl-webhost-remove "alpha")
    ;; Assert
    (should-not (gethash "id-alpha" agent-repl--webhost-pages))
    (should-not (gethash "id-alpha" agent-repl--webhost-input-backgrounds))))

(ert-deftest agent-repl-test-wh-removing-the-shown-page-shows-none ()
  "The host shows nothing it no longer holds."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-show "alpha")
    ;; Act
    (agent-repl-webhost-remove "alpha")
    ;; Assert
    (should (null agent-repl--webhost-shown))))

(ert-deftest agent-repl-test-wh-a-held-page-is-reported-held ()
  "The host is meant to hold a page that was ensured."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    ;; Act / Assert
    (should (agent-repl-webhost-has-page-p "alpha"))))

(ert-deftest agent-repl-test-wh-reloading-a-page-at-its-url-reloads-it ()
  "A page whose URL did not move is reloaded in place."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    ;; Act
    (cl-letf (((symbol-function 'agent-repl--now-unix-ms) (lambda () 1700000000500)))
      (agent-repl-webhost-reload-page "alpha"))
    ;; Assert
    (should (equal (agent-repl-test-wh--sent "reload")
                   (list (agent-repl--webhost-script "reload" "id-alpha" 1700000000500))))))

(ert-deftest agent-repl-test-wh-reloading-a-page-whose-url-moved-re-points-it ()
  "A handover moves the URL; the page is pointed at the new one, with the reload's start."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (cl-letf (((symbol-function 'agent-repl-frontend-webview-url)
               (lambda (_ws) "http://ws-alpha.localhost:2/"))
              ((symbol-function 'agent-repl-host-conn)
               (let ((successor (agent-repl-test-wh--conn "127.0.0.1:2"))) (lambda (_ws) successor)))
              ((symbol-function 'agent-repl--now-unix-ms) (lambda () 1700000000500)))
      ;; Act
      (agent-repl-webhost-reload-page "alpha"))
    ;; Assert
    (should (equal (gethash "id-alpha" agent-repl--webhost-pages) "http://ws-alpha.localhost:2/"))
    (should (equal (cadr (agent-repl-test-wh--sent "ensure"))
                   (agent-repl--webhost-script "ensure" "id-alpha" "http://ws-alpha.localhost:2/" nil 1700000000500)))))

(ert-deftest agent-repl-test-wh-a-page-moved-to-another-daemon-moves-the-hosts-calls-without-reloading-it ()
  "A handover never reloads the host: its own calls follow the page to the successor."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (setq agent-repl--webhost-url "http://host.localhost:1/?host=1"
          agent-repl--webhost-daemon-origin nil)
    (let ((reloaded nil)
          (successor (agent-repl-test-wh--conn "127.0.0.1:2")))
      (cl-letf (((symbol-function 'agent-repl-frontend-webview-url)
                 (lambda (_ws) "http://ws-alpha.localhost:2/"))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) successor))
                ((symbol-function 'agent-repl-webhost-reload) (lambda (conn) (push conn reloaded))))
        ;; Act
        (agent-repl-webhost-reload-page "alpha"))
      ;; Assert
      (should (null reloaded))
      (should (equal (agent-repl-test-wh--sent "setDaemon")
                     (list (agent-repl--webhost-script "setDaemon" "http://host.localhost:2"))))
      (should (equal (gethash "id-alpha" agent-repl--webhost-pages) "http://ws-alpha.localhost:2/"))
      (should (= 2 (length (agent-repl-test-wh--sent "ensure")))))))

(ert-deftest agent-repl-test-wh-the-hosts-calls-move-once-per-daemon ()
  "Every page of a handover names the same successor; the host is told it once."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (setq agent-repl--webhost-url "http://host.localhost:1/?host=1"
          agent-repl--webhost-daemon-origin nil)
    (let ((successor (agent-repl-test-wh--conn "127.0.0.1:2")))
      ;; Act
      (agent-repl-webhost-follow-daemon "alpha" successor)
      (agent-repl-webhost-follow-daemon "beta" successor)
      ;; Assert
      (should (= 1 (length (agent-repl-test-wh--sent "setDaemon")))))))

(ert-deftest agent-repl-test-wh-the-hosts-own-daemon-moves-nothing ()
  "A page re-pointed onto the daemon that served the host tells the host nothing."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (setq agent-repl--webhost-url "http://host.localhost:2/?host=1"
          agent-repl--webhost-daemon-origin nil)
    ;; Act
    (agent-repl-webhost-follow-daemon "alpha" (agent-repl-test-wh--conn "127.0.0.1:2"))
    ;; Assert
    (should-not (agent-repl-test-wh--sent "setDaemon"))))

(ert-deftest agent-repl-test-wh-a-host-reload-forgets-where-its-calls-were-moved ()
  "A host loaded on a daemon dials that daemon, so a later move is measured from there."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (setq agent-repl--webhost-daemon-origin "http://host.localhost:2"
          agent-repl--webhost-follow-conn (agent-repl-test-wh--conn "127.0.0.1:2"))
    (cl-letf (((symbol-function 'agent-repl-webhost-url) (lambda (_conn) "http://host.localhost:3/?host=1"))
              ((symbol-function 'agent-repl--frontend-webview-navigate-widget) (lambda (_xw _url) nil)))
      ;; Act
      (agent-repl--webhost-reload-now 'conn))
    ;; Assert
    (should-not agent-repl--webhost-daemon-origin)
    (should-not agent-repl--webhost-follow-conn)))

(ert-deftest agent-repl-test-wh-a-page-moved-to-the-hosts-own-daemon-is-re-pointed ()
  "A page moved onto the daemon the host already talks to is re-pointed, the host left alone."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (setq agent-repl--webhost-url "http://host.localhost:2/?host=1")
    (let ((reloaded nil)
          (successor (agent-repl-test-wh--conn "127.0.0.1:2")))
      (cl-letf (((symbol-function 'agent-repl-frontend-webview-url)
                 (lambda (_ws) "http://ws-alpha.localhost:2/"))
                ((symbol-function 'agent-repl-host-conn) (lambda (_ws) successor))
                ((symbol-function 'agent-repl-webhost-reload) (lambda (conn) (push conn reloaded))))
        ;; Act
        (agent-repl-webhost-reload-page "alpha"))
      ;; Assert
      (should (null reloaded))
      (should (= 2 (length (agent-repl-test-wh--sent "ensure")))))))

(ert-deftest agent-repl-test-wh-a-host-reload-records-the-daemon-it-loads-from ()
  "The host's URL is what a later handover is compared against."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-navigate-widget) (lambda (_xw _url) nil)))
      ;; Act
      (agent-repl--webhost-reload-now (agent-repl-test-wh--conn "127.0.0.1:7")))
    ;; Assert
    (should (equal agent-repl--webhost-url "http://host.localhost:7/?host=1"))))

(ert-deftest agent-repl-test-wh-killing-the-host-forgets-its-daemon ()
  "A killed host talks to no daemon; the next one records its own."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((buf (generate-new-buffer " host")))
      (setq agent-repl--webhost-buffer buf
            agent-repl--webhost-url "http://host.localhost:1/?host=1")
      (cl-letf (((symbol-function 'agent-repl--frontend-kill-webview) #'kill-buffer))
        ;; Act
        (agent-repl-webhost-kill))
      ;; Assert
      (should (null agent-repl--webhost-url)))))

(ert-deftest agent-repl-test-wh-an-input-background-is-kept-for-reloads ()
  "The background is recorded, so a reloaded host gets it again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-set-input-background "alpha" "#101010")
    ;; Assert
    (should (equal (gethash "id-alpha" agent-repl--webhost-input-backgrounds) "#101010"))))

(ert-deftest agent-repl-test-wh-a-gate-dock-is-kept-for-reloads ()
  "The gate's slot is recorded, so a reloaded host gets it again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-set-gate-dock "alpha" 90 nil)
    ;; Assert
    (should (equal (gethash "id-alpha" agent-repl--webhost-gate-docks) '(90)))))

(ert-deftest agent-repl-test-wh-held-ingress-entries-are-sent-and-kept-for-reloads ()
  "The editor's ingress entries reach the page, and a reloaded host gets them again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-set-held-ingress "alpha" [])
    ;; Assert
    (should (equal (gethash "id-alpha" agent-repl--webhost-held-ingress) []))
    (should (= 1 (length (agent-repl-test-wh--sent "setHeldIngress"))))))

(ert-deftest agent-repl-test-wh-removing-a-page-forgets-its-held-ingress ()
  "A page dropped from the host takes its ingress entries with it."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-set-held-ingress "alpha" [])
    ;; Act
    (agent-repl-webhost-remove "alpha")
    ;; Assert
    (should-not (gethash "id-alpha" agent-repl--webhost-held-ingress))))

(ert-deftest agent-repl-test-wh-dismissing-a-digest-asks-the-page ()
  "The host relays the close to the workspace's page."
  ;; Arrange
  (agent-repl-test-wh--with-host
    ;; Act
    (agent-repl-webhost-dismiss-digest "alpha")
    ;; Assert
    (should (= 1 (length (agent-repl-test-wh--sent "dismissDigest"))))))

;;;; ---- Replay ----

(defmacro agent-repl-test-wh--with-order (order current &rest body)
  "Run BODY with ORDER as the drawn tab order and CURRENT the current workspace."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl-roster-drawn-tab-order) (lambda () ,order))
             ((symbol-function 'agent-repl--ws-current-name) (lambda () ,current))
             ((symbol-function 'agent-repl--ws-by-ref-id) (lambda (id) (substring id 3))))
     ,@body))

(defun agent-repl-test-wh--load (status)
  "Deliver the host's `load-changed' at STATUS, the stage riding the event."
  (let ((last-input-event (list 'xwidget-event 'host-xwidget 'load-changed status)))
    (agent-repl--webhost-on-load 'load-changed)))

(ert-deftest agent-repl-test-wh-a-load-replays-every-page ()
  "A loaded host starts empty, so every page is handed to it again, in time."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (agent-repl-webhost-ensure-page "beta")
    (setq agent-repl-test-wh--calls nil)
    ;; Act
    (agent-repl-test-wh--load "load-committed")
    ;; Assert
    (should (= 2 (length (agent-repl-test-wh--sent "ensure"))))))

(ert-deftest agent-repl-test-wh-a-load-loads-the-shown-page-now-and-the-rest-in-turn ()
  "The shown page is ensured plainly and shown; every other page is ensured in turn."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (agent-repl-webhost-ensure-page "beta")
    (agent-repl-webhost-show "alpha")
    (setq agent-repl-test-wh--calls nil)
    ;; Act
    (agent-repl-test-wh--load "load-committed")
    ;; Assert
    (should (equal (agent-repl-test-wh--sent "ensure")
                   (list (agent-repl--webhost-script "ensure" "id-alpha" "http://ws-alpha.localhost:1/" nil nil)
                         (agent-repl--webhost-script "ensure" "id-beta" "http://ws-beta.localhost:1/" "after" nil))))))

(ert-deftest agent-repl-test-wh-a-reload-hands-its-start-with-every-page-it-replays ()
  "A replay after a reload hands every page the reload's start, then forgets it."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (agent-repl-webhost-ensure-page "beta")
    (agent-repl-webhost-show "alpha")
    (setq agent-repl-test-wh--calls nil
          agent-repl--webhost-reload-started-ms 1700000000500)
    ;; Act
    (agent-repl-test-wh--load "load-committed")
    ;; Assert
    (should (equal (agent-repl-test-wh--sent "ensure")
                   (list (agent-repl--webhost-script "ensure" "id-alpha" "http://ws-alpha.localhost:1/" nil 1700000000500)
                         (agent-repl--webhost-script "ensure" "id-beta" "http://ws-beta.localhost:1/" "after" 1700000000500))))
    (should-not agent-repl--webhost-reload-started-ms)))

(ert-deftest agent-repl-test-wh-a-reload-of-the-host-records-its-start ()
  "Navigating the host for a reload records the instant the reload began."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (cl-letf (((symbol-function 'agent-repl--now-unix-ms) (lambda () 1700000000500))
              ((symbol-function 'agent-repl-webhost-url) (lambda (_conn) "http://host.localhost:2/?host=1"))
              ((symbol-function 'agent-repl--frontend-webview-navigate-widget) (lambda (_xw _url) nil)))
      ;; Act
      (agent-repl--webhost-reload-now 'conn))
    ;; Assert
    (should (equal agent-repl--webhost-reload-started-ms 1700000000500))))

(defun agent-repl-test-wh--ensured-lanes ()
  "Each ensure sent, as (WORKSPACE . LANE), LANE nil for a plain ensure."
  (mapcar (lambda (script)
            (string-match "\"id-\\([a-z0-9]+\\)\",\"[^\"]*\",\\(?:null\\|\"\\([a-z]+\\)\"\\),\\(?:null\\|[0-9]+\\))" script)
            (cons (match-string 1 script) (match-string 2 script)))
          (agent-repl-test-wh--sent "ensure")))

(ert-deftest agent-repl-test-wh-the-in-turn-pages-are-handed-over-in-two-lanes ()
  "The pages after the current tab go in one lane, those before it in the other.
The current tab's page, not shown here, is the nearest of all."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (dolist (ws '("a" "b" "c" "d" "e")) (agent-repl-webhost-ensure-page ws))
    (setq agent-repl-test-wh--calls nil)
    (agent-repl-test-wh--with-order '("a" "b" "c" "d" "e") "c"
      ;; Act
      (agent-repl-test-wh--load "load-committed"))
    ;; Assert
    (should (equal (agent-repl-test-wh--ensured-lanes)
                   '(("c" . "after") ("d" . "after") ("e" . "after") ("b" . "before") ("a" . "before"))))))

(ert-deftest agent-repl-test-wh-the-lanes-halve-the-ring ()
  "The lanes wrap around the ring, and the lane after takes the odd one out."
  ;; Arrange
  (let ((tabs (mapcar #'number-to-string (number-sequence 1 11))))
    (dolist (case `((,tabs "10" ("11" "1" "2" "3" "4") ("9" "8" "7" "6" "5"))
                    (,tabs "1" ("2" "3" "4" "5" "6") ("11" "10" "9" "8" "7"))
                    (,(butlast tabs) "10" ("1" "2" "3" "4" "5") ("9" "8" "7" "6"))
                    (("1" "2") "1" ("2") ())
                    (("1") "1" () ())))
      (agent-repl-test-wh--with-host
        (agent-repl-test-wh--with-order (nth 0 case) (nth 1 case)
          ;; Act
          (let* ((ids (lambda (names) (mapcar (lambda (ws) (concat "id-" ws)) names)))
                 (lanes (agent-repl--webhost-replay-lanes
                         (funcall ids (remove (nth 1 case) (nth 0 case))))))
            ;; Assert
            (should (equal lanes (cons (funcall ids (nth 2 case))
                                       (funcall ids (nth 3 case)))))))))))

(ert-deftest agent-repl-test-wh-a-lane-skips-a-tab-with-no-page ()
  "A tab whose page the host does not hold keeps its place in its lane, empty."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-test-wh--with-order '("a" "b" "c" "d" "e") "a"
      ;; Act
      (let ((lanes (agent-repl--webhost-replay-lanes '("id-c" "id-e"))))
        ;; Assert
        (should (equal lanes '(("id-c") . ("id-e"))))))))

(ert-deftest agent-repl-test-wh-a-page-with-no-tab-goes-to-the-shorter-lane ()
  "A page whose workspace has no drawn tab goes after the tabbed ones, in the shorter lane."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-test-wh--with-order '("a" "b") "a"
      ;; Act
      (let ((lanes (agent-repl--webhost-replay-lanes '("id-y" "id-b" "id-z"))))
        ;; Assert
        (should (equal lanes '(("id-b" "id-z") . ("id-y"))))))))

(ert-deftest agent-repl-test-wh-with-no-current-tab-every-page-alternates-lanes ()
  "With no current tab every page goes to the shorter lane, in the order given."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-test-wh--with-order '("a" "b" "c") nil
      ;; Act
      (let ((lanes (agent-repl--webhost-replay-lanes '("id-a" "id-b" "id-c"))))
        ;; Assert
        (should (equal lanes '(("id-a" "id-c") . ("id-b"))))))))

(ert-deftest agent-repl-test-wh-a-load-loads-a-mid-adopt-page-at-once ()
  "A page whose workspace waits on its adopt is ensured plainly, out of every lane."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (dolist (ws '("a" "b" "c")) (agent-repl-webhost-ensure-page ws))
    (setq agent-repl-test-wh--calls nil)
    (agent-repl-test-wh--with-order '("a" "b" "c") "a"
      (cl-letf (((symbol-function 'agent-repl-host-adopting-p) (lambda (ws) (equal ws "c"))))
        ;; Act
        (agent-repl-test-wh--load "load-committed")))
    ;; Assert
    (should (equal (agent-repl-test-wh--ensured-lanes)
                   '(("c" . nil) ("a" . "after") ("b" . "after"))))))

(ert-deftest agent-repl-test-wh-the-replay-records-the-mid-adopt-pages ()
  "A load records how many pages it loaded at once for their adopts."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (let ((logged nil))
      (agent-repl-test-wh--with-order '("alpha") nil
        (cl-letf (((symbol-function 'agent-repl-host-adopting-p) (lambda (_ws) t))
                  ((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wh--load "load-committed")))
      ;; Assert
      (should (seq-some (lambda (text) (string-search "elisp.webhost.replay-in-turn after=0 before=0 adopting=1" text)) logged)))))

(ert-deftest agent-repl-test-wh-the-replay-records-the-pages-handed-over-in-turn ()
  "A load records how many pages it handed over in turn."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-test-wh--load "load-committed"))
      ;; Assert
      (should (seq-some (lambda (text) (string-search "elisp.webhost.replay-in-turn after=1 before=0" text)) logged)))))

(ert-deftest agent-repl-test-wh-a-load-replays-the-shown-page ()
  "A loaded host shows the page Emacs stands on again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (agent-repl-webhost-show "alpha")
    (setq agent-repl-test-wh--calls nil)
    ;; Act
    (agent-repl-test-wh--load "load-committed")
    ;; Assert
    (should (= 1 (length (agent-repl-test-wh--sent "show"))))))

;;;; ---- The log level window ----

(ert-deftest agent-repl-test-wh-set-log-level-tells-the-host-the-window ()
  "A raised level is sent to the host with its window's end."
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-log-level nil))
      (agent-repl-webhost-set-log-level 'debug 1792000000000)
      (should (equal (agent-repl-test-wh--sent "setLogLevel")
                     (list (agent-repl--webhost-script "setLogLevel" "debug" 1792000000000)))))))

(ert-deftest agent-repl-test-wh-set-log-level-info-closes-the-window ()
  "Info, with no end, is sent as the close and forgets the window."
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-log-level '(debug . 1792000000000)))
      (agent-repl-webhost-set-log-level 'info nil)
      (should (null agent-repl--webhost-log-level))
      (should (string-search "h.setLogLevel(\"info\",null)" (car (agent-repl-test-wh--sent "setLogLevel")))))))

(ert-deftest agent-repl-test-wh-a-load-replays-a-standing-log-level-window ()
  "A host that loads while a window stands is raised again."
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-log-level nil))
      (agent-repl-webhost-set-log-level 'debug (round (* 1000 (+ (float-time) 60))))
      (setq agent-repl-test-wh--calls nil)
      (agent-repl-test-wh--load "load-committed")
      (should (= 1 (length (agent-repl-test-wh--sent "setLogLevel")))))))

(ert-deftest agent-repl-test-wh-a-load-replays-no-ended-log-level-window ()
  "A window that has ended is not handed to a host that loads after it."
  (agent-repl-test-wh--with-host
    (let ((agent-repl--webhost-log-level (cons 'debug (round (* 1000 (- (float-time) 1))))))
      (agent-repl-test-wh--load "load-committed")
      (should (null (agent-repl-test-wh--sent "setLogLevel"))))))

(ert-deftest agent-repl-test-wh-a-load-replays-each-pages-state ()
  "A loaded host gets every page's input background and gate slot again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (agent-repl-webhost-set-input-background "alpha" "#101010")
    (agent-repl-webhost-set-gate-dock "alpha" 90 "#101010")
    (agent-repl-webhost-set-held-ingress "alpha" [])
    (setq agent-repl-test-wh--calls nil)
    ;; Act
    (agent-repl-test-wh--load "load-committed")
    ;; Assert
    (should (= 1 (length (agent-repl-test-wh--sent "setInputBackground"))))
    (should (= 1 (length (agent-repl-test-wh--sent "setGateDock"))))
    (should (= 1 (length (agent-repl-test-wh--sent "setHeldIngress"))))))

(ert-deftest agent-repl-test-wh-a-load-replays-once-at-commit ()
  "Of a navigation's stages only the commit replays."
  (dolist (case '(("load-started" . 0) ("load-redirected" . 0)
                  ("load-committed" . 1) ("load-finished" . 0)))
    ;; Arrange
    (agent-repl-test-wh--with-host
      (agent-repl-webhost-ensure-page "alpha")
      (setq agent-repl-test-wh--calls nil)
      ;; Act
      (agent-repl-test-wh--load (car case))
      ;; Assert
      (should (= (cdr case) (length (agent-repl-test-wh--sent "ensure")))))))

(ert-deftest agent-repl-test-wh-another-event-replays-nothing ()
  "Only a load replays."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (setq agent-repl-test-wh--calls nil)
    ;; Act
    (let ((last-input-event '(xwidget-event host-xwidget decide-policy)))
      (agent-repl--webhost-on-load 'decide-policy))
    ;; Assert
    (should (null agent-repl-test-wh--calls))))

(ert-deftest agent-repl-test-wh-the-load-handler-takes-the-one-argument-live-hosts-pass ()
  "A host's installed callback outlives a hot reload and calls the handler with one argument."
  ;; Act
  (let ((arity (func-arity #'agent-repl--webhost-on-load)))
    ;; Assert
    (should (equal arity '(1 . 1)))))

(ert-deftest agent-repl-test-wh-the-load-watch-keeps-the-prior-handler ()
  "The xwidget's own handler still gets every event."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((props nil) (seen nil))
      (cl-letf (((symbol-function 'xwidget-get) (lambda (_xw prop) (plist-get props prop)))
                ((symbol-function 'xwidget-put) (lambda (_xw prop value) (setq props (plist-put props prop value)))))
        (setq props (list 'callback (lambda (_xw event) (push event seen))))
        (agent-repl--webhost-watch-load 'host-xwidget)
        ;; Act
        (let ((last-input-event '(xwidget-event host-xwidget load-changed "load-finished")))
          (funcall (plist-get props 'callback) 'host-xwidget 'load-changed)))
      ;; Assert
      (should (equal seen '(load-changed))))))

(ert-deftest agent-repl-test-wh-the-load-watch-replays-at-the-events-commit ()
  "The watch reads the load's status off the event and replays at its commit."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (agent-repl-webhost-ensure-page "alpha")
    (setq agent-repl-test-wh--calls nil)
    (let ((props nil))
      (cl-letf (((symbol-function 'xwidget-get) (lambda (_xw prop) (plist-get props prop)))
                ((symbol-function 'xwidget-put) (lambda (_xw prop value) (setq props (plist-put props prop value)))))
        (agent-repl--webhost-watch-load 'host-xwidget)
        ;; Act
        (let ((last-input-event '(xwidget-event host-xwidget load-changed "load-committed")))
          (funcall (plist-get props 'callback) 'host-xwidget 'load-changed))))
    ;; Assert
    (should (= 1 (length (agent-repl-test-wh--sent "ensure"))))))

;;;; ---- The callback by name ----

(defmacro agent-repl-test-wh--with-props (props &rest body)
  "Run BODY with the xwidget's properties held in the plist variable PROPS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'xwidget-get) (lambda (_xw prop) (plist-get ,props prop)))
             ((symbol-function 'xwidget-put)
              (lambda (_xw prop value) (setq ,props (plist-put ,props prop value)))))
     ,@body))

(ert-deftest agent-repl-test-wh-the-load-watch-installs-the-callback-by-name ()
  "The host's callback is a symbol, so a hot reload reaches it."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((props (list 'callback 'xwidget-webkit-callback)))
      (agent-repl-test-wh--with-props props
        ;; Act
        (agent-repl--webhost-watch-load 'host-xwidget))
      ;; Assert
      (should (eq (plist-get props 'callback) 'agent-repl--webhost-callback)))))

(ert-deftest agent-repl-test-wh-the-installed-callback-reaches-a-redefined-handler ()
  "A handler redefined after the host was watched is the one a load runs."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((props nil) (seen nil))
      (agent-repl-test-wh--with-props props
        (agent-repl--webhost-watch-load 'host-xwidget)
        (cl-letf (((symbol-function 'agent-repl--webhost-on-load)
                   (lambda (event-type &optional _status) (push event-type seen))))
          ;; Act
          (funcall (plist-get props 'callback) 'host-xwidget 'load-changed)))
      ;; Assert
      (should (equal seen '(load-changed))))))

(ert-deftest agent-repl-test-wh-a-rewatch-replaces-an-earlier-builds-closure ()
  "A host an earlier build wrapped in a closure is put back on the callback by name."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((props (list 'callback (lambda (_xw _event) 'earlier-build))))
      (agent-repl-test-wh--with-props props
        ;; Act
        (agent-repl--webhost-rewatch))
      ;; Assert
      (should (eq (plist-get props 'callback) 'agent-repl--webhost-callback)))))

(ert-deftest agent-repl-test-wh-a-rewatch-restores-the-creators-handler-as-prior ()
  "The closure held the creator's handler, so that handler runs first again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((props (list 'callback (lambda (_xw _event) 'earlier-build))))
      (agent-repl-test-wh--with-props props
        ;; Act
        (agent-repl--webhost-rewatch))
      ;; Assert
      (should (eq (plist-get props 'agent-repl-prior-callback) 'xwidget-webkit-callback)))))

(ert-deftest agent-repl-test-wh-a-rewatch-keeps-a-host-already-called-by-name ()
  "A host already on the callback by name keeps the prior handler it was watched with."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let* ((prior (lambda (_xw _event) 'prior))
           (props (list 'callback prior)))
      (agent-repl-test-wh--with-props props
        (agent-repl--webhost-watch-load 'host-xwidget)
        ;; Act
        (agent-repl--webhost-rewatch))
      ;; Assert
      (should (eq (plist-get props 'agent-repl-prior-callback) prior)))))

(ert-deftest agent-repl-test-wh-a-rewatch-without-a-host-touches-nothing ()
  "With no host there is no callback to put back."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((agent-repl-test-wh--host nil) (props nil))
      (agent-repl-test-wh--with-props props
        ;; Act
        (agent-repl--webhost-rewatch))
      ;; Assert
      (should (null props)))))

(ert-deftest agent-repl-test-wh-a-rewatch-is-logged ()
  "Putting a host back on the callback by name is recorded with what it replaced."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let* ((closure (lambda (_xw _event) 'earlier-build))
           (props (list 'callback closure))
           (logged nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        (agent-repl-test-wh--with-props props
          ;; Act
          (agent-repl--webhost-rewatch)))
      ;; Assert
      (should (member (format "elisp.webhost.rewatched replaced=%s" (type-of closure)) logged)))))

;;;; ---- Reload ----

(ert-deftest agent-repl-test-wh-a-reload-burst-reloads-the-host-once ()
  "A rollout's burst of pushes reloads the host once, on the next turn."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((armed 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq armed (1+ armed)) 'reload-timer)))
        ;; Act
        (agent-repl-webhost-reload 'conn)
        (agent-repl-webhost-reload 'conn))
      ;; Assert
      (should (= armed 1)))))

(ert-deftest agent-repl-test-wh-a-deploy-reloads-the-host-once-per-build ()
  "Pushes naming one build, over several turns, reload the host once; a new build again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let ((reloads nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (push agent-repl--webhost-build reloads) 'reload-timer)))
        ;; Act
        (dolist (build '("Web2" "Web2" "Web2" "Web3"))
          (setq agent-repl--webhost-reload-timer nil)
          (agent-repl-webhost-reload-onto 'conn build)))
      ;; Assert
      (should (equal (length reloads) 2))
      (should (equal agent-repl--webhost-build "Web3")))))

(ert-deftest agent-repl-test-wh-a-reload-of-unnamed-build-forgets-the-deploys ()
  "A reload nobody named a build for (a handover's) forgets the deploy's build."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'reload-timer)))
      (agent-repl-webhost-reload-onto 'conn "Web2")
      (setq agent-repl--webhost-reload-timer nil)
      ;; Act
      (agent-repl-webhost-reload 'conn))
    ;; Assert
    (should (null agent-repl--webhost-build))))

(ert-deftest agent-repl-test-wh-a-reload-navigates-the-host-to-its-url ()
  "The host is navigated to the host page on the daemon."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (let (navigated)
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                 (lambda (_xw uri) (setq navigated uri))))
        ;; Act
        (agent-repl--webhost-reload-now (agent-repl-test-wh--conn "127.0.0.1:4242")))
      ;; Assert
      (should (equal navigated "http://host.localhost:4242/?host=1")))))

(ert-deftest agent-repl-test-wh-a-reload-clears-the-burst ()
  "Once the host reloaded, the next rollout reloads it again."
  ;; Arrange
  (agent-repl-test-wh--with-host
    (setq agent-repl--webhost-reload-timer 'reload-timer)
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-navigate-widget) #'ignore))
      ;; Act
      (agent-repl--webhost-reload-now (agent-repl-test-wh--conn "127.0.0.1:4242")))
    ;; Assert
    (should (null agent-repl--webhost-reload-timer))))

;;;; ---- The host's address ----

(ert-deftest agent-repl-test-wh-the-host-url-is-on-the-daemons-port ()
  "The host page is served by the daemon, at a host name of its own."
  ;; Arrange
  (let ((conn (agent-repl-test-wh--conn "127.0.0.1:5151")))
    ;; Act / Assert
    (should (equal (agent-repl-webhost-url conn) "http://host.localhost:5151/?host=1"))))

(ert-deftest agent-repl-test-wh-a-host-url-without-an-address-signals ()
  "A host addressed at nothing is refused loudly."
  ;; Arrange
  (let ((conn (agent-repl-test-wh--conn nil)))
    ;; Act / Assert
    (should-error (agent-repl-webhost-url conn))))

;;;; ---- The host's buffer ----

(ert-deftest agent-repl-test-wh-the-host-buffer-refuses-a-foreign-kill ()
  "A perspective killing its buffers must not take every page with it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--warn) #'ignore))
      ;; Act / Assert
      (should-not (agent-repl--webhost-refuse-kill)))))

(ert-deftest agent-repl-test-wh-a-refused-kill-is-recorded ()
  "The refusal is recorded at WARN, never silent."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (warned)
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
        ;; Act
        (agent-repl--webhost-refuse-kill))
      ;; Assert
      (should (string-prefix-p "elisp.webhost.kill-refused" (car warned))))))

(ert-deftest agent-repl-test-wh-the-host-buffer-allows-its-own-kill ()
  "This module's own kill goes through."
  ;; Arrange
  (let ((agent-repl--webhost-killing t))
    ;; Act / Assert
    (should (agent-repl--webhost-refuse-kill))))

;;; test-webhost.el ends here
