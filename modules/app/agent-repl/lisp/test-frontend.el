;;; test-frontend.el --- ERT tests for frontend.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the xwidget webview panel layer.  Batch Emacs has no
;; xwidget support, so the boundary wrapper
;; (`agent-repl--frontend-make-webview-buffer') is mocked to hand back
;; ordinary buffers; window placement runs against the batch frame's
;; real (single) window.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

(defconst agent-repl-test--frontend-build-id "bid1"
  "Artifact identity every webview URL in this suite is addressed by.

`agent-repl--frontend-build-id' reads the real stamp
\(`webapp/dist/.build-id') and — deliberately — signals when it is
absent, because a URL without the artifact's identity is the
stale-cache bug.  That makes the stamp a BUILD ARTIFACT the suite
would otherwise depend on: a clean checkout has no `webapp/dist', so
every URL-building test failed for want of a `bin/build-frontend.sh'
run rather than for anything about the code under test.

The stamp itself is not this suite's subject — reading it and refusing
a missing one are covered directly, against the real file, by
`agent-repl-test-frontend-build-id-reads-the-stamp' and
`agent-repl-test-frontend-build-id-refuses-a-missing-stamp' in
test-frontend-client.el, which is where that boundary belongs.")

(defmacro agent-repl-test--with-frontend-ws (ws plist &rest body)
  "Register workspace WS with PLIST for BODY, cleaning buffers after.
Also pins the webapp build id (see
`agent-repl-test--frontend-build-id'), so the URLs BODY builds are
independent of whether the webapp has been built in this checkout."
  (declare (indent 2))
  `(progn
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--frontend-build-id)
                    (lambda () agent-repl-test--frontend-build-id)))
           (puthash ,ws (copy-sequence ,plist) agent-repl--workspaces)
           ,@body)
       (let ((buf (agent-repl--ws-get ,ws :frontend-buffer)))
         (when (buffer-live-p buf) (kill-buffer buf)))
       (remhash ,ws agent-repl--workspaces))))

;; The webview boundary mock (`agent-repl-test--fake-webview-factory')
;; lives in test-helpers.el — the explain-config popup mounts the same
;; wrapper, and the two must not drift on what they pretend a webview is.

;;;; ---- Buffer naming -------------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-name-format ()
  "The webview buffer name follows the pinned `*agent-frontend-WS*' format."
  ;; Act
  (let ((name (agent-repl--frontend-webview-buffer-name "myws")))
    ;; Assert
    (should (equal name "*agent-frontend-myws*"))))

(ert-deftest agent-repl-test-frontend-webview-name-distinct-from-input-namespace ()
  "The webview name does not collide with the input buffer's naming scheme.
The two buffers are named by entirely different schemes
\(`agent-panel-input-' vs `agent-frontend-'), so a workspace's webview is
never mistaken for its input composer."
  ;; Act
  (let ((name (agent-repl--frontend-webview-buffer-name "myws")))
    ;; Assert
    (should-not (string-match-p agent-repl--input-buffer-re name))))

(ert-deftest agent-repl-test-frontend-webview-name-is-an-agent-panel-buffer ()
  "The webview name is recognized as an agent panel buffer.
Now that vterm is gone, the webview is one of a workspace's two panel
buffers (alongside the input composer) — `agent-repl--agent-panel-buffer-p'
matches it so the orphan sweep and close-panels-on-open treat it as the
agent panel it is, with no special-casing left to carve out."
  ;; Arrange
  (let* ((name (agent-repl--frontend-webview-buffer-name "myws"))
         (buf (get-buffer-create name)))
    (unwind-protect
        ;; Act / Assert
        (should (agent-repl--agent-panel-buffer-p buf))
      (kill-buffer buf))))

;;;; ---- ensure-webview-buffer ------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-created-and-pinned ()
  "A fresh webview is created at the workspace URL with a pinned name."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Assert
          (should (equal agent-repl-test--urls '("http://x/?workspace=%2Fw")))
          (should (equal (buffer-name buf) "*agent-frontend-ws1*"))
          (should (equal (buffer-local-value 'xwidget-webkit-buffer-name-format buf)
                         "*agent-frontend-ws1*"))
          (should (eq (agent-repl--ws-get "ws1" :frontend-buffer) buf)))))))

(ert-deftest agent-repl-test-frontend-webview-mount-never-probes-health ()
  "The render chokepoint asks the daemon nothing about the session's health.
The probe that used to gate this mount was the create-then-poll shape: the
daemon acked `createSession' as soon as a spawn was issued, so the mount had
to re-ask whether the shim was up — and lost that race.  The ack now proves
establishment, so a probe here would be a question already answered."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '())
        probed)
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-wait-session-healthy)
                 (lambda (&rest _) (setq probed t)))
                ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")
        ;; Assert
        (should-not probed)))))

(ert-deftest agent-repl-test-frontend-webview-stamped-with-its-owner ()
  "A fresh webview records the workspace that owns it.
The stamp is what `agent-repl--foreign-owned-buffer-p' reads, so an
unstamped webview would be invisible to every owner-keyed window sweep
and a background panel build could mount its page over it."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Assert
          (should (equal (agent-repl--buffer-owner buf) "ws1")))))))

(ert-deftest agent-repl-test-frontend-webview-is-foreign-to-another-workspace ()
  "A webview stamped for one workspace reads as foreign to another.
The end the stamp exists for: the sweep a background build runs must
classify another workspace's live page as untouchable."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Assert
          (should (agent-repl--foreign-owned-buffer-p buf "ws2"))
          (should-not (agent-repl--foreign-owned-buffer-p buf "ws1")))))))

(ert-deftest agent-repl-test-frontend-webview-adopted-with-nil-owner-is-foreign-to-nobody ()
  "A webview adopted with no owner (the explain-config popup) stays
eligible for no workspace's window sweep."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-popup-webview*")))
    (unwind-protect
        (progn
          ;; Act
          (agent-repl--frontend-adopt-webview-buffer buf "*agent-explain-config*" nil)
          ;; Assert
          (should-not (agent-repl--buffer-owner buf))
          (should-not (agent-repl--foreign-owned-buffer-p buf "ws1")))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-webview-header-line-cleared ()
  "The mount clears `xwidget-webkit-mode's \"WebKit: <title>\" header-line."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Assert
          (should-not (buffer-local-value 'header-line-format buf)))))))

(ert-deftest agent-repl-test-frontend-webview-reused-for-same-session ()
  "A live webview bound to the same session is reused, not recreated."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        (let ((first (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Act
          (let ((second (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
            ;; Assert — one creation only.
            (should (eq first second))
            (should (= (length agent-repl-test--urls) 1))))))))

(ert-deftest agent-repl-test-frontend-webview-aligns-default-directory ()
  "A mounted webview's `default-directory' is realigned to WS's :project-dir."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Assert
          (should (equal (buffer-local-value 'default-directory buf) "/w/")))))))

(ert-deftest agent-repl-test-frontend-webview-survives-a-session-change ()
  "A live webview is kept whatever happens to the workspace's session.
The mounted URL addresses the WORKSPACE, so a session change leaves it
naming the same thing; remounting for one would throw away a rendered
feed to navigate to the identical address."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        (let ((old (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Act — the workspace's session turns over underneath the view.
          (let ((new (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
            ;; Assert
            (should (eq old new))
            (should (buffer-live-p old))
            (should (= (length agent-repl-test--urls) 1))))))))

;;;; ---- webview URL ------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-url-carries-composer-flag ()
  "The webview URL always hides the webapp composer (Emacs owns input)."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act / Assert
    (should (string-match-p "composer=0"
                            (agent-repl--frontend-webview-url "ws1")))))

(ert-deftest agent-repl-test-frontend-webview-url-carries-parent-ws ()
  "A recorded parent worktree lands in the URL as parent_ws."
  (agent-repl-test--with-frontend-ws "ws1"
      '(:project-dir "/w" :source-ws-dir "/repos/parent-tree/")
    ;; Act / Assert
    (should (string-match-p "parent_ws=parent-tree"
                            (agent-repl--frontend-webview-url "ws1")))))

(ert-deftest agent-repl-test-frontend-webview-url-addresses-the-workspace ()
  "The webview URL names WS's workspace, and carries no session at all.
A session id in the address is what tied the view's lifetime to one
session's: a rotation invalidated the URL and a reload attached to
whatever session the address had recorded."
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/repos/proj")
      ;; Act
      (let ((url (agent-repl--frontend-webview-url "ws1")))
        ;; Assert
        (should (string-prefix-p
                 "http://127.0.0.1:9999/?workspace=%2Frepos%2Fproj" url))
        (should-not (string-match-p "session" url))))))

(ert-deftest agent-repl-test-frontend-webview-url-uses-the-command-wire-key ()
  "The URL's workspace is the SAME key the daemon routes WS's commands by.
Two keyings of one workspace would route a view and the commands sent
from it to different places."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/repos/proj")
    ;; Act / Assert
    (should (string-match-p
             (regexp-quote (url-hexify-string (agent-repl--frontend-ws-command-key "ws1")))
             (agent-repl--frontend-webview-url "ws1")))))

;;;; ---- remount-webview (bundle reload) ----------------------------------------

(ert-deftest agent-repl-test-frontend-remount-webview-reloads-live-buffer ()
  "Remount kills the live webview and mounts a fresh one at the same URL.
This is the whole point over `agent-repl--frontend-ensure-webview-buffer'
reuse: a live buffer would otherwise be kept, so the served bundle would
never be refetched."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls))
                 ((symbol-function 'agent-repl--frontend-after-ensure-session)
                  (lambda (_ws ok _fail) (funcall ok) :ready)))
        (let ((old (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Act
          (let ((new (agent-repl--frontend-remount-webview "ws1")))
            ;; Assert
            (should-not (buffer-live-p old))
            (should (eq new :pending))
            (should (buffer-live-p (agent-repl--ws-get "ws1" :frontend-buffer)))
            (should (= (length agent-repl-test--urls) 2))))))))

(ert-deftest agent-repl-test-frontend-remount-webview-noop-when-closed ()
  "Remount returns nil and mounts nothing when no webview is open."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls))
                ((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws &optional _purpose) "s_1")))
        ;; Act
        (let ((result (agent-repl--frontend-remount-webview "ws1")))
          ;; Assert
          (should (null result))
          (should (null agent-repl-test--urls)))))))

(ert-deftest agent-repl-test-frontend-remount-all-counts-open-webviews ()
  "Remount-all remounts only workspaces with an open webview, and counts them."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (agent-repl-test--with-frontend-ws "ws2" '(:project-dir "/w2")
        (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                   (agent-repl-test--fake-webview-factory 'agent-repl-test--urls))
                  ((symbol-function 'agent-repl--frontend-ensure-session)
                   (lambda (_ws &optional _purpose) "s_x"))
                  ((symbol-function 'agent-repl--live-ws-names)
                   (lambda () '("ws1" "ws2"))))
          (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")
          ;; Act
          (let ((count (agent-repl--frontend-remount-all-webviews)))
            ;; Assert — only ws1 had an open webview.
            (should (= count 1))
            (should (buffer-live-p (agent-repl--ws-get "ws1" :frontend-buffer)))
            (should (null (agent-repl--ws-get "ws2" :frontend-buffer)))))))))

(ert-deftest agent-repl-test-frontend-reload-webview-command-errors-without-webview ()
  "The interactive reload signals when the current workspace has no webview."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
      ;; Act / Assert
      (should-error (agent-repl-frontend-reload-webview) :type 'user-error))))

(ert-deftest agent-repl-test-frontend-reload-webview-command-remounts-current ()
  "The interactive reload remounts the current workspace's open webview."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls))
                 ((symbol-function 'agent-repl--frontend-after-ensure-session)
                  (lambda (_ws ok _fail) (funcall ok) :ready))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        (let ((old (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Act
          (agent-repl-frontend-reload-webview)
          ;; Assert
          (should-not (buffer-live-p old))
          (should (buffer-live-p (agent-repl--ws-get "ws1" :frontend-buffer)))
          (should (= (length agent-repl-test--urls) 2)))))))

;;;; ---- rescue-webview (navigated away) ----------------------------------------

(defmacro agent-repl-test--with-rescue-webview (uri remounted messages &rest body)
  "Run BODY with a mounted webview reporting URI, capturing rescue effects.
REMOUNTED collects the workspaces `agent-repl--frontend-remount-webview'
was asked to remount; MESSAGES collects the echoed user copy.  The
remount itself is mocked because the rescue's own contract is that it
DELEGATES navigation rather than implementing a second one."
  (declare (indent 3))
  `(let ((,remounted nil)
         (,messages nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                (lambda (_buf) 'fake-xwidget))
               ((symbol-function 'agent-repl--frontend-webview-uri)
                (lambda (_xw) ,uri))
               ((symbol-function 'agent-repl--frontend-remount-webview)
                (lambda (ws) (push ws ,remounted) :pending))
               ((symbol-function 'agent-repl--emit-message)
                (lambda (text &rest _) (push text ,messages))))
       ,@body)))

(ert-deftest agent-repl-test-frontend-rescue-webview-remounts-when-astray ()
  "A webview showing another site is remounted against its workspace."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (agent-repl--ws-put "ws1" :frontend-buffer (get-buffer-create "*agent-frontend-ws1*"))
    (agent-repl-test--with-rescue-webview "https://www.google.com/" remounted messages
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        ;; Act
        (agent-repl-frontend-rescue-webview)
        ;; Assert
        (ignore messages)
        (should (equal remounted '("ws1")))))))

(ert-deftest agent-repl-test-frontend-rescue-webview-names-the-stray-host ()
  "The echoed line says which host the webview was brought home from."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (agent-repl--ws-put "ws1" :frontend-buffer (get-buffer-create "*agent-frontend-ws1*"))
    (agent-repl-test--with-rescue-webview "https://www.google.com/" remounted messages
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        ;; Act
        (agent-repl-frontend-rescue-webview)
        ;; Assert
        (ignore remounted)
        (should (equal messages
                       (list "agent-repl: webview brought home from www.google.com")))))))

(ert-deftest agent-repl-test-frontend-rescue-webview-noop-when-already-home ()
  "A webview still on the daemon's own origin is reported and left alone.
Remounting it would throw away a rendered feed to navigate to where the
page already is."
  ;; Arrange
  (let ((agent-repl-frontend-daemon-addr "127.0.0.1:9999"))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (agent-repl--ws-put "ws1" :frontend-buffer (get-buffer-create "*agent-frontend-ws1*"))
      (agent-repl-test--with-rescue-webview "http://127.0.0.1:9999/?workspace=%2Fw&build=abc"
          remounted messages
        (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
          ;; Act
          (agent-repl-frontend-rescue-webview)
          ;; Assert
          (should (null remounted))
          (should (equal messages '("agent-repl: webview is already home"))))))))

(ert-deftest agent-repl-test-frontend-rescue-webview-targets-a-named-workspace ()
  "A workspace name argument targets THAT workspace, not the current one."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws2" '(:project-dir "/w2")
    (agent-repl--ws-put "ws2" :frontend-buffer (get-buffer-create "*agent-frontend-ws2*"))
    (agent-repl-test--with-rescue-webview "https://www.google.com/" remounted messages
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        ;; Act
        (agent-repl-frontend-rescue-webview "ws2")
        ;; Assert
        (ignore messages)
        (should (equal remounted '("ws2")))))))

(ert-deftest agent-repl-test-frontend-rescue-webview-errors-without-webview ()
  "The rescue signals when the workspace has no webview open at all."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
      ;; Act / Assert
      (should-error (agent-repl-frontend-rescue-webview) :type 'user-error))))

(ert-deftest agent-repl-test-frontend-rescue-webview-treats-unknown-uri-as-astray ()
  "A webview that cannot say where it is gets remounted rather than trusted."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (agent-repl--ws-put "ws1" :frontend-buffer (get-buffer-create "*agent-frontend-ws1*"))
    (agent-repl-test--with-rescue-webview nil remounted messages
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        ;; Act
        (agent-repl-frontend-rescue-webview)
        ;; Assert
        (ignore messages)
        (should (equal remounted '("ws1")))))))

(ert-deftest agent-repl-test-frontend-rescue-webview-uri-probe-is-a-registered-boundary ()
  "The read-only URI probe is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-uri
                agent-repl--external-boundary-functions)))

;;;; ---- Copy chords ------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-arms-copy-chords ()
  "The mount arms `agent-repl-frontend-webview-mode' on the webview."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/?workspace=%2Fw")))
          ;; Assert
          (should (buffer-local-value 'agent-repl-frontend-webview-mode buf)))))))

(ert-deftest agent-repl-test-frontend-copy-chord-y ()
  "`y' copies the webview's highlight (the vim reflex)."
  ;; Arrange + Act + Assert
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "y"))
              #'agent-repl-frontend-copy-selection)))

(ert-deftest agent-repl-test-frontend-copy-chord-c-c ()
  "`C-c' copies the webview's highlight (the terminal reflex)."
  ;; Arrange + Act + Assert
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "C-c"))
              #'agent-repl-frontend-copy-selection)))

(ert-deftest agent-repl-test-frontend-copy-mode-normalizes-evil-keymaps ()
  "Enabling the mode rebuilds evil's keymap list, or the chords never win.
Evil ignores a minor-mode map's per-state auxiliary keymaps until
`evil-normalize-keymaps' has run in the buffer, and enabling a minor mode
does not itself trigger it — unnormalized, evil's own maps still outrank
this one and `y' lands on the major mode's aux map instead."
  ;; Arrange — batch has no evil, so the call is observed through a stub.
  (let ((normalized 0))
    (cl-letf (((symbol-function 'evil-normalize-keymaps)
               (lambda (&optional _state) (cl-incf normalized))))
      (with-temp-buffer
        ;; Act
        (agent-repl-frontend-webview-mode 1)
        ;; Assert
        (should (= normalized 1))))))

(ert-deftest agent-repl-test-frontend-copy-mode-survives-a-non-evil-emacs ()
  "The mode enables cleanly where evil is absent (a plain Emacs, batch)."
  ;; Arrange
  (should-not (fboundp 'evil-normalize-keymaps))
  (with-temp-buffer
    ;; Act
    (agent-repl-frontend-webview-mode 1)
    ;; Assert
    (should agent-repl-frontend-webview-mode)))

(ert-deftest agent-repl-test-frontend-copy-selection-reads-the-webview ()
  "The copy command asks the webview for its selection and kills the answer."
  ;; Arrange
  (let ((kill-ring nil)
        (interprogram-cut-function nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-selection)
               (lambda (callback) (funcall callback "highlighted text"))))
      ;; Act
      (agent-repl-frontend-copy-selection)
      ;; Assert
      (should (equal (car kill-ring) "highlighted text")))))

(ert-deftest agent-repl-test-frontend-yank-selection-kills-the-text ()
  "A real selection lands on the kill ring verbatim, whitespace included."
  ;; Arrange
  (let ((kill-ring nil)
        (interprogram-cut-function nil))
    ;; Act
    (agent-repl--frontend-yank-selection "  indented\n")
    ;; Assert
    (should (equal (car kill-ring) "  indented\n"))))

(ert-deftest agent-repl-test-frontend-yank-selection-empty-never-clobbers ()
  "An empty selection leaves the kill ring alone (a stray click kills nothing)."
  ;; Arrange
  (let ((kill-ring '("previous kill"))
        (interprogram-cut-function nil))
    ;; Act
    (agent-repl--frontend-yank-selection "")
    ;; Assert
    (should (equal kill-ring '("previous kill")))))

(ert-deftest agent-repl-test-frontend-yank-selection-blank-never-clobbers ()
  "A whitespace-only selection is nothing highlighted, so nothing is killed."
  ;; Arrange
  (let ((kill-ring '("previous kill"))
        (interprogram-cut-function nil))
    ;; Act
    (agent-repl--frontend-yank-selection " \n ")
    ;; Assert
    (should (equal kill-ring '("previous kill")))))

(ert-deftest agent-repl-test-frontend-yank-selection-nil-never-clobbers ()
  "A nil selection (no answer from the webview) kills nothing."
  ;; Arrange
  (let ((kill-ring '("previous kill"))
        (interprogram-cut-function nil))
    ;; Act
    (agent-repl--frontend-yank-selection nil)
    ;; Assert
    (should (equal kill-ring '("previous kill")))))

;;;; ---- Snapping the feed to its newest message -------------------------------

(ert-deftest agent-repl-test-frontend-snap-to-tail-runs-the-hook ()
  "The snap evaluates the webapp's tail hook inside the live webview."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*"))
          (calls nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                       (lambda (b script) (push (cons b script) calls))))
              ;; Act
              (agent-repl--frontend-snap-webview-to-tail "ws1")
              ;; Assert
              (should (equal calls
                             (list (cons buf
                                         (concat "window.agentReplParkAtTail && "
                                                 "window.agentReplParkAtTail();")))))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-snap-to-tail-hook-name-matches-webapp ()
  "The hook name lisp calls is the one the webapp plants on `window'.
The two constants are a single cross-language contract: webapp/src/host.ts
exports `TAIL_HOOK', frontend.el names it in the script it evaluates, and a
rename on either side silently turns the snap into a no-op."
  ;; Arrange
  (let* ((host-ts (expand-file-name "webapp/src/host.ts" agent-repl--frontend-root))
         (source (progn
                   (should (file-exists-p host-ts))
                   (with-temp-buffer
                     (insert-file-contents host-ts)
                     (buffer-string)))))
    ;; Act + Assert
    (should (string-match-p
             (regexp-quote (format "export const TAIL_HOOK = \"%s\";"
                                   agent-repl-frontend-tail-hook))
             source))))

(ert-deftest agent-repl-test-frontend-snap-to-tail-without-webview-is-noop ()
  "A workspace with no webview yet (never opened, or its panel closed) is
never asked to snap.  The wrapper is left guarded, so any call would fail
the test loudly."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act + Assert — no :frontend-buffer, so the boundary is not reached.
    (agent-repl--frontend-snap-webview-to-tail "ws1")))

(ert-deftest agent-repl-test-frontend-snap-to-tail-dead-webview-is-noop ()
  "A recorded but killed webview is never asked to snap.
The wrapper is left guarded, so any call would fail the test loudly."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (kill-buffer buf)
      ;; Act + Assert — the dead buffer is not reachable by a script.
      (agent-repl--frontend-snap-webview-to-tail "ws1"))))

;;;; ---- Closing the topbar dropdowns on an input-window click -----------------

(ert-deftest agent-repl-test-frontend-close-menus-script-shape ()
  "The close script guards on the hook before calling it."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--frontend-close-menus-script)
                 (concat "window.agentReplCloseTopbarMenus && "
                         "window.agentReplCloseTopbarMenus();"))))

(ert-deftest agent-repl-test-frontend-close-topbar-menus-runs-the-hook ()
  "The close evaluates the webapp's close-menus hook inside the live webview."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*"))
          (calls nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                       (lambda (b script) (push (cons b script) calls))))
              ;; Act
              (agent-repl--frontend-close-topbar-menus "ws1")
              ;; Assert
              (should (equal calls
                             (list (cons buf
                                         (concat "window.agentReplCloseTopbarMenus && "
                                                 "window.agentReplCloseTopbarMenus();")))))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-close-menus-hook-name-matches-webapp ()
  "The hook name lisp calls is the one the webapp plants on `window'.
webapp/src/host.ts exports `CLOSE_MENUS_HOOK'; a rename on either side
silently turns the input-click dismissal into a no-op."
  ;; Arrange
  (let* ((host-ts (expand-file-name "webapp/src/host.ts" agent-repl--frontend-root))
         (source (progn
                   (should (file-exists-p host-ts))
                   (with-temp-buffer
                     (insert-file-contents host-ts)
                     (buffer-string)))))
    ;; Act + Assert
    (should (string-match-p
             (regexp-quote (format "export const CLOSE_MENUS_HOOK = \"%s\";"
                                   agent-repl-frontend-close-menus-hook))
             source))))

(ert-deftest agent-repl-test-frontend-close-topbar-menus-without-webview-is-noop ()
  "A workspace with no webview yet (never opened, or its panel closed) is
never asked to close menus.  The wrapper is left guarded, so any call
would fail the test loudly."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act + Assert — no :frontend-buffer, so the boundary is not reached.
    (agent-repl--frontend-close-topbar-menus "ws1")))

(ert-deftest agent-repl-test-frontend-close-topbar-menus-dead-webview-is-noop ()
  "A recorded but killed webview is never asked to close menus.
The wrapper is left guarded, so any call would fail the test loudly."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (kill-buffer buf)
      ;; Act + Assert — the dead buffer is not reachable by a script.
      (agent-repl--frontend-close-topbar-menus "ws1"))))

(ert-deftest agent-repl-test-frontend-close-menus-on-input-click-fires-on-mouse-click ()
  "A mouse click selecting the workspace's input window closes its dropdowns."
  ;; Arrange
  (let ((closed nil)
        (last-input-event '(mouse-1)))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1"))
              ((symbol-function 'agent-repl-window--panel-window)
               (lambda (kind &rest _) (and (eq kind :input) (selected-window))))
              ((symbol-function 'agent-repl--frontend-close-topbar-menus)
               (lambda (ws) (push ws closed))))
      ;; Act
      (agent-repl--frontend-close-menus-on-input-click (selected-frame))
      ;; Assert
      (should (equal closed '("ws1"))))))

(ert-deftest agent-repl-test-frontend-close-menus-on-input-click-skips-keyboard-selection ()
  "Keyboard selection of the input window leaves the dropdowns alone.
The gesture is a click; keyboard nav into the composer is not, and the
autoselect-on-switch path selects the input window without one."
  ;; Arrange
  (let ((closed nil)
        (last-input-event 'return))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1"))
              ((symbol-function 'agent-repl-window--panel-window)
               (lambda (kind &rest _) (and (eq kind :input) (selected-window))))
              ((symbol-function 'agent-repl--frontend-close-topbar-menus)
               (lambda (ws) (push ws closed))))
      ;; Act
      (agent-repl--frontend-close-menus-on-input-click (selected-frame))
      ;; Assert
      (should-not closed))))

(ert-deftest agent-repl-test-frontend-close-menus-on-input-click-skips-non-input-window ()
  "A click landing on a window other than the input panel leaves it alone."
  ;; Arrange
  (let ((closed nil)
        (last-input-event '(mouse-1))
        (other (split-window)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () "ws1"))
                  ;; The input panel is some OTHER live window, not the selected one.
                  ((symbol-function 'agent-repl-window--panel-window)
                   (lambda (kind &rest _) (and (eq kind :input) other)))
                  ((symbol-function 'agent-repl--frontend-close-topbar-menus)
                   (lambda (ws) (push ws closed))))
          ;; Act
          (agent-repl--frontend-close-menus-on-input-click (selected-frame))
          ;; Assert
          (should-not closed))
      (delete-window other))))

(ert-deftest agent-repl-test-frontend-close-menus-on-input-click-skips-without-workspace ()
  "Outside any workspace a click has no webview whose dropdowns to close."
  ;; Arrange
  (let ((closed nil)
        (last-input-event '(mouse-1)))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () nil))
              ((symbol-function 'agent-repl-window--panel-window)
               (lambda (kind &rest _) (and (eq kind :input) (selected-window))))
              ((symbol-function 'agent-repl--frontend-close-topbar-menus)
               (lambda (ws) (push ws closed))))
      ;; Act
      (agent-repl--frontend-close-menus-on-input-click (selected-frame))
      ;; Assert
      (should-not closed))))

(ert-deftest agent-repl-test-frontend-close-menus-registered-on-selection-change ()
  "The input-click handler is wired onto `window-selection-change-functions'.
An unwired handler is a silent regression — the composer click would
never reach the webview and the dropdowns would hang open."
  ;; Arrange + Act + Assert
  (should (memq #'agent-repl--frontend-close-menus-on-input-click
                window-selection-change-functions)))

;;;; ---- Adjusting the webview's text size -------------------------------------

(ert-deftest agent-repl-test-frontend-text-size-script-shape-positive-delta ()
  "The text-size script guards on the hook and passes a positive delta."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--frontend-text-size-script 0.02)
                 (concat "window.agentReplAdjustTextScale && "
                         "window.agentReplAdjustTextScale(0.02);"))))

(ert-deftest agent-repl-test-frontend-text-size-script-shape-negative-delta ()
  "The text-size script passes a negative delta as a bare JS number."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--frontend-text-size-script -0.02)
                 (concat "window.agentReplAdjustTextScale && "
                         "window.agentReplAdjustTextScale(-0.02);"))))

(ert-deftest agent-repl-test-frontend-text-size-script-shape-reset ()
  "The text-size script passes the `reset' symbol as the quoted JS string."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--frontend-text-size-script 'reset)
                 (concat "window.agentReplAdjustTextScale && "
                         "window.agentReplAdjustTextScale(\"reset\");"))))

(ert-deftest agent-repl-test-frontend-text-size-hook-name-matches-webapp ()
  "The hook name lisp calls is the one the webapp plants on `window'.
webapp/src/host.ts exports `TEXT_SCALE_HOOK'; a rename on either side
silently turns the text-size commands into no-ops."
  ;; Arrange
  (let* ((host-ts (expand-file-name "webapp/src/host.ts" agent-repl--frontend-root))
         (source (progn
                   (should (file-exists-p host-ts))
                   (with-temp-buffer
                     (insert-file-contents host-ts)
                     (buffer-string)))))
    ;; Act + Assert
    (should (string-match-p
             (regexp-quote (format "export const TEXT_SCALE_HOOK = \"%s\";"
                                   agent-repl-frontend-text-size-hook))
             source))))

(ert-deftest agent-repl-test-frontend-adjust-text-size-runs-the-hook ()
  "The adjust evaluates the webapp's text-size hook inside the live webview."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*"))
          (calls nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                       (lambda (b script) (push (cons b script) calls))))
              ;; Act
              (agent-repl--frontend-adjust-text-size "ws1" 0.02)
              ;; Assert
              (should (equal calls
                             (list (cons buf
                                         (agent-repl--frontend-text-size-script 0.02)))))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-adjust-text-size-returns-buffer ()
  "The adjust returns the live webview buffer it drove, so callers can tell
whether a script actually ran."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                       (lambda (_b _script) nil)))
              ;; Act + Assert
              (should (eq (agent-repl--frontend-adjust-text-size "ws1" 'reset) buf))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-adjust-text-size-without-webview-is-noop ()
  "A workspace with no webview is never asked to resize, and the adjust
reports nil so the interactive command can signal.  The wrapper is left
guarded, so any call would fail the test loudly."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act + Assert — no :frontend-buffer, so the boundary is not reached.
    (should-not (agent-repl--frontend-adjust-text-size "ws1" 0.02))))

(ert-deftest agent-repl-test-frontend-adjust-text-size-dead-webview-is-noop ()
  "A recorded but killed webview is never asked to resize, and reports nil.
The wrapper is left guarded, so any call would fail the test loudly."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (kill-buffer buf)
      ;; Act + Assert — the dead buffer is not reachable by a script.
      (should-not (agent-repl--frontend-adjust-text-size "ws1" 0.02)))))

(ert-deftest agent-repl-test-frontend-text-size-increase-sends-positive-step ()
  "The increase command drives the current workspace's webview by +one step."
  ;; Arrange
  (let ((agent-repl-frontend-text-size-step 0.05))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (let ((buf (generate-new-buffer "*agent-frontend-ws1*"))
            (calls nil))
        (unwind-protect
            (progn
              (agent-repl--ws-put "ws1" :frontend-buffer buf)
              (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                         (lambda (_b script) (push script calls)))
                        ((symbol-function 'agent-repl--ws-current-name)
                         (lambda () "ws1")))
                ;; Act
                (agent-repl-frontend-text-size-increase)
                ;; Assert
                (should (equal calls
                               (list (agent-repl--frontend-text-size-script 0.05))))))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-frontend-text-size-decrease-sends-negative-step ()
  "The decrease command drives the current workspace's webview by -one step."
  ;; Arrange
  (let ((agent-repl-frontend-text-size-step 0.05))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (let ((buf (generate-new-buffer "*agent-frontend-ws1*"))
            (calls nil))
        (unwind-protect
            (progn
              (agent-repl--ws-put "ws1" :frontend-buffer buf)
              (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                         (lambda (_b script) (push script calls)))
                        ((symbol-function 'agent-repl--ws-current-name)
                         (lambda () "ws1")))
                ;; Act
                (agent-repl-frontend-text-size-decrease)
                ;; Assert
                (should (equal calls
                               (list (agent-repl--frontend-text-size-script -0.05))))))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-frontend-text-size-reset-sends-reset ()
  "The reset command drives the current workspace's webview with `reset'."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*agent-frontend-ws1*"))
          (calls nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                       (lambda (_b script) (push script calls)))
                      ((symbol-function 'agent-repl--ws-current-name)
                       (lambda () "ws1")))
              ;; Act
              (agent-repl-frontend-text-size-reset)
              ;; Assert
              (should (equal calls
                             (list (agent-repl--frontend-text-size-script 'reset))))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-text-size-command-errors-without-workspace ()
  "The text-size commands signal when there is no current workspace."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
    ;; Act / Assert
    (should-error (agent-repl-frontend-text-size-increase) :type 'user-error)))

(ert-deftest agent-repl-test-frontend-text-size-command-errors-without-webview ()
  "The text-size commands signal when the current workspace has no webview."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
      ;; Act / Assert — no :frontend-buffer, so nothing to resize.
      (should-error (agent-repl-frontend-text-size-reset) :type 'user-error))))

;;;; ---- Placement ---------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-display-hides-panels-first ()
  "Visible agent panels are hidden through the module's own path.
Swapping the webview under the strongly-dedicated output window would
orphan the input panel for the sync sweep and break the next
show-panels — hiding first sidesteps the whole class."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (hidden nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                     (lambda () t))
                    ((symbol-function 'agent-repl--hide-panels)
                     (lambda () (setq hidden t)))
                    ((symbol-function 'agent-repl--ensure-input-buffer)
                     (lambda (_ws) (get-buffer-create "*hides-input*")))
                    ((symbol-function 'agent-repl-window--harden)
                     (lambda (&rest _) nil)))
            ;; Act
            (agent-repl--frontend-display-webview "ws1" buf)
            ;; Assert — panels were hidden and the webview is displayed.
            (should hidden)
            (should (get-buffer-window buf)))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer "*hides-input*")))))

(ert-deftest agent-repl-test-frontend-display-uses-main-area-window ()
  "Without panels, the webview takes a live main-area window."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                     (lambda () nil))
                    ((symbol-function 'agent-repl--ensure-input-buffer)
                     (lambda (_ws) (get-buffer-create "*main-input*")))
                    ((symbol-function 'agent-repl-window--harden)
                     (lambda (&rest _) nil)))
            ;; Act
            (agent-repl--frontend-display-webview "ws1" buf)
            ;; Assert — the webview occupies a main-area window.
            (should (get-buffer-window buf)))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer "*main-input*")))))

(ert-deftest agent-repl-test-frontend-display-mounts-input-panel-below ()
  "Hybrid UI: the classic input buffer splits in below the webview and takes focus."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (input-buf (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                     (lambda () nil))
                    ((symbol-function 'agent-repl--ensure-input-buffer)
                     (lambda (_ws) input-buf))
                    ((symbol-function 'agent-repl-window--harden)
                     (lambda (&rest _) nil)))
            ;; Act
            (agent-repl--frontend-display-webview "ws1" buf)
            ;; Assert — both visible, focus on the input window.
            (should (get-buffer-window buf))
            (should (get-buffer-window input-buf))
            (should (eq (window-buffer (selected-window)) input-buf)))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-frontend-display-clears-other-main-windows ()
  "display-webview wipes pre-existing main-area windows (fullscreen layout).
Whatever the frame carried before the mount (magit, the dashboard,
another workspace's leftovers) must not survive beside the webview +
input panels — the extra-windows-on-first-switch bug."
  ;; Arrange — a second main-area window shows an unrelated buffer.
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (leftover (generate-new-buffer "*leftover*")))
      (unwind-protect
          (progn
            (set-window-buffer (split-window) leftover)
            (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                       (lambda () nil))
                      ((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) (get-buffer-create "*clears-input*")))
                      ((symbol-function 'agent-repl-window--harden)
                       (lambda (&rest _) nil)))
              ;; Act
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert — the leftover window is gone; the webview is up.
              (should-not (get-buffer-window leftover))
              (should (get-buffer-window buf))))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer leftover)
        (kill-buffer "*clears-input*")))))

(ert-deftest agent-repl-test-frontend-display-reclaims-stale-input-window ()
  "Remounting over a surviving dedicated input window must not error.
The webview died but its input window survived (dedicated): the display
path removes or reclaims it instead of erroring \"Window is dedicated\"."
  ;; Arrange — the input window is visible AND dedicated, webview gone.
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (input-buf (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) input-buf)
            (set-window-dedicated-p (selected-window) t)
            (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                       (lambda () nil))
                      ((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input-buf))
                      ((symbol-function 'agent-repl-window--harden)
                       (lambda (&rest _) nil)))
              ;; Act — must not signal.
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert — canonical layout rebuilt: webview + input both visible.
              (should (get-buffer-window buf))
              (should (get-buffer-window input-buf))))
        (set-window-dedicated-p (selected-window) nil)
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-frontend-display-mounts-over-foreign-dedicated-input ()
  "Mounting succeeds when a FOREIGN workspace's dedicated input window is selected.
The real crash: `agent-repl--maybe-autoselect-input' leaves the previous
workspace's hardened (dedicated) input panel selected, the stale-input
reclaim only knows the NEW workspace's own input buffer, and the host
search handed that foreign dedicated window to `set-window-buffer'."
  ;; Arrange — selected window is dedicated to ANOTHER workspace's input.
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (foreign (generate-new-buffer "*agent-panel-input-other*"))
          (mine (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) foreign)
            (set-window-dedicated-p (selected-window) t)
            (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                       (lambda () nil))
                      ((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) mine))
                      ((symbol-function 'agent-repl-window--harden)
                       (lambda (&rest _) nil)))
              ;; Act — must not signal "Window is dedicated to ...".
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert — the webview actually mounted.
              (should (get-buffer-window buf))))
        (dolist (win (window-list nil 'no-minibuffer))
          (set-window-dedicated-p win nil))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer foreign)
        (kill-buffer mine)))))

(ert-deftest agent-repl-test-frontend-display-clears-foreign-dedicated-input ()
  "The foreign workspace's dedicated input window does not survive the mount.
Fullscreen is the sole display format: after the mount the main area
holds this workspace's webview + input and nothing else."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (foreign (generate-new-buffer "*agent-panel-input-other*"))
          (mine (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) foreign)
            (set-window-dedicated-p (selected-window) t)
            (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                       (lambda () nil))
                      ((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) mine))
                      ((symbol-function 'agent-repl-window--harden)
                       (lambda (&rest _) nil)))
              ;; Act
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert
              (should-not (get-buffer-window foreign))))
        (dolist (win (window-list nil 'no-minibuffer))
          (set-window-dedicated-p win nil))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer foreign)
        (kill-buffer mine)))))

(ert-deftest agent-repl-test-frontend-display-mounts-own-input-over-foreign-dedicated ()
  "The mounting workspace's own input panel comes up beside its webview.
Recovering from the foreign dedicated window must rebuild the FULL
canonical layout, not just get the webview on screen."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*"))
          (foreign (generate-new-buffer "*agent-panel-input-other*"))
          (mine (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) foreign)
            (set-window-dedicated-p (selected-window) t)
            (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                       (lambda () nil))
                      ((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) mine))
                      ((symbol-function 'agent-repl-window--harden)
                       (lambda (&rest _) nil)))
              ;; Act
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert
              (should (get-buffer-window mine))))
        (dolist (win (window-list nil 'no-minibuffer))
          (set-window-dedicated-p win nil))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer foreign)
        (kill-buffer mine)))))

(ert-deftest agent-repl-test-frontend-main-area-window-skips-dedicated ()
  "The host search never returns the dedicated selected window.
Regression: the fallback used to hand back `(selected-window)' WITHOUT
re-checking dedication — contradicting this test's own contract — so a
hardened input panel became the mount target and `set-window-buffer'
signalled \"Window is dedicated to ...\"."
  (let ((sel (selected-window)))
    (unwind-protect
        (progn
          ;; Arrange — the frame's only main-area window is dedicated.
          (set-window-dedicated-p sel t)
          ;; Act
          (let ((host (agent-repl--frontend-main-area-window)))
            ;; Assert
            (should-not (eq host sel))))
      (set-window-dedicated-p sel nil)
      (delete-other-windows))))

(ert-deftest agent-repl-test-frontend-main-area-window-host-is-undedicated ()
  "The host made when every window is dedicated is itself undedicated.
A split's child does not inherit its parent's dedication, which is why
splitting beats lifting a dedication another panel recipe set."
  (let ((sel (selected-window)))
    (unwind-protect
        (progn
          ;; Arrange
          (set-window-dedicated-p sel t)
          ;; Act
          (let ((host (agent-repl--frontend-main-area-window)))
            ;; Assert
            (should-not (window-dedicated-p host))))
      (set-window-dedicated-p sel nil)
      (delete-other-windows))))

(ert-deftest agent-repl-test-frontend-main-area-window-preserves-dedication ()
  "Making a host must not clear the dedication of the window it split.
The dedicated window belongs to another workspace's panel recipe;
reclaiming it here would orphan that panel."
  (let ((sel (selected-window)))
    (unwind-protect
        (progn
          ;; Arrange
          (set-window-dedicated-p sel t)
          ;; Act
          (agent-repl--frontend-main-area-window)
          ;; Assert
          (should (window-dedicated-p sel)))
      (set-window-dedicated-p sel nil)
      (delete-other-windows))))

(ert-deftest agent-repl-test-frontend-largest-main-area-window-excludes-side-windows ()
  "The split parent is never a side window.
Splitting a side window keeps the child inside the side-window tree,
so the webview would not land in the main area at all."
  (let ((sel (selected-window)))
    (unwind-protect
        (progn
          ;; Arrange — a second window, and everything but SEL reads as side.
          (split-window sel nil 'below)
          (cl-letf (((symbol-function 'agent-repl-window--side-window-p)
                     (lambda (win &optional _ws) (not (eq win sel)))))
            ;; Act / Assert
            (should (eq (agent-repl--frontend-largest-main-area-window) sel))))
      (delete-other-windows))))

(ert-deftest agent-repl-test-frontend-main-area-window-skips-side-windows ()
  "The webview host window is never a side window."
  ;; Arrange — mark every window EXCEPT the selected one as side.
  (let ((sel (selected-window)))
    (cl-letf (((symbol-function 'agent-repl-window--side-window-p)
               (lambda (win) (not (eq win sel)))))
      ;; Act / Assert
      (should (eq (agent-repl--frontend-main-area-window) sel)))))

;;;; ---- require-xwidget: the one error with no way forward --------------------

;; The gui is the only frontend, so an Emacs without xwidget-webkit cannot
;; open a workspace at all.  There is nothing to fall back to, which is why
;; this error is required to carry the recipe out, not just the diagnosis.

(ert-deftest agent-repl-test-frontend-require-xwidget-signals-when-unavailable ()
  "require-xwidget signals a `user-error' on a build without xwidget support."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
             (lambda () nil)))
    ;; Act / Assert
    (should-error (agent-repl--frontend-require-xwidget) :type 'user-error)))

(ert-deftest agent-repl-test-frontend-require-xwidget-passes-when-available ()
  "require-xwidget is a no-op on a build that has xwidget support."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
             (lambda () t)))
    ;; Act / Assert
    (should-not (agent-repl--frontend-require-xwidget))))

(ert-deftest agent-repl-test-frontend-require-xwidget-message-carries-the-remedy ()
  "The error names the flag that fixes it, not merely the capability that is missing.
A user hitting this has no working frontend, so a bare diagnosis strands them."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
             (lambda () nil)))
    ;; Act
    (let ((msg (condition-case err
                   (agent-repl--frontend-require-xwidget)
                 (user-error (error-message-string err)))))
      ;; Assert
      (should (string-match-p "--with-xwidgets" msg)))))

(ert-deftest agent-repl-test-frontend-require-xwidget-message-carries-the-verification ()
  "The error tells the user how to confirm the rebuild worked."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
             (lambda () nil)))
    ;; Act
    (let ((msg (condition-case err
                   (agent-repl--frontend-require-xwidget)
                 (user-error (error-message-string err)))))
      ;; Assert
      (should (string-match-p "featurep 'xwidget-internal" msg)))))

(ert-deftest agent-repl-test-frontend-xwidget-remedy-offers-homebrew-on-darwin ()
  "On darwin the remedy names the two Homebrew formulae that carry xwidgets."
  ;; Arrange
  (let ((system-type 'darwin))
    ;; Act
    (let ((remedy (agent-repl--xwidget-remedy)))
      ;; Assert
      (should (string-match-p "brew reinstall emacs-mac" remedy)))))

(ert-deftest agent-repl-test-frontend-xwidget-remedy-omits-homebrew-off-darwin ()
  "Off darwin the remedy does not advertise Homebrew formulae that do not apply."
  ;; Arrange
  (let ((system-type 'gnu/linux))
    ;; Act
    (let ((remedy (agent-repl--xwidget-remedy)))
      ;; Assert
      (should-not (string-match-p "brew" remedy)))))

(ert-deftest agent-repl-test-frontend-xwidget-remedy-always-offers-the-source-build ()
  "Every platform gets the from-source configure flag, Homebrew or not."
  ;; Arrange
  (let ((system-type 'gnu/linux))
    ;; Act
    (let ((remedy (agent-repl--xwidget-remedy)))
      ;; Assert
      (should (string-match-p "\\./configure --with-xwidgets" remedy)))))

;;;; ---- open-panel ------------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-open-panel-errors-without-xwidgets ()
  "open-panel refuses on an Emacs build without xwidget support.
The build feature is simulated absent: the test host's batch Emacs may
itself be an xwidget build (featurep reflects the BUILD, not the
session), so the no-support branch must be forced."
  ;; Arrange
  (cl-letf (((symbol-function 'featurep)
             (lambda (f &optional _sub) (not (eq f 'xwidget-internal)))))
    (should-not (agent-repl--frontend-xwidget-available-p))
    ;; Act / Assert
    (should-error (agent-repl-frontend-open-panel) :type 'user-error)))

(ert-deftest agent-repl-test-frontend-xwidget-available-requires-before-probe ()
  "The capability probe loads xwidget.el before the fboundp check.
The creator fn is not autoloaded, so probing first false-negatives on
every xwidget-capable build that has not loaded xwidget.el yet — the
exact failure seen live in the fresh instance."
  ;; Arrange — simulate an xwidget build where the fn appears only
  ;; after (require 'xwidget).
  (let ((required nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f &optional _sub) (eq f 'xwidget-internal)))
              ((symbol-function 'require)
               (lambda (f &optional _file _noerror)
                 (when (eq f 'xwidget) (setq required t) f)))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'xwidget-webkit--create-new-session-buffer)
                     required
                   t))))
      ;; Act / Assert
      (should (agent-repl--frontend-xwidget-available-p))
      (should required))))

(ert-deftest agent-repl-test-frontend-open-panel-wires-session-to-webview ()
  "open-panel establishes the workspace, then mounts and displays its webview."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((displayed nil)
          (ensured nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "ws1"))
                 ((symbol-function 'agent-repl--frontend-after-ensure-session)
                  (lambda (ws ok _fail) (setq ensured ws) (funcall ok) :ready))
                ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
                 (lambda (_ws url)
                   ;; composer=0: Emacs owns input in the hybrid UI.  build: the
                   ;; artifact's identity, so a rebuild is a different address.
                   (should (string-match-p
                            "/\\?workspace=%2Fw&build=[^&]+&composer=0\\'" url))
                   'fake-buffer))
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (_ws buf) (setq displayed buf))))
        ;; Act
        (agent-repl-frontend-open-panel)
        ;; Assert
        (should (equal ensured "ws1"))
        (should (eq displayed 'fake-buffer))))))

(ert-deftest agent-repl-test-frontend-open-mounts-in-the-target-perspective ()
  "The DEFERRED webview mount runs with the TARGET workspace activated.
Establishment is asynchronous, so the continuation fires after the user
may have moved on; without the background-workspace anchor the mount
would lay out the frame of whichever perspective is current then."
  ;; Arrange — establishment is stashed, not run, so the user \"moves\" first.
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((current "other")
          (continuation nil)
          (mounted-in nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () current))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq current ws)))
                ((symbol-function 'agent-repl--restore-focus)
                 (lambda (persp &rest _) (setq current persp)))
                ((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws ok _fail) (setq continuation ok) :pending))
                ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
                 (lambda (_ws _url) 'fake-buffer))
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (_ws _buf) (setq mounted-in current))))
        ;; Act
        (agent-repl--gui-open "ws1")
        (funcall continuation)
        ;; Assert
        (should (equal mounted-in "ws1"))
        (should (equal current "other"))))))

(ert-deftest agent-repl-test-frontend-open-carries-parent-ws-param ()
  "gui-open appends parent_ws (url-encoded :source-ws-dir basename) to the URL.
The webapp status bar renders it in its topbar."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1"
      '(:project-dir "/w" :source-ws-dir "/repos/parent dir/")
    (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
               (lambda () t))
              ((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws &optional _purpose) "s_42"))
              ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
               (lambda (_ws _id url)
                 ;; Assert — encoded basename rides after composer=0.
                 (should (string-suffix-p "&composer=0&parent_ws=parent%20dir" url))
                 'fake-buffer))
              ((symbol-function 'agent-repl--frontend-display-webview) #'ignore))
      ;; Act
      (agent-repl--gui-open "ws1"))))

(ert-deftest agent-repl-test-frontend-open-omits-parent-ws-when-absent ()
  "gui-open leaves parent_ws off the URL when no parent was recorded."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
               (lambda () t))
              ((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws &optional _purpose) "s_42"))
              ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
               (lambda (_ws _id url)
                 (should-not (string-match-p "parent_ws" url))
                 'fake-buffer))
              ((symbol-function 'agent-repl--frontend-display-webview) #'ignore))
      ;; Act
      (agent-repl--gui-open "ws1"))))

(ert-deftest agent-repl-test-frontend-parent-ws-name-empty-string-is-nil ()
  "An empty :source-ws-dir yields nil, not an empty parent name."
  (agent-repl-test--with-frontend-ws "ws1" '(:source-ws-dir "")
    (should-not (agent-repl--frontend-parent-ws-name "ws1"))))

(ert-deftest agent-repl-test-frontend-open-panel-marks-the-choice-explicit ()
  "Asking for the web panel by name is a DELIBERATE frontend choice."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
               (lambda () t))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--gui-open) #'ignore))
      ;; Act
      (agent-repl-frontend-open-panel)
      ;; Assert
      (should (eq (agent-repl--ws-get "ws1" :frontend) 'gui))
      (should (agent-repl--ws-get "ws1" :frontend-explicit)))))

;;;; ---- gui boot (headless) ----------------------------------------------------------

(ert-deftest agent-repl-test-frontend-gui-boot-ensures-session ()
  "The gui boot starts the workspace's daemon session."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((ensured nil))
       (cl-letf (((symbol-function 'agent-repl--frontend-after-ensure-session)
                  (lambda (ws ok _fail) (setq ensured ws) (funcall ok) :ready)))
        ;; Act
        (agent-repl--gui-boot "ws1" "/w" :bare-metal)
        ;; Assert
        (should (equal ensured "ws1"))))))

(ert-deftest agent-repl-test-frontend-gui-boot-mounts-no-webview ()
  "The gui boot is HEADLESS: no webview buffer, no window touched.
A generated workspace is not the current one, so mounting its view here
would evict the caller's windows."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((displayed nil)
          (mounted nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (_ws &optional _purpose) "s_42"))
                ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
                 (lambda (&rest _) (setq mounted t) 'fake-buffer))
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (&rest _) (setq displayed t))))
        ;; Act
        (agent-repl--gui-boot "ws1" "/w" :bare-metal)
        ;; Assert
        (should-not mounted)
        (should-not displayed)))))

(ert-deftest agent-repl-test-frontend-gui-boot-marks-the-workspace-starting ()
  "The gui boot marks :init before the session exists, so a generated
workspace shows a loading badge immediately instead of rendering no
state at all until its agent answered."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws &optional _purpose) "s_42")))
      ;; Act
      (agent-repl--gui-boot "ws1" "/w" :bare-metal)
      ;; Assert
      (should (eq (agent-repl--ws-get "ws1" :agent-state) :init)))))

(ert-deftest agent-repl-test-frontend-gui-boot-refuses-an-undeclared-env ()
  "The gui boot refuses a workspace whose env the gui does not declare.
`:bare-metal' is the gui's only declared environment, so any other value
must be rejected before the daemon is ever contacted."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :active-env :container)
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (&rest _) (error "must not reach the daemon"))))
      ;; Act / Assert
      (should-error (agent-repl--gui-boot "ws1" "/w" :container) :type 'user-error))))

(ert-deftest agent-repl-test-frontend-gui-declares-bare-metal-only ()
  "The registered gui frontend declares :bare-metal as its only environment."
  ;; Act / Assert
  (should (equal (agent-repl-frontend-supported-envs (agent-repl-frontend-get 'gui))
                 '(:bare-metal))))

(ert-deftest agent-repl-test-frontend-gui-registers-a-boot-capability ()
  "The gui frontend registers its headless boot capability."
  ;; Act / Assert
  (should (eq (agent-repl-frontend-boot-fn (agent-repl-frontend-get 'gui))
              'agent-repl--gui-boot)))

;;;; ---- close-panel ------------------------------------------------------------------

(ert-deftest agent-repl-test-frontend-close-panel-kills-and-clears ()
  "close-panel kills the webview and clears its plist key."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        ;; Act
        (agent-repl-frontend-close-panel)
        ;; Assert
        (should-not (buffer-live-p buf))
        (should (null (agent-repl--ws-get "ws1" :frontend-buffer)))))))

(ert-deftest agent-repl-test-frontend-detach-webview-kills-and-clears ()
  "Detaching a webview both kills the buffer and clears the plist key.
Either half alone is a bug: a stale key hands a dead buffer to the next
mount, a cleared key over a live buffer leaks the WKWebView."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      ;; Act
      (agent-repl--frontend-detach-webview "ws1" buf)
      ;; Assert
      (should-not (buffer-live-p buf))
      (should (null (agent-repl--ws-get "ws1" :frontend-buffer))))))

(ert-deftest agent-repl-test-frontend-teardown-sites-share-the-detach ()
  "Every take-down-for-remount site routes through the shared detach.
A hand-rolled kill-plus-clear at one of them fails here rather than
drifting silently out of step with the others."
  ;; Arrange — a detach that records instead of killing.
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((detached 0)
          (buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (cl-letf (((symbol-function 'agent-repl--frontend-detach-webview)
                 (lambda (&rest _) (setq detached (1+ detached))))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--gui-open) #'ignore)
                ((symbol-function 'message) (lambda (&rest _) nil)))
        ;; Act — close-panel and the restart verb's bounce.
        (agent-repl-frontend-close-panel)
        (agent-repl--frontend-bounce-webview "ws1")
        ;; Assert
        (should (equal detached 2)))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-kill-webview-suppresses-query-prompt ()
  "Webview kills bypass kill-buffer query functions.
The xwidget query fn raises a blocking yes-or-no prompt, which would
deadlock the non-interactive nuke hook."
  ;; Arrange — a query fn that refuses every kill.
  (let ((buf (generate-new-buffer "*fake-webview*"))
        (kill-buffer-query-functions (list (lambda () nil))))
    ;; Act
    (agent-repl--frontend-kill-webview buf)
    ;; Assert — killed despite the refusing query fn.
    (should-not (buffer-live-p buf))))

(ert-deftest agent-repl-test-frontend-gui-hide-restores-saved-layout ()
  "gui hide restores the pre-panel layout when one was saved.
Restoring is what removes BOTH gui windows, since the input window
cannot be deleted once it is the sole survivor.  Only fires for the
workspace currently on the frame, so the ws is stubbed active."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((restored nil)
          (closed nil))
      (cl-letf (((symbol-function 'agent-repl--current-ws-p)
                 (lambda (_ws) t))
                ((symbol-function 'agent-repl--restore-fullscreen-config)
                 (lambda (_ws) (setq restored t) t))
                ((symbol-function 'agent-repl--close-buffer-windows)
                 (lambda (&rest _) (setq closed t))))
        ;; Act
        (agent-repl--gui-hide "ws1")
        ;; Assert — restore path taken, no per-window closing.
        (should restored)
        (should-not closed)))))

(ert-deftest agent-repl-test-frontend-gui-hide-falls-back-to-window-close ()
  "Without a saved layout, gui hide closes the windows individually,
resolving the input buffer by name when the plist key is stale nil.
Only fires for the workspace on the frame, so the ws is stubbed active."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((named (get-buffer-create "*agent-panel-input-ws1*"))
          (closed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--current-ws-p)
                     (lambda (_ws) t))
                    ((symbol-function 'agent-repl--restore-fullscreen-config)
                     (lambda (_ws) nil))
                    ((symbol-function 'agent-repl--close-buffer-windows)
                     (lambda (&rest bufs) (setq closed bufs))))
            ;; Act — :input-buffer is nil; the named buffer must resolve.
            (agent-repl--gui-hide "ws1")
            ;; Assert
            (should (memq named closed)))
        (kill-buffer named)))))

(ert-deftest agent-repl-test-frontend-gui-hide-leaves-frame-alone-when-ws-not-current ()
  "gui hide never restores the layout of a NON-current workspace.
A background merge tearing down a DIFFERENT workspace routes through
`agent-repl--gui-kill' -> `agent-repl--gui-hide'.  Restoring that
workspace's saved config (a frame-global `set-window-configuration')
would clobber the visible workspace's windows, so the frame must be
left untouched — neither restore nor per-window close fires."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((restored nil)
          (closed nil))
      (cl-letf (((symbol-function 'agent-repl--current-ws-p)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--restore-fullscreen-config)
                 (lambda (_ws) (setq restored t) t))
                ((symbol-function 'agent-repl--close-buffer-windows)
                 (lambda (&rest _) (setq closed t))))
        ;; Act
        (agent-repl--gui-hide "ws1")
        ;; Assert — neither frame-global window op ran.
        (should-not restored)
        (should-not closed)))))

(ert-deftest agent-repl-test-frontend-gui-hide-drops-stale-layout-when-ws-not-current ()
  "Tearing down a non-current workspace drops its now-moot saved layout.
Leaving `:fullscreen-config' set would let a later reopen of the
workspace restore a stale configuration, so the plist key is cleared."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w"
                                             :fullscreen-config (fake-config))
    (cl-letf (((symbol-function 'agent-repl--current-ws-p)
               (lambda (_ws) nil)))
      ;; Act
      (agent-repl--gui-hide "ws1")
      ;; Assert
      (should (null (agent-repl--ws-get "ws1" :fullscreen-config))))))

(ert-deftest agent-repl-test-frontend-display-saves-layout-once ()
  "The display path saves :fullscreen-config only on a genuine open."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                     (lambda () nil))
                    ((symbol-function 'agent-repl--ensure-input-buffer)
                     (lambda (_ws) (get-buffer-create "*layout-input*")))
                    ((symbol-function 'agent-repl-window--harden)
                     (lambda (&rest _) nil)))
            ;; Act
            (agent-repl--frontend-display-webview "ws1" buf)
            (let ((saved (agent-repl--ws-get "ws1" :fullscreen-config)))
              ;; Assert — saved on first display, not clobbered on re-show.
              (should saved)
              (agent-repl--frontend-display-webview "ws1" buf)
              (should (eq (agent-repl--ws-get "ws1" :fullscreen-config) saved))))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer "*layout-input*")))))

(ert-deftest agent-repl-test-frontend-gui-kill-tears-down-layout-first ()
  "gui kill hides the webview/input windows BEFORE releasing the webview.
The registry's `:restart-fn' composes this kill immediately followed by
`agent-repl--gui-open'; a leftover dedicated input window aborts that
reopen mid-initialize (the \"webview buffer is null/dead\" cascade)."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((order nil))
      (cl-letf (((symbol-function 'agent-repl--gui-hide)
                 (lambda (_ws) (push 'hide order)))
                ((symbol-function 'agent-repl--frontend-release-workspace-webview)
                 (lambda (_ws) (push 'release-webview order))))
        ;; Act
        (agent-repl--gui-kill "ws1")
        ;; Assert — layout teardown precedes the release.
        (should (equal (nreverse order) '(hide release-webview)))
        (should (null (agent-repl--ws-get "ws1" :frontend-buffer)))))))

(ert-deftest agent-repl-test-frontend-gui-kill-leaves-the-daemon-session-alone ()
  "Closing a panel is not discarding the conversation.
A teardown that ended the session would stamp its record with a death
reason `resume-resolve' reads as the user discarding the conversation,
which is not what closing a panel says.  The daemon locates a session by
cwd, so a reopened workspace reattaches to the same record."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((commands nil))
      (cl-letf (((symbol-function 'agent-repl--gui-hide) #'ignore)
                ((symbol-function 'agent-repl--frontend-release-workspace-webview) #'ignore)
                ((symbol-function 'agent-repl--uds-send-command)
                 (lambda (field &rest _) (push field commands) "req")))
        ;; Act
        (agent-repl--gui-kill "ws1")
        ;; Assert
        (should (null commands))))))

(ert-deftest agent-repl-test-frontend-webview-killed-on-ws-nuke ()
  "The nuke hook kills the webview so the WKWebView never outlives the ws."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      ;; Act — simulate the pre-tombstone hook dispatch.
      (agent-repl--frontend-release-workspace-webview "ws1")
      ;; Assert
      (should-not (buffer-live-p buf)))))

(ert-deftest agent-repl-test-frontend-webview-release-registered-on-ws-del-hook ()
  "The webview release fn is registered on the pre-tombstone hook."
  ;; Assert
  (should (memq #'agent-repl--frontend-release-workspace-webview
                agent-repl-ws-del-hook)))

(ert-deftest agent-repl-test-frontend-close-panel-errors-without-webview ()
  "close-panel on a workspace with no webview raises a user-error."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
      ;; Act / Assert
      (should-error (agent-repl-frontend-close-panel) :type 'user-error))))

(provide 'test-frontend)

;;; test-frontend.el ends here

;;;; ---- Chess-board keyboard navigation ---------------------------------------

(ert-deftest agent-repl-test-frontend-chess-step-script-shape ()
  "The step script guards on the hook and passes the direction."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--frontend-chess-step-script "back")
                 (concat "window.agentReplChessStep && "
                         "window.agentReplChessStep(\"back\");"))))

(ert-deftest agent-repl-test-frontend-chess-back-evaluates-in-current-buffer ()
  "The back command drives the current buffer's webview with the back script."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
               (lambda (b script) (push (cons b script) calls))))
      (with-temp-buffer
        ;; Act
        (agent-repl-frontend-chess-back)
        ;; Assert
        (should (equal calls
                       (list (cons (current-buffer)
                                   (agent-repl--frontend-chess-step-script "back")))))))))

(ert-deftest agent-repl-test-frontend-chess-forward-evaluates-in-current-buffer ()
  "The forward command drives the current buffer's webview with the forward script."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
               (lambda (b script) (push (cons b script) calls))))
      (with-temp-buffer
        ;; Act
        (agent-repl-frontend-chess-forward)
        ;; Assert
        (should (equal calls
                       (list (cons (current-buffer)
                                   (agent-repl--frontend-chess-step-script "forward")))))))))

(ert-deftest agent-repl-test-frontend-chess-hook-name-matches-webapp ()
  "The nav hook name lisp calls is the one the webapp plants on `window'.
webapp/src/chess-game.ts exports `CHESS_NAV_HOOK'; a rename on either
side silently turns the keys into no-ops."
  ;; Arrange
  (let* ((ts (expand-file-name "webapp/src/chess-game.ts" agent-repl--frontend-root))
         (source (progn
                   (should (file-exists-p ts))
                   (with-temp-buffer
                     (insert-file-contents ts)
                     (buffer-string)))))
    ;; Act + Assert
    (should (string-match-p
             (format "CHESS_NAV_HOOK = \"%s\""
                     (regexp-quote agent-repl-frontend-chess-step-hook))
             source))))

(ert-deftest agent-repl-test-frontend-webview-map-binds-chess-nav-keys ()
  "The webview minor-mode map routes h/l and the arrows to board stepping."
  ;; Arrange + Act + Assert
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "h"))
              #'agent-repl-frontend-chess-back))
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "l"))
              #'agent-repl-frontend-chess-forward))
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "<left>"))
              #'agent-repl-frontend-chess-back))
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "<right>"))
              #'agent-repl-frontend-chess-forward)))

;;;; ---- the hard session restart command ---------------------------------

(defmacro agent-repl-test--with-restart-session (build &rest body)
  "Run BODY with the restart verb's collaborators faked, ws1 current.
BUILD stands in for `agent-repl--frontend-build-targets-async' and is
called with (TARGETS FORCE ON-SUCCESS ON-FAILURE), so each test decides
whether the build succeeds, fails, or never settles.
`agent-repl-test--restart-asked' records the workspace whose shim restart
was issued, and `agent-repl-test--restart-opened' the workspace whose
webview was reopened."
  (declare (indent 1))
  `(let ((agent-repl-test--restart-asked nil)
         (agent-repl-test--restart-opened nil))
     (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
               ((symbol-function 'agent-repl--frontend-restart-session)
                (lambda (ws) (setq agent-repl-test--restart-asked ws) "req-1"))
               ((symbol-function 'agent-repl--gui-open)
                (lambda (ws) (setq agent-repl-test--restart-opened ws)))
               ((symbol-function 'agent-repl--frontend-build-targets-async) ,build)
               ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body)))

(defvar agent-repl-test--restart-asked nil
  "Workspace whose shim restart the faked client recorded.")

(defvar agent-repl-test--restart-opened nil
  "Workspace whose webview the faked gui open recorded.")

(ert-deftest agent-repl-test-restart-session-command-dispatches ()
  "`agent-repl-restart-session' asks the client to restart the current ws."
  ;; Arrange
  (agent-repl-test--with-restart-session (lambda (&rest _) 'started)
    ;; Act
    (agent-repl-restart-session)
    ;; Assert
    (should (equal agent-repl-test--restart-asked "ws1"))))

(ert-deftest agent-repl-test-restart-session-kicks-off-the-webapp-build ()
  "The restart requests an asynchronous build of the webapp target alone.
The daemon is not rebuilt or restarted by this verb."
  ;; Arrange
  (let (targets)
    (agent-repl-test--with-restart-session
        (lambda (ts &rest _) (setq targets ts) 'started)
      ;; Act
      (agent-repl-restart-session)
      ;; Assert
      (should (equal targets '("webapp"))))))

(ert-deftest agent-repl-test-restart-session-issues-the-shim-restart-unbuilt ()
  "The shim restart is issued while the build is still in flight.
It is never held behind the build, so a slow or never-settling build
cannot delay the restart the user asked for."
  ;; Arrange — a build that captures its continuations and never calls them.
  (agent-repl-test--with-restart-session (lambda (&rest _) 'started)
    ;; Act
    (agent-repl-restart-session)
    ;; Assert
    (should (equal agent-repl-test--restart-asked "ws1"))
    (should (null agent-repl-test--restart-opened))))

(ert-deftest agent-repl-test-restart-session-bounces-the-webview-on-build-success ()
  "A successful build closes the webview and reopens it on the new build id."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (agent-repl-test--with-restart-session
          (lambda (_ts _force on-success _on-failure) (funcall on-success) 'started)
        ;; Act
        (agent-repl-restart-session)
        ;; Assert
        (should-not (buffer-live-p buf))
        (should (equal agent-repl-test--restart-opened "ws1"))))))

(ert-deftest agent-repl-test-restart-session-leaves-the-webview-on-build-failure ()
  "A failed build leaves the webview alone rather than bouncing it."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (agent-repl-test--with-restart-session
          (lambda (_ts _force _on-success on-failure)
            (funcall on-failure "exit 1") 'started)
        ;; Act
        (agent-repl-restart-session)
        ;; Assert
        (should (buffer-live-p buf))
        (should (null agent-repl-test--restart-opened))))))

(ert-deftest agent-repl-test-restart-session-warns-on-build-failure ()
  "A failed build is surfaced loudly rather than swallowed."
  ;; Arrange
  (let (warned)
    (agent-repl-test--with-restart-session
        (lambda (_ts _force _on-success on-failure)
          (funcall on-failure "exit 1") 'started)
      (cl-letf (((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args)))))
        ;; Act
        (agent-repl-restart-session)))
    ;; Assert
    (should (string-match-p "exit 1" (or warned "")))))

(ert-deftest agent-repl-test-restart-session-bounce-skips-a-closed-panel ()
  "A workspace with no open webview is not given one by a successful build."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (agent-repl-test--with-restart-session
        (lambda (_ts _force on-success _on-failure) (funcall on-success) 'started)
      ;; Act
      (agent-repl-restart-session)
      ;; Assert
      (should (null agent-repl-test--restart-opened)))))

(ert-deftest agent-repl-test-restart-session-command-needs-a-workspace ()
  "With no current workspace the restart signals rather than guessing one."
  (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
    (should-error (agent-repl-restart-session) :type 'user-error)))

;;;; ---- the deliberate hibernate command ---------------------------------

(ert-deftest agent-repl-test-hibernate-workspace-command-dispatches ()
  "`agent-repl-hibernate-workspace' asks the client to hibernate the current ws."
  ;; Arrange
  (let (asked)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--frontend-hibernate-workspace)
               (lambda (ws) (setq asked ws) "req-1"))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-hibernate-workspace)
      ;; Assert
      (should (equal asked "ws1")))))

(ert-deftest agent-repl-test-hibernate-workspace-command-needs-a-workspace ()
  "With no current workspace the hibernate signals rather than guessing one."
  (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
    (should-error (agent-repl-hibernate-workspace) :type 'user-error)))

;;;; ---- Refreshing live webviews -----------------------------------------

(defmacro agent-repl-test--with-webview-buffers (names &rest body)
  "Create a buffer per name in NAMES for BODY, killing them afterwards."
  (declare (indent 1))
  `(let ((agent-repl-test--bufs (mapcar #'get-buffer-create ,names)))
     (unwind-protect (progn ,@body)
       (dolist (b agent-repl-test--bufs)
         (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest agent-repl-test-refresh-webviews-skips-non-frontend-buffers ()
  "Only `*agent-frontend-WS*' buffers are reloaded; other buffers are untouched."
  ;; Arrange
  (let (reloaded)
    (agent-repl-test--with-webview-buffers
        '("*agent-frontend-ws1*" "*agent-panel-input-ws1*" "*scratch-not-ours*")
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (buf) (buffer-name buf)))
                ((symbol-function 'agent-repl--frontend-webview-reload-widget)
                 (lambda (xw) (push xw reloaded) "http://x/?session=s_1"))
                ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-refresh-webviews)
        ;; Assert
        (should (equal reloaded '("*agent-frontend-ws1*")))))))

(ert-deftest agent-repl-test-refresh-webviews-returns-refreshed-count ()
  "The sweep returns how many webviews it actually reloaded."
  ;; Arrange
  (agent-repl-test--with-webview-buffers
      '("*agent-frontend-ws1*" "*agent-frontend-ws2*" "*agent-panel-input-ws1*")
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
               (lambda (buf) (buffer-name buf)))
              ((symbol-function 'agent-repl--frontend-webview-reload-widget)
               (lambda (_xw) "http://x/?session=s_1"))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should (= (agent-repl-refresh-webviews) 2)))))

(ert-deftest agent-repl-test-refresh-webviews-dead-webview-warns-and-continues ()
  "A buffer whose WKWebView is dead warns, and the next webview still reloads."
  ;; Arrange
  (let (reloaded warnings)
    (agent-repl-test--with-webview-buffers
        '("*agent-frontend-dead*" "*agent-frontend-live*")
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (buf)
                   (unless (equal (buffer-name buf) "*agent-frontend-dead*")
                     (buffer-name buf))))
                ((symbol-function 'agent-repl--frontend-webview-reload-widget)
                 (lambda (xw) (push xw reloaded) "http://x/?session=s_1"))
                ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--warn)
                 (lambda (ws fmt &rest args)
                   (push (cons ws (apply #'format fmt args)) warnings))))
        ;; Act
        (should (= (agent-repl-refresh-webviews) 1))
        ;; Assert
        (should (equal reloaded '("*agent-frontend-live*")))
        (should (equal (length warnings) 1))
        (should (equal (car (car warnings)) "dead"))
        (should (string-match-p "outcome=dead-webview" (cdr (car warnings))))))))

(ert-deftest agent-repl-test-refresh-webviews-reload-error-warns-and-continues ()
  "A webview whose reload signals warns, and the sweep still reloads the next."
  ;; Arrange
  (let (reloaded warnings)
    (agent-repl-test--with-webview-buffers
        '("*agent-frontend-broken*" "*agent-frontend-ok*")
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (buf) (buffer-name buf)))
                ((symbol-function 'agent-repl--frontend-webview-reload-widget)
                 (lambda (xw)
                   (when (equal xw "*agent-frontend-broken*")
                     (error "no uri"))
                   (push xw reloaded)
                   "http://x/?session=s_1"))
                ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        ;; Act
        (should (= (agent-repl-refresh-webviews) 1))
        ;; Assert
        (should (equal reloaded '("*agent-frontend-ok*")))
        (should (string-match-p "outcome=reload-failed" (car warnings)))))))

(ert-deftest agent-repl-test-refresh-webviews-no-buffers-is-a-quiet-no-op ()
  "With no webview buffers open the sweep reloads nothing and returns 0."
  ;; Arrange
  (let (reloaded)
    (cl-letf (((symbol-function 'agent-repl--frontend-live-webview-buffers)
               (lambda () nil))
              ((symbol-function 'agent-repl--frontend-webview-reload-widget)
               (lambda (xw) (push xw reloaded) "http://x/"))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should (= (agent-repl-refresh-webviews) 0))
      (should-not reloaded))))

(ert-deftest agent-repl-test-refresh-webviews-logs-workspace-and-buffer ()
  "Each refresh is logged with its workspace and buffer name."
  ;; Arrange
  (let (logs)
    (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (buf) (buffer-name buf)))
                ((symbol-function 'agent-repl--frontend-webview-reload-widget)
                 (lambda (_xw) "http://x/?session=s_1"))
                ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (ws fmt &rest args)
                   (push (cons ws (apply #'format fmt args)) logs))))
        ;; Act
        (agent-repl-refresh-webviews)
        ;; Assert
        (should (seq-find (lambda (entry)
                            (and (equal (car entry) "ws1")
                                 (string-match-p "buffer=\\*agent-frontend-ws1\\* outcome=refreshed"
                                                 (cdr entry))))
                          logs))))))

(ert-deftest agent-repl-test-refresh-webviews-workspace-prefers-owning-local ()
  "The logged workspace comes from the buffer's owner when one is stamped."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (with-current-buffer "*agent-frontend-ws1*"
      (setq-local agent-repl--owning-workspace "renamed-ws"))
    ;; Act / Assert
    (should (equal (agent-repl--frontend-webview-workspace
                    (get-buffer "*agent-frontend-ws1*"))
                   "renamed-ws"))))

(ert-deftest agent-repl-test-refresh-webviews-widget-probe-is-a-registered-boundary ()
  "The live-widget probe is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-live-widget
                agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-test-refresh-webviews-reload-is-a-registered-boundary ()
  "The reload wrapper is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-reload-widget
                agent-repl--external-boundary-functions)))

;;;; ---- Returning the keyboard to Emacs after a script evaluation ------------

(defmacro agent-repl-test--capturing-scripts (var &rest body)
  "Run BODY with the raw execute-script boundary collecting scripts into VAR.
VAR is bound to a list in reverse call order."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script-1)
                (lambda (_buf script) (push script ,var))))
       ,@body)))

(defmacro agent-repl-test--with-window-buffer (buf &rest body)
  "Display BUF in the selected window for BODY, restoring the old buffer after."
  (declare (indent 1))
  `(let ((agent-repl-test--previous (window-buffer (selected-window))))
     (unwind-protect
         (progn (set-window-buffer (selected-window) ,buf) ,@body)
       (set-window-buffer (selected-window) agent-repl-test--previous))))

(defun agent-repl-test--count-substring (needle haystack)
  "Return how many non-overlapping times NEEDLE occurs in HAYSTACK."
  (let ((n 0) (start 0) hit)
    (while (setq hit (string-search needle haystack start))
      (setq n (1+ n)
            start (+ hit (length needle))))
    n))

(ert-deftest agent-repl-test-frontend-execute-script-appends-keyboard-release ()
  "A script evaluated while another window is selected carries the blur."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (agent-repl-test--capturing-scripts scripts
      ;; Act
      (agent-repl--frontend-webview-execute-script
       (get-buffer "*agent-frontend-ws1*") "noop();")
      ;; Assert
      (should (string-suffix-p agent-repl-frontend-keyboard-release-js
                               (car scripts))))))

(ert-deftest agent-repl-test-frontend-execute-script-keeps-the-callers-script ()
  "The keyboard release is appended to the caller's script, never replacing it."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (agent-repl-test--capturing-scripts scripts
      ;; Act
      (agent-repl--frontend-webview-execute-script
       (get-buffer "*agent-frontend-ws1*") "window.someHook();")
      ;; Assert
      (should (string-prefix-p "window.someHook();" (car scripts))))))

(ert-deftest agent-repl-test-frontend-execute-script-spares-the-selected-webview ()
  "No keyboard release is appended when the webview's own window is selected."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (let ((buf (get-buffer "*agent-frontend-ws1*")))
      (agent-repl-test--capturing-scripts scripts
        (agent-repl-test--with-window-buffer buf
          ;; Act
          (agent-repl--frontend-webview-execute-script buf "noop();"))
        ;; Assert
        (should (equal (car scripts) "noop();"))))))

(ert-deftest agent-repl-test-frontend-execute-script-burst-releases-once-each ()
  "A burst of evaluations issues no extra evaluations and blurs once per script."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (let ((buf (get-buffer "*agent-frontend-ws1*")))
      (agent-repl-test--capturing-scripts scripts
        ;; Act
        (dotimes (_ 6)
          (agent-repl--frontend-webview-execute-script buf "noop();"))
        ;; Assert
        (should (= (length scripts) 6))
        (should (cl-every (lambda (s)
                            (= 1 (agent-repl-test--count-substring
                                  agent-repl-frontend-keyboard-release-js s)))
                          scripts))))))

(ert-deftest agent-repl-test-frontend-keyboard-release-blurs-the-active-element ()
  "The release script blurs `document.activeElement', the NS focus gate."
  (should (string-match-p "document\\.activeElement\\.blur()"
                          agent-repl-frontend-keyboard-release-js)))

(ert-deftest agent-repl-test-frontend-execute-script-raw-is-a-registered-boundary ()
  "The raw execute-script wrapper is registered as an external boundary."
  (should (memq 'agent-repl--frontend-webview-execute-script-1
                agent-repl--external-boundary-functions)))

;;;; ---- snap-webview-to-tail skip-record routing ----

(ert-deftest agent-repl-test-frontend-snap-skip-for-a-placeholder-logs-globally ()
  "Snapping a persp PLACEHOLDER's absent webview records globally.
The workspace-switch path snaps whatever perspective persp-mode activated,
and \"main\"/\"none\" have neither a webview nor a durable log sink."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((logged 'no-record))
      (cl-letf (((symbol-function 'agent-repl--log-verbose)
                 (lambda (ws &rest _)
                   (when (eq logged 'no-record) (setq logged ws)))))
        ;; Act
        (agent-repl--frontend-snap-webview-to-tail "main"))
      ;; Assert
      (should (null logged)))))

(ert-deftest agent-repl-test-frontend-snap-skip-for-a-routable-ws-keeps-attribution ()
  "A REAL workspace's snap-skip record stays attributed to that workspace."
  (agent-repl-test--with-clean-state
    ;; Arrange
    (let ((project (make-temp-file "agent-repl-snap-route-" t))
          (logged 'no-record))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project)
            (cl-letf (((symbol-function 'agent-repl--log-verbose)
                       (lambda (ws &rest _)
                         (when (eq logged 'no-record) (setq logged ws)))))
              ;; Act
              (agent-repl--frontend-snap-webview-to-tail "ws1")))
        (delete-directory project t))
      ;; Assert
      (should (equal logged "ws1")))))

;;;; ---- The open path never holds the main thread ---------------------------
;;
;; `gui-open' and `gui-show' used to complete while the caller waited: the
;; lazy daemon ensure ran a whole-stack deploy through `call-process', so
;; the editor was frozen for the length of a Go/npm build and the only
;; escape was `C-g'.  Every test below pins one half of the replacement —
;; the command returns at once, the outcome (mount OR failure) arrives from
;; a continuation, and nothing the open touches is left half-written when a
;; quit lands.

(ert-deftest agent-repl-test-frontend-open-returns-before-the-ack ()
  "gui-open returns `:pending' with nothing mounted until establishment acks."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let (continuation mounted)
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws ok _fail) (setq continuation ok) :pending))
                ((symbol-function 'agent-repl--call-in-background-workspace)
                 (lambda (_ws fn) (funcall fn)))
                ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
                 (lambda (_ws _url) 'fake-buffer))
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (_ws _buf) (setq mounted t))))
        ;; Act — the command finishes here, with the daemon still working.
        (should (eq :pending (agent-repl--gui-open "ws1")))
        ;; Assert — no mount has happened yet.
        (should-not mounted)
        ;; Act — the ack arrives later, from a timer or a sentinel.
        (funcall continuation)
        ;; Assert
        (should mounted)))))

(ert-deftest agent-repl-test-frontend-show-returns-before-the-ack ()
  "gui-show returns `:pending' too; the wake gate is awaited, not waited on."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let (continuation shown)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws ok _fail) (setq continuation ok) :pending))
                ((symbol-function 'agent-repl--gui-open)
                 (lambda (_ws) (setq shown t) :pending)))
        ;; Act
        (should (eq :pending (agent-repl--gui-show "ws1")))
        (should-not shown)
        (funcall continuation)
        ;; Assert
        (should shown)))))

(ert-deftest agent-repl-test-frontend-open-timeout-surfaces-the-failure ()
  "An establishment timeout reaches the failure surface, mounting nothing.
The card the user reads is unchanged; only the waiting is gone."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let (fail warned mounted)
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws _ok on-failure) (setq fail on-failure) :pending))
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (&rest _) (setq mounted t)))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args)))))
        ;; Act — the command already returned; the deadline fires afterwards.
        (should (eq :pending (agent-repl--gui-open "ws1")))
        (funcall fail "timed out after 30.000s")
        ;; Assert
        (should (string-match-p "gui-open: FAILED" warned))
        (should (string-match-p "timed out after 30.000s" warned))
        (should-not mounted)))))

(ert-deftest agent-repl-test-frontend-show-timeout-surfaces-the-failure ()
  "gui-show's timeout warns exactly as it did when it blocked."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let (fail warned)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws _ok on-failure) (setq fail on-failure) :pending))
                ((symbol-function 'agent-repl--warn)
                 (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args)))))
        ;; Act
        (agent-repl--gui-show "ws1")
        (funcall fail "timed out after 30.000s")
        ;; Assert
        (should (string-match-p "gui-show: FAILED" warned))
        (should (string-match-p "timed out after 30.000s" warned))))))

(ert-deftest agent-repl-test-frontend-mount-holds-quit-off ()
  "The webview mount runs with quit inhibited, start to finish.
Creating the WKWebView, adopting it and binding it to the workspace are
one fact; a `C-g' between them leaks a live webview no workspace holds."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let (observed)
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (lambda (_url)
                   (setq observed inhibit-quit)
                   (generate-new-buffer "*fake-webview*")))
                ((symbol-function 'agent-repl--align-buffer-to-ws-dir) #'ignore))
        ;; Act
        (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/")
        ;; Assert
        (should observed)))))

(ert-deftest agent-repl-test-frontend-quit-mid-open-leaves-the-webview-registered ()
  "A quit after the mount finds the workspace already holding its webview.
An unregistered-but-live webview is invisible to `gui-kill', so it would
never be released and the next open would mount a second one beside it."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let (continuation)
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--frontend-after-ensure-session)
                 (lambda (_ws ok _fail) (setq continuation ok) :pending))
                ((symbol-function 'agent-repl--call-in-background-workspace)
                 (lambda (_ws fn) (funcall fn)))
                ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (lambda (_url) (generate-new-buffer "*fake-webview*")))
                ((symbol-function 'agent-repl--align-buffer-to-ws-dir) #'ignore)
                ;; The quit lands where a user's `C-g' realistically lands:
                ;; in the window work that follows the mount.
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (&rest _) (signal 'quit nil))))
        (agent-repl--gui-open "ws1")
        ;; Act
        (should-error (funcall continuation) :type 'quit)
        ;; Assert — the registry names the buffer that was created.
        (should (buffer-live-p (agent-repl--ws-get "ws1" :frontend-buffer)))))))

(ert-deftest agent-repl-test-frontend-open-leaves-the-heartbeat-armed ()
  "An open leaves the 1Hz heartbeat timer exactly as it found it.
The blocking open starved the timer queue for the length of the deploy,
which is what produced the `outcome=stranded' re-arms."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((agent-repl--timers nil)
          (agent-repl--keyed-timers nil)
          continuation)
      (unwind-protect
          (let ((heartbeat (agent-repl--register-timer
                            :state-poll (run-with-timer 3600 nil #'ignore))))
            (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                       (lambda () t))
                      ((symbol-function 'agent-repl--frontend-after-ensure-session)
                       (lambda (_ws ok _fail) (setq continuation ok) :pending))
                      ((symbol-function 'agent-repl--call-in-background-workspace)
                       (lambda (_ws fn) (funcall fn)))
                      ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
                       (lambda (_ws _url) 'fake-buffer))
                      ((symbol-function 'agent-repl--frontend-display-webview) #'ignore))
              ;; Act
              (agent-repl--gui-open "ws1")
              (funcall continuation)
              ;; Assert — same timer object, still scheduled.
              (should (agent-repl--timer-armed-p :state-poll))
              (should (eq heartbeat (cdr (assq :state-poll agent-repl--keyed-timers))))))
        (agent-repl--cancel-all-timers)))))
