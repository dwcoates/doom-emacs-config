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

;;;; The mount's quit deferral.
;;
;; The mount runs from the paced pre-creation drain, which the desktop focus
;; edge sets off — the same instant a keypress arrives — so a real `C-g' lands
;; in it often enough to have been observed.  Two claims: the quit does not
;; break the mount, and it is not lost either.

(ert-deftest agent-repl-test-frontend-ensure-webview-defers-a-quit-that-lands-in-it ()
  "A C-g arriving mid-mount is held for the command loop, not taken here."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (lambda (_url) (setq quit-flag t) buf))
                    ((symbol-function 'agent-repl--frontend-adopt-webview-buffer)
                     (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--frontend-watch-load)
                     (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--align-buffer-to-ws-dir)
                     (lambda (&rest _) nil)))
            ;; Act / Assert
            (should (agent-repl-test--quit-deferred-p
                      (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-test-frontend-ensure-webview-binds-the-buffer-despite-a-pending-quit ()
  "A quit already requested leaves no webview the registry does not name."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (lambda (_url) buf))
                    ((symbol-function 'agent-repl--frontend-adopt-webview-buffer)
                     (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--frontend-watch-load)
                     (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--align-buffer-to-ws-dir)
                     (lambda (&rest _) nil)))
            ;; Act
            (agent-repl-test--with-pending-quit
              (agent-repl--frontend-ensure-webview-buffer "ws1" "http://x/"))
            ;; Assert
            (should (eq (agent-repl--ws-get "ws1" :frontend-buffer) buf)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- webview URL ------------------------------------------------------------

;;;; ---- remount-webview (bundle reload) ----------------------------------------

;;;; ---- rescue-webview (navigated away) ----------------------------------------

(ert-deftest agent-repl-test-frontend-home-origin-is-the-owning-daemons-address ()
  "Home is the origin of the connection whose daemon owns the workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should (equal (agent-repl--frontend-home-origin "alpha")
                     "http://ws-ws-1.localhost:7777")))))

;; Page hosts: one per workspace, so pages never share a connection pool.

(ert-deftest agent-repl-test-frontend-page-host-is-the-workspace-id ()
  "A real (hex) workspace id names its own page host, readable as is."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "3e2d9cadc6794e13" :dir "/w") "127.0.0.1:61878"
      ;; Act / Assert
      (should (string-prefix-p "http://ws-3e2d9cadc6794e13.localhost:61878/"
                               (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-page-hosts-differ-per-workspace ()
  "Two workspaces on one daemon never share a page host."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (first second)
      (agent-repl-test-frontend--with-ref '(:id "aaaa" :dir "/a") "127.0.0.1:1"
        (setq first (agent-repl--frontend-home-origin "alpha")))
      (agent-repl-test-frontend--with-ref '(:id "bbbb" :dir "/b") "127.0.0.1:1"
        (setq second (agent-repl--frontend-home-origin "alpha")))
      ;; Act / Assert
      (should-not (equal first second)))))

(ert-deftest agent-repl-test-frontend-page-host-hashes-an-id-that-is-not-a-label ()
  "An id that is not a DNS label is hashed into one, never used raw."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "a b/c" :dir "/w") "127.0.0.1:1"
      ;; Act / Assert
      (should (string-match-p "\\`http://ws-[0-9a-f]\\{16\\}\\.localhost:1/"
                              (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-page-host-refuses-a-non-loopback-daemon ()
  "A localhost page host for a daemon that is not on loopback would address the wrong machine."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "10.0.0.5:7777"
      ;; Act / Assert
      (should-error (agent-repl-frontend-webview-url "alpha")))))

(ert-deftest agent-repl-test-frontend-home-origin-is-nil-without-a-ref ()
  "A workspace with no ref has no page host to be at home on."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'conn))
              ((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      ;; Act / Assert
      (should-not (agent-repl--frontend-home-origin "alpha")))))

(ert-deftest agent-repl-test-frontend-home-origin-is-nil-without-a-connection ()
  "A workspace whose daemon cannot be named has no home to be at."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil)))
      ;; Act / Assert
      (should-not (agent-repl--frontend-home-origin "alpha")))))

(ert-deftest agent-repl-test-frontend-at-home-accepts-the-daemons-own-page ()
  "The workspace's own webview URL is at home on the daemon that serves it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should (agent-repl--frontend-webview-at-home-p
               "alpha" (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-at-home-ignores-the-path-and-query ()
  "Home is the ORIGIN: the page rewrites its own query as the user navigates."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should (agent-repl--frontend-webview-at-home-p
               "alpha" "http://ws-ws-1.localhost:7777/other?workspace=someone-else")))))

(ert-deftest agent-repl-test-frontend-a-page-on-the-bare-daemon-address-is-not-home ()
  "A page still on the daemon's bare address shares the connection pool and must move."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should-not (agent-repl--frontend-webview-at-home-p
                   "alpha" "http://127.0.0.1:7777/?workspace=ws-1")))))

(ert-deftest agent-repl-test-frontend-at-home-refuses-another-daemons-port ()
  "A page served by a DIFFERENT daemon is astray, however similar its host."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should-not (agent-repl--frontend-webview-at-home-p
                   "alpha" "http://127.0.0.1:7778/?workspace=ws-1")))))

(ert-deftest agent-repl-test-frontend-at-home-refuses-a-page-that-left-the-web ()
  "`about:blank' — where an external hyperlink can strand a webview — is not home."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should-not (agent-repl--frontend-webview-at-home-p "alpha" "about:blank")))))

(ert-deftest agent-repl-test-frontend-at-home-refuses-a-webview-with-no-uri ()
  "A webview that cannot say where it is has no claim on being left alone."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:7777"
      ;; Act / Assert
      (should-not (agent-repl--frontend-webview-at-home-p "alpha" "")))))

(ert-deftest agent-repl-test-frontend-at-home-refuses-when-the-daemon-is-unknown ()
  "With no connection there is no origin to certify a page against."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil)))
      ;; Act / Assert
      (should-not (agent-repl--frontend-webview-at-home-p
                   "alpha" "http://127.0.0.1:7777/?workspace=ws-1")))))


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

(ert-deftest agent-repl-test-frontend-rescue-webview-errors-without-webview ()
  "The rescue signals when the workspace has no webview open at all."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
      ;; Act / Assert
      (should-error (agent-repl-frontend-rescue-webview) :type 'user-error))))

(ert-deftest agent-repl-test-frontend-rescue-webview-uri-probe-is-a-registered-boundary ()
  "The read-only URI probe is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-uri
                agent-repl--external-boundary-functions)))

;;;; ---- Copy chords ------------------------------------------------------------

;;;; ---- Snapping the feed to its newest message -------------------------------

;;;; ---- Closing the topbar dropdowns on an input-window click -----------------

;;;; ---- Adjusting the webview's text size -------------------------------------

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

(defvar agent-repl-test-frontend--told nil
  "Workspaces whose page was told its docked gate's height, newest first.")

(defun agent-repl-test-frontend--display-under (gated ws)
  "Mount WS's webview with GATED naming the workspaces a gate stands on.
Answers (VIEW-SHOWN INPUT-SHOWN SELECTED-IS-VIEW)."
  (let ((buf (generate-new-buffer "*fake-webview*"))
        (input-buf (generate-new-buffer (format "*agent-panel-input-%s*" ws))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                  ((symbol-function 'agent-repl--ensure-input-buffer) (lambda (_ws) input-buf))
                  ((symbol-function 'agent-repl-window--harden) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl-host-state)
                   (lambda (w) (and (member w gated) '(:gate (:arm :cold-gate :value nil)))))
                  ((symbol-function 'agent-repl-window-tell-gate-dock)
                   (lambda (w) (push w agent-repl-test-frontend--told) 100)))
          (agent-repl--frontend-display-webview ws buf)
          (list (and (get-buffer-window buf) t)
                (and (get-buffer-window input-buf) t)
                (eq (window-buffer (selected-window)) buf)))
      (delete-other-windows)
      (kill-buffer buf)
      (kill-buffer input-buf))))

(ert-deftest agent-repl-test-frontend-display-under-a-gate-mounts-no-input ()
  "A standing gate hides the input: the webview alone fills the main area
and is where focus lands, so the docked banner is the landing."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act / Assert
    (should (equal (agent-repl-test-frontend--display-under '("ws1") "ws1")
                   '(t nil t)))))

(ert-deftest agent-repl-test-frontend-display-under-a-gate-tells-the-page-the-input-height ()
  "Mounting under a gate tells the page the input's height for its dock."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((agent-repl-test-frontend--told nil))
      ;; Act
      (agent-repl-test-frontend--display-under '("ws1") "ws1")
      ;; Assert
      (should (equal agent-repl-test-frontend--told '("ws1"))))))

(ert-deftest agent-repl-test-frontend-switching-keeps-each-workspaces-own-layout ()
  "Switching while a gate stands on one workspace: the gated one mounts
without its input, the other with it."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (agent-repl--ws-put "ws2" :project-dir "/w2")
    ;; Act
    (let ((gated (agent-repl-test-frontend--display-under '("ws1") "ws1"))
          (open (agent-repl-test-frontend--display-under '("ws1") "ws2")))
      ;; Assert
      (should (equal gated '(t nil t)))
      (should (equal open '(t t nil))))))

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
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'featurep)
               (lambda (f &optional _sub) (not (eq f 'xwidget-internal)))))
      (should-not (agent-repl--frontend-xwidget-available-p))
      ;; Act / Assert
      (should-error (agent-repl-frontend-open-panel) :type 'user-error))))

(ert-deftest agent-repl-test-frontend-refused-open-panel-persists-no-frontend ()
  "A refused open leaves no durable frontend choice on the workspace.
Persisting ahead of the mount pinned a workspace to `gui' on a build
that can never show it — silently, and across restarts."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'featurep)
               (lambda (f &optional _sub) (not (eq f 'xwidget-internal)))))
      ;; Act
      (should-error (agent-repl-frontend-open-panel) :type 'user-error)
      ;; Assert
      (should (null (agent-repl--ws-get "ws1" :frontend)))
      (should (null (agent-repl--ws-get "ws1" :frontend-explicit))))))

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

(ert-deftest agent-repl-test-frontend-gui-boot-mounts-no-webview ()
  "The gui boot mounts nothing SYNCHRONOUSLY and touches no window.
A generated workspace is not the current one, so displaying its view
here would evict the caller's windows.  The page itself is now
pre-created, but only from the establishment continuation and only
without display (`agent-repl--frontend-precreate-webview')."
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

(ert-deftest agent-repl-test-frontend-kill-webview-suppresses-query-prompt ()
  "Webview kills bypass kill-buffer query functions.
The xwidget query fn raises a blocking yes-or-no prompt, which would
deadlock the non-interactive kill hook."
  ;; Arrange — a query fn that refuses every kill.
  (let ((buf (generate-new-buffer "*fake-webview*"))
        (kill-buffer-query-functions (list (lambda () nil))))
    ;; Act
    (agent-repl--frontend-kill-webview buf)
    ;; Assert — killed despite the refusing query fn.
    (should-not (buffer-live-p buf))))

(ert-deftest agent-repl-test-frontend-gui-running-p-holds-for-a-mounted-webview ()
  "A workspace holding a live webview reads as running, so the toggle SHOWS it."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            ;; Act / Assert
            (should (agent-repl--gui-running-p "ws1")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-gui-running-p-holds-for-a-hidden-webview ()
  "The plain close hides the panels and keeps the buffer, so the ws still runs.
This is the branch `SPC o c' toggles on: a second press must SHOW the page
back rather than mount a second one."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            (delete-other-windows)
            ;; Act — no window shows it, the buffer is alive.
            ;; Assert
            (should-not (get-buffer-window buf))
            (should (agent-repl--gui-running-p "ws1")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-gui-running-p-fails-without-a-webview ()
  "A workspace that was never opened has nothing to show, so the toggle OPENS."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act / Assert
    (should-not (agent-repl--gui-running-p "ws1"))))

(ert-deftest agent-repl-test-frontend-gui-running-p-fails-for-a-killed-webview ()
  "A webview that died leaves a dead buffer, and a dead buffer is not a page."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (kill-buffer buf)
      ;; Act / Assert
      (should-not (agent-repl--gui-running-p "ws1")))))

(ert-deftest agent-repl-test-frontend-gui-running-p-answers-a-boolean ()
  "The capability answers t or nil, never the buffer it looked at.
The registry's callers treat the answer as a predicate, and leaking the
buffer would make a truthy answer carry state no caller may rely on."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
            ;; Act / Assert
            (should (eq (agent-repl--gui-running-p "ws1") t)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-gui-registry-running-p-fn-is-defined ()
  "The gui registration's `:running-p-fn' names a function that EXISTS.
A registry slot pointing at a void symbol is a defect no unit test of the
function itself can catch: the toggle finds it only when a user presses
`SPC o c' on a workspace whose panels are hidden."
  ;; Act / Assert
  (should (fboundp (agent-repl-frontend-running-p-fn
                    (agent-repl-frontend-get 'gui)))))

(ert-deftest agent-repl-test-frontend-gui-registry-every-capability-is-defined ()
  "EVERY function-valued slot of the gui registration names a live function.
The registry is the one place a capability can name a symbol nothing
defines and no compiler complains: a `declare-function' satisfies the byte
compiler, and the void function surfaces only when a user reaches that
capability.  Deleting a source file (frontend-client.el) left three slots
in exactly that state, so this walks the struct rather than naming slots
one by one — a capability added later is covered the day it is added."
  ;; Arrange
  (let ((fe (agent-repl-frontend-get 'gui))
        (undefined nil))
    ;; Act
    (dolist (slot (cdr (cl-struct-slot-info 'agent-repl-frontend)))
      (let* ((name (car slot))
             (value (cl-struct-slot-value 'agent-repl-frontend name fe)))
        (when (and (string-suffix-p "-fn" (symbol-name name))
                   value
                   (not (functionp value)))
          (push name undefined))))
    ;; Assert
    (should (equal undefined nil))))

(ert-deftest agent-repl-test-frontend-gui-registry-declares-no-cancel-detached ()
  "The gui cannot stop detached work, and leaves the capability UNSET.
Stopping detached work is the `Interrupt' verb's `all_agents' target, and
`Interrupt' is a feed verb the webapp footer owns; Emacs calls no
interrupt rpc.  An unset slot makes the dispatch warn loudly instead of
sending something that could not reach the work."
  ;; Act / Assert
  (should-not (agent-repl-frontend-cancel-detached-fn
               (agent-repl-frontend-get 'gui))))

(ert-deftest agent-repl-test-frontend-gui-registry-declares-no-adopt-session ()
  "The gui cannot adopt a named vendor session, and leaves the slot UNSET.
No post-overhaul verb binds a workspace to a session uuid a client names:
the daemon owns session identity and resumes a workspace's own
conversation from its own record."
  ;; Act / Assert
  (should-not (agent-repl-frontend-adopt-session-fn
               (agent-repl-frontend-get 'gui))))

(ert-deftest agent-repl-test-frontend-gui-durable-session-id-is-the-vendor-id ()
  "The durable id is the vendor conversation's, read off the host stream."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl-host-vendor-session-id)
             (lambda (ws) (and (equal ws "ws1") "sess-uuid-1"))))
    ;; Act / Assert
    (should (equal (agent-repl--gui-durable-session-id "ws1") "sess-uuid-1"))))

(ert-deftest agent-repl-test-frontend-gui-durable-session-id-is-nil-without-a-conversation ()
  "No vendor conversation durably identifies nothing, which is an answer."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl-host-vendor-session-id)
             (lambda (_ws) nil)))
    ;; Act / Assert
    (should-not (agent-repl--gui-durable-session-id "ws1"))))

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
  "The display path saves :fullscreen-config only on a genuine open.
The workspace carries its panel buffers on the plist exactly as
production does — `--frontend-ensure-webview' records `:frontend-buffer'
before the display and `--ensure-input-buffer' records `:input-buffer'
on the way in — because that is how the re-show is RECOGNIZED as one:
the mount is a re-show when the frame holds nothing but this
workspace's own panels (`agent-repl--panels-cover-frame-p')."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                     (lambda () nil))
                    ((symbol-function 'agent-repl--ensure-input-buffer)
                     (lambda (ws)
                       (let ((input (get-buffer-create "*layout-input*")))
                         (agent-repl--ws-put ws :input-buffer input)
                         input)))
                    ((symbol-function 'agent-repl-window--harden)
                     (lambda (&rest _) nil)))
            (agent-repl--ws-put "ws1" :frontend-buffer buf)
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

(ert-deftest agent-repl-test-frontend-display-replaces-a-stale-layout ()
  "A remount over a dead mount saves the USER's layout, not the landing's.
`delete-other-windows' from the composer takes the webview window and
leaves the delete-protected composer alone, so the landing-era
`:fullscreen-config' outlives the mount it belongs to.  The next
display must therefore record the layout it is ACTUALLY covering — the
user's own work windows — so the close that follows restores those and
not the landing."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((view-buf    (generate-new-buffer "*fake-webview*"))
          (landing-buf (get-buffer-create "*stale-landing*"))
          (work-buf    (get-buffer-create "*stale-user-work*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p)
                     (lambda () nil))
                    ((symbol-function 'agent-repl--ensure-input-buffer)
                     (lambda (_ws) (get-buffer-create "*stale-input*")))
                    ((symbol-function 'agent-repl-window--harden)
                     (lambda (&rest _) nil)))
            (agent-repl--ws-put "ws1" :frontend-buffer view-buf)
            ;; The landing the panels first went up over.
            (delete-other-windows)
            (set-window-buffer (selected-window) landing-buf)
            (agent-repl--ws-put "ws1" :fullscreen-config
                                (current-window-configuration))
            ;; The frame the user has since built for themselves; the
            ;; webview holds no window, exactly as the sweep left it.
            (set-window-buffer (selected-window) work-buf)
            (should-not (get-buffer-window view-buf))
            ;; Act
            (agent-repl--frontend-display-webview "ws1" view-buf)
            ;; Assert — restoring what was saved brings the user's
            ;; window back, and the landing does not return.
            (set-window-configuration (agent-repl--ws-get "ws1" :fullscreen-config))
            (should (get-buffer-window work-buf))
            (should-not (get-buffer-window landing-buf)))
        (delete-other-windows)
        (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list view-buf landing-buf work-buf (get-buffer "*stale-input*")))))))

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

(ert-deftest agent-repl-test-frontend-webview-killed-on-ws-kill ()
  "The kill hook kills the webview so the WKWebView never outlives the ws."
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


;;;; ---- The webview URL -------------------------------------------------

(defmacro agent-repl-test-frontend--with-ref (ref address &rest body)
  "Run BODY with WS's ref answered by REF and its connection at ADDRESS.
A REAL connection object rather than a stubbed accessor: cl-defstruct
accessors are inlined into their callers at load time, so stubbing the
accessor would not reach the code under test."
  (declare (indent 2))
  `(let ((conn (agent-repl-connect-connection-create :address ,address))
         (process-environment
          (cons "AGENT_REPL_LOG_LEVEL" process-environment)))
     (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) ,ref))
               ((symbol-function 'agent-repl-host-conn) (lambda (_ws) conn)))
       ,@body)))

(ert-deftest agent-repl-test-frontend-url-is-the-daemon-address-and-the-ref ()
  "The URL is the owning daemon's address plus the ref's id and dir."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w/one") "127.0.0.1:7777"
      ;; Act / Assert
      (should (equal (agent-repl-frontend-webview-url "alpha")
                     "http://ws-ws-1.localhost:7777/?workspace=ws-1&dir=%2Fw%2Fone&log_level=info")))))

(ert-deftest agent-repl-test-frontend-url-hexifies-the-id ()
  "An opaque id is echoed verbatim, URL-encoded — never parsed or rebuilt."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "a b/c" :dir "/w") "127.0.0.1:1"
      ;; Act / Assert
      (should (string-match-p "workspace=a%20b%2Fc"
                              (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-url-hexifies-the-dir ()
  "The dir rides along encoded, for display and for opening files."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w/my ws") "127.0.0.1:1"
      ;; Act / Assert
      (should (string-match-p "dir=%2Fw%2Fmy%20ws"
                              (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-url-carries-no-composer-flag ()
  "The webapp runs composer-less unless `&composer=1', which is dev only."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:1"
      ;; Act / Assert
      (should-not (string-match-p "composer"
                                  (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-url-carries-the-configured-log-level ()
  "The page receives the daemon process's effective logging threshold."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:1"
      ;; Re-introduce the test's configured value after the helper removes
      ;; ambient host configuration for every ordinary URL case.
      (let ((process-environment
             (cons "AGENT_REPL_LOG_LEVEL=debug" process-environment)))
        ;; Act / Assert
        (should (string-match-p "log_level=debug"
                                (agent-repl-frontend-webview-url "alpha")))))))

(ert-deftest agent-repl-test-frontend-url-refuses-an-invalid-log-level ()
  "A misspelled threshold aborts before a webview can hide the defect."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:1"
      (let ((process-environment
             (cons "AGENT_REPL_LOG_LEVEL=trace" process-environment)))
      ;; Act / Assert
        (should-error (agent-repl-frontend-webview-url "alpha"))))))

(ert-deftest agent-repl-test-frontend-url-refuses-a-workspace-with-no-ref ()
  "A URL invented without a ref would address the wrong workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil))
              ((symbol-function 'agent-repl-host-conn) (lambda (_ws) 'conn)))
      ;; Act / Assert
      (should-error (agent-repl-frontend-webview-url "alpha")))))

(ert-deftest agent-repl-test-frontend-url-refuses-a-workspace-with-no-connection ()
  "During a handover a workspace's page must load from the daemon that owns it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl-host-ref)
               (lambda (_ws) '(:id "ws-1" :dir "/w")))
              ((symbol-function 'agent-repl-host-conn) (lambda (_ws) nil)))
      ;; Act / Assert
      (should-error (agent-repl-frontend-webview-url "alpha")))))

;;;; ---- Reloading the webview -------------------------------------------

(ert-deftest agent-repl-test-frontend-reload-navigates-the-live-widget ()
  "`reload_webapp' navigates the widget to the current URL."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((navigated nil)
          (buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "alpha" :frontend-buffer buf)
            (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:9"
              (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                         (lambda (_buf) 'widget))
                        ((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                         (lambda (_w uri) (setq navigated uri))))
                ;; Act
                (agent-repl-frontend-reload-webview "alpha")
                ;; Assert
                (should (equal navigated
                               "http://ws-ws-1.localhost:9/?workspace=ws-1&dir=%2Fw&log_level=info")))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-reload-does-not-remount-the-buffer ()
  "The webview is bound to its buffer for life: the widget is navigated."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "alpha" :frontend-buffer buf)
            (agent-repl-test-frontend--with-ref '(:id "ws-1" :dir "/w") "127.0.0.1:9"
              (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                         (lambda (_buf) 'widget))
                        ((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                         #'ignore))
                ;; Act
                (agent-repl-frontend-reload-webview "alpha")
                ;; Assert
                (should (eq (agent-repl--ws-get "alpha" :frontend-buffer) buf)))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-reload-without-a-webview-is-a-noop ()
  "A workspace with no page has nothing to reload."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert
    (should (null (agent-repl-frontend-reload-webview "alpha")))))

;;;; ---- Pre-creation eligibility ----------------------------------------

(ert-deftest agent-repl-test-frontend-precreate-refuses-a-workspace-with-no-ref ()
  "No ref means no URL to mount at, and a guessed one is not an option."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-live-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-gui-frontend-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      ;; Act / Assert
      (should (eq (agent-repl--frontend-precreate-refusal "alpha") :no-ref)))))

(ert-deftest agent-repl-test-frontend-precreate-refuses-an-already-mounted-workspace ()
  "Pre-creation is idempotent: every driver may call it freely."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "alpha" :frontend-buffer buf)
            (cl-letf (((symbol-function 'agent-repl--ws-live-p) (lambda (_ws) t))
                      ((symbol-function 'agent-repl--ws-gui-frontend-p) (lambda (_ws) t))
                      ((symbol-function 'agent-repl-host-ref)
                       (lambda (_ws) '(:id "ws-1" :dir "/w"))))
              ;; Act / Assert
              (should (eq (agent-repl--frontend-precreate-refusal "alpha")
                          :already-mounted))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-precreate-refuses-a-dead-workspace ()
  "A workspace that is gone gets no page."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-live-p) (lambda (_ws) nil)))
      ;; Act / Assert
      (should (eq (agent-repl--frontend-precreate-refusal "alpha") :not-live)))))

(ert-deftest agent-repl-test-frontend-precreate-accepts-an-eligible-workspace ()
  "A live gui workspace with a ref and no page is owed one."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-live-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-gui-frontend-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl-host-ref)
               (lambda (_ws) '(:id "ws-1" :dir "/w")))
              ((symbol-function 'agent-repl--frontend-xwidget-available-p)
               (lambda () t)))
      ;; Act / Assert
      (should (null (agent-repl--frontend-precreate-refusal "alpha"))))))

(ert-deftest agent-repl-test-frontend-precreate-mounts-without-waiting ()
  "There is no session to establish first: the mount waits on nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((mounted nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--frontend-precreate-mount)
                 (lambda (ws) (setq mounted ws))))
        ;; Act
        (should (eq (agent-repl--frontend-precreate-webview "alpha") :created))
        ;; Assert
        (should (equal mounted "alpha"))))))

;;;; ---- The load watcher ------------------------------------------------

(ert-deftest agent-repl-test-frontend-load-watcher-reports-the-load ()
  "A load-changed event advances the open-progress ladder to `:loaded'."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((noted nil)
          (props nil)
          (buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                     (lambda (_buf) 'widget))
                    ((symbol-function 'xwidget-get) (lambda (_w _p) nil))
                    ((symbol-function 'xwidget-put)
                     (lambda (_w _p v) (setq props v)))
                    ((symbol-function 'agent-repl-open-progress-note-loaded)
                     (lambda (ws) (setq noted ws))))
            (agent-repl--frontend-watch-load "alpha" buf)
            ;; Act
            (funcall props 'widget 'load-changed)
            ;; Assert
            (should (equal noted "alpha")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-a-load-tells-the-editor-startup-the-page-drew ()
  "The editor's startup opens a tab only once its page drew."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((noted nil)
          (props nil)
          (buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                     (lambda (_buf) 'widget))
                    ((symbol-function 'xwidget-get) (lambda (_w _p) nil))
                    ((symbol-function 'xwidget-put)
                     (lambda (_w _p v) (setq props v)))
                    ((symbol-function 'agent-repl-open-progress-note-loaded) #'ignore)
                    ((symbol-function 'agent-repl-startup-note-page-loaded)
                     (lambda (ws) (setq noted ws))))
            (agent-repl--frontend-watch-load "alpha" buf)
            ;; Act
            (funcall props 'widget 'load-changed)
            ;; Assert
            (should (equal noted "alpha")))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-load-watcher-defers-to-the-prior-callback ()
  "The webkit machinery still gets every event it needs."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((prior-called nil)
          (props nil)
          (buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                     (lambda (_buf) 'widget))
                    ((symbol-function 'xwidget-get)
                     (lambda (_w _p) (lambda (_w _e) (setq prior-called t))))
                    ((symbol-function 'xwidget-put)
                     (lambda (_w _p v) (setq props v)))
                    ((symbol-function 'agent-repl-open-progress-note-loaded) #'ignore))
            (agent-repl--frontend-watch-load "alpha" buf)
            ;; Act
            (funcall props 'widget 'load-changed)
            ;; Assert
            (should prior-called))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-load-watcher-ignores-other-events ()
  "Only a finished load is a `:loaded' report."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((noted nil)
          (props nil)
          (buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                     (lambda (_buf) 'widget))
                    ((symbol-function 'xwidget-get) (lambda (_w _p) nil))
                    ((symbol-function 'xwidget-put)
                     (lambda (_w _p v) (setq props v)))
                    ((symbol-function 'agent-repl-open-progress-note-loaded)
                     (lambda (ws) (setq noted ws))))
            (agent-repl--frontend-watch-load "alpha" buf)
            ;; Act
            (funcall props 'widget 'download-callback)
            ;; Assert
            (should (null noted)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-test-frontend-load-watcher-reports-on-the-info-rung ()
  "The load report survives the default `info' log level.
It is the ONE marker every painted panel leaves, whatever brought the
workspace into being; on the debug rung it never reached disk, and
realtest 5 reported a created workspace that had painted as one that
never did."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((rungs nil)
          (props nil)
          (buf (generate-new-buffer "*fake-webview*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                     (lambda (_buf) 'widget))
                    ((symbol-function 'xwidget-get) (lambda (_w _p) nil))
                    ((symbol-function 'xwidget-put)
                     (lambda (_w _p v) (setq props v)))
                    ((symbol-function 'agent-repl-open-progress-note-loaded) #'ignore)
                    ((symbol-function 'agent-repl--info)
                     (lambda (_ws fmt &rest _args)
                       (when (string-match-p "load-changed" fmt) (push :info rungs))))
                    ((symbol-function 'agent-repl--log)
                     (lambda (_ws fmt &rest _args)
                       (when (string-match-p "load-changed" fmt) (push :debug rungs)))))
            (agent-repl--frontend-watch-load "alpha" buf)
            ;; Act
            (funcall props 'widget 'load-changed)
            ;; Assert
            (should (equal '(:info) rungs)))
        (kill-buffer buf)))))

;;;; ---- No JavaScript surface remains -----------------------------------

(ert-deftest agent-repl-test-frontend-defines-no-script-evaluator ()
  "Every `xwidget-webkit-execute-script' call and script helper is deleted:
there is no `window.agentRepl*' hook surface at all, and the webview is
purely daemon-driven."
  ;; Act / Assert
  (should-not (fboundp 'agent-repl--frontend-webview-execute-script)))

(provide 'test-frontend)

;;; test-frontend.el ends here

;;;; ---- Chess-board keyboard navigation ---------------------------------------

;;;; ---- Refreshing live webviews -----------------------------------------

(defmacro agent-repl-test--with-webview-buffers (names &rest body)
  "Create a buffer per name in NAMES for BODY, killing them afterwards."
  (declare (indent 1))
  `(let ((agent-repl-test--bufs (mapcar #'get-buffer-create ,names)))
     (unwind-protect (progn ,@body)
       (dolist (b agent-repl-test--bufs)
         (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest agent-repl-test-frontend-webview-live-widget-is-a-registered-boundary ()
  "The live-widget probe is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-live-widget
                agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-test-frontend-webview-reload-is-a-registered-boundary ()
  "The reload wrapper is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-reload-widget
                agent-repl--external-boundary-functions)))

;;;; ---- The webview READ channel, and its crash invariants -------------------

;; WHY THESE ARE CRASH TESTS AND NOT STYLE TESTS: see the docstring of
;; `agent-repl--frontend-webview-execute-script-value'.  The NS port captures
;; the callback into a GC-invisible Objective-C block, so a non-symbol
;; callback is a use-after-free waiting on WebKit's reply, and a widget
;; resolved through the session fallback can be a dead or foreign one.

(ert-deftest agent-repl-test-webview-read-channel-is-a-registered-boundary ()
  "The webview read channel is registered as an external boundary wrapper."
  (should (memq 'agent-repl--frontend-webview-execute-script-value
                agent-repl--external-boundary-functions)))

(ert-deftest agent-repl-test-webview-read-channel-rejects-a-closure-callback ()
  "A freshly-consed closure is refused: the NS port cannot keep it alive."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (let ((buf (get-buffer "*agent-frontend-ws1*")))
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (_buf) 'live-widget)))
        ;; Act / Assert
        (should-error (agent-repl--frontend-webview-read-script
                       buf "1" (lambda (_raw) nil))
                      :type 'error)))))

(ert-deftest agent-repl-test-webview-read-channel-rejects-a-nil-callback ()
  "A nil callback is refused rather than silently injecting a write."
  ;; Arrange
  (agent-repl-test--with-webview-buffers '("*agent-frontend-ws1*")
    (let ((buf (get-buffer "*agent-frontend-ws1*")))
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (_buf) 'live-widget)))
        ;; Act / Assert
        (should-error (agent-repl--frontend-webview-read-script buf "1" nil)
                      :type 'error)))))

;;;; ---- The webview's external boundaries ------------------------------------

(ert-deftest agent-repl-test-frontend-webview-wrappers-are-registered-boundaries ()
  "Every webview wrapper frontend.el defines is a registered boundary."
  ;; Arrange
  (let ((wrappers '(agent-repl--frontend-make-webview-buffer
                    agent-repl--frontend-webview-execute-script-value
                    agent-repl--frontend-webview-live-widget
                    agent-repl--frontend-webview-reload-widget
                    agent-repl--frontend-webview-navigate-widget
                    agent-repl--frontend-webview-uri)))
    ;; Act / Assert
    (should (equal (seq-remove (lambda (w) (memq w agent-repl--external-boundary-functions))
                               wrappers)
                   nil))))

;;;; ---- snap-webview-to-tail skip-record routing ----

;;;; ---- The open path never holds the main thread ---------------------------
;;
;; `gui-open' and `gui-show' used to complete while the caller waited: the
;; lazy daemon ensure ran a whole-stack deploy through `call-process', so
;; the editor was frozen for the length of a Go/npm build and the only
;; escape was `C-g'.  Every test below pins one half of the replacement —
;; the command returns at once, the outcome (mount OR failure) arrives from
;; a continuation, and nothing the open touches is left half-written when a
;; quit lands.

;;;; ---- the restart/rebuild rendezvous ------------------------------------

(ert-deftest agent-repl-test-rendezvous-rejects-a-non-positive-party-count ()
  "A rendezvous of nobody is a caller bug and fails hard rather than firing."
  ;; Act / Assert
  (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
    (should-error (agent-repl--frontend-make-rendezvous 0 #'ignore) :type 'error)))

;;;; ---- the rebuild's success routing: gate vs direct bounce --------------

;;;; ---- Open-placeholder resolution -----------------------------------------
;;
;; The placeholder `SPC o c' raises must be resolved by the open it describes,
;; on every path.  These tests pin gui-open's and gui-show's half of that
;; contract: the ladder advances while establishment runs, teardown follows a
;; real mount, and a failure REPLACES the placeholder with the stated cause
;; rather than leaving it spinning forever.

(defmacro agent-repl-test--with-pending-open (ws &rest body)
  "Run BODY with a placeholder standing for WS over a private registry."
  (declare (indent 1))
  `(let ((agent-repl--open-progress (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'agent-repl--open-progress-show)
                (lambda (_ws buf) buf)))
       (unwind-protect
           (progn (agent-repl--open-progress-start ,ws) ,@body)
         (when-let ((entry (agent-repl--open-progress-entry ,ws)))
           (when-let ((timer (plist-get entry :timer)))
             (when (timerp timer) (cancel-timer timer)))
           (when (buffer-live-p (plist-get entry :buffer))
             (kill-buffer (plist-get entry :buffer))))))))

;;;; ---- Non-displaying pre-creation -------------------------------------------

(defvar agent-repl-test--precreate-urls nil
  "URLs the mocked webview factory was asked to mount, in call order.")

(defmacro agent-repl-test--with-precreate-boundaries (displayed &rest body)
  "Run BODY with the mount wired to a fake webview and display recorded.
DISPLAYED is bound to the flag the display path would set — a
pre-creation that ever reaches it is the defect these tests cover."
  (declare (indent 1))
  `(let ((,displayed nil)
         (agent-repl-test--precreate-urls nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
               ((symbol-function 'agent-repl--frontend-after-ensure-session)
                (lambda (_ws ok _fail &rest _) (funcall ok) :pending))
               ((symbol-function 'agent-repl--call-in-background-workspace)
                (lambda (_ws fn) (funcall fn)))
               ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                (agent-repl-test--fake-webview-factory 'agent-repl-test--precreate-urls))
               ((symbol-function 'agent-repl--frontend-display-webview)
                (lambda (_ws _buf) (setq ,displayed t))))
       ,@body)))

(ert-deftest agent-repl-test-frontend-precreate-never-displays ()
  "Pre-creation never reaches the display path."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend gui)
    (agent-repl-test--with-precreate-boundaries displayed
      ;; Act
      (agent-repl--frontend-precreate-webview "ws1")
      ;; Assert
      (should-not displayed))))

(ert-deftest agent-repl-test-frontend-precreate-leaves-the-window-configuration ()
  "Pre-creation leaves the frame's window configuration untouched."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend gui)
    (agent-repl-test--with-precreate-boundaries _displayed
      (let ((before (current-window-configuration)))
        ;; Act
        (agent-repl--frontend-precreate-webview "ws1")
        ;; Assert
        (should (compare-window-configurations before (current-window-configuration)))))))

(ert-deftest agent-repl-test-frontend-precreate-skips-an-already-mounted-workspace ()
  "A workspace already holding a live webview mounts nothing further."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend gui)
    (agent-repl-test--with-precreate-boundaries _displayed
      (agent-repl--ws-put "ws1" :frontend-buffer (generate-new-buffer "*pre-ws1*"))
      ;; Act
      (agent-repl--frontend-precreate-webview "ws1")
      ;; Assert
      (should (null agent-repl-test--precreate-urls)))))

(ert-deftest agent-repl-test-frontend-precreate-skips-a-fenced-workspace ()
  "A terminally fenced workspace is not given a page."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend gui :open-fenced t)
    (agent-repl-test--with-precreate-boundaries _displayed
      ;; Act
      (agent-repl--frontend-precreate-webview "ws1")
      ;; Assert
      (should (null agent-repl-test--precreate-urls)))))

(ert-deftest agent-repl-test-frontend-precreate-skips-a-killed-workspace ()
  "A tombstoned workspace is not given a page."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend gui :killed-at 1)
    (agent-repl-test--with-precreate-boundaries _displayed
      ;; Act
      (agent-repl--frontend-precreate-webview "ws1")
      ;; Assert
      (should (null agent-repl-test--precreate-urls)))))

(ert-deftest agent-repl-test-frontend-precreate-skips-without-xwidget-support ()
  "An Emacs with no xwidget support is refused quietly rather than erroring."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend gui)
    (agent-repl-test--with-precreate-boundaries _displayed
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () nil)))
        ;; Act + Assert
        (should (null (agent-repl--frontend-precreate-webview "ws1")))))))

;;;; ---- Pre-creating a SNAPSHOT-RESTORED workspace -------------------------

;; The fixture below is the shape a startup restore actually leaves behind
;; (`agent-repl--establish-workspace' + `agent-repl--initialize-ws-env'):
;; `:project-dir' and a cleared `:killed-at', the hydrated env, and the
;; display/priority state read back off the project's state.el.  There is NO
;; `:frontend' key — only a DELIBERATE choice is persisted, so a restored
;; workspace resolves its presentation from the default — and no `:type' key,
;; which the registry has never carried at all.  Idealizing the entry with an
;; explicit `:frontend gui' is what hid this bug.
(defconst agent-repl-test--restored-ws-plist
  '(:project-dir "/w/feed-tail" :killed-at nil :active-env :bare-metal
    :repl-state :idle :priority 3 :worktree-p t :source-ws-dir "/w/parent")
  "The registry entry a snapshot-restored gui workspace comes back as.")

(ert-deftest agent-repl-test-frontend-precreate-refuses-a-fenced-restored-workspace ()
  "A restored entry the daemon fenced is still refused a page."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "wsr"
      (append '(:open-fenced t) agent-repl-test--restored-ws-plist)
    (agent-repl-test--with-precreate-boundaries _displayed
      ;; Act
      (should (null (agent-repl--frontend-precreate-webview "wsr")))
      ;; Assert
      (should (null agent-repl-test--precreate-urls)))))


;;;; ---- Adopting a mounted webview ----

(ert-deftest agent-repl-test-frontend-adopt-webview-buffer-completes ()
  "Adoption runs to completion and hands the buffer back.
Every mount site funnels through `agent-repl--frontend-adopt-webview-buffer',
so a single stale call inside it to a command deleted with its feature
takes down EVERY webview mount with a void-function — the mount is the
one place where a decoration failing may not cost the user a page."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-adopt*")))
    (unwind-protect
        ;; Act
        (let ((adopted (agent-repl--frontend-adopt-webview-buffer
                        buf "*agent-repl-test-adopted*" "ws-1")))
          ;; Assert
          (should (eq adopted buf)))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-adopt-webview-buffer-stamps-the-owner ()
  "Adoption records the OWNER every owner-keyed predicate reads."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-adopt-owner*")))
    (unwind-protect
        (progn
          ;; Act
          (agent-repl--frontend-adopt-webview-buffer
           buf "*agent-repl-test-adopted-owner*" "ws-1")
          ;; Assert
          (should (equal (buffer-local-value 'agent-repl--owning-workspace buf)
                         "ws-1")))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-adopt-hook-failure-uses-the-explicit-owner ()
  "A failed webview decoration is attributed to the adopted workspace."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-adopt-log-owner*"))
        (agent-repl-frontend-webview-adopt-hook
         (list (lambda () (error "decoration failed"))))
        logged-workspace)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--warn)
                   (lambda (ws _fmt &rest _args)
                     (setq logged-workspace ws))))
          ;; Act
          (agent-repl--frontend-adopt-webview-buffer
           buf "*agent-repl-test-adopted-log-owner*" "owner-ws")
          ;; Assert
          (should (equal logged-workspace "owner-ws")))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-adopt-webview-buffer-clears-the-header-line ()
  "Adoption clears `xwidget-webkit-mode's header line: a webview is a
panel, not a browser."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-adopt-header*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (setq-local header-line-format "WebKit: x"))
          ;; Act
          (agent-repl--frontend-adopt-webview-buffer
           buf "*agent-repl-test-adopted-header*" "ws-1")
          ;; Assert
          (should (null (buffer-local-value 'header-line-format buf))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-adopt-remaps-stock-history-reload ()
  "Adoption remaps the broken stock `xwidget-webkit-history-reload' to ours.
The stock command calls `xwidget-webkit-back-forward-list', void in this
Emacs build, so an adopted panel routes the reload keystroke to
`agent-repl-frontend-reload-webview' instead."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-adopt-remap-hist*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (use-local-map (make-sparse-keymap)))
          ;; Act
          (agent-repl--frontend-adopt-webview-buffer
           buf "*agent-repl-test-adopted-remap-hist*" "ws-1")
          ;; Assert
          (with-current-buffer buf
            (should (eq (command-remapping 'xwidget-webkit-history-reload)
                        #'agent-repl-frontend-reload-webview))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-adopt-remaps-stock-reload ()
  "Adoption also remaps the sibling stock `xwidget-webkit-reload' to ours."
  ;; Arrange
  (let ((buf (generate-new-buffer " *agent-repl-test-adopt-remap-reload*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (use-local-map (make-sparse-keymap)))
          ;; Act
          (agent-repl--frontend-adopt-webview-buffer
           buf "*agent-repl-test-adopted-remap-reload*" "ws-1")
          ;; Assert
          (with-current-buffer buf
            (should (eq (command-remapping 'xwidget-webkit-reload)
                        #'agent-repl-frontend-reload-webview))))
      (kill-buffer buf))))

;;;; ---- The composer mounts at a fixed height -------------------------------
;;
;; The owner's report: the composer is not a consistent height —
;; different workspaces show different heights, and one workspace's
;; height changes.  Both were the same bug: the mount split its host
;; window by a fraction of THAT window's height at THAT instant.  The
;; height now comes from `agent-repl-window--input-height', derived once
;; per frame, so every mount asks for the same number of lines.

(defmacro agent-repl-test-frontend--mounting (&rest body)
  "Run BODY with the display path's non-window collaborators stubbed out."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
             ((symbol-function 'agent-repl-window--harden) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest agent-repl-test-frontend-composer-mounts-at-the-declared-height ()
  "The composer is created at the frame's declared composer height."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview-h1*"))
          (input-buf (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (agent-repl-test-frontend--mounting
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input-buf)))
              ;; Act
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert
              (should (= (window-total-height (get-buffer-window input-buf))
                         (agent-repl-window--input-height)))))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-frontend-composer-height-matches-across-workspaces ()
  "Two workspaces mounted on the same frame get the SAME composer height."
  (let ((buf1 (generate-new-buffer "*fake-webview-h2a*"))
        (buf2 (generate-new-buffer "*fake-webview-h2b*"))
        (input1 (generate-new-buffer "*agent-panel-input-ws1*"))
        (input2 (generate-new-buffer "*agent-panel-input-ws2*"))
        (first nil))
    (unwind-protect
        (agent-repl-test-frontend--mounting
          ;; Arrange / Act — ws1 mounts, then ws2 over it.
          (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input1)))
              (agent-repl--frontend-display-webview "ws1" buf1)
              (setq first (window-total-height (get-buffer-window input1)))))
          (agent-repl-test--with-frontend-ws "ws2" '(:project-dir "/w2")
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input2)))
              (agent-repl--frontend-display-webview "ws2" buf2)
              ;; Assert
              (should (= (window-total-height (get-buffer-window input2))
                         first)))))
      (delete-other-windows)
      (dolist (b (list buf1 buf2 input1 input2))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest agent-repl-test-frontend-composer-height-survives-a-remount ()
  "A remount of the same workspace lands on the same composer height.
`agent-repl-window--ensure-layout' repairs a broken pair by re-running
this very display path, which is how one workspace's composer used to
change height under the user."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview-h3*"))
          (input-buf (generate-new-buffer "*agent-panel-input-ws1*")))
      (unwind-protect
          (agent-repl-test-frontend--mounting
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input-buf)))
              ;; Arrange
              (agent-repl--frontend-display-webview "ws1" buf)
              (let ((before (window-total-height (get-buffer-window input-buf))))
                ;; Act — the repair path's remount.
                (agent-repl--frontend-display-webview "ws1" buf)
                ;; Assert
                (should (= (window-total-height (get-buffer-window input-buf))
                           before)))))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-frontend-composer-height-ignores-a-shrunken-main-area ()
  "A side window eating the frame's main area does not shrink the composer.
The old fraction was taken from the host window, so a workspace mounted
while a bottom side window stood came out shorter than its siblings."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview-h4*"))
          (input-buf (generate-new-buffer "*agent-panel-input-ws1*"))
          (side-buf (generate-new-buffer "*fake-side-h4*"))
          (side nil))
      (unwind-protect
          (agent-repl-test-frontend--mounting
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input-buf)))
              ;; Arrange — mount clean, then again under a side window.
              (agent-repl--frontend-display-webview "ws1" buf)
              (let ((clean (window-total-height (get-buffer-window input-buf))))
                (setq side (display-buffer-in-side-window
                            side-buf '((side . bottom) (slot . 0))))
                ;; Act
                (agent-repl--frontend-display-webview "ws1" buf)
                ;; Assert
                (should (= (window-total-height (get-buffer-window input-buf))
                           clean)))))
        (when (window-live-p side) (delete-window side))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer input-buf)
        (kill-buffer side-buf)))))

(ert-deftest agent-repl-test-frontend-composer-height-survives-a-restore ()
  "A RESTORED workspace — composer buffer already alive and full of text —
mounts at the same height as a freshly created one.  Nothing about the
composer's content may reach its height."
  (let ((buf1 (generate-new-buffer "*fake-webview-h5a*"))
        (buf2 (generate-new-buffer "*fake-webview-h5b*"))
        (fresh (generate-new-buffer "*agent-panel-input-ws1*"))
        (restored (generate-new-buffer "*agent-panel-input-ws2*"))
        (created nil))
    (unwind-protect
        (agent-repl-test-frontend--mounting
          ;; Arrange — ws1 is created; ws2's composer pre-exists with content.
          (with-current-buffer restored
            (insert (mapconcat #'identity (make-list 40 "carried-over text") "\n")))
          (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) fresh)))
              (agent-repl--frontend-display-webview "ws1" buf1)
              (setq created (window-total-height (get-buffer-window fresh)))))
          (agent-repl-test--with-frontend-ws "ws2" '(:project-dir "/w2")
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) restored)))
              ;; Act
              (agent-repl--frontend-display-webview "ws2" buf2)
              ;; Assert
              (should (= (window-total-height (get-buffer-window restored))
                         created)))))
      (delete-other-windows)
      (dolist (b (list buf1 buf2 fresh restored))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest agent-repl-test-frontend-composer-mount-records-its-height ()
  "The mount record carries the height the composer actually got."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview-h6*"))
          (input-buf (generate-new-buffer "*agent-panel-input-ws1*"))
          (records nil))
      (unwind-protect
          (agent-repl-test-frontend--mounting
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input-buf))
                      ((symbol-function 'agent-repl--log)
                       (lambda (_ws fmt &rest args)
                         (push (apply #'format fmt args) records))))
              ;; Act
              (agent-repl--frontend-display-webview "ws1" buf)
              ;; Assert
              (should (cl-find-if
                       (lambda (m)
                         (string-match-p "display-webview: mounted .*input-height=[0-9]+ target-height=[0-9]+" m))
                       records))))
        (delete-other-windows)
        (kill-buffer buf)
        (kill-buffer input-buf)))))

(ert-deftest agent-repl-test-frontend-composer-height-matches-across-differing-frame-states ()
  "Two workspaces mounted with DIFFERENT main areas still agree on the height.
This is the owner's second symptom directly: one workspace built while
the frame carried something extra came out a different height from its
sibling built on a clean frame."
  (let ((buf1 (generate-new-buffer "*fake-webview-h7a*"))
        (buf2 (generate-new-buffer "*fake-webview-h7b*"))
        (input1 (generate-new-buffer "*agent-panel-input-ws1*"))
        (input2 (generate-new-buffer "*agent-panel-input-ws2*"))
        (side-buf (generate-new-buffer "*fake-side-h7*"))
        (side nil)
        (clean nil))
    (unwind-protect
        (agent-repl-test-frontend--mounting
          ;; Arrange — ws1 on a clean frame.
          (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input1)))
              (agent-repl--frontend-display-webview "ws1" buf1)
              (setq clean (window-total-height (get-buffer-window input1)))))
          ;; Act — ws2 while a side window holds part of the main area.
          (setq side (display-buffer-in-side-window
                      side-buf '((side . bottom) (slot . 0))))
          (agent-repl-test--with-frontend-ws "ws2" '(:project-dir "/w2")
            (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                       (lambda (_ws) input2)))
              (agent-repl--frontend-display-webview "ws2" buf2)
              ;; Assert
              (should (= (window-total-height (get-buffer-window input2))
                         clean)))))
      (when (window-live-p side) (delete-window side))
      (delete-other-windows)
      (dolist (b (list buf1 buf2 input1 input2 side-buf))
        (when (buffer-live-p b) (kill-buffer b))))))

;;;; ---- Copying the webview's highlighted text -------------------------------

(defvar agent-repl-test--copy-ran nil
  "The keys the fall-through test commands ran under, newest first.")

(defun agent-repl-test--copy-record-y ()
  "Stand in for whatever `y' did before the copy chords."
  (interactive)
  (push 'y agent-repl-test--copy-ran))

(defun agent-repl-test--copy-record-c-c-c-k ()
  "Stand in for a `C-c C-k' binding behind the `C-c' prefix."
  (interactive)
  (push 'c-c-c-k agent-repl-test--copy-ran))

(defmacro agent-repl-test--with-copy-buffer (&rest body)
  "Run BODY in a webview-like buffer with the copy chords on.
Its local map binds `y' and `C-c C-k' to recording commands, standing
in for the keys' behavior before the chords existed."
  (declare (indent 0))
  `(let ((agent-repl-test--copy-ran nil)
         (unread-command-events nil)
         (kill-ring nil))
     (with-temp-buffer
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "y") #'agent-repl-test--copy-record-y)
         (define-key map (kbd "C-c C-k") #'agent-repl-test--copy-record-c-c-c-k)
         (use-local-map map))
       (agent-repl-frontend-webview-mode 1)
       ,@body)))

(defun agent-repl-test--copy-answer (text focused)
  "The JSON the page answers the request being waited on with."
  (json-encode `((id . ,agent-repl--frontend-copy-waiting)
                 (text . ,text)
                 (focused . ,(if focused t :json-false)))))

(defmacro agent-repl-test--answering (text focused &rest body)
  "Run BODY with the page answering TEXT and FOCUSED at once.
The read channel calls the callback symbol synchronously, as the
xwidget event handler would once the answer arrives."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
              (lambda (_buf) 'live-widget))
             ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
              (lambda (_xw _script callback)
                (funcall callback (agent-repl-test--copy-answer ,text ,focused)))))
     ,@body))

(defun agent-repl-test--press (keys)
  "Run the copy command as though KEYS (a `kbd' string) invoked it."
  (cl-letf (((symbol-function 'this-command-keys-vector)
             (lambda () (kbd keys))))
    (agent-repl-frontend-copy-selection)))

(ert-deftest agent-repl-test-copy-keys-share-one-command ()
  "`y', `C-c' and `s-c' are all bound to the ONE copy command."
  ;; Arrange / Act
  (let ((bound (mapcar (lambda (key)
                         (lookup-key agent-repl-frontend-webview-mode-map (kbd key)))
                       '("y" "C-c" "s-c"))))
    ;; Assert
    (should (equal bound (make-list 3 #'agent-repl-frontend-copy-selection)))))

(ert-deftest agent-repl-test-copy-keys-share-one-command-in-every-evil-state ()
  "Every Evil state map gets the same three keys, all on the one command."
  ;; Arrange
  (let ((planted nil))
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state _map key def)
                 (push (list state (key-description key) def) planted))))
      ;; Act
      (agent-repl--frontend-copy-bind-evil-keys))
    ;; Assert
    (should (equal (seq-uniq (mapcar #'caddr planted))
                   (list #'agent-repl-frontend-copy-selection)))
    (should (= (length planted) (* 5 3)))))

(ert-deftest agent-repl-test-the-webview-zooms-the-feed-too ()
  "The webview buffer's map carries the zoom chords, as the composer's does."
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "C-+"))
              #'agent-repl-feed-text-scale-increase))
  (should (eq (lookup-key agent-repl-frontend-webview-mode-map (kbd "C--"))
              #'agent-repl-feed-text-scale-decrease)))

(ert-deftest agent-repl-test-copy-mode-arms-on-every-adopted-webview ()
  "The adoption hook enables the copy chords."
  (should (memq #'agent-repl-frontend-webview-mode
                agent-repl-frontend-webview-adopt-hook)))

(ert-deftest agent-repl-test-copy-mode-normalizes-evil-keymaps ()
  "Enabling the mode rebuilds Evil's keymaps, or its state maps still win."
  ;; Arrange
  (let ((normalized 0))
    (cl-letf (((symbol-function 'evil-normalize-keymaps)
               (lambda (&optional _state) (cl-incf normalized))))
      (with-temp-buffer
        ;; Act
        (agent-repl-frontend-webview-mode 1)
        ;; Assert
        (should (= normalized 1))))))

(ert-deftest agent-repl-test-copy-nonempty-focused-selection-is-killed ()
  "A nonempty selection on a focused page goes onto the kill ring."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "boot_failed" t
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should (equal (current-kill 0 t) "boot_failed"))))

(ert-deftest agent-repl-test-copy-nonempty-selection-skips-the-old-binding ()
  "A copy does not also run the key's old binding."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "boot_failed" t
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should (null agent-repl-test--copy-ran))))

(ert-deftest agent-repl-test-copy-command-c-copies-like-y ()
  "Command-c copies through the same path as `y'."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "frame unreadable" t
      ;; Act
      (agent-repl-test--press "s-c"))
    ;; Assert
    (should (equal (current-kill 0 t) "frame unreadable"))))

(ert-deftest agent-repl-test-copy-empty-selection-runs-the-old-binding ()
  "With nothing selected, `y' does what it did before the chords."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "" t
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should (equal agent-repl-test--copy-ran '(y)))))

(ert-deftest agent-repl-test-copy-empty-selection-kills-nothing ()
  "With nothing selected, the kill ring is left alone."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "" t
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should (null kill-ring))))

(ert-deftest agent-repl-test-copy-unfocused-page-runs-the-old-binding ()
  "A selection on an unfocused page falls through to the old binding."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "boot_failed" nil
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should (equal (list agent-repl-test--copy-ran kill-ring) '((y) nil)))))

(ert-deftest agent-repl-test-copy-no-live-webview-runs-the-old-binding ()
  "A buffer with no live page to ask falls through to the old binding."
  (agent-repl-test--with-copy-buffer
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
               (lambda (_buf) nil)))
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should (equal agent-repl-test--copy-ran '(y)))))

(ert-deftest agent-repl-test-copy-c-c-falls-through-as-a-prefix ()
  "With nothing selected, `C-c' stays the prefix it was: `C-c C-k' still runs."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "" t
      (setq unread-command-events (listify-key-sequence (kbd "C-k")))
      ;; Act
      (agent-repl-test--press "C-c"))
    ;; Assert
    (should (equal agent-repl-test--copy-ran '(c-c-c-k)))))

(ert-deftest agent-repl-test-copy-replays-keys-typed-while-waiting ()
  "Keys read while the page answers are replayed after the copy, in order."
  (agent-repl-test--with-copy-buffer
    (let ((script '(?a ?b answer)))
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (_buf) 'live-widget))
                ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
                 #'ignore)
                ((symbol-function 'agent-repl--frontend-read-event)
                 (lambda ()
                   (let ((next (pop script)))
                     (if (not (eq next 'answer))
                         next
                       (agent-repl--frontend-copy-answered
                        (agent-repl-test--copy-answer "x" t))
                       (pop unread-command-events))))))
        ;; Act
        (agent-repl-test--press "y")))
    ;; Assert
    (should (equal unread-command-events '(?a ?b)))))

(ert-deftest agent-repl-test-copy-c-g-while-waiting-quits ()
  "`C-g' read while waiting abandons the wait with a quit."
  (agent-repl-test--with-copy-buffer
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
               (lambda (_buf) 'live-widget))
              ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
               #'ignore)
              ((symbol-function 'agent-repl--frontend-read-event)
               (lambda () ?\C-g)))
      ;; Act
      (let ((outcome (condition-case nil
                         (progn (agent-repl-test--press "y") 'returned)
                       (quit 'quit))))
        ;; Assert
        (should (eq outcome 'quit))))))

(ert-deftest agent-repl-test-copy-c-g-while-waiting-clears-the-wait ()
  "An abandoned wait leaves no request standing."
  (agent-repl-test--with-copy-buffer
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
               (lambda (_buf) 'live-widget))
              ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
               #'ignore)
              ((symbol-function 'agent-repl--frontend-read-event)
               (lambda () ?\C-g)))
      ;; Act
      (condition-case nil (agent-repl-test--press "y") (quit nil))
      ;; Assert
      (should (null agent-repl--frontend-copy-waiting)))))

(ert-deftest agent-repl-test-copy-wait-leaves-no-wake-event-behind ()
  "The wake event an answer queues never outlives the wait."
  (agent-repl-test--with-copy-buffer
    (agent-repl-test--answering "boot_failed" t
      ;; Act
      (agent-repl-test--press "y"))
    ;; Assert
    (should-not (memq agent-repl--frontend-copy-wake-event unread-command-events))))

(ert-deftest agent-repl-test-copy-stale-answer-wakes-nothing ()
  "An answer to a request nobody waits on queues no wake event."
  ;; Arrange
  (let ((unread-command-events nil)
        (agent-repl--frontend-copy-waiting 7))
    ;; Act
    (agent-repl--frontend-copy-answered "{\"id\":6,\"text\":\"x\",\"focused\":true}")
    ;; Assert
    (should (null unread-command-events))))

(ert-deftest agent-repl-test-copy-unreadable-answer-signals ()
  "An answer that is not the script's JSON object is an error."
  (should-error (agent-repl--frontend-copy-parse "not json")))

(ert-deftest agent-repl-test-copy-unreadable-answer-is-logged ()
  "An unreadable answer reaches the canonical error log."
  ;; Arrange
  (let ((errors nil))
    (cl-letf (((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
      ;; Act
      (ignore-errors (agent-repl--frontend-copy-parse "not json")))
    ;; Assert
    (should (string-match-p "copy-selection: unreadable answer" (car errors)))))

(ert-deftest agent-repl-test-copy-answer-without-id-signals ()
  "An answer with no integer id is refused."
  (should-error (agent-repl--frontend-copy-parse "{\"text\":\"x\"}")))

(ert-deftest agent-repl-test-copy-null-selection-object-signals ()
  "A page with no selection object is an invariant violation."
  (cl-letf (((symbol-function 'agent-repl--error) #'ignore))
    (should-error (agent-repl--frontend-copy-decision '(:id 1 :text nil :focused t)))))

(ert-deftest agent-repl-test-copy-null-selection-object-is-logged ()
  "The missing selection object reaches the canonical error log."
  ;; Arrange
  (let ((errors nil))
    (cl-letf (((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
      ;; Act
      (ignore-errors (agent-repl--frontend-copy-decision '(:id 1 :text nil :focused t))))
    ;; Assert
    (should (string-match-p "no selection object" (car errors)))))

(ert-deftest agent-repl-test-copy-answer-missing-text-signals ()
  "An answer with no text key at all is refused."
  (should-error (agent-repl--frontend-copy-decision '(:id 1 :focused t))))

(ert-deftest agent-repl-test-copy-script-answers-its-request-id ()
  "The script names its request id in the answer it builds."
  (should (string-match-p "id:42," (agent-repl--frontend-copy-script 42))))

(ert-deftest agent-repl-test-copy-script-reports-focus ()
  "The script reports whether the page has focus."
  (should (string-match-p "document\\.hasFocus()" (agent-repl--frontend-copy-script 1))))

(ert-deftest agent-repl-test-webview-read-channel-injects-into-a-live-webview ()
  "A read into a live webview is injected and answers non-nil."
  ;; Arrange
  (let ((injected nil))
    (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
               (lambda (_buf) 'live-widget))
              ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
               (lambda (xw script callback) (setq injected (list xw script callback)))))
      (with-temp-buffer
        ;; Act
        (let ((result (agent-repl--frontend-webview-read-script
                       (current-buffer) "1" 'agent-repl--frontend-copy-answered)))
          ;; Assert
          (should (equal (list result injected)
                         '(t (live-widget "1" agent-repl--frontend-copy-answered)))))))))

(ert-deftest agent-repl-test-webview-read-channel-skips-a-dead-webview ()
  "A buffer with no live webview injects nothing and answers nil."
  (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
             (lambda (_buf) nil)))
    (with-temp-buffer
      (should-not (agent-repl--frontend-webview-read-script
                   (current-buffer) "1" 'agent-repl--frontend-copy-answered)))))

(ert-deftest agent-repl-test-webview-read-channel-logs-a-refused-callback ()
  "A refused callback reaches the canonical error log."
  ;; Arrange
  (let ((errors nil))
    (cl-letf (((symbol-function 'agent-repl--error)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
      (with-temp-buffer
        ;; Act
        (ignore-errors (agent-repl--frontend-webview-read-script
                        (current-buffer) "1" (lambda (_raw) nil)))))
    ;; Assert
    (should (string-match-p "read-script: refused" (car errors)))))

;;;; ---- Webview presence on a switch ------------------------------------

(defmacro agent-repl-test--with-view-presence-stubs (view &rest body)
  "Run BODY with the xwidget primitives stubbed to display VIEW.
The current window shows the buffer, the buffer holds one xwidget, and
`xwidget-view-lookup' answers VIEW (a symbol stands in for a view)."
  (declare (indent 1))
  `(let ((agent-repl-frontend--last-view-on-switch (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'get-buffer-xwidgets) (lambda (_b) '(xw)))
               ((symbol-function 'xwidget-view-lookup) (lambda (_x &optional _w) ,view)))
       ,@body)))

(ert-deftest agent-repl-test-frontend-view-presence-first-switch-says-first ()
  ;; Arrange
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (agent-repl-test--with-view-presence-stubs 'view-a
      ;; Act
      (let ((presence (agent-repl-frontend--view-presence "ws" (current-buffer))))
        ;; Assert
        (should (eq (plist-get presence :view) 'first))))))

(ert-deftest agent-repl-test-frontend-view-presence-same-view-says-same ()
  ;; Arrange
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (agent-repl-test--with-view-presence-stubs 'view-a
      (agent-repl-frontend--view-presence "ws" (current-buffer))
      ;; Act
      (let ((presence (agent-repl-frontend--view-presence "ws" (current-buffer))))
        ;; Assert
        (should (eq (plist-get presence :view) 'same))))))

(ert-deftest agent-repl-test-frontend-view-presence-fresh-view-says-new ()
  ;; Arrange
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (let ((view 'view-a)
          (agent-repl-frontend--last-view-on-switch (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'get-buffer-xwidgets) (lambda (_b) '(xw)))
                ((symbol-function 'xwidget-view-lookup) (lambda (_x &optional _w) view)))
        (agent-repl-frontend--view-presence "ws" (current-buffer))
        (setq view 'view-b)
        ;; Act
        (let ((presence (agent-repl-frontend--view-presence "ws" (current-buffer))))
          ;; Assert
          (should (eq (plist-get presence :view) 'new)))))))

(ert-deftest agent-repl-test-frontend-view-presence-undisplayed-buffer-says-none ()
  ;; Arrange
  (let ((buf (generate-new-buffer " *undisplayed-view*")))
    (unwind-protect
        (agent-repl-test--with-view-presence-stubs 'view-a
          ;; Act
          (let ((presence (agent-repl-frontend--view-presence "ws" buf)))
            ;; Assert
            (should (eq (plist-get presence :view) 'none))
            (should (= (plist-get presence :windows) 0))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-frontend-view-presence-dead-buffer-says-none ()
  ;; Arrange
  (let ((buf (generate-new-buffer " *dead-view*")))
    (kill-buffer buf)
    (agent-repl-test--with-view-presence-stubs 'view-a
      ;; Act
      (let ((presence (agent-repl-frontend--view-presence "ws" buf)))
        ;; Assert
        (should (eq (plist-get presence :view) 'none))))))
