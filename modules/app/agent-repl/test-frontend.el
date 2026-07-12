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

(defmacro agent-repl-test--with-frontend-ws (ws plist &rest body)
  "Register workspace WS with PLIST for BODY, cleaning buffers after."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (puthash ,ws (copy-sequence ,plist) agent-repl--workspaces)
         ,@body)
     (let ((buf (agent-repl--ws-get ,ws :frontend-buffer)))
       (when (buffer-live-p buf) (kill-buffer buf)))
     (remhash ,ws agent-repl--workspaces)))

;; The webview boundary mock (`agent-repl-test--fake-webview-factory')
;; lives in test-helpers.el — the explain-config popup mounts the same
;; wrapper, and the two must not drift on what they pretend a webview is.

;;;; ---- Buffer naming -------------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-name-outside-panel-namespace ()
  "Webview names must not match the agent-panel regexes."
  ;; Act
  (let ((name (agent-repl--frontend-webview-buffer-name "myws")))
    ;; Assert
    (should (equal name "*agent-frontend-myws*"))
    (should-not (string-match-p agent-repl--vterm-buffer-re name))
    (should-not (string-match-p agent-repl--input-buffer-re name))))

;;;; ---- ensure-webview-buffer ------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-created-and-pinned ()
  "A fresh webview is created at the session URL with a pinned name."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer
                    "ws1" "s_1" "http://x/?session=s_1")))
          ;; Assert
          (should (equal agent-repl-test--urls '("http://x/?session=s_1")))
          (should (equal (buffer-name buf) "*agent-frontend-ws1*"))
          (should (equal (buffer-local-value 'xwidget-webkit-buffer-name-format buf)
                         "*agent-frontend-ws1*"))
          (should (eq (agent-repl--ws-get "ws1" :frontend-buffer) buf))
          (should (equal (agent-repl--ws-get "ws1" :frontend-buffer-session-id) "s_1")))))))

(ert-deftest agent-repl-test-frontend-webview-header-line-cleared ()
  "The mount clears `xwidget-webkit-mode's \"WebKit: <title>\" header-line."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (let ((buf (agent-repl--frontend-ensure-webview-buffer
                    "ws1" "s_1" "http://x/?session=s_1")))
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
        (let ((first (agent-repl--frontend-ensure-webview-buffer
                      "ws1" "s_1" "http://x/?session=s_1")))
          ;; Act
          (let ((second (agent-repl--frontend-ensure-webview-buffer
                         "ws1" "s_1" "http://x/?session=s_1")))
            ;; Assert — one creation only.
            (should (eq first second))
            (should (= (length agent-repl-test--urls) 1))))))))

(ert-deftest agent-repl-test-frontend-webview-rebound-on-session-change ()
  "A session change kills the stale webview and mounts a fresh one."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        (let ((old (agent-repl--frontend-ensure-webview-buffer
                    "ws1" "s_old" "http://x/?session=s_old")))
          ;; Act
          (let ((new (agent-repl--frontend-ensure-webview-buffer
                      "ws1" "s_new" "http://x/?session=s_new")))
            ;; Assert
            (should-not (buffer-live-p old))
            (should (buffer-live-p new))
            (should (equal (agent-repl--ws-get "ws1" :frontend-buffer-session-id) "s_new"))
            (should (= (length agent-repl-test--urls) 2))))))))

;;;; ---- sync-webview -----------------------------------------------------------

(ert-deftest agent-repl-test-frontend-webview-url-carries-composer-flag ()
  "The webview URL always hides the webapp composer (Emacs owns input)."
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    ;; Act / Assert
    (should (string-match-p "composer=0"
                            (agent-repl--frontend-webview-url "ws1" "s_1")))))

(ert-deftest agent-repl-test-frontend-webview-url-carries-parent-ws ()
  "A recorded parent worktree lands in the URL as parent_ws."
  (agent-repl-test--with-frontend-ws "ws1"
      '(:project-dir "/w" :source-ws-dir "/repos/parent-tree/")
    ;; Act / Assert
    (should (string-match-p "parent_ws=parent-tree"
                            (agent-repl--frontend-webview-url "ws1" "s_1")))))

(ert-deftest agent-repl-test-frontend-sync-webview-no-op-when-binding-matches ()
  "sync-webview does not touch a webview already bound to the session."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        (let ((buf (agent-repl--frontend-ensure-webview-buffer
                    "ws1" "s_1" "http://x/?session=s_1")))
          ;; Act
          (agent-repl--frontend-sync-webview "ws1" "s_1")
          ;; Assert — no second mount.
          (should (buffer-live-p buf))
          (should (= (length agent-repl-test--urls) 1)))))))

(ert-deftest agent-repl-test-frontend-sync-webview-remounts-on-heal ()
  "sync-webview remounts the webview onto a healed session's id."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        (let ((old (agent-repl--frontend-ensure-webview-buffer
                    "ws1" "s_dead" "http://x/?session=s_dead")))
          ;; Act
          (agent-repl--frontend-sync-webview "ws1" "s_fresh")
          ;; Assert — stale webview replaced, bound to the new session.
          (should-not (buffer-live-p old))
          (should (equal (agent-repl--ws-get "ws1" :frontend-buffer-session-id)
                         "s_fresh"))
          ;; The factory pushes newest-first; the remount URL is the head.
          (should (string-match-p "session=s_fresh"
                                  (car agent-repl-test--urls))))))))

(ert-deftest agent-repl-test-frontend-sync-webview-no-op-without-live-buffer ()
  "sync-webview does nothing when no webview buffer exists (panel closed)."
  ;; Arrange
  (defvar agent-repl-test--urls)
  (let ((agent-repl-test--urls '()))
    (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
      (cl-letf (((symbol-function 'agent-repl--frontend-make-webview-buffer)
                 (agent-repl-test--fake-webview-factory 'agent-repl-test--urls)))
        ;; Act
        (agent-repl--frontend-sync-webview "ws1" "s_1")
        ;; Assert
        (should (null agent-repl-test--urls))))))

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
        (let ((buf (agent-repl--frontend-ensure-webview-buffer
                    "ws1" "s_1" "http://x/?session=s_1")))
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

(ert-deftest agent-repl-test-frontend-main-area-window-skips-dedicated ()
  "The host search never returns a dedicated window."
  ;; Arrange — every window reads as dedicated except none: expect the
  ;; selected-window fallback rather than a dedicated pick.
  (cl-letf (((symbol-function 'agent-repl-window--side-window-p)
             (lambda (_win) nil))
            ((symbol-function 'window-dedicated-p)
             (lambda (_win) t)))
    ;; Act / Assert — falls back to selected-window instead of a
    ;; dedicated candidate.
    (should (eq (agent-repl--frontend-main-area-window) (selected-window)))))

(ert-deftest agent-repl-test-frontend-main-area-window-skips-side-windows ()
  "The webview host window is never a side window."
  ;; Arrange — mark every window EXCEPT the selected one as side.
  (let ((sel (selected-window)))
    (cl-letf (((symbol-function 'agent-repl-window--side-window-p)
               (lambda (win) (not (eq win sel)))))
      ;; Act / Assert
      (should (eq (agent-repl--frontend-main-area-window) sel)))))

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
  "open-panel threads ensure-session's id into the webview URL and display."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((displayed nil)
          (ensured nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                 (lambda () t))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "ws1"))
                ((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (ws) (setq ensured ws) "s_42"))
                ((symbol-function 'agent-repl--frontend-ensure-webview-buffer)
                 (lambda (_ws id url)
                   (should (equal id "s_42"))
                   ;; composer=0: Emacs owns input in the hybrid UI.
                   (should (string-suffix-p "/?session=s_42&composer=0" url))
                   'fake-buffer))
                ((symbol-function 'agent-repl--frontend-display-webview)
                 (lambda (_ws buf) (setq displayed buf))))
        ;; Act
        (agent-repl-frontend-open-panel)
        ;; Assert
        (should (equal ensured "ws1"))
        (should (eq displayed 'fake-buffer))))))

(ert-deftest agent-repl-test-frontend-open-carries-parent-ws-param ()
  "gui-open appends parent_ws (url-encoded :source-ws-dir basename) to the URL.
The webapp status bar renders it for vterm mode-line parity."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1"
      '(:project-dir "/w" :source-ws-dir "/repos/parent dir/")
    (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
               (lambda () t))
              ((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws) "s_42"))
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
               (lambda (_ws) "s_42"))
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
      (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                 (lambda (ws) (setq ensured ws) "s_42")))
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
                 (lambda (_ws) "s_42"))
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
  "The gui boot marks :init so a generated workspace shows a loading badge.
The vterm boot does the same; without it a gui-born workspace would render
no state at all until its agent answered."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (_ws) "s_42")))
      ;; Act
      (agent-repl--gui-boot "ws1" "/w" :bare-metal)
      ;; Assert
      (should (eq (agent-repl--ws-get "ws1" :agent-state) :init)))))

(ert-deftest agent-repl-test-frontend-gui-boot-refuses-a-sandbox-workspace ()
  "The gui boot refuses a :sandbox workspace instead of running it on the host."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :active-env :sandbox)
    (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
               (lambda (&rest _) (error "must not reach the daemon"))))
      ;; Act / Assert
      (should-error (agent-repl--gui-boot "ws1" "/w" :sandbox) :type 'user-error))))

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
  "close-panel kills the webview and clears both plist keys."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((buf (generate-new-buffer "*fake-webview*")))
      (agent-repl--ws-put "ws1" :frontend-buffer buf)
      (agent-repl--ws-put "ws1" :frontend-buffer-session-id "s_1")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        ;; Act
        (agent-repl-frontend-close-panel)
        ;; Assert
        (should-not (buffer-live-p buf))
        (should (null (agent-repl--ws-get "ws1" :frontend-buffer)))
        (should (null (agent-repl--ws-get "ws1" :frontend-buffer-session-id)))))))

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
Restoring is what removes BOTH gui windows — the vterm close contract."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((restored nil)
          (closed nil))
      (cl-letf (((symbol-function 'agent-repl--restore-fullscreen-config)
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
resolving the input buffer by name when the plist key is stale nil."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w")
    (let ((named (get-buffer-create "*agent-panel-input-ws1*"))
          (closed nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--restore-fullscreen-config)
                     (lambda (_ws) nil))
                    ((symbol-function 'agent-repl--close-buffer-windows)
                     (lambda (&rest bufs) (setq closed bufs))))
            ;; Act — :input-buffer is nil; the named buffer must resolve.
            (agent-repl--gui-hide "ws1")
            ;; Assert
            (should (memq named closed)))
        (kill-buffer named)))))

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
  "gui kill hides the webview/input windows BEFORE releasing state.
`agent-repl-switch-frontend' opens the next frontend right after kill;
a leftover dedicated input window aborts the vterm launch."
  ;; Arrange
  (agent-repl-test--with-frontend-ws "ws1" '(:project-dir "/w" :frontend-session-id "s_1")
    (let ((order nil))
      (cl-letf (((symbol-function 'agent-repl--gui-hide)
                 (lambda (_ws) (push 'hide order)))
                ((symbol-function 'agent-repl--frontend-release-workspace-session)
                 (lambda (_ws) (push 'release-session order)))
                ((symbol-function 'agent-repl--frontend-release-workspace-webview)
                 (lambda (_ws) (push 'release-webview order))))
        ;; Act
        (agent-repl--gui-kill "ws1")
        ;; Assert — layout teardown precedes the releases.
        (should (equal (nreverse order) '(hide release-session release-webview)))
        (should (null (agent-repl--ws-get "ws1" :frontend-buffer)))
        (should (null (agent-repl--ws-get "ws1" :frontend-buffer-session-id)))))))

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
