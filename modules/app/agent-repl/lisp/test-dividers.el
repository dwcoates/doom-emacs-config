;;; test-dividers.el --- Tests for agent-repl's hidden window dividers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for dividers.el -- while a frame shows agent-repl, the lines
;; between its windows are the panels' color, and its own colors come back
;; once it does not.  The sync runs against the batch session's own frame,
;; which holds frame parameters and per-frame faces like any other; only
;; whether a window shows a panel is stubbed.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test-dividers--with-frame (shows &rest body)
  "Run BODY against the batch frame, bound as `frame', showing panels if SHOWS.
`shows' stays bound, so BODY can change what the frame shows.  `logs'
collects every line the canonical log helper receives.  Each divider
face starts at a known color, and the frame is restored afterwards, so
no case leaks into another."
  (declare (indent 1))
  `(let ((frame (selected-frame))
         (shows ,shows)
         (logs nil)
         (agent-repl-input-background "#121212"))
     (cl-letf (((symbol-function 'agent-repl--dividers-shows-panels-p) (lambda (_frame) shows))
               ((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs)))
       (dolist (face agent-repl--dividers-faces)
         (set-face-attribute face frame :foreground "#686868"))
       (unwind-protect
           (progn ,@body)
         (agent-repl--dividers-restore frame)
         (agent-repl-frame-border-set-panels frame nil)))))

(put 'agent-repl-test-dividers--theme 'theme-settings
     '((theme-face window-divider agent-repl-test-dividers--theme ((t :foreground "#abcdef")))
       (theme-value some-variable agent-repl-test-dividers--theme t)))

(defun agent-repl-test-dividers--colors (frame)
  "Return the foreground of every divider face on FRAME."
  (mapcar (lambda (face) (face-attribute face :foreground frame)) agent-repl--dividers-faces))

;;;; ---- A frame showing agent-repl ----

(ert-deftest agent-repl-test-dividers-panels-paint-every-divider-face ()
  "A frame showing panels draws every divider face in the panels\=' color."
  (agent-repl-test-dividers--with-frame t
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (agent-repl-test-dividers--colors frame)
                   (make-list (length agent-repl--dividers-faces) "#121212")))))

(ert-deftest agent-repl-test-dividers-panels-keep-the-frames-own-colors ()
  "The mark holds each face\='s color from before the paint."
  (agent-repl-test-dividers--with-frame t
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (frame-parameter frame agent-repl--dividers-hidden)
                   (mapcar (lambda (face) (cons face "#686868")) agent-repl--dividers-faces)))))

(ert-deftest agent-repl-test-dividers-panels-log-the-hide ()
  "The hide is recorded through the canonical log helper with its color."
  (agent-repl-test-dividers--with-frame t
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (seq-some (lambda (line) (string-match-p "\\`elisp.dividers.hide .*color=#121212" line))
                      logs))))

(ert-deftest agent-repl-test-dividers-panels-hidden-twice-keep-the-first-colors ()
  "A second sync of a hidden frame keeps the colors from before the first."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (setq logs nil)
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (cdr (assq 'vertical-border (frame-parameter frame agent-repl--dividers-hidden)))
                   "#686868"))
    (should-not logs)))

;;;; ---- The frame's border ----

(ert-deftest agent-repl-test-dividers-panels-are-recorded-with-the-border ()
  "A frame showing panels is recorded with the border's owner, which paints it the ground."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (let ((recorded nil))
      (cl-letf (((symbol-function 'agent-repl-frame-border-set-panels)
                 (lambda (fr shows) (push (list fr shows) recorded))))
        ;; Act
        (agent-repl-dividers-sync frame))
      ;; Assert
      (should (equal recorded (list (list frame t)))))))

(ert-deftest agent-repl-test-dividers-no-panels-are-recorded-with-the-border ()
  "A frame that stops showing panels is recorded with the border's owner as showing none."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (setq shows nil)
    (let ((recorded nil))
      (cl-letf (((symbol-function 'agent-repl-frame-border-set-panels)
                 (lambda (fr shows) (push (list fr shows) recorded))))
        ;; Act
        (agent-repl-dividers-sync frame))
      ;; Assert
      (should (equal recorded (list (list frame nil)))))))

(ert-deftest agent-repl-test-dividers-panels-paint-the-frames-border-the-ground ()
  "Through the border's owner, a frame showing panels wears the ground round its edge."
  (agent-repl-test-dividers--with-frame t
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (face-attribute 'internal-border :background frame) "#121212"))))

;;;; ---- A new ground ----

(ert-deftest agent-repl-test-dividers-hidden-frames-are-the-marked-ones ()
  "The hidden frames are exactly those carrying the mark."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (let ((before (agent-repl--dividers-hidden-frames)))
      (agent-repl-dividers-sync frame)
      ;; Act + Assert
      (should (equal (list (memq frame before) (memq frame (agent-repl--dividers-hidden-frames)))
                     (list nil (list frame)))))))

(ert-deftest agent-repl-test-dividers-hidden-frames-walk-is-shared ()
  "The repaint and the theme reach the hidden frames through the one walk."
  ;; Arrange
  (let ((walks 0))
    (cl-letf (((symbol-function 'agent-repl--dividers-hidden-frames)
               (lambda () (cl-incf walks) nil)))
      ;; Act
      (agent-repl--dividers-repaint-all)
      (agent-repl--dividers-on-theme 'agent-repl-test-dividers--theme))
    ;; Assert
    (should (= walks 2))))


(ert-deftest agent-repl-test-dividers-repaint-paints-a-hidden-frame-the-new-ground ()
  "A ground change repaints every seam of a hidden frame in the new color."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    ;; Act
    (let ((agent-repl-input-background "#0a0a0a"))
      (agent-repl--dividers-repaint-all))
    ;; Assert
    (should (equal (agent-repl-test-dividers--colors frame)
                   (make-list (length agent-repl--dividers-faces) "#0a0a0a")))))

(ert-deftest agent-repl-test-dividers-repaint-keeps-the-frames-own-colors ()
  "A repaint leaves the colors kept for the restore as they were."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    ;; Act
    (let ((agent-repl-input-background "#0a0a0a"))
      (agent-repl--dividers-repaint-all))
    ;; Assert
    (should (equal (frame-parameter frame agent-repl--dividers-hidden)
                   (mapcar (lambda (face) (cons face "#686868")) agent-repl--dividers-faces)))))

(ert-deftest agent-repl-test-dividers-repaint-leaves-an-unhidden-frame ()
  "A frame showing no panels keeps its own colors through a ground change."
  (agent-repl-test-dividers--with-frame nil
    ;; Act
    (let ((agent-repl-input-background "#0a0a0a"))
      (agent-repl--dividers-repaint-all))
    ;; Assert
    (should (equal (agent-repl-test-dividers--colors frame)
                   (make-list (length agent-repl--dividers-faces) "#686868")))
    (should-not logs)))

(ert-deftest agent-repl-test-dividers-repaint-logs-the-frame-and-color ()
  "A repaint is recorded through the canonical log helper with its new color."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (setq logs nil)
    ;; Act
    (let ((agent-repl-input-background "#0a0a0a"))
      (agent-repl--dividers-repaint-all))
    ;; Assert
    (should (seq-some (lambda (line) (string-match-p "\\`elisp.dividers.repaint .*color=#0a0a0a" line))
                      logs))))

;;;; ---- A frame no longer showing agent-repl ----

(ert-deftest agent-repl-test-dividers-no-panels-restore-the-frames-own-colors ()
  "A hidden frame that stops showing panels gets its own colors back."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (setq shows nil)
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (agent-repl-test-dividers--colors frame)
                   (make-list (length agent-repl--dividers-faces) "#686868")))
    (should-not (frame-parameter frame agent-repl--dividers-hidden))))

(ert-deftest agent-repl-test-dividers-no-panels-restore-an-unspecified-color ()
  "A face with no color of its own on the frame is left with none."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (set-face-attribute 'window-divider frame :foreground 'unspecified)
    (agent-repl-dividers-sync frame)
    (setq shows nil)
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (eq (face-attribute 'window-divider :foreground frame) 'unspecified))))

(ert-deftest agent-repl-test-dividers-no-panels-log-the-restore ()
  "The restore is recorded through the canonical log helper."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (setq shows nil logs nil)
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (seq-some (lambda (line) (string-prefix-p "elisp.dividers.restore " line)) logs))))

;;;; ---- A frame never showing agent-repl ----

(ert-deftest agent-repl-test-dividers-other-frame-keeps-its-colors ()
  "A frame showing no panel is never painted, marked or logged."
  (agent-repl-test-dividers--with-frame nil
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (agent-repl-test-dividers--colors frame)
                   (make-list (length agent-repl--dividers-faces) "#686868")))
    (should-not (frame-parameter frame agent-repl--dividers-hidden))
    (should-not logs)))

(ert-deftest agent-repl-test-dividers-dead-frame-is-left-alone ()
  "A dead frame, which a late hook call can name, is not touched."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) nil)))
      ;; Act
      (agent-repl-dividers-sync frame))
    ;; Assert
    (should-not (frame-parameter frame agent-repl--dividers-hidden))
    (should-not logs)))

;;;; ---- A theme load ----

(ert-deftest agent-repl-test-dividers-theme-keeps-a-hidden-frame-hidden ()
  "A face a theme rewrote on a hidden frame is painted again."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (set-face-attribute 'window-divider frame :foreground "#abcdef")
    ;; Act
    (agent-repl--dividers-on-theme 'agent-repl-test-dividers--theme)
    ;; Assert
    (should (equal (face-attribute 'window-divider :foreground frame) "#121212"))))

(ert-deftest agent-repl-test-dividers-theme-keeps-the-themes-color ()
  "A face a theme rewrote keeps the theme\='s color for the restore."
  (agent-repl-test-dividers--with-frame t
    ;; Arrange
    (agent-repl-dividers-sync frame)
    (set-face-attribute 'window-divider frame :foreground "#abcdef")
    (agent-repl--dividers-on-theme 'agent-repl-test-dividers--theme)
    (setq shows nil)
    ;; Act
    (agent-repl-dividers-sync frame)
    ;; Assert
    (should (equal (face-attribute 'window-divider :foreground frame) "#abcdef"))
    (should (equal (face-attribute 'vertical-border :foreground frame) "#686868"))))

(ert-deftest agent-repl-test-dividers-theme-leaves-an-unhidden-frame ()
  "A theme load does not touch a frame whose dividers are not hidden."
  (agent-repl-test-dividers--with-frame nil
    ;; Act
    (agent-repl--dividers-on-theme 'agent-repl-test-dividers--theme)
    ;; Assert
    (should (equal (agent-repl-test-dividers--colors frame)
                   (make-list (length agent-repl--dividers-faces) "#686868")))
    (should-not logs)))

;;;; ---- Which faces a theme defines ----

(ert-deftest agent-repl-test-dividers-theme-faces-are-its-face-settings ()
  "A theme\='s faces are its `theme-face' settings, never its variables."
  ;; Act + Assert
  (should (equal (agent-repl--dividers-theme-faces 'agent-repl-test-dividers--theme)
                 '(window-divider))))

;;;; ---- Which frames show agent-repl ----

(ert-deftest agent-repl-test-dividers-shows-panels-for-a-panel-buffer ()
  "A frame with a window on an agent-repl panel buffer shows panels."
  (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p) (lambda (&optional _buf) t))
            ((symbol-function 'agent-repl-sidebar-view-window-p) (lambda (_win) nil)))
    ;; Act + Assert
    (should (agent-repl--dividers-shows-panels-p (selected-frame)))))

(ert-deftest agent-repl-test-dividers-shows-panels-for-the-sidebar ()
  "A frame with a window on the sidebar shows panels."
  (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p) (lambda (&optional _buf) nil))
            ((symbol-function 'agent-repl-sidebar-view-window-p) (lambda (_win) t)))
    ;; Act + Assert
    (should (agent-repl--dividers-shows-panels-p (selected-frame)))))

(ert-deftest agent-repl-test-dividers-shows-panels-not-for-other-buffers ()
  "A frame showing neither a panel nor the sidebar shows no panels."
  (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p) (lambda (&optional _buf) nil))
            ((symbol-function 'agent-repl-sidebar-view-window-p) (lambda (_win) nil)))
    ;; Act + Assert
    (should-not (agent-repl--dividers-shows-panels-p (selected-frame)))))

(ert-deftest agent-repl-test-dividers-shows-panels-for-a-chat-thread-composer ()
  "A zettel chat thread\='s composer, named from `chat/ID', is a panel buffer."
  (cl-letf (((symbol-function 'agent-repl-sidebar-view-window-p) (lambda (_win) nil)))
    ;; Arrange
    (let ((buf (get-buffer-create (format "*agent-panel-input-%s*"
                                          (agent-repl--sanitize-ws-name "chat/abc123")))))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer buf)
            ;; Act + Assert
            (should (agent-repl--dividers-shows-panels-p (selected-frame))))
        (kill-buffer buf)))))

;;;; ---- A web view's fringes ----

(defmacro agent-repl-test-dividers--with-webview (&rest body)
  "Run BODY with a web view buffer, bound as `buf', in the selected window `win'.
`set-window-fringes' is recorded into `fringed' as (WINDOW LEFT RIGHT),
since a batch frame draws no fringes; `logs' collects every log line."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer "*agent-repl-test-dividers-webview*"))
         (fringed nil)
         (logs nil))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer buf)
           (let ((win (selected-window)))
             (cl-letf (((symbol-function 'set-window-fringes)
                        (lambda (window left right &rest _) (push (list window left right) fringed)))
                       ((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs)))
               ,@body)))
       (kill-buffer buf))))

(ert-deftest agent-repl-test-dividers-webview-on-zeroes-the-buffers-fringes ()
  "A web view buffer given no fringes has zero fringe widths of its own."
  (agent-repl-test-dividers--with-webview
    ;; Act
    (agent-repl-dividers-fix-fringes buf 0)
    ;; Assert
    (should (equal (buffer-local-value 'left-fringe-width buf) 0))
    (should (equal (buffer-local-value 'right-fringe-width buf) 0))))

(ert-deftest agent-repl-test-dividers-webview-on-zeroes-a-showing-windows-fringes ()
  "A window already showing the web view is given zero fringes at once."
  (agent-repl-test-dividers--with-webview
    ;; Act
    (agent-repl-dividers-fix-fringes buf 0)
    ;; Assert
    (should (equal fringed (list (list win 0 0))))))

(ert-deftest agent-repl-test-dividers-webview-off-returns-the-frames-fringes ()
  "A web view given its fringes back drops its widths, and its windows take the frame\='s."
  (agent-repl-test-dividers--with-webview
    ;; Arrange
    (agent-repl-dividers-fix-fringes buf 0)
    (setq fringed nil)
    ;; Act
    (agent-repl-dividers-fix-fringes buf nil)
    ;; Assert
    (should-not (local-variable-p 'left-fringe-width buf))
    (should-not (local-variable-p 'right-fringe-width buf))
    (should (equal fringed (list (list win nil nil))))))

(ert-deftest agent-repl-test-dividers-webview-logs-the-widths ()
  "The fringe change is recorded through the canonical log helper."
  (agent-repl-test-dividers--with-webview
    ;; Act
    (agent-repl-dividers-fix-fringes buf 0)
    ;; Assert
    (should (seq-some (lambda (line)
                        (string-match-p "\\`elisp.dividers.fix-fringes .*width=0 windows=1" line))
                      logs))))

(ert-deftest agent-repl-test-dividers-webview-mode-on-gives-no-fringes ()
  "A slot whose web view mode turns on loses its fringes."
  (agent-repl-test-dividers--with-webview
    ;; Arrange
    (with-current-buffer buf (setq-local agent-repl-frontend-webview-mode t))
    ;; Act
    (with-current-buffer buf (agent-repl--dividers-on-webview-mode))
    ;; Assert
    (should (equal (buffer-local-value 'left-fringe-width buf) 0))))

(ert-deftest agent-repl-test-dividers-webview-mode-off-returns-the-fringes ()
  "A slot whose web view mode turns off gets the frame\='s fringes back."
  (agent-repl-test-dividers--with-webview
    ;; Arrange
    (agent-repl-dividers-fix-fringes buf 0)
    (with-current-buffer buf (setq-local agent-repl-frontend-webview-mode nil))
    ;; Act
    (with-current-buffer buf (agent-repl--dividers-on-webview-mode))
    ;; Assert
    (should-not (local-variable-p 'left-fringe-width buf))))

(ert-deftest agent-repl-test-dividers-webview-on-marks-the-buffer ()
  "A web view given no fringes is marked, so later window changes keep it so."
  (agent-repl-test-dividers--with-webview
    ;; Act
    (agent-repl-dividers-fix-fringes buf 0)
    ;; Assert
    (should (equal (buffer-local-value 'agent-repl--dividers-fixed-fringe buf) 0))))

;;;; ---- A window restored with its saved fringes ----

(defmacro agent-repl-test-dividers--with-fringed-window (width fringes &rest body)
  "Run BODY with the selected window showing a buffer held to fringe WIDTH.
WIDTH nil leaves the buffer the frame\='s fringes.  The window reports
FRINGES, as a batch frame draws none of its own."
  (declare (indent 2))
  `(agent-repl-test-dividers--with-webview
     (with-current-buffer buf (setq agent-repl--dividers-fixed-fringe ,width))
     (cl-letf (((symbol-function 'window-fringes) (lambda (&optional _win) ,fringes)))
       ,@body)))

(ert-deftest agent-repl-test-dividers-fit-gives-a-restored-window-zero-fringes ()
  "A window showing a fringeless web view with fringes again is given zero."
  (agent-repl-test-dividers--with-fringed-window 0 '(8 8 t nil)
    ;; Act
    (agent-repl--dividers-fit-fringes (selected-frame))
    ;; Assert
    (should (equal fringed (list (list win 0 0))))))

(ert-deftest agent-repl-test-dividers-fit-logs-the-restored-window ()
  "The fit is recorded through the canonical log helper with the widths it replaced."
  (agent-repl-test-dividers--with-fringed-window 0 '(8 8 t nil)
    ;; Act
    (agent-repl--dividers-fit-fringes (selected-frame))
    ;; Assert
    (should (seq-some (lambda (line) (string-match-p "\\`elisp.dividers.fit-fringes .*from=(8 8 t nil)" line))
                      logs))))

(ert-deftest agent-repl-test-dividers-fit-leaves-a-zero-fringed-window ()
  "A window already at zero fringes is not touched."
  (agent-repl-test-dividers--with-fringed-window 0 '(0 0 nil nil)
    ;; Act
    (agent-repl--dividers-fit-fringes (selected-frame))
    ;; Assert
    (should-not fringed)
    (should-not logs)))

(ert-deftest agent-repl-test-dividers-fit-gives-a-restored-window-its-buffers-width ()
  "A window showing a buffer held to one-pixel fringes is given one pixel each."
  (agent-repl-test-dividers--with-fringed-window 1 '(8 8 t nil)
    ;; Act
    (agent-repl--dividers-fit-fringes (selected-frame))
    ;; Assert
    (should (equal fringed (list (list win 1 1))))))

(ert-deftest agent-repl-test-dividers-fit-leaves-a-window-at-its-buffers-width ()
  "A window already at its buffer\='s one-pixel fringes is not touched."
  (agent-repl-test-dividers--with-fringed-window 1 '(1 1 nil nil)
    ;; Act
    (agent-repl--dividers-fit-fringes (selected-frame))
    ;; Assert
    (should-not fringed)))

(ert-deftest agent-repl-test-dividers-fix-holds-the-buffer-to-a-width ()
  "A buffer held to one-pixel fringes has those widths, and its windows take them."
  (agent-repl-test-dividers--with-webview
    ;; Act
    (agent-repl-dividers-fix-fringes buf 1)
    ;; Assert
    (should (equal (list (buffer-local-value 'left-fringe-width buf)
                         (buffer-local-value 'right-fringe-width buf)
                         (buffer-local-value 'agent-repl--dividers-fixed-fringe buf)
                         fringed)
                   (list 1 1 1 (list (list win 1 1)))))))

(ert-deftest agent-repl-test-dividers-fit-leaves-other-buffers-windows ()
  "A window showing a buffer with no fixed fringes keeps its fringes."
  (agent-repl-test-dividers--with-fringed-window nil '(8 8 t nil)
    ;; Act
    (agent-repl--dividers-fit-fringes (selected-frame))
    ;; Assert
    (should-not fringed)))

(ert-deftest agent-repl-test-dividers-sync-fits-the-fringes ()
  "The frame sync on every window change fits the web views\=' fringes."
  (agent-repl-test-dividers--with-fringed-window 0 '(8 8 t nil)
    (cl-letf (((symbol-function 'agent-repl--dividers-shows-panels-p) (lambda (_frame) nil)))
      ;; Act
      (agent-repl-dividers-sync (selected-frame))
      ;; Assert
      (should (equal fringed (list (list win 0 0)))))))

;;;; ---- Hooks ----

(ert-deftest agent-repl-test-dividers-hooks-are-installed ()
  "The sync follows every window change, theme load and web view mode change."
  ;; Act + Assert
  (should (memq #'agent-repl-dividers-sync (default-value 'window-state-change-functions)))
  (should (memq #'agent-repl--dividers-on-theme enable-theme-functions))
  (should (memq #'agent-repl--dividers-on-webview-mode agent-repl-frontend-webview-mode-hook)))

(ert-deftest agent-repl-test-dividers-repaint-follows-the-ground ()
  "A ground change repaints the hidden frames (`agent-repl-input-background-painters')."
  ;; Act + Assert
  (should (memq #'agent-repl--dividers-repaint-all agent-repl-input-background-painters)))

(provide 'test-dividers)
;;; test-dividers.el ends here
