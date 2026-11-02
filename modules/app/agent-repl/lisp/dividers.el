;;; dividers.el --- No seam between agent-repl's windows -*- lexical-binding: t; -*-

;;; Commentary:

;; WHILE A FRAME SHOWS AGENT-REPL, THE LINES BETWEEN ITS WINDOWS ARE THE
;; PANELS' OWN COLOR (owner request, 2026-11-01): the sidebar, the feed and
;; the composer read as one dark surface, with no light seam between them.
;; A zettel chat thread is a workspace with the same panels, so it is the
;; same case.  Every other frame keeps its dividers as they were.
;;
;; Emacs draws the line between two windows in the frame's
;; `window-divider' faces (Doom turns `window-divider-mode' on, one pixel
;; right and below) and, where a frame has no divider, in
;; `vertical-border'.  Both are frame-level faces no buffer can remap, so
;; the frame's own copy of each is set to `agent-repl-input-background',
;; the composer's color and the dark sidebar's `--sidebar-bg'.  The dividers keep their
;; width, so a window edge is still grabbed and dragged where it was.
;;
;; THE FRAME'S OWN COLORS ARE KEPT IN THE MARK.  A face set with no frame
;; (config.el's `vertical-border') has no spec to recalculate back to, so
;; the frame parameter marking a frame hidden holds each face's foreground
;; from before, and the restore puts exactly those back.  A theme loaded
;; while a frame is hidden rewrites the faces it defines on that frame:
;; each such face's kept color becomes the theme's, and the frame is hidden
;; afresh.
;;
;; The frame follows what it shows on every window change
;; (`window-state-change-functions', which calls with the frame) and on a
;; theme load.
;;
;; A WEB VIEW MEETS THE DIVIDER.  Every window draws a fringe on each side
;; of its body, and a web view's buffer (the sidebar, a workspace's slot)
;; draws it in its fringe face, a dark that is not the page's, so a strip
;; stood between each divider and the page beside it (#161719 beside the
;; page's #121212, measured 2026-11-01).  A web view's buffer has no
;; fringes: its fringe widths are buffer-local zeros, which a window it is
;; displayed in takes up.  A window can also come back with the fringes it
;; was saved with (a workspace switch restores its window configuration), so
;; every window change gives each window showing such a buffer zero fringes
;; again.  Emacs's own web views (the host, the sidebar) lose their fringes
;; as webhost.el makes them, a slot as its mode turns on.  The page is
;; fitted to its window's body (frontend.el), so it fills the window to the
;; divider.  The composer keeps its fringes, which it paints its own
;; background.

;;; Code:

(require 'seq)

(declare-function agent-repl--agent-panel-buffer-p "core" (&optional buf))
(declare-function agent-repl-sidebar-view-window-p "sidebar-view" (win))
(declare-function agent-repl-sidebar-view-buffer "sidebar-view" ())
(declare-function agent-repl-webhost-buffer "webhost" ())
(declare-function agent-repl--log "core" (ws fmt &rest args))
(defvar agent-repl-input-background)
(defvar agent-repl-frontend-webview-mode)

(defconst agent-repl--dividers-log-scope
  '(:agent-repl-central "a frame's dividers belong to no one workspace")
  "The central log scope every divider record is written under.")

(defconst agent-repl--dividers-faces
  '(window-divider window-divider-first-pixel window-divider-last-pixel vertical-border)
  "Every face Emacs draws the line between two windows in.")

(defconst agent-repl--dividers-hidden 'agent-repl-dividers-hidden
  "The frame parameter of a frame whose dividers are hidden.
Its value is an alist of each of `agent-repl--dividers-faces' to the
frame\='s foreground for it from before; nil when not hidden.")

(defun agent-repl--dividers-shows-panels-p (frame)
  "Return non-nil when a window of FRAME shows an agent-repl panel.
The sidebar, a workspace\='s page or its composer, a chat thread\='s
included."
  (seq-some (lambda (win)
              (or (agent-repl--agent-panel-buffer-p (window-buffer win))
                  (agent-repl-sidebar-view-window-p win)))
            (window-list frame 'no-minibuf)))

(defun agent-repl--dividers-paint (frame)
  "Set every divider face on FRAME to the panels\=' color."
  (dolist (face agent-repl--dividers-faces)
    (set-face-attribute face frame :foreground agent-repl-input-background)))

(defun agent-repl--dividers-hide (frame)
  "Draw FRAME\='s dividers in the panels\=' color, keeping its own colors.
A frame already hidden is left as it is."
  (unless (frame-parameter frame agent-repl--dividers-hidden)
    (let ((kept (mapcar (lambda (face) (cons face (face-attribute face :foreground frame)))
                        agent-repl--dividers-faces)))
      (agent-repl--log agent-repl--dividers-log-scope
                       "elisp.dividers.hide frame=%S color=%s kept=%S"
                       frame agent-repl-input-background kept)
      (agent-repl--dividers-paint frame)
      (set-frame-parameter frame agent-repl--dividers-hidden kept))))

(defun agent-repl--dividers-restore (frame)
  "Put FRAME\='s own divider colors back.
A frame not hidden is left as it is."
  (when-let ((kept (frame-parameter frame agent-repl--dividers-hidden)))
    (agent-repl--log agent-repl--dividers-log-scope
                     "elisp.dividers.restore frame=%S kept=%S" frame kept)
    (dolist (entry kept)
      (set-face-attribute (car entry) frame :foreground (cdr entry)))
    (set-frame-parameter frame agent-repl--dividers-hidden nil)))

(defvar-local agent-repl--dividers-no-fringes nil
  "Non-nil in a web view buffer that has no fringes.")

(defun agent-repl--dividers-fit-fringes (frame)
  "Give every window of FRAME showing a fringeless web view zero fringes.
A window restored with the fringes it was saved with is the one changed;
one already at zero is left alone."
  (dolist (win (window-list frame 'no-minibuf))
    (when (and (buffer-local-value 'agent-repl--dividers-no-fringes (window-buffer win))
               (not (equal (seq-take (window-fringes win) 2) '(0 0))))
      (agent-repl--log agent-repl--dividers-log-scope
                       "elisp.dividers.fit-fringes window=%S buffer=%s from=%S"
                       win (buffer-name (window-buffer win)) (window-fringes win))
      (set-window-fringes win 0 0))))

(defun agent-repl-dividers-sync (&optional frame)
  "Hide FRAME\='s dividers while it shows agent-repl, restore them otherwise.
Every window of FRAME showing a fringeless web view gets zero fringes.
FRAME defaults to the selected frame; a dead one is left alone.
Registered on `window-state-change-functions\=', which calls it with FRAME."
  (let ((frame (or frame (selected-frame))))
    (when (frame-live-p frame)
      (if (agent-repl--dividers-shows-panels-p frame)
          (agent-repl--dividers-hide frame)
        (agent-repl--dividers-restore frame))
      (agent-repl--dividers-fit-fringes frame))))

(defun agent-repl--dividers-theme-faces (theme)
  "Return the faces THEME defines, which enabling it recalculates."
  (seq-keep (lambda (setting)
              (and (eq (car setting) 'theme-face) (nth 1 setting)))
            (get theme 'theme-settings)))

(defun agent-repl--dividers-on-theme (theme)
  "Keep every hidden frame hidden across THEME\='s load.
The load recalculated, on each frame, the faces THEME defines: each such
divider face now holds THEME\='s color, which becomes its kept color, and
the frame is painted afresh.  Every other face kept its color from before."
  (let ((rewritten (agent-repl--dividers-theme-faces theme)))
    (dolist (frame (frame-list))
      (when-let ((kept (frame-parameter frame agent-repl--dividers-hidden)))
        (let ((rekept (mapcar (lambda (entry)
                                (if (memq (car entry) rewritten)
                                    (cons (car entry) (face-attribute (car entry) :foreground frame))
                                  entry))
                              kept)))
          (agent-repl--log agent-repl--dividers-log-scope
                           "elisp.dividers.theme frame=%S theme=%S kept=%S" frame theme rekept)
          (agent-repl--dividers-paint frame)
          (set-frame-parameter frame agent-repl--dividers-hidden rekept))))))

(defun agent-repl-dividers-webview-fringes (buf on)
  "Give web view buffer BUF no fringes when ON, or the frame\='s back.
ON, BUF\='s fringe widths are buffer-local zeros; off, they are the
frame\='s again.  Every window showing BUF takes the widths at once.
Called for each of Emacs\='s own web views as it is made (webhost.el) and
for a workspace\='s slot as `agent-repl-frontend-webview-mode\=' turns on
or off."
  (let ((width (and on 0))
        (windows (get-buffer-window-list buf nil t)))
    (agent-repl--log agent-repl--dividers-log-scope
                     "elisp.dividers.webview-fringes buffer=%s width=%S windows=%d"
                     (buffer-name buf) width (length windows))
    (with-current-buffer buf
      (setq agent-repl--dividers-no-fringes on)
      (if width
          (setq-local left-fringe-width width
                      right-fringe-width width)
        (kill-local-variable 'left-fringe-width)
        (kill-local-variable 'right-fringe-width)))
    (dolist (win windows)
      (set-window-fringes win width width))))

(defun agent-repl--dividers-on-webview-mode ()
  "Follow `agent-repl-frontend-webview-mode\=' turning on or off in its buffer."
  (agent-repl-dividers-webview-fringes (current-buffer) agent-repl-frontend-webview-mode))

(add-hook 'agent-repl-frontend-webview-mode-hook #'agent-repl--dividers-on-webview-mode)
(add-hook 'window-state-change-functions #'agent-repl-dividers-sync)
(add-hook 'enable-theme-functions #'agent-repl--dividers-on-theme)
;; A reload finds the frames already showing what they show, and the web
;; views already in their mode.
(mapc #'agent-repl-dividers-sync (frame-list))
(dolist (buf (buffer-list))
  (when (or (buffer-local-value 'agent-repl-frontend-webview-mode buf)
            (eq buf (agent-repl-sidebar-view-buffer))
            (eq buf (agent-repl-webhost-buffer)))
    (agent-repl-dividers-webview-fringes buf t)))

(provide 'dividers)
;;; dividers.el ends here
