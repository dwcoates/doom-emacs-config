;;; clipboard-image.el --- Attach clipboard images to the agent input buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Capture the system clipboard image into the workspace and drop its path
;; into the current agent input buffer, overlaid with a thumbnail.  This
;; covers the macOS screenshot-to-clipboard gesture (Cmd-Ctrl-Shift-4),
;; which puts RAW image bytes on the pasteboard (a `«class PNGf»' and
;; usually a `«class TIFF»' flavor) rather than a file on disk.
;;
;; The inserted text IS the image's file path, so it rides the normal
;; text send unchanged and the agent reads the image with its Read tool.
;; No daemon/shim/webapp changes are needed -- this is an Emacs-side,
;; input-buffer-only capability.
;;
;; Capture strategy (macOS): an AppleScript writes the clipboard's PNG
;; flavor straight to a file; when only a TIFF flavor is present, the TIFF
;; is written and then converted to PNG with `sips'.  Every shell-out goes
;; through the single external-boundary wrapper
;; `agent-repl--image-call-process', registered in
;; `agent-repl--external-boundary-functions' (core.el) so the batch test
;; harness stubs the boundary instead of shelling out.

;;; Code:

(require 'image)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))
(defvar agent-repl-input-mode-map)

(defcustom agent-repl-image-thumbnail-max-height 220
  "Max pixel height of the thumbnail overlaid on an attached image path.
Affects display only; the text sent to the agent is always the file path."
  :type 'integer
  :group 'agent-repl)

(defconst agent-repl--image-subdir ".claude/emacs/images/"
  "Workspace-relative directory captured clipboard images are written to.
Under `.claude/emacs/' so the file sits inside the workspace the agent
runs in (its Read tool resolves the path without a cross-project prompt).")

;;;; ---- External boundary ----------------------------------------------------

(defun agent-repl--image-call-process (program &rest args)
  "Run PROGRAM with ARGS synchronously; return its exit code (output discarded).
The one external-boundary wrapper for clipboard-image capture; registered
in `agent-repl--external-boundary-functions' (core.el) so tests stub it via
`cl-letf' rather than shelling out to `osascript'/`sips'."
  (apply #'call-process program nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

;;;; ---- Capture --------------------------------------------------------------

(defun agent-repl--image-dir (ws)
  "Return (creating) the captured-image directory for workspace WS."
  (let ((dir (expand-file-name agent-repl--image-subdir (agent-repl--ws-dir ws))))
    (make-directory dir t)
    dir))

(defun agent-repl--image-new-path (dir)
  "Return a fresh, non-created PNG path under DIR."
  (expand-file-name
   (format "clip-%s-%04x.png" (format-time-string "%Y%m%d-%H%M%S") (random #x10000))
   dir))

(defun agent-repl--image-nonempty-file-p (path)
  "Return non-nil when PATH exists and holds at least one byte."
  (and (file-exists-p path)
       (> (or (file-attribute-size (file-attributes path)) 0) 0)))

(defun agent-repl--image-write-flavor (as-class dest)
  "Write the clipboard's AS-CLASS flavor to DEST via AppleScript.
AS-CLASS is an AppleScript class literal such as \"«class PNGf»\".  Return
non-nil only when the write leaves a non-empty DEST."
  (let ((script (concat
                 (format "set theData to (the clipboard as %s)\n" as-class)
                 (format "set fp to open for access (POSIX file %S) with write permission\n"
                         (expand-file-name dest))
                 "set eof fp to 0\n"
                 "write theData to fp\n"
                 "close access fp")))
    (and (eq 0 (agent-repl--image-call-process "osascript" "-e" script))
         (agent-repl--image-nonempty-file-p dest))))

(defun agent-repl--image-capture-clipboard (dest)
  "Capture the clipboard image to DEST (a .png path); return DEST or signal.
Tries the PNG pasteboard flavor first (what a macOS screenshot provides),
then a TIFF flavor converted to PNG with `sips'.  Signals a `user-error'
when the clipboard holds no image at all."
  (cond
   ((agent-repl--image-write-flavor "«class PNGf»" dest) dest)
   ((let ((tiff (concat (file-name-sans-extension dest) ".tiff")))
      (and (agent-repl--image-write-flavor "«class TIFF»" tiff)
           (eq 0 (agent-repl--image-call-process
                  "sips" "-s" "format" "png" tiff "--out" dest))
           (agent-repl--image-nonempty-file-p dest)))
    dest)
   (t (user-error "agent-repl: no image found on the clipboard"))))

;;;; ---- Buffer insertion -----------------------------------------------------

(defun agent-repl--image-thumbnail (path)
  "Return an image descriptor for PATH scaled to the thumbnail height, or nil.
Nil when this frame cannot render PNG images (e.g. a TTY frame), so callers
fall back to the plain path text."
  (when (and (display-graphic-p) (image-type-available-p 'png))
    (create-image path 'png nil :max-height agent-repl-image-thumbnail-max-height)))

(defun agent-repl--image-insert-token (path)
  "Insert PATH on its own line at point, overlaying a thumbnail when possible.
The inserted buffer text is exactly PATH, so the normal text send carries
it unchanged; the thumbnail is an overlay `display' that never alters the
buffer text.  Return the overlay, or nil when no thumbnail was drawn."
  (unless (bolp) (insert "\n"))
  (let ((start (point)))
    (insert path)
    (prog1
        (when-let ((thumb (agent-repl--image-thumbnail path)))
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'display thumb)
            (overlay-put ov 'agent-repl-image path)
            (overlay-put ov 'help-echo path)
            ov))
      (insert "\n"))))

;;;###autoload
(defun agent-repl-attach-clipboard-image ()
  "Attach the clipboard image to this workspace's input buffer.
Writes the clipboard image (a pasted file OR a macOS screenshot) into the
workspace image dir, inserts its path at point, and overlays a thumbnail.
The path rides the normal text send, so the agent reads the image via its
Read tool.  Signals a `user-error' when the clipboard holds no image."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (dest (agent-repl--image-capture-clipboard
                (agent-repl--image-new-path (agent-repl--image-dir ws)))))
    (agent-repl--image-insert-token dest)
    (agent-repl--log ws "attach-clipboard-image: wrote %s" dest)
    (message "agent-repl: attached image %s" (file-name-nondirectory dest))
    dest))

;;;; ---- Keybinding -----------------------------------------------------------

;; Bound into the input mode map (defined in input.el, loaded first).  `map!'
;; is a no-op stub in the batch harness, so this binding is not test-covered.
(map! :map agent-repl-input-mode-map
      :ni "C-c C-i" #'agent-repl-attach-clipboard-image)

(provide 'clipboard-image)

;;; clipboard-image.el ends here
