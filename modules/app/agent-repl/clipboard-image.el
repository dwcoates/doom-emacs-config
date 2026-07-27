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
  (let* ((dir (expand-file-name agent-repl--image-subdir (agent-repl--ws-dir ws)))
         (already-exists (file-directory-p dir)))
    (agent-repl--log ws
                     "clipboard-image: ensure image directory path=%s already-exists=%s"
                     dir already-exists)
    (make-directory dir t)
    (agent-repl--log ws "clipboard-image: image directory ready path=%s" dir)
    dir))

(defun agent-repl--image-new-path (dir &optional ws)
  "Return a fresh, non-created PNG path under DIR."
  (let ((path (expand-file-name
               (format "clip-%s-%04x.png" (format-time-string "%Y%m%d-%H%M%S")
                       (random #x10000))
               dir)))
    (agent-repl--log ws "clipboard-image: allocated capture destination dir=%s path=%s"
                     dir path)
    path))

(defun agent-repl--image-nonempty-file-p (path &optional ws)
  "Return non-nil when PATH exists and holds at least one byte."
  (let* ((exists (file-exists-p path))
         (size (and exists (file-attribute-size (file-attributes path))))
         (nonempty (and size (> size 0))))
    (agent-repl--log ws
                     "clipboard-image: inspected captured file path=%s exists=%s size=%s nonempty=%s"
                     path exists size nonempty)
    nonempty))

(defun agent-repl--image-write-flavor (as-class dest &optional ws)
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
    (agent-repl--log ws "clipboard-image: writing clipboard flavor=%s destination=%s"
                     as-class dest)
    (let* ((exit-code (agent-repl--image-call-process "osascript" "-e" script))
           (nonempty (and (eq 0 exit-code)
                          (agent-repl--image-nonempty-file-p dest ws)))
           (written (and (eq 0 exit-code) nonempty)))
      (agent-repl--log ws
                       "clipboard-image: clipboard flavor write finished flavor=%s destination=%s exit=%s nonempty=%s written=%s"
                       as-class dest exit-code nonempty written)
      written)))

(defun agent-repl--image-capture-clipboard (dest &optional ws)
  "Capture the clipboard image to DEST (a .png path); return DEST or signal.
Tries the PNG pasteboard flavor first (what a macOS screenshot provides),
then a TIFF flavor converted to PNG with `sips'.  Signals a `user-error'
when the clipboard holds no image at all."
  (agent-repl--log ws "clipboard-image: capture started destination=%s" dest)
  (let ((png-written (agent-repl--image-write-flavor "«class PNGf»" dest ws)))
    (cond
     (png-written
      (agent-repl--log ws "clipboard-image: capture selected PNG flavor destination=%s" dest)
      dest)
     ((let* ((tiff (concat (file-name-sans-extension dest) ".tiff"))
             (tiff-written (agent-repl--image-write-flavor "«class TIFF»" tiff ws))
             (sips-exit (and tiff-written
                             (agent-repl--image-call-process
                              "sips" "-s" "format" "png" tiff "--out" dest)))
             (png-written (and (eq 0 sips-exit)
                               (agent-repl--image-nonempty-file-p dest ws))))
        (agent-repl--log ws
                         "clipboard-image: TIFF capture branch tiff=%s tiff-written=%s sips-exit=%s png-written=%s"
                         tiff tiff-written sips-exit png-written)
        png-written)
      (agent-repl--log ws "clipboard-image: capture selected TIFF conversion destination=%s" dest)
      dest)
     (t
      (agent-repl--log ws "clipboard-image: capture failed destination=%s png-written=%s"
                       dest png-written)
      (user-error "agent-repl: no image found on the clipboard")))))

;;;; ---- Buffer insertion -----------------------------------------------------

(defun agent-repl--image-thumbnail (path &optional ws)
  "Return an image descriptor for PATH scaled to the thumbnail height, or nil.
Nil when this frame cannot render PNG images (e.g. a TTY frame), so callers
fall back to the plain path text."
  (let* ((graphic (display-graphic-p))
         (png-available (and graphic (image-type-available-p 'png)))
         (thumbnail (and png-available
                         (create-image path 'png nil
                                       :max-height agent-repl-image-thumbnail-max-height))))
    (agent-repl--log ws
                     "clipboard-image: thumbnail decision path=%s graphic=%s png-available=%s thumbnail-created=%s max-height=%s"
                     path graphic png-available (not (null thumbnail))
                     agent-repl-image-thumbnail-max-height)
    thumbnail))

(defun agent-repl--image-insert-token (path &optional ws)
  "Insert PATH on its own line at point, overlaying a thumbnail when possible.
The inserted buffer text is exactly PATH, so the normal text send carries
it unchanged; the thumbnail is an overlay `display' that never alters the
buffer text.  Return the overlay, or nil when no thumbnail was drawn."
  (let ((started-at-bol (bolp)))
    (unless started-at-bol (insert "\n"))
    (let ((start (point)))
      (insert path)
      (let ((overlay
             (when-let ((thumb (agent-repl--image-thumbnail path ws)))
               (let ((ov (make-overlay start (point))))
                 (overlay-put ov 'display thumb)
                 (overlay-put ov 'agent-repl-image path)
                 (overlay-put ov 'help-echo path)
                 ov))))
        (insert "\n")
        (agent-repl--log ws
                         "clipboard-image: inserted token path=%s started-at-bol=%s thumbnail-overlay=%s"
                         path started-at-bol (not (null overlay)))
        overlay))))

;;;###autoload
(defun agent-repl-attach-clipboard-image ()
  "Attach the clipboard image to this workspace's input buffer.
Writes the clipboard image (a pasted file OR a macOS screenshot) into the
workspace image dir, inserts its path at point, and overlays a thumbnail.
The path rides the normal text send, so the agent reads the image via its
Read tool.  Signals a `user-error' when the clipboard holds no image."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (dir (agent-repl--image-dir ws))
         (path (agent-repl--image-new-path dir ws))
         (dest (agent-repl--image-capture-clipboard path ws)))
    (agent-repl--image-insert-token dest ws)
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
