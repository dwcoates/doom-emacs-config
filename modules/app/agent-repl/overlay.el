;;; overlay.el --- hide overlay system for vterm output -*- lexical-binding: t; -*-

;;; Variables and configuration

(defconst agent-repl--hide-overlay-line-count 4
  "Number of lines from the bottom of the vterm buffer to hide with the overlay.")

(defvar agent-repl--hide-overlay-refcount 0
  "Reference count for the hide overlay advice.
Only add advice when going from 0 to 1; only remove when going from 1 to 0.")

(defvar-local agent-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(defcustom agent-repl-hide-input-box nil
  "When non-nil, hide the Claude CLI input box in the vterm buffer."
  :type 'boolean
  :group 'agent-repl)

(defvar agent-repl--in-redraw-advice nil
  "Non-nil while the vterm redraw advice is executing, to prevent recursion.")

;;; Core overlay operations

(defun agent-repl--hide-overlay-region ()
  "Return (START . END) for the region to hide at the bottom of the current buffer.
Returns nil if the buffer is empty or the region would be degenerate.
The number of lines hidden is controlled by `agent-repl--hide-overlay-line-count'."
  (if (<= (point-max) 1)
      (progn
        (agent-repl--log-verbose (agent-repl--ws-current-name) "hide-overlay-region: nil (buffer empty)")
        nil)
    (let* ((end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line (- agent-repl--hide-overlay-line-count))
                    (line-beginning-position))))
      (if (< start end)
          (progn
            (agent-repl--log-verbose (agent-repl--ws-current-name) "hide-overlay-region: start=%d end=%d" start end)
            (cons start end))
        (agent-repl--log-verbose (agent-repl--ws-current-name) "hide-overlay-region: nil (degenerate region start=%d end=%d)" start end)
        nil))))

(defun agent-repl--create-hide-overlay ()
  "Create a new hide overlay covering the bottom lines of the current buffer.
Only acts when `agent-repl-hide-input-box' is non-nil.
The region is computed by `agent-repl--hide-overlay-region'."
  (if (not agent-repl-hide-input-box)
      (agent-repl--log-verbose (agent-repl--ws-current-name) "create-hide-overlay: skipped (hide-input-box nil)")
    (let ((region (agent-repl--hide-overlay-region)))
      (if (not region)
          (agent-repl--log-verbose (agent-repl--ws-current-name) "create-hide-overlay: skipped (no region)")
        (setq agent-repl-hide-overlay (make-overlay (car region) (cdr region) nil t nil))
        (overlay-put agent-repl-hide-overlay 'display "")
        (overlay-put agent-repl-hide-overlay 'evaporate t)
        (agent-repl--log-verbose (agent-repl--ws-current-name) "create-hide-overlay: created start=%d end=%d" (car region) (cdr region))))))

(defun agent-repl--delete-hide-overlay ()
  "Delete the hide overlay if it exists and clear the variable."
  (if (and agent-repl-hide-overlay
           (overlay-buffer agent-repl-hide-overlay))
      (progn
        (agent-repl--log-verbose (agent-repl--ws-current-name) "delete-hide-overlay: deleted")
        (delete-overlay agent-repl-hide-overlay))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "delete-hide-overlay: no overlay to delete"))
  (setq agent-repl-hide-overlay nil))

(defun agent-repl--resolve-overlay-target-buffer ()
  "Return the appropriate buffer for overlay updates.
When called from within a claude buffer, returns it directly.
Otherwise looks up the vterm buffer for the current workspace."
  (if (agent-repl--claude-buffer-p)
      (progn
        (agent-repl--log-verbose (agent-repl--ws-current-name) "resolve-overlay-target-buffer: using current claude buffer")
        (current-buffer))
    (let ((ws (agent-repl--ws-current-name)))
      (agent-repl--log-verbose ws "resolve-overlay-target-buffer: workspace lookup ws=%s" ws)
      (and ws (agent-repl--ws-get ws :vterm-buffer)))))

(defun agent-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer.
Resolves the target buffer, then deletes and recreates the overlay in it."
  (let ((buf (agent-repl--resolve-overlay-target-buffer)))
    (if (not (and buf (buffer-live-p buf)))
        (agent-repl--log-verbose (agent-repl--ws-current-name) "update-hide-overlay: skipped (buf nil or dead)")
      (agent-repl--log-verbose (agent-repl--ws-current-name) "update-hide-overlay: updating buf=%s" (buffer-name buf))
      (with-current-buffer buf
        (agent-repl--delete-hide-overlay)
        (agent-repl--create-hide-overlay)))))

;;; Advice management

(defun agent-repl--enable-hide-overlay ()
  "Enable the hide overlay advice.
Uses reference counting so multiple sessions don't clobber each other."
  (agent-repl--log (agent-repl--ws-current-name) "enable-hide-overlay refcount=%d" agent-repl--hide-overlay-refcount)
  (when (zerop agent-repl--hide-overlay-refcount)
    (advice-add 'vterm--redraw :after #'agent-repl--vterm-redraw-advice))
  (cl-incf agent-repl--hide-overlay-refcount))

(defun agent-repl--vterm-redraw-advice (&rest _)
  "Apply hide overlay after vterm redraws."
  (if agent-repl--in-redraw-advice
      (agent-repl--log-verbose (agent-repl--ws-current-name) "vterm-redraw-advice: skipped (recursion guard)")
    (let ((agent-repl--in-redraw-advice t))
      (if (not (agent-repl--claude-buffer-p))
          (agent-repl--log-verbose (agent-repl--ws-current-name) "vterm-redraw-advice: skipped (non-claude buffer)")
        (agent-repl--update-hide-overlay)))))

(defun agent-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up any existing overlay.
Uses reference counting so the advice is only removed when the last
session releases it."
  (setq agent-repl--hide-overlay-refcount
        (max 0 (1- agent-repl--hide-overlay-refcount)))
  (agent-repl--log (agent-repl--ws-current-name) "disable-hide-overlay refcount=%d" agent-repl--hide-overlay-refcount)
  (when (zerop agent-repl--hide-overlay-refcount)
    (advice-remove 'vterm--redraw #'agent-repl--vterm-redraw-advice))
  (agent-repl--delete-hide-overlay))

;;; User commands

(defun agent-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "toggle-hide-input-box -> %s" (not agent-repl-hide-input-box))
  (setq agent-repl-hide-input-box (not agent-repl-hide-input-box))
  (agent-repl--update-hide-overlay)
  (message "Claude input box hiding %s" (if agent-repl-hide-input-box "enabled" "disabled")))

;;; Display and color utilities

(defconst agent-repl--vterm-background-grey 15
  "Greyscale level (0-255) used for Claude vterm buffer backgrounds.
Shared between `agent-repl--vterm-color-advice' and buffer setup code.")

(defun agent-repl--grey-hex (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (format "#%02x%02x%02x" n n n))

(defun agent-repl--set-buffer-background (grey-level)
  "Set default and fringe background to greyscale GREY-LEVEL in current buffer."
  (let ((hex (agent-repl--grey-hex grey-level)))
    (face-remap-add-relative 'default :background hex)
    (face-remap-add-relative 'fringe :background hex)))

(defcustom agent-repl-vterm-font-scale 1.35
  "Font size multiplier for Claude output (vterm) buffers.
A number relative to the default face height, where 1.0 leaves the font
unchanged and larger values enlarge it.  Applied buffer-locally via
`face-remap-add-relative' on the `default' face when the vterm buffer is
initialized."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--apply-vterm-font-scale ()
  "Scale the `default' face height in the current buffer.
Multiplies the default face height by `agent-repl-vterm-font-scale' via
`face-remap-add-relative'.  A no-op when the scale is exactly 1.0."
  (unless (= agent-repl-vterm-font-scale 1.0)
    (face-remap-add-relative 'default :height agent-repl-vterm-font-scale)))

;; Override vterm--get-color so claude vterm buffers get a black background.
;; Solaire-mode advises this function and face-background ignores buffer-local
;; remaps, so we hardcode black for claude buffers' default background case.
(defun agent-repl--default-background-request-p (index args)
  "Return non-nil if INDEX and ARGS indicate a default background color request.
Vterm uses index -1 for the default face.  The request is for background
when neither :foreground nor :inverse-video appear in ARGS."
  (and (= index -1)
       (not (member :foreground args))
       (not (member :inverse-video args))))

(defun agent-repl--vterm-color-advice (fn index &rest args)
  "Around-advice for `vterm--get-color'.
Returns a dark background for Claude vterm buffers instead of letting
solaire-mode remap the default face."
  (let ((result (apply fn index args)))
    (if (and (agent-repl--default-background-request-p index args)
             (agent-repl--claude-buffer-p))
        (let ((override (agent-repl--grey-hex agent-repl--vterm-background-grey)))
          ;; WHY: vterm--get-color fires per cell on every redraw — historical
          ;; log analysis showed this single call site emitted ~88% of all
          ;; log lines (~134k entries per 56M log).  `agent-repl--log-verbose'
          ;; still writes to file unconditionally, so gate the call itself on
          ;; verbose mode to keep the file quiet when verbose is off.
          (when (eq agent-repl-debug 'verbose)
            (agent-repl--log-verbose (agent-repl--ws-current-name) "vterm-color-advice: claude buffer background override color=%s" override))
          override)
      result)))

(after! vterm
  (advice-add 'vterm--get-color :around #'agent-repl--vterm-color-advice))
