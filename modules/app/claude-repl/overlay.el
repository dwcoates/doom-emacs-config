;;; overlay.el --- hide overlay system for vterm output -*- lexical-binding: t; -*-

;;; Variables and configuration

(defconst claude-repl--hide-overlay-line-count 4
  "Number of lines from the bottom of the vterm buffer to hide with the overlay.")

(defvar claude-repl--hide-overlay-refcount 0
  "Reference count for the hide overlay advice.
Only add advice when going from 0 to 1; only remove when going from 1 to 0.")

(defvar-local claude-repl-hide-overlay nil
  "Overlay used to hide Claude CLI input box.")

(defcustom claude-repl-hide-input-box nil
  "When non-nil, hide the Claude CLI input box in the vterm buffer."
  :type 'boolean
  :group 'claude-repl)

(defvar claude-repl--in-redraw-advice nil
  "Non-nil while the vterm redraw advice is executing, to prevent recursion.")

;;; Core overlay operations

(defun claude-repl--hide-overlay-region ()
  "Return (START . END) for the region to hide at the bottom of the current buffer.
Returns nil if the buffer is empty or the region would be degenerate.
The number of lines hidden is controlled by `claude-repl--hide-overlay-line-count'."
  (if (<= (point-max) 1)
      (progn
        (claude-repl--log-verbose (+workspace-current-name) "hide-overlay-region: nil (buffer empty)")
        nil)
    (let* ((end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line (- claude-repl--hide-overlay-line-count))
                    (line-beginning-position))))
      (if (< start end)
          (progn
            (claude-repl--log-verbose (+workspace-current-name) "hide-overlay-region: start=%d end=%d" start end)
            (cons start end))
        (claude-repl--log-verbose (+workspace-current-name) "hide-overlay-region: nil (degenerate region start=%d end=%d)" start end)
        nil))))

(defun claude-repl--create-hide-overlay ()
  "Create a new hide overlay covering the bottom lines of the current buffer.
Only acts when `claude-repl-hide-input-box' is non-nil.
The region is computed by `claude-repl--hide-overlay-region'."
  (if (not claude-repl-hide-input-box)
      (claude-repl--log-verbose (+workspace-current-name) "create-hide-overlay: skipped (hide-input-box nil)")
    (let ((region (claude-repl--hide-overlay-region)))
      (if (not region)
          (claude-repl--log-verbose (+workspace-current-name) "create-hide-overlay: skipped (no region)")
        (setq claude-repl-hide-overlay (make-overlay (car region) (cdr region) nil t nil))
        (overlay-put claude-repl-hide-overlay 'display "")
        (overlay-put claude-repl-hide-overlay 'evaporate t)
        (claude-repl--log-verbose (+workspace-current-name) "create-hide-overlay: created start=%d end=%d" (car region) (cdr region))))))

(defun claude-repl--delete-hide-overlay ()
  "Delete the hide overlay if it exists and clear the variable."
  (if (and claude-repl-hide-overlay
           (overlay-buffer claude-repl-hide-overlay))
      (progn
        (claude-repl--log-verbose (+workspace-current-name) "delete-hide-overlay: deleted")
        (delete-overlay claude-repl-hide-overlay))
    (claude-repl--log-verbose (+workspace-current-name) "delete-hide-overlay: no overlay to delete"))
  (setq claude-repl-hide-overlay nil))

(defun claude-repl--resolve-overlay-target-buffer ()
  "Return the appropriate buffer for overlay updates.
When called from within a claude buffer, returns it directly.
Otherwise looks up the vterm buffer for the current workspace."
  (if (claude-repl--claude-buffer-p)
      (progn
        (claude-repl--log-verbose (+workspace-current-name) "resolve-overlay-target-buffer: using current claude buffer")
        (current-buffer))
    (let ((ws (+workspace-current-name)))
      (claude-repl--log-verbose ws "resolve-overlay-target-buffer: workspace lookup ws=%s" ws)
      (and ws (claude-repl--ws-get ws :vterm-buffer)))))

(defun claude-repl--update-hide-overlay ()
  "Update overlay to hide bottom lines of Claude vterm buffer.
Resolves the target buffer, then deletes and recreates the overlay in it."
  (let ((buf (claude-repl--resolve-overlay-target-buffer)))
    (if (not (and buf (buffer-live-p buf)))
        (claude-repl--log-verbose (+workspace-current-name) "update-hide-overlay: skipped (buf nil or dead)")
      (claude-repl--log-verbose (+workspace-current-name) "update-hide-overlay: updating buf=%s" (buffer-name buf))
      (with-current-buffer buf
        (claude-repl--delete-hide-overlay)
        (claude-repl--create-hide-overlay)))))

;;; Advice management

(defun claude-repl--enable-hide-overlay ()
  "Enable the hide overlay advice.
Uses reference counting so multiple sessions don't clobber each other."
  (claude-repl--log (+workspace-current-name) "enable-hide-overlay refcount=%d" claude-repl--hide-overlay-refcount)
  (when (zerop claude-repl--hide-overlay-refcount)
    (advice-add 'vterm--redraw :after #'claude-repl--vterm-redraw-advice))
  (cl-incf claude-repl--hide-overlay-refcount))

(defun claude-repl--vterm-redraw-advice (&rest _)
  "Apply hide overlay after vterm redraws."
  (if claude-repl--in-redraw-advice
      (claude-repl--log-verbose (+workspace-current-name) "vterm-redraw-advice: skipped (recursion guard)")
    (let ((claude-repl--in-redraw-advice t))
      (if (not (claude-repl--claude-buffer-p))
          (claude-repl--log-verbose (+workspace-current-name) "vterm-redraw-advice: skipped (non-claude buffer)")
        (claude-repl--update-hide-overlay)))))

(defun claude-repl--disable-hide-overlay ()
  "Disable the hide overlay advice and clean up any existing overlay.
Uses reference counting so the advice is only removed when the last
session releases it."
  (setq claude-repl--hide-overlay-refcount
        (max 0 (1- claude-repl--hide-overlay-refcount)))
  (claude-repl--log (+workspace-current-name) "disable-hide-overlay refcount=%d" claude-repl--hide-overlay-refcount)
  (when (zerop claude-repl--hide-overlay-refcount)
    (advice-remove 'vterm--redraw #'claude-repl--vterm-redraw-advice))
  (claude-repl--delete-hide-overlay))

;;; User commands

(defun claude-repl-toggle-hide-input-box ()
  "Toggle hiding of Claude CLI's input box in the vterm buffer."
  (interactive)
  (claude-repl--log (+workspace-current-name) "toggle-hide-input-box -> %s" (not claude-repl-hide-input-box))
  (setq claude-repl-hide-input-box (not claude-repl-hide-input-box))
  (claude-repl--update-hide-overlay)
  (message "Claude input box hiding %s" (if claude-repl-hide-input-box "enabled" "disabled")))

;;; Display and color utilities

(defconst claude-repl--vterm-background-grey 15
  "Greyscale level (0-255) used for Claude vterm buffer backgrounds.
Shared between `claude-repl--vterm-color-advice' and buffer setup code.")

(defun claude-repl--grey-hex (n)
  "Return a hex color string for greyscale value N (0=black, 255=white)."
  (format "#%02x%02x%02x" n n n))

(defun claude-repl--set-buffer-background (grey-level)
  "Set default and fringe background to greyscale GREY-LEVEL in current buffer."
  (let ((hex (claude-repl--grey-hex grey-level)))
    (face-remap-add-relative 'default :background hex)
    (face-remap-add-relative 'fringe :background hex)))

;; Override vterm--get-color so claude vterm buffers get a black background.
;; Solaire-mode advises this function and face-background ignores buffer-local
;; remaps, so we hardcode black for claude buffers' default background case.
(defun claude-repl--default-background-request-p (index args)
  "Return non-nil if INDEX and ARGS indicate a default background color request.
Vterm uses index -1 for the default face.  The request is for background
when neither :foreground nor :inverse-video appear in ARGS."
  (and (= index -1)
       (not (member :foreground args))
       (not (member :inverse-video args))))

(defun claude-repl--vterm-color-advice (fn index &rest args)
  "Around-advice for `vterm--get-color'.
Returns a dark background for Claude vterm buffers instead of letting
solaire-mode remap the default face."
  (let ((result (apply fn index args)))
    (if (and (claude-repl--default-background-request-p index args)
             (claude-repl--claude-buffer-p))
        (let ((override (claude-repl--grey-hex claude-repl--vterm-background-grey)))
          (claude-repl--log-verbose (+workspace-current-name) "vterm-color-advice: claude buffer background override color=%s" override)
          override)
      result)))

(after! vterm
  (advice-add 'vterm--get-color :around #'claude-repl--vterm-color-advice))
