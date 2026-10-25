;;; splash.el --- the ZETTEL splash screen over Doom's dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; DOOM'S DASHBOARD, DRESSED AS ZETTEL.  ZETTEL is agent-repl's coming name.
;; Doom's dashboard module draws the splash from three variables, and this
;; file sets each of them rather than forking the module:
;;
;;   - `+doom-dashboard-ascii-banner-fn' draws `agent-repl-splash-banner',
;;     ZETTEL in the style of Doom's own banner: dotted letter faces over a
;;     receding floor, EMACS beneath.
;;   - `+doom-dashboard-menu-sections' is `agent-repl-splash-menu-sections',
;;     agent-repl's own shortcuts; Doom reads each one's key off the live
;;     keymaps.
;;   - `+doom-dashboard--width' is the banner's width, so Doom centers the
;;     banner and the menu on one column.
;;
;; THE MODE LINE READS "DOOM vX ZETTEL vY".  The dashboard's `mode-name' is
;; Doom's "DOOM vX"; `agent-repl-splash-install-mode-name' makes it an
;; `:eval' of `agent-repl-splash-mode-name', which adds the release the
;; daemon last pushed in the roster (`RosterVersion.text', the same release
;; the sidebar's header shows).  Before the first roster push there is no
;; release to show, and the line reads "DOOM vX ZETTEL".
;;
;; A LOAD REDRAWS AN OPEN DASHBOARD, so a deploy's reload shows the splash
;; without a restart.

;;; Code:

(require 'seq)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function +doom-dashboard--center "ext:doom-dashboard" (len s))
(declare-function +doom-dashboard-reload "ext:doom-dashboard" (&optional force))
(declare-function nerd-icons-octicon "ext:nerd-icons" (name &rest args))
(defvar +doom-dashboard-ascii-banner-fn)
(defvar +doom-dashboard-menu-sections)
(defvar +doom-dashboard--width)
(defvar +doom-dashboard-name)
(defvar +doom-dashboard-mode-hook)
(defvar doom-version)
(defvar agent-repl-roster-view)
(defvar agent-repl-roster-update-functions)

(defconst agent-repl-splash--log-scope
  '(:agent-repl-central "the splash screen is one per Emacs, not per workspace")
  "The log scope every splash record is filed under.")

(defconst agent-repl-splash-banner
  '(
    "=================   =================   =================   =================   =================   ======="
    "\\\\ . . . . . . //   // . . . . . . .\\\\  // . . . . . . .\\\\  // . . . . . . .\\\\  // . . . . . . .\\\\  // . .\\\\"
    " `======= . ._-//   ||. . .=========='  `=====. . ._====='  `=====. . ._====='  ||. . .=========='  ||. . ||"
    "        // . _//    || . .||                 ||. . ||            ||. . ||       || . .||            || . .||"
    "       // . _//     ||. . `=======.          || . .||            || . .||       ||. . `=======.     ||. . ||"
    "      // . -//      || . . . . . .||         ||. ._||            ||. ._||       || . . . . . .||    || . .||"
    "     // ._-'//      ||. . .========'         || ._-||            || ._-||       ||. . .========'    ||. ._||"
    "    // .-' //       || ._-||                 ||.-' ||            ||.-' ||       || ._-||            || ._-||"
    " .=// -'  //=====.  ||.-' `=========.        ||-'  ||            ||-'  ||       ||.-' `=========.   ||.-' `=========."
    "|| -'           ||  ||-'            ||       ||    ||            ||    ||       ||-'            ||  ||-'            ||"
    "||-'            ||  ||              ||       ||    ||            ||    ||       ||              ||  ||              ||"
    "||         .=='        `==.    .=='        `==.    .=='        `==.    .=='        `==.    .=='        `==.         ||"
    "||      .=='              `==.='              `==.='              `==.='              `==.='              `==.      ||"
    "||   .=='                  `='                 `='                 `='                 `='                   `==.   ||"
    "||.=='                                                                                                          `==.||"
    "||                                                                                                                  ||"
    "=='                                                   E M A C S                                                    `=="
    "\\   _-'                                                                                                        `-_   /"
    " `''                                                                                                              ``'")
  "ZETTEL, drawn as Doom draws DOOM, one string per line.")

(defun agent-repl-splash-banner-width ()
  "Return the banner's width: its longest line."
  (apply #'max (mapcar #'length agent-repl-splash-banner)))

(defun agent-repl-splash-draw-banner ()
  "Insert `agent-repl-splash-banner' centered, in Doom's banner face.
Every line is padded to the banner's width first, so the lines center as
one block, as Doom's own banner does."
  (let ((width (agent-repl-splash-banner-width))
        (start (point)))
    (dolist (line agent-repl-splash-banner)
      (insert (+doom-dashboard--center +doom-dashboard--width (string-pad line width)) "\n"))
    (put-text-property start (point) 'face 'doom-dashboard-banner)))

(defconst agent-repl-splash-menu-sections
  '(("Add a project"
     :icon (nerd-icons-octicon "nf-oct-file_directory" :face 'doom-dashboard-menu-title)
     :action agent-repl-add-project-workspace)
    ("New project workspace"
     :icon (nerd-icons-octicon "nf-oct-plus" :face 'doom-dashboard-menu-title)
     :action agent-repl-create-workspace-static)
    ("Open workspace"
     :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
     :action agent-repl-switch-to-project)
    ("Re-open a closed workspace"
     :icon (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title)
     :action agent-repl-open-workspace)
    ("Help"
     :icon (nerd-icons-octicon "nf-oct-question" :face 'doom-dashboard-menu-title)
     :action agent-repl-input-help))
  "The splash's shortcuts, in `+doom-dashboard-menu-sections''s shape.
Doom shows each with the key that runs its `:action'.")

(defun agent-repl-splash-release ()
  "Return the agent-repl release the daemon last pushed, or nil before one."
  (plist-get (plist-get agent-repl-roster-view :version) :text))

(defun agent-repl-splash-mode-name ()
  "Return the dashboard's mode name: Doom's version, then ZETTEL's."
  (let ((release (agent-repl-splash-release)))
    (concat (format "DOOM v%s ZETTEL" doom-version)
            (and release (concat " v" release)))))

(defun agent-repl-splash-install-mode-name ()
  "Make the current buffer's `mode-name' `agent-repl-splash-mode-name'.
An `:eval', so the release appears once the roster brings it."
  (setq mode-name '(:eval (agent-repl-splash-mode-name)))
  (force-mode-line-update))

(defun agent-repl-splash--dashboard ()
  "Return Doom's live dashboard buffer, or nil when there is none."
  (and (boundp '+doom-dashboard-name) (get-buffer +doom-dashboard-name)))

(defun agent-repl-splash--roster-updated (_roster)
  "Redraw the dashboard's mode line, which shows the roster's release."
  (when-let ((dashboard (agent-repl-splash--dashboard)))
    (with-current-buffer dashboard
      (force-mode-line-update))))

(defun agent-repl-splash-install ()
  "Dress Doom's dashboard as ZETTEL, and redraw it when one is open."
  (setq +doom-dashboard-ascii-banner-fn #'agent-repl-splash-draw-banner
        +doom-dashboard-menu-sections agent-repl-splash-menu-sections
        +doom-dashboard--width (agent-repl-splash-banner-width))
  (add-hook '+doom-dashboard-mode-hook #'agent-repl-splash-install-mode-name)
  (add-hook 'agent-repl-roster-update-functions #'agent-repl-splash--roster-updated)
  (let ((dashboard (agent-repl-splash--dashboard)))
    (agent-repl--log agent-repl-splash--log-scope "elisp.splash.install: width=%d dashboard=%s"
                     +doom-dashboard--width (and dashboard t))
    (when dashboard
      (with-current-buffer dashboard
        (agent-repl-splash-install-mode-name))
      (+doom-dashboard-reload 'force))))

(agent-repl-splash-install)

(provide 'splash)

;;; splash.el ends here
