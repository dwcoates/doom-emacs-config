;;; config/splash/config.el -*- lexical-binding: t; -*-
;;
;; Our own splash screen on Doom's dashboard.
;;
;; Doom's dashboard draws its widgets from `+doom-dashboard-functions'.  The
;; banner widget below replaces Doom's, which draws the DOOM ASCII art and, in
;; a GUI frame, lays Doom's logo image over it; ours draws the same art in
;; both.  Doom's footer (a link to Doom's GitHub) goes, and Doom's menu and
;; load-time line stay.  `:ui doom-dashboard' loads before `:config' modules,
;; so its defaults are in place when this replaces them.

(defconst +dwc/splash-banner
  '("███████╗███╗   ███╗ █████╗  ██████╗███████╗"
    "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
    "█████╗  ██╔████╔██║███████║██║     ███████╗"
    "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
    "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
    "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
  "The splash screen's banner art, one string per line.")

(defconst +dwc/splash-tagline "home of agent-repl"
  "The line drawn under `+dwc/splash-banner'.")

(defun +dwc/splash-insert-centered (lines face)
  "Insert LINES centered as one block in the dashboard, in FACE.
Each line is padded to the longest, so the block keeps its left edge
rather than each line centering on its own."
  (let ((longest (apply #'max (mapcar #'string-width lines))))
    (dolist (line lines)
      (insert (propertize
               (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (- longest (string-width line)) ?\s)))
               'face face)
              "\n"))))

(defun +dwc/splash-widget-banner ()
  "Draw the splash banner and tagline, then the banner's bottom padding.
The padding's top value is Doom's own to apply: it centers the whole
dashboard vertically with it."
  (+dwc/splash-insert-centered +dwc/splash-banner 'doom-dashboard-banner)
  (insert "\n")
  (+dwc/splash-insert-centered (list +dwc/splash-tagline) 'doom-dashboard-menu-desc)
  (insert (make-string (cdr +doom-dashboard-banner-padding) ?\n)))

(setq +doom-dashboard-functions
      '(+dwc/splash-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))
