;;; test-splash.el --- ERT tests for agent-repl splash.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-splash.el -f ert-run-tests-batch-and-exit
;;
;; splash.el dresses Doom's dashboard as ZETTEL.  These tests pin the
;; banner and how it is drawn, the shortcuts, the mode name with and
;; without a release, and what installing sets and redraws.  Doom's
;; dashboard module is absent under `emacs -Q', so its two helpers are
;; stubbed at this boundary.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar +doom-dashboard-name)
(defvar +doom-dashboard--width)
(defvar +doom-dashboard-ascii-banner-fn)
(defvar +doom-dashboard-menu-sections)
(defvar +doom-dashboard-mode-hook)
(defvar +doom-dashboard-functions)

(defconst agent-repl-test-splash--doom-widgets
  '(doom-dashboard-widget-banner doom-dashboard-widget-shortmenu
    doom-dashboard-widget-loaded doom-dashboard-widget-footer)
  "Doom's own dashboard widgets, in its order.")
(defvar doom-version)

(defmacro agent-repl-test-splash--with-dashboard (name &rest body)
  "Run BODY with Doom's dashboard named NAME, its helpers stubbed.
Redraws are recorded in `reloads'; centering pads to the width as Doom's does."
  (declare (indent 1))
  `(let ((+doom-dashboard-name ,name)
         (+doom-dashboard--width 80)
         (+doom-dashboard-ascii-banner-fn nil)
         (+doom-dashboard-menu-sections nil)
         (+doom-dashboard-functions agent-repl-test-splash--doom-widgets)
         (+doom-dashboard-mode-hook nil)
         (agent-repl-roster-update-functions nil)
         (doom-version "2.1.0")
         (reloads nil))
     (cl-letf (((symbol-function '+doom-dashboard--center)
                (lambda (len s) (concat (make-string (ceiling (max 0 (- len (length s))) 2) ?\s) s)))
               ((symbol-function '+doom-dashboard-reload)
                (lambda (&optional force) (push force reloads))))
       ,@body)))

;;;; ---- The banner ----

(ert-deftest agent-repl-test-splash-banner-width-is-its-longest-line ()
  "The banner's width is the length of its longest line."
  (should (= (agent-repl-splash-banner-width)
             (apply #'max (mapcar #'length agent-repl-splash-banner)))))

(ert-deftest agent-repl-test-splash-banner-names-emacs-beneath ()
  "The banner keeps Doom's E M A C S line under the letters."
  (should (seq-some (lambda (line) (string-search "E M A C S" line)) agent-repl-splash-banner)))

(ert-deftest agent-repl-test-splash-draw-banner-centers-one-block ()
  "Every line is padded to the banner's width, then centered on the dashboard's."
  (agent-repl-test-splash--with-dashboard "*doom*"
    (let* ((+doom-dashboard--width (+ 10 (agent-repl-splash-banner-width)))
           (lines (with-temp-buffer
                    (agent-repl-splash-draw-banner)
                    (split-string (buffer-string) "\n" t))))
      (should (= (length lines) (length agent-repl-splash-banner)))
      (should (cl-every (lambda (line) (= (length line) (+ 5 (agent-repl-splash-banner-width)))) lines))
      (should (equal (string-trim (car lines)) (string-trim (car agent-repl-splash-banner)))))))

(ert-deftest agent-repl-test-splash-draw-banner-wears-the-banner-face ()
  "The drawn banner wears Doom's banner face."
  (agent-repl-test-splash--with-dashboard "*doom*"
    (with-temp-buffer
      (agent-repl-splash-draw-banner)
      (should (eq (get-text-property 1 'face) 'doom-dashboard-banner)))))

;;;; ---- The shortcuts ----

(ert-deftest agent-repl-test-splash-menu-sections-in-order ()
  "The shortcuts are the five agent-repl ones, Add a project first."
  (should (equal (mapcar #'car agent-repl-splash-menu-sections)
                 '("Add a project" "New project workspace" "Open workspace"
                   "Re-open a closed workspace" "Help"))))

(ert-deftest agent-repl-test-splash-menu-actions-are-commands ()
  "Every shortcut runs an interactive command, the one its label names."
  (should (equal (mapcar (lambda (section) (plist-get (cdr section) :action))
                         agent-repl-splash-menu-sections)
                 '(agent-repl-add-project-workspace agent-repl-create-workspace-static
                   agent-repl-switch-to-project agent-repl-open-workspace agent-repl-input-help)))
  (dolist (section agent-repl-splash-menu-sections)
    (should (commandp (plist-get (cdr section) :action)))))

;;;; ---- The Settings button ----

(ert-deftest agent-repl-test-splash-settings-widget-is-a-centered-button ()
  "The Settings widget is one button, centered on the dashboard's width."
  (agent-repl-test-splash--with-dashboard "*doom*"
    (with-temp-buffer
      (agent-repl-splash-widget-settings)
      (should (equal (buffer-string)
                     (concat (make-string (ceiling (- 80 (length "  Settings  ")) 2) ?\s)
                             "  Settings  \n")))
      (should (button-at (- (point-max) 4))))))

(ert-deftest agent-repl-test-splash-settings-widget-opens-settings ()
  "Pushing the splash's Settings button opens the Settings page."
  (let (opened)
    (agent-repl-test-splash--with-dashboard "*doom*"
      (cl-letf (((symbol-function 'agent-repl-settings) (lambda () (push t opened))))
        (with-temp-buffer
          (agent-repl-splash-widget-settings)
          (push-button (- (point-max) 4)))))
    (should opened)))

(ert-deftest agent-repl-test-splash-with-settings-goes-right-after-the-banner ()
  "The Settings widget is placed right after Doom's banner widget."
  (should (equal (agent-repl-splash--with-settings agent-repl-test-splash--doom-widgets)
                 '(doom-dashboard-widget-banner agent-repl-splash-widget-settings
                   doom-dashboard-widget-shortmenu doom-dashboard-widget-loaded
                   doom-dashboard-widget-footer))))

(ert-deftest agent-repl-test-splash-with-settings-adds-it-once ()
  "A second install, as a reload makes, does not add the widget again."
  (let ((once (agent-repl-splash--with-settings agent-repl-test-splash--doom-widgets)))
    (should (equal (agent-repl-splash--with-settings once) once))))

(ert-deftest agent-repl-test-splash-with-settings-without-a-banner-is-fatal ()
  "Doom's widgets with no banner to follow signal, naming the list."
  (let (fatals)
    (cl-letf (((symbol-function 'agent-repl--fatal)
               (lambda (_ws fmt &rest args) (push (apply #'format fmt args) fatals) (error "fatal"))))
      (should-error (agent-repl-splash--with-settings '(doom-dashboard-widget-footer))))
    (should (equal fatals '("elisp.splash.widgets: no banner widget functions=(doom-dashboard-widget-footer)")))))

;;;; ---- The mode name ----

(ert-deftest agent-repl-test-splash-mode-name-with-a-release ()
  "With a release pushed, the mode name carries both versions."
  (let ((doom-version "2.1.0")
        (agent-repl-roster-view '(:version (:text "0.230.1"))))
    (should (equal (agent-repl-splash-mode-name) "DOOM v2.1.0 ZETTEL v0.230.1"))))

(ert-deftest agent-repl-test-splash-mode-name-before-a-release ()
  "Before the roster brings a release, the mode name ends at ZETTEL."
  (let ((doom-version "2.1.0")
        (agent-repl-roster-view nil))
    (should (equal (agent-repl-splash-mode-name) "DOOM v2.1.0 ZETTEL"))))

(ert-deftest agent-repl-test-splash-install-mode-name-evaluates-the-release ()
  "The installed mode name is an `:eval', so it follows the roster."
  (with-temp-buffer
    (agent-repl-splash-install-mode-name)
    (should (equal mode-name '(:eval (agent-repl-splash-mode-name))))
    (let ((doom-version "2.1.0")
          (agent-repl-roster-view '(:version (:text "0.231.0"))))
      ;; Batch Emacs's `format-mode-line' skips `:eval', so the form is run directly.
      (should (equal (eval (cadr mode-name) t) "DOOM v2.1.0 ZETTEL v0.231.0")))))

;;;; ---- Installing ----

(ert-deftest agent-repl-test-splash-install-sets-the-dashboard-variables ()
  "Installing sets Doom's banner function, shortcuts and width."
  (agent-repl-test-splash--with-dashboard "agent-repl-test-splash-no-such-buffer"
    (agent-repl-splash-install)
    (should (eq +doom-dashboard-ascii-banner-fn #'agent-repl-splash-draw-banner))
    (should (eq +doom-dashboard-menu-sections agent-repl-splash-menu-sections))
    (should (= +doom-dashboard--width (agent-repl-splash-banner-width)))
    (should (memq #'agent-repl-splash-widget-settings +doom-dashboard-functions))))

(ert-deftest agent-repl-test-splash-install-hooks-the-mode-and-roster ()
  "Installing hooks the dashboard's mode and the roster's updates."
  (agent-repl-test-splash--with-dashboard "agent-repl-test-splash-no-such-buffer"
    (agent-repl-splash-install)
    (should (memq #'agent-repl-splash-install-mode-name +doom-dashboard-mode-hook))
    (should (memq #'agent-repl-splash--roster-updated agent-repl-roster-update-functions))))

(ert-deftest agent-repl-test-splash-install-without-a-dashboard-redraws-nothing ()
  "With no dashboard open, installing redraws nothing, and logs that."
  (let (logged)
    (agent-repl-test-splash--with-dashboard "agent-repl-test-splash-no-such-buffer"
      (cl-letf (((symbol-function 'agent-repl--log)
                 (agent-repl-test--record-into logged)))
        (agent-repl-splash-install))
      (should-not reloads))
    (should (string-match-p "elisp.splash.install: width=[0-9]+ dashboard=nil" (car logged)))))

(ert-deftest agent-repl-test-splash-install-redraws-an-open-dashboard ()
  "An open dashboard is redrawn, forced, and takes the ZETTEL mode name."
  (agent-repl-test--with-temp-buffer "agent-repl-test-splash-dashboard"
    (setq mode-name "DOOM v2.1.0")
    (agent-repl-test-splash--with-dashboard "agent-repl-test-splash-dashboard"
      (agent-repl-splash-install)
      (should (equal reloads '(force)))
      (should (equal mode-name '(:eval (agent-repl-splash-mode-name)))))))

(ert-deftest agent-repl-test-splash-roster-update-redraws-the-dashboard-mode-line ()
  "A roster push redraws an open dashboard's mode line."
  (let (updated)
    (agent-repl-test--with-temp-buffer "agent-repl-test-splash-dashboard"
      (agent-repl-test-splash--with-dashboard "agent-repl-test-splash-dashboard"
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&optional _all) (push (buffer-name) updated))))
          (agent-repl-splash--roster-updated '(:version (:text "1.0.0"))))))
    (should (equal updated '("agent-repl-test-splash-dashboard")))))

(ert-deftest agent-repl-test-splash-roster-update-without-a-dashboard-is-quiet ()
  "A roster push with no dashboard open touches no mode line."
  (let (updated)
    (agent-repl-test-splash--with-dashboard "agent-repl-test-splash-no-such-buffer"
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&optional _all) (push t updated))))
        (agent-repl-splash--roster-updated nil)))
    (should-not updated)))

(provide 'test-splash)

;;; test-splash.el ends here
