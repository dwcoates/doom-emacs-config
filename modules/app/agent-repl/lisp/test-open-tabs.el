;;; test-open-tabs.el --- ERT tests for agent-repl open-tabs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-open-tabs.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The opens are driven with roster-walk rows and DECODED `OpenTabsResponse'
;; frames, the shape wire-host.el produces.  The daemon (the sender), the
;; workspace registry, the page and the roster are stubbed at their
;; boundaries; what was asked, what is held, which pages were made or
;; dropped, the lines said and the switches asked are the observations.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-open-tabs--requests nil
  "Every request sent, oldest first, as (WORKSPACES ON-FRAME ON-CLOSE).")
(defvar agent-repl-test-open-tabs--connected t "Whether the stand-in daemon takes a request.")
(defvar agent-repl-test-open-tabs--lines nil "The lines said, in order.")
(defvar agent-repl-test-open-tabs--paged nil "Workspaces that have a live page.")
(defvar agent-repl-test-open-tabs--no-page nil "Workspaces this Emacs makes no page for.")
(defvar agent-repl-test-open-tabs--dropped nil "Workspaces whose page was dropped.")
(defvar agent-repl-test-open-tabs--switches nil "Every switch asked, as (WS . TRIGGER).")
(defvar agent-repl-test-open-tabs--undrawn nil "Workspaces the bar never draws.")
(defvar agent-repl-test-open-tabs--armed 0 "How often Doom's one-time setup was armed.")
(defvar agent-repl-test-open-tabs--noted nil "Workspaces whose open placeholder learned the page is up.")
(defvar agent-repl-test-open-tabs--buffer nil "The live buffer standing in for every page.")
(defvar agent-repl-test-open-tabs--rows-noted nil "Every roster row an answer indexed, oldest first.")
(defvar agent-repl-test-open-tabs--noted-at-show nil "The rows indexed when each tab was drawn.")

(defvar agent-repl-test-open-tabs--milestones nil
  "Every startup milestone the open reached, and `:ended' for the startup's end, in order.")

(defmacro agent-repl-test-open-tabs--with (&rest body)
  "Run BODY with fresh opens in their process-start state, boundaries stubbed.
A ref id names the workspace of the same name."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-open-tabs--startup 'expected)
           (agent-repl-roster-view nil)
           (agent-repl-open-tabs-sender
            (lambda (workspaces on-frame on-close)
              (when agent-repl-test-open-tabs--connected
                (setq agent-repl-test-open-tabs--requests
                      (append agent-repl-test-open-tabs--requests
                              (list (list workspaces on-frame on-close))))
                t)))
           (agent-repl-test-open-tabs--requests nil)
           (agent-repl-test-open-tabs--connected t)
           (agent-repl-test-open-tabs--lines nil)
           (agent-repl-test-open-tabs--paged nil)
           (agent-repl-test-open-tabs--no-page nil)
           (agent-repl-test-open-tabs--dropped nil)
           (agent-repl-test-open-tabs--switches nil)
           (agent-repl-test-open-tabs--undrawn nil)
           (agent-repl-test-open-tabs--armed 0)
           (agent-repl-test-open-tabs--noted nil)
           (agent-repl-test-open-tabs--milestones nil)
           (agent-repl-test-open-tabs--rows-noted nil)
           (agent-repl-test-open-tabs--noted-at-show nil)
           (agent-repl-test-open-tabs--buffer (generate-new-buffer " *open-tabs-page*")))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-repl--phase-echo)
                      (lambda (_ws fmt &rest args)
                        (setq agent-repl-test-open-tabs--lines
                              (append agent-repl-test-open-tabs--lines
                                      (list (apply #'format fmt args))))))
                     ((symbol-function 'agent-repl--ws-by-ref-id) #'identity)
                     ((symbol-function 'agent-repl--ws-get)
                      (lambda (ws key)
                        (pcase key
                          (:ref (list :id ws :dir (concat "/w/" ws)))
                          (:frontend-buffer
                           (and (member ws agent-repl-test-open-tabs--paged)
                                agent-repl-test-open-tabs--buffer)))))
                     ((symbol-function 'agent-repl--ensure-input-buffer) #'ignore)
                     ((symbol-function 'agent-repl--frontend-precreate-webview)
                      (lambda (ws)
                        (unless (member ws agent-repl-test-open-tabs--no-page)
                          (cl-pushnew ws agent-repl-test-open-tabs--paged :test #'equal)
                          :created)))
                     ((symbol-function 'agent-repl--frontend-detach-webview)
                      (lambda (ws _buf)
                        (push ws agent-repl-test-open-tabs--dropped)
                        (setq agent-repl-test-open-tabs--paged
                              (remove ws agent-repl-test-open-tabs--paged))))
                     ((symbol-function 'agent-repl-roster-note-row)
                      (lambda (row _revision)
                        (setq agent-repl-test-open-tabs--rows-noted
                              (append agent-repl-test-open-tabs--rows-noted (list row)))))
                     ((symbol-function 'agent-repl-roster-refresh-order)
                      (lambda ()
                        (push (copy-sequence agent-repl-test-open-tabs--rows-noted)
                              agent-repl-test-open-tabs--noted-at-show)))
                     ((symbol-function 'agent-repl-roster-apply-current) #'ignore)
                     ((symbol-function 'agent-repl-roster-drawn-tab-order)
                      (lambda ()
                        (cl-remove-if (lambda (n) (member n agent-repl-test-open-tabs--undrawn))
                                      (mapcar (lambda (row) (plist-get row :name))
                                              agent-repl-open-tabs--rows))))
                     ((symbol-function 'agent-repl-host-request-switch)
                      (lambda (ws trigger)
                        (setq agent-repl-test-open-tabs--switches
                              (append agent-repl-test-open-tabs--switches
                                      (list (cons ws trigger))))
                        t))
                     ((symbol-function 'agent-repl-startup-first-hooks-arm)
                      (lambda () (cl-incf agent-repl-test-open-tabs--armed)))
                     ((symbol-function 'agent-repl-startup-reach)
                      (lambda (milestone)
                        (setq agent-repl-test-open-tabs--milestones
                              (append agent-repl-test-open-tabs--milestones (list milestone)))))
                     ((symbol-function 'agent-repl-startup-ended)
                      (lambda ()
                        (setq agent-repl-test-open-tabs--milestones
                              (append agent-repl-test-open-tabs--milestones (list :ended)))))
                     ((symbol-function 'agent-repl-open-progress-note-loaded)
                      (lambda (ws) (push ws agent-repl-test-open-tabs--noted)))
                     ;; No host unless a test stages one.
                     ((symbol-function 'agent-repl-webhost-xwidget) #'ignore))
             ,@body)
         (kill-buffer agent-repl-test-open-tabs--buffer)))))

(defun agent-repl-test-open-tabs--row (id &optional collapsed)
  "A roster-walk row for workspace ID, COLLAPSED when non-nil."
  (list :id id :name id :ref (list :id id :dir (concat "/w/" id)) :collapsed collapsed))

(defun agent-repl-test-open-tabs--walk (&rest rows)
  "Reconcile the opens with ROWS, as one roster walk."
  (agent-repl-open-tabs-reconcile rows))

(defun agent-repl-test-open-tabs--asked (&optional n)
  "The ids the Nth request (default the last) named, in its order."
  (let ((request (if n (nth n agent-repl-test-open-tabs--requests)
                   (car (last agent-repl-test-open-tabs--requests)))))
    (mapcar (lambda (w) (plist-get (plist-get w :workspace) :id)) (car request))))

(defun agent-repl-test-open-tabs--page-arm (id)
  "The page arm the last request asked for workspace ID."
  (let ((workspace (cl-find id (car (car (last agent-repl-test-open-tabs--requests)))
                            :key (lambda (w) (plist-get (plist-get w :workspace) :id))
                            :test #'equal)))
    (plist-get (plist-get workspace :page) :arm)))

(defun agent-repl-test-open-tabs--frame (frame &optional n)
  "Hand FRAME to the Nth request's stream (default the last)."
  (funcall (nth 1 (if n (nth n agent-repl-test-open-tabs--requests)
                    (car (last agent-repl-test-open-tabs--requests))))
           frame))

(defun agent-repl-test-open-tabs--close (outcome &optional n)
  "Close the Nth request's stream (default the last) with OUTCOME."
  (funcall (nth 2 (if n (nth n agent-repl-test-open-tabs--requests)
                    (car (last agent-repl-test-open-tabs--requests))))
           outcome))

(defun agent-repl-test-open-tabs--opened (id arm &optional value)
  "An `opened' frame answering workspace ID with outcome ARM and VALUE."
  (list :arm :opened
        :value (list :workspace (list :id id :dir (concat "/w/" id)) :at-ms 1
                     :outcome (list :arm arm :value value))))

(defun agent-repl-test-open-tabs--step (id arm &optional value)
  "A `step' frame for workspace ID with step ARM and VALUE."
  (list :arm :step
        :value (list :workspace (list :id id :dir (concat "/w/" id)) :at-ms 1
                     :step (list :arm arm :value value))))

(defun agent-repl-test-open-tabs--finished (ready total &rest failed)
  "A `finished' frame: READY of TOTAL, FAILED naming the failed workspaces."
  (list :arm :finished
        :value (list :ready ready :total total
                     :failed (mapcar (lambda (id) (list :workspace (list :id id) :name id))
                                     failed))))

(defun agent-repl-test-open-tabs--selected-before (id)
  "Stage a roster naming workspace ID as the selection the restart found."
  (setq agent-repl-roster-view (list :current (list :workspace (list :id id)))))

;;;; ---- Holding ----

(ert-deftest agent-repl-test-open-tabs-holds-a-tab-never-asked-for ()
  "A tab nothing has asked open is held."
  (agent-repl-test-open-tabs--with
    (should (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-holds-a-tab-while-its-open-is-in-flight ()
  "A tab whose open is unanswered is held."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-draws-a-tab-answered-ready ()
  "A ready answer releases the tab."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should-not (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-draws-a-tab-answered-failed ()
  "A failed answer releases the tab too: it draws the fault."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "a" :failed '(:reason "spawn refused")))
    ;; Assert
    (should-not (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-a-failed-answer-says-its-reason ()
  "A failed answer is one line naming its reason."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "a" :failed '(:reason "spawn refused")))
    ;; Assert
    (should (member "a: session failed to start: spawn refused." agent-repl-test-open-tabs--lines))))

(ert-deftest agent-repl-test-open-tabs-one-answer-releases-only-its-own-tab ()
  "A ready answer for one workspace never releases another's tab."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "b" :ready))
    ;; Assert
    (should (equal (list (agent-repl-open-tabs-holds-p "a") (agent-repl-open-tabs-holds-p "b"))
                   '(t nil)))))

;;;; ---- Asking ----

(ert-deftest agent-repl-test-open-tabs-one-request-names-every-row-of-a-walk ()
  "A walk asks for all its rows in one request."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Assert
    (should (equal (list (length agent-repl-test-open-tabs--requests)
                         (agent-repl-test-open-tabs--asked))
                   '(1 ("a" "b"))))))

(ert-deftest agent-repl-test-open-tabs-loads-a-visible-rows-page-as-it-asks ()
  "A visible row's page is made when it is asked for, and asked `loading'."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (equal (list agent-repl-test-open-tabs--paged
                         (agent-repl-test-open-tabs--page-arm "a"))
                   '(("a") :loading)))))

(ert-deftest agent-repl-test-open-tabs-makes-a-visible-rows-input-buffer-as-it-asks ()
  "A visible row's input buffer is made when it is asked for."
  (agent-repl-test-open-tabs--with
    (let (made)
      (cl-letf (((symbol-function 'agent-repl--ensure-input-buffer)
                 (lambda (ws) (push ws made))))
        ;; Act
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                         (agent-repl-test-open-tabs--row "b" t)))
      ;; Assert
      (should (equal made '("a"))))))

(ert-deftest agent-repl-test-open-tabs-a-page-that-fails-is-asked-as-none ()
  "A page that fails to load still lets its workspace be asked, as `none'."
  (agent-repl-test-open-tabs--with
    (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
               (lambda (_ws) (error "the host would not start")))
              ((symbol-function 'agent-repl--error) #'ignore))
      ;; Act
      (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")))
    ;; Assert
    (should (eq (agent-repl-test-open-tabs--page-arm "a") :none))))

(ert-deftest agent-repl-test-open-tabs-a-page-that-fails-is-an-error-record ()
  "A page that fails to load is recorded at ERROR."
  (agent-repl-test-open-tabs--with
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                 (lambda (_ws) (error "the host would not start")))
                ((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Act
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")))
      ;; Assert
      (should (equal errors '("elisp.open-tabs.page-failed ws=a error=the host would not start"))))))

(ert-deftest agent-repl-test-open-tabs-a-page-that-fails-costs-no-other-row ()
  "One workspace's failed page never costs another its open."
  (agent-repl-test-open-tabs--with
    (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
               (lambda (ws) (if (equal ws "a") (error "the host would not start")
                              (push ws agent-repl-test-open-tabs--paged))))
              ((symbol-function 'agent-repl--error) #'ignore))
      ;; Act
      (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                       (agent-repl-test-open-tabs--row "b")))
    ;; Assert
    (should (equal (agent-repl-test-open-tabs--asked) '("a" "b")))))

(ert-deftest agent-repl-test-open-tabs-asks-a-row-with-no-page-as-none ()
  "A row this Emacs makes no page for is asked `none'."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-test-open-tabs--no-page '("a"))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (eq (agent-repl-test-open-tabs--page-arm "a") :none))))

(ert-deftest agent-repl-test-open-tabs-asks-a-hidden-row-without-loading-its-page ()
  "A hidden row is asked for, as `none', and no page is made for it."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Assert
    (should (equal (list (agent-repl-test-open-tabs--asked) agent-repl-test-open-tabs--paged
                         (agent-repl-test-open-tabs--page-arm "a"))
                   '(("a") nil :none)))))

(ert-deftest agent-repl-test-open-tabs-does-not-ask-again-while-in-flight ()
  "A row still in flight is not asked for by the next walk."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (= (length agent-repl-test-open-tabs--requests) 1))))

(ert-deftest agent-repl-test-open-tabs-does-not-ask-again-once-open ()
  "An open row is not asked for by the next walk."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (= (length agent-repl-test-open-tabs--requests) 1))))

(ert-deftest agent-repl-test-open-tabs-asks-again-for-a-row-shown-again ()
  "A row answered hidden is asked for again once it is shown."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :hidden))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (equal (list (length agent-repl-test-open-tabs--requests)
                         (agent-repl-test-open-tabs--page-arm "a"))
                   '(2 :loading)))))

(ert-deftest agent-repl-test-open-tabs-does-not-ask-again-while-hidden ()
  "A row answered hidden is not asked for again while it stays hidden."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :hidden))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Assert
    (should (= (length agent-repl-test-open-tabs--requests) 1))))

(ert-deftest agent-repl-test-open-tabs-does-not-ask-again-once-refused ()
  "A refused row is not asked for again by the next walk."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "a" :refused '(:cause (:arm :not-yet-adopted :value nil))))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (= (length agent-repl-test-open-tabs--requests) 1))))

(ert-deftest agent-repl-test-open-tabs-leaves-rows-unasked-without-a-connection ()
  "A request that cannot be sent leaves its rows unasked."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-test-open-tabs--connected nil)
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (= (hash-table-count agent-repl-open-tabs--state) 0))))

(ert-deftest agent-repl-test-open-tabs-forgets-a-row-gone-from-the-walk ()
  "A workspace the walk no longer names is forgotten."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "b"))
    ;; Assert
    (should-not (gethash "a" agent-repl-open-tabs--state))))

;;;; ---- Hiding ----

(ert-deftest agent-repl-test-open-tabs-hiding-an-open-row-drops-its-page ()
  "An open workspace that is hidden loses its page."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--dropped '("a")))))

(ert-deftest agent-repl-test-open-tabs-a-hidden-row-keeps-its-place ()
  "An open workspace that is hidden keeps its place in the order, undrawn."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Assert
    (should-not (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-a-row-shown-again-is-held-for-its-open ()
  "A hidden workspace shown again is held until its new open is answered."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :hidden))
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-a-row-hidden-while-opening-drops-its-page ()
  "A ready answer for a workspace hidden meanwhile drops its page."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--dropped '("a")))))

(ert-deftest agent-repl-test-open-tabs-a-hidden-answer-keeps-its-place ()
  "A workspace answered hidden keeps its place in the order, undrawn."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :hidden))
    ;; Assert
    (should-not (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-an-answer-within-a-walk-leaves-the-order-to-it ()
  "A tab answered open while a walk asks leaves ordering and selection to the walk."
  (agent-repl-test-open-tabs--with
    (let ((applied 0))
      (cl-letf (((symbol-function 'agent-repl-roster-apply-current)
                 (lambda () (cl-incf applied))))
        ;; Arrange
        (setq agent-repl-open-tabs--startup 'done)
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
        ;; Act
        (let ((agent-repl-roster--walking t))
          (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))))
      ;; Assert
      (should (= applied 0)))))

(ert-deftest agent-repl-test-open-tabs-a-hidden-answer-re-derives-the-order ()
  "A hidden answer outside a walk puts its tab back in the order at once."
  (agent-repl-test-open-tabs--with
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'agent-repl-roster-refresh-order)
                 (lambda () (cl-incf refreshed))))
        ;; Arrange
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
        ;; Act
        (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :hidden)))
      ;; Assert
      (should (= refreshed 1)))))

(ert-deftest agent-repl-test-open-tabs-hidden-p-answers-for-a-collapsed-row ()
  "A workspace the walk met collapsed is hidden."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Assert
    (should (agent-repl-open-tabs-hidden-p "a"))))

(ert-deftest agent-repl-test-open-tabs-hidden-p-is-nil-for-a-workspace-never-walked ()
  "A workspace no walk met is not hidden."
  (agent-repl-test-open-tabs--with
    (should-not (agent-repl-open-tabs-hidden-p "a"))))

;;;; ---- Refusals ----

(ert-deftest agent-repl-test-open-tabs-a-refusal-holds-the-tab ()
  "A refused workspace gets no tab."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "a" :refused '(:cause (:arm :unknown-workspace :value nil))))
    ;; Assert
    (should (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-a-refusal-says-its-cause ()
  "A refusal is one line naming its cause."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "a" :refused '(:cause (:arm :unknown-workspace :value nil))))
    ;; Assert
    (should (member "a: the daemon refused to open it (unknown-workspace)."
                    agent-repl-test-open-tabs--lines))))

(ert-deftest agent-repl-test-open-tabs-a-refusal-is-an-error-record ()
  "A refusal is recorded at ERROR."
  (agent-repl-test-open-tabs--with
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Arrange
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
        ;; Act
        (agent-repl-test-open-tabs--frame
         (agent-repl-test-open-tabs--opened "a" :refused '(:cause (:arm :unknown-workspace :value nil)))))
      ;; Assert
      (should (cl-some (lambda (line) (string-prefix-p "elisp.open-tabs.refused" line)) errors)))))

;;;; ---- The stream ending ----

(ert-deftest agent-repl-test-open-tabs-a-stream-that-ends-unanswered-unasks-its-rows ()
  "Rows a closed stream left unanswered are unasked."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--close '(:error "connection reset"))
    ;; Assert
    (should-not (gethash "a" agent-repl-open-tabs--state))))

(ert-deftest agent-repl-test-open-tabs-a-stream-that-ends-asks-nothing-at-once ()
  "A closed stream does not dial the daemon again by itself."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--close '(:error "connection reset"))
    ;; Assert
    (should (= (length agent-repl-test-open-tabs--requests) 1))))

(ert-deftest agent-repl-test-open-tabs-a-link-up-asks-again-for-the-unanswered ()
  "The link-up after a stream ended unanswered asks for its rows again."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--close '(:error "connection reset"))
    ;; Act
    (agent-repl-open-tabs--ask-owed 'conn)
    ;; Assert
    (should (equal (agent-repl-test-open-tabs--asked) '("a")))))

(ert-deftest agent-repl-test-open-tabs-a-stream-ending-keeps-every-open-tab ()
  "A DAEMON-ONLY RESTART NEVER CLOSES A TAB: a stream ending after its answer
leaves the tab open."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--close '(:error "connection reset"))
    ;; Assert
    (should-not (agent-repl-open-tabs-holds-p "a"))))

(ert-deftest agent-repl-test-open-tabs-a-link-up-asks-nothing-for-open-tabs ()
  "A link-up after a daemon restart asks nothing for tabs already open."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-open-tabs--ask-owed 'conn)
    ;; Assert
    (should (= (length agent-repl-test-open-tabs--requests) 1))))

;;;; ---- The startup ----

(ert-deftest agent-repl-test-open-tabs-the-first-request-says-how-many-it-opens ()
  "The startup's request is one line counting its workspaces."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Assert
    (should (equal (car agent-repl-test-open-tabs--lines) "opening 2 workspaces…"))))

(ert-deftest agent-repl-test-open-tabs-the-startup-selects-the-first-tab-it-opens ()
  "The startup's first drawn tab answered open is selected."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "b" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("b" . startup))))))

(ert-deftest agent-repl-test-open-tabs-the-startup-keeps-its-first-selection ()
  "Later tabs opening never move the startup's selection."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "b" :ready))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("b" . startup))))))

(ert-deftest agent-repl-test-open-tabs-the-startup-never-selects-an-undrawn-tab ()
  "A tab the bar does not draw is never the startup's selection."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-test-open-tabs--undrawn '("a"))
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (null agent-repl-test-open-tabs--switches))))

;;;; ---- The startup keeps the selection before the restart ----

(ert-deftest agent-repl-test-open-tabs-the-startup-lands-on-the-selection-before-the-restart ()
  "The roster's `current' is the startup's landing, though another tab opened first."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "b" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("b" . startup))))))

(ert-deftest agent-repl-test-open-tabs-the-startup-selects-nothing-before-the-selection-opens ()
  "A tab opened ahead of the selection before the restart is not selected."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (null agent-repl-test-open-tabs--switches))))

(ert-deftest agent-repl-test-open-tabs-the-startup-keeps-choosing-while-the-selection-opens ()
  "The startup is still choosing while it waits for the selection before the restart."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (agent-repl-open-tabs-choosing-p))))

(ert-deftest agent-repl-test-open-tabs-a-hidden-selection-lands-on-the-first-drawn-tab ()
  "A selection in a collapsed section has no tab, so the first drawn tab is the landing."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b" t))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-open-tabs-an-undrawn-selection-lands-on-the-first-drawn-tab ()
  "A selection the bar does not draw is not waited for."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-test-open-tabs--undrawn '("b"))
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-open-tabs-a-selection-outside-the-walk-lands-on-the-first-drawn-tab ()
  "A selection naming no row of the walk is not waited for."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "gone")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-open-tabs-a-refused-selection-lands-on-a-tab-already-open ()
  "The selection the startup waited for is refused, so it lands on a tab already drawn."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "b" :refused '(:cause (:arm :unknown-workspace :value nil))))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-open-tabs-a-refused-selection-with-nothing-open-still-waits ()
  "The selection is refused before any tab is drawn, so nothing is selected yet."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "b" :refused '(:cause (:arm :unknown-workspace :value nil))))
    ;; Assert
    (should (null agent-repl-test-open-tabs--switches))))

(ert-deftest agent-repl-test-open-tabs-a-refused-selection-lands-on-the-next-tab-drawn ()
  "Once the selection is refused, the next tab drawn is the landing."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--selected-before "b")
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "b" :refused '(:cause (:arm :unknown-workspace :value nil))))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--switches '(("a" . startup))))))

(ert-deftest agent-repl-test-open-tabs-the-startups-landing-is-an-info-record-naming-its-source ()
  "The landing is recorded at INFO with the source that decided it."
  (agent-repl-test-open-tabs--with
    (let (infos)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Arrange
        (agent-repl-test-open-tabs--selected-before "b")
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                         (agent-repl-test-open-tabs--row "b"))
        ;; Act
        (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "b" :ready)))
      ;; Assert
      (should (member "elisp.open-tabs.startup-lands ws=b source=roster" infos)))))

(ert-deftest agent-repl-test-open-tabs-a-landing-with-no-selection-names-the-first-drawn-source ()
  "With no selection before the restart, the landing record names `first-drawn'."
  (agent-repl-test-open-tabs--with
    (let (infos)
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
        ;; Arrange
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
        ;; Act
        (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready)))
      ;; Assert
      (should (member "elisp.open-tabs.startup-lands ws=a source=first-drawn" infos)))))

(ert-deftest agent-repl-test-open-tabs-the-startup-is-choosing-until-it-selects ()
  "Until the startup selects, the frame has no workspace to move to."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (should (agent-repl-open-tabs-choosing-p))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should-not (agent-repl-open-tabs-choosing-p))))

(ert-deftest agent-repl-test-open-tabs-a-tab-opened-in-the-startup-reaches-the-first-tab ()
  "Every tab the startup opens reaches the first-tab milestone, which keeps only the first."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "b" :ready))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--milestones '(:first-tab)))))

(ert-deftest agent-repl-test-open-tabs-a-later-open-reaches-no-milestone ()
  "A tab opened after the startup is no startup milestone."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-open-tabs--startup 'done)
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should-not agent-repl-test-open-tabs--milestones)))

(ert-deftest agent-repl-test-open-tabs-a-later-open-selects-nothing ()
  "A tab opened after the startup is not selected by the open."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-open-tabs--startup 'done)
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (null agent-repl-test-open-tabs--switches))))

(ert-deftest agent-repl-test-open-tabs-the-startups-finish-ends-it ()
  "The startup ends with its request's finish."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 1 1))
    ;; Assert
    (should-not (agent-repl-open-tabs-starting-p))))

(ert-deftest agent-repl-test-open-tabs-the-startups-finish-says-all-ready ()
  "A finish with no failure says every workspace is ready."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 1 1))
    ;; Assert
    (should (equal (car (last agent-repl-test-open-tabs--lines)) "all 1 workspaces ready."))))

(ert-deftest agent-repl-test-open-tabs-the-startups-finish-names-the-failed ()
  "A finish with failures names them."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")
                                     (agent-repl-test-open-tabs--row "b"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 1 2 "b"))
    ;; Assert
    (should (equal (car (last agent-repl-test-open-tabs--lines))
                   "1 of 2 workspaces ready; b failed to start."))))

(ert-deftest agent-repl-test-open-tabs-the-startups-finish-arms-doom-setup ()
  "The startup's finish arms Doom's one-time setup."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 1 1))
    ;; Assert
    (should (= agent-repl-test-open-tabs--armed 1))))

(ert-deftest agent-repl-test-open-tabs-doom-setup-waits-for-the-startups-finish ()
  "Nothing is armed while the startup's request is still answering."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (= agent-repl-test-open-tabs--armed 0))))

(ert-deftest agent-repl-test-open-tabs-a-later-finish-says-nothing ()
  "A finish of a request after the startup says no summary."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-open-tabs--startup 'done)
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 1 1))
    ;; Assert
    (should (null agent-repl-test-open-tabs--lines))))

(ert-deftest agent-repl-test-open-tabs-a-startup-stream-that-ends-ends-the-startup ()
  "The startup ends when its stream ends before its finish."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--close '(:error "connection reset"))
    ;; Assert
    (should-not (agent-repl-open-tabs-starting-p))))

(ert-deftest agent-repl-test-open-tabs-every-end-of-the-startup-tells-the-milestones ()
  "However the startup ends, its end is the all-tabs milestone."
  (dolist (end (list (lambda () (agent-repl-test-open-tabs--walk))
                     (lambda ()
                       (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
                       (agent-repl-test-open-tabs--close '(:error "connection reset")))
                     (lambda ()
                       (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
                       (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 0 1)))))
    (agent-repl-test-open-tabs--with
      ;; Act
      (funcall end)
      ;; Assert
      (should (equal agent-repl-test-open-tabs--milestones '(:ended))))))

(ert-deftest agent-repl-test-open-tabs-an-empty-roster-ends-the-startup ()
  "A registry with no workspaces still ends the startup."
  (agent-repl-test-open-tabs--with
    ;; Act
    (agent-repl-test-open-tabs--walk)
    ;; Assert
    (should-not (agent-repl-open-tabs-starting-p))))

(ert-deftest agent-repl-test-open-tabs-an-unsent-startup-stays-expected ()
  "A startup request that could not be sent is the startup still to come."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-test-open-tabs--connected nil)
    ;; Act
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Assert
    (should (eq agent-repl-open-tabs--startup 'expected))))

;;;; ---- The row rides the answer ----

(ert-deftest agent-repl-test-open-tabs-a-ready-answer-indexes-its-row ()
  "A ready answer's roster row is indexed."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready '(:row (:name "a"))))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--rows-noted '((:name "a"))))))

(ert-deftest agent-repl-test-open-tabs-the-row-is-indexed-before-the-tab-draws ()
  "The tab is drawn with the answer's row already indexed."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (setq agent-repl-open-tabs--startup 'done)
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready '(:row (:name "a"))))
    ;; Assert
    (should (equal (car agent-repl-test-open-tabs--noted-at-show) '((:name "a"))))))

(ert-deftest agent-repl-test-open-tabs-a-failed-answer-indexes-its-row ()
  "A failed answer's roster row is indexed too: it draws the failure."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame
     (agent-repl-test-open-tabs--opened "a" :failed '(:reason "x" :row (:name "a"))))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--rows-noted '((:name "a"))))))

(ert-deftest agent-repl-test-open-tabs-a-hidden-answer-indexes-no-row ()
  "A hidden answer carries no row and indexes none."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a" t))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :hidden))
    ;; Assert
    (should (null agent-repl-test-open-tabs--rows-noted))))

;;;; ---- Lines ----

(ert-deftest agent-repl-test-open-tabs-says-each-step-as-the-design-lists-it ()
  "Every step arm is one line, worded as the contract lists it."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    (setq agent-repl-test-open-tabs--lines nil)
    ;; Act
    (dolist (step '((:starting-session) (:waking) (:resuming) (:vendor-retrying (:attempt 2))
                    (:vendor-rejected (:cause "bad key")) (:vendor-failed) (:cold-gate)
                    (:offline) (:served) (:page-connected) (:conversation-read)
                    (:resuming-cut-turn) (:page-drawn)))
      (agent-repl-test-open-tabs--frame
       (agent-repl-test-open-tabs--step "a" (car step) (cadr step))))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--lines
                   '("a: starting session…"
                     "a: waking from sleep…"
                     "a: resuming the conversation…"
                     "a: Claude did not start, retrying (attempt 2)…"
                     "a: Claude refused to start: bad key."
                     "a: Claude failed to start after 10 minutes."
                     "a: needs your answer to resume (large context)."
                     "a: offline, waiting for the network…"
                     "a: session up, waiting for its page…"
                     "a: page connected, drawing the conversation…"
                     "a: conversation read."
                     "a: resuming the turn the restart cut…"
                     "a: page drawn.")))))

(ert-deftest agent-repl-test-open-tabs-a-drawn-page-tells-its-open-placeholder ()
  "The page-drawn step tells the workspace's open placeholder the page is up."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--step "a" :page-drawn))
    ;; Assert
    (should (equal agent-repl-test-open-tabs--noted '("a")))))

(ert-deftest agent-repl-test-open-tabs-a-ready-answer-says-ready ()
  "A ready answer is one line."
  (agent-repl-test-open-tabs--with
    ;; Arrange
    (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
    ;; Act
    (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--opened "a" :ready))
    ;; Assert
    (should (equal (car (last agent-repl-test-open-tabs--lines)) "a: ready."))))

(ert-deftest agent-repl-test-open-tabs-an-unknown-frame-is-an-error ()
  "A frame arm this build does not know is recorded at ERROR."
  (agent-repl-test-open-tabs--with
    (let (errors)
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors))))
        ;; Arrange
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
        ;; Act
        (agent-repl-test-open-tabs--frame '(:arm :mystery :value nil)))
      ;; Assert
      (should (equal errors '("elisp.open-tabs.unknown-frame arm=:mystery"))))))

;;;; ---- The host on screen while the startup chooses ----

(defmacro agent-repl-test-open-tabs--with-startup-view (current &rest body)
  "Run BODY with a host, the frame standing on CURRENT (a placeholder when it
starts with \"pseudo\"), and the selected window's buffer recorded in `shown'."
  (declare (indent 1))
  `(let ((shown nil))
     (cl-letf (((symbol-function 'agent-repl-webhost-xwidget) (lambda () 'host-xw))
               ((symbol-function 'agent-repl--ws-current-name) (lambda () ,current))
               ((symbol-function 'agent-repl--pseudo-workspace-name-p)
                (lambda (ws) (and ws (string-prefix-p "pseudo" ws))))
               ((symbol-function 'agent-repl--frontend-make-slot-buffer)
                (lambda (name _xw) (get-buffer-create name)))
               ((symbol-function 'set-window-buffer)
                (lambda (_w buf &rest _) (setq shown (buffer-name buf)))))
       (unwind-protect
           (progn ,@body)
         (when-let ((b (get-buffer agent-repl-open-tabs--startup-view-name)))
           (kill-buffer b))))))

(ert-deftest agent-repl-test-open-tabs-the-startup-shows-the-host-on-the-placeholder ()
  "While the frame stands on the placeholder, the startup puts the host on screen."
  (agent-repl-test-open-tabs--with
    (agent-repl-test-open-tabs--with-startup-view "pseudo-main"
      ;; Act
      (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
      ;; Assert
      (should (equal shown agent-repl-open-tabs--startup-view-name)))))

(ert-deftest agent-repl-test-open-tabs-the-startup-leaves-a-users-workspace-alone ()
  "A frame already on a workspace of the user's keeps what it shows."
  (agent-repl-test-open-tabs--with
    (agent-repl-test-open-tabs--with-startup-view "a"
      ;; Act
      (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
      ;; Assert
      (should (null shown)))))

(ert-deftest agent-repl-test-open-tabs-the-startup-shows-nothing-without-a-host ()
  "With no host (no page was loaded) the placeholder keeps what it shows."
  (agent-repl-test-open-tabs--with
    (agent-repl-test-open-tabs--with-startup-view "pseudo-main"
      (cl-letf (((symbol-function 'agent-repl-webhost-xwidget) #'ignore))
        ;; Act
        (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a")))
      ;; Assert
      (should (null shown)))))

(ert-deftest agent-repl-test-open-tabs-a-later-open-shows-no-host ()
  "Only the startup puts the host on screen."
  (agent-repl-test-open-tabs--with
    (agent-repl-test-open-tabs--with-startup-view "pseudo-main"
      ;; Arrange
      (setq agent-repl-open-tabs--startup 'done)
      ;; Act
      (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
      ;; Assert
      (should (null shown)))))

(ert-deftest agent-repl-test-open-tabs-the-startups-end-drops-its-view ()
  "The view that held the host on screen is dropped when the startup ends."
  (agent-repl-test-open-tabs--with
    (agent-repl-test-open-tabs--with-startup-view "pseudo-main"
      ;; Arrange
      (agent-repl-test-open-tabs--walk (agent-repl-test-open-tabs--row "a"))
      ;; Act
      (agent-repl-test-open-tabs--frame (agent-repl-test-open-tabs--finished 1 1))
      ;; Assert
      (should-not (get-buffer agent-repl-open-tabs--startup-view-name)))))

(provide 'test-open-tabs)
;;; test-open-tabs.el ends here
