;;; test-vterm-freeze.el --- Tests for vterm-freeze.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Dedicated test file for vterm-freeze.el — covers the buffer-local
;; freeze flag, the around-advice on `vterm--invalidate', the
;; bump/unfreeze pair, and the integration where
;; `agent-repl--scroll-vterm-output' arms the freeze.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: invalidate advice respects the freeze flag ----

(ert-deftest agent-repl-test-vterm-invalidate-advice-skips-when-frozen ()
  "`agent-repl--vterm-invalidate-advice' must short-circuit ORIG-FN when
the current buffer has `agent-repl--vterm-frozen' set non-nil."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-frozen*"
      (setq agent-repl--vterm-frozen t)
      (let ((called nil))
        (agent-repl--vterm-invalidate-advice
         (lambda (&rest _) (setq called t)))
        (should-not called)))))

(ert-deftest agent-repl-test-vterm-invalidate-advice-passes-when-unfrozen ()
  "`agent-repl--vterm-invalidate-advice' must call ORIG-FN normally when
`agent-repl--vterm-frozen' is nil — non-frozen buffers keep their
normal redraw cadence."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-unfrozen*"
      (setq agent-repl--vterm-frozen nil)
      (let ((called nil))
        (agent-repl--vterm-invalidate-advice
         (lambda (&rest _) (setq called t)))
        (should called)))))

(ert-deftest agent-repl-test-vterm-invalidate-advice-forwards-args ()
  "`agent-repl--vterm-invalidate-advice' must forward its ARGS to ORIG-FN
verbatim when the buffer is not frozen."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-args*"
      (setq agent-repl--vterm-frozen nil)
      (let ((received nil))
        (agent-repl--vterm-invalidate-advice
         (lambda (&rest args) (setq received args))
         'a 'b 'c)
        (should (equal received '(a b c)))))))

;;;; ---- Tests: freeze-bump sets the flag and schedules a timer ----

(ert-deftest agent-repl-test-vterm-freeze-bump-sets-flag ()
  "`agent-repl--vterm-freeze-bump' must set the buffer-local
`agent-repl--vterm-frozen' flag in BUF."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-bump-flag*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) 'fake-timer)))
          (agent-repl--vterm-freeze-bump buf)
          (should (buffer-local-value 'agent-repl--vterm-frozen buf)))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-schedules-timer ()
  "`agent-repl--vterm-freeze-bump' must arm an unfreeze timer using
`agent-repl-vterm-freeze-duration' as the delay."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-bump-timer*"
      (let ((buf (current-buffer))
            (scheduled nil)
            (agent-repl-vterm-freeze-duration 0.25))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (delay repeat fn &rest args)
                     (setq scheduled (list delay repeat fn args))
                     'fake-timer)))
          (agent-repl--vterm-freeze-bump buf)
          (should (equal (nth 0 scheduled) 0.25))
          (should (null (nth 1 scheduled)))
          (should (eq (nth 2 scheduled) #'agent-repl--vterm-unfreeze))
          (should (equal (nth 3 scheduled) (list buf))))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-stores-timer-in-buffer ()
  "`agent-repl--vterm-freeze-bump' must stash the returned timer object
in the buffer-local `agent-repl--vterm-freeze-timer' slot so a
subsequent bump can cancel it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-bump-store*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) 'sentinel-timer)))
          (agent-repl--vterm-freeze-bump buf)
          (should (eq (buffer-local-value 'agent-repl--vterm-freeze-timer buf)
                      'sentinel-timer)))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-cancels-previous-timer ()
  "A second `agent-repl--vterm-freeze-bump' on the same buffer must
cancel the previously-scheduled timer so only one unfreeze is pending.
Re-arming on every scroll keypress is the whole point — without the
cancel, an early scroll would unfreeze mid-burst."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-bump-cancel*"
      (let ((buf (current-buffer))
            (cancelled nil)
            (next-timer 0))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _)
                     (cl-incf next-timer)
                     (intern (format "timer-%d" next-timer))))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (push timer cancelled)))
                  ((symbol-function 'timerp)
                   (lambda (obj) (and obj (symbolp obj)
                                       (string-prefix-p "timer-"
                                                        (symbol-name obj))))))
          (agent-repl--vterm-freeze-bump buf)
          (agent-repl--vterm-freeze-bump buf)
          (should (equal cancelled '(timer-1)))
          (should (eq (buffer-local-value 'agent-repl--vterm-freeze-timer buf)
                      'timer-2)))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-persist-sets-flag ()
  "With PERSIST non-nil, `agent-repl--vterm-freeze-bump' must still set
the buffer-local `agent-repl--vterm-frozen' flag — the freeze is active,
it is merely untimed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-persist-flag*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) 'fake-timer)))
          (agent-repl--vterm-freeze-bump buf t)
          (should (buffer-local-value 'agent-repl--vterm-frozen buf)))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-persist-arms-no-timer ()
  "With PERSIST non-nil, `agent-repl--vterm-freeze-bump' must NOT arm an
unfreeze timer — the freeze holds indefinitely on an UP scroll."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-persist-notimer*"
      (let ((buf (current-buffer))
            (scheduled nil))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) (setq scheduled t) 'fake-timer)))
          (agent-repl--vterm-freeze-bump buf t)
          (should-not scheduled))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-persist-nils-timer-slot ()
  "With PERSIST non-nil, `agent-repl--vterm-freeze-bump' must leave the
buffer-local timer slot nil so no stale handle lingers for the untimed
freeze."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-persist-slot*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) 'fake-timer)))
          (agent-repl--vterm-freeze-bump buf t)
          (should-not (buffer-local-value 'agent-repl--vterm-freeze-timer buf)))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-persist-cancels-previous-timer ()
  "A PERSIST bump (UP scroll) following a timed bump (DOWN scroll) must
cancel the previously-scheduled timer so the pending unfreeze cannot
fire and yank the display while the user reads history."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-persist-cancel*"
      (let ((buf (current-buffer))
            (cancelled nil)
            (next-timer 0))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _)
                     (cl-incf next-timer)
                     (intern (format "timer-%d" next-timer))))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (push timer cancelled)))
                  ((symbol-function 'timerp)
                   (lambda (obj) (and obj (symbolp obj)
                                       (string-prefix-p "timer-"
                                                        (symbol-name obj))))))
          (agent-repl--vterm-freeze-bump buf)      ; DOWN: arms timer-1
          (agent-repl--vterm-freeze-bump buf t)    ; UP: must cancel timer-1
          (should (equal cancelled '(timer-1)))
          (should-not (buffer-local-value 'agent-repl--vterm-freeze-timer buf)))))))

(ert-deftest agent-repl-test-vterm-freeze-bump-dead-buffer-noop ()
  "`agent-repl--vterm-freeze-bump' must be a no-op on a dead buffer —
no timer scheduled, no error."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-bump-dead*"))
          (scheduled nil))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq scheduled t) 'fake)))
        (agent-repl--vterm-freeze-bump buf)
        (should-not scheduled)))))

;;;; ---- Tests: unfreeze clears state and forces a redraw ----

(ert-deftest agent-repl-test-vterm-unfreeze-clears-flag ()
  "`agent-repl--vterm-unfreeze' must clear the buffer-local freeze flag."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-unfreeze-flag*"
      (let ((buf (current-buffer)))
        (setq agent-repl--vterm-frozen t)
        (cl-letf (((symbol-function 'vterm--invalidate) (lambda (&rest _) nil)))
          (agent-repl--vterm-unfreeze buf)
          (should-not (buffer-local-value 'agent-repl--vterm-frozen buf)))))))

(ert-deftest agent-repl-test-vterm-unfreeze-clears-timer-slot ()
  "`agent-repl--vterm-unfreeze' must null out the buffer-local
`agent-repl--vterm-freeze-timer' slot once the timer has fired —
otherwise a stale handle lingers and the next bump tries to cancel
an already-expired timer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-unfreeze-timer*"
      (let ((buf (current-buffer)))
        (setq agent-repl--vterm-freeze-timer 'expired)
        (cl-letf (((symbol-function 'vterm--invalidate) (lambda (&rest _) nil)))
          (agent-repl--vterm-unfreeze buf)
          (should-not (buffer-local-value 'agent-repl--vterm-freeze-timer buf)))))))

(ert-deftest agent-repl-test-vterm-unfreeze-forces-redraw-when-term-live ()
  "`agent-repl--vterm-unfreeze' must call `vterm--invalidate' to flush
any output that accumulated during the freeze — but only when
`vterm--term' is non-nil (real libvterm state is attached)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-unfreeze-flush*"
      (let ((buf (current-buffer))
            (invalidated 0))
        (setq-local vterm--term 'fake-term)
        (setq agent-repl--vterm-frozen t)
        (cl-letf (((symbol-function 'vterm--invalidate)
                   (lambda (&rest _) (cl-incf invalidated))))
          (agent-repl--vterm-unfreeze buf)
          (should (= invalidated 1)))))))

(ert-deftest agent-repl-test-vterm-unfreeze-no-redraw-when-term-nil ()
  "`agent-repl--vterm-unfreeze' must NOT call `vterm--invalidate' when
`vterm--term' is nil — there's no live libvterm state to flush from,
and the C-side invalidate would crash on a nil term."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-vterm-unfreeze-noterm*"
      (let ((buf (current-buffer))
            (invalidated 0))
        (setq-local vterm--term nil)
        (setq agent-repl--vterm-frozen t)
        (cl-letf (((symbol-function 'vterm--invalidate)
                   (lambda (&rest _) (cl-incf invalidated))))
          (agent-repl--vterm-unfreeze buf)
          (should (= invalidated 0)))))))

(ert-deftest agent-repl-test-vterm-unfreeze-dead-buffer-noop ()
  "`agent-repl--vterm-unfreeze' on a dead buffer must be a no-op — no
error, no invalidate call.  The timer callback can race with buffer
kill, so this path has to be safe."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-unfreeze-dead*"))
          (invalidated 0))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'vterm--invalidate)
                 (lambda (&rest _) (cl-incf invalidated))))
        (agent-repl--vterm-unfreeze buf)
        (should (= invalidated 0))))))

;;;; ---- Tests: scroll-vterm-output integration arms the freeze ----

(ert-deftest agent-repl-test-scroll-vterm-output-arms-freeze ()
  "`agent-repl--scroll-vterm-output' must call
`agent-repl--vterm-freeze-bump' on the live vterm buffer so that
subsequent process output won't yank the scroll position while the
user is still pressing scroll keys."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create " *test-scroll-arms-freeze*"))
          (bumped-with nil))
      (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "test-ws"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'window-start) (lambda (&rest _) 1))
                    ((symbol-function 'set-window-start) (lambda (&rest _) nil))
                    ((symbol-function 'set-window-point) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--vterm-freeze-bump)
                     (lambda (buf &optional _persist) (setq bumped-with buf))))
            (agent-repl--scroll-vterm-output -5)
            (should (eq bumped-with vterm-buf)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-scroll-vterm-output-no-bump-without-live-vterm ()
  "`agent-repl--scroll-vterm-output' must NOT call freeze-bump when
there is no live vterm buffer for the current workspace — the
`with-vterm-buf' guard should skip the body entirely."
  (agent-repl-test--with-clean-state
    (let ((bumped nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "no-such-ws"))
                ((symbol-function 'agent-repl--vterm-freeze-bump)
                 (lambda (_buf) (setq bumped t))))
        (agent-repl--scroll-vterm-output -5)
        (should-not bumped)))))

(ert-deftest agent-repl-test-scroll-up-freeze-persists ()
  "An UP scroll (negative LINES) must call `agent-repl--vterm-freeze-bump'
with PERSIST non-nil so the freeze holds indefinitely."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create " *test-scroll-up-persist*"))
          (persist-arg 'unset))
      (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "test-ws"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'window-start) (lambda (&rest _) 1))
                    ((symbol-function 'set-window-start) (lambda (&rest _) nil))
                    ((symbol-function 'set-window-point) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--vterm-freeze-bump)
                     (lambda (_buf &optional persist) (setq persist-arg persist))))
            (agent-repl--scroll-vterm-output -5)
            (should persist-arg))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest agent-repl-test-scroll-down-freeze-times-out ()
  "A DOWN scroll (positive LINES) must call `agent-repl--vterm-freeze-bump'
with PERSIST nil so the timed freeze lapses and auto-scroll resumes."
  (agent-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create " *test-scroll-down-timed*"))
          (persist-arg 'unset))
      (agent-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "test-ws"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'window-start) (lambda (&rest _) 1))
                    ((symbol-function 'set-window-start) (lambda (&rest _) nil))
                    ((symbol-function 'set-window-point) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--vterm-freeze-bump)
                     (lambda (_buf &optional persist) (setq persist-arg persist))))
            (agent-repl--scroll-vterm-output 5)
            (should-not persist-arg))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

;;;; ---- Tests: advice install/uninstall round-trip ----

(ert-deftest agent-repl-test-vterm-freeze-advice-installed-at-load ()
  "Loading `vterm-freeze.el' must leave the around-advice attached to
`vterm--invalidate' — the freeze can't take effect otherwise."
  (let ((found nil))
    (advice-mapc (lambda (adv _props)
                   (when (eq adv #'agent-repl--vterm-invalidate-advice)
                     (setq found t)))
                 'vterm--invalidate)
    (should found)))

(ert-deftest agent-repl-test-vterm-freeze-advice-disable-removes-it ()
  "`agent-repl--disable-vterm-freeze-advice' must detach the advice;
`agent-repl--enable-vterm-freeze-advice' must reattach it.  Tests
must reinstall the advice afterward to keep global state intact."
  (unwind-protect
      (progn
        (agent-repl--disable-vterm-freeze-advice)
        (let ((found nil))
          (advice-mapc (lambda (adv _props)
                         (when (eq adv #'agent-repl--vterm-invalidate-advice)
                           (setq found t)))
                       'vterm--invalidate)
          (should-not found)))
    (agent-repl--enable-vterm-freeze-advice)))

;;; test-vterm-freeze.el ends here
