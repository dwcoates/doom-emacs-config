;;; test-vterm-freeze.el --- Tests for vterm-freeze.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Dedicated test file for vterm-freeze.el — covers the buffer-local
;; freeze flag, the around-advice on `vterm--invalidate', the
;; bump/unfreeze pair, and the integration where
;; `claude-repl--scroll-vterm-output' arms the freeze.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: invalidate advice respects the freeze flag ----

(ert-deftest claude-repl-test-vterm-invalidate-advice-skips-when-frozen ()
  "`claude-repl--vterm-invalidate-advice' must short-circuit ORIG-FN when
the current buffer has `claude-repl--vterm-frozen' set non-nil."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-frozen*"
      (setq claude-repl--vterm-frozen t)
      (let ((called nil))
        (claude-repl--vterm-invalidate-advice
         (lambda (&rest _) (setq called t)))
        (should-not called)))))

(ert-deftest claude-repl-test-vterm-invalidate-advice-passes-when-unfrozen ()
  "`claude-repl--vterm-invalidate-advice' must call ORIG-FN normally when
`claude-repl--vterm-frozen' is nil — non-frozen buffers keep their
normal redraw cadence."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-unfrozen*"
      (setq claude-repl--vterm-frozen nil)
      (let ((called nil))
        (claude-repl--vterm-invalidate-advice
         (lambda (&rest _) (setq called t)))
        (should called)))))

(ert-deftest claude-repl-test-vterm-invalidate-advice-forwards-args ()
  "`claude-repl--vterm-invalidate-advice' must forward its ARGS to ORIG-FN
verbatim when the buffer is not frozen."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-args*"
      (setq claude-repl--vterm-frozen nil)
      (let ((received nil))
        (claude-repl--vterm-invalidate-advice
         (lambda (&rest args) (setq received args))
         'a 'b 'c)
        (should (equal received '(a b c)))))))

;;;; ---- Tests: freeze-bump sets the flag and schedules a timer ----

(ert-deftest claude-repl-test-vterm-freeze-bump-sets-flag ()
  "`claude-repl--vterm-freeze-bump' must set the buffer-local
`claude-repl--vterm-frozen' flag in BUF."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-bump-flag*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) 'fake-timer)))
          (claude-repl--vterm-freeze-bump buf)
          (should (buffer-local-value 'claude-repl--vterm-frozen buf)))))))

(ert-deftest claude-repl-test-vterm-freeze-bump-schedules-timer ()
  "`claude-repl--vterm-freeze-bump' must arm an unfreeze timer using
`claude-repl-vterm-freeze-duration' as the delay."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-bump-timer*"
      (let ((buf (current-buffer))
            (scheduled nil)
            (claude-repl-vterm-freeze-duration 0.25))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (delay repeat fn &rest args)
                     (setq scheduled (list delay repeat fn args))
                     'fake-timer)))
          (claude-repl--vterm-freeze-bump buf)
          (should (equal (nth 0 scheduled) 0.25))
          (should (null (nth 1 scheduled)))
          (should (eq (nth 2 scheduled) #'claude-repl--vterm-unfreeze))
          (should (equal (nth 3 scheduled) (list buf))))))))

(ert-deftest claude-repl-test-vterm-freeze-bump-stores-timer-in-buffer ()
  "`claude-repl--vterm-freeze-bump' must stash the returned timer object
in the buffer-local `claude-repl--vterm-freeze-timer' slot so a
subsequent bump can cancel it."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-bump-store*"
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _) 'sentinel-timer)))
          (claude-repl--vterm-freeze-bump buf)
          (should (eq (buffer-local-value 'claude-repl--vterm-freeze-timer buf)
                      'sentinel-timer)))))))

(ert-deftest claude-repl-test-vterm-freeze-bump-cancels-previous-timer ()
  "A second `claude-repl--vterm-freeze-bump' on the same buffer must
cancel the previously-scheduled timer so only one unfreeze is pending.
Re-arming on every scroll keypress is the whole point — without the
cancel, an early scroll would unfreeze mid-burst."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-bump-cancel*"
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
          (claude-repl--vterm-freeze-bump buf)
          (claude-repl--vterm-freeze-bump buf)
          (should (equal cancelled '(timer-1)))
          (should (eq (buffer-local-value 'claude-repl--vterm-freeze-timer buf)
                      'timer-2)))))))

(ert-deftest claude-repl-test-vterm-freeze-bump-dead-buffer-noop ()
  "`claude-repl--vterm-freeze-bump' must be a no-op on a dead buffer —
no timer scheduled, no error."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-bump-dead*"))
          (scheduled nil))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq scheduled t) 'fake)))
        (claude-repl--vterm-freeze-bump buf)
        (should-not scheduled)))))

;;;; ---- Tests: unfreeze clears state and forces a redraw ----

(ert-deftest claude-repl-test-vterm-unfreeze-clears-flag ()
  "`claude-repl--vterm-unfreeze' must clear the buffer-local freeze flag."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-unfreeze-flag*"
      (let ((buf (current-buffer)))
        (setq claude-repl--vterm-frozen t)
        (cl-letf (((symbol-function 'vterm--invalidate) (lambda (&rest _) nil)))
          (claude-repl--vterm-unfreeze buf)
          (should-not (buffer-local-value 'claude-repl--vterm-frozen buf)))))))

(ert-deftest claude-repl-test-vterm-unfreeze-clears-timer-slot ()
  "`claude-repl--vterm-unfreeze' must null out the buffer-local
`claude-repl--vterm-freeze-timer' slot once the timer has fired —
otherwise a stale handle lingers and the next bump tries to cancel
an already-expired timer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-unfreeze-timer*"
      (let ((buf (current-buffer)))
        (setq claude-repl--vterm-freeze-timer 'expired)
        (cl-letf (((symbol-function 'vterm--invalidate) (lambda (&rest _) nil)))
          (claude-repl--vterm-unfreeze buf)
          (should-not (buffer-local-value 'claude-repl--vterm-freeze-timer buf)))))))

(ert-deftest claude-repl-test-vterm-unfreeze-forces-redraw-when-term-live ()
  "`claude-repl--vterm-unfreeze' must call `vterm--invalidate' to flush
any output that accumulated during the freeze — but only when
`vterm--term' is non-nil (real libvterm state is attached)."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-unfreeze-flush*"
      (let ((buf (current-buffer))
            (invalidated 0))
        (setq-local vterm--term 'fake-term)
        (setq claude-repl--vterm-frozen t)
        (cl-letf (((symbol-function 'vterm--invalidate)
                   (lambda (&rest _) (cl-incf invalidated))))
          (claude-repl--vterm-unfreeze buf)
          (should (= invalidated 1)))))))

(ert-deftest claude-repl-test-vterm-unfreeze-no-redraw-when-term-nil ()
  "`claude-repl--vterm-unfreeze' must NOT call `vterm--invalidate' when
`vterm--term' is nil — there's no live libvterm state to flush from,
and the C-side invalidate would crash on a nil term."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer " *test-vterm-unfreeze-noterm*"
      (let ((buf (current-buffer))
            (invalidated 0))
        (setq-local vterm--term nil)
        (setq claude-repl--vterm-frozen t)
        (cl-letf (((symbol-function 'vterm--invalidate)
                   (lambda (&rest _) (cl-incf invalidated))))
          (claude-repl--vterm-unfreeze buf)
          (should (= invalidated 0)))))))

(ert-deftest claude-repl-test-vterm-unfreeze-dead-buffer-noop ()
  "`claude-repl--vterm-unfreeze' on a dead buffer must be a no-op — no
error, no invalidate call.  The timer callback can race with buffer
kill, so this path has to be safe."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-unfreeze-dead*"))
          (invalidated 0))
      (kill-buffer buf)
      (cl-letf (((symbol-function 'vterm--invalidate)
                 (lambda (&rest _) (cl-incf invalidated))))
        (claude-repl--vterm-unfreeze buf)
        (should (= invalidated 0))))))

;;;; ---- Tests: scroll-vterm-output integration arms the freeze ----

(ert-deftest claude-repl-test-scroll-vterm-output-arms-freeze ()
  "`claude-repl--scroll-vterm-output' must call
`claude-repl--vterm-freeze-bump' on the live vterm buffer so that
subsequent process output won't yank the scroll position while the
user is still pressing scroll keys."
  (claude-repl-test--with-clean-state
    (let ((vterm-buf (get-buffer-create " *test-scroll-arms-freeze*"))
          (bumped-with nil))
      (claude-repl--ws-put "test-ws" :vterm-buffer vterm-buf)
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "test-ws"))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'window-start) (lambda (&rest _) 1))
                    ((symbol-function 'set-window-start) (lambda (&rest _) nil))
                    ((symbol-function 'set-window-point) (lambda (&rest _) nil))
                    ((symbol-function 'claude-repl--vterm-freeze-bump)
                     (lambda (buf) (setq bumped-with buf))))
            (claude-repl--scroll-vterm-output -5)
            (should (eq bumped-with vterm-buf)))
        (when (buffer-live-p vterm-buf) (kill-buffer vterm-buf))))))

(ert-deftest claude-repl-test-scroll-vterm-output-no-bump-without-live-vterm ()
  "`claude-repl--scroll-vterm-output' must NOT call freeze-bump when
there is no live vterm buffer for the current workspace — the
`with-vterm-buf' guard should skip the body entirely."
  (claude-repl-test--with-clean-state
    (let ((bumped nil))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "no-such-ws"))
                ((symbol-function 'claude-repl--vterm-freeze-bump)
                 (lambda (_buf) (setq bumped t))))
        (claude-repl--scroll-vterm-output -5)
        (should-not bumped)))))

;;;; ---- Tests: advice install/uninstall round-trip ----

(ert-deftest claude-repl-test-vterm-freeze-advice-installed-at-load ()
  "Loading `vterm-freeze.el' must leave the around-advice attached to
`vterm--invalidate' — the freeze can't take effect otherwise."
  (let ((found nil))
    (advice-mapc (lambda (adv _props)
                   (when (eq adv #'claude-repl--vterm-invalidate-advice)
                     (setq found t)))
                 'vterm--invalidate)
    (should found)))

(ert-deftest claude-repl-test-vterm-freeze-advice-disable-removes-it ()
  "`claude-repl--disable-vterm-freeze-advice' must detach the advice;
`claude-repl--enable-vterm-freeze-advice' must reattach it.  Tests
must reinstall the advice afterward to keep global state intact."
  (unwind-protect
      (progn
        (claude-repl--disable-vterm-freeze-advice)
        (let ((found nil))
          (advice-mapc (lambda (adv _props)
                         (when (eq adv #'claude-repl--vterm-invalidate-advice)
                           (setq found t)))
                       'vterm--invalidate)
          (should-not found)))
    (claude-repl--enable-vterm-freeze-advice)))

;;; test-vterm-freeze.el ends here
