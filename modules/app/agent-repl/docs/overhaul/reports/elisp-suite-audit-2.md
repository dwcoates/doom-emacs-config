# Elisp integration-suite adversarial audit 2 (2026-09-01)

Fresh-context fable auditor, pointed at docs/overhaul/elisp.md, elisp-fanout.md
(ledger through R-VERBS-SUITE), the Emacs-facing passages of daemon.md/webapp.md,
the consumed protos and the merged suite at 0677708d1. Teamlead triage: ALL 46
findings accepted for R-SUITE-2. Production defects surfaced by the audit:
#1 (roster must re-subscribe on promotion), #32 (SubmitPrompt's
transferring_away / not_yet_adopted route to `agent-repl-host-handle-refusal`
exactly as verbs.el does — ruled by the teamlead for consistency with fanout §7
"every per-workspace rpc"), #26 (verify the fork-without-parent guard) →
R-AUDIT2-PROD after R-STABILITY and R-DEADCODE merge.

# Elisp integration-suite adversarial audit 2

Scope note: audit 1's 92 findings are all pinned at the current tip (verified test by test) and are not repeated. Every finding below is new or concerns behavior that landed after audit 1 (final handover sequence, typed refusal arms, cold start, composer image blocks, scoped merge-queue, drain-cause text).

## test-integration-link.el

1. MISSING — roster does not follow a promotion. elisp.md FINAL HANDOVER SEQUENCE: "Roster and daemon-link then follow the successor's address as the current daemon (promotion)"; fanout §6 "PROMOTE the successor to primary silently (no down/up hooks)". Production: `agent-repl-link--promote-successor` runs no hooks and `agent-repl-connect-close`s the old conn, which cancels the roster stream as `(:cancelled)`; roster.el re-subscribes only on `agent-repl-link-up-functions`, so after every handover Emacs has NO roster stream. Script: real-hooks link on primary, announce successor, end the primary's daemon stream; assert the successor lists one `roster` subscriber. This test fails today and is the highest-value gap in the suite.

2. MISSING — clean end frame on WatchDaemon is link-down. fanout §3 "`(:ended)` ... for a standing stream the caller treats it as a failure"; daemon-link.el `--handle-close` documents it. Script `/_fake/end daemon` with no error; assert `elisp.link.down` WARN, down hooks run, and reconnect stands a stream on a restarted daemon. Only the abort path is pinned.

3. WEAK — `drain_cancelled` clears only `agent-repl-link-drain` in the assertion. fanout §14 scenario 8 "drain_cancelled → removed" names the indicator. Add `(should (null agent-repl-link-drain-segment))`.

4. MISSING — successor dies before promotion keeps the OLD link up and forgets the successor. fanout §6 dual attach + daemon-link.el "the old daemon still owns whatever it has not transferred, so the link is not down". The log test ends the successor but asserts only the later `elisp.link.down`; add a case asserting `(agent-repl-link-up-p)`, `(agent-repl-link-primary)` unchanged, `agent-repl-link-successor` nil, `elisp.link.successor-stream-lost` ERROR, and no down hook.

5. MISSING — `scheduled_drain{maintenance}` cause never announced. endpoint_watch_daemon.proto DaemonShutdownScheduledDrain carries a DrainReason with three arms; the table drives deploy (scheduled) and operator (immediate) only. Add `scheduledDrain{reason:{maintenance}}` and assert the indicator names maintenance.

## test-integration-host.el

6. MISSING — `transferring_away{address}` refusal redial. fanout §7 HANDOVER REDIAL / FINAL HANDOVER SEQUENCE and elisp.md "ordering is enforced BY REFUSAL ... a lagging client self-heals from the refusal". `agent-repl-host-handle-refusal` exists and is untested at integration level. Script the primary's `SelectWorkspace` as `{error:{transferringAway:{address:<successor>}}}` with NO prior announcement; call `agent-repl-host-select`; assert `elisp.host.redial` INFO, a WatchDaemon subscriber appears on the successor, `AdoptHostWorkspace` lands on the successor, `agent-repl-host-conn` becomes the successor, and the old host stream is cancelled.

7. MISSING — `not_yet_adopted` retry on acceptance. fanout §7 "`not_yet_adopted` → INFO, retry the adopt once the successor's WatchDaemon is accepted". Script successor `AdoptHostWorkspace` to answer `{error:{notYetAdopted:{}}}` once (script, then re-script success after the first call is recorded); push `transferred`; assert `elisp.host.not-yet-adopted` INFO, a second `AdoptHostWorkspace` call, and eventual re-subscribe on the successor. Also cover `agent-repl-host--adopt-on-acceptance`: call `agent-repl-host-handle-refusal` with `:not-yet-adopted` while no successor is standing, then announce one; assert exactly one adopt lands after acceptance.

8. MISSING — the webview redial after adoption. fanout §7 "host.el updates the workspace's `:conn` to the successor FIRST and then calls `agent-repl-frontend-reload-webview`, so the webview navigates to `http://<successor>/?workspace=<id>&dir=<dir>`". No transfer test observes the reload or its URL. Mount the webview (as the URL test does), push `transferred`, and assert `agent-repl-itest-webview-urls` gains exactly one URL naming the successor's address; capture `agent-repl-frontend-reload-webview` via `cl-letf` that records `(agent-repl-host-conn ws)` at call time and assert it is already the successor.

9. MISSING — `transferring_away` without `address` is a breach. host.el logs `elisp.host.transferring-away-without-address` ERROR; the proto makes `address` a plain string, so an empty one is the daemon's zero value. Script `{error:{transferringAway:{}}}` on Select; assert the ERROR record and no dial (successor still nil, no new WatchDaemon anywhere).

10. MISSING — per-workspace ownership during dual attach. daemon.md handover step 3 "each workspace's updates flow ONLY from the daemon that currently owns it". Subscribe TWO workspaces; push `transferred` for A only; assert B's host stream still stands on the primary, no `AdoptHostWorkspace` for B, and a later host push for B on the primary still updates B's gate.

11. WEAK — `reload_webapp` "for that workspace only" (scenario 7) is asserted with one workspace, so a reload-all implementation passes. Subscribe two workspaces, push `reloadWebapp` for A, assert the recorded list is exactly `(A)`.

12. MISSING — host stream ended by the producer. fanout §3 "an end frame or process death on a standing stream is always a failure". `/_fake/end host <id>` (clean and `abort`); assert an ERROR-level host.el record, `agent-repl-host-state` kept, and `agent-repl-host-conn`/stream marked gone. Nothing pins host.el's on-close at all.

13. MISSING — two oneof arms set is a breach. fanout §14 scenario 9 "two arms → ERROR"; §0 "a oneof with two arms set". The fake refuses this before the wire (protojson), so pin the decoder directly like the `hibernated` pin: `(agent-repl-wire-decode-watch-host-workspace-response '((host . ((none . nil) (existing . ...) (naming . nil)))))` → `agent-repl-wire-error`; likewise a live arm with both `open` and `merging`.

14. MISSING — non-handover Select refusals. §0 "every logical branch ... logs"; landing 4 relay "the host decoder accepts every new `<Rpc>Error` arm". Script Select `{error:{workspaceRefMismatch:{registryDir:"/x"}}}` and `{error:{unknownWorkspace:{}}}`; assert a WARN/ERROR record whose context carries the arm keyword and `/x`, and that `agent-repl-host-last-selected-id` is NOT updated on a refusal (today the test asserts it is set after a success only).

15. WEAK — the unfocused-banner test stubs `agent-repl--notify` and asserts the message text only; the harness's production fake backend (`agent-repl-itest-notifications`) is installed and unused here. Assert through the backend record (WS, title, message, activate) so the arity contract R-CLICK fixed is pinned on this path too.

16. WEAK — `agent-repl-host-display-title` is asserted, but fanout §7 says titles NAME THE BUFFERS. Assert the input buffer (or webview buffer) name changes after the `naming.title` push, or record the production rename seam; a display-title accessor alone passes with no buffer ever renamed.

## test-integration-roster.el

17. MISSING — a row that LEAVES the roster tears its tab down. E5 "nuked rows leave the roster"; R8 "Tabs derive from `closed = false` rows"; roster.el's reconcile documents "tears down every roster-owned tab whose row is gone". Push rows A,B; push A only; assert B's tab is not live and A's is.

18. WEAK — tab creation asserts `agent-repl--ws-by-ref-id` only. fanout §8 "workspace.el creates it with `:ref`, `:dir` = ref.dir, `:name`". Assert `(agent-repl--ws-get ws :project-dir)` equals the row ref's `dir` and `(agent-repl-host-ref ws)` equals the row ref (id and dir).

19. WEAK — reaction hook REGISTRATION is never pinned. Every finish-edge and attention test binds `agent-repl-roster-finish-functions`/`-update-functions` to exactly the one consumer it tests, so a production that dropped its `add-hook` (roster.el:480-482, prompt-queue.el:281, status.el:818) passes. Add one test asserting the GLOBAL (default) value of `agent-repl-roster-finish-functions` contains the four registered reactions and `agent-repl-roster-update-functions` contains `agent-repl-status-sync-attention`.

20. MISSING — closed teardown is idempotent. fanout §8 "`closed` true → ensure no tab (teardown is idempotent)". Push the closed row twice (and once for a workspace never opened); assert no error record and no second `elisp.roster.tab-teardown`.

21. MISSING — a `current` naming a row that is `closed = true` or absent from the roster. sidebar.proto: current is "the last SelectWorkspace the daemon received", which can lag a close. Assert no switch, no signal, and no `push-invalid` (a switch to a torn-down tab would call `agent-repl--ws-switch` on a dead workspace).

22. MISSING — two status arms set (decoder pin), companion to #13: `agent-repl-wire-decode-...roster-row` with both `ready` and `thinking` → `agent-repl-wire-error`. Scenario 9 lists it explicitly; only unset and unknown are covered.

23. WEAK — the banner reaction test asserts message text only; fanout §8 (1) is the UNFOCUSED banner. Stub `agent-repl--emacs-focused-p` → t and assert NO banner is recorded, else the focus guard is unpinned.

## test-integration-verbs.el

24. MISSING — handover arms on verb acks route to host.el silently. verbs.el `agent-repl-verbs--handover-arms` + fanout §7 HANDOVER REDIAL ("on `transferring_away{address}` (`agent-repl-host-handle-refusal WS ARM-PLIST`)"). Script `MergeWorkspace` `{error:{transferringAway:{address:<successor>}}}`; assert `elisp.verbs.merge-handover-refusal` INFO, NO `merge refused` message, and `AdoptHostWorkspace` landing on the successor. Same for `notYetAdopted` (INFO, no message, adopt retried once a successor stands).

25. WEAK — the generic refusal test asserts only the `elisp.verbs.merge-refused` operation. §0 "Dynamic values go in the context"; verbs.el logs `arm=%S fields=%S`. Assert the record carries `:already-queued`, and add one payload-bearing arm (`{error:{workspaceRefMismatch:{registryDir:"/x"}}}` on Close, or `{error:{baseRefUnresolved:{ref:"nope"}}}` on Create) asserting `/x`/`nope` appear in the context and in the `message` text ("close refused: workspace-ref-mismatch ...").

26. MISSING — `fork` without `parent` is refused before send. endpoint_create_workspace.proto "a fork without a parent is unrepresentable". Call `agent-repl-verb-create repo :standard :fork t`; assert a signal (`agent-repl-wire-error` or `user-error`) and zero CreateWorkspace calls. Check the verb's guard first: if it silently drops the fork, that is a WRONG-class production gap to report.

27. MISSING — `now` without a reason is refused before send. endpoint_update_shutdown_schedule.proto UpdateShutdownScheduleNow "Why — REQUIRED". `(agent-repl-verb-shutdown-schedule (list :arm :now))` → signal, zero calls. Only the blank operator note is pinned.

28. MISSING — `evict` without a workspace is refused before send (UpdateMergeQueueEvict.workspace is a non-optional message; §0 "an incomplete request errors before send").

29. MISSING — priority arms `p2`, `p3`. workspace_priority.proto declares four; only `p05` (set) and `p1` (create) ride the suite. Table-drive all four through `agent-repl-verb-set-priority`.

30. MISSING — the interactive nuke confirm. fanout §9 "`agent-repl-nuke-workspace` (y/n confirm: data destruction)". `cl-letf` `yes-or-no-p`/`y-or-n-p` → nil; call the command; assert zero NukeWorkspace calls and the tab live; then → t and assert one call. The interactive layer (SPC j x close, prefix-arg force on restart) is otherwise entirely untested; `agent-repl-restart-workspace` with `current-prefix-arg` non-nil → `"force":true` on the raw wire is the second case worth pinning.

31. MISSING — DaemonHealth/SessionHealth `error` arm rendering. endpoint_session_health.proto "error = the question could not be ANSWERED (unknown workspace)". Script `SessionHealth` `{error:{unknownWorkspace:{}}}`; assert the health buffer states the question was not answered (not "HEALTHY") and a WARN record names the arm.

## test-integration-composer.el

32. MISSING — non-merging SubmitPrompt refusals keep the text. endpoint_submit_prompt.proto declares nine refusal arms; input.el's `_` branch logs `elisp.input.unknown-error-arm` at ERROR and messages "submission refused". Script `{error:{noSession:{}}}` and `{error:{turnAlreadyOpen:{}}}`; assert the buffer text is unchanged, attachments survive, no posthook ran, and a record names the arm. Also decide whether `transferring_away`/`not_yet_adopted` on SubmitPrompt should route to `agent-repl-host-handle-refusal` as verbs.el does (fanout §7 names "every per-workspace rpc"); today input.el treats them as unknown arms, which is a WRONG-class inconsistency to surface, and the test should pin the ruled behavior.

33. WEAK — the outage-drain tests never exercise the real link-up seam: they stub `agent-repl-link-up-p` → t and call `agent-repl--prompt-queue-on-link-up` by hand after a hand-rolled `--reattach`. fanout §10 "the outage queue (drained on `agent-repl-link-up-functions`)"; audit-1 #67 asked for the real path. Drive it with `agent-repl-itest-link--with-real-hooks`: connect, stop the daemon, send (queued), start the successor, and assert the drained SubmitPrompt lands with no manual drain call.

34. MISSING — `merge_parked` badge. fanout §7 "`:merge-parked` send, with the input mode-line badge 'merge parked — prompts go to the resolution agent'". Under the `mergeParked` gate assert the input buffer's mode-line construct (or the badge variable input.el maintains) carries that exact text, and that it is absent under `open`.

35. WEAK — the `merging` refusal test asserts the `message` but not the mode-line flash. fanout §10 "`message` + a mode-line flash 'refused: merge in flight'". Capture `agent-repl--input-flash` (or read the flash variable) and assert the exact text.

36. MISSING — sending on a workspace with no host ref is refused before send. fanout §10 `:workspace (agent-repl-host-ref WS)` is REQUIRED; §0 "an incomplete request errors before send". Call `agent-repl--send :user-sent "x" "never-registered"`; assert a signal and zero SubmitPrompt calls.

37. MISSING — UNSPECIFIED/unknown origin refused before send. fanout §5 "UNSPECIFIED is refused before send". `(agent-repl--send :unspecified ...)` and `(agent-repl--send :no-such-site ...)` → `agent-repl-wire-error`, zero calls.

38. MISSING — a finish-edge deferral mints a FRESH key. Ledger (R-COMPOSER): "the drain resends under [the failed key] (deferrals mint fresh)". In the roster drain test (or here), enqueue a deferred prompt, drain on the finish edge, assert the key is v4-shaped and differs from any prior submit's key. Only the outage-reuse half of the ruling is pinned.

## test-integration-daemon.el

39. MISSING — a DaemonHealth `error` arm still adopts. elisp.md "Emacs adopts any daemon that answers DaemonHealth"; daemon.el's `:error` branch warns `elisp.daemon.health-error` and adopts. Script `DaemonHealth` `{error:{}}` (DaemonHealthError has no arms, so `{}` is legal); assert no build/start, link up, WatchDaemon standing, and the WARN record.

40. WEAK — boot-timeout is asserted by operation name with no level and no `message`. fanout §11 names the surfaced timeout beside the build failure's "WARNING, `message`"; daemon.el logs it at ERROR. Pin the level and capture `message`; also assert the mode-line segment's state after a timeout (today unspecified in the test).

41. MISSING — `agent-repl-frontend-daemon-restart` = stop then ensure. fanout §11. With an answering fake: invoke it; assert `UpdateShutdownSchedule{now, operator "emacs"}` recorded, then (after the fake exits and daemon.addr is gone) the build and start stubs ran.

42. WEAK — no-daemon hook registration is never exercised end to end. Every cold-start test binds `agent-repl-link-no-daemon-functions nil` and calls `agent-repl-daemon-ensure` directly, so `(add-hook 'agent-repl-link-no-daemon-functions #'agent-repl-daemon-ensure)` (daemon.el:587) is unpinned. Delete daemon.addr, call `agent-repl-link-connect` with the hook variable UNBOUND-to-scratch, assert the build stub ran.

## test-integration-connect.el

43. MISSING — request headers. fanout §3 "headers `Content-Type: application/json`, `Connect-Protocol-Version: 1`" and stream `Content-Type: application/connect+json`. Nothing observes them; the fake's raw-body middleware would need to record headers too. Add `headers` to `/_fake/calls` (mux middleware, like `raw`) and assert both headers on one unary and one stream call.

44. MISSING — a stream on a webapp-only rpc is refused before acceptance. fakedaemon README "Every other agentrepl.v1 stream ... the fake answers those `unimplemented` so a wrong caller fails loudly"; §3 ON-OPEN "a non-200 ... never calls it". Open `WatchFeed` with `{}`; assert ON-OPEN never fires, ON-CLOSE is `(:error ...)` with code `unimplemented`, no subscriber listed.

45. WEAK — the unary timeout test leaves the gate's held call answering `canceled` after `connect-close`; it never asserts the failed call is NOT retried ("Never retried here", §3). Assert exactly one `RegisterWorkspace` recorded after release.

## Cross-suite

46. WEAK — the `+workspace-switch` seam. roster's `--with-real-select` registers `agent-repl-host--on-workspace-activated` on `persp-activated-functions` by hand because persp-mode is absent in batch; fanout §7 "called from workspace.el's perspective-activated hook". No test pins that production installs it (`agent-repl--ws-add-activated-hook`). Assert the `with-eval-after-load 'persp-mode` form registers it by `(provide 'persp-mode)` in a scratch feature and checking the hook afterward.

## Verdict

Forty-six new findings (about 34 MISSING, 12 WEAK, 0 WRONG as test assertions, though #1 and #32 expose production behavior the suite should have caught). The suite is markedly stronger than at audit 1: request shapes, the adopt-before-cancel order, blink cadence, finish-edge reactions, roster walk order, image blocks and the outage key reuse are all pinned against the real schema. It is still not adequate to gate the system, for two reasons. First, the whole refusal-driven half of the handover (`transferring_away` redial, `not_yet_adopted` retry, the webview redial URL, and roster following the promoted successor) is untested, and the last of these is a live defect: after any blue-green rollout Emacs loses its roster stream, so tabs stop reconciling until a link bounce. Second, several reactive registrations (finish-edge hooks, attention sync, the outage drain on link-up, the no-daemon hook) are asserted only after the test re-installs the one consumer it wants, so the suite cannot detect a dropped `add-hook`. Closing #1, #6-#8, #19, #24, #32-#33 would make it gate-worthy; the remainder are edge coverage on the proto's declared arms.

result: Elisp integration-suite audit 2 found 46 new spec holes (about 34 MISSING, 12 WEAK) beyond audit 1's 92, the gravest being an untested and currently broken roster re-subscribe after daemon promotion plus the wholly untested refusal-driven handover redial and retry; verdict: not yet adequate to gate the system.
