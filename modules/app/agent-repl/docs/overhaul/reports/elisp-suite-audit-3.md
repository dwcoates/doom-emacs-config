# Elisp integration-suite adversarial audit 3 (2026-09-01)

Fresh-context fable auditor at tip 44476838d (landing-6 adaptations in flight,
excluded). Teamlead triage: ALL 58 findings accepted for R-SUITE-3.
Production work → R-AUDIT3-PROD: #33 (WRONG: `agent-repl-roster--rename-tab`
must re-key `agent-repl-host--by-name` — and a rename colliding with a
tombstoned name is applied-or-refused-loudly, never half-applied), #27
(the input buffer created after a naming.title push carries the title),
#43-45 verified against production (hold + replay under the same key, in
order; dead-conn drain refusal). RULING #51: a command send with EXPLICIT
text (update-pr, explain, rebase, create-or-update-pr) must NOT erase the
composer's unrelated draft; only a send sourced from the composer buffer
clears it (listed as a user toss-up in the final report).

# Elisp integration-suite adversarial audit 3 (tip 4dc0515f5 / 44476838d)

Scope: all 138 findings of audits 1 and 2 are pinned and not repeated. Excluded: workflow, Q1-Q5, and the in-flight landing-6 adaptations plus the composer `--await-log` flake.

## test-integration-connect.el

1. MISSING — a call on a CLOSED connection fails loudly without touching the wire. elisp.md "unary calls fail loudly"; fanout §3 "`agent-repl-connect-close` marks the connection dead" (connect.el `--check-alive`). Open, close, then `unary-sync` RegisterWorkspace and `connect-stream` WatchDaemon on the dead conn; assert `agent-repl-connect-error` (record `:kind`), zero recorded calls, zero subscribers.

2. MISSING — ON-OPEN fires once, before any frame, on a real accepted stream. fanout §3 STANDING-STREAM ACCEPTANCE; R-ACCEPT pinned it in unit tests only. Stage a `drainScheduled` snapshot, open WatchDaemon with an ON-OPEN recording an ordinal; assert exactly one ON-OPEN before the snapshot's ON-PUSH, and that the two refusal tests (unset ref, WatchFeed) pass an ON-OPEN that is never called.

3. WEAK — "No compression negotiated" (fanout §3) is unobserved although the fake records headers. Assert one unary and one stream call carry no `Connect-Accept-Encoding` / `Connect-Content-Encoding` / `Accept-Encoding`.

## test-integration-link.el

4. MISSING — contained hook runs at the integration seam. R-STABILITY ruling / fanout §6 `elisp.link.hook-consumer-failed`; pinned only in test-daemon-link.el. With real hooks, `add-hook` a signalling consumer at depth −100 ahead of host.el/roster.el; connect. Assert the ERROR record names hook and consumer AND host.el still recorded RegisterWorkspace and a roster subscriber stands. Repeat for the down hook (abort; assert reconnect still stands).

5. MISSING — promote hook contract `(OLD NEW)` and timing. fanout §6 "the one hook that fires on promotion"; daemon-link runs it BEFORE closing OLD. Record `(old new (alive-p old))`; drive a promotion; assert OLD is the former primary, NEW the former successor, OLD alive at call time, exactly one call, and no call on a plain link-down.

6. WEAK — up/down hook arguments never asserted (every registration is `(lambda (&rest _))`). fanout §6 "run `agent-repl-link-up-functions` with CONN". Capture and assert the up arg is `eq` to `(agent-repl-link-primary)` and the down arg is the conn that died.

7. MISSING — promotion on a CLEAN end frame from the old daemon. fanout §6 + §3 "`(:ended)` … a failure"; both promotion tests use `abort`. Add `/_fake/end daemon` after successor acceptance; assert primary becomes the successor, no down/up hooks, `elisp.link.handover-complete`.

8. MISSING — re-announcement idempotence. daemon-link.el "an already attached successor at the same address is a no-op, never a second connection"; elisp.md dual attach opens "a second connection" (singular). Push `shutdownAnnounced{address}` twice; assert exactly one `daemon` subscriber on the successor, `elisp.link.successor-already-attached`, handover hook once. Sibling: a different address → `elisp.link.successor-address-changed` ERROR, successor unchanged.

9. MISSING — successor refused before acceptance. daemon-link.el pending-successor branch; audit-2 #4 covers death AFTER acceptance only. Announce `127.0.0.1:1`; assert `elisp.link.successor-open-refused` ERROR, successor nil, old link up, no down/handover hook.

10. WEAK — the bounce indicator is never asserted to CLEAR. fanout §6 "daemon restarting (<cause>)". In `bounce-reconnects-once-the-addr-returns`, after `up-p` assert `(null agent-repl-link-drain-segment)`.

11. MISSING — a blank operator note on the daemon stream is a breach. drain_reason.proto "REQUIRED non-blank"; production logs `elisp.link.drain-operator-note-blank` ERROR. Push `drainScheduled{reason:{operator:{note:""}}}` and `shutdownAnnounced{cause:{immediate:{reason:{operator:{}}}}}`; assert the ERROR and the stream standing. Companion: `drainScheduled` WITHOUT `reason` → `elisp.rpc.push-invalid`, `agent-repl-link-drain` unchanged.

## test-integration-daemon.el

12. WEAK — the restart's "await daemon.addr removal" step is asserted by a race, not a gate. R-RED-MISC ruling "stop ack → teardown → await daemon.addr removal → ensure". `restart-then-ensures-a-fresh-daemon` calls `/_fake/exit` by hand right after the ack. Keep the fake alive after the ack; assert for a bounded interval zero DaemonHealth, no build, `elisp.daemon.departure-waiting`; then exit and assert build+start. Add the timeout arm (fake never exits, small deadline): `elisp.daemon.departure-timeout` WARN, then ensure ADOPTS the still-live daemon.

13. MISSING — a refused or unreachable stop abandons the restart. daemon.el "the refusal stands and the link is left alone". Script `UpdateShutdownSchedule {error:{nothingScheduled:{}}}` (the only wire-legal arm); invoke `agent-repl-frontend-daemon-restart`; assert `elisp.daemon.stop-refused` + `restart-abandoned`, the `message`, link up, fake alive, zero build/start. Transport variant via a gate → `elisp.daemon.stop-failed`.

14. MISSING — restart with no link is just the ensure. fanout §11; daemon.el "nothing to stop". No daemon.addr, no link → `elisp.daemon.restart-nothing-to-stop`, build+start ran, no UpdateShutdownSchedule ever recorded. Also `-stop` with no link → `stop-skipped` WARN + message, no signal.

15. MISSING — concurrent ensure is single-flight. daemon.el `ensure-already-in-flight`. Two back-to-back `agent-repl-daemon-ensure` with a slow-publishing start stub; assert the stub ran once and one daemon subscriber.

16. WEAK — DaemonFault kinds beyond `wsmReadOnly` never ride the cold-start path. endpoint_daemon_health.proto six typed arms with payload; landing-4 relay. Table-drive all six through `unhealthy-faults-reach-the-health-buffer`; assert adoption still happens and each detail renders.

17. WEAK — own-provenance asserted only as the negative. Ledger "logs own-/foreign-adopted". In `links-up-once-the-addr-appears` also assert `elisp.daemon.own-adopted` INFO present.

## test-integration-host.el

18. WEAK — `not_yet_adopted` retry pacing. R-RED-HOST "paced by `agent-repl-host-handover-retry-delay`". The retried test asserts only `>= 2` adopts, so a synchronous tight loop passes. `cl-letf` `run-at-time` to record the delay; assert it equals the defcustom, no second adopt before it, and with success re-scripted after the first refusal EXACTLY two adopts.

19. MISSING — `transferring_away{address}` answered on `AdoptHostWorkspace` itself. endpoint_adopt_host_workspace.proto `AdoptHostWorkspaceTransferringAway{address}`. Only Select carries it in the suite. Script the successor's adopt as `transferringAway{address:<other>}`; assert `elisp.host.adopt-handover-refusal` INFO, `elisp.host.redial`, adopt landing at the named address, old stream standing meanwhile.

20. MISSING — the non-handover adopt refusal arms' treatment. Landing-4 relay; §0 "Dynamic values go in the context". `workspace_ref_mismatch{registry_dir}`, `no_transfer_announced`, `participant_not_expected` are decoder-pinned only. Table-drive to `elisp.host.adopt-refused`; assert the arm keyword and `/x` in context, old stream kept, successor lists no subscriber.

21. MISSING — adopt TRANSPORT failure keeps the old stream. host.el "kept standing on every failure path". Gate adopt on the successor, push `transferred`, await the call, exit the successor; assert `elisp.host.adopt-failed` ERROR, primary still lists the subscriber, `agent-repl-host-conn` unchanged, no webview reload.

22. MISSING — RegisterWorkspace TRANSPORT failure answers ON-DONE nil. fanout §7 + host.el `elisp.host.register-failed`; audit-1 #33 pinned the error ARM only. Gate Register, call `agent-repl-host-register`, close the conn; assert ON-DONE nil and the ERROR record.

23. MISSING — link-up register skips and refusals. fanout §7 "for every live workspace register its dir, then subscribe"; host.el `elisp.host.link-up-skipped` (no `:project-dir`) and `link-up-register-failed`. Two tests: a live ws without dir → WARN, zero Register for it, others still registered; Register scripted `{error:{notAWorktree:{}}}` → ERROR, zero WatchHostWorkspace subscribers.

24. MISSING — Select with no ref / no conn is skipped, not sent. host.el `select-skipped reason=no-ref|no-connection`. Assert zero SelectWorkspace and the WARN for no-connection; for the primary fallback, clear `:conn` via link down and assert the select lands on `(agent-repl-link-primary)`.

25. WEAK — unfocused precedence over "tab selected". Proto `notification` comment orders unfocused FIRST. Stub `agent-repl--ws-current-name` → the ws AND focused → nil; assert the banner reaches the backend and `elisp.host.notification-selected` is NOT logged.

26. WEAK — the renamed input buffer must still be an agent panel. host.el `--apply-naming` "keeps the name matching `agent-repl--input-buffer-re`". Assert the regexp match, that `agent-repl--input-buffer-name-for-id` still resolves it, a title containing `*` is stripped, a title equal to WS yields the canonical name, a second different title renames again, and `naming:{}` reverts (whole-replace).

27. MISSING (probable production gap) — title present BEFORE the composer exists. host.el docstring says the name is "built at creation from the title the daemon has by then", but panels.el:885 creates the input buffer with the bare canonical name and `apply-naming` runs only on a push. Push `naming.title`, then create the input panel through production; assert the buffer name carries the title. Expected red today.

28. MISSING — two input buffers handed the same title. host.el "UNIQUE-OK … a rename that ERRORED on the collision would strand the second composer". Two subscriptions, same title for both; assert both live, no ERROR record, both match the regexp and resolve by their own identity segment.

29. WEAK — HostFault KIND never reaches anything observable. endpoint_watch_host_workspace.proto "supplements the kind, never replaces it"; landing-4 relay "eight arms"; the only pushes carry `linkSevered` and verbs.el `--fault-lines` prints `detail` alone. Table-drive the eight kinds; assert `(plist-get fault :kind)` per arm and the kind name in `*agent-repl-health*`. Also pin the breach: HostFault with `detail`+`openedAtMs` and no kind → `push-invalid` (R-POLISH found this refused; nothing pins it).

30. MISSING — `transferring_away` naming an address DIFFERENT from the standing successor. host.el `--redial-successor` "a successor already standing at a DIFFERENT address is a stale handover". Announce A via the real link, script Select `transferringAway{address:B}` (third instance); assert WatchDaemon and adopt land on B, not A. Companion: the `awaiting-successor` branch is otherwise reached only via the hand-called `handle-refusal` test.

31. MISSING — `not_yet_adopted` on SelectWorkspace end to end. endpoint_select_workspace.proto `SelectWorkspaceNotYetAdopted`. Script Select `{error:{notYetAdopted:{}}}` with a successor standing; assert `elisp.host.select-handover-refusal` INFO, NO `select-refused` ERROR, one adopt after the retry delay.

32. WEAK — link-down leaves successor-owned workspaces untouched. fanout §7 "On link down: mark streams gone"; host.el logs `elisp.host.link-down workspaces=N`. With A transferred to the successor and B on the primary, abort the primary; assert A's `:conn`/stream survive and the WARN counts exactly 1.

## test-integration-roster.el

33. WRONG (production; the suite's rename test cannot see it) — fanout §8 "a rename of the row renames the tab" + §7 host state keyed WS-NAME + §9 "REF via `agent-repl-host-ref` (nil → `user-error`)". `agent-repl-roster--rename-tab` (roster.el:312) calls `agent-repl--ws-rename-state` and `--ws-rename-persp` but nothing re-keys `agent-repl-host--by-name` (verified: workspace.el:240-252 mutates `agent-repl--workspaces` only). After a rename `agent-repl-host-ref NEW` is nil (composer and every verb refuse), host pushes update a dead name's gate, and teardown by NEW unsubscribes nothing (stream leak). The existing rename test runs with no primary link. Script: real-select fixture with a primary conn, push id X as "old", await `agent-repl-host-ref "old"`, push X as "new"; assert `(agent-repl-host-ref "new")` equals the ref, `"old"` is nil, a following host push for X updates `(agent-repl-host-composer-gate "new")`, `agent-repl--live-ws-names` lacks "old", tab order reads `("new")`. Also: rename target colliding with a TOMBSTONED name makes `--ws-rename-state` signal `user-error` out of the push handler (workspace.el:225) — assert applied or refused loudly, not half-applied.

34. MISSING — `when` and `detail` never ride a roster push. fanout §2 int64 "integer OR a decimal string"; §5 "when with 2 arms or unset, detail with presence-optional lines". Every fixture row has unset `when` and empty `detail`, so a decoder mis-typing `lastSelected.atMs` (which Go emits as a string) would refuse every real push while the suite stays green. Push rows with `when.lastSelected.atMs`, `when.merged.atMs`, and all three detail lines; assert applied, no `push-invalid`.

35. MISSING — roster stream ended by the producer. fanout §3; roster.el `elisp.roster.stream-close` ERROR. Audit 2 pinned daemon (#2) and host (#12) only. `/_fake/end roster` clean and `abort`; assert the ERROR per reason, `agent-repl-roster-view` KEPT, and a following subscribe stands a fresh subscriber.

36. MISSING — closed true→false for the SAME id (reopen). fanout §8 "closed false → ensure a tab exists"; `--ws-del` tombstones (workspace.el:49) and `--open-tab` writes through `--ws-put`. Push open, closed, open; assert `agent-repl--ws-by-ref-id` returns a LIVE name, exactly two `tab-open` records, a third identical push adds none.

37. MISSING — the successor's snapshot after restart leaves tabs unchanged. elisp.md "a reconnect re-opens and re-pulls"; `elisp.roster.tab-kept`. Real hooks, snapshot rows A,B, restart with the same snapshot; assert zero `tab-teardown`, no second `tab-open`, order unchanged.

38. MISSING — duplicate ref id in one push is dropped whole. sidebar.proto RosterRow.workspace "its identity"; roster.el `reason=duplicate-ref-id` ERROR. Push two rows sharing an id; assert the ERROR, no tab for either, `agent-repl-roster-view` unchanged.

39. MISSING — `WorkspaceRoster.task` absent is a breach (sidebar.proto:60 non-optional); audit-1 #49 pinned `repository`/`recentlyMerged` only. Assert `push-invalid` with the raw payload.

40. WEAK — deferred drain asserts `origin` only; the gated case is untested anywhere. fanout §8 reaction (4), §10 liveness gate. Assert the body's text block and `workspace.id`, the `:deferred` queue emptied; add gate `merging` at the finish edge → NO SubmitPrompt and `elisp.prompt-queue.finish-edge-deferred` INFO (no suite references it).

41. WEAK — banner reaction pins the message text only. R-CLICK backend `(WS TITLE MESSAGE ACTIVATE)`. Assert `(nth 0 …)` is the workspace and ACTIVATE non-nil.

42. MISSING — priority badge removal. sidebar.proto RosterRowPriorityBadge "UNSET = unprioritized (no badge)". Push with then without `priority`; assert `agent-repl--tab-badge-str` no longer starts with "P1".

## test-integration-composer.el

43. MISSING — a handover refusal on SubmitPrompt HOLDS the prompt and re-drives it on promotion under the same key. elisp.md "Prompts arriving during the window are HELD (never errored)"; input.el `--input-on-handover-refusal` ("outage queue under THIS attempt's KEY … released on the promotion"); prompt-queue.el `--prompt-queue-on-link-promote`. The audit-2 #32 tests stub `agent-repl-host-handle-refusal`, so a production that dropped the prompt or minted a new key passes. Real link, announce successor, script the primary's SubmitPrompt `transferringAway{address}`; send. Assert one `:outage` entry, text still in the buffer, adopt on the successor; end the primary's daemon stream; assert exactly one SubmitPrompt on the successor with the SAME `idempotencyKey` and an empty queue. Also pin that the default `agent-repl-link-promote-functions` contains `agent-repl--prompt-queue-on-link-promote`.

44. MISSING — dead-conn drain refusal. R-STABILITY ruling "the outage drain skips a dead conn and re-queues"; `elisp.prompt-queue.dead-conn` / `no-conn` WARN (no suite greps for either). Hold an outage prompt, `agent-repl-connect-close` the host conn with `link-up-p` still t, drain; assert zero SubmitPrompt, entry pending, the WARN. Second case with host conn and primary both nil → `no-conn`.

45. MISSING — held prompts replay IN ORDER. elisp.md "replay in order"; prompt-queue.el "oldest first". Hold two; assert the two SubmitPrompt bodies carry the texts in order with their original keys.

46. MISSING — image-only submission. user.proto UserSaid "NOT a bare TextBlock"; input.el "Empty TEXT contributes no block". Attach one image to an EMPTY buffer and send; assert exactly one `image` block, no `text` block, not treated as send-empty.

47. MISSING — `agent-repl-queue-deferred-prompt` is never invoked (both suites use `--prompt-queue-enqueue`). fanout §10. Call it with text + attachment; assert composer emptied, history pushed, attachments cleared, one `:deferred` pending; on the finish edge the SubmitPrompt carries text AND image blocks. Also "one deferred prompt per finish edge": queue two, one edge → one SubmitPrompt, one pending.

48. WEAK — the twelve origin tests call `agent-repl--send` with the keyword directly; the real command sites (`agent-repl-update-pr`, `-explain`, `-rebase-onto-origin-master`, `-create-or-update-pr`, `-send-and-hide`, `-send-with-metaprompt`, `-metaprompt-read`) are never shown to pass THEIR value. prompt_origin.proto "exactly one production send site". Invoke each command (stub git/gh boundaries) and assert the recorded origin and text.

49. MISSING — a refusal must NOT push history. fanout §10 "success `:turn` → clear the input, push history". After the `merging` refusal assert the history head is unchanged.

50. WEAK — the generic-refusal test asserts the ERROR record but not the `message "agent-repl: submission refused (%S)"`. Capture and assert it names the arm.

51. WEAK (needs a ruling) — `agent-repl--send-to-agent` with explicit TEXT still erases the composer's unrelated draft on `turn` (`--input-accepted` erases unconditionally). fanout §10 says only "success → clear the input". Teamlead to rule; pin: buffer holds "my draft", send `:command-update-pr` with explicit text, assert the buffer state after the ack.

## test-integration-verbs.el

52. MISSING — scoped merge-queue commands resolve the repository from the ROSTER and refuse without one. fanout §9; endpoint_update_merge_queue.proto "a caller that means one names it"; verbs.el `--merge-queue-repository` → `user-error`, `elisp.verbs.no-repository`. Push a roster holding the ws; call `(agent-repl-merge-queue-pause)`; assert `pause.repository.id` equals the section key; without the ws → `user-error`, WARN, zero calls; prefix arg → `repository` absent on the raw wire. Same for resume.

53. MISSING — the interactive create family. fanout §9 create-workspace (prefix arg = child), `agent-repl-fork-workspace`, one-shot variants with `agent-repl-oneshot-model-candidates`. Stub the readers; assert (a) no prefix → no `parent`; (b) prefix → `parent.workspace.id` = current ref, no `fork`; (c) fork → `parent`+`fork`; (d) reviewed open-pr → raw `"selfCertified":false`, plain → `true`; (e) model prefix → chosen candidate in `model`.

54. MISSING — `agent-repl-open-workspace` picks a CLOSED roster row. fanout §9 "(completing-read over closed rows)". Roster with one closed and one open row; stub `completing-read`; assert OpenWorkspace carries the closed row's ref (id AND dir); no closed rows → `user-error`, zero calls. Also `agent-repl-daemon-shutdown-schedule` (minutes → `atMs`) and `-now` with a blank operator note → `user-error`, zero calls.

55. MISSING — typed fault KINDS never ride the health path. DaemonFault.kind (6 arms, payload-bearing) and SessionFault.kind (8 arms: `shim_start_failed{exit_code, stderr_tail}`, `shim_reported{component,kind}`, …); the suite scripts only `logSinkPoisoned{}` and `linkSevered{}`. Table-drive every kind with payload through DaemonHealth/SessionHealth; assert the detail renders and no `unknown-response-arm`/decode error.

56. WEAK — transport failure asserts the ERROR record only. fanout §9 "`agent-repl--error` + `message`". Capture `message` and assert "merge failed -- the daemon did not answer".

57. WEAK — restart success asserts `string-match-p "restart"`, which the INFO `elisp.verbs.send op=restart` echo also satisfies (the close-blocked test documents this race). Assert the exact per-force text.

58. MISSING — admin-verb refusal arms and op-named slugs. `UpdateShutdownScheduleError.nothing_scheduled` on cancel; `UpdateMergeQueueError.already_paused` / `not_paused` / `no_such_queued_merge`; only merge and close refusals are pinned. Script them; assert `elisp.verbs.shutdown-refused` / `merge-queue-refused` WARN and the message. (`unknown_repository` is in flight and excluded.)

## Cross-suite

- No finding on test-integration-helpers.el. One note: `agent-repl-daemon-boot-timeout-seconds` doubles as the departure-wait deadline (daemon.el:420) though the spec names it only for boot; #12's timeout arm relies on that.

## Verdict

Fifty-eight findings (about 38 MISSING, 19 WEAK, 1 WRONG). The suite is markedly stronger than at audit 2: the announced handover, adopt-before-cancel order, blink cadence, roster walk, request shapes, raw-wire explicit false, request headers, and the promote re-subscribe are all pinned against the real schema. It is still not adequate to gate the system, for three reasons. First, one live production defect (#33): a roster rename never re-keys host.el, so after any daemon-side rename the composer and every verb refuse the workspace and its host stream leaks — the rename test runs without a link and cannot see it. Second, the held-prompt half of the SubmitPrompt handover (#43-45) — the one path where a wrong implementation silently loses user intent — is unpinned end to end, as is the prompt-queue dead-conn ruling it depends on. Third, the reactive contracts landed since audit 2 are asserted by consequence rather than by contract: hook containment and the promote hook's arguments (#4-5), the `not_yet_adopted` pacing (#18), and the restart's addr-removal gate (#12) would all pass against implementations that violate the rulings that motivated them. Closing #33, #43-45, #4-5, #12, #18-19, #27, and #52 would make the suite gate-worthy; the remainder are edge coverage on declared arms and interactive-layer seams.

result: Elisp integration-suite audit 3 found 58 new spec holes (about 38 MISSING, 19 WEAK, 1 WRONG) beyond the 138 pinned by audits 1 and 2; the WRONG is a live defect where a roster rename never re-keys host.el (composer and verbs refuse the renamed workspace, host stream leaks), and the gravest gaps are the unpinned hold-and-replay of prompts refused during a handover and consequence-only assertions on hook containment, retry pacing, and restart sequencing; verdict: not yet adequate to gate the system.
