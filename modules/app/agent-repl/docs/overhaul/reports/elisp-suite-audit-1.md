# Elisp integration-suite adversarial audit 1 (2026-08-29)

Fresh-context fable auditor, pointed at docs/overhaul/elisp.md, elisp-fanout.md,
the Emacs-facing passages of daemon.md/webapp.md, the consumed protos and the
merged suite at 942659cd6. Teamlead triage: ALL findings accepted for the
suite-extension pass except #93 (no finding). Ruling on #66: a prompt re-sent
from the outage queue carries the SAME idempotency key as the failed attempt
(a re-drive is a retry; the proto's duplicate refusal makes it not a second
turn) — pin that.

Audit complete. Sanity check: I re-read the ask (holes relative to specs, tagged, grouped, §15b items excluded), read every spec passage named, all 21 protos, all eight suite files in full, and spot-checked host.el/status.el to grade weakness claims; R-ACCEPT, R-QUESTION, and R-CLICK are excluded below. Note verbs.el is absent from this checkout (mid-merge), so verbs findings grade the suite against the spec only.

**test-integration-connect.el**

1. MISSING — fanout §3 "malformed content signals `agent-repl-connect-error`" — write `garbage` into daemon.addr; assert the signal with `:kind :malformed-addr`.

2. MISSING — fanout §3 "ON-PUSH exceptions are caught at the filter boundary: log ERROR with the payload in context; the stream stays open" — ON-PUSH that signals on the first push; assert ERROR log and that a second push still arrives.

3. MISSING — fanout §3 "`(agent-repl-connect-close CONN)` ... cancels every standing stream as `(:cancelled)`" — open two streams, close the conn, assert both ON-CLOSE outcomes are `(:cancelled)` and the fake's subscriber list empties.

4. MISSING — fanout §3 "`(:error DETAIL)` ... process death without an end frame — logged ERROR here" — the abort and error-end-frame tests assert only the callback outcome; add an ERROR log assertion (`elisp.connect.*`) for each.

5. MISSING — fanout §3 "Default timeout ... (10)" / `:timeout` kind — script nothing, stop the fake's answer path (or use a non-listening port with an accepted TCP connect), call unary-sync with TIMEOUT 0.2, assert `:kind :timeout`.

6. WEAK — fanout §3 "Any non-200 → parse the Connect error body `{code,message}` → failure" — unknown-field test asserts only `:kind :http` and status 400; would pass with an unparsed body; assert `:code "invalid_argument"` and a non-empty `:message`. Same for the missing-required-field test (asserts neither status nor code).

7. MISSING — fanout §3 STANDING-STREAM ACCEPTANCE "a non-200 ... never calls [ON-OPEN]" at the integration level — open WatchHostWorkspace with an unset workspace ref; assert the fake never lists a subscriber and ON-CLOSE is `(:error ...)` with status 400.

**test-integration-link.el**

8. MISSING — endpoint_watch_daemon.proto DaemonShutdownAnnounced "A late receiver SHORTENS its quiet window by the time already elapsed rather than restarting it" — announce with `minted_at_ms` = now − 1400 and `expected_outage_ms` = 1500; assert the reconnect's first WatchDaemon on the successor lands within ~100 ms, not 1500.

9. MISSING — fanout §6 "the reconnect loop waits until then before polling" — announce a bounce with a 1.5 s outage, start the successor immediately; assert no WatchDaemon call reaches the successor before `minted_at_ms + expected_outage_ms` (compare the fake's recorded call time or a `float-time` stamp at first subscriber).

10. MISSING — fanout §6 "the indicator reads 'daemon restarting (<cause>)'" — after a bounce announcement assert the mode-line segment text names the cause arm.

11. MISSING — endpoint_watch_daemon.proto DaemonShutdownCause arms `scheduled_drain{reason}` / `immediate{reason}` — every announce in the suite uses `selfMergeRollout`; table-drive the three cause arms and assert each decodes (indicator names deploy/maintenance/note for the reason-bearing arms).

12. MISSING — fanout §0 validation invariant on WatchDaemonResponse — push `shutdownAnnounced` without `cause` (non-optional message); assert `elisp.rpc.push-invalid` ERROR and that the link stays up and a following `drainScheduled` still arrives.

13. WEAK — fanout §6 "'drain HH:MM · deploy' / '· maintenance' / '· <operator note>'" — the segment test matches only the reason substring; assert the full format including the `drain HH:MM` prefix derived from `at_ms` (fix the clock via `cl-letf` on `format-time-string` or compare against `(format-time-string "%H:%M" (/ at-ms 1000))`).

14. WEAK — endpoint_watch_daemon.proto "re-pushed to late subscribers per the subscription invariant" — the late-subscriber test asserts only `:at-ms`; also assert `(plist-get agent-repl-link-drain :reason)` is the `:maintenance` arm and the segment is drawn.

15. WEAK — fanout §6 "PROMOTE the successor to primary silently" — the promotion test asserts only `(agent-repl-link-successor)` is nil, which is also true if both links died; assert `(agent-repl-link-primary)` is the successor conn and `(agent-repl-link-up-p)`.

16. WEAK — fanout §14 scenario 2 "Re-register + re-subscribe after a daemon restart" — the idempotence test registers twice by hand on one live daemon; drive it through the restart: register on the primary, stop it, start the successor, assert the successor records RegisterWorkspace with the same dir and a WatchHostWorkspace for the re-minted ref (host.el's link-up hook doing the work, not the test).

17. MISSING — fanout §7 "On link down: mark streams gone; keep the last host state" — after a host push, abort the daemon; assert `agent-repl-host-state` still returns the last plist and `agent-repl-host-conn` is nil.

18. MISSING — fanout §8/§6 "roster.el re-subscribes" on link-up — after restart assert the successor lists one roster subscriber without the test calling `agent-repl-roster-subscribe`.

**test-integration-host.el**

19. MISSING — elisp.md HOST section / proto "Emacs's obligation, in order: call AdoptHostWorkspace ... then cancel the old stream and re-subscribe" — no test pins ORDER; script the successor's AdoptHostWorkspace to answer only after a control-plane gate, then assert the primary still lists the host subscriber while the adopt is pending, and the successor lists none until the adopt answers.

20. MISSING — fanout §7 "error arm → ERROR log, keep the old stream" — script `AdoptHostWorkspace` `{error:{}}` on the successor; assert `elisp.host.adopt-refused` ERROR, the primary still lists the subscriber, and the successor lists none.

21. MISSING — fanout §7 "success → ... update `:conn`" — after a transfer assert `(agent-repl-host-conn WS)` is the successor conn (and a subsequent `agent-repl-host-select` lands on the successor's `/_fake/calls`).

22. MISSING — elisp.md handover "Emacs is the relay" end to end — the three transfer tests stub `agent-repl-link-successor`; add one scenario driving `shutdown_announced{address}` through the real link and then `transferred`, asserting the adopt lands on the successor with no stubs.

23. MISSING — fanout §0 validation: unset `session` oneof — push `{host:{naming:{}}}`; assert `elisp.rpc.push-invalid` ERROR and stream standing.

24. MISSING — fanout §0 validation: `existing` without `id` (non-optional) and `existing` with unset `standing` — one test each; assert ERROR + dropped.

25. MISSING — fanout §0 validation: `notification` without `kind` (non-optional message) — assert ERROR, no banner/blink, stream standing.

26. WEAK — the two "unknown arm is refused" tests (`hibernated`) assert a 400 from the fake's control plane and would pass against any elisp; either drop them from the elisp suite or turn them into a unit-level decoder pin (`agent-repl-wire-decode-watch-host-workspace-response` on a hand-built alist with an unknown key → `agent-repl-wire-error`).

27. MISSING — endpoint_watch_host_workspace.proto HostBackfill "none | pending | done | failed" — only `failed` is asserted; table-drive the four arms through `agent-repl-host-backfill`.

28. MISSING — proto "vendor_info ... Unset while no vendor conversation exists yet" — push a live arm with no `claude`; assert the push is accepted (gate `:open`), not treated as a breach.

29. MISSING — fanout §7 "buffer titles use `naming.title`, else `naming.slug`, else the row name" — push `naming:{}`; assert `agent-repl-host-display-title` equals the roster row name.

30. WEAK — fanout §14 scenario 3 "faults → health buffer" — the faults test checks only the accessor; assert the fault detail appears in `*agent-repl-health*` after `agent-repl-session-health` (or wherever the standing faults are rendered).

31. MISSING — fanout §7 "`agent-repl-host-update-functions` (WS HOST-PLIST) runs after every host push" — register a hook, push twice, assert two invocations with the decoded plists.

32. MISSING — fanout §7 "`(agent-repl-host-unsubscribe WS)` ... unsubscribe cancels" — call unsubscribe explicitly and assert the fake's subscriber list for that id empties (today it only happens in teardown, unasserted).

33. MISSING — fanout §7 "error arm → `agent-repl--error` and ON-DONE nil" — script RegisterWorkspace `{error:{}}`; assert ON-DONE receives nil and an ERROR log is written.

34. MISSING — elisp.md notifications "blink that tab-bar entry per the canonical cadence" / fanout §8 exact instants — every host and roster test stubs `agent-repl-status-blink-tab`; add one un-stubbed case with `run-with-timer` captured via `cl-letf`, asserting delays exactly (0 on, 0.5 off, 1.0 on, 1.5 off, 2.0 steady-on) after a real `notification` push.

35. MISSING — status.el "Re-entrant: a second call while a blink is in flight RESTARTS the cadence" — push two notifications back to back; assert the first schedule's timers are cancelled and exactly one five-step schedule stands.

36. WEAK — fanout §0 "Dynamic values go in the context" for `question_asked{header}` — the header test matches only `question-asked` in the message and passes today although the header is not logged at all (host.el's `_` branch drops it); assert `"Which approach?"` appears in the record.

37. MISSING — fanout §7 "`open_in_editor` ... a directory opens in dired. Log INFO with the path" and proto "UNSET = the file's top" — push without `line` and assert `(path nil)`; push a directory path with `agent-repl-popup-open` un-stubbed and assert a dired buffer; assert the INFO log carries the path.

38. MISSING — kickoff ruling "The webview URL is `http://<daemon.addr>/?workspace=<id>&dir=<dir>`" / fanout §12 "Nothing else rides the URL" — the harness records `agent-repl-itest-webview-urls` but no test reads it; mount the workspace and assert the exact URL with hexified id/dir and no `composer` parameter.

**test-integration-roster.el**

39. MISSING — fanout §8 "rows depth-first (row, then its children)" — push a row with `children`; assert tab order parent, child, next sibling.

40. MISSING — fanout §8 "then `recently_merged.rows`" — a recently-merged row with `closed=false` must be walked after every repo section's rows; assert order.

41. MISSING — fanout §8 "walk `repository.sections` in order" — two repo sections; assert tabs follow section order.

42. MISSING — fanout §8 "The task view is ignored (the same rows regrouped)" — put the rows in `task.sections` in a different order; assert tab order follows the repository view only and no duplicate tabs appear.

43. MISSING — fanout §8 "on collision within the roster, append '·<repo label>'" — two rows named `fix` in two repos; assert the tab labels.

44. MISSING — fanout §14 scenario 10 "Emacs's own switch → exactly one SelectWorkspace" — perform a real `agent-repl--ws-switch` (host.el's activated hook), let the fake echo `current`, and assert exactly one SelectWorkspace in `/_fake/calls` after the echo.

45. WEAK — R8 "re-selection is idempotent, no loop" — the daemon-originated test stubs `agent-repl--ws-switch`, so the SelectWorkspace the switch would send (and any loop) is never exercised; un-stub, let the echo come back, assert exactly one SelectWorkspace.

46. MISSING — fanout §8 "`current.workspace.id` differs from the selected tab's ref id AND from last-selected" — case where `current` equals the selected tab but not last-selected; assert no switch.

47. MISSING — sidebar.proto "`current` ... UNSET when there is none" — roster without `current`; assert no switch and no error.

48. MISSING — fanout §0 validation on RosterRow non-optional fields — one test each for a row missing `workspace`, `name`, `current`, `closed`; assert `elisp.rpc.push-invalid` ERROR (only unset `status` is pinned).

49. MISSING — fanout §0 validation on WorkspaceRoster — roster missing `repository` (or `recentlyMerged`); assert ERROR and dropped.

50. MISSING — fanout §8 finish edge "RUNNING = {submitting thinking clearing compacting permission}; SETTLED = {ready done interrupted idle-async}" — only thinking→done/ready/idle_async pinned; table-drive the remaining running sources (submitting, clearing, compacting, permission) and the `interrupted` target.

51. MISSING — "once per edge" — settled→settled (`ready`→`done`) must not fire; and a row first appearing already settled must not fire.

52. MISSING — fanout §8 "Registered reactions: (1) unfocused desktop banner 'Agent ready: <name>'; (2) cross-workspace echo; (3) magit-status refresh; (4) deferred-prompt drain" — the suite asserts only the hook variable; assert each registered reaction observably (notifier call with "Agent ready: itest-fin", `message` captured when not selected, the magit refresh function called with the dir, a queued deferred prompt producing a SubmitPrompt with `PROMPT_ORIGIN_DEFERRED_PROMPT`).

53. MISSING — fanout §8 "Attention present → blink once (below) then a steady marker until the marker leaves the row" — re-push the same row with `attention` still present and assert no second blink; push without `attention` and assert the marker clears.

54. MISSING — fanout §8 "Priority badge label draws before the name" — row with `priority.label "P1"`; assert the tab label starts with the badge.

55. MISSING — fanout §8 "inactive → none with a '?' glyph" and the merge glyphs — assert the drawn tab glyph for `inactive` and one merge arm through `agent-repl-status-tab-state`'s render path.

**test-integration-composer.el**

56. MISSING — fanout §10 origins "Sites: user-sent-and-hide, ... user-sent-with-postfix, user-sent-with-prefix, metaprompt-read, command-*" — only three origins pinned; table-drive every send site to its `PROMPT_ORIGIN_*` string.

57. MISSING — fanout §14 scenario 12 "prefix/postfix" — send through the prefix and postfix variants; assert the submitted text is the composed text and the origin is the variant's.

58. MISSING — fanout §7 "`:terminal` SEND" and "`:unknown` ... SEND as well, logging INFO" — push a terminal standing and send (assert SubmitPrompt recorded); send before any host push (assert SubmitPrompt recorded and an INFO log).

59. WEAK — fanout §7 fixed refusal texts ("composer closed: a merge owns this session" / "daemon draining" / "restarting") — the three gate tests wrap `agent-repl--send` in `ignore-errors` and assert only "no rpc"; capture the `user-error` and assert the exact text, and assert the input buffer still holds the text.

60. WEAK — fanout §10 "error `:merging` → keep the text, `message` + a mode-line flash 'refused: merge in flight'" — the merging-refusal test asserts only that posthooks did not run; assert the input buffer's content is unchanged and the message text.

61. WEAK — fanout §10 "success `:turn` → clear the input, push history" — asserts only posthooks; assert the input buffer is empty and the history's newest entry is the text.

62. WEAK — fanout §10 "`:command-panel` or `:command-refused` → ... clear the input" — both tests assert only the INFO log; assert the input buffer is emptied and the log context names the arm.

63. MISSING — fanout §10 "cleared on a successful send" (attachments) — after a `turn` success assert a second send carries no image blocks; after a `merging` refusal assert the attachment survives.

64. MISSING — fanout §10 "the text block(s) plus one ... image block per image" — attach two images; assert block order (text first, then both images in attach order) and both `media_type`s.

65. WEAK — fanout §10 "`(agent-repl--uuid)` (RFC 4122 v4 from `random`)" — only inequality of two keys is asserted; assert the v4 shape (`[0-9a-f]{8}-…-4[0-9a-f]{3}-[89ab]…`).

66. MISSING — endpoint_submit_prompt.proto "a retried request is not a second turn" — no test pins whether a prompt re-sent from the outage queue reuses its original idempotency key; the spec is silent, so pin whichever the teamlead rules (proposed: the drained resend carries the SAME key as the failed attempt) and surface the gap.

67. MISSING — fanout §10 "transport failure → keep the text and offer it to prompt-queue.el (drained on link-up)" — the test stubs the queue; add the end-to-end: fail the send, restart the fake, and assert the drained SubmitPrompt arrives on the successor with `PROMPT_ORIGIN_DEFERRED_PROMPT` and the same text.

68. MISSING — fanout §10 "Its liveness gate is `agent-repl-link-up-p` and the composer gate" — queue a prompt while the link is down and the gate is `merging`; assert the drain on link-up does not send until the gate opens.

**test-integration-verbs.el**

69. MISSING — fanout §9 "Kill/Nuke success → tear the tab down" — only Close success is asserted; add one test each asserting `agent-repl--ws-known-p` goes nil after Kill and after Nuke.

70. MISSING — fanout §9 "Restart success → `message`" and "Merge success → `message "merge enqueued"`" — capture `message` and assert the texts.

71. WEAK — fanout §9 "Close `blocked` → log INFO + `message "close blocked — see the workspace footer"`, no dialog" — asserts the log and tab only; capture `message` and assert the text, and `cl-letf` `y-or-n-p`/`yes-or-no-p` to error so a dialog fails the test.

72. WEAK — fanout §5 "`force` ... always encoded explicitly, false included" — the graceful-restart test's `should-not (eq … t)` passes whether `force` was omitted or sent false, because protojson drops false on the fake's side; assert via the request encoder's JSON (`agent-repl-wire-encode-restart-workspace-request` output contains `"force":false`) or have the fake echo raw bodies.

73. WEAK — endpoint_create_workspace.proto "Default false" for `self_certified`/`add_to_merge_queue` — the open_pr test sends both true; add the both-false case and assert explicit encoding as in 72.

74. MISSING — fanout §5 "an unset one-shot `finish` is refused before send" — call `agent-repl-verb-create … :one-shot :prompt "x"` with no `:finish`; assert a signal and zero CreateWorkspace calls.

75. MISSING — CreateWorkspaceRequest `model`, `allow_ungated`, `standard.base_ref`, `standard.name`, `standard.merge_actions` — one test each asserting the wire field (presence-only `allowUngated` as `{}`; `mergeActions.beforeWsMerge` as UserSaid).

76. MISSING — CreateWorkspaceStandard "UNSET = an empty workspace; presence, never an empty UserSaid" — create with no prompt; assert `initialPrompt` is absent from the body.

77. MISSING — drain_reason.proto "The note is REQUIRED non-blank — a blank note is refused at the request" — schedule with `(:arm :operator :note "")`; assert refused before send and zero UpdateShutdownSchedule calls.

78. WEAK — endpoint_update_shutdown_schedule.proto `schedule.at_ms` — the schedule test asserts only the reason arm; also assert `schedule.atMs` equals `"1735689600000"`.

79. MISSING — UpdateMergeQueue `resume` arm — assert the `resume` key on the wire (pause and evict are pinned).

80. MISSING — fanout §9 "render into `*agent-repl-health*`: verdict, each fault's detail, plus the host stream's standing faults" — DaemonHealth `healthy` renders a healthy verdict; SessionHealth `unhealthy` prints its faults AND the host push's standing `faults` for that workspace (only DaemonHealth-unhealthy is pinned).

81. MISSING — fanout §9 "Each resolves REF via `agent-repl-host-ref` (nil → `user-error`)" — call a verb on an unregistered workspace name; assert `user-error` and zero calls.

82. MISSING — fanout §9 "CONN via `agent-repl-host-conn` (falls back to `agent-repl-link-primary`)" — after a transfer, assert a verb lands on the successor's `/_fake/calls`, not the primary's.

83. MISSING — fanout §11 "`agent-repl-frontend-daemon-stop` = UpdateShutdownSchedule{now, operator "emacs"}" — invoke the command itself (not the verb) and assert the request shape.

**test-integration-daemon.el**

84. WEAK — fanout §11 "any ANSWER ... = a daemon is there → adopt it" then "`agent-repl-link-connect`" — the adoption tests await only the DaemonHealth call; assert `(agent-repl-link-up-p)` and a WatchDaemon subscriber on the fake, otherwise "adopted" is never shown to mean connected.

85. MISSING — fanout §11 "unhealthy faults go to `*agent-repl-health*` as a WARNING" — the buffer is asserted, the WARNING level is not; assert a `warn`-level log.

86. MISSING — fanout §11 "INFO `elisp.daemon.foreign-adopted` when this Emacs did not spawn it" — the negative: after cold start spawns the fake itself (the links-up test), assert `foreign-adopted` is NOT logged.

87. MISSING — fanout §11 "wait for daemon.addr up to `agent-repl-daemon-boot-timeout-seconds` (30)" — a start stub that never publishes; assert the timeout is surfaced (WARNING/`message`) with no link-up and no hang beyond the deadline.

88. MISSING — fanout §11 "the mode-line segment 'daemon: build failed'; no automatic retry; `agent-repl-frontend-daemon-ensure` (interactive) retries" — assert the segment text after a failed build, that the build stub ran exactly once, and that calling the interactive retry runs it a second time.

89. MISSING — fanout §11 stale addr "treated as absent" — the stale test asserts only the WARNING and the build; also assert the start stub ran (absent means build AND start).

90. MISSING — fanout §11 "`agent-repl-daemon-command` (default the module's `daemon/bin/claude-repld`, no argv)" — assert the default value's path and that the spawn passes no arguments (record `$#` in the stub).

**Cross-suite**

91. MISSING — fanout §0 "Operation names: `elisp.<module>.<operation>`" per-module logging — the suite pins exactly eight operations; no test covers `elisp.host.adopted`, `elisp.host.transferred`, `elisp.verbs.*` success acks, `elisp.roster.*` reconcile/finish, `elisp.input.submit` (with origin in context), or `elisp.link.*` down/up/handover; add one log assertion per branch the spec names.

92. WEAK — fanout §4 "A push that fails decoding is logged ERROR (`elisp.rpc.push-invalid`) with the raw JSON in context" — every push-invalid assertion checks operation+level only; assert the record carries the raw payload.

93. No finding — no `sleep-for`/`sit-for` anywhere in the suite; all waits go through `accept-process-output` under a deadline.

**Verdict:** 92 findings (roughly 60 MISSING, 30 WEAK, 0 WRONG). The suite is not adequate to gate the system: the handover ordering, the blink cadence, the finish-edge reactions, the plain-bounce quiet-window arithmetic, tab-walk order beyond a flat single section, and most verb/creation request shapes are either unpinned or pinned by assertions a wrong implementation would satisfy.

result: Elisp integration-suite audit found 92 spec holes (about 60 MISSING, 30 WEAK, 0 WRONG) across all seven suites; verdict: not adequate to gate the system, with the handover ordering, blink cadence, finish-edge reactions, bounce quiet window, roster walk order, and verb request shapes as the largest gaps.
