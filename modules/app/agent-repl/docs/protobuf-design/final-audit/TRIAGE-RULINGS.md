# Final-audit triage rulings — 2026-08-29

Every finding in the seven final-audit reports was walked with Dodge
one-by-one (AskUserQuestion, dedicated options per finding). This ledger
records each disposition. Legend: DOC = lands as doc edits; PROTO = also a
sanctioned contract increment; DEAD = feature ruled removed; SKIP = nothing
lands; WAVE = known-open note, implementers escalate; INV = investigation
owed before landing.

## Vetting-gaps

1. Unruled producer-less arms — DOC: shim.md KNOWN-OPEN best-effort list
   (fill if the wave finds a producer, unset otherwise, no escalation owed).
2. Capture harness vs mock mandate — DOC: PROJECTLEAD.md carve-out for a
   ONE-TIME supervised real-capture run (user-approved, projectlead
   dispatches); mock scripts built FROM captures; harness assigned to the
   shim teamlead.
3. SDK pin + upgrade canary — DOC: shim.md pins the SDK version + one
   integration test asserting every relied-on method/shape exists;
   get_usage relied on, canary covers its reshape risk.
4. question free_text — DOC: shim.md defines the producer — echo-validation
   extracts the residue left after removing validated labels; that residue
   IS free_text.
5. Compaction bottom rung — DOC: shim.md gains a compaction
   integration-test subject (write → resume → serve) + the explicit
   undocumented-field-union warning (test doubles as the experiment).
6. Refuted transcript premise — DOC: daemon.md sentence corrected — the
   STORE must hold every failure frame; the vendor transcript provably
   lacks several failure classes and is never their recovery source.
7. Orphaned background processes — DOC: freeness (no turn, no detached
   work) is a rollout INVARIANT at shim kill; the CLI kill is
   inconsequential by design, and that is noted.
8. Ctrl-B'd agent detach cause — DOC: shim.md gotcha scoped to SHELLS;
   task_updated{is_backgrounded} named as the agent-case candidate
   producer; live observation assigned to the wave.
9. Unmodeled-tool warning legibility — DOC: name + count only; the
   abbreviated-account requirement is dropped (raw call is in the store).
10. Keep-alive rewind reliability — DOC: shim.md integration-test subject
    (prompt after keep-alives → served context excludes them).
11. Footer Status×SubStatus coverage review — DROPPED (waived).
12. Owed H never-nil parent vs store.md — DOC: one-line supersession note
    in store.md.

## Deferred-gaps

1. /agents + /help panels producer-less — WAVE: KNOWN-OPEN in daemon.md;
   shim probe-resolved catalogs named as the restoration path;
   projectlead decides.
2. /context panel — PROTO: full support; the protobuf redesigned by the
   orchestrator (me), strongly structured per the vendor's actual
   get_context_usage schema (tool calls as an encapsulated list, etc.);
   webapp renders the panel CUSTOM — tool calls in an automatically
   folded foldable render; rest of presentation the implementer's.
3. Workflow drawing — WAVE-KICKED: workflow APIs (WatchWorkflow, store
   table, sidecar ingestion) stay but are NOT to be implemented now; both
   prompt docs say so; frontend surface is future work.
4. Prompt origin durability — PROTO: StartTurn's origin persists onto the
   turn's durable record (conversation.v1/store) so replay can route
   merge-born rows and label restart re-drives.
5. + 6. Forbidden pointers — DOC/SWEEP: remove every "see the
   deferred-work metadocument" (and any pointer to planning docs or
   skills) from proto comments and docs/protobuf-design files; only real
   source/proto files may be suggested reading.
7. /status near-empty — DOC: one-line notes in daemon.md + webapp.md
   predicting the degraded panel (version + spliced rows only).

## Daemon coverage

1. Login — INV: the CURRENT implementation solves login without the TUI;
   investigate how and land THAT mechanism (do not port the TUI).
2. Fresh-conversation gate — DOC: daemon.md INVARIANT — StartSession(fresh)
   only with proof the workspace never had a conversation; else resume or
   refuse loudly.
3. Transcript backup — DOC: SHIM owns it (turn-end + rotation capture,
   pruned, restorable); lands in shim.md.
4. Shim-less replay — RULED: mounting a parked workspace's frontend IS an
   implicit revival (spawn on mount); one history path via ReadHistory.
5. File ingress non-merge verbs — DOC: the file route survives as a
   general non-Emacs ingress; verbs map onto the same internal paths as
   the rpcs.
6. Configured merge actions — PROTO: CreateWorkspaceRequest gains
   before_ws_merge + postprocessing_prompt (part of the creation-facts
   increment).
7. Keep-alive — RULED: wholly shim-internal; daemon knows nothing; remove
   "keep-alive turn" from daemon.md's genuine-holds list; shim.md states
   the ownership (store exclusion rides the shim's stamp).
8. Withholding family — DOC: sidecar/shim conversion classifies the
   withheld classes into non-feed kinds at ingest (machinery, skill-body,
   notification envelope, no-response synthetic, context-cut exclusions).
9. Kernel locks — RULED: the SHIM holds both kernel locks (session +
   workspace) for its lifetime; daemon PROBES them; rollout transfer waits
   on the shim's workspace lock; daemon.md handover wording corrected.
10. Webapp asset origin — DOC: daemon serves the assets; entry point
    re-stat'd per request; Cache-Control no-store on the entry point only.
11. Hibernation directive — PROTO: hibernation is a first-class shim
    directive — daemon calls a Hibernate rpc; shim compacts, then acks;
    daemon then stands the shim down. daemon.md + shim.md + contract.
12. Merge replay — DOC: minimal durable merge-lease ledger in WSM (lease
    id, tab intervals) joined with the persisted turn origin to rebuild
    tab membership on replay.
13. Bounce accountability — DOC: intent manifest at stand-down + boot
    reconciliation against kernel locks; four verdicts surfaced per
    workspace.
14. Fault closure — DOC: WSM fault records carry open/closed with a
    persisted resolved-at instant; daemon writes the closing edge.
15. Duplicate-daemon arbitration — DOC: bind-the-address-first claim; a
    successor is distinguishable by its explicit joining argument; an
    unflagged second daemon exits without touching the incumbent's
    sockets.
16. Restart announcement — PROTO: shutdown_announced gains cause +
    expected-outage bound + minted-at (no address = plain bounce).
17. Fan-wide cancel — PROTO: CancelDetachedAgents verb + Interrupt refusal
    arm confirm_required{live_agent_count} answered by confirm resend.
18. User rewind — DEAD (keep-alive discard stays shim-internal; the three
    durable columns die).
19. Permission decline — RULED (deliberate reversal): decline is
    deny-and-continue; Interrupt is the stop.
20. state.db — RULED: fresh WSM database; old state.db abandoned in place.
21. WSM retention — DOC: live never pruned; terminal records capped per
    table; growth bounded by construction.
22.+23. Log surfaces — DOC: port the full existing design (shared-fd
    per-workspace targets with borrow semantics, caps, eviction; capped
    run log fatal-on-open-failure; ClientLog lands in the owning
    workspace's log; terminal mirror never blocks the durable sink).
24. Skill windows — RULED: no temporal fold; the skill card is populated
    from the TWO shim messages (invocation + skill content) and nothing
    else joins the bubble.
25. /open-external — PROTO: OpenExternal{url} verb (WEB LINK section);
    daemon opens the pinned profile.
26. Widget/chess surface — DEAD (both halves; /show-chess-game degrades).
27. add-support — DOC: flow ported (offer on the unsupported-command
    card; daemon-composed brief spawns the support workspace).
28. Boot-sweep verdicts — SKIP (logs suffice).
29. pprof — DOC: opt-in local-only pprof surface, opened before
    dependency boot (daemon.md + store.md).
30. CLI subcommands — DEAD.
31. Prompt files — DOC: briefs read from prompts/ at use time,
    customizable, loud on absence.
32. Resume guards — DOC: both — vanished-transcript typed refusal before
    spawn + resumed-identity verification (mismatch errors).
33. Git env hygiene — DOC: strip inherited GIT_* selectors; -C is the
    only repository selector.
34. Page position — RULED: per-reader ephemeral, dropped at open; nothing
    persists (daemon.md WSM inventory corrected).
35. Turn tombstone — SKIP.
36. Typing cut — RULED: the no-timer rule is relaxed for exactly this — a
    preview with a dead stream may be retired client-side.
37. Creation facts — PROTO: priority + fork-from + ungated-consent flag
    all join the creation-facts increment (with #6's fields, elisp #4's).
38. Wire version — SKIP.
39. Corrupt durable state — DOC: generalized WSM invariant — corrupt or
    partial reads refuse the load loudly, every table.
40. Model ordering — DOC: (a) model fact is last-writer-wins by
    SHIM-observed order; (b) bare /model is refused/absorbed daemon-side.
41. Unwatched machine turns — DOC: boot/adoption re-opens watches or
    reconciles via GetLiveWork for every in-flight daemon-originated turn.
42. Deploy chain — DOC (one line): build mechanics are the deploy
    script's domain; rollout invokes it; proto prefix = everything.
43. Durable receipts trio — SKIP (force-kill surfaces as workspace error;
    user re-prompts).
44. Boot observability — SKIP.
45. Env contracts — DOC: all four (-fake mode, FORBID_VENDOR_CALLS,
    AGENT_REPL_STATE_DIR as a named cross-process contract,
    AGENT_REPL_OWNED spawn marker).
46. Merge terminal durability + post-merge session — SKIP (resumed-or-
    loudly-failed covers it; ordinary close path).
47. Key canonicalization — SKIP.
48. Schema doctrine — SKIP (nuke ruling covers it).
49.+50. Ack latency / rebase sweep / restart epoch — SKIP (all three).

## Elisp coverage

1. Turn axis — RULED: Emacs subscribes WatchWorkspaceRoster and paints
   tabs + sidebar dot from it (one source of truth; no HostWorkspace
   change).
2. Finish edge — DOC: the roster turn-running→idle transition is the
   finish edge; all four reactions (banner, echo, magit refresh, queue
   drain) survive as Emacs-local reactions.
3. Rename — DEAD (names daemon-minted, permanent; added to removals).
4. Creation gestures — PROTO: name, parentage (source workspace), and
   model all join the creation-facts increment.
5. One-shots — PROTO: a dedicated one-shot creation form (likely an arm
   on CreateWorkspace); Emacs supplies {prompt, model, parentage}; the
   DAEMON owns the full sequence (name, worktree, decoration, merge/PR
   postprocessing).
6.+16. Prompt decoration — BLESSED: Emacs composes freely BEFORE
   submission (metaprompt, canned SPC j prompts, suffixes); verbatim
   means no post-submission rewriting.
7. Completion — DEAD (composer completion dies; panels stay
   wave-escalated per deferred #1).
8. Open ladder — DOC: blessed as host-native UX over Emacs-observable
   stages, stall diagnosis included; no wire change.
9. Codex — DEAD for now (vendor oneof stays extensible).
10. Daemon supervision — DOC: Emacs owns COLD START (auto-start,
    stale-binary rebuild-and-start, foreign-daemon handling, build-failure
    surfacing); post-boot is the daemon's blue-green.
11. Restart reason — PROTO: schedule{at_ms, reason} (required) + a
    drain-scheduled WatchDaemon push carrying reason + at_ms (feeds the
    webapp drain banner too).
12. Tab order — RULED: Emacs tabs follow roster order strictly;
    push/pull + priority reordering + hide-project-dirs die; folding
    affects the sidebar only.
13. Priority setter — PROTO: SetWorkspacePriority (or UpdateWorkspace
    arm); roster resolver orders by priority; roster row carries the
    badge fact.
14. Tasks — PROTO: CreateTask / UpdateTask{title|done} /
    AssignWorkspaceTask; WSM stores tasks + assignments; roster task view
    fillable. Org notes files stay Emacs-local.
15. explain-config — DEAD.
17. Pre-close gns round-trip — DEAD.
18. Readiness segment + recovery SLO — DEAD (both).
19. Tab rehydration — RULED: the daemon is the source; Emacs opens tabs
    from the roster on connect; the local durable snapshot dies.
20. Local presentation axis — BLESSED: pushed arm × local-only facts
    (panels-dismissed, dwell fade) is legitimate host presentation.
21. Permission banner — PROTO: a permission ask fires the workspace's
    notification push and sets attention; the notification arm gains a
    TYPED kind oneof (agent_addressed | permission_requested | …) with
    the composed text alongside; Emacs's existing policy does the rest.
22. Restart compound — RULED: SPC o C-c calls RestartWorkspace{force};
    the daemon owns what that entails INCLUDING bouncing the webapp (if
    Emacs coordination is needed, Emacs reloads the webview after the
    daemon's ack).
23. Webview lifetime — RULED: pool/pre-creation + staggering blessed
    ("bound from first mount"); rescue survives as a manual escape hatch;
    stale-bundle sweep dies (reload_webapp replaces it).
24. Link-degraded banner — DEAD.
25. Editor-local tail (walked individually):
    - KEEP: tab-bar geometry; panel/window discipline; commit-emoji +
      hook; magit integrations; interaction record/replay; autosave
      sweep; /runtime-eval-code; composer input history (persistence,
      fuzzy search, glyph); the five misc commands (copy-reference,
      copy-workspace-name, revert-and-eval, reload-config, print-branch).
    - DEAD: external-browser browse-url pinning (daemon verb only); debug
      keymap; memory-state dump + sentinel recovery; skill-symlink
      auto-installer (install.sh owns provisioning); ALL webview key
      affordances (copy chords, chess stepping, text size); per-workspace
      clipboard slot, data payloads, PGN popup, profiler dump; sidebar
      keyboard nav + single-prompt guard; output-feed navigation.

## Webapp coverage

1. Login — INV (rides daemon #1: land the current non-TUI mechanism).
2. Chess widget — DEAD (with daemon #26).
3. Host hooks — DEAD: NO window.agentRepl* surface at all; the webview is
   purely daemon-driven.
4. In-feed search — DEAD.
5. Bubble navigation — DEAD (with elisp tail O).
6. Copy chords — DEAD (with elisp tail K).
7. Permission mode — PROTO: SetPermissionMode verb (mirrors SetModel);
   picker survives; the standing ungated banner dies.
8. Counter chips — DEAD: footer live-work chips are the successor;
   turn-aged retention dies (live work only).
9. Outage prompt queue — DEAD: the Emacs composer's hold-and-replay is
   the outage absorber.
10. add-support offer — ported (daemon #27's client half).
11. gns bridge fold — DEAD.
12. TLDR-tree re-render — KEEP (blessed as a rendering-layer nicety).
13. Capped sections — KEEP all three (click-to-expand, edge-gated scroll,
    tail-follow) as a webapp.md line.
14. Drain banner — covered by elisp #11's reason-on-the-wire push.
15. Catalogue page — DEAD (projectlead may recreate something similar at
    its choice — noted in PROJECTLEAD.md).
16. Metaprompt-span stripping — DOC: the DAEMON's feed resolver strips
    sentinel-marked spans from the drawn prompt (full text stays on the
    record); sentinel convention noted in elisp.md.
17. Title tracking + log toggle — DEAD (both).

## Shim coverage

1. --fake + scripted vendor — LANDS (with daemon #45 and the vetting #2
   capture carve-out: mock rebuilt from captures, wide prompt array).
2. System prompt — DOC: claude_code preset + harness metaprompt append
   (file-based metaprompt.md survives) + settingSources
   user/project/local, named load-bearing for the permission gate.
3. Kernel locks — shim holds both (daemon #9).
4. Build sha — PROTO: SessionStarted (or WatchSession hello) carries the
   shim build sha; daemon bounces stale shims at freeness.
5. Command-list probe — folded into deferred #1's wave escalation.
6. Rewind lineage — dies with daemon #18 (keep-alive rewind is
   shim-internal).
7. Signal boundary — DOC: SIGTERM = graceful stand-down (same path as
   KillSession); SIGINT refused and logged.
8. Permission callbacks — DOC: every teardown path resolves pending
   permission callbacks as denied — an unresolved gate callback is a
   wedge.
9. Replay gating gotcha — SKIP.
10. --claude-bin — DEAD (bundled pinned binary only).
11. Compacting indicator — PROTO: a compacting SessionUpdate arm
    (start; ContextCut is the end).
12. Cache-hit warning — DEAD.
13. --version smoke — DOC: kept.
14. Log-sink survival (EPIPE property, --log-fd 3) — DOC: kept.

## Store + sidecar coverage

1. Store-link invariant — DOC: connection-free restatement — every
   production cycle begins with a successful cursor read; any store error
   suspends ALL production until recover-then-rescan succeeds.
2. Hold/rewind protocol — DOC: stated in the cursor section (bounded to
   one redelivery; out-of-batch holds refused loudly).
3. TaskStop terminal — DOC: exempt set amended — the CALL stays dropped,
   the RESULT is consumed as the owning task's CANCELLED terminal.
4. Held spools — RULED (reversal): NEVER drop — aged unowned spools
   ingest as residue with unknown ownership; total ingestion absolute.
5. Backpressure — DOC: buffering bound is the lead's call; if bounded,
   substantially higher than 1024 (daemon bounces happen).
6. Spool kinds — DOC: three kinds by prefix (b* shell, a* agent, w*
   workflow), each routed to its conversion; unknown prefix = loud
   violation.
7. SQLite/ordering — SKIP.
8. pprof — kept (daemon #29).
9. Slow-query warns — DEAD.
10. Discovery roots — DOC: multi-root requirement stated (both account
    config roots, configurable); latency mechanism implementer's.
11. write_id derivation — DOC: deterministic digest of source
    coordinates; randomness forbidden (store.md + sidecar.md).
12. Owner rules — only rule 1 lands (symlink normalization before path
    comparison); ranking/similarity/conflict rules SKIP.
13. Log correlation — DOC: store/sidecar work with the projectlead to
    consolidate the structured-logging + correlation scheme, unified
    under /debug-emacs-agent-repl.
14. Bounded-read losses — SKIP.
15. Line taxonomy — SKIP.
16. Context-lifecycle conversion — DOC: /clear via envelope unwrapping +
    compaction boundary+summary coalescing in FILE order → ContextCut.
17. Tail resets — SKIP.
18. Doctor probe — DOC: JSON/exit-code contract dies; doctor updated to
    probe Connect endpoints (store teamlead's task).
19. api_error records — DOC: transcript api_error converts to
    ApiRequestFailed (mid-turn evidence, not a terminal).

## Owed follow-ups

- INV login: read the current daemon/webapp code to find the non-TUI
  login mechanism; land that design (daemon.md + webapp.md + any verb).
- PROTO increments to land: /context rich schema (orchestrator-designed
  from the vendor's get_context_usage), durable prompt origin, creation
  facts (merge actions, priority, fork-from, consent, name?, parentage,
  model, one-shot arm — note elisp #4 "All three" includes NAME),
  Hibernate rpc, enriched shutdown_announced, CancelDetachedAgents +
  confirm_required, OpenExternal, SetPermissionMode, SetWorkspacePriority,
  task verbs, typed notification kinds, drain-schedule reason + push,
  shim build sha, compacting arm.
- SWEEP: planning-doc/skill pointers out of proto comments and
  docs/protobuf-design files.
- Doc landings across all six system docs + both prompt docs per the
  rulings above.
