# Shim feature coverage against `docs/overhaul/shim.md`

Scope: every behavior-bearing feature in `agent-shim/claude/shim/src/` (4,628 lines,
16 files), checked against what `docs/overhaul/shim.md` accounts for — as a port,
a replacement, a ruled removal, or a deliberate ignore. Implicit coverage counts.
A FINDING is a real existing feature with NO accounting of any of those kinds.

Paths below are relative to `agent-shim/claude/shim/`.

## Findings (most significant first)

1. **`--fake` offline mode and the entire scripted fake query.**
   - Evidence: `src/main.ts:181` (`--fake`), `src/main.ts:430-441`
     (`makeCreateQuery`), `src/main.ts:450-497` (`makeUdsQueryFactory`'s fake arm,
     incl. a fake `subscriptionUsage`), `src/fake-query.ts:1-869` in whole.
   - Why unaccounted: shim.md's "Replacement integration-test specs" replaces the
     14 deleted suites with rpc-level integration specs and "golden transcripts
     (real captures)" for conversion. Golden transcripts are a *converter* input;
     they cannot stand up a live shim process. `--fake` is what lets the whole
     daemon/webapp/Emacs stack run end-to-end with no API key or network, and the
     doc neither ports it, rules it removed, nor mentions it. Nothing in the
     contract sections implies a scripted vendor either.
   - Sub-features individually unaccounted, each purchased against a specific
     past failure:
     - the turn gate (`src/fake-query.ts:35-76`, `AGENT_REPL_FAKE_TURN_GATE`) —
       the only non-interrupt way to hold a turn in flight deterministically;
     - `FAIL_TURN_MARKER` (`src/fake-query.ts:77-91`) — the only offline way to
       provoke a failing turn, spelled identically in the daemon's e2e gate;
     - `onBringUpFailureInjector` (`src/fake-query.ts:138-142`,
       `src/main.ts:464-466`, `AGENT_REPL_E2E_FAIL_RESUMED_FAKE_QUERY`) — resumed
       query dying during bring-up;
     - the branch vocabulary `!tool` / `!hold` / `!md` / `!rotate` / `!agent` /
       `!bg` / `!query-eof` / `!usage-subagent` (`src/fake-query.ts:735-800`),
       each exercising a distinct downstream path (permission gate, live turn,
       markdown render, vendor-uuid rotation, detached agent, backgrounded bash +
       `<task-notification>`, producer EOF, whole-tree usage accounting);
     - the fake's live-task set backing `stopTask` (`src/fake-query.ts:547-551`,
       `:832-857`) — a stop that actually settles the task the way the CLI does.
   - Confidence: high.

2. **The session's SYSTEM PROMPT: `claude_code` preset + metaprompt append +
   `settingSources`.**
   - Evidence: `src/metaprompt.ts:1-104` (whole module; canonical
     `~/.config/doom/.../metaprompt.md`, `systemPromptOption`),
     `src/main.ts:330-331` (`systemPrompt: systemPromptOption()`,
     `settingSources: ["user","project","local"]`), rationale at
     `src/main.ts:301-319`.
   - Why unaccounted: shim.md's `StartSession` takes `fresh {model,
     permission_mode}` and nothing else, and no section mentions a system prompt,
     the harness metaprompt, settings sources, or interactive-CLI parity. These
     are not cosmetic: without the preset the model cannot resolve `~` and invents
     paths; without `settingSources` the session loses the user's permission
     allowlists, hooks, and CLAUDE.md, and the command probe resolves only the 8
     built-ins. The doc's permission-gate section assumes vendor-side policy
     denials (`denied.by_policy`) that only exist when settings are loaded, but it
     never says who loads them.
   - Confidence: high.

3. **The dual kernel-enforced exclusivity claim (session lock + workspace lock).**
   - Evidence: `src/uds/session-lock.ts:1-205` (whole module; `O_EXLOCK`,
     `acquireSessionLock`, `acquireWorkspaceLock`, `workspaceLockKey`),
     `src/main.ts:561-588` (both claims, session-then-workspace, before anything
     else; failure = refusal to start).
   - Why unaccounted: shim.md never mentions exclusivity, the "two shims on one
     conversation means two writers on one transcript" invariant, the lock files
     the daemon probes before spawning, or the Linux loud-refusal. `StartSession`
     is described purely as a vendor bring-up. The invariant is unchanged by the
     transport switch, so its absence from the plan is silence, not a ruling —
     and `runUdsMode` currently *releases* both locks before its stub throw
     (`src/main.ts:601-602`), so a reimplementer has no surviving call site to
     copy.
   - Confidence: high.

4. **Build identity (`SHIM_BUILD_SHA`) and the daemon's stale-shim bounce.**
   - Evidence: `src/build-identity.ts:17-19`, `build.mjs:46-62` (define-time
     substitution; `dist/.built-sha` written by `bin/build-frontend.sh`), and the
     comment naming its carrier — the deleted `protocol.v1` `ShimHello.build_sha`.
   - Why unaccounted: a shim outlives its daemon by design, so a survivor from
     before a deploy runs old code forever; the daemon compares the reported sha
     against the current stamp and bounces a mismatch. shim.md's `StartSession` /
     `SessionStarted` reports model, permission mode, `live_work`, and
     `vendor_session_id` — no build identity, and no ruling that the bounce is
     gone. The old carrier died with `protocol.v1`, so the mechanism is currently
     orphaned and the plan does not re-home it.
   - Confidence: high.

5. **Slash-command list resolution, publication, and refresh via a throwaway
   probe query.**
   - Evidence: `src/main.ts:373-422` (`probeQueryOptions`, `realProbeCommands` —
     derived from the live options, `resume` deliberately dropped, aborted to
     avoid leaking a `claude` child), `src/session.ts:374-407` (publish at start,
     `republishCommands` off the probe because the live query memoizes),
     `src/protocol.ts:111-127` (`SlashCommand`), `src/protocol.ts:207-219`
     (`RefreshCommandsCmd`).
   - Why unaccounted: shim.md's file map cites `slash_command.proto` as "the
     SessionCommand enum + the ContextCut family" — a fixed enum of *session*
     commands, not the per-session resolved list of invocable skills. No rpc, no
     `WatchSession` arm, and no push carries the command menu; the WatchSession
     arm list is enumerated exhaustively and has no `commands` member. The webapp
     completion menu has no other source.
   - Confidence: high.

6. **The rewind-lineage spawn contract and its durable record.**
   - Evidence: `src/main.ts:128-141` (`rewoundFrom`, `rewindRetainedLeaf`,
     `rewindDroppedTurns`), `src/main.ts:204-212`, `src/main.ts:239-283`
     (`parseDroppedTurns` order-is-contract, `validateRewindLineage` — a partial
     set is a loud startup failure so the lineage stays reconstructable).
   - Why unaccounted: shim.md's keep-alive section states the yield obligation
     (a real prompt rolls context back past trailing keep-alive turns) and says
     discarded turns are excluded from replay — so the *purpose* is covered — but
     the mechanism it describes is shim-internal, while today's is the daemon
     truncating the transcript under a NEW vendor uuid and respawning with the
     lineage trio, plus a `SessionRewound` / `KeepAliveDiscard.dropped_turn_ids`
     record. `StartSession` has only `fresh | resume{vendor_session_id,
     remediation}` — no rewind arm, no retained-leaf, no dropped-turn ids. Partial
     coverage of the intent; none of the lineage record.
   - Confidence: medium-high (the mechanism may be intended to change; the durable
     lineage record has no successor named).

7. **The process-signal lifecycle boundary.**
   - Evidence: `src/main.ts:610-643` (`udsShutdownSignalHandlers`: SIGTERM is the
     one authorized shutdown, SIGINT is explicitly REFUSED and logged so an
     attached terminal cannot end the query), `src/main.ts:547-553`.
   - Why unaccounted: shim.md has `KillSession {force}` on the wire, but the
     daemon's deliberate teardown and hibernation drive the shim by SIGTERM at the
     process level, and the SIGINT refusal is a deliberate capability *denial*.
     Neither appears in the plan, and an rpc cannot replace a signal handler for a
     daemon that is going away.
   - Confidence: medium-high.

8. **Resolving pending permission callbacks on interrupt, shutdown, and SDK
   abort.**
   - Evidence: `src/session.ts:495-504` (`cancelPendingPermissions`, called on
     interrupt `:279`, shutdown `:304`, and close `:523`), `src/session.ts:459-463`
     (`options.signal` abort → deny).
   - Why unaccounted: shim.md's permission-gate section covers the three emit
     points (start / success / `denied.by_policy`) and says the shim "already holds
     the pending callback while the agent blocks", but never says what resolves
     that callback when the turn or session is killed instead of answered. An
     unresolved `canUseTool` promise wedges the vendor process, so this is a
     liveness obligation, not a detail.
   - Confidence: medium.

9. **Vendor stream replay handling: `isReplay` gating and per-uuid
   task-notification dedup.**
   - Evidence: `src/session.ts:607-631` (the whole purchased ruling),
     `src/session.ts:641-655` (notification dedup via `seenNotificationUuids`,
     scanned BEFORE the replay guard on purpose), `src/session.ts:663`
     (`if (msg.isReplay === true) continue` for tool results).
   - Why unaccounted: shim.md's Gotchas section is where facts of exactly this
     class are "purchased once", and this one is absent. Store-side `write_id`
     dedup and whole-row upsert make a duplicate *record* harmless, which is
     partial implicit cover, but the asymmetry — notifications must survive the
     replay flag or detached work never settles, tool results must not — is a
     vendor behavior nothing in the doc states.
   - Confidence: medium.

10. **`--claude-bin`: driving the user's system `claude` for vterm version
    parity.**
    - Evidence: `src/main.ts:142-150` (rationale: the user upgrades `claude`
      independently of our lockfile), `src/main.ts:332-334`
      (`pathToClaudeCodeExecutable`).
    - Why unaccounted: shim.md defines "the agent binary" and "the SDK" as
      vocabulary but says nothing about which binary is driven or who chooses it.
      No ruling that the bundled binary is now mandatory.
    - Confidence: medium.

11. **The vendor's compaction-IN-PROGRESS indicator.**
    - Evidence: `src/session.ts:774-796` (forwarding `system` `status:
      "compacting"` — "the `compact_boundary` that ends it is the only other
      signal — there is no progress percentage").
    - Why unaccounted: shim.md says compaction "is OURS: daemon-directed,
      shim-implemented via a throwaway summarizing session", and `ContextCut`
      covers clear/compaction *outcomes*. Vendor-initiated auto-compaction still
      happens, and its start signal (the only thing that can light a
      compaction-in-progress indicator) has no arm named. Possibly a deliberate
      consequence of owning compaction, but not stated.
    - Confidence: medium-low.

12. **The shim-side prompt-cache hit-rate warning.**
    - Evidence: `src/session.ts:36` (`CACHE_HIT_RATE_WARNING_THRESHOLD = 0.8`),
      `src/session.ts:722-772` (`logCacheUsage`, whole-tree vs top-level scope,
      warn below threshold).
    - Why unaccounted: `src/usage-log.ts:1-12` already records that token JUDGMENT
      moved to the daemon, which reads as an implicit ruling that this survivor in
      `session.ts` goes too — but shim.md never says so, and the threshold warning
      is the only place the whole-tree (subagent-inclusive) cache scope is
      computed shim-side.
    - Confidence: low-medium (likely deliberate removal, unstated).

13. **`--version` as a dependency-free bundle smoke.**
    - Evidence: `src/main.ts:157-158`, `src/main.ts:502-508` (loads every static
      import incl. proto stubs, exits before touching a socket or the SDK).
    - Why unaccounted: not mentioned anywhere; it is the cheap check that a built
      bundle is loadable at all, which the new service's much larger static import
      graph makes *more* valuable, not less.
    - Confidence: low (small, but real and cheap to lose).

14. **The stderr-mirror retirement and the durable inherited log sink.**
    - Evidence: `src/uds/log.ts:37-58` (the 2026-08-10 incident: EPIPE on a dead
      daemon's stderr pipe killed every preserved shim), `retireStderrMirror`,
      the poisoned-sink rethrow, `configureLog`'s `process.stderr.on("error")`
      listener; `src/main.ts:219-224` (`--log-fd 3` required).
    - Why unaccounted: shim.md explicitly defers logging conventions to the
      teamlead prompt (not read, per this audit's scope), so this may well be
      covered there. Flagged only because the specific behavior — a shim
      surviving its daemon's death without dying on its own log line — is an
      incident-purchased property tied to the shim-outlives-daemon design that
      shim.md itself relies on.
    - Confidence: low (probable coverage out of scope).

## Checked and accounted for

- **Dead `protocol.v1` transport**: `src/uds/framing.ts`'s envelope half
  (`MessageConn`, `encodeMessage`, `decodeEnvelope`, `envelopeType`, `unpackAs`)
  and `runUdsMode`'s loud-throw stub — named explicitly under "Dead code to
  remove"; the codec half is expressly allowed to survive.
- **`AGENTS.md`'s three-surface story** — explicit rewrite instruction.
- **`src/uds/proto.ts`'s deliberate non-export of `shim.v1` / `store.v1`** — the
  file's own note and the plan agree: added back endpoint by endpoint.
- **One-turn-in-flight and the daemon-owned queue**: `turnsInFlight`
  (`src/session.ts:184-190`), `AsyncQueue` streaming input
  (`src/input-queue.ts`) — replaced by `StartTurn`'s structural refusal.
- **Interrupt `still_queued` reporting** (`src/session.ts:55-84`, `:486-493`) —
  explicitly RULED REMOVED ("the interrupt's still_queued lists are dropped").
  Note in passing: the loud anomaly surfacing goes with it, which is the intended
  reading of the ruling but does delete an error-surfacing path.
- **Native per-task stop** (`QueryLike.stopTask`, `src/session.ts:88-101`) —
  replaced by the per-kind `Stop*` verbs and `UpdateAgent{stop}`.
- **Permission gate mechanics** (`canUseTool`, `src/session.ts:453-474`;
  `PermissionDecisionCmd` handling `:424-451`; `permission_denials` on result
  `:688-712`) — covered by the permission-gate section, incl. the
  question-rides-the-same-gate ruling and `denied.by_policy`.
- **Model menu + mid-session model/permission switching**
  (`publishSupportedModels` `src/session.ts:345-361`; `set-model` /
  `set-permission-mode` `:288-292`; `PERMISSION_MODES` /
  `SWITCHABLE_PERMISSION_MODES` `src/protocol.ts:48-78`) — `api.proto`'s
  `ModelOption`/capabilities plus `SetSessionModel` / `SetSessionPermissionMode`
  and the `model_changed` / `permission_mode_changed` arms.
- **Synthetic-model normalization** (`src/model.ts`) — `api.proto`'s
  `ModelMarker`, read off the schema.
- **Subscription / five-hour rate-limit sampling** (`src/subscription-usage.ts`)
  — `WatchSession`'s `account_usage` arm.
- **Assistant-message API error field** (`src/session.ts:558-563`) —
  `api.proto`'s `ApiRequestFailed` taxonomy.
- **Usage validation and unmodeled-field capture** (`src/api-usage.ts`,
  `src/usage-log.ts`) — the fidelity principle, `TokenUsage`, and the
  loud-on-unmodeled convention.
- **`tool_progress` relay** (`src/session.ts:590-598`) — the `progress` arm and
  the wedge ruling.
- **Result subtype collapse and turn terminals** (`src/session.ts:680-720`,
  `mapResultSubtype` `:828-837`) — `agent.proto`'s ~16-arm turn-stop failure
  taxonomy.
- **Tool results carrying `tool_use_result`** (`src/session.ts:626-676`) — the
  typed-always, never-parse-prose rule and the `tool_use_id` join.
- **Vendor-uuid rotation mid-session** (fake `!rotate`, `src/fake-query.ts:711-732`)
  — `SessionIdentityRotated` and the "changes only the resume handle" ruling.
- **Vendor-call hermeticity chokepoint** (`src/vendor-guard.ts`) — accounted only
  as far as it is the SDK import site the new service must reuse; its role as the
  test-mode guard is entangled with finding 1 and stands or falls with `--fake`.
