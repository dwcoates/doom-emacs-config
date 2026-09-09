# STATE — overhaul/int-fork-history (PAUSED)

Worktree: `/Users/dodgecoates/.config/doom-overhaul/integration-agents/fork-history`
Branch: `overhaul/int-fork-history`, cut from `overhaul/integration` at `cb085cba9`.
Working tree CLEAN. Two commits landed.

## Ruling 2 — creating or forking selects the workspace — DONE

Commit `c3490676b` feat(lisp/verbs): a workspace you just made is the one you are standing in

- `agent-repl-verb-create` gained a `select` key, default OFF.
  - ON, it reads `CreateWorkspaceSuccess.workspace` — the one place the minted
    identity exists before the roster push carries it — and calls
    `agent-repl-switch-to-project` on its `dir`. That is register's own step
    (`agent-repl-add-project-workspace`), reused rather than re-invented.
  - OFF by default because a one-shot is fire-and-forget and must not steal the
    user's place.
  - A success whose ref carries no dir is REPORTED (error record + message),
    never silently skipped.
- `agent-repl-create-workspace` (`SPC TAB n`) and `agent-repl-fork-workspace`
  (`SPC TAB f`) pass `:select t`.
- Files: `modules/app/agent-repl/lisp/verbs.el`,
  `modules/app/agent-repl/lisp/test-verbs.el`.

Tests (all in `lisp/test-verbs.el`), one edge case each:
- `agent-repl-verbs-create-command-selects-the-created-workspace`
- `agent-repl-verbs-fork-command-selects-the-created-workspace`
- `agent-repl-verbs-create-without-select-stands-still`
- `agent-repl-verbs-create-select-without-a-dir-is-reported`
- Fixture gained `agent-repl-test-verbs--selected` (records
  `agent-repl-switch-to-project` calls, the way tab teardown is recorded) and
  `agent-repl-test-verbs--created` (scripts a create success carrying a ref).
  Three existing create/fork command tests now script that answer, because the
  verb reads the success it previously ignored.

Commit `8f7675669` test(e2e/playtest): A.5 asserts the create SELECTED what it made
- `e2e/playtest_02_create_and_fork_test.go`, `TestPlaytestNewWorkspaceAndChild`:
  the "did creating select it?" narration is replaced by
  `playtestAwaitCurrent` plus a fatal if `playtestStandOn` reports a switch was
  needed. Manifest sentence updated.

### Gate results so far
- `bin/byte-compile.sh` — exit 0, zero warnings.
- `emacs -batch -Q -l ert -l lisp/test-verbs.el` — 114/114, 1.14s.
- lisp aggregate `lisp/test-agent-repl.el` — 3738/3738, 34.9s.
- daemon `gofmt -l` clean, `go vet ./...` clean,
  `TMPDIR=/tmp go test ./... -count=1` — all ok, 8.05s wall.
- daemon integration through the gate
  (`bin/suite-slot.sh env TMPDIR=/tmp go test -C daemon -tags integration ./integration/... -timeout 180s -count=1 -parallel 8`)
  — green: `integration 17.294s`, `fakegit 0.035s`, `fakeshim 0.017s`,
  `harness 0.673s`; 4:20 total wall of which 240s was queueing for slot 1.
  NOTE: no daemon change was made, so this is the BASELINE; there is no "after"
  to compare it with yet.
- `gofmt -l e2e/` clean, `go vet -tags playtest ./...` clean in `e2e/`.

### Ruling 2 gates NOT yet run
- the full Go e2e package on the host
- the two playbooks in the sandbox (`bin/suite-slot.sh bin/playtest.sh -run 'TestPlaytestNewWorkspaceAndChild|TestPlaytestForkWorkspaceAndConversation'`)
- the Emacs e2e layer 51/51 (`bin/test-e2e-emacs.sh`) — this one MATTERS, the
  selection behavior changed and several scenarios switch workspaces.

## Ruling 1 — the fork's feed shows the parent's history — NOT IMPLEMENTED, BLOCKED

Nothing was written for this. The playbook A.6 assertion is UNCHANGED (it still
asserts the absence). Reason: the brief's stated cause is not the real one, and
the real one cannot be closed inside this brief's scope.

### The brief's premise is wrong

The brief (and A.6's own recorded note) say the feed is composed from "the
daemon's own per-workspace conversation rows, keyed by workspace id". There are
no such rows. `wsm`'s schema has `turns` (prompt text and lifecycle only) and
nothing else conversational. Feed rows are composed by
`daemon/internal/resolve/feed` from `conversation.v1.HistoryPage`s the SHIM
serves (`shim.v1` ReadHistory / WatchAgent), which the shim reads out of the
singleton record store (`store.v1`, `agent-shim/shim-store`). A book in that
store is keyed by `conversation.v1.AgentId`, and the main agent's AgentId is the
conversation's ORIGINAL vendor session id.

### The real cause (traced, not run)

1. `verbs.forkTranscript` (`daemon/internal/workspace/create.go`) calls
   `Accounts.PortTranscript`, which is a BYTE COPY under a new filename
   (`internal/account/transcript.go`, `transferSpec.destID`). Every record
   inside keeps the parent's `uuid`, `message.id` and `tool_use_id`.
2. The child's shim, resuming that copy, finds no `agent-id.json` for the
   child's workspace key and ADOPTS the forked vendor session id as the
   ORIGINAL (`agent-shim/claude/shim/src/engine/identity.ts`,
   `SessionIdentity.resume`). So the child's book is a NEW book.
3. The sidecar discovers the ported file, resolves its book to that same new id
   (`shim-sidecar/internal/identity`, `cycle.go: mainAgentFor`), converts the
   records, and mints upsert keys that are derived from RECORD CONTENT ONLY —
   `activity:<message id>:<block index>`, `session:<arm>:<uuid>`,
   `terminal:<agent>:<uuid>`, `question:<tool_use_id>`
   (`shim-sidecar/internal/convert/keys.go`).
4. Those keys already exist in the store under the PARENT's book, and
   `upsert_key` is globally UNIQUE. `DB.requireStableIdentity`
   (`shim-store/internal/db/write.go:294`) refuses: "would move the row from
   book A to book B — an upsert supersedes a row's content, never its
   identity". The batch fails, the file is parked, its cursor never advances.

This is the SAME failure `e2e/coldgate_e2e_test.go`'s header documents for
`/clear`, and it was fixed there by teaching the sidecar to resolve two files to
ONE book. A fork is the opposite shape: one set of records that must exist in
TWO books. There is no fork handling anywhere in the file plane — grep for
"fork" under `shim-sidecar` finds only prose and an unrelated subagent flag.

### Why each candidate fix is out of scope

- **Let the fork keep the parent's AgentId.** This is literally what
  `conversation/v1/agent_activity.proto`'s `AgentId` comment says ("a resume,
  rotation or fork keeps it"), and it makes the refusal disappear — but then the
  fork's OWN rows land in the parent's book and the parent's feed draws the
  fork's conversation. That is the double-draw the brief forbids. THE PROTO
  COMMENT AND THE PRODUCT RULING DISAGREE HERE; the lead should settle it.
- **Scope the upsert-key space by book.** The correct expression of "these
  records legitimately exist in two books". `upsert_key` is documented as opaque
  and producer-minted, so no proto change is needed — but BOTH producers mint
  keys into one space (the shim's stream plane and the sidecar's file plane
  must agree, or a file-plane row stops superseding its stream-plane row), so
  this is a shim + sidecar + store cross-plane landing, not a daemon leaf.
- **Rewrite the ported transcript's record ids at fork time.** Puts deep vendor
  JSONL knowledge (uuid/parentUuid/message.id/tool_use_id remapping) into the
  daemon, which is explicitly not the daemon's plane.
- **Capture the parent's `HistoryPage` at fork time and replay it into the
  child's feed** (daemon-only; a new `wsm` table, `ReadHistory` added to
  `workspace.Deps.Shim`, and a replay ahead of the child's own opening page in
  `sessionwatcher.routeOpeningPageLocked`). This is implementable inside the
  daemon and was the design I was about to build. Two things stop it:
  - the capture needs the PARENT's shim to be live at the instant of the fork,
    and a fork off a hibernated parent has no live shim. Refusing that fork
    needs a refusal arm `CreateWorkspaceError` does not have (the existing
    `fork_parent_has_no_conversation` would be a lie — the parent has one).
    **THAT IS THE MISSING PROTO FIELD**: a `CreateWorkspaceError` arm for "the
    parent's conversation could not be read at the fork" (or, equivalently, a
    fork-point marker the fork could record without reading the parent).
  - it leaves the parked-transcript defect above standing, so the fork's file
    plane stays dead for the life of the workspace.

### What is next for ruling 1

The lead decides between: (a) settle the proto's "a fork keeps the AgentId"
against the no-double-draw ruling; (b) commission the book-scoped upsert-key
landing across store/shim/sidecar; or (c) approve the daemon-side capture design
plus the new `CreateWorkspaceError` arm it needs. Nothing here should be built
until that is chosen.

## What is next overall

1. Run the outstanding ruling-2 gates (Emacs e2e layer 51/51 first, then the
   host Go e2e package, then A.5 in the sandbox).
2. Wait on the lead for ruling 1.
