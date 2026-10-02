# UI and lifecycle wave (2026-10-02)

The protobuf decisions behind the owner's 2026-10-01 agent-repl feature list
(`docs/session-instructions/astra-2026-10-01-verbatim.md` at the repo root).
The `.proto` files are the contract; this file records what changed, why, and
what an implementer must know that the schema does not say.

## Principles stated by the owner

- Effort and model changes hold "for the rest of the session" in the workspace,
  and both cost prompt-cache misses, which the topbar states on hover.
- Changing effort never creates a visible row in the feed.
- The effort selector's starting value is the session's default, read by the
  daemon from the canonical Claude settings location at workspace
  initialization, never a placeholder.
- The topbar context figure is colored by the SAME code that colors the
  footer's allowance percentages, keyed on the fraction of the context window
  in use.

## Decisions

### 1. The quiet tier is retired

- WHAT: `frontend.v1.FooterActivityQuietStretch`,
  `frontend.v1.FooterStatusQuietStretchEnding` and
  `frontend.v1.FooterActivityTransientOverQuietOverEnduring` are deleted.
  `frontend.v1.FooterStatusWorking.quiet_stretch_ending` (tag 12) and
  `frontend.v1.FooterStatusBackground.quiet_stretch_ending` (tag 2) are
  reserved. `frontend.v1.FooterStatusWorkingActivity.unpinned` and
  `frontend.v1.FooterStatusBackgroundActivity.unpinned` are now
  `frontend.v1.FooterActivityTransientOverEnduring`, the shape every other
  status already used.
- WHY: the owner found the quiet-stretch lines ("✅ Bash finished — handling
  result...") too chatty and of next to no information value, and asked for
  every such line to be removed entirely.
- CONSEQUENCES:
  - The tier model is three tiers (salient, transient, enduring). Every
    document that states four tiers is updated with the code:
    `AGENTS.md`, `daemon/AGENTS.md`, `webapp/AGENTS.md`.
  - Daemon: `daemon/internal/resolve/footer/quietstretch.go` and its test go
    away; the quiet-stretch plumbing in `activity.go`, `api.go`, `state.go`,
    `status.go`, `transient.go`, `daemon/internal/resolve/feed/api.go` and
    `daemon/internal/sessionwatcher/api.go` is removed, not stubbed.
  - Webapp: `webapp/src/footer/quiet-hold.ts` and the paint-tracking it
    needed in `webapp/src/feed/painted.ts` go away, unless `painted.ts` has
    another consumer. `footer.ts` and `activity.ts` draw `unpinned` with the
    existing transient-over-enduring code path.
  - Tests referencing the deleted symbols are deleted, not adapted. The
    replacement coverage is: the working and background statuses never
    carry a line composed from a landed feed item (daemon unit test on the
    footer resolver), the webapp draws the enduring line after a transient
    lapses under `working` (webapp unit test), and the e2e
    `footeractivity_e2e_test.go` asserts that no footer push between two
    feed items carries a feed-item-derived line.

### 2. The effort selector

- WHAT: `frontend.v1.TopbarView.effort_selector` (tag 14,
  `frontend.v1.TopbarEffortSelector`) with `supported` and `unsupported`
  arms; `frontend.v1.TopbarEffortOption`; the endpoint
  `agentrepl.v1.SetEffort` (`endpoint_set_effort.proto`); the shim endpoint
  `shim.v1.SetSessionEffort` (`endpoint_set_session_effort.proto`); and
  `conversation.v1.SessionEffortChanged`, carried by the shim's success.
- WHY: the owner wants an effort selector between the model and
  permission-mode selectors, in the same form as both.
- CONSEQUENCES:
  - The selector is OPTIONAL BY PRESENCE exactly like the model selector:
    absent means no session, and the client draws its dash in the slot.
  - The `unsupported` arm follows the selected model's
    `conversation.v1.ModelCapabilities.effort_support`. A model with no
    capability block stated gets no guess either way: the selector is
    ABSENT.
  - The starting level comes from the daemon reading the session's Claude
    settings (the config root the session spends as) at workspace
    initialization. When the settings leave the level unset, the vendor's
    own default applies. The implementer finds where that default is stated
    programmatically. If nothing states it, the implementer asks the
    orchestrator rather than hard-coding a level. Either way, the log record
    names the source the level was read from.
  - The effort switch is NOT cold-gated, unlike the model switch: the owner
    asked for a hover disclaimer, not a refusal. The shim applies it from
    the next turn on, and resolves after the current turn ends, mirroring
    `shim.v1.SetSessionModel`.
  - The hover text on both selectors is client-owned static copy:
    model: "Changes the model this workspace uses for the rest of the
    session. Will cause token cache misses."; effort: "Changes the agent's
    reasoning effort in this workspace for the rest of the session. Will
    cause token cache misses."
  - No feed row and no `/effort` user message is ever fabricated.

### 3. The context chip's window fill

- WHAT: `frontend.v1.TopbarContextChip.window_fill` (tag 3), a 0..1 fraction.
- WHY: the owner wants the topbar's yellow token figure colored by the same
  gradient the footer's allowance percentages use, keyed on the percentage
  of the context window in use.
- CONSEQUENCES:
  - The window size is the vendor's figure when the vendor states one, and
    1,000,000 tokens otherwise. The owner explicitly sanctioned that
    assumed figure; it is the only one.
  - Webapp: `footerPercentColor` (`webapp/src/footer/activity.ts`) becomes a
    shared helper used by both the footer allowance percentage and the
    topbar chip, with a test asserting both call sites use it.

### 4. The roster row states its live detached work

- WHAT: `frontend.v1.RosterRow.detached_live` (tag 40,
  `frontend.v1.RosterRowDetachedLive`), a presence-only marker set while
  detached work runs, on any status arm. The comments of
  `agentrepl.v1.MarkWorkspaceViewed` and `frontend.v1.RosterRowViewed` now
  also name the daemon's own read-on-cut.
- WHY: the owner's viewed-timing rule 2. A `done` row with background work
  goes PARTIAL (yellow, `idle_async`) after one second of viewing rather
  than five. An unread `done` deliberately outranks `idle_async`, so the
  status alone cannot tell the editor that background work runs.
- CONSEQUENCES:
  - The dwell threshold stays the editor's own (`lisp/status.el`
    `agent-repl--tab-dwell-seconds`); the marker only states the fact it
    is chosen from. The daemon does not pick thresholds.
  - A completed `/clear` or compaction is marked read by the daemon itself
    on the push that ends the cut (rule 3), with no editor dwell.

### 5. The compaction summary's fold is retired

- WHAT: `frontend.v1.FeedContextCutCompacted.fold` (tag 2) is reserved, and
  `frontend.v1.FeedContextCutFold` is deleted.
- WHY: the owner wants the orange-border summary bubble always drawn under
  the compaction bar, in its normal collapsed bubble form, with no
  "Summary" disclosure. A fold state nobody draws is an obviated field.
- CONSEQUENCES:
  - The daemon stops setting it (`daemon/internal/resolve/feed/separation.go`).
  - The webapp stops requiring it in its decoder. The bubble's own expand
    toggle is the only fold the summary has.

## Features that need no protobuf change

- Persistent-wifi click: the webapp calls the existing
  `agentrepl.v1.UpdatePersistentWifiMode` with the `toggle` arm, the same
  endpoint Emacs's `agent-repl-persistent-wifi-mode-toggle` calls.
- Scroll to the bottom on returning to a workspace, viewed-state timing,
  startup and bounce bring-up, compaction summary display, topbar spacing,
  text selection and copy, and the Emacs input window height. An
  implementer who finds one of these needs a contract change asks the
  orchestrator; implementers never edit `.proto` files.
