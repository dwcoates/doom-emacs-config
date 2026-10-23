# A focus gain claims the newest banner in its own report

Switching to the workspace of the newest desktop banner when Emacs comes
forward landed long after the refocus. The owner's target is tens of
milliseconds. Measured from the 2026-10-23 logs, the chain from Emacs's focus
event to its frame move was three serial steps:

1. `agentrepl.v1.ReportEditorFocus` (focused), answered in ~10ms.
2. Only on that answer, `agentrepl.v1.ClaimLiveNotification`, which ran
   `terminal-notifier -list ALL` to ask which banners Notification Center
   still lists: 77-160ms, killed at its 160ms bound under load (three claims
   failed that way at 15:13), so no switch at all.
3. `SelectWorkspace` and the frame move.

A focus flap (focused, then unfocused within a few ms) also landed between
step 1 and step 2, and the claim was refused `editor_unfocused` (15:14:36).

Every proto change here is pre-approved by the owner's standing ruling
(2026-10-13: decide contract changes and carry them through, report after).

## Landed changes

### `agentrepl.v1.ReportEditorFocusSuccess.claim` (new oneof)

- WHAT: `no_gain` (1), `nothing_held` (2), `banner` (3, carrying
  `agentrepl.v1.ReportEditorFocusBannerClaimed.workspace`).
- WHY IN THE REPORT: the gain is the report's own edge. The daemon reads the
  previous focus under the same slot lock it writes the new one under
  (`streamslot.Slot.Update` answers the value it replaced), so the claim is
  made under exactly the gain that justifies it. No unfocused report can land
  between the two any more: that refusal is now unrepresentable, not merely
  rare. And the switch costs one round trip.
- WHY THE DAEMON DECIDES THE GAIN, not Emacs: the daemon holds the focus the
  banners are decided on. Emacs's own accepted-focus tracking
  (`agent-repl--focus-accepted`) is gone with the call it gated.

### `agentrepl.v1.ClaimLiveNotification` (removed)

- The rpc and its messages are retired. A peer still calling it is answered
  Unimplemented, loudly, during a rollout's mixed window only.

### No platform query at the claim

- WHAT: `desktopnotify.Backend.Delivered`, the `-list ALL` parse and the
  `AGENT_REPL_NOTIFIER_LIST_BOUND` knob are gone. A handed (macOS) banner is
  held until a claim, a selection, or a later banner of its workspace lets it
  go.
- WHY: the query is a program run on the path from focus to switch, and its
  healthy run (40-76ms) alone is most of the budget.
- WHAT IT COSTS: a banner the user cleared from Notification Center still
  steers the next return to Emacs. Its click and any workspace selection still
  let every held banner go.
