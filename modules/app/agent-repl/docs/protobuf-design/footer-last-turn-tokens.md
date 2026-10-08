# The footer's tokens figure stands until the next submission

The owner's request (2026-10-08): the last turn's token accounting in the
footer stays in the footer until the next prompt is sent, and clears the
moment it is sent.

## Landed changes

### 1. `FooterTokensCell.input` carries the most recent turn's figure while idle

- **What changed:** no field moved. The comments on `FooterTokensCell`,
  `FooterTokensCell.input`, `FooterTokensCellInput.text` and
  `FooterTokensCellInput.heat` now say that with no turn in flight the figure
  is the most recent turn's, heat included, and that `--` (uncolored) is drawn
  only when no turn's figure stands.
- **Why it cleared before:** the daemon's tokens resolver
  (`resolve/footer/tokens.go`, `cell`) drew the figure only while a turn was
  in flight, so it fell to `--` on the edge that ended the turn (the main
  agent's terminal, the prompt queue's close, a dead query or shim, a
  compaction's cut). The panel already kept the most recent turn's breakdown
  until the next turn's reset.
- **The one owner of the clear:** the daemon's turn reset
  (`applyTurnStarted` → `tokenState.reset`), which runs at `SetTurn` — the
  daemon accepting the prompt for delivery, the same edge that raises
  `working · submitting`. Not the shim's ack, not the first response. A held
  or queued prompt makes no `SetTurn`, so the figure stands through it. Every
  client draws the cell verbatim, so no client changed.
- **When `--` still stands while idle:** the most recent turn's main agent
  stated no usage — no turn has run, a /clear, or a submission the shim
  refused (its reset cleared the old figure and nothing replaced it). A
  `0 in` is drawn only while a turn is in flight.

### 2. A relaunched daemon restates the figure

- **What changed:** no field moved. `FooterTokensCell`'s comment says a daemon
  relaunch or handover is not a submission, and the relaunched daemon
  restates the figure from the main agent's opening history page.
- **Why:** the usage stamps are durable — settled frames of the main agent's
  book, each `HistoryEntryAt` naming its turn — so dropping the figure across
  a relaunch would clear it on an edge that is not a submission.
- **How:** the first main-agent page the footer reads, only while no turn has
  opened on this daemon, restates the newest turn the page names
  (`restoreLastTurn`, `resolve/footer/history.go`). Only the main agent's
  frames count; the alarm is re-evaluated.
- **What a restated turn lacks:** a verdict (`FooterTokensCell.verdict` and
  `FooterExpandedTokens.verdict` stay UNSET), first-token latency and context
  growth. Those were the watching process's facts. A turn straddling the
  page's boundary is restated from the part the page holds.
