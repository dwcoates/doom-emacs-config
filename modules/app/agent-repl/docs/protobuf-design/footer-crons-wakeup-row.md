# The ⏱ panel draws the pending wakeup the ⏱ chip counts

The owner's report (2026-10-17): the `agent-repl-response-tree` workspace's
footer showed a scheduled-work chip reading "1", and its expanded panel read
"nothing scheduled". The chip must never show for detached work that has
nothing corresponding to it in its panel.

The cause, shown by the workspace's log: the workspace held a pending
self-scheduled wakeup (`daemon.footer.activity_line_changed`,
`kind=salient.wakeup`). `FooterLiveWorkChips.crons` is specified as counting
"cron/wakeup schedules", and `FooterChipCrons.count` as "how many rows the
scheduled-jobs panel holds", but `FooterExpandedCrons` had no way to carry a
wakeup. So the daemon counted the wakeup in the chip and the panel could not
draw it.

## Landed changes

### 1. `FooterExpandedCrons.wakeup` (new, tag 2, additive) and `FooterWakeupRow`

- **What changed:** `optional FooterWakeupRow wakeup = 2` on
  `FooterExpandedCrons`, with the row's elements `FooterWakeupRowReason` and
  `FooterWakeupRowWakeAt`. `FooterChipCrons.count` is restated as the cron
  rows plus the wakeup row.
- **Why the panel gains the row, rather than the chip dropping the wakeup:**
  the chip's contract already names wakeups as scheduled jobs. The panel was
  the side that broke it. Dropping the wakeup from the chip would leave a
  pending wakeup with no ⏱ presence at all outside the idle-only status
  fallback.
- **Why a single optional row and not an arm of a list:** the daemon holds at
  most one pending wakeup per session (`wsState.wakeup`), so a list would make
  an impossible state representable. Changing `rows` to a list of a `oneof`
  would also have been a breaking change to tag 1.
- **Why `wake_at` is optional:** the daemon records the wakeup at the schedule
  call's start, before the vendor confirms the instant. The row then draws
  without a countdown rather than one toward a guessed time.
- **Consequences:** the chip's count and the panel's rows now come from one
  daemon helper, so the two cannot disagree again.
