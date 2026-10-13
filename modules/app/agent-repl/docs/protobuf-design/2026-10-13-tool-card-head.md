# Every tool card heads the same way

Every purple-titled card in the feed (a tool call such as Bash or Write, a
question, a permission, a skill, a hook, a subagent or detached-work bubble)
is a "tool card", and its head row must have one format, as an invariant:
the title, then the status badge, then the settled time, then any
card-specific chips. Only the words differ per card. Today the head is built
per card, and the question and permission cards put a ticking "N ago" stamp
at the bottom of the card instead of a settled time in the head. That ticking
stamp is also what knocked the feed off its tail once a second in WebKit.

## Core design principles

- **One head format for every tool card (owner, 2026-10-13).** Every tool
  card's head is built by one function, in Bash's arrangement: title, badge,
  settled time, chips. The vocabulary of the title and the badge is bespoke
  per card; the arrangement, fonts and spacing are not.
  - Consequence for the contract: every settled tool card carries the same
    settled-time element, `frontend.v1.FeedToolCallRuntime`, composed by the
    daemon.
  - Reopens: the question and permission cards' `at_ms` stamps (they become
    runtimes), and the permission card's client-side `waiting` clock (dropped).
  - Does not claim: anything about responses, prompts, the cold-context gate
    or any card without a purple title; those are untouched.
- **The head reads title, badge, time -- and nothing else (owner,
  2026-10-13).** No chips in a tool card's head: Bash's exit code and a
  hook's exit code are bespoke to their cards and sit in the card's content.
  This order matches the 2026-10-11 ruling for bubble heads; Bash's old
  title, time, chips, badge order is reversed.
- **The time starts when the card is created and freezes when it resolves
  (owner, 2026-10-13):** a running call, an open question or permission and a
  loading skill count up live; a settled one shows the span it took.
- **Bubbles are out of scope (owner, 2026-10-13):** subagent, detached-work,
  shell and merge bubble heads are their own custom category and are left as
  they are.

## Landed changes

### Settle instants on the ask frames (conversation.v1)

- WHAT: `AgentPermissionSuccess` and `AgentQuestionSuccess` each gain an
  optional `AgentActivitySettledAt settled_at`, with the instant the ask was
  posed restated inside it. `AgentPermissionSuccess` reserves tag 3, which a
  comment already called retired, so the new field takes tag 4.
- WHY: the head's settled time must be measured between two instants the
  producer observed. The daemon used to stamp `at_ms` with its own clock when
  it resolved the frame, which on a replay after a restart is the replay's
  time, not the ask's.
- Consequences:
  - The shim stamps the settle in `PermissionGate.settleQuestion` and the
    permission decision path, restating the start it already records
    (`PendingQuestion.startedAtMs`, `PendingPermission.startedAtMs`).
  - A policy denial (`convert/permission.ts` `policyDenial`) never posed an ask
    and carries no clock, so it carries no `settled_at`, and its card states
    no time. Accepted.
  - Ask failures carry their instant inside `AgentToolFailure.settled_at`
    already, so the abandoned/expired-by-failure cards read it from there.
  - Rows stored before this change have no `settled_at`, so their cards show
    no time. Accepted, the same as a Bash call whose start is missing.

### The settled time on every tool card (frontend.v1)

- WHAT: `FeedPermissionAnswered.at_ms`, `FeedPermissionAbandoned.at_ms`,
  `FeedQuestionAnswered.at_ms` and `FeedQuestionExpired.at_ms` are removed
  (tags and names reserved) and replaced by `optional FeedToolCallRuntime
  runtime`. `FeedSkillLoaded`, `FeedSkillFailed` and `FeedHookFailed` gain the
  same optional runtime.
- WHY: the owner's principle above. One element states the settled time of
  every tool card, so the head builder draws it the same way for each.
- Consequences:
  - Asks are worded "waited 2m 14s"; tool-like units (skill, hook) "ran 4.2 s".
    The words are the daemon's, composed by one formatter.
  - A failed hook's runtime is the hook's own `duration_ms`.
  - `FeedHookBlocked` gets no runtime: `AgentHookBlockingError` carries no
    instant and no duration, so the field would never be set. Its head shows
    no time.
  - The webapp's `stampedAge` helper and the permission card's `waiting` clock
    have no remaining caller and are deleted.

### The head's clock replaces the settled-time text (frontend.v1)

- WHAT: the `optional FeedToolCallRuntime runtime` fields added above are
  withdrawn before they shipped, `FeedToolCallReturned.runtime` (tag 9) is
  retired, and `FeedToolCallRuntime` is deleted. In their place one element,
  `FeedToolCardClock` (a start instant plus a running/stopped arm, the stopped
  arm carrying the end instant), sits on every tool card view as `optional
  clock`: `FeedSimpleToolCall`, `FeedSkill`, `FeedHook`, `FeedPermission`,
  `FeedQuestion`.
- WHY: the owner ruled that the time ticks from the card's creation and
  freezes when it resolves. A daemon-composed sentence ("ran 4.2 s") cannot
  tick, and a live client count beside a frozen daemon sentence would change
  format at the settle. Instants on the wire with the client ticking is the
  shape bubble heads already use (`FeedSubagentRuntime`, `elapsed-clock.ts`),
  so every clock on the page reads the same way.
- Correction kept visible: the earlier entry composed settled sentences
  daemon-side ("waited 2m 14s", "ran 4.2 s"). Root cause: it was designed from
  the old ruling "show the time as Bash does" before the owner said the time
  must tick live; a live clock forces instants, not text.
- Consequences:
  - Bash's settled time changes wording from "ran 4.2 s" to the shared
    elapsed format ("4s", "2m 14s"), the same as a subagent's head. Accepted
    as the price of one clock format.
  - The clock is drawn at a reserved width, so a tick never reflows the head.
  - A unit whose start the producer never stated has no clock; a settled unit
    whose settle instant is missing has no clock either, since it can neither
    tick nor show a span.
  - A blocked hook has no settle instant at the source, so it has no clock.
    A failed hook's end is its start plus the duration it reported.
  - The daemon's `formatRuntime` / `formatSettledClock` / `settledRuntime`
    have no remaining caller and are deleted.
