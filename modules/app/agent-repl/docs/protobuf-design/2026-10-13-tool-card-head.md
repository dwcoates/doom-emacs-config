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
- **A settled tool card's head shows its time as Bash's does (owner,
  2026-10-13):** a fixed figure, never a ticking one.

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
