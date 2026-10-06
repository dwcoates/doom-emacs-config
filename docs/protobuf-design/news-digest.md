# Daily Claude news digest

## Problem (owner, 2026-10-02)

A daily digest of Claude news and announcements — new features, and
especially announced deprecations and changes to the Claude Agent SDK that
agent-repl's backend runs on (e.g. removed support, or a move from
subscription to API-only billing) — made by the daemon on a reliable daily
cadence that survives restarts and crashes, condensed by a Sonnet model, and
drawn as a large, sleek overlay over the feed in every webview until it is
dismissed in any one of them (which closes it in all).

## Watched sources (verified reachable 2026-10-02)

Feeds (structured; new entries since the previous run):
- Agent SDK (TypeScript) releases — https://github.com/anthropics/claude-agent-sdk-typescript/releases.atom
- Agent SDK (Python) releases — https://github.com/anthropics/claude-agent-sdk-python/releases.atom
- Agent SDK npm versions — https://registry.npmjs.org/@anthropic-ai/claude-agent-sdk (the `time` map)
- Claude Code releases — https://github.com/anthropics/claude-code/releases.atom
- Claude Code changelog — https://raw.githubusercontent.com/anthropics/claude-code/main/CHANGELOG.md
- Anthropic status history — https://status.anthropic.com/history.rss

Pages (no feed; text compared against the previous run's snapshot, only the
changed text is condensed):
- Anthropic news — https://www.anthropic.com/news (no RSS: /rss.xml is 404)
- Claude platform release notes — https://docs.claude.com/en/release-notes/overview
- Claude Code changelog (docs) — https://docs.claude.com/en/docs/claude-code/changelog
- Model deprecations — https://docs.claude.com/en/docs/about-claude/model-deprecations
- Claude apps release notes — https://support.claude.com/en/articles/12138966-release-notes

## Landed contract

- `frontend.v1.NewsDigestOverlay` (news_digest.proto): id (typed echo
  token), header (title, period), sections by kind (backend / deprecation /
  policy / feature / release / incident — backend ranked first and drawn as
  a warning), items (title, summary, optional effective date, links), and
  the per-source read outcome.
- `agentrepl.v1.WatchDaemonResponse.news_digest = 9` →
  `NewsDigestStanding { shown | none }`, standing state replayed to late
  subscribers, webview streams only.
- `DismissNewsDigest(id)` → takes it down in every webview;
  `unknown_digest` for a stale/foreign id.
- `RefreshNewsDigest()` → run now; `shown{items}` / `nothing_new`, errors
  `already_running` / `model_failed` / `no_source_read`.

## Decisions (orchestrator, under "implement it")

- Cadence: one run every 24h measured from the previous run's END, persisted
  in the daemon's state store; on daemon start an overdue run happens once
  (never a burst of missed days). A RefreshNewsDigest run resets the cadence.
- A run that finds nothing new shows NO overlay (a daily "nothing new" panel
  would train the reader to dismiss it unread); the run is still recorded.
- The standing digest is persisted, so it survives daemon restarts until
  dismissed.
- Sonnet condenses; the daemon only fetches, diffs and prompts. The model is
  told to classify by the section kinds and to put anything touching the
  Agent SDK or Claude Code's supported use, billing or subscription terms in
  `backend`.

## Addendum: "Since last week" (owner, 2026-10-06)

Problem: a digest of the past seven days' digests holding ONLY what changed,
or was announced to change, that could regress agent-repl (API/SDK breaking
changes, deprecations, removals, changed defaults or behavior of the Agent
SDK, Claude Code CLI or Messages API; pricing, billing, plan, usage-limit or
rate-limit changes; auth, login or account changes; model retirements or
renames; policy or terms changes touching automated or headless use). Bug
fixes, performance work and new features that change no existing behavior
are excluded. Drawn first in the overlay, titled "Since last week".

Landed contract (`news_digest.proto`):

- `NewsDigestOverlay.week = 5` → `NewsDigestWeek { heading, oneof outcome {
  risks | quiet } }`.
- `NewsDigestWeekRisks { repeated NewsDigestRiskItem items }`, never empty;
  `NewsDigestRiskItem { NewsDigestItem item; NewsDigestRiskReason reason }`
  — the item is drawn as any section's item (its `effective` carries an
  announced future date), the reason is the one-line why, naming what in
  agent-repl it touches.
- `NewsDigestWeekQuiet { text }`: nothing regressive is TOLD, never left to
  silence. The daemon composes the text, and names the span it covers when
  its record of digests began inside the week.
- `week` is set on every digest the daemon makes; it is unset only on a
  digest made before the field existed and still standing across the deploy,
  which draws no weekly section.

Decisions:

- Classification at the source: the condensing call marks each item that
  could regress agent-repl with a one-line `risk` reason; the per-run
  sections on the wire are unchanged.
- Retention: every run's items are kept in wsm with their run's end and
  their mark; rows older than 14 days are pruned by each run that keeps
  items.
- Dedupe: the same announcement recurs across runs (a page re-read, a
  carried digest re-marked) under different titles and the same page URLs,
  so neither title nor link is a reliable identity. A second Sonnet call over
  only the week's marked items merges them; it answers groups of member ids,
  every marked item belongs to exactly one group (validated hard), an item
  with a stated date keeps one of its members' dates, and links are the
  union of the members' links, composed by the daemon. No call is made for
  zero or one marked item.

## Addendum: the SDK version in the header (owner, 2026-10-06)

- `NewsDigestHeader.sdk_version = 3` → `NewsDigestSdkVersion { oneof answer {
  known { version } | unknown {} } }`, drawn as "SDK Version: <version>" in the
  middle of the header row.
- Source: the `conversation.v1.SessionRuntime.sdk_version` the shim reports on
  every session start (its installed `@anthropic-ai/claude-agent-sdk`), the
  latest one the daemon's fleet saw. Nothing reports it to a daemon before a
  session starts (`SessionDiagnostics` carries only `shim_build`), so a daemon
  that has started no session since it came up answers `unknown`, never a
  guess.
