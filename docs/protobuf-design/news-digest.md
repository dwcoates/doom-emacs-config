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
