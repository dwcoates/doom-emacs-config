# The cold gate's summary leaves StartSession (shim.v1.SummarizeConversation)

Answering a cold gate with compaction failed for a large conversation: the shim
summarized the conversation inside the `StartSession` call, so the daemon's
60s `StartSession` bound had to cover a summarization whose time grows with
the conversation. On 2026-11-13 the owner answered 20 cold gates at once;
`disk-usage-audit-prune` (596,848 context tokens) was still summarizing when
the bound fired, and the gate re-armed as `start_failed`. The requirement was
to make that failure structurally impossible rather than raise the number:
the summary is its own operation with its own bound.

## Assumptions (autonomous design)

- Objective: no `StartSession` ever runs a summarizer, so its bound covers only
  the vendor opening; the summarizer gets a bound sized from its own measured
  time.
- Systems: `proto` (shim.v1), the shim (`agent-shim/claude/shim`), the daemon
  (`daemon/internal/workspace`, `daemon/internal/shimclient`) and the
  integration fakeshim. No frontend component: the compaction's phases already
  ride the session stream unchanged.
- Non-additive: `StartSessionResume.carry_summary`,
  `StartSessionSuccess.carried_summary` and
  `StartSessionVendorStartFailed.compaction` are retired, and
  `StartSessionCarrySummary`, `StartSessionCarriedSummary` and
  `StartSessionCompactionFailed` deleted.
- Iteration sequence: the default, with one rpc added to an existing service,
  so the one increment is the new endpoint plus the retirements in
  `StartSession`.

## Landed changes

### 1. `SummarizeConversation`, and `StartSession` stops summarizing

WHAT. A new rpc `shim.v1.Shim.SummarizeConversation` in the Session section,
declared in `endpoint_summarize_conversation.proto`. Its request names the
workspace and profile (as `StartSession` does, because the summarizer is a
query like any other), the summarizer (`conversation.v1.SessionColdCompact`,
imported whole) and a `use` oneof: `compact` (a conversation the next
`StartSession` resumes, with its rolled-back turns) or `carry` (another
conversation, summarized on an ephemeral fork). The response is the standing
success/failure oneof; success's arm mirrors the use (`compacted` with the
token figures, `carried` with the text); failure's arms are
`summarizer_failed` (reusing `StartSessionVendorStartFailed`, so the retry
label, network-versus-vendor cause and API refusal mean exactly what they mean
for a start), `unknown_session` and `already_started`.

`StartSessionResume.cold_remediation.compact` now names a compaction ALREADY
MADE by the preceding `SummarizeConversation` on the same shim. The carried
summary moved wholly to `SummarizeConversation.carry`, so `carry_summary` (tag
6), `carried_summary` (tag 2) and the `compaction` failure marker (tag 3) are
reserved.

WHY. The two operations have different time profiles. A start's is a vendor
opening (hundreds of milliseconds healthy). A summarizer's is tens of seconds
and grows loosely with the conversation (measurement below). One call with one
bound made the start bound a guess about the largest conversation anyone would
compact. Two calls make the start bound's coverage structural: it can no
longer contain a summarizer.

WHY NOT the alternatives.
- Raising `DefaultStartSessionBound`: rejected by the requirement, and it would
  make every hung start wait out the summarizer's bound.
- Pausing the start bound while the session stream reports `summarizing`: the
  phase frames ride WatchSession, a different stream from the StartSession
  call, with no ordering between them, so a late frame could still let the
  bound fire. That is improbable, not impossible.
- `StartSession` answering before the compaction: the resume must read the
  compacted transcript, so the summary has to be written before the vendor
  opens.

CONSEQUENCES.
- The daemon summarizes before every `StartSession` that carries a `compact`
  remediation or an owed summary
  (`daemon/internal/workspace/owedstart.go`, `sessions.go` `askToStartSession`),
  under its own bound `DefaultSummarizeBound`, and labels a failure exactly as
  before (`compactionFailure` still reads the label's `compaction` marker),
  so `compaction_failed` answers and the vendor-start retry loop are unchanged.
  Every retry of a start retries its summary too, which matches the old
  behavior (the old `StartSession` re-ran `compact` on every attempt).
- The summarizer still runs inside the vendor-start slot the start holds
  (`vendorstart.go`), so the concurrency cap covers summarizers as before.
- A pay answer whose start carries an owed summary can now be taken by a
  parked conversation's bridge, since the summary is the daemon's to deliver
  and the bridge's start writes nothing of it. `promoteParked` no longer needs
  its `carrySummary === undefined` exclusion.
- The shim fails loudly (a thrown error, an rpc `internal`) on a `compact`
  start no compaction of that conversation on that shim preceded: the
  daemon always summarizes first, so the violation is a daemon defect.

MEASUREMENT (the bound's basis). `bin/logs.sh --all --since 2000h`, every
`shim.engine.session` record with phase `SUMMARIZING` paired with the same
vendor session's `SUMMARIZED`, read 2026-11-13: 55 successful summarizations
on Haiku between 2026-10-27 and 2026-11-13, 4.7s to 95.0s. The time is only
loosely tied to size (11k tokens took 20.4s, 663k took 56.8s, 872k took
36.2s). The healthy worst case is 95.0s (593,525 tokens, 2026-10-27); the
failing 596,848-token run exceeded 60s. `DefaultSummarizeBound` is 240s, about
2.5x that worst case.

### Summarizer model logging

The `SUMMARIZING` record logged `summarizer_model=""` because it is written
before the summarizer answers, and the field reads the answered spend. The
gate's `-model haiku` did reach the summarizer (every `SUMMARIZED` record of
2026-11-13 reads `claude-haiku-5-5`). The phase records now also log
`requested_model`, the model the request named, so the `SUMMARIZING` record
states it.
