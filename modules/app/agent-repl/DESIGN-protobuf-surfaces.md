
## `conversation.v1` is frontend-ready by design; only bookkeeping is synthesized

**Decided.** A conversation record reaches a frontend AS ITSELF. The daemon
neither mints one nor re-encodes one. The only thing it adds is bookkeeping —
facts it worked out that no producer observed.

**The test, which subsumes the earlier `frontend.v1` membership test.** Did a
PRODUCER OBSERVE it, or did the DAEMON WORK IT OUT? Observed goes in
`conversation.v1` and travels unchanged; worked out goes in `frontend.v1` and
rides alongside. The producers are `claude-shim` (stream plane) and
`shim-claude-sidecar` (file plane); `claude-repld` produces nothing.

**Verified state at the time of the decision.**

- The daemon does NOT synthesize conversation records. It constructs
  `conversation.v1` messages at exactly three sites, all in
  `claude-repld.internal.tokenusage.fromCounters()`, and both callers convert
  the daemon's OWN durable records — `state.v1.VendorTokenUsage` and
  `state.v1.TokenUsageTotals` — into the canonical display shape. That is the
  daemon reading its own bookkeeping.
- The daemon DOES re-encode them. The webapp imports `conversation.v1` in
  exactly two files (`webapp/src/tokens.ts`, `webapp/src/agent-emission.ts`);
  everything else arrives as `frontend.v1` re-encodings.
  `frontend.v1.Message`'s payload oneof has no arm that can carry a
  `conversation.v1.MessageEntry`, so the re-encoding is currently FORCED by the
  schema rather than chosen. That is gap 11 stated at the level that matters:
  not "the feed lacks an arm" but "a translation layer exists that should not".

**What this settles that was open.** `RESPELLINGS.md` §2 was blocked on whether
`frontend.v1.Message` should carry a `conversation.v1.MessageEntry` arm ALONGSIDE
`frontend.v1.AgentEmission`, leaving two routes for agent output. It should not:
`frontend.v1.Message` is a conversation record PLUS the daemon's stamps, one
route. `frontend.v1.AgentResponse` stays legitimate under this rule, because
`frontend.v1.ResponseUsageStamp` is bookkeeping riding beside
`conversation.v1.AgentContent` rather than a restatement of it.

**The worked example, because it shows the halves separating cleanly.** A
background shell's EXIT CODE is observed — a producer watched the process exit
137 — so it belongs in `conversation.v1`. The OUTCOME resolved from that code is
the daemon's, because a killed process also exits nonzero and reading the code
as "it failed" would report a user's own interrupt back to them as an error. The
code rides in `conversation.v1`, the verdict in `frontend.v1`, and neither
restates the other.

**A correction this forces.** `GAPS-wave-two.md` §D records
`conversation.v1.DetachedWorkKind` and `conversation.v1.DetachedWorkEnded` as a
"strictly richer" replacement for `protocol.v1.TaskKind`/`TerminalStatus`. They
are not: the exit status was dropped, and `claude-repld` still reaches for
`protocolv1.TerminalStatus` at the settle site. `frontend.v1.DetachedWorkShellExit`
should not exist; the exit status belongs on `conversation.v1.DetachedWorkEnded`.
