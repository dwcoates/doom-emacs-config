# Owner-pending items

UPDATE 2026-09-04: item 1 done by the owner (dirs deleted, verified absent); item 2 ruled "remediate yourself" (agent dispatched); lost-arm cause ruled "carry it" (proto landing 11); items 3-6 accepted as decided. — 2026-09-05

Items that need a decision or an action from the owner. Everything else is under
the project lead's delegation and is tracked in RESUME-2026-09-03.md.

## Needs your explicit yes (writes outside the project)

1. Delete seven leftover capture directories in the real `~/.claude/projects/`
   (left by the capture harness's reclaim running before the vendor child had
   exited; the harness bug is fixed in 7b530ffcc so no new ones will appear):
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-auto-compaction-e7PBGB-cwd
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-auto-compaction-PS2TGH-cwd
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-auto-compaction-Uzawus-cwd
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-context-budget-warning-GNFp1s-cwd
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-context-budget-warning-qfobth-cwd
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-context-budget-warning-SjFpvk-cwd
   - -private-var-folders--m-7ff9yqwn6y91dgtws1zpsb680000gn-T-agent-repl-capture-conversation-history-fJyhbV-cwd

## Needs a contract judgment

2. Shim integration test "WatchSession's head arrives before any body byte over
   HTTP/1.1" (agent-shim/claude/shim/test/integration/transport.test.ts) flakes
   ~1 in 6 under load: the head and the first frame legitimately coalesce into one
   TCP read, so the ordering claim is unobservable over h1 (the h2c sibling test
   carries it). Proposed: reshape the h1 claim to "acceptance is decidable from
   the head alone" rather than keep asserting an unobservable order.

## Decided by the lead, listed for visibility (say so if you disagree)

3. `SubmitPrompt.sent_at_ms` proto field DECLINED for the perf spec (rows 1a/1b
   measure the two halves with existing clocks).
4. Three declared correlation fields (`request_id`, `agent_repl_session_id`,
   `claude_session_id`) are populated by nothing; decision after perf phase 1
   reports whether any perf row needs them (populate or retire).
5. `!context-budget-warning` stays UNGROUNDED after three real-vendor levers; no
   fourth attempt without a genuinely new idea.
6. Sandbox concurrency default = 1 container (Docker VM 5.8 GiB / 4 CPUs); raise
   Docker Desktop's allocation if you want more.

## Coming to you when ready (no action yet)

7. Before/after unicode duration table for all suites (needs a quiet box after
   the last agents drain).
8. Perf phase 1 measured results: which UI-latency rows meet their budgets and
   which are production findings.
9. Emacs layer E3 result after the Emacs speed pass lands.
