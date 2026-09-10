# Shim integration-suite audit #2 (fresh-context fable auditor over 3dc64a89b) — triaged by the shim lead

Input for remediation #4. The auditor's 61 items are ACCEPTED as written
unless a disposition below says otherwise; the full item text lives in the
auditor's report (session transcript) and is summarized here by number and
target file. Paths: I = agent-shim/claude/shim/test/integration.

## Lead dispositions (everything else: accept, implement as stated)

- 32/57 (standing set_mode accepted without offer): ACCEPT — this is audit-1
  B1, still unaddressed. Lever `!perm-allow-standing-mode` whose OFFER carries
  set_mode; altered standing → answer_mismatch; unoffered standing → refused.
- 58 (three turn-stop terminals declared-only but unmarked): ACCEPT — the
  captures turn-stop-error-during-execution (aborted_streaming →
  success.interrupted), turn-stop-hook-stop and
  turn-stop-max-structured-output-retries (success.completed) do NOT ground
  `!fail-execution`, `!fail-stop-hook`, `!fail-structured-output` failure
  terminals. Mark all three declared-only in AGENTS.md and list them in the
  MANIFEST evidence-gaps section; the mock keeps the declared terminals.
- 59 (Hibernate test asserts compact_boundary): ACCEPT — label the test as
  graded against the SYNTHETIC compaction fixture (the helper is synthetic by
  ruling); do not delete it.
- 56 (compacting without a following ContextCut): ACCEPT as "the shim never
  invents a cut": a `compacting` beat with no compaction record produces no
  ContextCut, and the scenarios.test.ts SESSION_ARMS row for
  compaction-directed reflects what the capture holds.
- 61 (redrain 20 ms level re-check under fs.watch): KEEP the mechanism — it
  is the ruled bounded re-drain for FSEvents lost appends (bucket 1), a
  level-then-edge guard, not sleep-based sequencing; FIX the docstrings that
  claim purely event-driven waits. The other 61 sub-items (race-as-ordering
  in session:276/gate:145, magic fan-out >= 3, harness.ts swallowing drive
  exceptions, duplicated keep-alive literal) are accepted.
- 47 (h1 bodyBeforeHeaders hard-coded): ACCEPT — audit-1 E8, make the h1 raw
  client observe the head/body boundary.
- 21 (unknown target accepts any throw): ACCEPT — pin Code.NotFound per the
  interim ruling (ledger, rebuild merge).
- 22 (ONE book): ACCEPT — assert the subagent's frames are absent from the
  main book and present on its own WatchAgent.
- 6/13 and every audit-1 carry-over (A1–A3, A5–A8, A11–A13, A15, A17,
  A20–A24, B1, B5–B7, B9, B10, B13–B15, C1–C9, C11, C12, E1, E8): ACCEPT;
  they were assigned in remediation #2 and did not land.

## Split for dispatch (two opus-low agents, disjoint test files)

- AGENT S (session/process/record/transport): items 4, 5, 6, 9, 11, 13, 14,
  15 (session/process arms), 23, 24, 25, 26, 27, 30, 31, 33, 34, 35, 38, 43
  (session part), 44, 45, 47, 59, 61 (session:276, harness docstrings).
- AGENT T (turn/gate/detached + goldens/conformance): items 1, 2, 3, 7, 8,
  10, 12, 15 (turn/gate arms), 16, 17, 18, 19, 20, 21, 22, 28, 29, 32/57,
  36, 37, 39, 40, 41, 42, 43 (turn/detached part), 46, 48, 49, 50, 51, 52,
  53, 54, 55, 56, 58, 60, 61 (gate:145, detached:801, fake/harness.ts,
  turn:909).
- Production fixes each test exposes belong to the agent whose test found
  them; a shared engine file touched by both is noted in both reports and
  the lead resolves the seam at merge.
