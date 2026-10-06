// mock_scenarios_test.go — THE SCENARIO TABLE, driven through the real mocked
// vendor and the real sidecar.
//
// Every row is one of the mocked vendor's `!name` scenarios, exactly as
// `agent-shim/claude/shim/AGENTS.md`'s "Mocked vendor: prompt → scenario table"
// declares it. The table is that document's table, restated as data: the
// expectations are read off its "What it writes on disk" and "conversation.v1
// arms exercised" columns and nothing is invented here.
//
// EACH SUBTEST IS ONE EDGE: one scenario, generated fresh by the mock, ingested
// by the real sidecar into the real store, and asserted. Every subtest asserts
// the four invariants that bind ALL of them (no unparsed residue, no unknown
// residue, only documented withholding classes, and books that name real
// file-plane identities) plus whatever its own row declares.
package integration

import "testing"

// mockScenario is one row of the mocked vendor's scenario table.
type mockScenario struct {
	// Prompt is the `!name` prompt that selects the scenario, verbatim.
	Prompt string
	// Wait says what ends the drive. Most scenarios reach a turn terminal; the
	// ones that deliberately never do are marked waitEntries, and the ones that
	// block on the user are marked waitAnswer so the drive decides their gate.
	Wait mockWait

	// Subagent — the scenario writes at least one `agent-<id>.jsonl` plus its
	// `.meta.json`, so the subagent's own book must exist, keyed by the meta's
	// toolUseId, with the main agent as the top level.
	Subagent bool
	// BashRun — the scenario writes a `b*.output` spool, so the run must be
	// readable through the real store's WatchBashRun.
	BashRun bool
	// ExitCode — the spool is TERMINATED by `EXIT=<code>`, so the terminal row
	// must carry an `exited` termination. A live-forever spool sets BashRun
	// without this.
	ExitCode bool
	// SpilledOutput — the scenario's result declares a `persistedOutputSize`, so
	// the settled shell's text output must state the PARTIAL extent and its
	// omitted byte count. THE FILE PLANE OWES THIS AS MUCH AS THE STREAM PLANE
	// DOES: both write the same unit under one upsert key, so a row here
	// claiming `whole` erases the truncation the other plane already drew.
	SpilledOutput bool
	// MovedShell — the scenario's `Bash` result names a `backgroundTaskId`, so
	// the command MOVED rather than ended and NO settled shell frame may reach
	// the agent's book from this plane. The detached-work frames naming the
	// unit are what say where it went, and a terminal here would replace the
	// stream plane's live card with a finished one carrying no output.
	MovedShell bool
	// ContextCut — the scenario cuts the conversation (a clear, a compaction),
	// so exactly one `AgentUpdate.context_cut` page line must land.
	ContextCut bool
	// APIError — the scenario writes a `system:api_error` record, so an
	// `AgentUpdate.api_error` page line must land as MID-TURN evidence.
	APIError bool
	// KeepAlive — the prompt carries the keep-alive marker, so no record of the
	// turn is stored and NO page line appears.
	KeepAlive bool
	// HeldTurn — the scenario's turn is STOPPED rather than finished, and the
	// stop is reached with no permission question in the way. The TERMINAL
	// itself (`AgentSuccess.interrupted.by_user`) is the STREAM plane's to
	// produce and the sidecar never mints one, so what the file plane owes is
	// the other half of the row's column: the turn's content reached the main
	// agent's book, and nothing on the way to the stop was a question.
	HeldTurn bool

	// BlockedExpectation names a CONCERN that stops this row's OWN expectation
	// from being asserted while the universal invariants still run. Only the
	// declared expectation is skipped, with the reason quoted into the skip so
	// it is visible in the run rather than buried in a report.
	BlockedExpectation string
	// Blocked names a CONCERN that stops the row ENTIRELY — one whose defect
	// shows up in the universal invariants themselves, so there is nothing left
	// to assert honestly. The fixture is still GENERATED and INGESTED, so the
	// evidence is produced on every run and the day the concern is resolved the
	// row starts asserting by deleting one field.
	Blocked string
}

// TestMockScenarios drives every scenario the mocked vendor declares.
func TestMockScenarios(t *testing.T) {
	t.Parallel()
	for _, tc := range mockScenarios {
		t.Run(tc.Prompt, func(t *testing.T) {
			// EACH ROW IS ITS OWN FOUR PROCESSES OVER ITS OWN t.TempDir()
			// trees and its own randomly-named sockets, so no row can observe
			// another's records and the table is safe to run concurrently.
			// generateMock's own drive slot is what keeps "concurrently" from
			// meaning "all 133 at once".
			t.Parallel()

			tree := generateMock(t, tc.Prompt, tc.Wait)
			in := ingestMock(t, tree)
			entries := in.Entries()
			// THE ORPHAN INVARIANT RUNS FIRST, AHEAD OF THE BLOCKED SKIP.
			// An orphan tool_result is a settle the converter failed to
			// perform (see mock_orphans_test.go for the subject's own
			// statement of it), and a BLOCKED row is still a row whose
			// fixture was generated and ingested — so it is still evidence
			// about the join, and the concern that blocks its own
			// expectation does not license a missed settle. It is asserted
			// here, on THIS scenario's already-generated tree, rather than
			// by a second function regenerating all 133 scenarios to run
			// one assertion.
			requireNoOrphanToolResults(t, tc.Prompt, entries)
			if tc.Blocked != "" {
				// Generated and ingested regardless: the fixture is the evidence.
				t.Skipf("this row is BLOCKED: %s", tc.Blocked)
			}

			// The invariants that bind every scenario.
			requireNoUnparsedResidue(t, tc.Prompt, entries)
			requireNoUnknownResidue(t, tc.Prompt, entries)
			requireDocumentedVendorSpecificKinds(t, tc.Prompt, entries)
			requireBooksAreKnownAgents(t, tc.Prompt, tree, entries)
			requireBlockUnitsAreAddressedByBlock(t, tc.Prompt, entries)
			requireUsageOnTheFirstBlockOnly(t, tc.Prompt, entries)
			requireProducer(t, in.Proxy.Batches())
			for _, e := range entries {
				requireFilePlane(t, e)
			}

			if tc.BlockedExpectation != "" {
				t.Skipf("this row's own expectation is BLOCKED: %s", tc.BlockedExpectation)
			}
			if tc.Subagent {
				requireSubagentBook(t, tc.Prompt, tree, entries)
			}
			if tc.BashRun {
				requireBashRunReadableThroughTheStore(t, tc.Prompt, in)
			}
			if tc.ExitCode {
				requireExitCodeInTheTerminal(t, tc.Prompt, in)
			}
			if tc.SpilledOutput {
				requireBashPartialExtent(t, tc.Prompt, entries)
			}
			if tc.MovedShell {
				requireNoSettledShellForMovedWork(t, tc.Prompt, entries)
			}
			if tc.ContextCut {
				requireContextCutPageLine(t, tc.Prompt, entries)
			}
			if tc.APIError {
				requireAPIErrorPageLine(t, tc.Prompt, entries)
			}
			if tc.KeepAlive {
				requireKeepAliveStoresNothing(t, tc.Prompt, entries)
			}
			if tc.HeldTurn {
				requireHeldTurnLandsWithoutAQuestion(t, tc.Prompt, tree, entries)
			}
		})
	}
}

// mockScenarios is the table. It is SEEDED here and completed below; see the
// file header for where each expectation comes from.
var mockScenarios = []mockScenario{
	{Prompt: "!md", Wait: waitTerminal},
	{Prompt: "!read", Wait: waitTerminal},
	{Prompt: "!read-head", Wait: waitTerminal},
	{Prompt: "!read-range", Wait: waitTerminal},
	{Prompt: "!read-truncated", Wait: waitTerminal},
	{Prompt: "!read-image", Wait: waitTerminal},
	{Prompt: "!write-create", Wait: waitTerminal},
	{Prompt: "!write-update", Wait: waitTerminal},
	{Prompt: "!edit", Wait: waitTerminal},
	{Prompt: "!ide-diagnostics", Wait: waitTerminal},
	{Prompt: "!grep-content", Wait: waitTerminal},
	{Prompt: "!grep-files", Wait: waitTerminal},
	{Prompt: "!grep-count", Wait: waitTerminal},
	{Prompt: "!glob", Wait: waitTerminal},
	{Prompt: "!bash", Wait: waitTerminal},
	{Prompt: "!bash-fail", Wait: waitTerminal},
	{Prompt: "!bash-timeout", Wait: waitTerminal, MovedShell: true},
	{Prompt: "!bash-spill", Wait: waitTerminal, SpilledOutput: true},
	{Prompt: "!bash-image", Wait: waitTerminal},
	{Prompt: "!bash-detach", Wait: waitTerminal, BashRun: true, ExitCode: true, MovedShell: true},
	{Prompt: "!bash-detach-fail", Wait: waitTerminal, BashRun: true, ExitCode: true},
	{Prompt: "!bash-detach-live", Wait: waitTerminal, BashRun: true},
	{Prompt: "!vendor-backgrounded", Wait: waitDetach},
	{Prompt: "!web-fetch", Wait: waitTerminal},
	{Prompt: "!web-fetch-redirect", Wait: waitTerminal},
	{Prompt: "!web-search", Wait: waitTerminal},
	{Prompt: "!skill", Wait: waitTerminal},
	{Prompt: "!skill-fail", Wait: waitTerminal},
	{Prompt: "!memory", Wait: waitTerminal},
	{Prompt: "!skills-injected", Wait: waitTerminal},
	{Prompt: "!task-create", Wait: waitTerminal},
	{Prompt: "!task-change", Wait: waitTerminal},
	{Prompt: "!task-reject", Wait: waitTerminal},
	{Prompt: "!send-message", Wait: waitTerminal},
	{Prompt: "!send-message-resumed", Wait: waitTerminal},
	{Prompt: "!send-message-refused", Wait: waitTerminal},
	{Prompt: "!subagent", Wait: waitTerminal, Subagent: true},
	{Prompt: "!subagent-detached", Wait: waitTerminal, Subagent: true},
	{
		// Subagent is NOT asserted here: this scenario's subagent transcript
		// holds a single user record ("Fail."), and R15 withholds a file-plane
		// user prompt as vendor_specific, so its book legitimately serves
		// nothing. The universal invariants still cover the whole tree.
		Prompt: "!subagent-failed", Wait: waitTerminal,
	},
	{Prompt: "!cancel-all", Wait: waitEntries, Subagent: true},
	{Prompt: "!plan", Wait: waitTerminal},
	{Prompt: "!findings", Wait: waitTerminal},
	{Prompt: "!worktree-keep", Wait: waitTerminal},
	{Prompt: "!worktree-remove", Wait: waitTerminal},
	{Prompt: "!cron", Wait: waitTerminal},
	{Prompt: "!push-sent", Wait: waitTerminal},
	{Prompt: "!push-config-off", Wait: waitTerminal},
	{Prompt: "!push-user-present", Wait: waitTerminal},
	{Prompt: "!push-no-transport", Wait: waitTerminal},
	{Prompt: "!monitor-deadline", Wait: waitTerminal},
	{Prompt: "!monitor-persistent", Wait: waitTerminal},
	{Prompt: "!wakeup-schedule", Wait: waitTerminal},
	{Prompt: "!wakeup-stop", Wait: waitTerminal},
	{Prompt: "!artifact-publish", Wait: waitTerminal},
	{Prompt: "!artifact-list", Wait: waitTerminal},
	{Prompt: "!unmodeled", Wait: waitTerminal},
	{Prompt: "!hook-success", Wait: waitTerminal},
	{Prompt: "!hook-blocked", Wait: waitTerminal},
	{Prompt: "!hook-failed", Wait: waitTerminal},
	{Prompt: "!hook-cancelled", Wait: waitTerminal},
	{Prompt: "!perm-allow-once", Wait: waitAnswer},
	{Prompt: "!perm-allow-standing", Wait: waitAnswer},
	{Prompt: "!perm-deny-user", Wait: waitAnswer},
	{Prompt: "!perm-deny-policy", Wait: waitAnswer},
	{Prompt: "!perm-undecidable", Wait: waitAnswer},
	{Prompt: "!ask-single", Wait: waitAnswer},
	{Prompt: "!ask-multi", Wait: waitAnswer},
	{Prompt: "!ask-free", Wait: waitAnswer},
	{Prompt: "!ask-unanswered", Wait: waitEntries},
	{Prompt: "!rotate", Wait: waitTerminal, ContextCut: true},
	{Prompt: "!slash", Wait: waitTerminal},
	{Prompt: "!context-usage-drift", Wait: waitTerminal},
	{Prompt: "!model-fallback", Wait: waitTerminal},
	{Prompt: "!fast-on", Wait: waitTerminal},
	{Prompt: "!fast-off", Wait: waitTerminal},
	{Prompt: "!fast-cooldown", Wait: waitTerminal},
	{Prompt: "!mcp-all", Wait: waitTerminal},
	{Prompt: "!mcp-healthy", Wait: waitTerminal},
	{Prompt: "!usage-available", Wait: waitTerminal},
	{Prompt: "!usage-full", Wait: waitTerminal},
	{Prompt: "!usage-opus-absent", Wait: waitTerminal},
	{Prompt: "!usage-service-unavailable", Wait: waitTerminal},
	{Prompt: "!usage-window-unavailable", Wait: waitTerminal},
	{Prompt: "!usage-utilization-unavailable", Wait: waitTerminal},
	{Prompt: "!usage-sampling-failure", Wait: waitTerminal},
	{Prompt: "!rate-limit", Wait: waitTerminal},
	{Prompt: "!compact", Wait: waitTerminal, ContextCut: true},
	{Prompt: "!compact-auto", Wait: waitTerminal, ContextCut: true},
	{Prompt: "!compact-failed", Wait: waitTerminal},
	{Prompt: "!away-summary", Wait: waitTerminal},
	{Prompt: "!residue", Wait: waitTerminal},
	{Prompt: "!cold-seed", Wait: waitTerminal},
	{Prompt: "!fail-execution", Wait: waitTerminal},
	{Prompt: "!fail-max-turns", Wait: waitTerminal},
	{Prompt: "!fail-budget", Wait: waitTerminal},
	{Prompt: "!fail-structured-output", Wait: waitTerminal},
	{Prompt: "!fail-blocking-limit", Wait: waitTerminal},
	{Prompt: "!fail-rapid-refill", Wait: waitTerminal},
	{Prompt: "!fail-prompt-too-long", Wait: waitTerminal},
	{Prompt: "!fail-image", Wait: waitTerminal},
	{Prompt: "!fail-model", Wait: waitTerminal},
	{Prompt: "!fail-malformed-tool-use", Wait: waitTerminal},
	{Prompt: "!fail-tool-deferred", Wait: waitTerminal},
	{Prompt: "!fail-tool-deferred-unavailable", Wait: waitTerminal},
	{Prompt: "!fail-turn-setup", Wait: waitTerminal},
	{Prompt: "!fail-aborted-tools", Wait: waitTerminal},
	{Prompt: "!fail-stop-hook", Wait: waitTerminal},
	{Prompt: "!fail-hook-stopped", Wait: waitTerminal},
	{Prompt: "!fail-continuation-prevented", Wait: waitTerminal},
	{Prompt: "!api-429", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-529", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-401", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-403", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-400", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-413", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-404", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-500", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-billing", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-oauth-org", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-max-output", Wait: waitTerminal, APIError: true},
	{Prompt: "!api-unmodeled", Wait: waitTerminal, APIError: true},
	{Prompt: "!max-tokens", Wait: waitTerminal},
	{Prompt: "!refusal-fallback", Wait: waitTerminal},
	{Prompt: "!refusal-no-fallback", Wait: waitTerminal},
	{Prompt: "!context-window", Wait: waitTerminal},
	{Prompt: "!fault-converter", Wait: waitTerminal},
	{Prompt: "!fault-recover", Wait: waitTerminal},
	{Prompt: "!hold", Wait: waitEntries, HeldTurn: true},
	{Prompt: "!interrupt", Wait: waitEntries},
	{Prompt: "!query-eof", Wait: waitEntries},
	{Prompt: "!query-fail", Wait: waitEntries},
	{
		// KeepAlive is NOT set here, and cannot be. The `!keepalive` scenario is
		// an ORDINARY short turn: the marker is the SHIM's, the mocked vendor
		// neither adds nor removes it, and the scenario selector needs the
		// `!name` at position 0 — so no `!scenario` prompt can carry the marker.
		// A row asserting the keep-alive expectation here would be asserting it
		// against a turn that is not keep-alive, and it fails exactly that way.
		// The edge is covered instead by TestMockKeepAliveTurnsStoreNothing,
		// which drives the marker on an ordinary prose prompt — the production
		// shape.
		Prompt: "!keepalive", Wait: waitTerminal,
	},
}

// TestMockKeepAliveTurnsStoreNothing covers the keep-alive edge, which no
// `!scenario` can: the marker is the SHIM's and the mocked vendor neither adds
// nor removes it, while the scenario selector needs the `!name` at position 0.
// So the marker rides an ORDINARY prose prompt — which is exactly the shape the
// sidecar sees in production — and not one of the turn's records may be
// stored. The mocked vendor links its records exactly as the real one does,
// which is what the keep-alive rule reads.
func TestMockKeepAliveTurnsStoreNothing(t *testing.T) {
	t.Parallel()
	tree := generateMock(t, keepaliveMarker+" say something short", waitTerminal)
	in := ingestMock(t, tree)
	entries := in.Entries()

	requireNoUnparsedResidue(t, keepaliveMarker, entries)
	requireNoUnknownResidue(t, keepaliveMarker, entries)
	requireKeepAliveStoresNothing(t, keepaliveMarker, entries)
}
