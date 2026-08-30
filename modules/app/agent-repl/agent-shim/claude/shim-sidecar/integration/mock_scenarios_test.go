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
	// ones that deliberately never do are marked waitEntries.
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
	// ContextCut — the scenario cuts the conversation (a clear, a compaction),
	// so exactly one `AgentUpdate.context_cut` page line must land.
	ContextCut bool
	// APIError — the scenario writes a `system:api_error` record, so an
	// `AgentUpdate.api_error` page line must land as MID-TURN evidence.
	APIError bool
	// BudgetWarning — the scenario writes the vendor's context-budget warning,
	// so an `AgentUpdate.context_budget_warning` carrying its text must land.
	BudgetWarning bool
	// KeepAlive — the prompt carries the keep-alive marker, so every record of
	// the turn lands on `unserved_item.keepalive` and NO page line appears.
	KeepAlive bool

	// Blocked names a CONCERN that stops this row's own expectation from being
	// asserted. The universal invariants still run; only the declared
	// expectation is skipped, with the reason quoted into the skip so it is
	// visible in the run rather than buried in a report.
	Blocked string
}

// TestMockScenarios drives every scenario the mocked vendor declares.
func TestMockScenarios(t *testing.T) {
	for _, tc := range mockScenarios {
		t.Run(tc.Prompt, func(t *testing.T) {
			tree := generateMock(t, tc.Prompt, tc.Wait)
			in := ingestMock(t, tree)
			entries := in.Entries()

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

			if tc.Blocked != "" {
				t.Skipf("this row's own expectation is BLOCKED: %s", tc.Blocked)
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
			if tc.ContextCut {
				requireContextCutPageLine(t, tc.Prompt, entries)
			}
			if tc.APIError {
				requireAPIErrorPageLine(t, tc.Prompt, entries)
			}
			if tc.BudgetWarning {
				requireContextBudgetWarning(t, tc.Prompt, entries)
			}
			if tc.KeepAlive {
				requireKeepAliveNeverReachesAPage(t, tc.Prompt, entries)
			}
		})
	}
}

// mockScenarios is the table. It is SEEDED here and completed below; see the
// file header for where each expectation comes from.
var mockScenarios = []mockScenario{
	{Prompt: "!md", Wait: waitTerminal},
	{Prompt: "!read", Wait: waitTerminal},
	{Prompt: "!subagent", Wait: waitTerminal, Subagent: true},
	{Prompt: "!bash-detach", Wait: waitTerminal, BashRun: true, ExitCode: true},
	{Prompt: "!bash-detach-fail", Wait: waitTerminal, BashRun: true, ExitCode: true},
	{Prompt: "!compact", Wait: waitTerminal, ContextCut: true},
	{Prompt: "!compact-auto", Wait: waitTerminal, ContextCut: true},
	{Prompt: "!api-429", Wait: waitTerminal, APIError: true},
	{
		Prompt: "!context-budget", Wait: waitTerminal, BudgetWarning: true,
		Blocked: mockBlockedContextBudget,
	},
	{Prompt: "!residue", Wait: waitTerminal},
}

// mockBlockedContextBudget records the one cross-plane disagreement this suite
// found: the mocked vendor writes the budget warning as a `context_tip`
// attachment (the corpus's real capture, `attachments/context_tip.jsonl`),
// while the sidecar's converter recognizes `context_budget_warning` (the
// corpus's SYNTHETIC sample, which MANIFEST.md itself flags as composed from
// the proto and asks to have its spelling re-checked against a real capture).
// One of the two must move, and choosing which is not this suite's call.
const mockBlockedContextBudget = "the mocked vendor writes the budget warning as an `attachment/context_tip` " +
	"(corpus: attachments/context_tip.jsonl, a REAL capture) while internal/convert recognizes " +
	"`attachment/context_budget_warning` (corpus: attachments/context_budget_warning.jsonl, marked SYNTHETIC in " +
	"testdata/corpus/MANIFEST.md with an explicit 're-check the attachment type spelling' note). The record is " +
	"withheld as vendor_specific rather than lost, so nothing is dropped; which spelling wins is a lead-level call."
