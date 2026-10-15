package main

// mockgoldens_test.go — THE MOCKED VENDOR'S SCENARIOS, read off their checked-in
// goldens and ingested by the sidecar in-process.
//
// Every row is one of the mocked vendor's `!name` scenarios, as the shim's
// AGENTS.md "Mocked vendor: prompt → scenario table" declares it. Its FILE
// plane (the transcripts, subagent transcripts and metas, and spools the mock
// wrote) is checked in at testdata/scenario-goldens/<scenario>/tree, generated
// and kept fresh by the shim's own suite (agent-shim/claude/shim/test/fake/
// goldens.ts). This suite never boots the shim: it materializes the tree into
// the harness's roots and runs the real cycle over it (agent-repl/AGENTS.md,
// "A system's integration tests run that system alone").
//
// EACH SUBTEST IS ONE EDGE: one scenario, ingested, and asserted. Every subtest
// asserts the invariants that bind ALL of them (every transcript read to its
// end, no orphan tool_result, no
// unparsed residue, no unknown residue, only documented withholding classes,
// books that name real file-plane identities, block units addressed by block,
// usage on block 0 alone, every entry on the file plane) plus whatever its own
// row declares.
//
// THE TABLE HOLDS ONE ROW PER DISTINCT ON-DISK SHAPE. Scenarios whose files
// fingerprint identically (record types, keys and value types, with ids and
// free text masked) and whose rows declare nothing of their own are one row:
// the prose-only family is `!md`, the identical `!fail-*` family is
// `!fail-execution`, the non-retried `!api-*` family is `!api-401`, the retried
// one `!api-529`, and `!push-config-off`, `!web-fetch` and `!query-eof` stand
// for their twins (test-cross-product-audit, 2026-10-14).

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"

	sharedlogging "agentrepl/logging"

	"agentrepl/shim-claude-sidecar/internal/discover"
)

// goldenRoot is where the shim's suite keeps the scenario goldens.
const goldenRoot = "../../../testdata/scenario-goldens"

// The tokens the shim's goldens stand for the drive's temp root with.
const (
	goldenRootToken     = "${GOLDEN_ROOT}"
	goldenRootSlugToken = "${GOLDEN_ROOT_SLUG}"
)

// goldenScenario is one row of the table.
type goldenScenario struct {
	// Prompt is the `!name` prompt that selects the scenario, verbatim.
	Prompt string

	// Subagent — the scenario writes at least one `agent-<id>.jsonl` plus its
	// `.meta.json`, so the subagent's own book must exist, keyed by the meta's
	// toolUseId.
	Subagent bool
	// BashRun — the scenario writes a `b*.output` spool, so the run's rows must
	// replay in the endpoint's order.
	BashRun bool
	// ExitCode — the spool is TERMINATED by `EXIT=<code>`, so the terminal row
	// must carry an `exited` termination.
	ExitCode bool
	// SpilledOutput — the scenario's result declares a `persistedOutputSize`, so
	// the settled shell states the PARTIAL extent and its omitted byte count.
	SpilledOutput bool
	// MovedShell — the scenario's `Bash` result names a `backgroundTaskId`, so no
	// settled shell frame may reach the agent's book from this plane.
	MovedShell bool
	// ContextCut — the scenario cuts the conversation, so exactly one
	// `AgentUpdate.context_cut` page line must land.
	ContextCut bool
	// APIError — the scenario writes a `system:api_error` record, so an
	// `AgentUpdate.api_error` page line must land.
	APIError bool
	// HeldTurn — the turn is STOPPED rather than finished, so its content
	// reached the main agent's book and nothing on the way was a question.
	HeldTurn bool
}

// goldenScenarios is the table.
var goldenScenarios = []goldenScenario{
	{Prompt: "!md"},
	{Prompt: "!read"},
	{Prompt: "!read-head"},
	{Prompt: "!read-range"},
	{Prompt: "!read-truncated"},
	{Prompt: "!read-image"},
	{Prompt: "!write-create"},
	{Prompt: "!write-update"},
	{Prompt: "!edit"},
	{Prompt: "!ide-diagnostics"},
	{Prompt: "!grep-content"},
	{Prompt: "!grep-files"},
	{Prompt: "!grep-count"},
	{Prompt: "!glob"},
	{Prompt: "!bash"},
	{Prompt: "!bash-fail"},
	{Prompt: "!bash-timeout", MovedShell: true},
	{Prompt: "!bash-spill", SpilledOutput: true},
	{Prompt: "!bash-image"},
	{Prompt: "!bash-detach", BashRun: true, ExitCode: true, MovedShell: true},
	{Prompt: "!bash-detach-fail", BashRun: true, ExitCode: true},
	{Prompt: "!bash-detach-live", BashRun: true},
	{Prompt: "!vendor-backgrounded"},
	{Prompt: "!web-fetch"},
	{Prompt: "!web-search"},
	{Prompt: "!skill"},
	{Prompt: "!skill-fail"},
	{Prompt: "!memory"},
	{Prompt: "!skills-injected"},
	{Prompt: "!task-create"},
	{Prompt: "!task-change"},
	{Prompt: "!task-reject"},
	{Prompt: "!send-message"},
	{Prompt: "!send-message-resumed"},
	{Prompt: "!send-message-refused"},
	{Prompt: "!subagent", Subagent: true},
	{Prompt: "!subagent-detached", Subagent: true},
	{Prompt: "!subagent-failed"},
	{Prompt: "!cancel-all", Subagent: true},
	{Prompt: "!plan"},
	{Prompt: "!findings"},
	{Prompt: "!worktree-keep"},
	{Prompt: "!worktree-remove"},
	{Prompt: "!cron"},
	{Prompt: "!push-sent"},
	{Prompt: "!push-config-off"},
	{Prompt: "!monitor-deadline"},
	{Prompt: "!monitor-persistent"},
	{Prompt: "!wakeup-schedule"},
	{Prompt: "!wakeup-stop"},
	{Prompt: "!artifact-publish"},
	{Prompt: "!artifact-list"},
	{Prompt: "!unmodeled"},
	{Prompt: "!hook-success"},
	{Prompt: "!hook-blocked"},
	{Prompt: "!hook-failed"},
	{Prompt: "!hook-cancelled"},
	{Prompt: "!perm-allow-once"},
	{Prompt: "!perm-allow-standing"},
	{Prompt: "!perm-deny-user"},
	{Prompt: "!perm-deny-policy"},
	{Prompt: "!perm-undecidable"},
	{Prompt: "!ask-single"},
	{Prompt: "!ask-multi"},
	{Prompt: "!ask-free"},
	{Prompt: "!ask-unanswered"},
	{Prompt: "!rotate", ContextCut: true},
	{Prompt: "!slash"},
	{Prompt: "!model-fallback"},
	{Prompt: "!compact", ContextCut: true},
	{Prompt: "!compact-auto", ContextCut: true},
	{Prompt: "!away-summary"},
	{Prompt: "!residue"},
	{Prompt: "!cold-seed"},
	{Prompt: "!fail-execution"},
	{Prompt: "!fail-stop-hook"},
	{Prompt: "!fail-continuation-prevented"},
	{Prompt: "!api-429", APIError: true},
	{Prompt: "!api-529", APIError: true},
	{Prompt: "!api-401", APIError: true},
	{Prompt: "!max-tokens"},
	{Prompt: "!refusal-fallback"},
	{Prompt: "!refusal-no-fallback"},
	{Prompt: "!context-window"},
	{Prompt: "!hold", HeldTurn: true},
	{Prompt: "!interrupt"},
	{Prompt: "!query-eof"},
}

func TestEveryMockedScenarioGoldenIngestsCleanly(t *testing.T) {
	for _, tc := range goldenScenarios {
		t.Run(tc.Prompt, func(t *testing.T) {
			t.Parallel()

			// Arrange: the scenario's files, written where the mock wrote them.
			h := newHarness(t, &fakeStore{})
			tree := materializeGolden(t, h, tc.Prompt)

			// Act.
			ingestGolden(t, h)

			// Assert: the invariants that bind every scenario.
			store := h.store
			batches := goldenBatches(store)
			entries := goldenEntries(store)
			requireEveryTranscriptReadToItsEnd(t, tc.Prompt, tree, batches)
			requireNoOrphanToolResults(t, tc.Prompt, batches)
			requireNoUnparsedResidue(t, tc.Prompt, batches)
			requireNoUnknownResidue(t, tc.Prompt, batches)
			requireDocumentedVendorSpecificKinds(t, tc.Prompt, batches)
			requireBooksAreKnownAgents(t, tc.Prompt, tree, entries)
			requireBlockUnitsAreAddressedByBlock(t, tc.Prompt, entries)
			requireUsageOnTheFirstBlockOnly(t, tc.Prompt, entries)
			for _, e := range entries {
				requireFilePlane(t, e)
			}

			// ...and what the row declares of its own.
			if tc.Subagent {
				requireSubagentBook(t, tc.Prompt, tree, entries)
			}
			if tc.BashRun {
				requireBashRunsReplayInOrder(t, tc.Prompt, entries)
			}
			if tc.ExitCode {
				requireExitCodeInTheTerminal(t, tc.Prompt, entries)
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
			if tc.HeldTurn {
				requireHeldTurnLandsWithoutAQuestion(t, tc.Prompt, tree, entries)
			}
		})
	}
}

func TestEveryGoldenScenarioRowHasAGolden(t *testing.T) {
	for _, tc := range goldenScenarios {
		// Arrange, Act.
		_, err := os.Stat(filepath.Join(goldenRoot, strings.TrimPrefix(tc.Prompt, "!"), "tree"))

		// Assert.
		if err != nil {
			t.Errorf("row %s names no golden: %v", tc.Prompt, err)
		}
	}
}

// cwdSlug is the vendor's project and spool directory name for an absolute
// directory: every byte that is not an ASCII letter or digit becomes `-`
// (the shim's engine/vendor-paths.ts states the same rule).
func cwdSlug(dir string) string {
	return strings.Map(func(r rune) rune {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') {
			return r
		}
		return '-'
	}, dir)
}

// goldenTree is what a materialized golden holds, by role.
type goldenTree struct {
	// Transcripts is every `<session>.jsonl`, each basename a main agent id.
	Transcripts []string
	// Subagents is every `agent-<id>.jsonl` under any session's subagents dir.
	Subagents []string
}

// mainAgentIDs answers the main agent id of every transcript: its FILE's session uuid.
func (g *goldenTree) mainAgentIDs() []string {
	ids := make([]string, 0, len(g.Transcripts))
	for _, p := range g.Transcripts {
		ids = append(ids, strings.TrimSuffix(filepath.Base(p), ".jsonl"))
	}
	sort.Strings(ids)
	return ids
}

// materializeGolden writes a scenario's golden tree into the harness's roots:
// the mock's config root is the sidecar's config root, its spool root the
// sidecar's spool root, and its temp root the harness's base, whose
// `workspace` is the harness's workspace. Every session the tree names is made
// LIVE and attributed, as the mock's own shim left it, and the sidecar is
// rebuilt over the roots.
func materializeGolden(t *testing.T, h *harness, prompt string) *goldenTree {
	t.Helper()
	h.rootA = filepath.Join(h.base, "cfg")
	src := filepath.Join(goldenRoot, strings.TrimPrefix(prompt, "!"), "tree")
	substitute := strings.NewReplacer(
		goldenRootToken+"/spools", h.spool,
		goldenRootToken+"/cfg", h.rootA,
		goldenRootToken, h.base,
		goldenRootSlugToken, cwdSlug(h.base),
	).Replace
	tree := &goldenTree{}
	err := filepath.WalkDir(src, func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		rel, err := filepath.Rel(src, path)
		if err != nil {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		var dest string
		switch {
		case strings.HasPrefix(rel, "cfg"+string(filepath.Separator)):
			dest = filepath.Join(h.rootA, substitute(strings.TrimPrefix(rel, "cfg"+string(filepath.Separator))))
		case strings.HasPrefix(rel, "spools"+string(filepath.Separator)):
			dest = filepath.Join(h.spool, substitute(strings.TrimPrefix(rel, "spools"+string(filepath.Separator))))
		default:
			return fmt.Errorf("a golden file outside cfg/ and spools/: %s", rel)
		}
		if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(dest, []byte(substitute(string(raw))), 0o644); err != nil {
			return err
		}
		switch {
		case strings.HasSuffix(dest, ".jsonl") && strings.HasPrefix(filepath.Base(dest), "agent-"):
			tree.Subagents = append(tree.Subagents, dest)
		case strings.HasSuffix(dest, ".jsonl") && filepath.Base(filepath.Dir(filepath.Dir(dest))) == "projects":
			tree.Transcripts = append(tree.Transcripts, dest)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("materializing the %s golden from %s: %v", prompt, src, err)
	}
	if len(tree.Transcripts) == 0 {
		t.Fatalf("the %s golden holds no transcript", prompt)
	}
	// THE SESSIONS ARE LIVE, as the mock's shim left them, and attributed to
	// the harness's workspace, as the shim's identity records attribute them.
	project := cwdSlug(filepath.Join(h.base, "workspace"))
	workspaceID, err := sharedlogging.WorkspaceID(h.workspace)
	if err != nil {
		t.Fatalf("derive the fixture's workspace id: %v", err)
	}
	h.sc = h.buildSidecar()
	for _, id := range tree.mainAgentIDs() {
		h.activate(t, id)
		h.sc.workspaceBySession[discover.Normalize(h.rootA)+"\x00"+project+"\x00"+id] = workspaceAttribution{dir: h.workspace, id: workspaceID}
	}
	return tree
}

// ingestGolden runs the cycle over the materialized tree until a whole pass
// writes nothing new: discovery, every file's poll, and the lost sweep, with
// the clock moved past every window a pass can wait on.
func ingestGolden(t *testing.T, h *harness) {
	t.Helper()
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	// 20 passes: a scenario's longest chain is a spawn, its subagent's meta, the
	// subagent's transcript and a spool claimed from it, each a pass apart.
	for pass := 0; pass < 20; pass++ {
		before := len(h.store.writes)
		h.sc.rescan()
		h.sc.producing(h.sc.pollAll)
		h.advance(time.Minute)
		h.sc.producing(h.sc.sweep)
		if len(h.store.writes) == before && pass > 0 {
			return
		}
	}
	t.Fatalf("the cycle was still writing after 20 passes")
}

// goldenBatches answers every write the store took, as the request it was.
func goldenBatches(store *fakeStore) []*storev1.WriteBatchRequest {
	out := make([]*storev1.WriteBatchRequest, 0, len(store.writes))
	for i, b := range store.writes {
		out = append(out, &storev1.WriteBatchRequest{Batch: b, Shapes: store.shapes[i]})
	}
	return out
}

// goldenEntries answers every entry the store took, in write order.
func goldenEntries(store *fakeStore) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, b := range store.writes {
		out = append(out, b.GetEntries()...)
	}
	return out
}

// ---- reading conversation.v1 facts out of what was written ----

func mockPageLinesOf(entries []*storev1.StoreEntry) []*storev1.StorePageLine {
	var out []*storev1.StorePageLine
	for _, e := range entries {
		if line := e.GetAgentUpdate().GetServeableFrame(); line != nil {
			out = append(out, line)
		}
	}
	return out
}

func mockFrameOf(line *storev1.StorePageLine) *conversationv1.AgentFrame {
	return line.GetAgentItem().GetAgentFrame()
}

func mockActivityOf(line *storev1.StorePageLine) *conversationv1.AgentActivity {
	return mockFrameOf(line).GetUpdate().GetActivity()
}

func mockLinesForBook(entries []*storev1.StoreEntry, agent string) []*storev1.StorePageLine {
	var out []*storev1.StorePageLine
	for _, line := range mockPageLinesOf(entries) {
		if line.GetPageAgentId().GetValue() == agent {
			out = append(out, line)
		}
	}
	return out
}

// withheldShapesOf answers the shape observations in BATCHES whose kind is
// KIND or, for a KIND ending in "/", starts with it.
func withheldShapesOf(batches []*storev1.WriteBatchRequest, kind string) []*storev1.ShapeObservation {
	var out []*storev1.ShapeObservation
	for _, b := range batches {
		for _, shape := range b.GetShapes() {
			if shape.GetKind() == kind || (strings.HasSuffix(kind, "/") && strings.HasPrefix(shape.GetKind(), kind)) {
				out = append(out, shape)
			}
		}
	}
	return out
}

// ---- the invariants every scenario must satisfy ----

// allowedVendorSpecificPrefixes and allowedVendorSpecificKinds are the residue
// classes the sidecar's AGENTS.md documents ("Withholding classes"). A kind
// outside them is a MODELLING GAP the mock exposed, not a licence to widen the
// list: widening it needs the evidence stated in the report.
var (
	allowedVendorSpecificPrefixes = []string{
		"attachment/",    // context-cut exclusions and the other attachment machinery
		"content_block/", // unmodeled content blocks
		"system/",        // the informational / turn_duration / away_summary … system lines
		"file-history-",  // the file-history pair
		"workflow/",      // workflow journals and spools — KICKED this wave
	}
	allowedVendorSpecificKinds = map[string]bool{
		"mode":                 true,
		"permission-mode":      true,
		"queue-operation":      true,
		"last-prompt":          true,
		"ai-title":             true,
		"pr-link":              true,
		"frame-link":           true,
		"attribution-snapshot": true,
		// A harness-injected user record: the system reminder, and the
		// `<local-command-caveat>` the `identity-rotation-clear` capture shows
		// immediately before a slash command's envelope.
		"user/meta":             true,
		"no_response_requested": true,
		"user_prompt":           true, // R15: a file-plane user prompt is never a page line
	}
)

// errorReporter is the part of testing.T a residue guard reports through, so
// TestTheMockResidueGuardsReadTheShapesSent can prove each guard fails.
type errorReporter interface {
	Helper()
	Errorf(format string, args ...any)
}

// orphanToolResultKind is what a tool_result whose CALL is behind the cursor
// lands on. It is DELIBERATELY NOT a documented withholding class: it is a
// settle the converter FAILED to perform, and the mock writes each scenario's
// transcript from the top, so no scenario has any excuse to produce one.
const orphanToolResultKind = "orphan_tool_result"

func requireNoOrphanToolResults(t errorReporter, scenario string, batches []*storev1.WriteBatchRequest) {
	t.Helper()
	for _, shape := range withheldShapesOf(batches, "vendor_specific/"+orphanToolResultKind) {
		t.Errorf("%s: a tool_result was withheld as %q — its call was never joined: %s",
			scenario, orphanToolResultKind, shape.GetFirstExample())
	}
}

// requireEveryTranscriptReadToItsEnd: the store holds a cursor at the end of
// every transcript and subagent transcript the golden wrote. The cursor is the
// sidecar's own statement of what it committed, so no row's other assertions
// can pass over a file the cycle never read.
func requireEveryTranscriptReadToItsEnd(t *testing.T, scenario string, tree *goldenTree, batches []*storev1.WriteBatchRequest) {
	t.Helper()
	committed := map[string]int64{}
	for _, b := range batches {
		if c := b.GetBatch().GetCursorAdvance(); c != nil && c.GetOffset() > committed[c.GetPath()] {
			committed[c.GetPath()] = c.GetOffset()
		}
	}
	for _, path := range append(append([]string{}, tree.Transcripts...), tree.Subagents...) {
		info, err := os.Stat(path)
		if err != nil {
			t.Fatalf("%s: stat %s: %v", scenario, path, err)
		}
		if got := committed[discover.Normalize(path)]; got != info.Size() {
			t.Errorf("%s: the cursor for %s stands at %d, want the file's end at %d", scenario, path, got, info.Size())
		}
	}
}

// requireNoUnparsedResidue: NOTHING the mocked vendor writes may fail to parse.
func requireNoUnparsedResidue(t errorReporter, scenario string, batches []*storev1.WriteBatchRequest) {
	t.Helper()
	for _, shape := range withheldShapesOf(batches, "unparsed") {
		t.Errorf("%s: the sidecar could not PARSE a line the mocked vendor wrote: %q", scenario, shape.GetFirstExample())
	}
}

// requireNoUnknownResidue: every discriminator the mock writes is one the
// converter models or deliberately withholds.
func requireNoUnknownResidue(t errorReporter, scenario string, batches []*storev1.WriteBatchRequest) {
	t.Helper()
	for _, shape := range withheldShapesOf(batches, "unknown/") {
		t.Errorf("%s: the sidecar parsed a record it does not model: %s (example %s)", scenario, shape.GetKind(), shape.GetFirstExample())
	}
}

// requireDocumentedVendorSpecificKinds: withholding is deliberate and its
// classes are enumerated.
func requireDocumentedVendorSpecificKinds(t errorReporter, scenario string, batches []*storev1.WriteBatchRequest) {
	t.Helper()
	for _, shape := range withheldShapesOf(batches, "vendor_specific/") {
		kind := strings.TrimPrefix(shape.GetKind(), "vendor_specific/")
		if !vendorSpecificKindAllowed(kind) {
			t.Errorf("%s: vendor_specific kind %q is outside the documented withholding classes", scenario, kind)
		}
	}
}

func vendorSpecificKindAllowed(kind string) bool {
	if allowedVendorSpecificKinds[kind] {
		return true
	}
	for _, prefix := range allowedVendorSpecificPrefixes {
		if strings.HasPrefix(kind, prefix) {
			return true
		}
	}
	return false
}

// requireBooksAreKnownAgents: every page line names a book that EXISTS as a
// file-plane identity — a transcript FILE's session uuid for a main agent, the
// spawning call's tool_use_id from `agent-<id>.meta.json` for a subagent.
func requireBooksAreKnownAgents(t *testing.T, scenario string, tree *goldenTree, entries []*storev1.StoreEntry) {
	t.Helper()
	known := map[string]bool{}
	for _, id := range tree.mainAgentIDs() {
		known[id] = true
	}
	subagents := goldenSubagentBookIDs(t, tree)
	for _, id := range subagents {
		known[id] = true
	}
	for _, line := range mockPageLinesOf(entries) {
		if book := line.GetPageAgentId().GetValue(); !known[book] {
			t.Errorf("%s: a page line names the book %q, which is neither a transcript file's session uuid %v nor a subagent's meta.json toolUseId %v",
				scenario, book, tree.mainAgentIDs(), subagents)
		}
	}
}

// goldenSubagentBookIDs reads each `agent-<id>.meta.json` and answers its
// `toolUseId` — the identity a subagent's own book is keyed by.
func goldenSubagentBookIDs(t *testing.T, tree *goldenTree) []string {
	t.Helper()
	var out []string
	for _, transcript := range tree.Subagents {
		meta := strings.TrimSuffix(transcript, ".jsonl") + ".meta.json"
		raw, err := os.ReadFile(meta)
		if err != nil {
			t.Fatalf("a subagent transcript has no metadata sidecar beside it (%s): %v", meta, err)
		}
		var fields struct {
			ToolUseID string `json:"toolUseId"`
		}
		if err := json.Unmarshal(raw, &fields); err != nil {
			t.Fatalf("decoding %s: %v", meta, err)
		}
		if fields.ToolUseID == "" {
			t.Fatalf("%s carries no toolUseId, which is the only key a subagent's book has", meta)
		}
		out = append(out, fields.ToolUseID)
	}
	sort.Strings(out)
	return out
}

// blockUnit is one text-or-thinking unit addressed as `<message.id>:<ordinal>`.
type blockUnit struct {
	messageID  string
	ordinal    int
	hasUsage   bool
	kind       string
	activityID string
}

// goldenBlockUnits reads every `activity:<message.id>:<ordinal>` unit written.
// Tool-call units are addressed by their `tool_use_id` instead and are not here.
func goldenBlockUnits(entries []*storev1.StoreEntry) []blockUnit {
	var out []blockUnit
	for _, e := range entries {
		key := e.GetUpsertKey()
		if !strings.HasPrefix(key, "activity:") {
			continue
		}
		id := strings.TrimPrefix(key, "activity:")
		cut := strings.LastIndex(id, ":")
		if cut < 0 {
			continue
		}
		ordinal, err := strconv.Atoi(id[cut+1:])
		if err != nil {
			continue
		}
		activity := e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
		out = append(out, blockUnit{
			messageID:  id[:cut],
			ordinal:    ordinal,
			hasUsage:   activity.GetUsage() != nil,
			kind:       fmt.Sprintf("%T", activity.GetItem()),
			activityID: activity.GetActivityId().GetValue(),
		})
	}
	return out
}

// requireBlockUnitsAreAddressedByBlock: an assistant message becomes one unit
// per content block, each addressed `<message.id>:<block ordinal>`, the
// ordinals running across the lines that share the message id.
func requireBlockUnitsAreAddressedByBlock(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	seen := map[string]blockUnit{}
	for _, unit := range goldenBlockUnits(entries) {
		identity := fmt.Sprintf("%s:%d", unit.messageID, unit.ordinal)
		if unit.ordinal < 0 {
			t.Errorf("%s: block unit %s has a negative ordinal", scenario, identity)
			continue
		}
		if unit.activityID != identity {
			t.Errorf("%s: the unit keyed %s carries the activity id %q, so its key and its identity disagree", scenario, identity, unit.activityID)
		}
		if prior, ok := seen[identity]; ok && prior.kind != unit.kind {
			t.Errorf("%s: the identity %s addresses two DIFFERENT units (%s and %s)", scenario, identity, prior.kind, unit.kind)
		}
		seen[identity] = unit
	}
}

// requireUsageOnTheFirstBlockOnly: EXACTLY ONE unit per API response carries
// `usage`, the unit for block 0.
func requireUsageOnTheFirstBlockOnly(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	for _, unit := range goldenBlockUnits(entries) {
		if unit.ordinal != 0 && unit.hasUsage {
			t.Errorf("%s: unit %s:%d carries usage, but only block 0 of a response may", scenario, unit.messageID, unit.ordinal)
		}
	}
}

// requireFilePlane states the envelope duty every sidecar write owes: the file
// plane arm, a non-empty write_id, and a non-empty upsert_key.
func requireFilePlane(t *testing.T, e *storev1.StoreEntry) {
	t.Helper()
	if e.GetPlane().GetFile() == nil {
		t.Errorf("entry %q is not on the file plane: %v", e.GetUpsertKey(), e.GetPlane())
	}
	if e.GetWriteId() == "" {
		t.Errorf("entry %q carries no write_id", e.GetUpsertKey())
	}
	if e.GetUpsertKey() == "" {
		t.Errorf("entry with write_id %q carries no upsert_key", e.GetWriteId())
	}
}

// ---- what a row declares of its own ----

// requireContextCutPageLine: the compaction and clear scenarios each land ONE
// context_cut page line.
func requireContextCutPageLine(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	cuts := map[string]bool{}
	for _, e := range entries {
		if line := e.GetAgentUpdate().GetServeableFrame(); line != nil && mockFrameOf(line).GetUpdate().GetContextCut() != nil {
			cuts[e.GetUpsertKey()] = true
		}
	}
	if len(cuts) != 1 {
		t.Errorf("%s: expected exactly one AgentUpdate.context_cut page line, got %d", scenario, len(cuts))
	}
}

// requireAPIErrorPageLine: a `system/api_error` record is MID-TURN EVIDENCE and
// lands as its own page line.
func requireAPIErrorPageLine(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	for _, line := range mockPageLinesOf(entries) {
		if mockFrameOf(line).GetUpdate().GetApiError() != nil {
			return
		}
	}
	t.Errorf("%s: no AgentUpdate.api_error page line was produced", scenario)
}

// requireSubagentBook: a subagent's constituents form ITS OWN book, keyed by
// the spawning call's tool_use_id.
func requireSubagentBook(t *testing.T, scenario string, tree *goldenTree, entries []*storev1.StoreEntry) {
	t.Helper()
	books := goldenSubagentBookIDs(t, tree)
	if len(books) == 0 {
		t.Fatalf("%s: the scenario is declared to spawn a subagent, but the golden holds no agent-<id>.meta.json", scenario)
	}
	for _, book := range books {
		if len(mockLinesForBook(entries, book)) == 0 {
			t.Errorf("%s: the subagent book %q (its meta.json toolUseId) holds no page lines", scenario, book)
		}
	}
}

// goldenBashReplay answers each run's rows as the store replays them: one row
// per upsert key, at that key's FIRST insert, carrying its LATEST frame.
func goldenBashReplay(entries []*storev1.StoreEntry) map[string][]*conversationv1.AgentBash {
	type row struct {
		key   string
		frame *conversationv1.AgentBash
	}
	runs := map[string][]*row{}
	byKey := map[string]*row{}
	for _, e := range entries {
		bash := e.GetAgentUpdate().GetBash()
		if bash == nil {
			continue
		}
		if r, ok := byKey[e.GetUpsertKey()]; ok {
			r.frame = bash.GetFrame()
			continue
		}
		r := &row{key: e.GetUpsertKey(), frame: bash.GetFrame()}
		byKey[r.key] = r
		run := bash.GetRun().GetValue()
		runs[run] = append(runs[run], r)
	}
	out := map[string][]*conversationv1.AgentBash{}
	for run, rows := range runs {
		for _, r := range rows {
			out[run] = append(out[run], r.frame)
		}
	}
	return out
}

func isTerminalBash(frame *conversationv1.AgentBash) bool {
	return frame.GetSuccess() != nil || frame.GetFailure() != nil
}

// describeBashRows names each row's arm, for a failure message.
func describeBashRows(rows []*conversationv1.AgentBash) []string {
	var out []string
	for _, row := range rows {
		switch {
		case row.GetStart() != nil:
			out = append(out, "start")
		case row.GetTail() != nil:
			out = append(out, fmt.Sprintf("tail@%d", row.GetTail().GetBytesOmitted()+uint64(len(row.GetTail().GetText()))))
		case row.GetProgress() != nil:
			out = append(out, "progress")
		case row.GetSuccess() != nil:
			out = append(out, "success")
		case row.GetFailure() != nil:
			out = append(out, "failure")
		default:
			out = append(out, "unset")
		}
	}
	return out
}

// requireBashRunsReplayInOrder: a detached shell's rows replay start first,
// then its tail, then its terminal LAST and once; no tail carries more than the
// renderer's cap, and each accounts for at least as many written bytes as the
// one before it.
func requireBashRunsReplayInOrder(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	runs := goldenBashReplay(entries)
	if len(runs) == 0 {
		t.Fatalf("%s: the scenario is declared to launch a detached shell, but no bash rows were written", scenario)
	}
	for run, rows := range runs {
		terminals := 0
		seenTerminal := false
		var written uint64
		for i, row := range rows {
			if (row.GetStart() != nil || row.GetTail() != nil) && seenTerminal {
				t.Errorf("%s: run %s replayed a start or tail AFTER its terminal: %v", scenario, run, describeBashRows(rows))
			}
			if tail := row.GetTail(); tail != nil {
				if n := len(tail.GetText()); n > int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES) {
					t.Errorf("%s: run %s row %d stores %d bytes of tail, past the renderer's cap", scenario, run, i, n)
				}
				total := tail.GetBytesOmitted() + uint64(len(tail.GetText()))
				if total < written {
					t.Errorf("%s: run %s row %d accounts for %d written bytes after a row that accounted for %d", scenario, run, i, total, written)
				}
				written = total
			}
			if isTerminalBash(row) {
				terminals++
				seenTerminal = true
			}
		}
		if terminals > 1 {
			t.Errorf("%s: run %s replayed %d terminal rows, want at most 1: %v", scenario, run, terminals, describeBashRows(rows))
		}
		if seenTerminal && !isTerminalBash(rows[len(rows)-1]) {
			t.Errorf("%s: run %s did not end on its terminal row: %v", scenario, run, describeBashRows(rows))
		}
	}
}

// requireExitCodeInTheTerminal: the spool's `EXIT=<code>` marker is the run's
// termination and rides the terminal row.
func requireExitCodeInTheTerminal(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	for run, rows := range goldenBashReplay(entries) {
		terminal := rows[len(rows)-1]
		if !isTerminalBash(terminal) {
			t.Errorf("%s: run %s replayed no terminal: %v", scenario, run, describeBashRows(rows))
			continue
		}
		if terminal.GetSuccess().GetCompleted().GetTermination().GetExited() == nil {
			t.Errorf("%s: run %s terminated without an `exited` termination: %v", scenario, run, describeBashRows(rows[len(rows)-1:]))
		}
	}
}

// requireHeldTurnLandsWithoutAQuestion: a turn that is STOPPED rather than
// finished still had its content read off disk, and nothing on the way to the
// stop was a question. The terminal is the stream plane's, never asserted here.
func requireHeldTurnLandsWithoutAQuestion(t *testing.T, scenario string, tree *goldenTree, entries []*storev1.StoreEntry) {
	t.Helper()
	ids := tree.mainAgentIDs()
	main := ids[len(ids)-1]
	if len(mockLinesForBook(entries, main)) == 0 {
		t.Errorf("%s: the held turn's records reached no page line of the main agent's book %q", scenario, main)
	}
	for _, line := range mockPageLinesOf(entries) {
		if q := mockFrameOf(line).GetUpdate().GetQuestion(); q != nil {
			t.Errorf("%s: the held turn produced a question page line: %v", scenario, q)
		}
	}
}

// requireBashPartialExtent: a settled shell whose output was TRUNCATED says so
// on this plane too, with the omitted byte count and the spill file.
func requireBashPartialExtent(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	settled := 0
	for _, line := range mockPageLinesOf(entries) {
		bash := mockActivityOf(line).GetBash().GetSuccess()
		if bash == nil {
			continue
		}
		settled++
		partial := bash.GetCompleted().GetOutput().GetText().GetPartial()
		switch {
		case partial == nil:
			t.Errorf("%s: the settled shell's output extent is not the partial arm", scenario)
		case partial.GetBytesOmitted() == 0:
			t.Errorf("%s: the settled shell's partial extent omits 0 bytes", scenario)
		case partial.GetSpilled().GetPath() == "":
			t.Errorf("%s: the settled shell's partial extent names no spill file", scenario)
		}
	}
	if settled == 0 {
		t.Errorf("%s: no settled shell reached a page line; there is nothing to state an extent about", scenario)
	}
}

// requireNoSettledShellForMovedWork: a command that MOVED to the background is
// not settled by this plane.
func requireNoSettledShellForMovedWork(t *testing.T, scenario string, entries []*storev1.StoreEntry) {
	t.Helper()
	started := 0
	for _, line := range mockPageLinesOf(entries) {
		bash := mockActivityOf(line).GetBash()
		if bash == nil {
			continue
		}
		if bash.GetStart() != nil {
			started++
		}
		if bash.GetSuccess() != nil || bash.GetFailure() != nil {
			t.Errorf("%s: the file plane settled a shell whose work moved to the background: %v", scenario, bash.GetResult())
		}
	}
	if started == 0 {
		t.Errorf("%s: no shell START reached a page line, so the negative above asserts nothing", scenario)
	}
}

// recordingReporter is an errorReporter that keeps what it was told.
type recordingReporter struct{ errors []string }

func (r *recordingReporter) Helper() {}
func (r *recordingReporter) Errorf(format string, args ...any) {
	r.errors = append(r.errors, fmt.Sprintf(format, args...))
}

// batchWithShapes is a recorded batch carrying shape observations of KINDS.
func batchWithShapes(kinds ...string) []*storev1.WriteBatchRequest {
	b := &storev1.WriteBatchRequest{}
	for _, kind := range kinds {
		b.Shapes = append(b.Shapes, &storev1.ShapeObservation{Kind: kind, FirstExample: []byte(`{"k":1}`)})
	}
	return []*storev1.WriteBatchRequest{b}
}

func TestTheMockResidueGuardsReadTheShapesSent(t *testing.T) {
	tests := []struct {
		name  string
		guard func(errorReporter, string, []*storev1.WriteBatchRequest)
		kind  string
		fails bool
	}{
		{"an unparsed shape fails the unparsed guard", requireNoUnparsedResidue, "unparsed", true},
		{"a vendor_specific shape passes the unparsed guard", requireNoUnparsedResidue, "vendor_specific/queue-operation", false},
		{"an unknown shape fails the unknown guard", requireNoUnknownResidue, "unknown/type:mystery", true},
		{"an unparsed shape passes the unknown guard", requireNoUnknownResidue, "unparsed", false},
		{"an undocumented vendor_specific kind fails the documented-kinds guard", requireDocumentedVendorSpecificKinds, "vendor_specific/nobody-decided-this", true},
		{"an orphan tool_result fails the orphan guard", requireNoOrphanToolResults, "vendor_specific/" + orphanToolResultKind, true},
		{"another vendor_specific kind passes the orphan guard", requireNoOrphanToolResults, "vendor_specific/queue-operation", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := &recordingReporter{}

			// Act.
			tt.guard(r, "scenario", batchWithShapes(tt.kind))

			// Assert.
			if failed := len(r.errors) > 0; failed != tt.fails {
				t.Fatalf("guard over a %q shape failed = %v (%v), want %v", tt.kind, failed, r.errors, tt.fails)
			}
		})
	}
}
