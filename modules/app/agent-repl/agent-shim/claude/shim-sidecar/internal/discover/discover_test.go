package discover

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func TestMain(m *testing.M) {
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	os.Exit(m.Run())
}

// fixture lays out roots under one temp dir and returns a Discoverer plus the
// log lines it wrote.
func fixture(t *testing.T, files ...string) (*Discoverer, string, string, *[]string) {
	t.Helper()
	base := t.TempDir()
	rootA := filepath.Join(base, "config-a")
	rootB := filepath.Join(base, "config-b")
	spool := filepath.Join(base, "spool")
	for _, dir := range []string{rootA, rootB, spool} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("creating %s: %v", dir, err)
		}
	}
	for _, rel := range files {
		write(t, filepath.Join(base, rel))
	}
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "discover-test"})
	return New([]string{rootA, rootB}, spool, log), base, spool, &logs
}

type sliceWriter struct{ lines *[]string }

func (w sliceWriter) Write(p []byte) (int, error) {
	*w.lines = append(*w.lines, string(p))
	return len(p), nil
}

func write(t *testing.T, path string) {
	t.Helper()
	body := []byte("{}\n")
	if strings.HasSuffix(path, ".meta.json") {
		// A META FIXTURE MUST BE A META FILE. It is the only source of the
		// agent's identity, so an empty object is not a lesser version of one —
		// it is a file the reader must refuse, which is its own subject below.
		body = []byte(metaBody(toolUseIDFor(path)))
	}
	writeRaw(t, path, body)
}

// writeRaw writes exactly the given bytes, for the subjects about a meta file
// that is present and unusable.
func writeRaw(t *testing.T, path string, body []byte) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("creating %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, body, 0o644); err != nil {
		t.Fatalf("writing %s: %v", path, err)
	}
}

// metaBody spells the vendor's companion file: FOUR camelCase fields and no
// model — the model is stated only by the transcript's own assistant lines.
func metaBody(toolUseID string) string {
	return `{"agentType":"general-purpose","description":"do the thing","toolUseId":"` +
		toolUseID + `","spawnDepth":1}`
}

// toolUseIDFor is the spawning-call id this suite gives an agent-<id> fixture,
// so an assertion can name the identity the meta file supplies WITHOUT it being
// derivable from the file name in production.
func toolUseIDFor(metaPath string) string {
	base := strings.TrimSuffix(filepath.Base(metaPath), ".meta.json")
	return "toolu_" + strings.TrimPrefix(base, "agent-")
}

// find returns the scanned target whose path ends with suffix.
func find(t *testing.T, targets []Target, suffix string) Target {
	t.Helper()
	for _, target := range targets {
		if strings.HasSuffix(target.Path, suffix) {
			return target
		}
	}
	t.Fatalf("no target ending in %q among %d targets", suffix, len(targets))
	return Target{}
}

func TestScanFindsSessionTranscript(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t, "config-a/projects/proj/sess-1.jsonl")

	// Act.
	got := find(t, d.Scan(), "sess-1.jsonl")

	// Assert.
	if got.Kind != tail.KindSessionTranscript || got.SessionID != "sess-1" {
		t.Fatalf("target = %+v, want a session transcript identified sess-1", got)
	}
}

func TestScanFindsSubagentTranscript(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t,
		"config-a/projects/proj/sess-1/subagents/agent-abc.jsonl",
		"config-a/projects/proj/sess-1/subagents/agent-abc.meta.json",
	)

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	// THE IDENTITY IS THE SPAWNING CALL FROM THE META FILE; `agent-abc` is a
	// locator that stays on VendorAgentID and never becomes an AgentId.
	if got.Kind != tail.KindAgentTranscript || got.AgentID != "toolu_abc" || got.SessionID != "sess-1" {
		t.Fatalf("target = %+v, want a subagent transcript identified toolu_abc under sess-1", got)
	}
	if got.VendorAgentID != "abc" {
		t.Fatalf("vendor agent id = %q, want the file name's locator", got.VendorAgentID)
	}
}

func TestScanFindsWorkflowJournal(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t, "config-a/projects/proj/sess-1/subagents/workflows/wf_7/journal.jsonl")

	// Act.
	got := find(t, d.Scan(), "journal.jsonl")

	// Assert.
	if got.Kind != tail.KindWorkflowJournal || got.RunID != "wf_7" {
		t.Fatalf("target = %+v, want the journal of run wf_7", got)
	}
}

func TestScanFindsWorkflowPerAgentTranscript(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t,
		"config-a/projects/proj/sess-1/subagents/workflows/wf_7/agent-xyz.jsonl",
		"config-a/projects/proj/sess-1/subagents/workflows/wf_7/agent-xyz.meta.json",
	)

	// Act.
	got := find(t, d.Scan(), "agent-xyz.jsonl")

	// Assert.
	if got.Kind != tail.KindWorkflowJournal || got.RunID != "wf_7" || got.AgentID != "toolu_xyz" {
		t.Fatalf("target = %+v, want workflow wf_7's per-agent transcript identified toolu_xyz", got)
	}
}

func TestScanCoversBothConfigRoots(t *testing.T) {
	// Arrange: the second account's transcript is invisible to a single-root scan.
	d, _, _, _ := fixture(t,
		"config-a/projects/proj/sess-a.jsonl",
		"config-b/projects/proj/sess-b.jsonl",
	)

	// Act.
	targets := d.Scan()

	// Assert.
	find(t, targets, "sess-a.jsonl")
	find(t, targets, "sess-b.jsonl")
}

func TestScanClassifiesSpoolsByPrefix(t *testing.T) {
	tests := []struct {
		name     string
		file     string
		wantKind tail.Kind
		wantRaw  bool
	}{
		{name: "shell output", file: "b17.output", wantKind: tail.KindShellSpool, wantRaw: true},
		{name: "agent transcript", file: "a17.output", wantKind: tail.KindAgentTranscript, wantRaw: false},
		// R-S4: workflow is KICKED this wave, so a w* spool is a RECOGNIZED
		// prefix whose conversion deliberately does not exist yet — declared
		// residue rather than a journal, read raw because no conversion would
		// use its record structure.
		{name: "workflow spool", file: "w17.output", wantKind: tail.KindWorkflowSpool, wantRaw: true},
		// An unrecognized prefix is a different thing entirely: a
		// total-ingestion VIOLATION, whose bytes land as unparsed residue.
		{name: "unrecognized prefix", file: "q17.output", wantKind: tail.KindResidueSpool, wantRaw: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			d, _, _, _ := fixture(t, filepath.Join("spool", "claude-501", "proj", "sess-1", "tasks", tc.file))

			// Act.
			got := find(t, d.Scan(), tc.file)

			// Assert.
			if got.Kind != tc.wantKind || got.Raw != tc.wantRaw {
				t.Fatalf("target = %+v, want kind %s raw=%t", got, tc.wantKind, tc.wantRaw)
			}
		})
	}
}

func TestSpoolCarriesNoSessionID(t *testing.T) {
	// Arrange: the spool path's session-shaped segment is the harness's RUNTIME
	// id, which disagrees with the transcript's after a resume.
	d, _, _, _ := fixture(t, "spool/claude-501/proj/runtime-sess/tasks/b1.output")

	// Act.
	got := find(t, d.Scan(), "b1.output")

	// Assert.
	if got.SessionID != "" {
		t.Fatalf("spool target claimed session %q; a spool path is a location, never an identity", got.SessionID)
	}
}

func TestUnknownSpoolPrefixIsIngestedAsResidue(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t, "spool/claude-501/proj/sess-1/tasks/q17.output")

	// Act.
	got := find(t, d.Scan(), "q17.output")

	// Assert: dropping it from discovery is the one outcome total ingestion
	// forbids, so it is kept and routed to residue.
	if got.Kind != tail.KindResidueSpool {
		t.Fatalf("target kind = %s, want the residue spool kind", got.Kind)
	}
}

func TestUnknownSpoolPrefixIsLoggedAsAViolation(t *testing.T) {
	// Arrange.
	d, _, _, logs := fixture(t, "spool/claude-501/proj/sess-1/tasks/q17.output")

	// Act.
	d.Scan()

	// Assert.
	// The branch is the OPERATION at ERROR, and it names the path and the task id
	// whose prefix could not select a conversion.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "classify-spool", "error")
	if got := ctxString(t, rec, "task_id"); got != "q17" {
		t.Fatalf("task_id = %q, want the unclassifiable spool's task id", got)
	}
}

func TestSubagentTranscriptWithoutMetaIsHeld(t *testing.T) {
	// Arrange: the transcript exists, its meta does not.
	d, _, _, _ := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if !got.MetaMissing {
		t.Fatal("a transcript with no meta file was reported as ingestible")
	}
}

func TestSubagentTranscriptWithoutMetaIsStillDiscovered(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")

	// Act: two scans, as a held transcript is re-checked on every rescan.
	d.Scan()
	targets := d.Scan()

	// Assert: it is held, never dropped.
	find(t, targets, "agent-abc.jsonl")
}

func TestHeldTranscriptWarnsOnce(t *testing.T) {
	// Arrange.
	d, _, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")

	// Act.
	d.Scan()
	d.Scan()

	// Assert: a rescan every few seconds must not repeat the same line forever.
	requireOnceIn(t, parseLogLines(t, *logs), "discover-meta", "warn")
}

func TestMetaAppearingClearsTheHold(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	d.Scan()
	write(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"))

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if got.MetaMissing {
		t.Fatal("the transcript stayed held after its meta file appeared")
	}
}

func TestMetaCompanionIsNotItselfATarget(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t,
		"config-a/projects/proj/sess-1/subagents/agent-abc.jsonl",
		"config-a/projects/proj/sess-1/subagents/agent-abc.meta.json",
	)

	// Act.
	_, ok := d.Classify(filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"))

	// Assert.
	if ok {
		t.Fatal("a meta.json companion was classified as a tailable file")
	}
}

func TestClassifyNormalizesTheSymlinkedPath(t *testing.T) {
	// Arrange: a link standing in for macOS's /tmp -> /private/tmp.
	d, base, spool, _ := fixture(t, "spool/claude-501/proj/sess-1/tasks/b1.output")
	link := filepath.Join(base, "spool-link")
	if err := os.Symlink(spool, link); err != nil {
		t.Fatalf("linking %s: %v", link, err)
	}

	// Act.
	got, ok := d.Classify(filepath.Join(link, "claude-501", "proj", "sess-1", "tasks", "b1.output"))

	// Assert: the same file must not read as two.
	if !ok {
		t.Fatal("the symlinked spelling was not classified at all")
	}
	want := filepath.Join(Normalize(spool), "claude-501", "proj", "sess-1", "tasks", "b1.output")
	if got.Path != want {
		t.Fatalf("path = %q, want the resolved spelling %q", got.Path, want)
	}
}

func TestNormalizeKeepsAPathThatDoesNotExistYet(t *testing.T) {
	// Arrange: a spool is observed before it exists.
	base := t.TempDir()
	path := filepath.Join(base, "not", "created", "yet.output")

	// Act.
	got := Normalize(path)

	// Assert.
	if !strings.HasSuffix(got, filepath.Join("not", "created", "yet.output")) {
		t.Fatalf("Normalize(%q) = %q, want the not-yet-created suffix preserved", path, got)
	}
}

func TestNormalizeEmptyPath(t *testing.T) {
	// Arrange, Act.
	got := Normalize("")

	// Assert.
	if got != "" {
		t.Fatalf("Normalize(\"\") = %q, want the empty path unchanged", got)
	}
}

func TestClassifyRejectsAnUnwatchedShape(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t)

	// Act.
	_, ok := d.Classify(filepath.Join(base, "config-a", "settings.json"))

	// Assert.
	if ok {
		t.Fatal("a path matching none of the four shapes was classified")
	}
}

// The on-disk layout: transcripts
// <config root>/projects/<cwd-slug>/<vendor session>.jsonl, subagents
// .../<vendor session>/subagents/agent-<id>.jsonl + meta.json, spools
// <spool root>/[claude-<uid>/]<cwd-slug>/<vendor session>/tasks/<task>.output.
//
// <cwd-slug> replaces EVERY byte of the absolute cwd outside [A-Za-z0-9] with
// '-' (underscores included, case preserved), so it is LOSSY and not
// invertible. These tests pin that the slug is matched positionally and its
// content is never read.

func TestScanAcceptsALossyCwdSlug(t *testing.T) {
	// Arrange: the slug of /private/var/folders/_m/x, whose underscore and dots
	// are all flattened to dashes.
	d, _, _, _ := fixture(t, "config-a/projects/-private-var-folders--m-x/sess-1.jsonl")

	// Act.
	got := find(t, d.Scan(), "sess-1.jsonl")

	// Assert.
	if got.Kind != tail.KindSessionTranscript || got.SessionID != "sess-1" {
		t.Fatalf("target = %+v, want the session transcript inside the slugged dir", got)
	}
}

func TestATargetDecodesNothingFromTheSlug(t *testing.T) {
	// Arrange: two DIFFERENT cwds can render to one slug, so a slug read back as
	// a path would be a guess.
	d, _, _, _ := fixture(t, "config-a/projects/-a-b-c/sess-1.jsonl")

	// Act.
	got := find(t, d.Scan(), "sess-1.jsonl")

	// Assert: no field carries the slug or anything derived from it.
	for name, value := range map[string]string{
		"SessionID": got.SessionID, "AgentID": got.AgentID,
		"TaskID": got.TaskID, "RunID": got.RunID,
	} {
		if strings.Contains(value, "-a-b-c") {
			t.Fatalf("%s = %q carries the cwd slug; the slug is an opaque directory name", name, value)
		}
	}
}

func TestASpoolDecodesNothingFromItsSlugOrRuntimeSession(t *testing.T) {
	// Arrange.
	d, _, _, _ := fixture(t, "spool/claude-501/-a-b-c/runtime-sess/tasks/b1.output")

	// Act.
	got := find(t, d.Scan(), "b1.output")

	// Assert: only the task basename carries an identity.
	if got.TaskID != "b1" || got.SessionID != "" {
		t.Fatalf("target = %+v, want only the task id read from the path", got)
	}
}

func TestScanFindsASpoolWhenTheRootIsTheUIDDir(t *testing.T) {
	// Arrange: a mock harness points --spool-root straight at claude-<uid>.
	base := t.TempDir()
	root := filepath.Join(base, "config-a")
	spool := filepath.Join(base, "claude-501")
	for _, dir := range []string{root, spool} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("creating %s: %v", dir, err)
		}
	}
	write(t, filepath.Join(spool, "-Users-me-work", "sess-1", "tasks", "b1f.output"))
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "discover-test"})
	d := New([]string{root}, spool, log)

	// Act.
	got := find(t, d.Scan(), "b1f.output")

	// Assert: neither spelling of the root may make a spool invisible.
	if got.Kind != tail.KindShellSpool || got.TaskID != "b1f" {
		t.Fatalf("target = %+v, want the shell spool b1f", got)
	}
}

func TestScanFindsASpoolWhenTheRootIsTheUIDDirsParent(t *testing.T) {
	// Arrange: production points --spool-root at /tmp and lets discovery resolve
	// claude-<uid> itself.
	d, _, _, _ := fixture(t, "spool/claude-501/-Users-me-work/sess-1/tasks/a2c.output")

	// Act.
	got := find(t, d.Scan(), "a2c.output")

	// Assert.
	if got.Kind != tail.KindAgentTranscript || got.TaskID != "a2c" {
		t.Fatalf("target = %+v, want the agent spool a2c", got)
	}
}

func TestASpoolIsFoundOnlyOnceUnderEitherSpelling(t *testing.T) {
	// Arrange: both globs match the same file when the root is the uid dir's
	// parent, because claude-501/... is also <anything>/<...>.
	d, _, _, _ := fixture(t, "spool/claude-501/-Users-me-work/sess-1/tasks/b1.output")

	// Act.
	var found int
	for _, target := range d.Scan() {
		if strings.HasSuffix(target.Path, "b1.output") {
			found++
		}
	}

	// Assert: one file must not enter the reader twice.
	if found != 1 {
		t.Fatalf("the spool was discovered %d times, want once", found)
	}
}

func TestClassifyRejectsASpoolWithNoTasksSegment(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t)

	// Act.
	_, ok := d.Classify(filepath.Join(base, "spool", "claude-501", "proj", "sess-1", "notes", "b1.output"))

	// Assert.
	if ok {
		t.Fatal("a path outside a tasks/ directory was classified as a spool")
	}
}

func TestTheMetaFileSuppliesTheAgentsIdentity(t *testing.T) {
	// Arrange. THE CROSS-PLANE MINTING RULE: a subagent's AgentId is the
	// tool_use_id of the call that spawned it, which the file plane reads here
	// and the stream plane reads off the SDK — one id for one agent.
	d, _, _, _ := fixture(t,
		"config-a/projects/proj/sess-1/subagents/agent-abc.jsonl",
		"config-a/projects/proj/sess-1/subagents/agent-abc.meta.json",
	)

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if got.AgentID != got.Meta.ToolUseID {
		t.Fatalf("AgentID = %q but the meta names toolUseId %q; the identity IS the spawning call",
			got.AgentID, got.Meta.ToolUseID)
	}
	if got.AgentID == got.VendorAgentID {
		t.Fatal("the agent's identity must not be its file name's locator")
	}
}

func TestTheMetaFilesFourFieldsAreParsed(t *testing.T) {
	// Arrange. Exactly four camelCase fields, and NO model: the model is not
	// stated here at all, and the agent's models_used comes from its transcript's
	// own assistant lines.
	d, _, _, _ := fixture(t,
		"config-a/projects/proj/sess-1/subagents/agent-abc.jsonl",
		"config-a/projects/proj/sess-1/subagents/agent-abc.meta.json",
	)

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if got.Meta.AgentType != "general-purpose" {
		t.Errorf("agentType = %q", got.Meta.AgentType)
	}
	if got.Meta.Description != "do the thing" {
		t.Errorf("description = %q", got.Meta.Description)
	}
	if got.Meta.ToolUseID != "toolu_abc" {
		t.Errorf("toolUseId = %q", got.Meta.ToolUseID)
	}
	if got.Meta.SpawnDepth != 1 {
		t.Errorf("spawnDepth = %d", got.Meta.SpawnDepth)
	}
}

func TestAMetaFileWithNoToolUseIdHoldsTheTranscript(t *testing.T) {
	// Arrange. Without it the agent has no identity the other plane would agree
	// with, and naming it by its file name would mint a SECOND book for one
	// agent that no consumer could reconcile. Held, exactly as a missing file is.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	writeRaw(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"),
		[]byte(`{"agentType":"general-purpose","description":"d","spawnDepth":1}`))

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if !got.MetaMissing {
		t.Fatal("a meta file that names no spawning call must hold its transcript, not name the agent by its file")
	}
	if got.AgentID != "" {
		t.Fatalf("AgentID = %q; a held transcript has no identity to offer", got.AgentID)
	}
}

func TestAnUnparsableMetaFileHoldsTheTranscriptLoudly(t *testing.T) {
	// Arrange. Unlike a missing file this one will not fix itself, so it is an
	// ERROR rather than the ordinary held warning.
	d, base, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	writeRaw(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"),
		[]byte("not json at all"))

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if !got.MetaMissing {
		t.Fatal("an unreadable meta file must hold its transcript")
	}
	requireOnceIn(t, parseLogLines(t, *logs), "discover-meta", "error")
}

func TestAnUnknownSpoolPrefixIsStatedOncePerPath(t *testing.T) {
	// Arrange. Every scan re-classifies every file, and a task id with no kind
	// prefix will never grow one, so a defect stated per scan is a defect stated
	// forever — the loop that drowns every other reader's records.
	d, _, _, logs := fixture(t, "spool/claude-501/proj/sess-1/tasks/q17.output")

	// Act: three scans over the same unclassifiable spool.
	d.Scan()
	d.Scan()
	d.Scan()

	// Assert.
	stated := opsAt(parseLogLines(t, *logs), "classify-spool", "error")
	if len(stated) != 1 {
		t.Fatalf("the classification defect was stated %d times across three scans, want exactly once", len(stated))
	}
}

// A WORKFLOW AGENT'S META IS A DIFFERENT DOCUMENT, NOT AN UNREADABLE ONE. The
// vendor writes no toolUseId for an agent no tool call spawned; the run and the
// parent session on the path are what attribute it, so the transcript is
// ingestible and nothing is held.
func TestWorkflowAgentTranscriptIsNotHeldForAMissingToolUseID(t *testing.T) {
	// Arrange.
	const rel = "config-a/projects/proj/sess-1/subagents/workflows/wf_0297f159-ca1/agent-a1e.jsonl"
	d, base, _, logs := fixture(t, rel)
	writeRaw(t, filepath.Join(base, strings.TrimSuffix(rel, ".jsonl")+".meta.json"), []byte(workflowMetaBody))

	// Act.
	got := find(t, d.Scan(), "agent-a1e.jsonl")

	// Assert.
	if got.MetaMissing {
		t.Fatal("a workflow agent's transcript was held for a toolUseId the vendor never writes")
	}
	if got.Meta.Shape != ShapeWorkflow || got.Meta.AgentType != "workflow-subagent" {
		t.Fatalf("meta = %+v, want the parsed workflow shape", got.Meta)
	}
	if got.AgentID != "" {
		t.Fatalf("AgentID = %q; a workflow agent is attributed to its run, never to a spawning call", got.AgentID)
	}
	if got.RunID != "wf_0297f159-ca1" {
		t.Fatalf("RunID = %q, want the workflow run the path names", got.RunID)
	}
	if records := opsAt(parseLogLines(t, *logs), "discover-meta", "warn"); len(records) != 0 {
		t.Fatalf("a legitimate workflow meta was warned about: %v", records)
	}
}

// A workflow-SHAPED meta outside a workflow run names nothing: there is no run
// to attribute it to and no spawning call either.
func TestWorkflowShapedMetaOutsideAWorkflowRunIsHeld(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	writeRaw(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"),
		[]byte(workflowMetaBody))

	// Act.
	got := find(t, d.Scan(), "agent-abc.jsonl")

	// Assert.
	if !got.MetaMissing {
		t.Fatal("a meta naming neither a run nor a spawning call must hold its transcript")
	}
}

// A HOLD IS A CONDITION, NOT AN EVENT: the rescan that re-enters it must say
// something new — the running count — at a level a reader is not drowned by.
func TestARepeatedHoldIsRestatedVerboselyWithItsCount(t *testing.T) {
	// Arrange.
	d, _, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")

	// Act: the first scan holds it, two more re-enter the same hold.
	d.Scan()
	d.Scan()
	d.Scan()

	// Assert.
	records := opsAt(parseLogLines(t, *logs), "discover-meta", "debug")
	if len(records) != 2 {
		t.Fatalf("repeat records = %d, want one per re-entered hold: %v", len(records), records)
	}
	if got := records[1].Context["repeat_count"]; got != float64(2) {
		t.Fatalf("repeat_count = %v, want 2", got)
	}
	if got := ctxString(t, records[1], "reason"); got != holdMetaAbsent {
		t.Fatalf("reason = %q, want %q", got, holdMetaAbsent)
	}
}

// A DIFFERENT REASON IS A DIFFERENT FACT and is stated again, rather than
// swallowed as a repeat of the hold it replaced.
func TestAChangedHoldReasonIsStatedAgain(t *testing.T) {
	// Arrange: held first for an absent meta.
	d, base, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	d.Scan()

	// Act: the meta appears and is unreadable, which is a different condition.
	writeRaw(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"),
		[]byte("not json at all"))
	d.Scan()

	// Assert: the change itself is a warning, and the new reason is stated at
	// its own level.
	records := parseLogLines(t, *logs)
	warns := opsAt(records, "discover-meta", "warn")
	if len(warns) != 2 {
		t.Fatalf("warn records = %d, want the first hold plus the reason change: %v", len(warns), operationLevels(records))
	}
	if got := ctxString(t, warns[1], "reason"); got != holdMetaUnreadable {
		t.Fatalf("reason = %q, want %q", got, holdMetaUnreadable)
	}
	requireOnceIn(t, records, "discover-meta", "error")
}

// A RELEASE IS A LIFECYCLE EDGE: it is stated once, at info, and carries how
// long the hold had been repeating.
func TestAReleasedHoldIsStatedAtInfoWithItsRepeatCount(t *testing.T) {
	// Arrange.
	d, base, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	d.Scan()
	d.Scan()

	// Act.
	write(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"))
	d.Scan()

	// Assert.
	records := parseLogLines(t, *logs)
	released := requireOnceIn(t, records, "discover-meta", "info")
	if got := released.Context["repeat_count"]; got != float64(1) {
		t.Fatalf("repeat_count = %v, want the one repeat the hold accumulated", got)
	}
	// A released transcript stops being held, so a later scan repeats nothing.
	d.Scan()
	if got := opsAt(parseLogLines(t, *logs), "discover-meta", "info"); len(got) != 1 {
		t.Fatalf("release records = %d, want exactly one", len(got))
	}
}

// The STANDING set is what a reader needs when nothing changes, and one
// periodic record carries it without a record per file per pass.
func TestTheStandingHoldSetIsSummarizedPeriodically(t *testing.T) {
	// Arrange.
	d, _, _, logs := fixture(t,
		"config-a/projects/proj/sess-1/subagents/agent-abc.jsonl",
		"config-a/projects/proj/sess-1/subagents/agent-def.jsonl")
	clock := time.Date(2026, 9, 11, 12, 0, 0, 0, time.UTC)
	d.now = func() time.Time { return clock }

	// Act: the first pass summarizes, the second is inside the interval, the
	// third is past it.
	d.Scan()
	d.Scan()
	clock = clock.Add(DefaultHoldSummaryInterval)
	d.Scan()

	// Assert.
	records := opsAt(parseLogLines(t, *logs), "discover-holds", "info")
	if len(records) != 2 {
		t.Fatalf("hold summaries = %d, want one per elapsed interval: %v", len(records), records)
	}
	if !strings.Contains(records[0].Message, "2 transcript(s) held") {
		t.Fatalf("summary message = %q, want the held count", records[0].Message)
	}
}

// Nothing held is nothing to say.
func TestNoHoldsMeansNoSummary(t *testing.T) {
	// Arrange.
	d, base, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	write(t, filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json"))

	// Act.
	d.Scan()

	// Assert.
	if got := opsAt(parseLogLines(t, *logs), "discover-holds", ""); len(got) != 0 {
		t.Fatalf("hold summaries with nothing held = %d, want none", len(got))
	}
}

// TestTheRootAccessorsReportTheGlobbedSpelling asserts each accessor answers
// the SYMLINK-RESOLVED root, not the spelling the caller handed New.
//
// The accessors exist so the boot record can name what discovery actually
// globs. A caller may pass `/tmp`, and the sidecar then globs `/private/tmp`
// and stamps every discovered `path` that way; an accessor echoing the caller's
// spelling would put a root in the boot record that prefixes none of the paths
// logged under it.
func TestTheRootAccessorsReportTheGlobbedSpelling(t *testing.T) {
	// Arrange: a link whose target is the real root, so the two spellings
	// genuinely differ on every platform rather than only on macOS.
	base := t.TempDir()
	real := filepath.Join(base, "real")
	link := filepath.Join(base, "link")
	if err := os.MkdirAll(real, 0o755); err != nil {
		t.Fatalf("creating %s: %v", real, err)
	}
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("linking %s -> %s: %v", link, real, err)
	}
	want := Normalize(real)
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "discover-test"})

	for _, tc := range []struct {
		name string
		got  func(*Discoverer) string
	}{
		{name: "config root", got: func(d *Discoverer) string { return d.ConfigRoots()[0] }},
		{name: "spool root", got: func(d *Discoverer) string { return d.SpoolRoot() }},
	} {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			// Act: the link spelling is what the caller passes for BOTH roots.
			d := New([]string{link}, link, log)

			// Assert.
			if got := tc.got(d); got != want {
				t.Fatalf("%s = %q, want the resolved spelling %q (the caller passed %q)", tc.name, got, want, link)
			}
		})
	}
}

// TestTransientReadFailureNamesOnlyTheErrnosARetryAnswers is the whole of the
// deferred/unreadable decision, so it is tested as the table it is.
//
// THE ERRNO IS THE SUBJECT, NEVER THE MESSAGE TEXT: "too many open files in
// system" is one platform's rendering of ENFILE and is not a contract. Each
// case wraps the errno the way ReadMeta wraps it, because that wrapping is what
// the classifier has to see through.
func TestTransientReadFailureNamesOnlyTheErrnosARetryAnswers(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{name: "ENFILE, the system had no descriptor", err: syscall.ENFILE, want: true},
		{name: "EMFILE, this process had no descriptor", err: syscall.EMFILE, want: true},
		{name: "EINTR, the call was interrupted", err: syscall.EINTR, want: true},
		{name: "EAGAIN, momentarily unavailable", err: syscall.EAGAIN, want: true},
		{name: "EACCES, a permission this process does not have", err: syscall.EACCES, want: false},
		{name: "ENOENT, the file is not there", err: syscall.ENOENT, want: false},
		{name: "a parse failure, which no retry answers", err: errors.New("parsing agent meta: invalid character"), want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: wrapped exactly as ReadMeta wraps what os.ReadFile returns.
			wrapped := fmt.Errorf("reading agent meta %s: %w", "/tmp/agent-x.meta.json", tc.err)

			// Act.
			got := transientReadFailure(wrapped)

			// Assert.
			if got != tc.want {
				t.Fatalf("transientReadFailure(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}

// TestAMetaThatWillNeverBecomeReadableIsStillAnError guards the fix from
// swallowing the class it was carved out of. A meta this process may not read is
// not a read a retry answers, and it keeps the loud record.
func TestAMetaThatWillNeverBecomeReadableIsStillAnError(t *testing.T) {
	// Arrange.
	d, base, _, logs := fixture(t, "config-a/projects/proj/sess-1/subagents/agent-abc.jsonl")
	meta := filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-abc.meta.json")
	writeRaw(t, meta, []byte(`{"toolUseId":"toolu_abc"}`))
	if err := os.Chmod(meta, 0o000); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(meta, 0o644) })
	if _, err := os.ReadFile(meta); err == nil {
		t.Skip("this filesystem or user ignores mode 000, so no permanent read failure can be arranged")
	}

	// Act.
	d.Scan()

	// Assert.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "discover-meta", "error")
	if got := ctxString(t, rec, "reason"); got != holdMetaUnreadable {
		t.Fatalf("reason = %q, want %q: a permission failure is not a deferred read", got, holdMetaUnreadable)
	}
}

// TestALinkedAgentSpoolIsDiscoveredOnceAsItsTranscript asserts the vendor's
// real a* spool shape — a LINK to the subagent's own sidechain transcript — is
// one target, the transcript, and never a second spool target reading the same
// bytes again under another path.
func TestALinkedAgentSpoolIsDiscoveredOnceAsItsTranscript(t *testing.T) {
	// Arrange.
	d, base, spool, _ := fixture(t,
		"config-a/projects/proj/sess-1/subagents/agent-a17.jsonl",
		"config-a/projects/proj/sess-1/subagents/agent-a17.meta.json",
	)
	transcript := filepath.Join(base, "config-a/projects/proj/sess-1/subagents/agent-a17.jsonl")
	tasks := filepath.Join(spool, "claude-501/proj/runtime-sess/tasks")
	if err := os.MkdirAll(tasks, 0o755); err != nil {
		t.Fatalf("creating %s: %v", tasks, err)
	}
	if err := os.Symlink(transcript, filepath.Join(tasks, "a17.output")); err != nil {
		t.Fatalf("linking the spool: %v", err)
	}

	// Act.
	targets := d.Scan()

	// Assert.
	var matches []Target
	for _, target := range targets {
		if strings.HasSuffix(target.Path, "agent-a17.jsonl") || strings.HasSuffix(target.Path, "a17.output") {
			matches = append(matches, target)
		}
	}
	if len(matches) != 1 {
		t.Fatalf("the linked spool and its transcript were discovered as %d targets, want one: %+v", len(matches), matches)
	}
	if matches[0].Path != Normalize(transcript) || matches[0].SessionID == "" {
		t.Fatalf("target = %+v, want the transcript under its config-root path", matches[0])
	}
}
