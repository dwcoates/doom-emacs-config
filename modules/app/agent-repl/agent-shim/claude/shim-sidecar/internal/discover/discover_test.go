package discover

import (
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

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
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("creating %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte("{}\n"), 0o644); err != nil {
		t.Fatalf("writing %s: %v", path, err)
	}
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
	if got.Kind != tail.KindAgentTranscript || got.AgentID != "abc" || got.SessionID != "sess-1" {
		t.Fatalf("target = %+v, want a subagent transcript for agent abc of sess-1", got)
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
	if got.Kind != tail.KindWorkflowJournal || got.RunID != "wf_7" || got.AgentID != "xyz" {
		t.Fatalf("target = %+v, want workflow wf_7's per-agent transcript for xyz", got)
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
		{name: "workflow journal", file: "w17.output", wantKind: tail.KindWorkflowJournal, wantRaw: false},
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
	joined := strings.Join(*logs, "\n")
	if !strings.Contains(joined, "no a/b/w kind prefix") || !strings.Contains(joined, `"level":"error"`) {
		t.Fatalf("the ingestion violation was not stated loudly; got %v", *logs)
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
	if got := strings.Count(strings.Join(*logs, "\n"), "transcript held"); got != 1 {
		t.Fatalf("held warning emitted %d times, want once", got)
	}
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
