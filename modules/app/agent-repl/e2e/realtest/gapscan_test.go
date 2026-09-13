//go:build realtest

package realtest

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// These touch no real path and start no process: a mark is written into
// t.TempDir(), a buffer is a string, and a manifest is read back off disk.

// TestSweepMarkRoundTripsThroughDisk is the ordinary case: what one sweep
// records is what the next one reads, including the snapshot the whole
// windowing depends on.
func TestSweepMarkRoundTripsThroughDisk(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "realtest", "last-sweep-end")
	ended := time.Now().Round(0)
	want := SweepMark{
		EndedAt:      ended,
		Snapshot:     Snapshot{Sizes: map[string]int64{"16777232:99": 4096}, Resolved: map[string]string{"/a": "/b"}},
		MessagesSize: 1234,
		WarningsSize: 7,
	}

	// Act.
	if err := WriteSweepMark(path, want); err != nil {
		t.Fatalf("write the sweep mark: %v", err)
	}
	got, err := ReadSweepMark(path)

	// Assert.
	if err != nil {
		t.Fatalf("read the sweep mark back: %v", err)
	}
	if !got.EndedAt.Equal(want.EndedAt) {
		t.Errorf("the mark came back with end time %s, not %s", got.EndedAt, want.EndedAt)
	}
	if got.Snapshot.Sizes["16777232:99"] != 4096 {
		t.Errorf("the snapshot did not survive the round trip: %+v", got.Snapshot)
	}
	if got.MessagesSize != 1234 || got.WarningsSize != 7 {
		t.Errorf("the buffer sizes came back as %d and %d", got.MessagesSize, got.WarningsSize)
	}
}

// TestReadSweepMarkReportsAnAbsentMarkAsNotExist is the first-sweep edge: the
// caller has to be able to tell "there is no previous sweep" from "the
// previous sweep ended at the zero time", because the second would make the
// window the whole of recorded history.
func TestReadSweepMarkReportsAnAbsentMarkAsNotExist(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "last-sweep-end")

	// Act.
	_, err := ReadSweepMark(path)

	// Assert.
	if !os.IsNotExist(err) {
		t.Fatalf("an absent mark must be reported as os.ErrNotExist, got %v", err)
	}
}

// TestReadSweepMarkRefusesAMarkWithNoEndTime is the corrupt-mark edge: a
// decoded mark carrying the zero time bounds nothing, and accepting it would
// silently scan from the beginning of time.
func TestReadSweepMarkRefusesAMarkWithNoEndTime(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "last-sweep-end")
	body, err := json.Marshal(SweepMark{MessagesSize: 3})
	if err != nil {
		t.Fatalf("render the fixture mark: %v", err)
	}
	if err := os.WriteFile(path, body, 0o644); err != nil {
		t.Fatalf("write the fixture mark: %v", err)
	}

	// Act.
	_, err = ReadSweepMark(path)

	// Assert.
	if err == nil {
		t.Fatalf("a mark with no end time was accepted, so the window would start at the zero time")
	}
}

// TestNewestManifestTimeAnswersTheNewestOne is the fallback's own case: with
// no mark, the window starts at the last sweep that actually wrote a report.
func TestNewestManifestTimeAnswersTheNewestOne(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	older := filepath.Join(root, "realtest-1", "MANIFEST.md")
	newer := filepath.Join(root, "realtest-2", "between-sweeps", "MANIFEST.md")
	for _, path := range []string{older, newer} {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatalf("build the fixture tree: %v", err)
		}
		if err := os.WriteFile(path, []byte("# a manifest\n"), 0o644); err != nil {
			t.Fatalf("write %s: %v", path, err)
		}
	}
	want := time.Now().Add(-time.Hour)
	if err := os.Chtimes(older, want.Add(-time.Hour), want.Add(-time.Hour)); err != nil {
		t.Fatalf("age the older manifest: %v", err)
	}
	if err := os.Chtimes(newer, want, want); err != nil {
		t.Fatalf("age the newer manifest: %v", err)
	}

	// Act.
	got, ok := NewestManifestTime(root)

	// Assert.
	if !ok {
		t.Fatalf("no manifest was found under %s", root)
	}
	if !got.Equal(want) {
		t.Fatalf("the newest manifest time is %s, not %s", got, want)
	}
}

// TestNewestManifestTimeAnswersNothingWhenThereIsNoManifest is the very first
// run on a machine: there is no previous sweep, and the scan must say so
// rather than invent a window.
func TestNewestManifestTimeAnswersNothingWhenThereIsNoManifest(t *testing.T) {
	// Arrange.
	root := t.TempDir()

	// Act.
	_, ok := NewestManifestTime(root)

	// Assert.
	if ok {
		t.Fatalf("a tree with no MANIFEST.md in it reported a window start")
	}
}

// TestHarvestWarningsBufferMakesEveryLineAFinding is the *Warnings* contract:
// no pattern set and no allowlist, because a line is in that buffer only
// because `display-warning` put it there.
func TestHarvestWarningsBufferMakesEveryLineAFinding(t *testing.T) {
	// Arrange.
	text := "Warning (agent-repl): workspace \"ws-a\" cannot host a durable log sink\n" +
		"something nobody wrote a pattern for\n"

	// Act.
	findings := HarvestWarningsBuffer(text, 0, nil)

	// Assert.
	if len(findings) != 2 {
		t.Fatalf("expected both lines to be findings, got %d: %+v", len(findings), findings)
	}
	if findings[0].Source != warningsBufferSource {
		t.Errorf("the finding is reported under %q, not %q", findings[0].Source, warningsBufferSource)
	}
}

// TestHarvestWarningsBufferReadsOnlyTheTailAfterTheMark is what stops a
// standing buffer from being re-reported every sweep forever.
func TestHarvestWarningsBufferReadsOnlyTheTailAfterTheMark(t *testing.T) {
	// Arrange.
	old := "Warning (agent-repl): the previous sweep already reported this\n"
	text := old + "Warning (agent-repl): this one is new\n"

	// Act.
	findings := HarvestWarningsBuffer(text, len(old), nil)

	// Assert.
	if len(findings) != 1 {
		t.Fatalf("expected only the line added after the mark, got %d: %+v", len(findings), findings)
	}
	if !strings.Contains(findings[0].Raw, "this one is new") {
		t.Fatalf("the wrong line was reported: %q", findings[0].Raw)
	}
}

// TestHarvestWarningsBufferReadsTheWholeBufferWhenItShrank is the restart
// edge: a buffer shorter than the mark recorded belongs to a new Emacs, so
// every line in it is new. The same rule the inode snapshot applies to a file.
func TestHarvestWarningsBufferReadsTheWholeBufferWhenItShrank(t *testing.T) {
	// Arrange.
	text := "Warning (agent-repl): the editor restarted and this is its first warning\n"

	// Act: a mark far past the end of the buffer.
	findings := HarvestWarningsBuffer(text, 10_000, nil)

	// Assert.
	if len(findings) != 1 {
		t.Fatalf("a buffer shorter than the mark must be read whole, got %d findings", len(findings))
	}
}

// TestHarvestWarningsBufferAttributesALineToItsWorkspace is the attribution
// edge: the module's own warnings carry a `ws=<name>` token, and a finding
// filed under `global` sends the reader to the wrong place.
func TestHarvestWarningsBufferAttributesALineToItsWorkspace(t *testing.T) {
	// Arrange.
	workspaces := []Workspace{{ID: "ws-id-1", Name: "workspace-c22fed99"}}
	text := "Warning (agent-repl): ws=workspace-c22fed99 cannot host a durable log sink\n"

	// Act.
	findings := HarvestWarningsBuffer(text, 0, workspaces)

	// Assert.
	if len(findings) != 1 {
		t.Fatalf("expected one finding, got %d", len(findings))
	}
	if findings[0].Workspace != "ws-id-1" {
		t.Fatalf("the finding is attributed to %q, not to the workspace the line names", findings[0].Workspace)
	}
}

// TestGapScanRenderCarriesTheBetweenSweepsHeading is the contract the sweep's
// output and the docs both point at: the section is named "## Between
// sweeps", and a reader who is told to look there finds it.
func TestGapScanRenderCarriesTheBetweenSweepsHeading(t *testing.T) {
	// Arrange.
	scan := GapScan{From: time.Now().Add(-time.Hour), To: time.Now(), FromSource: "the previous sweep's mark"}

	// Act.
	text := scan.Render()

	// Assert.
	if !strings.Contains(text, "## Between sweeps") {
		t.Fatalf("the report has no `## Between sweeps` section:\n%s", text)
	}
}

// TestGapScanRenderReproducesAFindingVerbatim is the evidence rule: the owner
// rules on the raw line, and a paraphrase is not evidence.
func TestGapScanRenderReproducesAFindingVerbatim(t *testing.T) {
	// Arrange.
	raw := `{"timestamp":"2026-09-13T11:22:33Z","level":"warn","message":"cannot host a durable log sink"}`
	scan := GapScan{
		From: time.Now().Add(-time.Hour), To: time.Now(), FromSource: "the previous sweep's mark",
		Findings: []Finding{{Kind: KindRecord, Source: "emacs.global", Path: "/log", Line: 4, Level: "warn", Raw: raw}},
	}

	// Act.
	text := scan.Render()

	// Assert.
	if !strings.Contains(text, raw) {
		t.Fatalf("the finding's own line is not in the report:\n%s", text)
	}
}

// TestWriteGapScanManifestKeepsItsOwnSubdirectory is the collision edge:
// realtest 1 writes MANIFEST.md to the run directory itself, so a gap scan
// writing there too would have its report overwritten by the first realtest
// that ran.
func TestWriteGapScanManifestKeepsItsOwnSubdirectory(t *testing.T) {
	// Arrange.
	runDir := t.TempDir()
	scan := GapScan{From: time.Now().Add(-time.Hour), To: time.Now(), FromSource: "the previous sweep's mark"}

	// Act.
	path, err := WriteGapScanManifest(runDir, scan)

	// Assert.
	if err != nil {
		t.Fatalf("write the between-sweeps manifest: %v", err)
	}
	want := filepath.Join(runDir, betweenSweepsDirName, "MANIFEST.md")
	if path != want {
		t.Fatalf("the manifest went to %s, not to %s, where nothing else in a run writes", path, want)
	}
	if _, err := os.Stat(filepath.Join(runDir, betweenSweepsDirName, fullHarvestName)); err != nil {
		t.Fatalf("the full harvest was not written beside the manifest: %v", err)
	}
}
