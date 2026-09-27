//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// THE SWEEP'S TWO EDGES, driven from bin/realtest.sh.
//
// `TestBetweenSweepsGapScan` runs BEFORE the first realtest and reads the gap
// since the previous sweep ended. `TestBetweenSweepsMarkTheSweepEnd` runs
// after the last one and records where every source stands, which is the next
// sweep's window start.
//
// NEITHER IS A REALTEST, and neither may be named like one: bin/test-realtest.sh
// asserts that every `func TestRealtest*` here has a row in the runner's world
// table, and docs/REALTEST-PLAN.md is the contract for which realtests exist.
// These drive no editor and press no key — the one thing they ask of Emacs is
// a read-only probe for two buffers, and an editor that is not answering is a
// note in the report rather than a failure.

// gapScanEnv puts the between-sweeps scan in gear. bin/realtest.sh sets it
// once, before the first realtest.
const gapScanEnv = "AGENT_REPL_REALTEST_GAP_SCAN"

// sweepMarkEnv asks for the high-water mark to be written. bin/realtest.sh
// sets it at the end of the sweep, however the sweep ended.
const sweepMarkEnv = "AGENT_REPL_REALTEST_SWEEP_MARK"

// gapScanFindingName is the ONE name this finding is reported under, wherever
// it is reported.
const gapScanFindingName = "BETWEEN-SWEEPS FINDINGS"

func TestBetweenSweepsGapScan(t *testing.T) {
	if os.Getenv(gapScanEnv) != "1" {
		t.Skipf("the between-sweeps scan runs only from bin/realtest.sh, which sets %s=1", gapScanEnv)
	}
	ctx := context.Background()
	home, stateDir, runDir := sweepEdgePaths(t)
	workspaces := sweepEdgeWorkspaces(ctx, t, stateDir)

	sources, err := EnumerateSources(RealEnv(home, workspaces))
	if err != nil {
		t.Fatalf("enumerate the logs to scan: %v", err)
	}

	scan := GapScan{To: time.Now()}
	markPath := SweepMarkPath(stateDir)
	mark, err := ReadSweepMark(markPath)
	switch {
	case err == nil:
		scan.From, scan.FromSource = mark.EndedAt, fmt.Sprintf("the previous sweep's mark (%s)", markPath)
	default:
		// The mark is new and the run directories are not, so the first sweep
		// after this lands has none. Falling back to the newest manifest reads
		// the gap since the last sweep anyone ran, which is the question.
		scan.Notes = append(scan.Notes, fmt.Sprintf("no usable sweep mark at %s (%v)", markPath, err))
		newest, ok := NewestManifestTime(filepath.Join(stateDir, "realtest"))
		if !ok {
			t.Logf("no sweep mark and no previous MANIFEST.md under %s: there is no previous sweep to "+
				"read the gap since, so nothing is scanned. The mark this sweep writes at its end is what "+
				"the next one will use.", filepath.Join(stateDir, "realtest"))
			return
		}
		scan.From = newest
		scan.FromSource = "the newest MANIFEST.md, because no sweep mark was readable"
	}

	harvest, err := HarvestSources(sources, mark.Snapshot, Window{Start: scan.From, End: scan.To}, workspaces)
	if err != nil {
		t.Fatalf("read the logs between the sweeps: %v", err)
	}
	scan.Findings = append(scan.Findings, harvest.Findings...)
	scan.Notes = append(scan.Notes, fmt.Sprintf("%d log source(s) enumerated, %d of them carrying bytes in "+
		"this window", len(sources), len(harvest.SourcesRead)))

	// The editor's own two buffers. It is READ-ONLY and it is optional: an
	// editor that is not answering is a fact about the gap, not a failure of
	// the scan, and the note says so rather than the run stopping.
	client := sweepEdgeClient(t, runDir)
	if client.Alive(ctx) {
		scan.Findings = append(scan.Findings, sweepEdgeBufferFindings(ctx, t, client, mark, workspaces, &scan.Notes)...)
	} else {
		scan.Notes = append(scan.Notes,
			"no Emacs was answering, so *Messages* and *Warnings* were not read for this window")
	}

	path, err := WriteGapScanManifest(runDir, scan)
	if err != nil {
		t.Fatalf("write the between-sweeps manifest: %v", err)
	}
	t.Logf("the between-sweeps window %s to %s is reported in %s",
		scan.From.Format(time.RFC3339), scan.To.Format(time.RFC3339), path)

	if scan.Count() > 0 {
		t.Errorf("%s: %d finding(s) were written between the previous sweep and this one, when no realtest "+
			"was running and the editor was the owner's. They are held to the same bar as a finding inside a "+
			"run window — there is no allowlist — and they are in %s, verbatim.",
			gapScanFindingName, scan.Count(), path)
	}
}

func TestBetweenSweepsMarkTheSweepEnd(t *testing.T) {
	if os.Getenv(sweepMarkEnv) != "1" {
		t.Skipf("the sweep mark is written only by bin/realtest.sh, which sets %s=1", sweepMarkEnv)
	}
	ctx := context.Background()
	home, stateDir, runDir := sweepEdgePaths(t)
	workspaces := sweepEdgeWorkspaces(ctx, t, stateDir)

	sources, err := EnumerateSources(RealEnv(home, workspaces))
	if err != nil {
		t.Fatalf("enumerate the logs to mark: %v", err)
	}

	// A buffer that could not be read is recorded as -1 rather than as 0: a
	// zero would tell the next sweep the buffer was EMPTY here, and it would
	// then read the whole thing and report every line the owner has already
	// seen. Negative says "unknown", and the next scan reads the buffer whole
	// for a reason it can state.
	mark := SweepMark{
		EndedAt:      time.Now(),
		Snapshot:     TakeSnapshot(sources),
		MessagesSize: -1,
		WarningsSize: -1,
	}
	client := sweepEdgeClient(t, runDir)
	if client.Alive(ctx) {
		if size, err := client.MessagesSize(ctx); err == nil {
			mark.MessagesSize = size
		} else {
			t.Logf("the *Messages* buffer size could not be read for the mark: %v", err)
		}
		if size, err := client.WarningsSize(ctx); err == nil {
			mark.WarningsSize = size
		} else {
			t.Logf("the *Warnings* buffer size could not be read for the mark: %v", err)
		}
	} else {
		t.Logf("no Emacs was answering, so the mark records both buffer sizes as unknown")
	}

	path := SweepMarkPath(stateDir)
	if err := WriteSweepMark(path, mark); err != nil {
		t.Fatalf("write the sweep mark: %v", err)
	}
	t.Logf("the sweep mark at %s now stands at %s over %d source(s); the next sweep reads the gap from there",
		path, mark.EndedAt.Format(time.RFC3339Nano), len(mark.Snapshot.Sizes))
}

// sweepEdgePaths answers the three roots both edges need.
func sweepEdgePaths(t *testing.T) (home, stateDir, runDir string) {
	t.Helper()
	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}
	stateDir = filepath.Join(home, ".claude-emacs")
	runDir = os.Getenv(outEnv)
	if runDir == "" {
		t.Fatalf("%s is not set; bin/realtest.sh exports the run directory and this writes into it", outEnv)
	}
	return home, stateDir, runDir
}

// sweepEdgeWorkspaces reads every workspace the registry holds, OPEN AND
// CLOSED.
//
// Closed ones are included here where realtest 1 excludes them, and the
// difference is what each is for: realtest 1 asserts that every open workspace
// drew a tab, while this reads logs — and a workspace closed between sweeps
// wrote records right up to the close, in a sink nothing else would enumerate.
func sweepEdgeWorkspaces(ctx context.Context, t *testing.T, stateDir string) []Workspace {
	t.Helper()
	open, closed, err := ReadWorkspaces(ctx, StateDBPath(stateDir))
	if err != nil {
		t.Fatalf("read the workspaces the state database holds: %v", err)
	}
	return append(append([]Workspace{}, open...), closed...)
}

// sweepEdgeClient is the read-only probe both edges use. The scratch directory
// is the run directory, the same one every other probe in this package writes
// its answers into.
func sweepEdgeClient(t *testing.T, runDir string) *Client {
	t.Helper()
	socket := os.Getenv(socketEnv)
	if socket == "" {
		socket = filepath.Join(os.TempDir(), fmt.Sprintf("emacs%d", os.Getuid()), "server")
	}
	return &Client{Socket: socket, Scratch: runDir}
}

// sweepEdgeBufferFindings reads *Messages* and *Warnings* from the tail the
// mark recorded and returns everything found in them.
//
// A buffer the mark could not size (-1) is read WHOLE, and the note says so:
// reading it from an offset nobody recorded would silently skip lines, and
// this scan's whole reason for existing is lines nobody looked at.
func sweepEdgeBufferFindings(ctx context.Context, t *testing.T, client *Client, mark SweepMark,
	workspaces []Workspace, notes *[]string) []Finding {
	t.Helper()
	var findings []Finding

	if text, err := client.Messages(ctx); err != nil {
		*notes = append(*notes, fmt.Sprintf("*Messages* could not be read: %v", err))
	} else {
		from := sweepEdgeTail(mark.MessagesSize, len(text), "*Messages*", notes)
		findings = append(findings, HarvestMessages(text, from, workspaces)...)
	}

	if text, err := client.Warnings(ctx); err != nil {
		*notes = append(*notes, fmt.Sprintf("*Warnings* could not be read: %v", err))
	} else {
		from := sweepEdgeTail(mark.WarningsSize, len(text), "*Warnings*", notes)
		findings = append(findings, HarvestWarningsBuffer(text, from, workspaces)...)
	}
	return findings
}

// sweepEdgeTail decides where in a buffer this window starts, and states every
// case it cannot answer exactly.
func sweepEdgeTail(marked, now int, buffer string, notes *[]string) int {
	switch {
	case marked < 0:
		*notes = append(*notes, fmt.Sprintf(
			"%s was read WHOLE: the previous sweep could not record its size, so where this window begins "+
				"in it is unknown and reading from a guess would skip lines", buffer))
		return 0
	case marked > now:
		*notes = append(*notes, fmt.Sprintf(
			"%s is shorter than the previous sweep recorded (%d, now %d), so Emacs restarted or the buffer "+
				"was cleared and every line in it is new", buffer, marked, now))
		return 0
	default:
		return marked
	}
}
