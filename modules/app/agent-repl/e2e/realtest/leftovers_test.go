//go:build realtest

package realtest

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// These start no daemon and no editor. The registry is a scratch database
// built by state_test.go's own fixture helpers, and the command-file ingress
// is a directory nobody sweeps — which is exactly the world a leftover clean
// meets when the daemon is gone, and the world the survivor path exists for.

// leftoverFixture is a registry holding one open and one closed workspace
// under `run`, plus one workspace belonging to the owner that no run may
// touch. It answers the database path and the run directory.
func leftoverFixture(t *testing.T) (dbPath, runDir string) {
	t.Helper()
	snapshotsUnder(t)

	stateDir := t.TempDir()
	runDir = filepath.Join(stateDir, "realtest", "realtest-20260913-111010")
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		t.Fatalf("create the fixture run directory: %v", err)
	}
	dbPath = filepath.Join(stateDir, "wsm.db")

	db := openStateFixture(t, dbPath)
	stmts := []string{
		`CREATE TABLE workspaces (id TEXT PRIMARY KEY, dir TEXT, name TEXT, closed INTEGER);`,
		`INSERT INTO workspaces VALUES ('ws-open', '` + runDir + `/scratch-repo-worktrees/ws-open', 'rt-open', 0);`,
		`INSERT INTO workspaces VALUES ('ws-closed', '` + runDir + `/scratch-repo-worktrees/ws-closed', 'rt-closed', 1);`,
		`INSERT INTO workspaces VALUES ('ws-owner', '/Users/owner/repos/real', 'owner', 0);`,
	}
	for _, stmt := range stmts {
		if _, err := db.Exec(stmt); err != nil {
			t.Fatalf("seed the leftover fixture with %q: %v", stmt, err)
		}
	}
	return dbPath, runDir
}

// TestUnderDirMatchesADirectoryBeneathThePrefix is the ordinary case: a
// workspace worktree inside the run directory is a leftover of that run.
func TestUnderDirMatchesADirectoryBeneathThePrefix(t *testing.T) {
	// Arrange.
	prefix := "/state/realtest/realtest-20260913"
	dir := prefix + "/scratch-repo-worktrees/workspace-abc"

	// Act.
	got := UnderDir(dir, prefix)

	// Assert.
	if !got {
		t.Fatalf("UnderDir(%q, %q) = false, but the directory is inside the run directory", dir, prefix)
	}
}

// TestUnderDirMatchesThePrefixItself is the edge case "the row names the run
// directory itself": a workspace registered ON the run directory is still this
// run's residue.
func TestUnderDirMatchesThePrefixItself(t *testing.T) {
	// Arrange.
	prefix := "/state/realtest/realtest-20260913"

	// Act.
	got := UnderDir(prefix, prefix)

	// Assert.
	if !got {
		t.Fatalf("UnderDir(%q, %q) = false, but a row naming the run directory itself is this run's", prefix, prefix)
	}
}

// TestUnderDirRejectsASiblingSharingThePrefixText is the edge case a plain
// string prefix gets wrong: `realtest-20260913-2` is a DIFFERENT run's
// directory, and cleaning it would forget rows this run never created.
func TestUnderDirRejectsASiblingSharingThePrefixText(t *testing.T) {
	// Arrange.
	prefix := "/state/realtest/realtest-20260913"
	sibling := "/state/realtest/realtest-20260913-2/worktrees/ws"

	// Act.
	got := UnderDir(sibling, prefix)

	// Assert.
	if got {
		t.Fatalf("UnderDir(%q, %q) = true; a sibling directory that merely shares the prefix TEXT is "+
			"another run's, and forgetting its rows would destroy records this run never created", sibling, prefix)
	}
}

// TestUnderDirRejectsAnUnrelatedDirectory is the owner's own repository: never
// a leftover, whatever else is true.
func TestUnderDirRejectsAnUnrelatedDirectory(t *testing.T) {
	// Arrange.
	prefix := "/state/realtest/realtest-20260913"
	owner := "/Users/owner/repos/real"

	// Act.
	got := UnderDir(owner, prefix)

	// Assert.
	if got {
		t.Fatalf("UnderDir(%q, %q) = true, which would put the owner's own repository in a clean", owner, prefix)
	}
}

// TestUnderDirRejectsAnEmptyDirectory is the edge case "the row names nothing":
// an empty directory matches no prefix, rather than matching every one.
func TestUnderDirRejectsAnEmptyDirectory(t *testing.T) {
	// Arrange.
	prefix := "/state/realtest/realtest-20260913"

	// Act.
	got := UnderDir("", prefix)

	// Assert.
	if got {
		t.Fatalf("UnderDir(%q, %q) = true; a row naming no directory is not under anything", "", prefix)
	}
}

// TestUnderDirMatchesAMissingDirectoryUnderASymlinkedPrefix is the edge case
// this whole file is about, and the one the first implementation got wrong: a
// leftover row names a directory that is GONE, while the run directory above
// it still stands under a symlink. Canonicalizing only the side that exists
// made the two spellings of one directory compare unequal, and the leftover
// was then reported as somebody else's and left in the registry.
func TestUnderDirMatchesAMissingDirectoryUnderASymlinkedPrefix(t *testing.T) {
	// Arrange: a real directory, and a symlink standing in for the spelling
	// the registry happens to hold.
	real := t.TempDir()
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("build the symlinked prefix: %v", err)
	}
	gone := filepath.Join(link, "worktrees", "workspace-c22fed997b234b27")

	// Act.
	got := UnderDir(gone, real)

	// Assert.
	if !got {
		t.Fatalf("UnderDir(%q, %q) = false; the row names a directory that no longer exists under a prefix "+
			"reached through a symlink, which is exactly what a realtest leftover looks like", gone, real)
	}
}

// TestLeftoverWorkspacesFindsOpenAndClosedRowsUnderTheRunDirectory is the
// central claim: a closed row counts. The owner's stale-registration warning
// fires on a closed row exactly as it does on an open one, and a clean that
// ignored them would leave the very row the 2026-09-13 complaint was about.
func TestLeftoverWorkspacesFindsOpenAndClosedRowsUnderTheRunDirectory(t *testing.T) {
	// Arrange.
	dbPath, runDir := leftoverFixture(t)

	// Act.
	rows, err := LeftoverWorkspaces(context.Background(), dbPath, runDir)

	// Assert.
	if err != nil {
		t.Fatalf("read the leftovers: %v", err)
	}
	if len(rows) != 2 {
		t.Fatalf("expected the open row and the closed row, got %d: %+v", len(rows), rows)
	}
	if rows[0].ID != "ws-closed" || !rows[0].Closed {
		t.Errorf("expected the closed row to be reported as closed, got %+v", rows[0])
	}
	if rows[1].ID != "ws-open" || rows[1].Closed {
		t.Errorf("expected the open row to be reported as open, got %+v", rows[1])
	}
}

// TestLeftoverWorkspacesLeavesTheOwnersOwnRowsAlone is the safety edge: a
// registry row outside the run directory is never a leftover.
func TestLeftoverWorkspacesLeavesTheOwnersOwnRowsAlone(t *testing.T) {
	// Arrange.
	dbPath, runDir := leftoverFixture(t)

	// Act.
	rows, err := LeftoverWorkspaces(context.Background(), dbPath, runDir)

	// Assert.
	if err != nil {
		t.Fatalf("read the leftovers: %v", err)
	}
	for _, row := range rows {
		if row.ID == "ws-owner" {
			t.Fatalf("the owner's own workspace %s at %s was reported as a leftover of %s",
				row.ID, row.Dir, runDir)
		}
	}
}

// TestDescribeLeftoversNamesEveryRowAndItsDirectory is the reporting edge: a
// refusal the operator cannot act on is not a refusal, so every row's id and
// directory must appear in the text.
func TestDescribeLeftoversNamesEveryRowAndItsDirectory(t *testing.T) {
	// Arrange.
	rows := []LeftoverRow{
		{Workspace: Workspace{ID: "ws-1", Dir: "/state/realtest/run/ws-1", Name: "one"}, Closed: true},
	}

	// Act.
	text := DescribeLeftovers("/state/realtest/run", rows)

	// Assert.
	for _, want := range []string{"ws-1", "/state/realtest/run/ws-1", "closed", "MISSING"} {
		if !strings.Contains(text, want) {
			t.Errorf("the description does not name %q:\n%s", want, text)
		}
	}
}

// TestWriteWorkspaceCommandNamesTheVerbAndTheWorkspace is the command-file
// edge: the entry the daemon reads must be exactly one, of the verb asked for,
// naming the workspace asked for.
func TestWriteWorkspaceCommandNamesTheVerbAndTheWorkspace(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()

	// Act.
	path, err := WriteWorkspaceCommand(stateDir, "close", "ws-123")

	// Assert.
	if err != nil {
		t.Fatalf("write the close command file: %v", err)
	}
	var entries []struct {
		Type      string `json:"type"`
		Workspace string `json:"workspace"`
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the command file back: %v", err)
	}
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("the command file %s did not decode as JSON: %v", path, err)
	}
	if len(entries) != 1 || entries[0].Type != "close" || entries[0].Workspace != "ws-123" {
		t.Fatalf("expected one close entry naming ws-123, got %+v", entries)
	}
}

// TestWriteWorkspaceCommandMatchesTheDaemonsOwnGlob is its own edge case
// because getting it wrong is silent: a file the glob does not claim is never
// read, and the clean would report a daemon that refused when nothing was ever
// asked.
func TestWriteWorkspaceCommandMatchesTheDaemonsOwnGlob(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()

	// Act.
	path, err := WriteWorkspaceCommand(stateDir, "forget", "ws-123")

	// Assert.
	if err != nil {
		t.Fatalf("write the forget command file: %v", err)
	}
	matched, err := filepath.Match("workspace_commands_*.json", filepath.Base(path))
	if err != nil {
		t.Fatalf("match the daemon's glob: %v", err)
	}
	if !matched {
		t.Fatalf("%s does not match the daemon's own glob workspace_commands_*.json "+
			"(daemon/internal/stateroot/stateroot.go CommandFileGlob), so nothing would ever read it",
			filepath.Base(path))
	}
}

// TestCleanLeftoversReportsWhatSurvivedWhenNoDaemonSweepsTheIngress is the
// failure edge, and the one the sweep's verdict rests on: with nothing reading
// the command files, every row is still there afterwards and the caller is
// told so rather than being told the clean succeeded.
func TestCleanLeftoversReportsWhatSurvivedWhenNoDaemonSweepsTheIngress(t *testing.T) {
	// Arrange.
	dbPath, runDir := leftoverFixture(t)
	stateDir := filepath.Dir(dbPath)
	shrinkLeftoverWaits(t)

	// Act.
	remaining, err := CleanLeftovers(context.Background(), dbPath, stateDir, runDir, nil)

	// Assert.
	if err != nil {
		t.Fatalf("clean the leftovers: %v", err)
	}
	if len(remaining) != 2 {
		t.Fatalf("no daemon read the command files, so both rows must be reported as survivors; got %d: %+v",
			len(remaining), remaining)
	}
}

// TestCleanLeftoversAsksTheDaemonToCloseAnOpenRowBeforeForgettingIt is the
// ordering edge: `Forget` refuses an open workspace (daemon/internal/workspace
// /forget.go), so a clean that asked for the forget first would be refused
// every time and would report a product defect that is really its own.
func TestCleanLeftoversAsksTheDaemonToCloseAnOpenRowBeforeForgettingIt(t *testing.T) {
	// Arrange.
	dbPath, runDir := leftoverFixture(t)
	stateDir := filepath.Dir(dbPath)
	shrinkLeftoverWaits(t)

	// Act.
	if _, err := CleanLeftovers(context.Background(), dbPath, stateDir, runDir, nil); err != nil {
		t.Fatalf("clean the leftovers: %v", err)
	}

	// Assert: the open row got a close and, since it never read back closed,
	// no forget; the closed row got a forget and no close.
	verbs := commandFileVerbs(t, filepath.Join(stateDir, "output"))
	if verbs["close ws-open"] != 1 {
		t.Errorf("expected exactly one close for the open row, got %d (all: %v)", verbs["close ws-open"], verbs)
	}
	if verbs["forget ws-open"] != 0 {
		t.Errorf("the open row never read back closed, so it must not have been forgotten: %v", verbs)
	}
	if verbs["forget ws-closed"] != 1 {
		t.Errorf("expected exactly one forget for the already-closed row, got %d (all: %v)",
			verbs["forget ws-closed"], verbs)
	}
	if verbs["close ws-closed"] != 0 {
		t.Errorf("an already-closed row must not be closed again: %v", verbs)
	}
}

// TestCleanLeftoversTouchesNoRowOutsideThePrefix is the safety edge stated as
// a test of the WRITES rather than of the read: no command file may ever name
// a workspace the run did not create.
func TestCleanLeftoversTouchesNoRowOutsideThePrefix(t *testing.T) {
	// Arrange.
	dbPath, runDir := leftoverFixture(t)
	stateDir := filepath.Dir(dbPath)
	shrinkLeftoverWaits(t)

	// Act.
	if _, err := CleanLeftovers(context.Background(), dbPath, stateDir, runDir, nil); err != nil {
		t.Fatalf("clean the leftovers: %v", err)
	}

	// Assert.
	for verb := range commandFileVerbs(t, filepath.Join(stateDir, "output")) {
		if strings.HasSuffix(verb, "ws-owner") {
			t.Fatalf("a command file named the owner's own workspace: %s", verb)
		}
	}
}

// shrinkLeftoverWaits makes the ceiling and the poll small enough that a test
// of "nobody answered" finishes in milliseconds rather than in minutes. The
// production values are restored afterwards.
func shrinkLeftoverWaits(t *testing.T) {
	t.Helper()
	ceiling, interval := leftoverCleanCeiling, leftoverPollInterval
	leftoverCleanCeiling, leftoverPollInterval = 20*time.Millisecond, 5*time.Millisecond
	t.Cleanup(func() { leftoverCleanCeiling, leftoverPollInterval = ceiling, interval })
}

// commandFileVerbs reads every command file in the ingress directory and
// counts "<verb> <workspace>" pairs.
func commandFileVerbs(t *testing.T, dir string) map[string]int {
	t.Helper()
	counts := make(map[string]int)
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return counts
		}
		t.Fatalf("list the ingress directory %s: %v", dir, err)
	}
	for _, entry := range entries {
		data, err := os.ReadFile(filepath.Join(dir, entry.Name()))
		if err != nil {
			t.Fatalf("read %s: %v", entry.Name(), err)
		}
		var parsed []struct {
			Type      string `json:"type"`
			Workspace string `json:"workspace"`
		}
		if err := json.Unmarshal(data, &parsed); err != nil {
			t.Fatalf("decode %s: %v", entry.Name(), err)
		}
		for _, one := range parsed {
			counts[one.Type+" "+one.Workspace]++
		}
	}
	return counts
}
