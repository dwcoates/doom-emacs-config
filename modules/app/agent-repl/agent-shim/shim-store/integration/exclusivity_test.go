// exclusivity_test.go — SUBJECT: the store is a singleton and the socket is its
// token.
//
// Unlinking a socket without dialling it first is a check-then-act on somebody
// else's file: a second store would delete the path the incumbent is accepting
// on, bind its own, and take every caller — while the incumbent kept serving a
// socket nothing could reach. The kernel arbitrates instead: a path that
// ACCEPTS is owned, a path that REFUSES is debris.
package integration

import (
	"os"
	"path/filepath"
	"testing"
)

// secondStoreOnTheSameSocket launches a rival over an incumbent's socket and
// database, without waiting for a readiness it must never reach.
func secondStoreOnTheSameSocket(t *testing.T, incumbent *storeProcess) *storeProcess {
	t.Helper()
	return startStore(t, storeOptions{
		socketPath: incumbent.socket,
		dbPath:     incumbent.dbPath,
		logPath:    filepath.Join(t.TempDir(), "rival.log"),
		noWait:     true,
	})
}

func TestASecondStoreOnTheSameSocketExitsNonZero(t *testing.T) {
	// Arrange
	incumbent := startStore(t, storeOptions{})

	// Act
	rival := secondStoreOnTheSameSocket(t, incumbent)

	// Assert
	if err := rival.awaitExit(); err == nil {
		t.Fatalf("the second store exited cleanly; want a non-zero exit\nstderr:\n%s", rival.stderrText())
	}
}

func TestASecondStoreOnTheSameSocketRecordsTheOccupancyRefusal(t *testing.T) {
	// Arrange
	incumbent := startStore(t, storeOptions{})

	// Act
	rival := secondStoreOnTheSameSocket(t, incumbent)
	if err := rival.awaitExit(); err == nil {
		t.Fatalf("the second store exited cleanly; want a non-zero exit")
	}

	// Assert
	found := false
	for _, rec := range rival.logRecords() {
		if rec.Operation == "store.listen.occupied" && rec.Level == "error" {
			found = true
		}
	}
	if !found {
		t.Fatalf("the refused store wrote no store.listen.occupied error record\nstderr:\n%s", rival.stderrText())
	}
}

func TestTheIncumbentStoreKeepsServingAfterARivalIsRefused(t *testing.T) {
	// Arrange
	incumbent := startStore(t, storeOptions{})
	rival := secondStoreOnTheSameSocket(t, incumbent)
	if err := rival.awaitExit(); err == nil {
		t.Fatalf("the second store exited cleanly; want a non-zero exit")
	}

	// Act
	ctx, cancel := callContext(t)
	defer cancel()
	live := liveWork(ctx, t, incumbent.client(), "main")

	// Assert: the incumbent still owns its socket and answers on it.
	if live == nil {
		t.Fatal("the incumbent answered no live work after a rival was refused")
	}
	incumbent.assertNoErrorRecords()
}

func TestAStoreReclaimsTheSocketAKilledPredecessorLeftBehind(t *testing.T) {
	// Arrange: SIGKILL leaves the socket file on disk, which is the only way a
	// real store ever meets a stale one.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-kill-1", "u-kill-1",
		frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))
	mark := store.logMark()

	// Act
	store.restartAfterKill()

	// Assert
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	page := openSession(after, t, store.client(), "main", nil)
	assertTexts(t, "the book after a killed predecessor", pageTexts(page.GetPage()), []string{"L1"})

	reclaimed := false
	for _, rec := range store.logRecordsAfter(mark) {
		if rec.Operation == "store.listen.reclaim" && rec.Level == "warn" {
			reclaimed = true
		}
	}
	if !reclaimed {
		t.Fatalf("the successor did not record a store.listen.reclaim warning\nstderr:\n%s", store.stderrText())
	}
}

// TestARegularFileAtTheListenPathIsRefusedAndLeftIntact: the reclaim rule is
// about a SOCKET a dead store left behind. A regular file at the listen path is
// somebody's file — a typo'd flag, a log, a note — and unlinking it would be
// the store deleting data it was never given. The mode is proved before the
// path is removed, so this boot fails and the file survives byte for byte.
func TestARegularFileAtTheListenPathIsRefusedAndLeftIntact(t *testing.T) {
	// Arrange.
	const contents = "this is somebody's file, not a socket"
	path := shortSocketPath(t)
	if err := os.WriteFile(path, []byte(contents), 0o600); err != nil {
		t.Fatalf("staging a regular file at the listen path: %v", err)
	}

	// Act.
	store := startStore(t, storeOptions{socketPath: path, noWait: true})

	// Assert: the boot failed...
	if err := store.awaitExit(); err == nil {
		t.Fatalf("the store bound a listen path occupied by a regular file\nstderr:\n%s", store.stderrText())
	}
	// ...loudly...
	refused := false
	for _, rec := range recordsAtOperation(store.logRecords(), "store.listen") {
		if rec.Level == "error" {
			refused = true
		}
	}
	if !refused {
		t.Errorf("the refused boot wrote no store.listen error record\nstderr:\n%s", store.stderrText())
	}
	// ...and the file is exactly as it was.
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("the refused boot removed the file at the listen path: %v", err)
	}
	if string(got) != contents {
		t.Errorf("the refused boot rewrote the file at the listen path as %q, want %q", got, contents)
	}
}
