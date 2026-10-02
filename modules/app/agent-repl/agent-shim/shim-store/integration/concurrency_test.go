// concurrency_test.go — SUBJECT: many producers writing at once.
//
// One store process serves every live producer on one pooled connection, which
// is why the DSN carries _txlock=immediate: a write batch READS (the ledger
// probe, the write ordinal) before it inserts, and a DEFERRED transaction would
// take a WAL read snapshot and then try to upgrade — which SQLite answers with
// SQLITE_BUSY_SNAPSHOT immediately, never running the busy handler. That
// rationale was never exercised: nothing in the suite wrote concurrently at all.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"fmt"
	"sync"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

const (
	// concurrentWriters is deliberately above the pool's idle size so writers
	// genuinely contend rather than serializing on one connection.
	concurrentWriters = 8
	// linesPerWriter keeps each writer's batch small so the interleaving is
	// fine-grained.
	linesPerWriter = 4
)

// writeConcurrently drives N writers at one book and returns only once every
// one of them has been acknowledged. The WaitGroup is the synchronization —
// there is no sleep anywhere in this path.
func writeConcurrently(t *testing.T, store *storeProcess, book string, writers, perWriter int) {
	t.Helper()

	var wg sync.WaitGroup
	errs := make(chan error, writers)
	for w := 0; w < writers; w++ {
		wg.Add(1)
		go func(w int) {
			defer wg.Done()
			ctx, cancel := callContext(t)
			defer cancel()
			// Alternating planes, because the two real producers are a stream
			// writer and a file writer sharing one upsert-key space.
			cli := store.client()
			p := streamProducer(cli)
			if w%2 == 1 {
				p = fileProducer(cli)
			}
			for i := 0; i < perWriter; i++ {
				label := fmt.Sprintf("w%d-l%d", w, i)
				resp, err := p.attempt(ctx, &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
					p.agentEntry("write-"+label, "unit-"+label,
						frameLine(agentID(book), responseFrame(book, "act-"+label, label))),
				}})
				if err != nil {
					errs <- fmt.Errorf("writer %d line %d: transport error: %w", w, i, err)
					return
				}
				if failure := resp.GetFailure(); failure != nil {
					errs <- fmt.Errorf("writer %d line %d refused: %s", w, i, failure.GetDetail())
					return
				}
			}
		}(w)
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
}

func TestConcurrentProducersAreAllAcknowledged(t *testing.T) {
	// Arrange: the BUSY_SNAPSHOT rationale in one subject — every writer must
	// succeed, not merely most of them.
	store := startStore(t, storeOptions{})

	// Act
	writeConcurrently(t, store, "main", concurrentWriters, linesPerWriter)

	// Assert
	store.assertNoErrorRecords()
}

func TestConcurrentProducersLandEveryLineExactlyOnce(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	writeConcurrently(t, store, "main", concurrentWriters, linesPerWriter)
	ctx, cancel := callContext(t)
	defer cancel()

	// Act
	page := openSession(ctx, t, store.client(), "main", nil)

	// Assert
	want := concurrentWriters * linesPerWriter
	if got := len(page.GetPage().GetLines()); got != want {
		t.Fatalf("the book holds %d lines, want %d", got, want)
	}
}

func TestConcurrentProducersMintDistinctPointers(t *testing.T) {
	// Arrange: position is an AUTOINCREMENT primary key, so two rows sharing a
	// pointer would mean the page order itself collided under contention.
	store := startStore(t, storeOptions{})
	writeConcurrently(t, store, "main", concurrentWriters, linesPerWriter)
	ctx, cancel := callContext(t)
	defer cancel()

	// Act
	page := openSession(ctx, t, store.client(), "main", nil)

	// Assert
	seen := map[string]bool{}
	for _, pointer := range pagePointers(page.GetPage()) {
		if seen[pointer] {
			t.Fatalf("pointer %q was served for two different lines", pointer)
		}
		seen[pointer] = true
	}
}

func TestAWatcherReceivesEveryConcurrentlyWrittenLine(t *testing.T) {
	// Arrange: the fan-out publishes under its own lock while writers commit,
	// so a lost line here is a lost line for every live consumer.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	seedBook(ctx, t, streamProducer(cli), "main", "concurrent")
	opened := openSession(ctx, t, cli, "main", nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act
	writeConcurrently(t, store, "main", concurrentWriters, linesPerWriter)

	// Assert: exactly the written count, and the stream delivering them is the
	// synchronization — receiveLines blocks until they arrive.
	want := concurrentWriters * linesPerWriter
	got := receiveLines(t, stream, want)
	seen := map[string]bool{}
	for _, line := range got {
		if seen[line.text] {
			t.Fatalf("the watcher received %q twice", line.text)
		}
		seen[line.text] = true
	}
}

func TestTwoPlanesWritingOneUnitLeaveOneLineAtOnePointer(t *testing.T) {
	// Arrange: the stream plane and the file plane observe the SAME unit and
	// key it identically — that shared upsert-key space is the design. The file
	// plane is authoritative for content, and its write must supersede the row
	// in place rather than adding a second line.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-draft", "unit-x",
		frameLine(agentID("main"), responseFrame("main", "act-x", "draft"))))
	before := pagePointers(openSession(ctx, t, cli, "main", nil).GetPage())

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-final", "unit-x",
		frameLine(agentID("main"), responseFrame("main", "act-x", "final"))))

	// Assert
	page := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the book after both planes wrote", pageTexts(page.GetPage()), []string{"final"})
	assertTexts(t, "the pointer after the supersession", pagePointers(page.GetPage()), before)
}

func TestReplayingBothPlanesWritesDeliversNothingToAWatcher(t *testing.T) {
	// Arrange: both write_ids already landed, so both replays are absorbed —
	// and absorption must not re-publish the line to a live tail.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	draft := shim.agentEntry("w-draft", "unit-x", frameLine(agentID("main"), responseFrame("main", "act-x", "draft")))
	final := sidecar.agentEntry("w-final", "unit-x", frameLine(agentID("main"), responseFrame("main", "act-x", "final")))
	shim.write(ctx, t, draft)
	sidecar.write(ctx, t, final)
	opened := openSession(ctx, t, cli, "main", nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act
	shim.write(ctx, t, draft)
	sidecar.write(ctx, t, final)
	shim.write(ctx, t, shim.agentEntry("w-after", "unit-after",
		frameLine(agentID("main"), responseFrame("main", "act-after", "after"))))

	// Assert: the very next frame is the new line, so neither replay published.
	assertTexts(t, "the tail after two absorbed replays", receivedTexts(receiveLines(t, stream, 1)), []string{"after"})
}

// TestConcurrentUpsertsOfOneKeyLeaveOneRow is contention on ONE row rather than
// on the table: every writer names the same upsert_key, so the store's
// serialization is what decides whether a row exists once or several times.
//
// A key that raced into two rows would give one unit two pointers, and a caller
// walking the book would read the same unit twice — which is exactly what the
// page model promises cannot happen.
func TestConcurrentUpsertsOfOneKeyLeaveOneRow(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})

	// Act: every writer supersedes the same row, under its own write_id.
	var wg sync.WaitGroup
	errs := make(chan error, concurrentWriters)
	for w := 0; w < concurrentWriters; w++ {
		wg.Add(1)
		go func(w int) {
			defer wg.Done()
			ctx, cancel := callContext(t)
			defer cancel()
			cli := store.client()
			p := streamProducer(cli)
			if w%2 == 1 {
				p = fileProducer(cli)
			}
			label := fmt.Sprintf("settled-by-%d", w)
			resp, err := p.attempt(ctx, &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
				p.agentEntry("write-"+label, "u-one-key",
					frameLine(agentID("main"), responseFrame("main", "act-one-key", label))),
			}})
			if err != nil {
				errs <- fmt.Errorf("writer %d: transport error: %w", w, err)
				return
			}
			if failure := resp.GetFailure(); failure != nil {
				errs <- fmt.Errorf("writer %d refused: %s", w, failure.GetDetail())
			}
		}(w)
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}

	// Assert: one row, at one pointer.
	ctx, cancel := callContext(t)
	defer cancel()
	page := openSession(ctx, t, store.client(), "main", nil)
	if got := len(page.GetPage().GetLines()); got != 1 {
		t.Fatalf("the book holds %d lines for one upsert_key, want 1 (%v)", got, pageTexts(page.GetPage()))
	}
	store.assertNoErrorRecords()
}
