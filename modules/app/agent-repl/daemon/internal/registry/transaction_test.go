package registry

import (
	"fmt"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
)

// THE ROTATION ADOPTION IS ONE INDIVISIBLE WRITE.
//
// When the vendor retires a session uuid and mints another, the record adopts
// the new uuid AND resets the cursors that counted in the retired uuid's store
// seq space. A reader that saw the new identity next to the old cursors would
// resubscribe past the end of a seq space that has just begun at 1, and read
// its own first event as a regression. The JSON registry made this one write by
// discipline; the table makes it one transaction.

func TestRotationAdoptionAndCursorResetAreNeverVisibleApart(t *testing.T) {
	// Arrange — a session deep into its conversation, and a reader watching
	// the row from a SEPARATE connection to the same store.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{
		SessionID: "s_1", CWD: "/w", ClaudeSessionID: "uuid-old",
		LastSeq: 5990, NewestClearOrCompactSeq: 5000,
	}); err != nil {
		t.Fatal(err)
	}
	reader, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("open reader handle: %v", err)
	}
	t.Cleanup(func() { reader.Close() })

	adopted := make(chan struct{})
	torn := make(chan string, 1)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		for {
			var (
				uuid  string
				last  int64
				floor int64
			)
			if err := reader.QueryRow(
				`SELECT claude_session_id, last_seq, newest_clear_or_compact_seq
				 FROM session_record WHERE session_id = 's_1'`).Scan(&uuid, &last, &floor); err != nil {
				torn <- fmt.Sprintf("read: %v", err)
				return
			}
			if uuid == "uuid-new" && (last != 0 || floor != 0) {
				torn <- fmt.Sprintf("saw uuid-new with last_seq=%d floor=%d", last, floor)
				return
			}
			if uuid == "uuid-old" && (last != 5990 || floor != 5000) {
				torn <- fmt.Sprintf("saw uuid-old with last_seq=%d floor=%d", last, floor)
				return
			}
			select {
			case <-adopted:
				return
			default:
			}
		}
	}()

	// Act — the adoption the vendor-rotation path performs.
	found, err := r.Update("s_1", func(rec *Record) {
		rec.ClaudeSessionID = "uuid-new"
		rec.LastSeq = 0
		rec.NewestClearOrCompactSeq = 0
	})
	close(adopted)
	wg.Wait()

	// Assert — no interleaving showed a blend of the two states, and the
	// committed row is the rotated one.
	if err != nil || !found {
		t.Fatalf("Update: found=%v err=%v", found, err)
	}
	select {
	case why := <-torn:
		t.Fatalf("a reader observed a torn rotation: %s", why)
	default:
	}
	rec, ok := Open(path, discardLogf).Get("s_1")
	if !ok || rec.ClaudeSessionID != "uuid-new" || rec.LastSeq != 0 || rec.NewestClearOrCompactSeq != 0 {
		t.Fatalf("rotated record = %+v ok=%v, want uuid-new with zeroed cursors", rec, ok)
	}
}

func TestAFailedRotationLeavesTheRetiredIdentityAndItsCursors(t *testing.T) {
	// Arrange — a session mid-conversation.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{
		SessionID: "s_1", CWD: "/w", ClaudeSessionID: "uuid-old",
		LastSeq: 5990, NewestClearOrCompactSeq: 5000,
	}); err != nil {
		t.Fatal(err)
	}

	// Act — an adoption that the maintenance pass refuses (the same write also
	// carries an unusable backfill state).
	found, err := r.Update("s_1", func(rec *Record) {
		rec.ClaudeSessionID = "uuid-new"
		rec.LastSeq = 0
		rec.NewestClearOrCompactSeq = 0
		rec.BackfillState = "teleporting"
	})

	// Assert — neither half landed: the record still names the retired uuid
	// AND still carries the cursors that count in its seq space.
	if err == nil {
		t.Fatalf("Update succeeded with an invalid backfill state (found=%v)", found)
	}
	rec, ok := Open(path, discardLogf).Get("s_1")
	if !ok || rec.ClaudeSessionID != "uuid-old" || rec.LastSeq != 5990 || rec.NewestClearOrCompactSeq != 5000 {
		t.Fatalf("record after the refused rotation = %+v ok=%v, want the untouched retired identity", rec, ok)
	}
}

// THE REGISTRY AND THE STATE LOG SHARE ONE STORE.
//
// They are two owners of one database, so their writes must serialize rather
// than compete. These cases drive both at once over ONE handle.

func TestRegistryAndStateLogInterleaveOnOneSharedStore(t *testing.T) {
	// Arrange — one store, one connection, both owners.
	db, err := statedb.Open(testPath(t))
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	reg := OpenWith(Options{DB: db, Logf: discardLogf})
	if err := reg.Put(Record{SessionID: "uuid-1", CWD: "/w"}); err != nil {
		t.Fatal(err)
	}
	mgr, err := ssm.Open(ssm.Options{
		DB:       db,
		Resolver: registryResolver{reg},
		Logf:     func(string, ...any) {},
	})
	if err != nil {
		t.Fatalf("ssm.Open on the shared store: %v", err)
	}
	t.Cleanup(func() { mgr.Close() })

	// Act — interleaved writes from both owners.
	const rounds = 20
	var wg sync.WaitGroup
	regErrs := make(chan error, rounds)
	ssmErrs := make(chan error, rounds)
	for i := range rounds {
		wg.Add(2)
		go func() {
			defer wg.Done()
			if _, err := reg.Update("uuid-1", func(rec *Record) { rec.LastSeq = uint64(i + 1) }); err != nil {
				regErrs <- err
			}
		}()
		go func() {
			defer wg.Done()
			if err := mgr.Apply(&corev1.Event{
				SessionId: "uuid-1",
				Seq:       uint64(i + 1),
				Plane:     corev1.Plane_PLANE_STREAM,
				RequestId: "turn",
				Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "turn"}},
			}); err != nil {
				ssmErrs <- err
			}
		}()
	}
	wg.Wait()
	close(regErrs)
	close(ssmErrs)

	// Assert — neither owner hit a lock error, and both wrote.
	for err := range regErrs {
		t.Fatalf("registry write over the shared store: %v", err)
	}
	for err := range ssmErrs {
		t.Fatalf("state-log write over the shared store: %v", err)
	}
	if rec, ok := reg.Get("uuid-1"); !ok || rec.LastSeq == 0 {
		t.Fatalf("record after interleaved writes = %+v ok=%v", rec, ok)
	}
	var rows int
	if err := db.QueryRow(`SELECT COUNT(*) FROM workspace_state WHERE session_id = 'uuid-1'`).Scan(&rows); err != nil {
		t.Fatalf("count state rows: %v", err)
	}
	if rows != rounds {
		t.Fatalf("state-log rows = %d, want %d", rows, rounds)
	}
}

// registryResolver binds session ids to workspaces out of the registry, as
// server.RegistryResolver does in production (which this package cannot import
// without a cycle).
type registryResolver struct{ reg *Registry }

func (r registryResolver) Workspace(sessionID string) (string, bool) {
	rec, ok := r.reg.Get(sessionID)
	if !ok || rec.CWD == "" {
		return "", false
	}
	return rec.CWD, true
}
