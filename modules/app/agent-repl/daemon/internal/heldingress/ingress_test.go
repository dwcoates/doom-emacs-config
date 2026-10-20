package heldingress

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/flock"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	full := func() Deps {
		w := newWorld(t)
		return Deps{
			Dir:            w.dir,
			WorkspaceByDir: w.ingressLookup(),
			Prompts:        w.handler,
			Pending:        w.pending,
			PublishHost:    func(ids.WorkspaceID) {},
			Serves:         func() bool { return true },
			Log:            w.log,
		}
	}
	tests := []struct {
		name   string
		mutate func(*Deps)
	}{
		{name: "no directory", mutate: func(d *Deps) { d.Dir = "" }},
		{name: "no workspace lookup", mutate: func(d *Deps) { d.WorkspaceByDir = nil }},
		{name: "no prompt handler", mutate: func(d *Deps) { d.Prompts = nil }},
		{name: "no held section", mutate: func(d *Deps) { d.Pending = nil }},
		{name: "no host publisher", mutate: func(d *Deps) { d.PublishHost = nil }},
		{name: "no log surfaces", mutate: func(d *Deps) { d.Log = nil }},
		{name: "no serving answer", mutate: func(d *Deps) { d.Serves = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.mutate(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatalf("New(%s) = nil error, want a refusal", tc.name)
			}
		})
	}
}

func TestSweepIngestsEveryEntryInNameOrderUnderItsOwnKeyAndOrigin(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120002_b.json", "/work/one", "k-2", "second")
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

	// Act
	w.sweep(w.ingress())

	// Assert
	if got := w.keys(); !reflect.DeepEqual(got, []string{"k-1", "k-2"}) {
		t.Fatalf("submitted keys = %v, want the name order [k-1 k-2]", got)
	}
	for _, c := range w.handler.calls {
		if c.WS != "ws-one" || c.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT {
			t.Fatalf("submission = %+v, want ws-one under the written origin", c)
		}
	}
	if left := w.entries(); len(left) != 0 {
		t.Fatalf("entries left = %v, want every ingested entry removed", left)
	}
	if got := len(w.records("info", opIngest)); got != 2 {
		t.Fatalf("INFO %s records = %d, want one per ingested entry", opIngest, got)
	}
}

func TestSweepPublishesTheHostOnlyAfterTheEntryIsRemoved(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

	// Act
	w.sweep(w.ingress())

	// Assert
	if !reflect.DeepEqual(w.published, []ids.WorkspaceID{"ws-one"}) {
		t.Fatalf("published = %v, want ws-one once", w.published)
	}
	if w.publishedSaw[0] != 0 {
		t.Fatalf("entries present at the publish = %d, want 0 (the removal precedes the push)", w.publishedSaw[0])
	}
}

func TestSweepRecordsAnAnsweredEntryOnlyOnceItIsRemoved(t *testing.T) {
	tests := []struct {
		name      string
		accepted  bool
		operation string
	}{
		{name: "an ingested entry", operation: opIngest},
		{name: "a deduplicated entry", accepted: true, operation: opDedupe},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the removal notes how many answer records already stand.
			w := newWorld(t)
			w.handler.accepted["k-1"] = tt.accepted
			w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
			recordedAtRemoval := -1
			w.remove = func(path string) error {
				recordedAtRemoval = len(w.records("info", tt.operation))
				return removeFile(path)
			}

			// Act
			w.sweep(w.ingress())

			// Assert: a reader waiting on the record finds the entry gone.
			if recordedAtRemoval != 0 {
				t.Fatalf("INFO %s records at the removal = %d, want 0 (the record follows the removal)", tt.operation, recordedAtRemoval)
			}
			if got := len(w.records("info", tt.operation)); got != 1 {
				t.Fatalf("INFO %s records = %d, want 1", tt.operation, got)
			}
		})
	}
}

func TestSweepDoesNotDeliverAPromptTheDaemonAlreadyAcceptedUnderItsKey(t *testing.T) {
	// Arrange: the SubmitPrompt the client gave up on DID land.
	w := newWorld(t)
	w.handler.accepted["k-landed"] = true
	w.write("held_20260928T120001_a.json", "/work/one", "k-landed", "already in")

	// Act
	w.sweep(w.ingress())

	// Assert
	if got := w.handler.delivered("k-landed"); got != 0 {
		t.Fatalf("deliveries = %d, want 0 (the earlier acceptance stands)", got)
	}
	if left := w.entries(); len(left) != 0 {
		t.Fatalf("entries left = %v, want the answered entry removed", left)
	}
	if got := len(w.records("info", opDedupe)); got != 1 {
		t.Fatalf("INFO %s records = %d, want 1", opDedupe, got)
	}
}

func TestACrashBetweenTheAcceptanceAndTheRemovalNeitherLosesNorDuplicates(t *testing.T) {
	// Arrange: the first daemon accepts the prompt and dies before its file
	// is removed -- the removal never happens.
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	w.remove = func(string) error { return errors.New("the process died here") }
	w.sweep(w.ingress())

	// Act: a restarted daemon sweeps the same directory against the same
	// durable claim.
	w.remove = nil
	w.sweep(w.ingress())

	// Assert
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries = %d, want exactly 1", got)
	}
	if left := w.entries(); len(left) != 0 {
		t.Fatalf("entries left = %v, want the entry removed by the restart", left)
	}
}

func TestACrashBeforeTheSubmissionLeavesTheEntryForTheNextSweep(t *testing.T) {
	// Arrange: the sweep is cut off before it reaches the entry.
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if err := w.ingress().Sweep(ctx); !errors.Is(err, context.Canceled) {
		t.Fatalf("a cancelled Sweep = %v, want context.Canceled", err)
	}

	// Act
	w.sweep(w.ingress())

	// Assert
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries = %d, want exactly 1", got)
	}
	if left := w.entries(); len(left) != 0 {
		t.Fatalf("entries left = %v, want none", left)
	}
}

func TestARefusedEntryStaysAndHoldsBackItsWorkspacesLaterEntries(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrMerging
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	w.write("held_20260928T120002_b.json", "/work/one", "k-2", "second")
	w.write("held_20260928T120003_c.json", "/work/two", "k-3", "elsewhere")

	// Act
	w.sweep(w.ingress())

	// Assert
	if got := w.keys(); !reflect.DeepEqual(got, []string{"k-1", "k-3"}) {
		t.Fatalf("submitted keys = %v, want k-1 (refused) and k-3 (another workspace), never k-2 ahead of k-1", got)
	}
	if left := len(w.entries()); left != 2 {
		t.Fatalf("entries left = %d, want the refused entry and the one behind it", left)
	}
}

func TestARefusalIsRecordedOnceAtInfoAndItsRetriesAtDebug(t *testing.T) {
	tests := []struct {
		name      string
		refusal   error
		wantLevel string
	}{
		{name: "a merge in flight", refusal: promptqueue.ErrMerging, wantLevel: "info"},
		{name: "a cold gate", refusal: &promptqueue.ColdGateRefusal{Detail: "answer the gate"}, wantLevel: "info"},
		{name: "no session", refusal: promptqueue.ErrNoSession, wantLevel: "info"},
		{name: "a move sealed toward another daemon", refusal: fmt.Errorf("session act: %w", bounce.ErrMovedAway), wantLevel: "info"},
		{name: "an unexpected fault", refusal: errors.New("the database is locked"), wantLevel: "error"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			w.handler.refuse["k-1"] = tc.refusal
			w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
			in := w.ingress()

			// Act: the first refusal, then a retry once it is due.
			w.sweep(in)
			w.now = w.now.Add(time.Hour)
			w.sweep(in)

			// Assert
			if got := len(w.records(tc.wantLevel, opDefer)); got != 1 {
				t.Fatalf("%s %s records = %d, want exactly 1", tc.wantLevel, opDefer, got)
			}
			if got := len(w.records("debug", opDefer)); got < 1 {
				t.Fatalf("debug %s records = %d, want the retry recorded at debug", opDefer, got)
			}
		})
	}
}

func TestEachRefusalKindAnEntryMeetsIsRecordedOnceAtInfo(t *testing.T) {
	tests := []struct {
		name     string
		refusals []error
		wantInfo int
	}{
		{name: "one standing condition is stated once", refusals: []error{promptqueue.ErrMerging, promptqueue.ErrMerging, promptqueue.ErrMerging}, wantInfo: 1},
		{name: "a condition that changes is stated again", refusals: []error{promptqueue.ErrMerging, &promptqueue.ColdGateRefusal{Detail: "answer the gate"}}, wantInfo: 2},
		{name: "a condition that returns is not stated twice", refusals: []error{promptqueue.ErrMerging, promptqueue.ErrNoSession, promptqueue.ErrMerging}, wantInfo: 2},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
			in := w.ingress()

			// Act: one sweep per refusal, each once its retry is due.
			for _, refusal := range tc.refusals {
				w.handler.refuse["k-1"] = refusal
				w.sweep(in)
				w.now = w.now.Add(time.Hour)
			}

			// Assert
			if got := len(w.records("info", opDefer)); got != tc.wantInfo {
				t.Fatalf("info %s records = %d, want %d", opDefer, got, tc.wantInfo)
			}
		})
	}
}

func TestARefusedEntryIsRetriedOnlyOnceItsDelayHasPassed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrMerging
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	in := w.ingress()
	w.sweep(in)
	delete(w.handler.refuse, "k-1")

	// Act: a sweep before the delay, then one after it.
	w.sweep(in)
	early := len(w.handler.calls)
	w.now = w.now.Add(DefaultInterval)
	w.sweep(in)

	// Assert
	if early != 1 {
		t.Fatalf("submissions before the retry was due = %d, want only the first", early)
	}
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries after the retry = %d, want 1", got)
	}
}

func TestTheRetryDelayDoublesUpToTheCeiling(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrMerging
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	in := w.ingress()
	var delays []int64

	// Act: sweep exactly when each retry falls due.
	for n := 0; n < 8; n++ {
		w.sweep(in)
		records := w.records("info", opDefer)
		records = append(records, w.records("debug", opDefer)...)
		last := latestRetry(records)
		delays = append(delays, last)
		w.now = w.now.Add(time.Duration(last) * time.Millisecond)
	}

	// Assert
	want := []int64{250, 500, 1000, 2000, 4000, 8000, 10000, 10000}
	if !reflect.DeepEqual(delays, want) {
		t.Fatalf("retry delays = %v, want %v", delays, want)
	}
}

// latestRetry is the retry_in_ms of the newest deferral that carries one.
func latestRetry(records []dlog.Record) int64 {
	var last int64
	for _, r := range records {
		if v, ok := r.Context["retry_in_ms"].(int64); ok {
			last = v
		}
	}
	return last
}

func TestAnEntryForAnUnregisteredDirectoryWaitsAndIsWarnedOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/unknown", "k-1", "first")
	in := w.ingress()

	// Act
	w.sweep(in)
	w.now = w.now.Add(time.Hour)
	w.sweep(in)

	// Assert
	if len(w.handler.calls) != 0 {
		t.Fatalf("submissions = %v, want none for an unregistered directory", w.handler.calls)
	}
	if left := len(w.entries()); left != 1 {
		t.Fatalf("entries left = %d, want the entry kept", left)
	}
	if got := len(w.records("warn", opDefer)); got != 1 {
		t.Fatalf("WARN %s records = %d, want exactly 1", opDefer, got)
	}
}

func TestAnUnregisteredDirectoryIsIngestedOnceItIsRegistered(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/late", "k-1", "first")
	in := w.ingress()
	w.sweep(in)

	// Act
	w.byDir["/work/late"] = w.byDir["/work/one"]
	w.now = w.now.Add(time.Hour)
	w.sweep(in)

	// Assert
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries = %d, want 1 once the workspace is registered", got)
	}
}

func TestAMalformedEntryIsQuarantinedWithAWarning(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.writeRaw("held_20260928T120001_a.json", `{"version":1`)

	// Act
	w.sweep(w.ingress())

	// Assert
	if left := len(w.entries()); left != 0 {
		t.Fatalf("entries left = %d, want the malformed one moved aside", left)
	}
	if _, err := os.Stat(filepath.Join(w.dir, "quarantine", "held_20260928T120001_a.json")); err != nil {
		t.Fatalf("quarantined file: %v, want it readable in quarantine/", err)
	}
	if got := len(w.records("warn", opQuarantine)); got != 1 {
		t.Fatalf("WARN %s records = %d, want 1", opQuarantine, got)
	}
}

func TestAQuarantineThatCannotMoveTheEntryIsAnErrorNotAWarning(t *testing.T) {
	// Arrange: a plain file where the quarantine directory belongs.
	w := newWorld(t)
	w.writeRaw("held_20260928T120001_a.json", `{"version":1`)
	if err := os.WriteFile(filepath.Join(w.dir, "quarantine"), nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	w.sweep(w.ingress())

	// Assert
	if got := len(w.records("warn", opQuarantine)); got != 0 {
		t.Fatalf("WARN %s records = %d, want 0: the entry never moved", opQuarantine, got)
	}
	if got := len(w.records("error", opQuarantine)); got != 1 {
		t.Fatalf("ERROR %s records = %d, want 1", opQuarantine, got)
	}
}

func TestATemporaryFileIsNeverRead(t *testing.T) {
	// Arrange: a producer still writing, under its dot-prefixed temp name.
	w := newWorld(t)
	if err := os.WriteFile(filepath.Join(w.dir, ".held_20260928T120001_a.json.tmp"), []byte(`{"vers`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	w.sweep(w.ingress())

	// Assert
	if len(w.handler.calls) != 0 || len(w.records("warn", opQuarantine)) != 0 {
		t.Fatalf("a temp file was read: calls=%v", w.handler.calls)
	}
}

func TestAFailedRemovalIsAnErrorAndTheNextSweepDedupesIt(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	w.remove = func(string) error { return errors.New("permission denied") }
	in := w.ingress()

	// Act
	w.sweep(in)

	// Assert
	if got := len(w.records("error", opRemove)); got != 1 {
		t.Fatalf("ERROR %s records = %d, want 1", opRemove, got)
	}
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries = %d, want 1", got)
	}
}

func TestRunSweepsAtStartBeforeTheFirstTick(t *testing.T) {
	// Arrange: an interval no test would wait out.
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	published := make(chan ids.WorkspaceID, 1)
	in, err := New(Deps{
		Dir:            w.dir,
		WorkspaceByDir: w.ingressLookup(),
		Prompts:        w.handler,
		Pending:        w.pending,
		PublishHost:    func(ws ids.WorkspaceID) { published <- ws },
		Serves:         func() bool { return true },
		Log:            w.log,
		Interval:       time.Hour,
	})
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act
	go func() { done <- in.Run(ctx) }()
	got := <-published
	cancel()

	// Assert
	if got != "ws-one" {
		t.Fatalf("published = %q, want ws-one from the start-up sweep", got)
	}
	if err := <-done; !errors.Is(err, context.Canceled) {
		t.Fatalf("Run = %v, want context.Canceled", err)
	}
}

// ---- only the daemon that serves takes the intake, one sweep at a time ----

func TestASweepTakesTheIntakeOnlyWhileThisDaemonServes(t *testing.T) {
	tests := []struct {
		name      string
		serves    bool
		wantCalls int
		wantLeft  int
	}{
		{name: "a serving daemon ingests the entry", serves: true, wantCalls: 1, wantLeft: 0},
		{name: "a daemon that does not serve leaves it on disk", serves: false, wantCalls: 0, wantLeft: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			w.serves = tc.serves
			w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

			// Act
			w.sweep(w.ingress())

			// Assert
			if got := len(w.handler.calls); got != tc.wantCalls {
				t.Fatalf("submissions = %d, want %d", got, tc.wantCalls)
			}
			if got := len(w.entries()); got != tc.wantLeft {
				t.Fatalf("entries left = %d, want %d", got, tc.wantLeft)
			}
		})
	}
}

func TestASweepTakesNothingWhileAnotherDaemonHoldsTheSweepLock(t *testing.T) {
	// Arrange: another daemon on this state root is mid-sweep.
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	other, held, err := flock.TryExclusiveIn(filepath.Join(w.dir, LockName))
	if err != nil || !held {
		t.Fatalf("TryExclusive = (%v, %v), want the lock", held, err)
	}
	t.Cleanup(func() { _ = other.Release() })

	// Act
	w.sweep(w.ingress())

	// Assert
	if len(w.handler.calls) != 0 || len(w.entries()) != 1 {
		t.Fatalf("calls = %v, entries = %d: a sweep under another daemon's lock must take nothing", w.handler.calls, len(w.entries()))
	}
}

func TestASweepReleasesItsLockSoTheNextOneRuns(t *testing.T) {
	// Arrange
	w := newWorld(t)
	in := w.ingress()
	w.sweep(in)
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

	// Act
	w.sweep(in)

	// Assert
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries = %d, want the second sweep to run and ingest the entry", got)
	}
}

func TestASweepOfAMissingDirectoryNeverCreatesIt(t *testing.T) {
	// Arrange: nothing was ever written, so the ingress directory is absent.
	w := newWorld(t)
	w.dir = filepath.Join(w.dir, "held-prompts")

	// Act
	w.sweep(w.ingress())

	// Assert
	if _, err := os.Stat(w.dir); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("stat %s = %v, want the sweep to leave the directory absent", w.dir, err)
	}
}

func TestSweepSubmitsAnEntryUnderItsOwnDelivery(t *testing.T) {
	tests := []struct {
		name     string
		delivery string
		want     wsm.Delivery
	}{
		{name: "an ordinary entry", delivery: "", want: wsm.DeliveryOrdinary},
		{name: "a deferred entry stays deferred", delivery: "SUBMIT_PROMPT_DELIVERY_DEFERRED", want: wsm.DeliveryDeferred},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			w.writeDelivered("held_20260928T120001_a.json", "/work/one", "k-1", "first", tc.delivery)

			// Act
			w.sweep(w.ingress())

			// Assert
			if len(w.handler.calls) != 1 || w.handler.calls[0].Delivery != tc.want {
				t.Fatalf("submissions = %+v, want one delivered %s", w.handler.calls, tc.want)
			}
		})
	}
}

// keysAndWaits lists ENTRIES' keys and wait kinds, in order.
func keysAndWaits(entries []holds.IngressEntry) []string {
	out := make([]string, 0, len(entries))
	for _, e := range entries {
		out = append(out, fmt.Sprintf("%s:%d:%s", e.Key, e.Wait.Kind, e.Wait.Detail))
	}
	return out
}

func TestARefusedEntryIsStatedPendingWithWhatItWaitsOn(t *testing.T) {
	tests := []struct {
		name     string
		refusal  error
		wantWait holds.IngressWait
	}{
		{name: "no session", refusal: promptqueue.ErrNoSession, wantWait: holds.IngressWait{Kind: holds.IngressNoSession}},
		{name: "a merge", refusal: promptqueue.ErrMerging, wantWait: holds.IngressWait{Kind: holds.IngressMerge}},
		{name: "a cold gate", refusal: promptqueue.ErrColdGate, wantWait: holds.IngressWait{Kind: holds.IngressColdGate}},
		{name: "a move away", refusal: bounce.ErrMovedAway, wantWait: holds.IngressWait{Kind: holds.IngressMovedAway}},
		{name: "a fault", refusal: errors.New("boom"), wantWait: holds.IngressWait{Kind: holds.IngressRefused, Detail: "the daemon could not queue this prompt and keeps trying"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			w.handler.refuse["k-1"] = tc.refusal
			w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

			// Act
			w.sweep(w.ingress())

			// Assert
			got, ok := w.pending.lastSet("ws-one")
			if !ok || len(got) != 1 {
				t.Fatalf("stated entries = %+v (stated %v), want the refused entry", got, ok)
			}
			if got[0].Key != "k-1" || got[0].Wait != tc.wantWait {
				t.Fatalf("stated entry = %s, want k-1 waiting on %+v", keysAndWaits(got), tc.wantWait)
			}
		})
	}
}

func TestAPendingEntryCarriesItsSaidAndQueuedAt(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrNoSession
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

	// Act
	w.sweep(w.ingress())

	// Assert
	got, _ := w.pending.lastSet("ws-one")
	if len(got) != 1 {
		t.Fatalf("stated entries = %+v, want one", got)
	}
	if text := got[0].Said.GetContent().GetBlocks()[0].GetText().GetText(); text != "first" {
		t.Fatalf("said text = %q, want first", text)
	}
	if want := time.Date(2026, 9, 28, 11, 59, 0, 0, time.UTC); !got[0].QueuedAt.Equal(want) {
		t.Fatalf("queued at = %v, want %v", got[0].QueuedAt, want)
	}
}

func TestAnEntryBehindAWaitingOneIsStatedWaitingBehindIt(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrMerging
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	w.write("held_20260928T120002_b.json", "/work/one", "k-2", "second")

	// Act
	w.sweep(w.ingress())

	// Assert
	got, _ := w.pending.lastSet("ws-one")
	want := []string{
		fmt.Sprintf("k-1:%d:", holds.IngressMerge),
		fmt.Sprintf("k-2:%d:%s", holds.IngressAwaitingIntake, behindEarlier.Detail),
	}
	if !reflect.DeepEqual(keysAndWaits(got), want) {
		t.Fatalf("stated entries = %v, want %v", keysAndWaits(got), want)
	}
}

func TestAnEntryWhoseRetryIsNotDueKeepsWhatItLastWaitedOn(t *testing.T) {
	// Arrange: the first sweep is refused for want of a session.
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrNoSession
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	in := w.ingress()
	w.sweep(in)

	// Act: a second sweep before the retry is due.
	w.sweep(in)

	// Assert
	if calls := len(w.handler.calls); calls != 1 {
		t.Fatalf("submissions = %d, want the retry not yet made", calls)
	}
	got, _ := w.pending.lastSet("ws-one")
	if want := []string{fmt.Sprintf("k-1:%d:", holds.IngressNoSession)}; !reflect.DeepEqual(keysAndWaits(got), want) {
		t.Fatalf("stated entries = %v, want %v", keysAndWaits(got), want)
	}
}

func TestAnEntryTheQueueAnsweredIsDroppedAndNeverStatedPending(t *testing.T) {
	tests := []struct {
		name   string
		accept bool
	}{
		{name: "accepted now", accept: false},
		{name: "already accepted", accept: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			w.handler.accepted["k-1"] = tc.accept
			w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")

			// Act
			w.sweep(w.ingress())

			// Assert
			want := []pendingCall{{Op: "drop", WS: "ws-one", Key: "k-1"}}
			if !reflect.DeepEqual(w.pending.calls, want) {
				t.Fatalf("held-section calls = %+v, want only the drop", w.pending.calls)
			}
		})
	}
}

func TestAWorkspaceWithNoEntriesLeftIsStatedEmptyOnce(t *testing.T) {
	// Arrange: a refused entry is stated, then accepted.
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrNoSession
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	in := w.ingress()
	w.sweep(in)
	delete(w.handler.refuse, "k-1")
	w.now = w.now.Add(time.Hour)

	// Act: the sweep that accepts it, and one more with nothing left.
	w.sweep(in)
	w.sweep(in)

	// Assert
	var sets []int
	for _, c := range w.pending.calls {
		if c.Op == "set" {
			sets = append(sets, len(c.Entries))
		}
	}
	if !reflect.DeepEqual(sets, []int{1, 0}) {
		t.Fatalf("stated list sizes = %v, want the entry, then the empty list once", sets)
	}
}

func TestAnEntryOfAnUnregisteredDirectoryIsNeverStated(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/nowhere", "k-1", "first")

	// Act
	w.sweep(w.ingress())

	// Assert
	if len(w.pending.calls) != 0 {
		t.Fatalf("held-section calls = %+v, want none: no workspace has a feed to draw it on", w.pending.calls)
	}
}
