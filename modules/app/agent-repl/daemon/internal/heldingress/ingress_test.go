package heldingress

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/recordlock"
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

// A SUBMISSION THAT NEVER ANSWERS ENDS AT ITS BOUND (2026-10-20: a re-driven
// /compact waited with no end, holding its workspace's one shim call and every
// sweep after it).
func TestASubmissionThatNeverAnswersEndsAtItsBoundAndStays(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.submitBound = time.Millisecond
	w.handler.hang["k-1"] = true
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "/compact")
	w.write("held_20260928T120002_b.json", "/work/two", "k-2", "elsewhere")

	// Act
	w.sweep(w.ingress())

	// Assert
	if got := w.keys(); !reflect.DeepEqual(got, []string{"k-1", "k-2"}) {
		t.Fatalf("submitted keys = %v, want the hung k-1 ended and k-2 of another workspace after it", got)
	}
	if left := len(w.entries()); left != 1 {
		t.Fatalf("entries left = %d, want the hung entry kept for a later sweep", left)
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
			}

			// Assert
			if got := len(w.records("info", opDefer)); got != tc.wantInfo {
				t.Fatalf("info %s records = %d, want %d", opDefer, got, tc.wantInfo)
			}
		})
	}
}

func TestARefusedEntryIsSubmittedAgainAtTheNextSweep(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrMerging
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	in := w.ingress()
	w.sweep(in)
	delete(w.handler.refuse, "k-1")

	// Act: the very next sweep, with no delay between.
	w.sweep(in)

	// Assert
	if got := w.handler.delivered("k-1"); got != 1 {
		t.Fatalf("deliveries at the next sweep = %d, want 1", got)
	}
}

func TestAnEntryForAnUnregisteredDirectoryWaitsAndIsWarnedOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.write("held_20260928T120001_a.json", "/work/unknown", "k-1", "first")
	in := w.ingress()

	// Act
	w.sweep(in)
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

// runWorld is a world whose ingress Run drives, with every signal Run waits
// on in the test's hands.
type runWorld struct {
	*world
	// dir is the ingress directory, which a case may leave unmade.
	dir string
	// calls receives every submission's key, and published every workspace
	// whose host was republished, as they happen.
	calls     chan string
	published chan ids.WorkspaceID
	// refusals is how many submissions of any key are refused before the
	// next is accepted.
	refusals atomic.Int32
	serves   atomic.Bool
	// intake is closed and replaced to say the serving answer changed.
	intakeMu sync.Mutex
	intake   chan struct{}
	fleet    chan struct{}
}

// runHandler is a Submit safe to call from Run's goroutine while the test
// reads: it refuses while refusals stand, then accepts.
type runHandler struct {
	prompthandler.Handler
	w *runWorld
}

func (h runHandler) Submit(_ context.Context, _ ids.WorkspaceID, _ *conversationv1.UserSaid, key string, _ conversationv1.PromptOrigin, _ *ids.WorkspaceID, _ wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	h.w.calls <- key
	if h.w.refusals.Add(-1) >= 0 {
		return prompthandler.Outcome{}, promptqueue.ErrMerging
	}
	return prompthandler.Outcome{Turn: ids.TurnID("turn-" + key)}, nil
}

func newRunWorld(t *testing.T, dir string) *runWorld {
	t.Helper()
	r := &runWorld{
		world: newWorld(t), dir: dir,
		calls: make(chan string, 16), published: make(chan ids.WorkspaceID, 16),
		intake: make(chan struct{}), fleet: make(chan struct{}),
	}
	r.serves.Store(true)
	return r
}

// openIntake makes the daemon serve and says so.
func (r *runWorld) openIntake() {
	r.serves.Store(true)
	r.intakeMu.Lock()
	defer r.intakeMu.Unlock()
	close(r.intake)
	r.intake = make(chan struct{})
}

// run starts Run, answering its end.
func (r *runWorld) run(t *testing.T) (cancel func(), done <-chan error) {
	t.Helper()
	in, err := New(Deps{
		Dir:            r.dir,
		WorkspaceByDir: r.ingressLookup(),
		Prompts:        runHandler{w: r},
		Pending:        nopPending{},
		PublishHost:    func(ws ids.WorkspaceID) { r.published <- ws },
		Serves:         r.serves.Load,
		IntakeChanged: func() <-chan struct{} {
			r.intakeMu.Lock()
			defer r.intakeMu.Unlock()
			return r.intake
		},
		FleetChanges: func(context.Context) <-chan struct{} { return r.fleet },
		Log:          r.log,
	})
	if err != nil {
		t.Fatal(err)
	}
	ctx, stop := context.WithCancel(context.Background())
	ended, finished := make(chan error, 1), make(chan struct{})
	go func() {
		ended <- in.Run(ctx)
		close(finished)
	}()
	t.Cleanup(func() { stop(); <-finished })
	return stop, ended
}

// writeIn drops one entry into DIR, made if it is missing.
func (r *runWorld) writeIn(t *testing.T, name string) {
	t.Helper()
	if err := os.MkdirAll(r.dir, 0o755); err != nil {
		t.Fatal(err)
	}
	r.world.dir = r.dir
	r.write(name, "/work/one", "k-1", "first")
}

// nopPending is a held section Run's goroutine may call freely.
type nopPending struct{}

func (nopPending) SetIngressPending(ids.WorkspaceID, []holds.IngressEntry) {}
func (nopPending) ClaimIngress(ids.WorkspaceID, string, ids.TurnID)        {}
func (nopPending) DropIngress(ids.WorkspaceID, string)                     {}

func TestRunIngestsAnEntryOnTheChangeThatLetsItIn(t *testing.T) {
	tests := []struct {
		name string
		// unmade leaves the ingress directory unmade until the entry.
		unmade bool
		// before runs before Run starts; after, once Run has started.
		before, after func(t *testing.T, r *runWorld)
	}{
		{name: "an entry on disk before the start is taken by the start's sweep",
			before: func(t *testing.T, r *runWorld) { r.writeIn(t, "held_20260928T120001_a.json") }},
		{name: "an entry written while running is taken on the directory's change",
			after: func(t *testing.T, r *runWorld) { r.writeIn(t, "held_20260928T120001_a.json") }},
		{name: "an entry written into a directory made while running is taken", unmade: true,
			after: func(t *testing.T, r *runWorld) { r.writeIn(t, "held_20260928T120001_a.json") }},
		{name: "an entry left while the daemon did not serve is taken once it does",
			before: func(t *testing.T, r *runWorld) {
				r.serves.Store(false)
				r.writeIn(t, "held_20260928T120001_a.json")
			},
			after: func(_ *testing.T, r *runWorld) { r.openIntake() }},
		{name: "an entry the queue refused is taken at the next change of the fleet",
			before: func(t *testing.T, r *runWorld) {
				r.refusals.Store(1)
				r.writeIn(t, "held_20260928T120001_a.json")
			},
			after: func(_ *testing.T, r *runWorld) {
				<-r.calls // the refused submission
				r.fleet <- struct{}{}
			}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			dir := root
			if tc.unmade {
				dir = filepath.Join(root, "held-prompts")
			}
			r := newRunWorld(t, dir)
			if tc.before != nil {
				tc.before(t, r)
			}

			// Act
			r.run(t)
			if tc.after != nil {
				tc.after(t, r)
			}
			got := <-r.published

			// Assert
			if got != "ws-one" {
				t.Fatalf("published = %q, want ws-one once its entry was taken", got)
			}
		})
	}
}

func TestRunTakesAFleetChangeWithNothingWaitingAsNoSweep(t *testing.T) {
	// Arrange
	r := newRunWorld(t, t.TempDir())
	r.run(t)

	// Act: a fleet change Run takes (the send completes only once Run has
	// received it), then an entry written.
	r.fleet <- struct{}{}
	r.writeIn(t, "held_20260928T120001_a.json")
	<-r.published

	// Assert: every sweep after the start was woken by the directory.
	for _, rec := range r.records("debug", opRun) {
		if woke, _ := rec.Context["woke"].(string); woke != "" && woke != wokeByDir {
			t.Fatalf("a sweep was woken by %q with nothing waiting, want only %q", woke, wokeByDir)
		}
	}
}

func TestRunEndsWithItsContext(t *testing.T) {
	// Arrange
	r := newRunWorld(t, t.TempDir())
	cancel, done := r.run(t)

	// Act
	cancel()
	err := <-done

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run = %v, want context.Canceled", err)
	}
}

func TestRunRefusesAnIngressItCannotWatch(t *testing.T) {
	// Arrange: neither the directory nor its parent exists.
	r := newRunWorld(t, filepath.Join(t.TempDir(), "gone", "held-prompts"))
	_, done := r.run(t)

	// Act
	err := <-done

	// Assert
	if !errors.Is(err, kernelevent.ErrNothingToWatch) {
		t.Fatalf("Run = %v, want the missing parent refused", err)
	}
	if got := len(r.records("error", opRun)); got != 1 {
		t.Fatalf("ERROR %s records = %d, want 1", opRun, got)
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
	other, held, err := recordlock.HoldIn(filepath.Join(w.dir, LockName))
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

func TestAnEntryRefusedAgainKeepsStatingWhatItWaitsOn(t *testing.T) {
	// Arrange: the first sweep is refused for want of a session.
	w := newWorld(t)
	w.handler.refuse["k-1"] = promptqueue.ErrNoSession
	w.write("held_20260928T120001_a.json", "/work/one", "k-1", "first")
	in := w.ingress()
	w.sweep(in)

	// Act: the next sweep submits it again, and it is refused again.
	w.sweep(in)

	// Assert
	if calls := len(w.handler.calls); calls != 2 {
		t.Fatalf("submissions = %d, want the entry submitted again at the next sweep", calls)
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
