package netwatch

import (
	"context"
	"errors"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
)

// fakeSource answers the changes and errors a test feeds it, one per Next.
type fakeSource struct {
	next      chan fakeEvent
	closeOnce sync.Once
	closed    chan struct{}
}

type fakeEvent struct {
	change Change
	err    error
}

func newFakeSource() *fakeSource {
	return &fakeSource{next: make(chan fakeEvent), closed: make(chan struct{})}
}

func (s *fakeSource) Next() (Change, error) {
	select {
	case e := <-s.next:
		return e.change, e.err
	case <-s.closed:
		return Change{}, errors.New("closed")
	}
}

func (s *fakeSource) Close() error {
	s.closeOnce.Do(func() { close(s.closed) })
	return nil
}

func hasRecord(log *dlog.TestLogger, level, message string) (dlog.Record, bool) {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == opWatch && r.Message == message {
			return r, true
		}
	}
	return dlog.Record{}, false
}

func TestNewRefusesMissingDeps(t *testing.T) {
	tests := []struct {
		name string
		open func() (Source, error)
		log  dlog.Logger
	}{
		{name: "no source", log: dlog.NewTestLogger()},
		{name: "no logger", open: Silent},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := New(tt.open, tt.log)

			// Assert.
			if err == nil {
				t.Fatal("New() error = nil, want a refusal")
			}
		})
	}
}

func TestChangedIsClosedAtEachChangeAndReplaced(t *testing.T) {
	// Arrange.
	src := newFakeSource()
	log := dlog.NewTestLogger()
	w, err := New(func() (Source, error) { return src, nil }, log)
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- w.Run(ctx) }()
	first := w.Changed()

	// Act.
	src.next <- fakeEvent{change: Change{Kind: "address_added", Interface: "en0", Detail: "192.168.1.5"}}
	<-first
	second := w.Changed()
	cancel()
	runErr := <-done

	// Assert.
	if first == second {
		t.Fatal("Changed() answered the closed channel after the change, want a fresh one")
	}
	select {
	case <-second:
		t.Fatal("the fresh channel is closed with no second change")
	default:
	}
	if runErr != nil {
		t.Fatalf("Run() = %v after its context ended, want nil", runErr)
	}
	record, ok := hasRecord(log, "info", "the network changed; whatever waits on it is checked now")
	if !ok {
		t.Fatalf("no info record of the change: %+v", log.Records())
	}
	if record.Context["kind"] != "address_added" || record.Context["interface"] != "en0" || record.Context["detail"] != "192.168.1.5" {
		t.Fatalf("change record context = %+v", record.Context)
	}
}

func TestRunFailsLoudly(t *testing.T) {
	tests := []struct {
		name    string
		open    func(*fakeSource) func() (Source, error)
		feed    func(*fakeSource)
		message string
	}{
		{
			name: "the source cannot be opened",
			open: func(*fakeSource) func() (Source, error) {
				return func() (Source, error) { return nil, errors.New("no routing socket") }
			},
			feed:    func(*fakeSource) {},
			message: "the operating system's network events could not be opened; a reconnect is noticed only at each wait's next periodic check",
		},
		{
			name: "the source fails while the context is live",
			open: func(s *fakeSource) func() (Source, error) {
				return func() (Source, error) { return s, nil }
			},
			feed:    func(s *fakeSource) { s.next <- fakeEvent{err: errors.New("no routing socket")} },
			message: "the operating system's network events failed; a reconnect is noticed only at each wait's next periodic check",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			src := newFakeSource()
			log := dlog.NewTestLogger()
			w, err := New(tt.open(src), log)
			if err != nil {
				t.Fatal(err)
			}
			done := make(chan error, 1)
			before := w.Changed()

			// Act.
			go func() { done <- w.Run(context.Background()) }()
			tt.feed(src)
			runErr := <-done

			// Assert.
			if runErr == nil {
				t.Fatal("Run() = nil, want the cause")
			}
			record, ok := hasRecord(log, "error", tt.message)
			if !ok {
				t.Fatalf("no error record %q: %+v", tt.message, log.Records())
			}
			if record.Context["cause"] != "no routing socket" {
				t.Fatalf("cause = %v, want the source's", record.Context["cause"])
			}
			select {
			case <-before:
				t.Fatal("a failure was announced as a change")
			default:
			}
		})
	}
}

func TestRunClosesTheSourceWhenItEnds(t *testing.T) {
	// Arrange.
	src := newFakeSource()
	w, err := New(func() (Source, error) { return src, nil }, dlog.NewTestLogger())
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() { done <- w.Run(context.Background()) }()

	// Act.
	src.next <- fakeEvent{err: errors.New("broken")}
	<-done

	// Assert.
	select {
	case <-src.closed:
	default:
		t.Fatal("the source was left open after Run ended")
	}
}

// broadcaster is a hand-driven Changed for Follow's tests.
type broadcaster struct {
	mu sync.Mutex
	ch chan struct{}
	// taken is told each time Follow takes a channel, so a test fires only
	// once Follow is listening.
	taken chan struct{}
}

func newBroadcaster() *broadcaster {
	return &broadcaster{ch: make(chan struct{}), taken: make(chan struct{}, 16)}
}

func (b *broadcaster) changed() <-chan struct{} {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.taken <- struct{}{}
	return b.ch
}

func (b *broadcaster) fire() {
	b.mu.Lock()
	defer b.mu.Unlock()
	close(b.ch)
	b.ch = make(chan struct{})
}

func TestFollowCallsOncePerChange(t *testing.T) {
	// Arrange.
	b := newBroadcaster()
	ctx, cancel := context.WithCancel(context.Background())
	calls := make(chan struct{})
	finished := make(chan struct{})
	go func() {
		Follow(ctx, b.changed, func(context.Context) { calls <- struct{}{} })
		close(finished)
	}()

	<-b.taken

	// Act.
	b.fire()
	<-calls
	cancel()
	<-finished

	// Assert: reaching here is the assertion; one change made one call, and
	// the end of the context ended Follow.
}

func TestFollowNeverMissesAChangeDuringACall(t *testing.T) {
	// Arrange.
	b := newBroadcaster()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	entered := make(chan struct{})
	release := make(chan struct{})
	var mu sync.Mutex
	count := 0
	go Follow(ctx, b.changed, func(context.Context) {
		mu.Lock()
		count++
		mu.Unlock()
		entered <- struct{}{}
		<-release
	})

	<-b.taken

	// Act: a change starts a call; three more land while it runs.
	b.fire()
	<-entered
	b.fire()
	b.fire()
	b.fire()
	release <- struct{}{}
	<-entered
	release <- struct{}{}

	// Assert: the three folded into exactly one more call.
	mu.Lock()
	got := count
	mu.Unlock()
	if got != 2 {
		t.Fatalf("Follow called fn %d times, want 2", got)
	}
	select {
	case <-entered:
		t.Fatal("Follow called fn a third time with no change after the second call began")
	default:
	}
}

func TestSilentReportsNothingUntilClosed(t *testing.T) {
	// Arrange.
	src, err := Silent()
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() {
		_, err := src.Next()
		done <- err
	}()

	// Act.
	_ = src.Close()
	_ = src.Close()

	// Assert.
	if err := <-done; err == nil {
		t.Fatal("Next() after Close = nil error, want the close")
	}
}

// failingClose is a source whose Close fails.
type failingClose struct{ *fakeSource }

func (s failingClose) Close() error {
	_ = s.fakeSource.Close()
	return errors.New("bad descriptor")
}

func TestRunRecordsASourceThatFailsToClose(t *testing.T) {
	// Arrange.
	src := failingClose{newFakeSource()}
	log := dlog.NewTestLogger()
	w, err := New(func() (Source, error) { return src, nil }, log)
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- w.Run(ctx) }()

	// Act.
	cancel()
	<-done

	// Assert.
	record, ok := hasRecord(log, "error", "the operating system's network events could not be closed")
	if !ok || record.Context["cause"] != "bad descriptor" {
		t.Fatalf("no error record of the failed close: %+v", log.Records())
	}
}
