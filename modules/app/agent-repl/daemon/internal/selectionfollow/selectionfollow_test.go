package selectionfollow

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
)

var errScripted = errors.New("scripted failure")

// fakeVersions answers scripted data_version reads.
type fakeVersions struct {
	mu      sync.Mutex
	version int64
	err     error
}

func (v *fakeVersions) DataVersion(context.Context) (int64, error) {
	v.mu.Lock()
	defer v.mu.Unlock()
	return v.version, v.err
}

func (v *fakeVersions) set(version int64) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.version = version
}

// fakeFollow counts follows and answers a scripted error.
type fakeFollow struct {
	mu    sync.Mutex
	calls int
	err   error
}

func (f *fakeFollow) follow(context.Context) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls++
	return f.err
}

func (f *fakeFollow) count() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.calls
}

type world struct {
	versions *fakeVersions
	follow   *fakeFollow
	clock    *clocktest.Scripted
	log      *dlog.TestLogger
	follower *Follower
}

func newWorld(t *testing.T) *world {
	t.Helper()
	w := &world{
		versions: &fakeVersions{version: 7},
		follow:   &fakeFollow{},
		clock:    clocktest.New(time.Time{}),
		log:      dlog.NewTestLogger(),
	}
	follower, err := New(Deps{Versions: w.versions, Follow: w.follow.follow, Clock: w.clock, Every: DefaultEvery, Log: w.log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	w.follower = follower
	return w
}

func (w *world) level(message string) string {
	for _, r := range w.log.Records() {
		if r.Message == message {
			return r.Level
		}
	}
	return ""
}

func TestCheckFollowsBeforeAnyFollowHasLanded(t *testing.T) {
	// Arrange.
	w := newWorld(t)

	// Act.
	w.follower.Check(context.Background())

	// Assert.
	if got := w.follow.count(); got != 1 {
		t.Fatalf("follows = %d, want 1", got)
	}
}

func TestCheckDoesNotFollowWhenNoOtherDaemonCommitted(t *testing.T) {
	// Arrange: one follow landed at version 7.
	w := newWorld(t)
	w.follower.Check(context.Background())

	// Act: data_version has not moved.
	w.follower.Check(context.Background())

	// Assert.
	if got := w.follow.count(); got != 1 {
		t.Fatalf("follows = %d, want 1", got)
	}
}

func TestCheckFollowsWhenAnotherDaemonCommitted(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.follower.Check(context.Background())

	// Act.
	w.versions.set(8)
	w.follower.Check(context.Background())

	// Assert.
	if got := w.follow.count(); got != 2 {
		t.Fatalf("follows = %d, want 2", got)
	}
}

func TestCheckRetriesAFollowThatDidNotLand(t *testing.T) {
	// Arrange: the first follow fails at version 7.
	w := newWorld(t)
	w.follow.err = errScripted
	w.follower.Check(context.Background())
	w.follow.err = nil

	// Act: data_version has not moved since.
	w.follower.Check(context.Background())

	// Assert.
	if got := w.follow.count(); got != 2 {
		t.Fatalf("follows = %d, want 2", got)
	}
}

func TestCheckRecordsAnUnreadableDataVersionAtErrorAndDoesNotFollow(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.versions.err = errScripted

	// Act.
	w.follower.Check(context.Background())

	// Assert.
	if got := w.follow.count(); got != 0 {
		t.Fatalf("follows = %d, want 0", got)
	}
	if got := w.level("could not read the state file's data_version; another daemon's selection is not followed this tick"); got != "error" {
		t.Fatalf("the failed read was recorded at %q, want error", got)
	}
}

func TestRunChecksOnceBeforeItsFirstWait(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act.
	go func() { done <- w.follower.Run(ctx) }()
	<-w.clock.Asked

	// Assert.
	if got := w.follow.count(); got != 1 {
		t.Fatalf("follows before the first wait = %d, want 1", got)
	}
	cancel()
	if err := <-done; !errors.Is(err, context.Canceled) {
		t.Fatalf("Run answered %v, want context.Canceled", err)
	}
}

func TestRunChecksAgainOnEveryTick(t *testing.T) {
	// Arrange: the opening check has landed and the schedule waits.
	w := newWorld(t)
	go func() { _ = w.follower.Run(t.Context()) }()
	<-w.clock.Asked
	w.versions.set(8)

	// Act.
	w.clock.Fire <- time.Time{}
	<-w.clock.Asked

	// Assert.
	if got := w.follow.count(); got != 2 {
		t.Fatalf("follows after one tick = %d, want 2", got)
	}
}

func TestNewRefusesAnIncompleteDeps(t *testing.T) {
	full := func() Deps {
		return Deps{Versions: &fakeVersions{}, Follow: (&fakeFollow{}).follow, Clock: clocktest.New(time.Time{}), Every: DefaultEvery, Log: dlog.NewTestLogger()}
	}
	cases := []struct {
		name   string
		break_ func(*Deps)
	}{
		{name: "no versions", break_: func(d *Deps) { d.Versions = nil }},
		{name: "no follow", break_: func(d *Deps) { d.Follow = nil }},
		{name: "no clock", break_: func(d *Deps) { d.Clock = nil }},
		{name: "no log", break_: func(d *Deps) { d.Log = nil }},
		{name: "a zero cadence", break_: func(d *Deps) { d.Every = 0 }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			deps := full()
			tc.break_(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatal("New accepted an incomplete Deps")
			}
		})
	}
}
