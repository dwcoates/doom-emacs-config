package selectionfollow

import (
	"context"
	"errors"
	"sync"
	"testing"

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

// fakeFollow counts follows and answers a scripted error. followed, when
// set, is told of each follow.
type fakeFollow struct {
	mu       sync.Mutex
	calls    int
	err      error
	followed chan struct{}
}

func (f *fakeFollow) follow(context.Context) error {
	f.mu.Lock()
	f.calls++
	err, followed := f.err, f.followed
	f.mu.Unlock()
	if followed != nil {
		followed <- struct{}{}
	}
	return err
}

func (f *fakeFollow) count() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.calls
}

// fakeChanges is a scripted commit signal: a test sends on commits, and
// watching is closed once Run has taken the watch.
type fakeChanges struct {
	commits  chan error
	watching chan struct{}
}

func (c *fakeChanges) source(context.Context) (<-chan error, func()) {
	close(c.watching)
	return c.commits, func() {}
}

type world struct {
	versions *fakeVersions
	follow   *fakeFollow
	changes  *fakeChanges
	log      *dlog.TestLogger
	follower *Follower
}

func newWorld(t *testing.T) *world {
	t.Helper()
	w := &world{
		versions: &fakeVersions{version: 7},
		follow:   &fakeFollow{followed: make(chan struct{}, 8)},
		changes:  &fakeChanges{commits: make(chan error), watching: make(chan struct{})},
		log:      dlog.NewTestLogger(),
	}
	follower, err := New(Deps{Versions: w.versions, Follow: w.follow.follow, Changes: w.changes.source, Log: w.log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	w.follower = follower
	return w
}

func (w *world) level(message string) string {
	r, _ := dlog.FindRecord(w.log.Records(), dlog.RecordQuery{Message: message})
	return r.Level
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
	if got := w.level("could not read the state file's data_version; another daemon's selection is not followed on this commit"); got != "error" {
		t.Fatalf("the failed read was recorded at %q, want error", got)
	}
}

func TestRunChecksOnceAfterTakingItsWatch(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act.
	go func() { done <- w.follower.Run(ctx) }()
	<-w.changes.watching
	<-w.follow.followed
	w.versions.set(8)
	w.changes.commits <- nil
	<-w.follow.followed
	cancel()
	err := <-done

	// Assert.
	if got := w.follow.count(); got != 2 {
		t.Fatalf("follows = %d, want 2: the opening check and the commit's", got)
	}
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run answered %v, want context.Canceled", err)
	}
}

func TestRunChecksAgainOnEveryCommit(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- w.follower.Run(ctx) }()
	<-w.changes.watching
	<-w.follow.followed

	// Act: two commits that each moved data_version.
	for v := int64(8); v <= 9; v++ {
		w.versions.set(v)
		w.changes.commits <- nil
		<-w.follow.followed
	}
	cancel()
	<-done

	// Assert.
	if got := w.follow.count(); got != 3 {
		t.Fatalf("follows = %d, want 3", got)
	}
}

func TestRunEndsWithTheWatchsFailure(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	done := make(chan error, 1)
	go func() { done <- w.follower.Run(t.Context()) }()
	<-w.changes.watching

	// Act.
	w.changes.commits <- errScripted
	err := <-done

	// Assert.
	if !errors.Is(err, errScripted) {
		t.Fatalf("Run answered %v, want the watch's failure", err)
	}
}

func TestRunRecordsTheWatchsFailureAtError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	done := make(chan error, 1)
	go func() { done <- w.follower.Run(t.Context()) }()
	<-w.changes.watching

	// Act.
	w.changes.commits <- errScripted
	<-done

	// Assert.
	if got := w.level("the state file's commits could not be followed; another daemon's selection is no longer followed"); got != "error" {
		t.Fatalf("the failed watch was recorded at %q, want error", got)
	}
}

func TestNewRefusesAnIncompleteDeps(t *testing.T) {
	full := func() Deps {
		return Deps{Versions: &fakeVersions{}, Follow: (&fakeFollow{}).follow, Changes: (&fakeChanges{}).source, Log: dlog.NewTestLogger()}
	}
	cases := []struct {
		name   string
		break_ func(*Deps)
	}{
		{name: "no versions", break_: func(d *Deps) { d.Versions = nil }},
		{name: "no follow", break_: func(d *Deps) { d.Follow = nil }},
		{name: "no changes", break_: func(d *Deps) { d.Changes = nil }},
		{name: "no log", break_: func(d *Deps) { d.Log = nil }},
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
