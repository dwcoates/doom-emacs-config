package appfreeze

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"sync"
	"syscall"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// handshakeBound bounds each wait on the follower's loop in these tests: a
// catch-up over a fixed table with no host tool takes microseconds, and the
// bound only ends a test that would otherwise hang.
const handshakeBound = 5 * time.Second

// fakeSpawns is a scripted follower of process starts. Each Wait announces
// itself on waiting, then returns the next value sent on fire, or ctx's end.
type fakeSpawns struct {
	fire    chan error
	waiting chan struct{}
	closed  chan struct{}
	mu      sync.Mutex
	forks   []int
	execs   []int
	execErr map[int]error
	once    sync.Once
}

func newFakeSpawns() *fakeSpawns {
	return &fakeSpawns{fire: make(chan error), waiting: make(chan struct{}, 16), closed: make(chan struct{}), execErr: map[int]error{}}
}

func (s *fakeSpawns) FollowForks(pid int) (bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.forks = append(s.forks, pid)
	return true, nil
}

func (s *fakeSpawns) FollowExec(pid int) (bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if err := s.execErr[pid]; err != nil {
		return false, err
	}
	s.execs = append(s.execs, pid)
	return true, nil
}

func (s *fakeSpawns) Wait(ctx context.Context) error {
	s.waiting <- struct{}{}
	select {
	case err := <-s.fire:
		return err
	case <-ctx.Done():
		return ctx.Err()
	}
}

func (s *fakeSpawns) Close() { s.once.Do(func() { close(s.closed) }) }

func (s *fakeSpawns) followedExecs() []int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return slices.Clone(s.execs)
}

// await reads ch within handshakeBound.
func await(t *testing.T, ch <-chan struct{}, what string) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(handshakeBound):
		t.Fatalf("%s did not happen within %v", what, handshakeBound)
	}
}

// freezeAndFollow freezes r's applications and waits until the follower's
// loop is waiting for its first report.
func (r *rig) freezeAndFollow(t *testing.T) {
	t.Helper()
	r.f.Freeze(context.Background(), "lid_closed")
	await(t, r.spawns.waiting, "the follower's first wait")
	r.events = nil
}

// report reports one process start and waits until the catch-up it asks
// for is done (the loop waits again).
func (r *rig) report(t *testing.T) {
	t.Helper()
	select {
	case r.spawns.fire <- nil:
	case <-time.After(handshakeBound):
		t.Fatalf("the follower did not take a report within %v", handshakeBound)
	}
	await(t, r.spawns.waiting, "the catch-up after a report")
}

var slackLate = Process{PID: 203, PPID: 1, Path: "/Applications/Slack.app/Contents/PlugIns/ServiceExtension.appex/Contents/MacOS/ServiceExtension"}

func TestALateProcess(t *testing.T) {
	tests := []struct {
		name        string
		signalErrs  map[int]error
		wantEvents  []string
		wantKept    map[string][]int
		wantChanged bool
		wantErrors  int
	}{
		{name: "one started while the lid is closed is kept before its stop, then frozen",
			wantEvents:  []string{"save:map[Slack:[200 201 202 203]]", "stop:203", "save:map[Slack:[200 201 202 203]]"},
			wantKept:    map[string][]int{"Slack": {200, 201, 202, 203}},
			wantChanged: true},
		{name: "one that exits before its stop is left out quietly",
			signalErrs: map[int]error{203: syscall.ESRCH},
			wantEvents: []string{"save:map[Slack:[200 201 202 203]]", "stop:203", "save:map[Slack:[200 201 202]]"},
			wantKept:   map[string][]int{"Slack": {200, 201, 202}}},
		{name: "one whose stop is refused is recorded and not tried again",
			signalErrs: map[int]error{203: syscall.EPERM},
			wantEvents: []string{"save:map[Slack:[200 201 202 203]]", "stop:203", "save:map[Slack:[200 201 202]]"},
			wantKept:   map[string][]int{"Slack": {200, 201, 202}}, wantErrors: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t, []string{"Slack"}, tt.signalErrs, nil, nil)
			r.freezeAndFollow(t)
			r.table = append(r.table, slackLate)

			// Act: the start is reported twice, so a pid tried once is not tried again.
			r.report(t)
			r.report(t)

			// Assert
			if !reflect.DeepEqual(r.events, tt.wantEvents) {
				t.Errorf("events = %q, want %q", r.events, tt.wantEvents)
			}
			if got := describe(*r.store.rec); !reflect.DeepEqual(got, tt.wantKept) {
				t.Errorf("kept = %v, want %v", got, tt.wantKept)
			}
			if got := len(r.changes) > 0; got != tt.wantChanged {
				t.Errorf("OnChange told = %v, want %v", got, tt.wantChanged)
			}
			if got := len(r.records(dlog.LevelError)); got != tt.wantErrors {
				t.Errorf("ERROR records = %+v, want %d", r.records(dlog.LevelError), tt.wantErrors)
			}
		})
	}
}

func TestALateFreezeIsRecordedAtInfoWithItsProcesses(t *testing.T) {
	// Arrange
	r := newRig(t, []string{"Slack"}, nil, nil, nil)
	r.freezeAndFollow(t)
	r.table = append(r.table, slackLate)

	// Act
	r.report(t)

	// Assert
	var late []dlog.Record
	for _, rec := range r.records(dlog.LevelInfo) {
		if rec.Context["trigger"] == triggerStarted {
			late = append(late, rec)
		}
	}
	if len(late) != 1 || !reflect.DeepEqual(late[0].Context["frozen"], map[string][]int{"Slack": {203}}) {
		t.Fatalf("late INFO records = %+v, want one naming Slack's pid 203", late)
	}
}

func TestALaunchdChildIsFollowedUntilItExecsIntoAConfiguredApplication(t *testing.T) {
	// Arrange: launchd starts 700 as xpcproxy.
	r := newRig(t, []string{"Slack"}, nil, nil, nil)
	r.freezeAndFollow(t)
	r.table = append(r.table, Process{PID: 700, PPID: 1, Path: "/usr/libexec/xpcproxy"})
	r.report(t)
	if !slices.Contains(r.spawns.followedExecs(), 700) || len(r.events) != 0 {
		t.Fatalf("execs followed %v, events %q: want 700 followed and nothing frozen", r.spawns.followedExecs(), r.events)
	}

	// Act: 700 execs into Slack's service.
	r.table[len(r.table)-1].Path = slackLate.Path
	r.report(t)

	// Assert
	if !slices.Contains(r.events, "stop:700") || !slices.Contains(describe(*r.store.rec)["Slack"], 700) {
		t.Fatalf("events %q, kept %v: want 700 kept and stopped", r.events, describe(*r.store.rec))
	}
}

func TestEveryLaunchdChildPresentAtTheFreezeIsFollowedForItsExec(t *testing.T) {
	// Arrange
	r := newRig(t, []string{"Slack"}, nil, nil, nil)

	// Act
	r.freezeAndFollow(t)

	// Assert: every ppid-1 process of the table, and launchd's forks.
	want := []int{90, 200, 202, 300, 400, 500}
	if got := r.spawns.followedExecs(); !reflect.DeepEqual(got, want) {
		t.Fatalf("execs followed %v, want %v", got, want)
	}
	if !reflect.DeepEqual(r.spawns.forks, []int{launchdPID}) {
		t.Fatalf("forks followed %v, want launchd's", r.spawns.forks)
	}
}

func TestAnExecThatCannotBeFollowedIsRecordedAndTheFreezeGoesOn(t *testing.T) {
	// Arrange
	r := newRig(t, []string{"Slack"}, nil, nil, nil)
	r.spawns.execErr[300] = errors.New("EPERM")

	// Act
	frozen := r.f.Freeze(context.Background(), "lid_closed")

	// Assert
	errs := r.records(dlog.LevelError)
	if len(errs) != 1 || errs[0].Context["pid"] != 300 {
		t.Fatalf("ERROR records = %+v, want one naming pid 300", errs)
	}
	if len(frozen) != 1 || frozen[0].Processes != 3 {
		t.Fatalf("Freeze() = %v, want Slack's three processes", frozen)
	}
}

func TestTheThawCoversEveryProcessFrozenAndEndsTheFollower(t *testing.T) {
	// Arrange: frozen at the close, one missed by a kept record, one late.
	store := &memStore{rec: &Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{200}}}}}
	r := newRig(t, []string{"Slack"}, nil, nil, store)
	r.freezeAndFollow(t)
	r.table = append(r.table, slackLate)
	r.report(t)
	r.events = nil

	// Act
	r.f.Thaw(context.Background(), "lid_open")

	// Assert
	want := []string{"cont:200", "cont:201", "cont:202", "cont:203", "clear"}
	if !reflect.DeepEqual(r.events, want) {
		t.Fatalf("events = %q, want %q", r.events, want)
	}
	await(t, r.spawns.closed, "the follower's close")
}

func TestAFollowerThatCannotBeOpenedIsRecordedAndTheFreezeGoesOn(t *testing.T) {
	// Arrange
	r := newRig(t, []string{"Slack"}, nil, nil, nil)
	r.f.d.Spawns = func() (SpawnFollower, error) { return nil, errors.New("kqueue: too many open files") }

	// Act
	frozen := r.f.Freeze(context.Background(), "lid_closed")

	// Assert
	errs := r.records(dlog.LevelError)
	if len(errs) != 1 || errs[0].Context["cause"] != "kqueue: too many open files" || errs[0].Context["trigger"] != "lid_closed" {
		t.Fatalf("ERROR records = %+v, want the follower's failure with its cause", errs)
	}
	if len(frozen) != 1 || frozen[0].Processes != 3 {
		t.Fatalf("Freeze() = %v, want Slack's three processes", frozen)
	}
}

func TestAFollowerThatFailsWhileTheLidIsClosedIsRecordedAndCloses(t *testing.T) {
	// Arrange
	r := newRig(t, []string{"Slack"}, nil, nil, nil)
	r.freezeAndFollow(t)

	// Act
	r.spawns.fire <- errors.New("kevent: bad file descriptor")
	await(t, r.spawns.closed, "the follower's close")

	// Assert
	errs := r.records(dlog.LevelError)
	if len(errs) != 1 || errs[0].Context["cause"] != "kevent: bad file descriptor" {
		t.Fatalf("ERROR records = %+v, want the follower's failure with its cause", errs)
	}
}

func TestMerge(t *testing.T) {
	tests := []struct {
		name string
		a, b Record
		want Record
	}{
		{name: "an application in both gets both's pids",
			a:    Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{1}}}},
			b:    Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{2}}}},
			want: Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{1, 2}}}}},
		{name: "an application only in b follows a's",
			a:    Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{1}}}},
			b:    Record{Apps: []RecordedApp{{Name: "Spotify", PIDs: []int{2}}}},
			want: Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{1}}, {Name: "Spotify", PIDs: []int{2}}}}},
		{name: "an application with no pids is dropped",
			a:    Record{Apps: []RecordedApp{{Name: "Slack"}}},
			want: Record{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := merge(tt.a, tt.b)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("merge = %+v, want %+v", got, tt.want)
			}
		})
	}
}
