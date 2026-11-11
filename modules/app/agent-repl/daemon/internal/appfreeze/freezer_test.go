package appfreeze

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"syscall"
	"testing"

	"agent-repld/internal/dlog"
)

// memStore is an in-memory Store whose every operation can fail, and which
// records the order of saves, clears and the signals interleaved with them.
type memStore struct {
	rec              *Record
	loadErr, saveErr error
	clearErr         error
	// failSaveAfter fails every save after the first n (0 never fails).
	failSaveAfter int
	saves         int
	events        *[]string
}

func (m *memStore) Load() (Record, bool, error) {
	if m.loadErr != nil {
		return Record{}, false, m.loadErr
	}
	if m.rec == nil {
		return Record{}, false, nil
	}
	return *m.rec, true, nil
}

func (m *memStore) Save(r Record) error {
	m.saves++
	if m.saveErr != nil || (m.failSaveAfter > 0 && m.saves > m.failSaveAfter) {
		*m.events = append(*m.events, "save-failed")
		if m.saveErr != nil {
			return m.saveErr
		}
		return errors.New("disk full")
	}
	m.rec = &r
	*m.events = append(*m.events, fmt.Sprintf("save:%v", describe(r)))
	return nil
}

func (m *memStore) Clear() error {
	if m.clearErr != nil {
		*m.events = append(*m.events, "clear-failed")
		return m.clearErr
	}
	m.rec = nil
	*m.events = append(*m.events, "clear")
	return nil
}

// signalLog records every signal in events and answers each pid's scripted
// error.
func signalLog(events *[]string, errs map[int]error) Signaler {
	return func(pid int, sig syscall.Signal) error {
		name := map[syscall.Signal]string{syscall.SIGSTOP: "stop", syscall.SIGCONT: "cont"}[sig]
		*events = append(*events, fmt.Sprintf("%s:%d", name, pid))
		return errs[pid]
	}
}

type countTelemetry struct{ frozen, thawed []string }

func (c *countTelemetry) CountAppFrozen(app string) { c.frozen = append(c.frozen, app) }
func (c *countTelemetry) CountAppThawed(app string) { c.thawed = append(c.thawed, app) }

// table is a process table: the daemon (100) under Emacs (90), Slack and
// two helpers, Chrome with a renderer, Chrome for Testing, and a Chrome an
// agent session spawned (under the sidecar).
var table = []Process{
	{PID: 90, PPID: 1, Path: "/Applications/Emacs.app/Contents/MacOS/Emacs"},
	{PID: 100, PPID: 90, Path: "/Users/u/agent-repl/daemon/bin/agent-repld"},
	{PID: 200, PPID: 1, Path: "/Applications/Slack.app/Contents/MacOS/Slack"},
	{PID: 201, PPID: 200, Path: "/Applications/Slack.app/Contents/Frameworks/Slack Helper.app/Contents/MacOS/Slack Helper"},
	{PID: 202, PPID: 1, Path: "/Applications/Slack.app/Contents/Frameworks/Squirrel.framework/Resources/ShipIt"},
	{PID: 300, PPID: 1, Path: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"},
	{PID: 301, PPID: 300, Path: "/Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)"},
	{PID: 400, PPID: 1, Path: "/Applications/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing"},
	{PID: 500, PPID: 1, Path: "/Users/u/.cache/agent-repl/bin/shim-claude-sidecar"},
	{PID: 501, PPID: 500, Path: "/opt/homebrew/bin/node"},
	{PID: 502, PPID: 501, Path: "/Applications/Spotify.app/Contents/MacOS/Spotify"},
}

var protection = Protection{Self: 100, PathPrefixes: []string{"/Users/u/agent-repl/daemon/bin", "/Users/u/.cache/agent-repl"}}

type rig struct {
	f      *Freezer
	store  *memStore
	events []string
	log    *dlog.TestLogger
	tel    *countTelemetry
}

func newRig(t *testing.T, apps []string, signalErrs map[int]error, listErr error, store *memStore) *rig {
	t.Helper()
	r := &rig{log: dlog.NewTestLogger(), tel: &countTelemetry{}}
	if store == nil {
		store = &memStore{}
	}
	store.events = &r.events
	r.store = store
	f, err := New(Deps{
		Apps: apps,
		Processes: func(context.Context) ([]Process, error) {
			if listErr != nil {
				return nil, listErr
			}
			return table, nil
		},
		Signal:     signalLog(&r.events, signalErrs),
		Store:      store,
		Protection: protection,
		Log:        r.log,
		Telemetry:  r.tel,
	})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}
	r.f = f
	return r
}

func (r *rig) records(level string) []dlog.Record {
	return dlog.FilterRecords(r.log.Records(), dlog.RecordQuery{Level: level, Operation: opFreeze})
}

func TestFreeze(t *testing.T) {
	tests := []struct {
		name       string
		apps       []string
		signalErrs map[int]error
		listErr    error
		store      *memStore
		wantEvents []string
		wantFrozen []Frozen
		wantErrors int
		wantKept   map[string][]int
	}{
		{name: "the record is kept before the first stop, then narrowed",
			apps:       []string{"Slack", "Google Chrome"},
			wantEvents: []string{"save:map[Google Chrome:[300 301] Slack:[200 201 202]]", "stop:200", "stop:201", "stop:202", "stop:300", "stop:301", "save:map[Google Chrome:[300 301] Slack:[200 201 202]]"},
			wantFrozen: []Frozen{{Name: "Slack", Processes: 3}, {Name: "Google Chrome", Processes: 2}},
			wantKept:   map[string][]int{"Slack": {200, 201, 202}, "Google Chrome": {300, 301}}},
		{name: "a set that cannot be kept freezes nothing",
			apps: []string{"Slack"}, store: &memStore{saveErr: errors.New("disk full")},
			wantEvents: []string{"save-failed"}, wantErrors: 1},
		{name: "a process gone before its stop is left out quietly",
			apps: []string{"Slack"}, signalErrs: map[int]error{201: syscall.ESRCH},
			wantEvents: []string{"save:map[Slack:[200 201 202]]", "stop:200", "stop:201", "stop:202", "save:map[Slack:[200 202]]"},
			wantFrozen: []Frozen{{Name: "Slack", Processes: 2}}, wantKept: map[string][]int{"Slack": {200, 202}}},
		{name: "a refused stop is recorded with its pid and errno",
			apps: []string{"Slack"}, signalErrs: map[int]error{202: syscall.EPERM},
			wantEvents: []string{"save:map[Slack:[200 201 202]]", "stop:200", "stop:201", "stop:202", "save:map[Slack:[200 201]]"},
			wantFrozen: []Frozen{{Name: "Slack", Processes: 2}}, wantErrors: 1, wantKept: map[string][]int{"Slack": {200, 201}}},
		{name: "a narrowing that cannot be kept leaves the wider set",
			apps: []string{"Slack"}, signalErrs: map[int]error{201: syscall.ESRCH}, store: &memStore{failSaveAfter: 1},
			wantEvents: []string{"save:map[Slack:[200 201 202]]", "stop:200", "stop:201", "stop:202", "save-failed"},
			wantFrozen: []Frozen{{Name: "Slack", Processes: 3}}, wantErrors: 1, wantKept: map[string][]int{"Slack": {200, 201, 202}}},
		{name: "every stop failing clears the record",
			apps: []string{"Slack"}, signalErrs: map[int]error{200: syscall.ESRCH, 201: syscall.ESRCH, 202: syscall.ESRCH},
			wantEvents: []string{"save:map[Slack:[200 201 202]]", "stop:200", "stop:201", "stop:202", "clear"},
			wantKept:   nil},
		{name: "an application with a protected process is refused whole",
			apps:       []string{"Spotify", "Slack"},
			wantEvents: []string{"save:map[Slack:[200 201 202]]", "stop:200", "stop:201", "stop:202", "save:map[Slack:[200 201 202]]"},
			wantFrozen: []Frozen{{Name: "Slack", Processes: 3}}, wantErrors: 1, wantKept: map[string][]int{"Slack": {200, 201, 202}}},
		{name: "an application not running freezes nothing", apps: []string{"WhatsApp"}},
		{name: "no applications configured reads nothing", apps: nil},
		{name: "an unreadable process table freezes nothing",
			apps: []string{"Slack"}, listErr: errors.New("ps: not found"), wantErrors: 1},
		{name: "a kept set is adopted and nothing is stopped again",
			apps: []string{"Slack"}, store: &memStore{rec: &Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{200}}}}},
			wantFrozen: []Frozen{{Name: "Slack", Processes: 1}}, wantKept: map[string][]int{"Slack": {200}}},
		{name: "an unreadable kept set freezes nothing",
			apps: []string{"Slack"}, store: &memStore{loadErr: errors.New("corrupt")}, wantErrors: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t, tt.apps, tt.signalErrs, tt.listErr, tt.store)

			// Act
			frozen := r.f.Freeze(context.Background(), "lid_closed")

			// Assert
			if !reflect.DeepEqual(r.events, tt.wantEvents) {
				t.Errorf("events = %q, want %q", r.events, tt.wantEvents)
			}
			if !reflect.DeepEqual(frozen, tt.wantFrozen) {
				t.Errorf("Freeze() = %v, want %v", frozen, tt.wantFrozen)
			}
			if got := len(r.records(dlog.LevelError)); got != tt.wantErrors {
				t.Errorf("ERROR records = %+v, want %d", r.records(dlog.LevelError), tt.wantErrors)
			}
			var kept map[string][]int
			if r.store.rec != nil && !r.store.rec.empty() {
				kept = describe(*r.store.rec)
			}
			if !reflect.DeepEqual(kept, tt.wantKept) {
				t.Errorf("kept = %v, want %v", kept, tt.wantKept)
			}
		})
	}
}

func TestFreezeRecordsARefusedStopWithItsContext(t *testing.T) {
	// Arrange
	r := newRig(t, []string{"Slack"}, map[int]error{202: syscall.EPERM}, nil, nil)

	// Act
	r.f.Freeze(context.Background(), "lid_closed")

	// Assert
	errs := r.records(dlog.LevelError)
	if len(errs) != 1 || errs[0].Context["pid"] != 202 || errs[0].Context["app"] != "Slack" || errs[0].Context["errno"] != "EPERM" {
		t.Fatalf("ERROR records = %+v, want pid 202, app Slack, errno EPERM", errs)
	}
	if !reflect.DeepEqual(r.tel.frozen, []string{"Slack"}) {
		t.Fatalf("freezes counted %v, want [Slack]", r.tel.frozen)
	}
}

func TestThaw(t *testing.T) {
	kept := func() *memStore {
		return &memStore{rec: &Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{200, 201}}, {Name: "Spotify", PIDs: []int{600}}}}}
	}
	tests := []struct {
		name       string
		store      *memStore
		signalErrs map[int]error
		wantEvents []string
		wantErrors int
		wantKept   bool
	}{
		{name: "the record is cleared only after the last continue", store: kept(),
			wantEvents: []string{"cont:200", "cont:201", "cont:600", "clear"}},
		{name: "a process gone is no failure", store: kept(), signalErrs: map[int]error{201: syscall.ESRCH},
			wantEvents: []string{"cont:200", "cont:201", "cont:600", "clear"}},
		{name: "a refused continue is recorded and the rest still thaw", store: kept(), signalErrs: map[int]error{200: syscall.EPERM},
			wantEvents: []string{"cont:200", "cont:201", "cont:600", "clear"}, wantErrors: 1},
		{name: "a record that cannot be cleared is recorded", store: func() *memStore { m := kept(); m.clearErr = errors.New("locked"); return m }(),
			wantEvents: []string{"cont:200", "cont:201", "cont:600", "clear-failed"}, wantErrors: 1, wantKept: true},
		{name: "nothing kept signals nothing", store: &memStore{}},
		{name: "an unreadable record signals nothing", store: &memStore{loadErr: errors.New("corrupt")}, wantErrors: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t, []string{"Slack"}, tt.signalErrs, nil, tt.store)

			// Act
			r.f.Thaw(context.Background(), "lid_open")

			// Assert
			if !reflect.DeepEqual(r.events, tt.wantEvents) {
				t.Errorf("events = %q, want %q", r.events, tt.wantEvents)
			}
			if got := len(r.records(dlog.LevelError)); got != tt.wantErrors {
				t.Errorf("ERROR records = %+v, want %d", r.records(dlog.LevelError), tt.wantErrors)
			}
			if got := r.store.rec != nil; got != tt.wantKept {
				t.Errorf("record kept = %v, want %v", got, tt.wantKept)
			}
			if len(r.f.Frozen()) != 0 {
				t.Errorf("Frozen() = %v after a thaw, want none", r.f.Frozen())
			}
		})
	}
}

func TestThawAfterAClearFailureFreezesAgainOnTheNextClose(t *testing.T) {
	// Arrange: the processes were thawed, the record was not cleared.
	store := &memStore{rec: &Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{200}}}}, clearErr: errors.New("locked")}
	r := newRig(t, []string{"Slack"}, nil, nil, store)
	r.f.Thaw(context.Background(), "lid_open")
	store.clearErr = nil

	// Act
	frozen := r.f.Freeze(context.Background(), "lid_closed")

	// Assert: the mirror says nothing is frozen, so the freeze runs.
	if len(frozen) != 1 || frozen[0].Processes != 3 {
		t.Fatalf("Freeze() = %v, want Slack's three processes frozen afresh", frozen)
	}
}

func TestNewRefusesIncompleteDeps(t *testing.T) {
	full := func() Deps {
		return Deps{
			Processes: func(context.Context) ([]Process, error) { return nil, nil },
			Signal:    func(int, syscall.Signal) error { return nil },
			Store:     &memStore{}, Log: dlog.NewTestLogger(),
		}
	}
	tests := []struct {
		name   string
		break_ func(*Deps)
	}{
		{name: "no lister", break_: func(d *Deps) { d.Processes = nil }},
		{name: "no signaler", break_: func(d *Deps) { d.Signal = nil }},
		{name: "no store", break_: func(d *Deps) { d.Store = nil }},
		{name: "no logger", break_: func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d := full()
			tt.break_(&d)

			// Act
			_, err := New(d)

			// Assert
			if err == nil {
				t.Fatal("New() accepted incomplete deps")
			}
		})
	}
}

func TestProtectionCoversTheDaemonItsEditorAgentRepAndAgentSessions(t *testing.T) {
	// Act
	got := protection.protected(table)

	// Assert
	for _, pid := range []int{90, 100, 500, 501, 502} {
		if !got[pid] {
			t.Errorf("pid %d is not protected", pid)
		}
	}
	for _, pid := range []int{200, 201, 202, 300, 301, 400} {
		if got[pid] {
			t.Errorf("pid %d is protected, want it freezable", pid)
		}
	}
}

func TestBundleOf(t *testing.T) {
	tests := []struct {
		path, app string
		want      bool
	}{
		{path: "/Applications/Slack.app/Contents/MacOS/Slack", app: "Slack", want: true},
		{path: "/Applications/Google Chrome.app/Contents/Frameworks/x.app/Contents/MacOS/x", app: "Google Chrome", want: true},
		{path: "/Applications/Google Chrome for Testing.app/Contents/MacOS/x", app: "Google Chrome", want: false},
		{path: "/opt/homebrew/bin/slack", app: "Slack", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			// Act and assert
			if got := bundleOf(tt.path, tt.app); got != tt.want {
				t.Fatalf("bundleOf(%q, %q) = %v, want %v", tt.path, tt.app, got, tt.want)
			}
		})
	}
}

func TestErrnoOf(t *testing.T) {
	tests := []struct {
		err  error
		want string
	}{
		{err: syscall.ESRCH, want: "ESRCH"},
		{err: syscall.EPERM, want: "EPERM"},
		{err: syscall.EINVAL, want: fmt.Sprintf("errno %d", int(syscall.EINVAL))},
		{err: errors.New("a fake host signals no process"), want: "a fake host signals no process"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act and assert
			if got := errnoOf(tt.err); got != tt.want {
				t.Fatalf("errnoOf(%v) = %q, want %q", tt.err, got, tt.want)
			}
		})
	}
}
