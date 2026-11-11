package appfreeze

import (
	"context"
	"fmt"
	"syscall"

	"agent-repld/internal/dlog"
)

const opFreeze = "daemon.appfreeze"

// Telemetry counts each application frozen and thawed. The metrics surface
// is the production one; nil counts nothing.
type Telemetry interface {
	CountAppFrozen(app string)
	CountAppThawed(app string)
}

// Deps are the freezer's collaborators.
type Deps struct {
	// Apps is the configured list (AppsFromEnv).
	Apps []string
	// Processes reads the process table. REQUIRED.
	Processes Lister
	// Signal sends one signal. REQUIRED.
	Signal Signaler
	// Store keeps the frozen set durably. REQUIRED.
	Store Store
	// Protection names the processes never frozen.
	Protection Protection
	// Log is the package's canonical logger. REQUIRED.
	Log dlog.Logger
	// Telemetry counts freezes and thaws; nil counts nothing.
	Telemetry Telemetry
}

// Frozen is one application the freezer holds frozen, for the standing.
type Frozen struct {
	Name      string
	Processes int
}

// Freezer freezes and thaws the configured applications. Its caller
// serializes Freeze and Thaw (the persistent-wifi controller's lock), so it
// keeps no lock of its own.
type Freezer struct {
	d Deps
	// held mirrors the kept record once loaded; loaded says it was read.
	held   Record
	loaded bool
}

// New builds the freezer.
func New(d Deps) (*Freezer, error) {
	switch {
	case d.Processes == nil:
		return nil, fmt.Errorf("appfreeze: a process lister is required")
	case d.Signal == nil:
		return nil, fmt.Errorf("appfreeze: a signaler is required")
	case d.Store == nil:
		return nil, fmt.Errorf("appfreeze: a store for the frozen set is required")
	case d.Log == nil:
		return nil, fmt.Errorf("appfreeze: a logger is required")
	}
	return &Freezer{d: d}, nil
}

// load reads the kept record into the mirror once. A read that fails is
// recorded at ERROR and answered false: what is frozen is then unknown, and
// nothing is signalled until a later read succeeds.
func (f *Freezer) load(trigger string) bool {
	if f.loaded {
		return true
	}
	r, _, err := f.d.Store.Load()
	if err != nil {
		f.d.Log.Error(opFreeze, "the frozen set could not be read; no application is frozen or thawed until it can be",
			dlog.Context{"trigger": trigger, "cause": err.Error()})
		return false
	}
	f.held, f.loaded = r, true
	return true
}

// Frozen answers the applications held frozen now, in the configured order.
func (f *Freezer) Frozen() []Frozen {
	var out []Frozen
	for _, app := range f.held.Apps {
		if len(app.PIDs) > 0 {
			out = append(out, Frozen{Name: app.Name, Processes: len(app.PIDs)})
		}
	}
	return out
}

// Freeze freezes every configured application's processes, and answers what
// is frozen. A KEPT RECORD IS ADOPTED, never added to: its processes were
// stopped by an earlier freeze (this daemon's, or one before a restart or a
// handover), so freezing again would freeze twice. trigger names what asked,
// for the record.
//
// THE RECORD IS WRITTEN BEFORE THE FIRST SIGSTOP: a set that cannot be kept
// freezes nothing. Once the stops are sent the record is narrowed to the
// processes actually stopped; a narrowing that cannot be kept leaves the
// wider record, which names only processes a thaw may signal harmlessly.
func (f *Freezer) Freeze(ctx context.Context, trigger string) []Frozen {
	if !f.load(trigger) {
		return nil
	}
	if !f.held.empty() {
		f.d.Log.Info(opFreeze, "a kept frozen set was adopted; nothing was frozen again", dlog.Context{
			"trigger": trigger, "frozen": describe(f.held)})
		return f.Frozen()
	}
	if len(f.d.Apps) == 0 {
		return nil
	}
	table, err := f.d.Processes(ctx)
	if err != nil {
		f.d.Log.Error(opFreeze, "the process table could not be read; no application was frozen",
			dlog.Context{"trigger": trigger, "cause": err.Error()})
		return nil
	}
	want, notRunning, refused := f.plan(table)
	for _, r := range refused {
		f.d.Log.Error(opFreeze, "a configured application has a process agent-repl never freezes (the editor, a terminal, agent-repl or an agent session's), so none of it was frozen",
			dlog.Context{"trigger": trigger, "app": r.app, "protected_pids": r.pids})
	}
	if want.empty() {
		f.d.Log.Info(opFreeze, "no configured application was running; nothing was frozen",
			dlog.Context{"trigger": trigger, "not_running": notRunning})
		return nil
	}
	if err := f.d.Store.Save(want); err != nil {
		f.d.Log.Error(opFreeze, "the frozen set could not be kept, so nothing was frozen",
			dlog.Context{"trigger": trigger, "would_freeze": describe(want), "cause": err.Error()})
		return nil
	}
	f.held = want
	stopped := Record{}
	var failed []dlog.Context
	for _, app := range want.Apps {
		kept := RecordedApp{Name: app.Name}
		for _, pid := range app.PIDs {
			if err := f.d.Signal(pid, syscall.SIGSTOP); err != nil {
				if !gone(err) {
					failed = append(failed, dlog.Context{"pid": pid, "app": app.Name, "errno": errnoOf(err)})
					f.d.Log.Error(opFreeze, "a process could not be frozen", dlog.Context{
						"trigger": trigger, "pid": pid, "app": app.Name, "errno": errnoOf(err), "cause": err.Error()})
				}
				continue
			}
			kept.PIDs = append(kept.PIDs, pid)
		}
		if len(kept.PIDs) > 0 {
			stopped.Apps = append(stopped.Apps, kept)
			if f.d.Telemetry != nil {
				f.d.Telemetry.CountAppFrozen(app.Name)
			}
		}
	}
	narrow := f.d.Store.Save
	if stopped.empty() {
		narrow = func(Record) error { return f.d.Store.Clear() }
	}
	if err := narrow(stopped); err != nil {
		f.d.Log.Error(opFreeze, "the frozen set could not be narrowed to the processes stopped; the wider set stands",
			dlog.Context{"trigger": trigger, "cause": err.Error()})
	} else {
		f.held = stopped
	}
	f.d.Log.Info(opFreeze, "the configured applications were frozen for the closed lid", dlog.Context{
		"trigger": trigger, "frozen": describe(stopped), "not_running": notRunning, "failed": len(failed)})
	return f.Frozen()
}

// Thaw thaws every process the kept record names, then clears it. THE RECORD
// IS CLEARED ONLY AFTER THE LAST SIGCONT, so an exit between leaves it naming
// every process still to thaw. A process already gone is not a failure. A
// record that cannot be cleared is recorded at ERROR; the processes are
// thawed regardless, so the mirror is cleared and a later thaw of the stale
// record signals only running processes. trigger names what asked.
func (f *Freezer) Thaw(ctx context.Context, trigger string) {
	if !f.load(trigger) || f.held.empty() {
		return
	}
	thawed, goneCount, failed := thaw(f.held, f.d.Signal, f.d.Log, trigger)
	for _, app := range f.held.Apps {
		if len(app.PIDs) > 0 && f.d.Telemetry != nil {
			f.d.Telemetry.CountAppThawed(app.Name)
		}
	}
	was := describe(f.held)
	f.held = Record{}
	if err := f.d.Store.Clear(); err != nil {
		f.d.Log.Error(opFreeze, "the applications were thawed, but the frozen set could not be cleared", dlog.Context{
			"trigger": trigger, "thawed": was, "cause": err.Error()})
		return
	}
	f.d.Log.Info(opFreeze, "the frozen applications were thawed", dlog.Context{
		"trigger": trigger, "thawed": was, "signalled": thawed, "gone": goneCount, "failed": failed})
}

// thaw sends SIGCONT to every process r names, recording each failure at
// ERROR, and answers how many were signalled, gone and failed. Freezer.Thaw
// and the thaw guard share it.
func thaw(r Record, signal Signaler, log dlog.Logger, trigger string) (signalled, goneCount, failed int) {
	for _, app := range r.Apps {
		for _, pid := range app.PIDs {
			err := signal(pid, syscall.SIGCONT)
			switch {
			case err == nil:
				signalled++
			case gone(err):
				goneCount++
			default:
				failed++
				log.Error(opFreeze, "a frozen process could not be thawed", dlog.Context{
					"trigger": trigger, "pid": pid, "app": app.Name, "errno": errnoOf(err), "cause": err.Error()})
			}
		}
	}
	return signalled, goneCount, failed
}

// refusal is a configured application refused for a protected process.
type refusal struct {
	app  string
	pids []int
}

// plan answers, from the process table, each configured application's
// processes to freeze, the applications not running, and those refused
// because a protected process belongs to them.
func (f *Freezer) plan(table []Process) (Record, []string, []refusal) {
	protected := f.d.Protection.protected(table)
	var want Record
	var notRunning []string
	var refused []refusal
	for _, app := range f.d.Apps {
		var pids, guarded []int
		for _, proc := range table {
			if !bundleOf(proc.Path, app) {
				continue
			}
			if protected[proc.PID] {
				guarded = append(guarded, proc.PID)
				continue
			}
			pids = append(pids, proc.PID)
		}
		switch {
		case len(guarded) > 0:
			refused = append(refused, refusal{app: app, pids: guarded})
		case len(pids) == 0:
			notRunning = append(notRunning, app)
		default:
			want.Apps = append(want.Apps, RecordedApp{Name: app, PIDs: pids})
		}
	}
	return want, notRunning, refused
}

// describe states a record for the log: each application with its pids.
func describe(r Record) map[string][]int {
	out := map[string][]int{}
	for _, app := range r.Apps {
		if len(app.PIDs) > 0 {
			out[app.Name] = app.PIDs
		}
	}
	return out
}
