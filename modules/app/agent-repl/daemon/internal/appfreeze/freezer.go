package appfreeze

import (
	"context"
	"fmt"
	"sync"
	"syscall"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/kernelevent"
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
	// Spawns opens the follower of process starts. REQUIRED.
	Spawns SpawnSource
	// Protection names the processes never frozen.
	Protection Protection
	// Log is the package's canonical logger. REQUIRED.
	Log dlog.Logger
	// Telemetry counts freezes and thaws; nil counts nothing.
	Telemetry Telemetry
}

// SpawnFollower is the kernel's report of processes starting
// (kernelevent.Spawns); a test hands a scripted one.
type SpawnFollower interface {
	FollowForks(pid int) (bool, error)
	FollowExec(pid int) (bool, error)
	Wait(ctx context.Context) error
	Close()
}

// SpawnSource opens a SpawnFollower. KernelSpawns is the production one.
type SpawnSource func() (SpawnFollower, error)

// KernelSpawns opens the kernel's follower (kernelevent.NewSpawns).
func KernelSpawns() (SpawnFollower, error) {
	s, err := kernelevent.NewSpawns()
	if err != nil {
		return nil, err
	}
	return s, nil
}

// Frozen is one application the freezer holds frozen, for the standing.
type Frozen struct {
	Name      string
	Processes int
}

// Freezer freezes and thaws the configured applications. IT IS THE ONE
// OWNER OF THE FROZEN SET: the quiet state's Freeze and Thaw and its own
// follower of process starts change the set only under mu.
type Freezer struct {
	d  Deps
	mu sync.Mutex
	// held mirrors the kept record once loaded; loaded says it was read.
	held   Record
	loaded bool
	// follow is the follower of process starts while the quiet state
	// stands, nil otherwise.
	follow *following
	// onChange is told, outside mu, of each late freeze (OnChange).
	onChange func()
}

// following follows process starts for one quiet state.
type following struct {
	// spawns is the kernel's report; nil when it could not be opened.
	spawns SpawnFollower
	cancel context.CancelFunc
	// seen is every launchd child already followed for its exec.
	seen map[int]bool
	// skip is every pid not to stop again: its stop failed, or it was
	// gone, or it was refused as protected (logged once).
	skip map[int]bool
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
	case d.Spawns == nil:
		return nil, fmt.Errorf("appfreeze: a source of process starts is required")
	case d.Log == nil:
		return nil, fmt.Errorf("appfreeze: a logger is required")
	}
	return &Freezer{d: d}, nil
}

// OnChange names the function told of each late freeze, so the quiet
// state's standing shows what is frozen now. It is called outside the
// freezer's lock. Set it before the first Freeze.
func (f *Freezer) OnChange(fn func()) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.onChange = fn
}

// load reads the kept record into the mirror once. A read that fails is
// recorded at ERROR and answered false: what is frozen is then unknown, and
// nothing is signalled until a later read succeeds. Caller holds mu.
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
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.frozenLocked()
}

func (f *Freezer) frozenLocked() []Frozen {
	var out []Frozen
	for _, app := range f.held.Apps {
		if len(app.PIDs) > 0 {
			out = append(out, Frozen{Name: app.Name, Processes: len(app.PIDs)})
		}
	}
	return out
}

// Freeze freezes every configured application's processes for the quiet
// state, and FOLLOWS PROCESS STARTS UNTIL THAW, so every process of a
// configured application is frozen while the state stands: one running now,
// one a kept record already names (adopted, never stopped twice), one
// running but missing from a kept record, and one started later (a launchd
// relaunch, a new helper). trigger names what asked, for the record.
//
// THE FOLLOWER IS REGISTERED BEFORE THE FIRST READ of the process table, so
// a process that starts between the read and the registration is still
// reported. Each catch-up keeps the widened record BEFORE ITS FIRST SIGSTOP
// (stopAndKeep), so a thaw covers every process stopped.
func (f *Freezer) Freeze(ctx context.Context, trigger string) []Frozen {
	f.mu.Lock()
	defer f.mu.Unlock()
	if !f.load(trigger) {
		return nil
	}
	adopted := !f.held.empty()
	if adopted {
		f.d.Log.Info(opFreeze, "a kept frozen set was adopted; its processes are not stopped again", dlog.Context{
			"trigger": trigger, "frozen": describe(f.held)})
	}
	if len(f.d.Apps) == 0 {
		return f.frozenLocked()
	}
	if f.follow == nil {
		f.startFollowingLocked(trigger)
	}
	f.catchUpLocked(ctx, trigger, !adopted)
	return f.frozenLocked()
}

// launchdPID is launchd's: the parent of every process a relaunch, an
// application launch or an XPC service starts.
const launchdPID = 1

// startFollowingLocked opens the follower of process starts and runs its
// loop. A follower that cannot be opened is recorded at ERROR: the quiet
// state then freezes only what each Freeze finds. Caller holds mu.
func (f *Freezer) startFollowingLocked(trigger string) {
	ctx, cancel := context.WithCancel(context.Background())
	fl := &following{cancel: cancel, seen: map[int]bool{}, skip: map[int]bool{}}
	f.follow = fl
	spawns, err := f.d.Spawns()
	if err == nil {
		if _, err = spawns.FollowForks(launchdPID); err != nil {
			spawns.Close()
		}
	}
	if err != nil {
		f.d.Log.Error(opFreeze, "process starts cannot be followed, so a process an application starts while the lid is closed is not frozen",
			dlog.Context{"trigger": trigger, "cause": err.Error()})
		return
	}
	fl.spawns = spawns
	inflight.Standing(func() { f.followLoop(ctx, fl) })
}

// followLoop catches up on each reported process start until the quiet
// state ends (Thaw cancels ctx). It owns fl.spawns and closes it on return.
// A follower that fails while the state stands is recorded at ERROR.
func (f *Freezer) followLoop(ctx context.Context, fl *following) {
	defer fl.spawns.Close()
	for {
		err := fl.spawns.Wait(ctx)
		f.mu.Lock()
		if f.follow != fl {
			f.mu.Unlock()
			return
		}
		if err != nil {
			f.d.Log.Error(opFreeze, "following process starts failed, so a process an application starts from now on while the lid is closed is not frozen",
				dlog.Context{"trigger": triggerStarted, "cause": err.Error()})
			f.mu.Unlock()
			return
		}
		changed := f.catchUpLocked(ctx, triggerStarted, false)
		onChange := f.onChange
		f.mu.Unlock()
		if changed && onChange != nil {
			onChange()
		}
	}
}

// triggerStarted is the trigger of a catch-up a process start asked for.
const triggerStarted = "process_started"

// catchUpLocked freezes every configured application's process not yet
// held, reading the table again until a read finds nothing new: a process
// forked before its parent's stop, or a launchd child that execed after its
// exec was followed, is found by the next read. first says the set was
// empty when the quiet state asked (the freeze of the closing lid, logged
// as such). It answers whether anything was frozen. Caller holds mu.
func (f *Freezer) catchUpLocked(ctx context.Context, trigger string, first bool) bool {
	changed := false
	for round := 0; ; round++ {
		table, err := f.d.Processes(ctx)
		if err != nil {
			f.d.Log.Error(opFreeze, "the process table could not be read; no new process was frozen",
				dlog.Context{"trigger": trigger, "cause": err.Error()})
			return changed
		}
		followed := f.followLaunchdChildrenLocked(table, trigger)
		want, notRunning, refused := f.plan(table)
		for _, r := range refused {
			f.d.Log.Error(opFreeze, "a configured application has a process agent-repl never freezes (the editor, a terminal, agent-repl or an agent session's), so none of it was frozen",
				dlog.Context{"trigger": trigger, "app": r.app, "protected_pids": r.pids})
		}
		if want.empty() {
			if first && round == 0 {
				f.d.Log.Info(opFreeze, "no configured application was running; nothing was frozen, and one started while the lid is closed is",
					dlog.Context{"trigger": trigger, "not_running": notRunning})
			}
			if followed {
				continue
			}
			return changed
		}
		lateFreeze := !(first && !changed)
		stopped, ok := f.stopAndKeepLocked(want, trigger)
		if !ok {
			return changed
		}
		changed = changed || !stopped.empty()
		if lateFreeze {
			f.d.Log.Info(opFreeze, "a configured application's process started while the lid was closed, and was frozen", dlog.Context{
				"trigger": trigger, "frozen": describe(stopped), "held": describe(f.held)})
		} else {
			f.d.Log.Info(opFreeze, "the configured applications were frozen for the closed lid", dlog.Context{
				"trigger": trigger, "frozen": describe(stopped), "not_running": notRunning})
		}
	}
}

// followLaunchdChildrenLocked follows the exec of every launchd child not
// yet followed, so a service still running as xpcproxy is read again once
// it is its own binary; it answers whether any follow was newly
// registered, which asks the caller to read the table again (the exec may
// have landed before the registration). seen is narrowed to the table, so a
// reused pid is followed afresh. Caller holds mu.
func (f *Freezer) followLaunchdChildrenLocked(table []Process, trigger string) bool {
	fl := f.follow
	if fl == nil || fl.spawns == nil {
		return false
	}
	live := map[int]bool{}
	registered := false
	for _, proc := range table {
		live[proc.PID] = true
		if proc.PPID != launchdPID || fl.seen[proc.PID] {
			continue
		}
		fl.seen[proc.PID] = true
		ok, err := fl.spawns.FollowExec(proc.PID)
		if err != nil {
			f.d.Log.Error(opFreeze, "a launchd child's exec could not be followed; if it becomes a configured application's process it is frozen only by a later read",
				dlog.Context{"trigger": trigger, "pid": proc.PID, "path": proc.Path, "cause": err.Error()})
			continue
		}
		registered = registered || ok
	}
	for pid := range fl.seen {
		if !live[pid] {
			delete(fl.seen, pid)
		}
	}
	for pid := range fl.skip {
		if !live[pid] {
			delete(fl.skip, pid)
		}
	}
	return registered
}

// stopAndKeepLocked freezes want's processes on top of what is held. THE
// WIDENED RECORD IS KEPT BEFORE THE FIRST SIGSTOP: a set that cannot be
// kept freezes nothing (ok false). Once the stops are sent the record is
// narrowed to what is held plus what was stopped; a narrowing that cannot be
// kept leaves the wider record, which names only processes a thaw may signal
// harmlessly. A process gone before its stop is left out quietly, and one
// whose stop is refused is recorded at ERROR; neither is tried again this
// quiet state. Caller holds mu.
func (f *Freezer) stopAndKeepLocked(want Record, trigger string) (stopped Record, ok bool) {
	before := f.held
	wider := merge(before, want)
	if err := f.d.Store.Save(wider); err != nil {
		f.d.Log.Error(opFreeze, "the frozen set could not be kept, so nothing more was frozen",
			dlog.Context{"trigger": trigger, "would_freeze": describe(want), "cause": err.Error()})
		return Record{}, false
	}
	f.held = wider
	for _, app := range want.Apps {
		kept := RecordedApp{Name: app.Name}
		for _, pid := range app.PIDs {
			if err := f.d.Signal(pid, syscall.SIGSTOP); err != nil {
				f.skipLocked(pid)
				if !gone(err) {
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
	narrowed := merge(before, stopped)
	narrow := f.d.Store.Save
	if narrowed.empty() {
		narrow = func(Record) error { return f.d.Store.Clear() }
	}
	if err := narrow(narrowed); err != nil {
		f.d.Log.Error(opFreeze, "the frozen set could not be narrowed to the processes stopped; the wider set stands",
			dlog.Context{"trigger": trigger, "cause": err.Error()})
		for _, app := range want.Apps {
			for _, pid := range app.PIDs {
				f.skipLocked(pid)
			}
		}
	} else {
		f.held = narrowed
	}
	return stopped, true
}

// skipLocked marks pid not to be stopped again this quiet state. Caller
// holds mu.
func (f *Freezer) skipLocked(pid int) {
	if f.follow != nil {
		f.follow.skip[pid] = true
	}
}

// Thaw ends the follower of process starts, then thaws every process the
// kept record names and clears it. THE FOLLOWER ENDS FIRST, under the same
// lock, so no process is frozen after the thaw. THE RECORD IS CLEARED ONLY
// AFTER THE LAST SIGCONT, so an exit between leaves it naming every process
// still to thaw. A process already gone is not a failure. A record that
// cannot be cleared is recorded at ERROR; the processes are thawed
// regardless, so the mirror is cleared and a later thaw of the stale record
// signals only running processes. trigger names what asked.
func (f *Freezer) Thaw(ctx context.Context, trigger string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.follow != nil {
		f.follow.cancel()
		f.follow = nil
	}
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
// processes to freeze that are not held yet, the applications not running,
// and those refused because a protected process belongs to them. A refused
// or skipped pid is answered once per quiet state. Caller holds mu.
func (f *Freezer) plan(table []Process) (Record, []string, []refusal) {
	protected := f.d.Protection.protected(table)
	held := map[int]bool{}
	for _, app := range f.held.Apps {
		for _, pid := range app.PIDs {
			held[pid] = true
		}
	}
	skip := map[int]bool{}
	if f.follow != nil {
		skip = f.follow.skip
	}
	var want Record
	var notRunning []string
	var refused []refusal
	for _, app := range f.d.Apps {
		var pids, guarded []int
		running := false
		for _, proc := range table {
			if !bundleOf(proc.Path, app) {
				continue
			}
			running = true
			if held[proc.PID] || skip[proc.PID] {
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
			for _, pid := range append(guarded, pids...) {
				if f.follow != nil {
					f.follow.skip[pid] = true
				}
			}
		case !running:
			notRunning = append(notRunning, app)
		case len(pids) == 0:
		default:
			want.Apps = append(want.Apps, RecordedApp{Name: app, PIDs: pids})
		}
	}
	return want, notRunning, refused
}

// merge answers a with b's processes added, in a's order of applications
// and then b's.
func merge(a, b Record) Record {
	var out Record
	index := map[string]int{}
	for _, rec := range []Record{a, b} {
		for _, app := range rec.Apps {
			if len(app.PIDs) == 0 {
				continue
			}
			i, ok := index[app.Name]
			if !ok {
				i = len(out.Apps)
				index[app.Name] = i
				out.Apps = append(out.Apps, RecordedApp{Name: app.Name})
			}
			out.Apps[i].PIDs = append(out.Apps[i].PIDs, app.PIDs...)
		}
	}
	return out
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
