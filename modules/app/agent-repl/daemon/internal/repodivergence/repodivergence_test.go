package repodivergence

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// GIT IS NEVER CALLED: every git fact is a scripted answer of fakeGit.

var errScripted = errors.New("scripted failure")

// fakeGit answers the measurer's four calls from scripted fields, recording
// every call in order. A call whose context has already ended answers the
// context's error, as the real leaf's Cancelled unwraps to.
type fakeGit struct {
	mu    sync.Mutex
	calls []string

	branch      string
	branchErr   error
	upstream    gitclient.Upstream
	tracked     bool
	upstreamErr error
	fetchErr    error
	// fetchErrIn fails one directory's fetch only.
	fetchErrIn map[string]error
	ahead      int
	behind     int
	countErr   error
}

func (g *fakeGit) record(call string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, call)
}

func (g *fakeGit) DefaultBranch(ctx context.Context, dir string) (string, error) {
	g.record("default_branch " + dir)
	if ctx.Err() != nil {
		return "", ctx.Err()
	}
	return g.branch, g.branchErr
}

func (g *fakeGit) Upstream(ctx context.Context, dir, branch string) (gitclient.Upstream, bool, error) {
	g.record("upstream " + dir + " " + branch)
	if ctx.Err() != nil {
		return gitclient.Upstream{}, false, ctx.Err()
	}
	return g.upstream, g.tracked, g.upstreamErr
}

func (g *fakeGit) Fetch(ctx context.Context, dir, remote string) error {
	g.record("fetch " + dir + " " + remote)
	if ctx.Err() != nil {
		return ctx.Err()
	}
	if err := g.fetchErrIn[dir]; err != nil {
		return err
	}
	return g.fetchErr
}

func (g *fakeGit) AheadBehind(ctx context.Context, dir, local, upstream string) (int, int, error) {
	g.record("ahead_behind " + dir + " " + local + " " + upstream)
	if ctx.Err() != nil {
		return 0, 0, ctx.Err()
	}
	return g.ahead, g.behind, g.countErr
}

// tracking is a fakeGit whose master tracks origin/master.
func tracking() *fakeGit {
	return &fakeGit{
		branch:   "master",
		upstream: gitclient.Upstream{Remote: "origin", Ref: "origin/master"},
		tracked:  true,
		ahead:    2,
		behind:   7,
	}
}

type fakeRegistry struct {
	repos []wsm.Repository
	err   error
}

func (r *fakeRegistry) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return r.repos, r.err
}

// told is one sink call.
type told struct {
	repo    ids.RepoID
	measure Measure
}

type world struct {
	t        *testing.T
	git      *fakeGit
	registry *fakeRegistry
	clock    *clocktest.Scripted
	log      *dlog.TestLogger
	mu       sync.Mutex
	sunk     []told
}

func newWorld(t *testing.T, git *fakeGit, repos ...wsm.Repository) *world {
	return &world{t: t, git: git, registry: &fakeRegistry{repos: repos}, clock: clocktest.New(time.Time{}), log: dlog.NewTestLogger()}
}

func repo(id string) wsm.Repository {
	return wsm.Repository{ID: ids.RepoID(id), Dir: "/src/" + id, Name: id}
}

func (w *world) deps() Deps {
	return Deps{
		Git:      w.git,
		Registry: w.registry,
		Sink: func(repo ids.RepoID, measure Measure) {
			w.mu.Lock()
			defer w.mu.Unlock()
			w.sunk = append(w.sunk, told{repo: repo, measure: measure})
		},
		Clock:   w.clock,
		Every:   DefaultEvery,
		Timeout: DefaultTimeout,
		Log:     w.log,
	}
}

func (w *world) measurer() *Measurer {
	w.t.Helper()
	m, err := New(w.deps())
	if err != nil {
		w.t.Fatalf("New: %v", err)
	}
	return m
}

func (w *world) told() []told {
	w.mu.Lock()
	defer w.mu.Unlock()
	return append([]told(nil), w.sunk...)
}

func (w *world) records(level, operation string) []dlog.Record {
	return dlog.FilterRecords(w.log.Records(), dlog.RecordQuery{Level: level, Operation: operation})
}

func (w *world) assertNoWarnings() {
	w.t.Helper()
	for _, r := range w.log.Records() {
		if r.Level == "error" || r.Level == "warn" {
			w.t.Fatalf("unexpected %s record %s: %s %v", r.Level, r.Operation, r.Message, r.Context)
		}
	}
}

func TestNewRefusesAMissingCollaboratorOrWindow(t *testing.T) {
	tests := []struct {
		name      string
		breakDeps func(*Deps)
	}{
		{name: "git", breakDeps: func(d *Deps) { d.Git = nil }},
		{name: "registry", breakDeps: func(d *Deps) { d.Registry = nil }},
		{name: "sink", breakDeps: func(d *Deps) { d.Sink = nil }},
		{name: "clock", breakDeps: func(d *Deps) { d.Clock = nil }},
		{name: "log", breakDeps: func(d *Deps) { d.Log = nil }},
		{name: "every", breakDeps: func(d *Deps) { d.Every = 0 }},
		{name: "timeout", breakDeps: func(d *Deps) { d.Timeout = -time.Second }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			deps := newWorld(t, tracking()).deps()
			tc.breakDeps(&deps)

			// Act.
			m, err := New(deps)

			// Assert.
			if err == nil || m != nil {
				t.Fatalf("New = %v, %v; want a refusal", m, err)
			}
		})
	}
}

func TestATrackedBranchIsFetchedThenCounted(t *testing.T) {
	// Arrange.
	w := newWorld(t, tracking(), repo("a"))

	// Act.
	w.measurer().Sweep(context.Background())

	// Assert.
	want := []string{
		"default_branch /src/a",
		"upstream /src/a master",
		"fetch /src/a origin",
		"ahead_behind /src/a master origin/master",
	}
	if !reflect.DeepEqual(w.git.calls, want) {
		t.Fatalf("git calls = %v, want %v", w.git.calls, want)
	}
	wantTold := []told{{repo: "a", measure: Counted{Branch: "master", Upstream: "origin/master", Ahead: 2, Behind: 7}}}
	if got := w.told(); !reflect.DeepEqual(got, wantTold) {
		t.Fatalf("sink told %+v, want %+v", got, wantTold)
	}
	w.assertNoWarnings()
}

func TestAnUntrackedBranchIsNoUpstreamAndNothingIsFetched(t *testing.T) {
	// Arrange.
	git := tracking()
	git.tracked = false
	git.upstream = gitclient.Upstream{}
	w := newWorld(t, git, repo("a"))

	// Act.
	w.measurer().Sweep(context.Background())

	// Assert.
	wantTold := []told{{repo: "a", measure: NoUpstream{Branch: "master"}}}
	if got := w.told(); !reflect.DeepEqual(got, wantTold) {
		t.Fatalf("sink told %+v, want %+v", got, wantTold)
	}
	for _, call := range git.calls {
		if strings.HasPrefix(call, "fetch") || strings.HasPrefix(call, "ahead_behind") {
			t.Fatalf("git calls = %v, want no fetch and no count", git.calls)
		}
	}
	w.assertNoWarnings()
}

func TestAFailedStepIsAFailedMeasureAndStopsTheRepository(t *testing.T) {
	tests := []struct {
		name       string
		breakGit   func(*fakeGit)
		wantReason string
		wantCalls  int
		wantStep   string
	}{
		{name: "primary branch", breakGit: func(g *fakeGit) { g.branchErr = errScripted },
			wantReason: "resolving the primary branch failed: scripted failure", wantCalls: 1, wantStep: "resolving the primary branch"},
		{name: "upstream", breakGit: func(g *fakeGit) { g.upstreamErr = errScripted },
			wantReason: "reading master's upstream failed: scripted failure", wantCalls: 2, wantStep: "reading master's upstream"},
		{name: "fetch", breakGit: func(g *fakeGit) { g.fetchErr = errScripted },
			wantReason: "fetching origin failed: scripted failure", wantCalls: 3, wantStep: "fetching origin"},
		{name: "count", breakGit: func(g *fakeGit) { g.countErr = errScripted },
			wantReason: "counting master against origin/master failed: scripted failure", wantCalls: 4, wantStep: "counting master against origin/master"},
		{name: "a step that runs out of time", breakGit: func(g *fakeGit) { g.fetchErr = context.DeadlineExceeded },
			wantReason: "fetching origin did not finish within 2m0s", wantCalls: 3, wantStep: "fetching origin"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			git := tracking()
			tc.breakGit(git)
			w := newWorld(t, git, repo("a"))

			// Act.
			w.measurer().Sweep(context.Background())

			// Assert.
			wantTold := []told{{repo: "a", measure: Failed{Reason: tc.wantReason}}}
			if got := w.told(); !reflect.DeepEqual(got, wantTold) {
				t.Fatalf("sink told %+v, want %+v", got, wantTold)
			}
			if len(git.calls) != tc.wantCalls {
				t.Fatalf("git calls = %v, want the measurement to stop after %d", git.calls, tc.wantCalls)
			}
			records := w.records("info", opMeasure)
			if len(records) != 1 || records[0].Context["step"] != tc.wantStep || records[0].Context["repo_id"] != "a" ||
				records[0].Context["cause"] == nil {
				t.Fatalf("measure records = %+v, want one INFO naming the step, repository and cause", records)
			}
		})
	}
}

func TestAFailedRepositoryDoesNotStopTheSweep(t *testing.T) {
	// Arrange: the first repository's fetch fails, the second's succeeds.
	git := tracking()
	git.fetchErrIn = map[string]error{"/src/a": errScripted}
	w := newWorld(t, git, repo("a"), repo("b"))

	// Act.
	w.measurer().Sweep(context.Background())

	// Assert.
	got := w.told()
	if len(got) != 2 || got[0].repo != "a" || got[1].repo != "b" {
		t.Fatalf("sink told %+v, want both repositories in registry order", got)
	}
	if _, failed := got[0].measure.(Failed); !failed {
		t.Fatalf("a's measure = %+v, want Failed", got[0].measure)
	}
	if _, counted := got[1].measure.(Counted); !counted {
		t.Fatalf("b's measure = %+v, want Counted", got[1].measure)
	}
}

func TestAStoppedContextTellsTheSinkNothing(t *testing.T) {
	// Arrange.
	w := newWorld(t, tracking(), repo("a"), repo("b"))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	w.measurer().Sweep(ctx)

	// Assert.
	if got := w.told(); len(got) != 0 {
		t.Fatalf("sink told %+v, want nothing for a stopped daemon", got)
	}
	if len(w.git.calls) != 1 {
		t.Fatalf("git calls = %v, want the sweep to stop at the first repository", w.git.calls)
	}
	if len(w.records("info", opMeasure)) != 0 {
		t.Fatal("a stopped measurement was recorded as a failed one")
	}
}

func TestAnUnreadableRegistryMeasuresNothing(t *testing.T) {
	// Arrange.
	w := newWorld(t, tracking(), repo("a"))
	w.registry.err = errScripted

	// Act.
	w.measurer().Sweep(context.Background())

	// Assert.
	if got := w.told(); len(got) != 0 || len(w.git.calls) != 0 {
		t.Fatalf("sink told %+v, git %v; want nothing", got, w.git.calls)
	}
	records := w.records("info", opSweep)
	if len(records) != 1 || records[0].Context["cause"] != errScripted.Error() {
		t.Fatalf("sweep records = %+v, want one INFO carrying the cause", records)
	}
}

// running is one schedule run on its own goroutine.
type running struct {
	cancel   context.CancelFunc
	finished chan struct{}
	err      error
}

func startRun(t *testing.T, m *Measurer) *running {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	run := &running{cancel: cancel, finished: make(chan struct{})}
	go func() {
		run.err = m.Run(ctx)
		close(run.finished)
	}()
	t.Cleanup(func() {
		cancel()
		<-run.finished
	})
	return run
}

func TestTheFirstSweepRunsAtOnce(t *testing.T) {
	// Arrange.
	w := newWorld(t, tracking(), repo("a"))

	// Act.
	startRun(t, w.measurer())
	first := <-w.clock.Asked // asked only once the first sweep is over

	// Assert.
	if got := w.told(); len(got) != 1 {
		t.Fatalf("sink told %+v before the first wait, want the startup sweep", got)
	}
	if first != DefaultEvery {
		t.Fatalf("the first wait = %v, want the cadence %v", first, DefaultEvery)
	}
}

func TestEverySweepAfterTheFirstWaitsTheCadence(t *testing.T) {
	// Arrange.
	w := newWorld(t, tracking(), repo("a"))
	startRun(t, w.measurer())
	<-w.clock.Asked

	// Act.
	w.clock.Fire <- time.Time{}
	second := <-w.clock.Asked

	// Assert.
	if second != DefaultEvery {
		t.Fatalf("the second wait = %v, want the cadence", second)
	}
	if got := w.told(); len(got) != 2 {
		t.Fatalf("sink told %d times, want once per sweep", len(got))
	}
}

func TestTheScheduleEndsWithItsContext(t *testing.T) {
	// Arrange.
	w := newWorld(t, tracking(), repo("a"))
	run := startRun(t, w.measurer())
	<-w.clock.Asked

	// Act.
	run.cancel()
	<-run.finished

	// Assert.
	if !errors.Is(run.err, context.Canceled) {
		t.Fatalf("Run = %v, want context.Canceled", run.err)
	}
}
