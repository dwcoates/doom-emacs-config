package hotreload

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// start is the instant every test's deploy begins at.
var start = time.UnixMilli(1_000_000)

// fakeFeed records every row the tracker drew.
type fakeFeed struct {
	mu   sync.Mutex
	ws   []ids.WorkspaceID
	rows []*frontendv1.FeedRow
}

func (f *fakeFeed) UpsertDurable(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow) {
	if !feed.Root {
		panic("the card was drawn off the root feed")
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ws = append(f.ws, ws)
	f.rows = append(f.rows, row)
}

// drawn counts the rows drawn.
func (f *fakeFeed) drawn() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.rows)
}

// last answers the last card drawn.
func (f *fakeFeed) last(t *testing.T) *frontendv1.FeedHotReload {
	t.Helper()
	f.mu.Lock()
	defer f.mu.Unlock()
	if len(f.rows) == 0 {
		t.Fatal("no card was drawn")
	}
	return f.rows[len(f.rows)-1].GetActivity().GetHotReload()
}

// clock is a settable clock.
type clock struct{ now time.Time }

func (c *clock) Now() time.Time          { return c.now }
func (c *clock) advance(d time.Duration) { c.now = c.now.Add(d) }

// fixture is a tracker over a fake feed, a settable clock and a test logger.
type fixture struct {
	t     *Tracker
	feed  *fakeFeed
	clock *clock
	log   *dlog.TestLogger
}

// newFixture builds a tracker whose deploy mediator is "mediator" and whose
// workspaces are named "name-<id>".
func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{feed: &fakeFeed{}, clock: &clock{now: start}, log: dlog.NewTestLogger()}
	tracker, err := New(Deps{
		Feed:     f.feed,
		Mediator: func(context.Context) (ids.WorkspaceID, bool, error) { return "mediator", true, nil },
		WorkspaceName: func(_ context.Context, ws ids.WorkspaceID) (string, error) {
			return "name-" + string(ws), nil
		},
		Now: f.clock.Now,
		Log: f.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.t = tracker
	return f
}

// begun is a fixture whose deploy began.
func begun(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.t.Begin(context.Background(), start)
	return f
}

// service answers the drawn card's service NAME.
func service(t *testing.T, card *frontendv1.FeedHotReload, name string) *frontendv1.FeedHotReloadService {
	t.Helper()
	for _, s := range card.GetServices() {
		if s.GetName().GetText() == name {
			return s
		}
	}
	t.Fatalf("the card draws no service %q", name)
	return nil
}

// stepOf answers the drawn service's step NAME.
func stepOf(t *testing.T, svc *frontendv1.FeedHotReloadService, name string) *frontendv1.FeedHotReloadStep {
	t.Helper()
	for _, st := range svc.GetSteps() {
		if st.GetName().GetText() == name {
			return st
		}
	}
	t.Fatalf("service %q draws no step %q", svc.GetName().GetText(), name)
	return nil
}

// arm names a drawn state's arm and words, "arm:words".
func arm(st *frontendv1.FeedHotReloadState) string {
	switch s := st.GetState().(type) {
	case *frontendv1.FeedHotReloadState_Scheduled:
		return "scheduled:" + s.Scheduled.GetWords().GetText()
	case *frontendv1.FeedHotReloadState_Running:
		return "running:" + s.Running.GetWords().GetText()
	case *frontendv1.FeedHotReloadState_Waiting:
		return "waiting:" + s.Waiting.GetWords().GetText()
	case *frontendv1.FeedHotReloadState_Done:
		return "done:" + s.Done.GetWords().GetText()
	case *frontendv1.FeedHotReloadState_Unchanged:
		return "unchanged:" + s.Unchanged.GetWords().GetText()
	case *frontendv1.FeedHotReloadState_Failed:
		return "failed:" + s.Failed.GetWords().GetText()
	case *frontendv1.FeedHotReloadState_Skipped:
		return "skipped:" + s.Skipped.GetWords().GetText()
	default:
		return "unset"
	}
}

// badge names the drawn card's badge: "live", "succeeded" or "failed:<why>".
func badge(card *frontendv1.FeedHotReload) string {
	switch s := card.GetState().(type) {
	case *frontendv1.FeedHotReload_Live:
		return "live"
	case *frontendv1.FeedHotReload_Settled:
		if f := s.Settled.GetFailed(); f != nil {
			return "failed:" + f.GetSummary()
		}
		return "succeeded"
	default:
		return "unset"
	}
}

// finishEverythingButShims takes a begun deploy through its build and
// install, and puts every service but the shim into service.
func finishEverythingButShims(f *fixture) {
	f.t.BuildStepBegan("build")
	f.t.BuildStepEnded("build", nil)
	f.t.Built(map[string]string{ServiceWebapp: "web-fresh-build"})
	f.t.InstallBegan()
	f.t.InstallEnded()
	for _, name := range []string{ServiceStore, ServiceSidecar, ServiceZettel, ServiceTests} {
		f.t.ServiceCurrent(name)
	}
	f.t.DaemonCurrent()
	f.t.WebappJudged()
	f.t.ElispCurrent()
	f.t.BuildStepBegan("proto")
	f.t.BuildStepEnded("proto", nil)
}

func TestNewRefusesAMissingDependency(t *testing.T) {
	tests := []struct {
		name  string
		strip func(*Deps)
		want  string
	}{
		{"no feed", func(d *Deps) { d.Feed = nil }, "feed"},
		{"no mediator lookup", func(d *Deps) { d.Mediator = nil }, "mediator"},
		{"no name lookup", func(d *Deps) { d.WorkspaceName = nil }, "name"},
		{"no clock", func(d *Deps) { d.Now = nil }, "clock"},
		{"no logger", func(d *Deps) { d.Log = nil }, "logger"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			deps := Deps{
				Feed:          &fakeFeed{},
				Mediator:      func(context.Context) (ids.WorkspaceID, bool, error) { return "", false, nil },
				WorkspaceName: func(context.Context, ids.WorkspaceID) (string, error) { return "", nil },
				Now:           time.Now,
				Log:           dlog.NewTestLogger(),
			}
			tt.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("New = %v, want a refusal naming %q", err, tt.want)
			}
		})
	}
}

func TestBeginDrawsEveryServiceScheduledInTheMediatorsRootFeed(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.t.Begin(context.Background(), start)

	// Assert
	card := f.feed.last(t)
	var names []string
	for _, s := range card.GetServices() {
		names = append(names, s.GetName().GetText())
		if got := arm(s.GetState()); got != "scheduled:scheduled" {
			t.Fatalf("service %s = %s, want scheduled", s.GetName().GetText(), got)
		}
	}
	want := []string{ServiceStore, ServiceSidecar, ServiceZettel, ServiceTests, ServiceDaemon, ServiceShim, ServiceWebapp, ServiceElisp}
	if strings.Join(names, ",") != strings.Join(want, ",") {
		t.Fatalf("services %v, want %v", names, want)
	}
	if badge(card) != "live" || card.GetHead().GetLabel().GetText() != "hot-reloading" || f.feed.ws[0] != "mediator" {
		t.Fatalf("card badge %s label %q in %s, want a live hot-reloading card in the mediator's feed", badge(card), card.GetHead().GetLabel().GetText(), f.feed.ws[0])
	}
}

func TestTheCardsRowIsKeyedByTheDeploysStart(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.t.Begin(context.Background(), start)

	// Assert
	ref, err := feedid.Decode(f.feed.rows[0].GetId())
	if err != nil || ref.WS != "mediator" || ref.Row.Kind != feedid.KindHotReload || ref.Row.ID != "1000000" {
		t.Fatalf("row id = %+v (%v), want the mediator's hot_reload row keyed 1000000", ref, err)
	}
}

func TestBeginWithoutADrawableMediatorDrawsNoCard(t *testing.T) {
	tests := []struct {
		name      string
		mediator  func(context.Context) (ids.WorkspaceID, bool, error)
		wantLevel string
		wantMsg   string
	}{
		{"a repository with no mediator", func(context.Context) (ids.WorkspaceID, bool, error) { return "", false, nil },
			"info", "the repository has no deploy mediator; this deploy's hot reload draws no card"},
		{"a mediator that cannot be read", func(context.Context) (ids.WorkspaceID, bool, error) { return "", false, errors.New("disk gone") },
			"error", "could not read the deploy mediator to draw the hot reload for; this deploy draws no card"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.t.deps.Mediator = tt.mediator

			// Act
			f.t.Begin(context.Background(), start)
			f.t.BuildStepBegan("shim")

			// Assert
			if f.feed.drawn() != 0 {
				t.Fatalf("drew %d cards, want none", f.feed.drawn())
			}
			if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: opHotReload, Message: tt.wantMsg}); !ok {
				t.Fatalf("records %+v, want %s %q", f.log.Records(), tt.wantLevel, tt.wantMsg)
			}
		})
	}
}

func TestTheMediatorReadFailureRecordsItsCauseAndTheDeploy(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.t.deps.Mediator = func(context.Context) (ids.WorkspaceID, bool, error) { return "", false, errors.New("disk gone") }

	// Act
	f.t.Begin(context.Background(), start)

	// Assert
	r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opHotReload})
	if !ok || r.Context["deploy_started_ms"] != int64(1_000_000) || !strings.Contains(r.Context["cause"].(string), "disk gone") {
		t.Fatalf("record %+v, want the deploy's start and the cause", r)
	}
}

func TestBuildSteps(t *testing.T) {
	tests := []struct {
		name    string
		act     func(f *fixture)
		service string
		step    string
		want    string
	}{
		{"a target building", func(f *fixture) { f.t.BuildStepBegan("shim") }, ServiceShim, "build", "running:building"},
		{"a target built", func(f *fixture) { f.t.BuildStepBegan("webapp"); f.t.BuildStepEnded("webapp", nil) }, ServiceWebapp, "build", "done:built"},
		{"a target that failed", func(f *fixture) { f.t.BuildStepBegan("store"); f.t.BuildStepEnded("store", errors.New("x")) }, ServiceStore, "build", "failed:the build failed"},
		{"a rider target gets its own step", func(f *fixture) { f.t.BuildStepBegan("lock") }, ServiceShim, "build shim-lock", "running:building"},
		{"the Wi-Fi helper rides with the daemon", func(f *fixture) { f.t.BuildStepBegan("wifi") }, ServiceDaemon, "build Wi-Fi helper", "running:building"},
		{"an override builds every service", func(f *fixture) { f.t.BuildStepBegan("build") }, ServiceTests, "build", "running:building"},
		{"the test service's target", func(f *fixture) { f.t.BuildStepBegan("tests") }, ServiceTests, "build", "running:building"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := begun(t)

			// Act
			tt.act(f)

			// Assert
			if got := arm(stepOf(t, service(t, f.feed.last(t), tt.service), tt.step).GetState()); got != tt.want {
				t.Fatalf("step = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestARiderBuildStepSitsBeforeTheInstall(t *testing.T) {
	// Arrange
	f := begun(t)

	// Act
	f.t.BuildStepBegan("lock")

	// Assert
	var names []string
	for _, st := range service(t, f.feed.last(t), ServiceShim).GetSteps() {
		names = append(names, st.GetName().GetText())
	}
	if strings.Join(names, ",") != "build,build shim-lock,install,replace" {
		t.Fatalf("steps %v, want the rider's build before the install", names)
	}
}

func TestTheProtobufsAreTheCardsSharedStep(t *testing.T) {
	// Arrange
	f := begun(t)

	// Act
	f.t.BuildStepBegan("proto")
	f.t.BuildStepEnded("proto", nil)

	// Assert
	steps := f.feed.last(t).GetSteps()
	if len(steps) != 1 || steps[0].GetName().GetText() != "protobufs" || arm(steps[0].GetState()) != "done:generated" {
		t.Fatalf("shared steps %v, want the protobufs generated", steps)
	}
}

func TestAnUnknownBuildTargetIsACallersBug(t *testing.T) {
	// Arrange
	f := begun(t)
	defer func() {
		if recover() == nil {
			t.Fatal("an unknown target did not panic")
		}
	}()

	// Act
	f.t.BuildStepBegan("nonsense")
}

func TestTheInstall(t *testing.T) {
	tests := []struct {
		name         string
		act          func(f *fixture)
		wantInstall  string
		wantPrevious string
	}{
		{"an artifact replaced", func(f *fixture) {
			f.t.ArtifactInstalled(ServiceShim, true, "0123456789abc", true)
		}, "done:installed", "0123456"},
		{"every artifact already the fresh build", func(f *fixture) {
			f.t.ArtifactInstalled(ServiceShim, true, "0123456789abc", false)
		}, "unchanged:already installed", "0123456"},
		{"a rider replaced names no previous build", func(f *fixture) {
			f.t.ArtifactInstalled(ServiceShim, false, "lockbuild", true)
		}, "done:installed", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := begun(t)
			f.t.Built(map[string]string{ServiceShim: "fedcba9876543"})
			f.t.InstallBegan()

			// Act
			tt.act(f)
			f.t.InstallEnded()

			// Assert
			svc := service(t, f.feed.last(t), ServiceShim)
			if got := arm(stepOf(t, svc, "install").GetState()); got != tt.wantInstall {
				t.Fatalf("install = %s, want %s", got, tt.wantInstall)
			}
			if svc.GetBuilds().GetFresh().GetShortHash() != "fedcba9" || svc.GetBuilds().GetPrevious().GetShortHash() != tt.wantPrevious {
				t.Fatalf("builds %v, want fresh fedcba9 and previous %q", svc.GetBuilds(), tt.wantPrevious)
			}
		})
	}
}

func TestServiceRestarts(t *testing.T) {
	tests := []struct {
		name string
		act  func(f *fixture)
		want string
	}{
		{"restarting", func(f *fixture) { f.t.ServiceRestarting(ServiceStore) }, "running:restarting"},
		{"restarted", func(f *fixture) { f.t.ServiceRestarting(ServiceStore); f.t.ServiceRestarted(ServiceStore) }, "done:restarted"},
		{"already current", func(f *fixture) { f.t.ServiceCurrent(ServiceStore) }, "unchanged:already running the fresh build"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := begun(t)

			// Act
			tt.act(f)

			// Assert
			if got := arm(stepOf(t, service(t, f.feed.last(t), ServiceStore), "restart").GetState()); got != tt.want {
				t.Fatalf("restart = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestShimJudgements(t *testing.T) {
	tests := []struct {
		name    string
		outcome ShimOutcome
		want    string
	}{
		{"replaced now", ShimReplacing, "running:replacing"},
		{"behind its turn", ShimWaitsForTurn, "waiting:waits for its turn to end"},
		{"behind its detached work", ShimWaitsForWork, "waiting:waits for its detached work to end"},
		{"behind a replacement under way", ShimQueued, "waiting:waits for a replacement already under way"},
		{"still stale after replacing", ShimStillStale, "failed:still runs the old build after it was replaced"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := begun(t)
			f.t.ShimsJudging()

			// Act
			f.t.ShimJudged(context.Background(), "ws-a", tt.outcome)

			// Assert
			instances := service(t, f.feed.last(t), ServiceShim).GetInstances()
			if len(instances) != 1 || instances[0].GetName().GetText() != "name-ws-a" || arm(instances[0].GetState()) != tt.want {
				t.Fatalf("instances %v, want name-ws-a %s", instances, tt.want)
			}
		})
	}
}

func TestAShimJudgedCurrentAfterItsReplacementIsReplaced(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.ShimsJudging()
	f.t.ShimJudged(context.Background(), "ws-a", ShimReplacing)
	f.t.ShimJudged(context.Background(), "ws-b", ShimWaitsForTurn)
	f.t.ShimsJudged()

	// Act
	f.clock.advance(2 * time.Second)
	f.t.ShimJudged(context.Background(), "ws-a", ShimCurrent)

	// Assert
	svc := service(t, f.feed.last(t), ServiceShim)
	if arm(svc.GetInstances()[0].GetState()) != "done:replaced" || svc.GetSummary().GetText() != "1 of 2 replaced" {
		t.Fatalf("instances %v summary %q, want ws-a replaced and 1 of 2", svc.GetInstances(), svc.GetSummary().GetText())
	}
	if got := arm(stepOf(t, svc, "replace").GetState()); got != "waiting:waits for its turn to end" {
		t.Fatalf("replace = %s, want waiting on ws-b's turn", got)
	}
}

func TestAShimJudgedCurrentThatTheDeployNeverReplacedDrawsNothing(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.ShimsJudging()

	// Act
	f.t.ShimJudged(context.Background(), "ws-a", ShimCurrent)

	// Assert
	if n := len(service(t, f.feed.last(t), ServiceShim).GetInstances()); n != 0 {
		t.Fatalf("instances %d, want none", n)
	}
}

func TestAShimJudgedBeforeTheDeployDecidesShimsIsNoPartOfTheCard(t *testing.T) {
	// Arrange
	f := begun(t)

	// Act
	f.t.ShimJudged(context.Background(), "ws-a", ShimReplacing)

	// Assert
	if n := len(service(t, f.feed.last(t), ServiceShim).GetInstances()); n != 0 {
		t.Fatalf("instances %d, want none", n)
	}
}

func TestEveryShimCurrentIsUnchanged(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.ShimsJudging()

	// Act
	f.t.ShimsJudged()

	// Assert
	svc := service(t, f.feed.last(t), ServiceShim)
	if got := arm(stepOf(t, svc, "replace").GetState()); got != "unchanged:every shim runs the fresh build" {
		t.Fatalf("replace = %s, want unchanged", got)
	}
	if svc.Summary != nil {
		t.Fatalf("summary %q, want none for no instances", svc.GetSummary().GetText())
	}
}

func TestAShimThatCouldNotBeReplacedFails(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.ShimsJudging()
	f.t.ShimJudged(context.Background(), "ws-a", ShimReplacing)

	// Act
	f.t.ShimReplaceFailed("ws-a", errors.New("spawn refused"))

	// Assert
	in := service(t, f.feed.last(t), ServiceShim).GetInstances()[0]
	if got := arm(in.GetState()); got != "failed:could not be replaced: spawn refused" {
		t.Fatalf("instance = %s, want failed with its cause", got)
	}
}

func TestAWorkspaceWhoseNameCannotBeReadIsDrawnByItsIdAndRecorded(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.deps.WorkspaceName = func(context.Context, ids.WorkspaceID) (string, error) { return "", errors.New("gone") }
	f.t.ShimsJudging()

	// Act
	f.t.ShimJudged(context.Background(), "ws-a", ShimReplacing)

	// Assert
	if name := service(t, f.feed.last(t), ServiceShim).GetInstances()[0].GetName().GetText(); name != "ws-a" {
		t.Fatalf("name %q, want the id", name)
	}
	r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opHotReload})
	if !ok || r.Context["workspace"] != "ws-a" {
		t.Fatalf("record %+v, want the unreadable name recorded with its workspace", r)
	}
}

func TestWebviews(t *testing.T) {
	tests := []struct {
		name string
		act  func(f *fixture)
		want string
	}{
		{"told to reload", func(f *fixture) {}, "running:reloading"},
		{"returned on the fresh build", func(f *fixture) { f.t.WebviewReported("ws-a", "web-fresh-build") }, "done:reloaded"},
		{"returned on an older build", func(f *fixture) { f.t.WebviewReported("ws-a", "web-old-build") }, "running:reloading"},
		{"closed while reloading is not skipped", func(f *fixture) { f.t.WebviewGone("ws-a") }, "running:reloading"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := begun(t)
			f.t.Built(map[string]string{ServiceWebapp: "web-fresh-build"})
			f.t.WebviewReloading(context.Background(), "ws-a")

			// Act
			tt.act(f)

			// Assert
			if got := arm(service(t, f.feed.last(t), ServiceWebapp).GetInstances()[0].GetState()); got != tt.want {
				t.Fatalf("webview = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestTheWebViewHostReloads(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.Built(map[string]string{ServiceWebapp: "web-fresh-build"})
	f.t.WebhostReloading()
	f.t.WebappJudged()

	// Act
	f.t.WebhostReported("web-fresh-build")

	// Assert
	svc := service(t, f.feed.last(t), ServiceWebapp)
	in := svc.GetInstances()[0]
	if in.GetName().GetText() != "the web view host" || arm(in.GetState()) != "done:reloaded" || arm(stepOf(t, svc, "reload").GetState()) != "done:1 reloaded" {
		t.Fatalf("webapp %v, want the host reloaded and the step done", svc)
	}
}

func TestElisp(t *testing.T) {
	tests := []struct {
		name string
		act  func(f *fixture)
		want string
	}{
		{"pushed", func(f *fixture) { f.t.ElispPushed(1) }, "done:pushed to 1 Emacs, which loads it itself"},
		{"current", func(f *fixture) { f.t.ElispCurrent() }, "unchanged:every Emacs runs the fresh elisp"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := begun(t)

			// Act
			tt.act(f)

			// Assert
			if got := arm(service(t, f.feed.last(t), ServiceElisp).GetState()); got != tt.want {
				t.Fatalf("elisp = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestAFailedDeployFailsTheStepAtWorkAndSkipsTheRest(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.BuildStepBegan("shim")

	// Act
	f.t.Failed(errors.New("deploy: the shim build failed"))

	// Assert
	card := f.feed.last(t)
	if badge(card) != "failed:deploy: the shim build failed" {
		t.Fatalf("badge %s, want the deploy's account", badge(card))
	}
	if got := arm(service(t, card, ServiceShim).GetState()); got != "failed:the deploy failed" {
		t.Fatalf("shim = %s, want failed", got)
	}
	if got := arm(service(t, card, ServiceStore).GetState()); got != "skipped:the deploy stopped first" {
		t.Fatalf("store = %s, want skipped", got)
	}
}

func TestASettledCardDrawsNoLaterReport(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.Failed(errors.New("boom"))
	drawn := f.feed.drawn()

	// Act
	f.t.ElispPushed(1)

	// Assert
	if f.feed.drawn() != drawn {
		t.Fatalf("drew %d cards after settling, want none", f.feed.drawn()-drawn)
	}
}

func TestADeployThatReloadedEverythingSettlesSucceeded(t *testing.T) {
	// Arrange
	f := begun(t)
	finishEverythingButShims(f)
	f.t.ShimsJudging()

	// Act
	f.clock.advance(time.Minute)
	f.t.ShimsJudged()

	// Assert
	card := f.feed.last(t)
	settled := card.GetSettled()
	if badge(card) != "succeeded" || settled.GetStartedAtMs() != 1_000_000 {
		t.Fatalf("card %s %v, want succeeded from the deploy's start", badge(card), settled)
	}
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Message: "the deploy's hot reload settled"}); !ok {
		t.Fatal("the settle was not recorded")
	}
}

func TestAShimWaitingOnItsTurnKeepsTheCardLive(t *testing.T) {
	// Arrange
	f := begun(t)
	finishEverythingButShims(f)
	f.t.ShimsJudging()
	f.t.ShimJudged(context.Background(), "ws-a", ShimWaitsForTurn)

	// Act
	f.t.ShimsJudged()

	// Assert
	card := f.feed.last(t)
	if badge(card) != "live" || arm(service(t, card, ServiceShim).GetState()) != "waiting:waits for its turn to end" {
		t.Fatalf("card %s shim %s, want live and the shim waiting", badge(card), arm(service(t, card, ServiceShim).GetState()))
	}
}

func TestANewDeploySettlesTheLastOnesStillLiveCard(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.ShimsJudging()
	f.t.ShimJudged(context.Background(), "ws-a", ShimWaitsForTurn)

	// Act
	f.t.Begin(context.Background(), start.Add(time.Hour))

	// Assert
	f.feed.mu.Lock()
	earlier := f.feed.rows[len(f.feed.rows)-2].GetActivity().GetHotReload()
	f.feed.mu.Unlock()
	in := service(t, earlier, ServiceShim).GetInstances()[0]
	if badge(earlier) != "succeeded" || arm(in.GetState()) != "skipped:carried on by the next deploy" {
		t.Fatalf("earlier card %s, instance %s, want settled with its wait carried on", badge(earlier), arm(in.GetState()))
	}
}

func TestADaemonThatMovesLeavesItsSuccessorsServicesWaiting(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.BuildStepBegan("build")
	f.t.BuildStepEnded("build", nil)
	f.t.InstallBegan()
	f.t.InstallEnded()

	// Act
	f.t.DaemonMoving(context.Background(), "handing over every workspace", []ids.WorkspaceID{"ws-a"})

	// Assert
	card := f.feed.last(t)
	if got := arm(stepOf(t, service(t, card, ServiceDaemon), "hand over").GetState()); got != "running:handing over every workspace" {
		t.Fatalf("hand over = %s", got)
	}
	for _, name := range []string{ServiceShim, ServiceElisp} {
		if got := arm(service(t, card, name).GetState()); got != "waiting:waits for the new daemon" {
			t.Fatalf("%s = %s, want waiting for the new daemon", name, got)
		}
	}
	in := service(t, card, ServiceWebapp).GetInstances()[0]
	if in.GetName().GetText() != "name-ws-a" || arm(in.GetState()) != "waiting:reloads as it reattaches" {
		t.Fatalf("webview %v, want ws-a waiting to reattach", in)
	}
}

func TestADaemonThatMovesWithEveryWebviewCurrentLeavesNoWebappWait(t *testing.T) {
	// Arrange
	f := begun(t)

	// Act
	f.t.DaemonMoving(context.Background(), "handing over every workspace", nil)

	// Assert
	if got := arm(stepOf(t, service(t, f.feed.last(t), ServiceWebapp), "reload").GetState()); got != "unchanged:every web view runs the fresh build" {
		t.Fatalf("reload = %s, want unchanged", got)
	}
}

func TestAWaitingWebviewThatClosesIsSkipped(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.DaemonMoving(context.Background(), "handing over every workspace", []ids.WorkspaceID{"ws-a"})

	// Act
	f.t.WebviewGone("ws-a")

	// Assert
	if got := arm(service(t, f.feed.last(t), ServiceWebapp).GetInstances()[0].GetState()); got != "skipped:its web view closed" {
		t.Fatalf("webview = %s, want skipped", got)
	}
}

func TestAHandoffStopsThisDaemonDrawing(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.DaemonMoving(context.Background(), "handing over every workspace", nil)

	// Act
	handoff := f.t.Handoff()
	drawn := f.feed.drawn()
	f.t.ElispPushed(1)

	// Assert
	if len(handoff) == 0 || f.feed.drawn() != drawn {
		t.Fatalf("handoff %d bytes, drew %d after it; want a card handed on and nothing drawn", len(handoff), f.feed.drawn()-drawn)
	}
}

func TestAHandoffWithNoCardIsEmpty(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	handoff := f.t.Handoff()

	// Assert
	if handoff != nil {
		t.Fatalf("handoff = %q, want nil", handoff)
	}
}

// successor is a fixture holding the card INCUMBENT handed off.
func successor(t *testing.T, incumbent *fixture) *fixture {
	t.Helper()
	handoff := incumbent.t.Handoff()
	s := newFixture(t)
	s.t.Resume(handoff)
	return s
}

func TestASuccessorDrawsTheCardOnlyOnceItTakesOver(t *testing.T) {
	// Arrange
	incumbent := begun(t)
	incumbent.t.DaemonMoving(context.Background(), "handing over every workspace", nil)
	s := successor(t, incumbent)
	s.t.ShimJudged(context.Background(), "ws-a", ShimReplacing)
	if s.feed.drawn() != 0 {
		t.Fatalf("the successor drew %d cards before taking over", s.feed.drawn())
	}

	// Act
	s.t.TookOver()

	// Assert
	card := s.feed.last(t)
	if got := arm(stepOf(t, service(t, card, ServiceDaemon), "hand over").GetState()); got != "done:handed over" {
		t.Fatalf("hand over = %s, want done", got)
	}
	shim := service(t, card, ServiceShim)
	if len(shim.GetInstances()) != 1 || arm(shim.GetInstances()[0].GetState()) != "running:replacing" {
		t.Fatalf("shim instances %v, want ws-a replacing", shim.GetInstances())
	}
	ref, _ := feedid.Decode(s.feed.rows[0].GetId())
	if ref.Row.ID != "1000000" || s.feed.ws[0] != "mediator" {
		t.Fatalf("drawn under %+v in %s, want the incumbent's row", ref, s.feed.ws[0])
	}
}

func TestResumingTheSameCardAgainKeepsWhatTheSuccessorLearned(t *testing.T) {
	// Arrange
	incumbent := begun(t)
	incumbent.t.DaemonMoving(context.Background(), "handing over every workspace", nil)
	handoff := incumbent.t.Handoff()
	s := newFixture(t)
	s.t.Resume(handoff)
	s.t.ShimJudged(context.Background(), "ws-a", ShimReplacing)

	// Act
	s.t.Resume(handoff)
	s.t.TookOver()

	// Assert
	if n := len(service(t, s.feed.last(t), ServiceShim).GetInstances()); n != 1 {
		t.Fatalf("instances %d, want the judgement kept", n)
	}
}

func TestResumeRefusesAnUnreadableCardLoudly(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.t.Resume([]byte("{not json"))
	f.t.TookOver()

	// Assert
	if f.feed.drawn() != 0 {
		t.Fatalf("drew %d cards from an unreadable handoff", f.feed.drawn())
	}
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opHotReload}); !ok {
		t.Fatal("the unreadable handoff was not recorded at ERROR")
	}
}

func TestAnEmptyHandoffResumesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.t.Resume(nil)
	f.t.TookOver()

	// Assert
	if f.feed.drawn() != 0 || len(f.log.Records()) != 0 {
		t.Fatalf("drew %d, recorded %v; want nothing", f.feed.drawn(), f.log.Records())
	}
}

func TestAFailedHandoverMakesTheCardTheIncumbentsAgainAndFailsIt(t *testing.T) {
	// Arrange
	f := begun(t)
	f.t.DaemonMoving(context.Background(), "handing over every workspace", nil)
	f.t.Handoff()

	// Act
	f.t.HandoverFailed("the handover was not accepted: busy")

	// Assert
	card := f.feed.last(t)
	if badge(card) != "failed:the handover was not accepted: busy" {
		t.Fatalf("badge %s, want the handover's failure", badge(card))
	}
	if got := arm(service(t, card, ServiceElisp).GetState()); got != "failed:the deploy failed" {
		t.Fatalf("elisp = %s, want its wait failed", got)
	}
}

func TestHandoverEndsWithNoCardDoNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.t.TookOver()
	f.t.HandoverFailed("x")

	// Assert
	if f.feed.drawn() != 0 {
		t.Fatalf("drew %d cards with none tracked", f.feed.drawn())
	}
}
