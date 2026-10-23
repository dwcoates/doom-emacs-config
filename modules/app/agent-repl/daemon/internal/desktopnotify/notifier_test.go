package desktopnotify

import (
	"context"
	"errors"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// fakeBackend records each posted banner and its target, reports the banner
// made unless its program is scripted to fail, and answers a scripted outcome.
type fakeBackend struct {
	mu      sync.Mutex
	posted  []Banner
	targets []Target
	outcome Outcome
	err     error
	// madeTwice, when set, reports the banner made twice: a broken backend.
	madeTwice bool
	// block, when set, holds Post until closed or ctx ends.
	block chan struct{}
	// madeSignal, when set, is closed once the banner is reported made.
	madeSignal chan struct{}
	// onPost, when set, runs as Post is called, which is where a test reads
	// what the notifier had already done when it handed the banner over.
	onPost func()
}

func (b *fakeBackend) Program() string { return "fake-banner" }

func (b *fakeBackend) Post(ctx context.Context, target Target, banner Banner, made func()) (Outcome, error) {
	if b.onPost != nil {
		b.onPost()
	}
	b.mu.Lock()
	b.posted = append(b.posted, banner)
	b.targets = append(b.targets, target)
	b.mu.Unlock()
	if b.err == nil {
		made()
		if b.madeTwice {
			made()
		}
		if b.madeSignal != nil {
			close(b.madeSignal)
		}
	}
	if b.block != nil {
		select {
		case <-b.block:
		case <-ctx.Done():
			return Unclicked, ctx.Err()
		}
	}
	return b.outcome, b.err
}

func (b *fakeBackend) postedTargets() []Target {
	b.mu.Lock()
	defer b.mu.Unlock()
	return append([]Target(nil), b.targets...)
}

func (b *fakeBackend) banners() []Banner {
	b.mu.Lock()
	defer b.mu.Unlock()
	return append([]Banner(nil), b.posted...)
}

// fakeClicks records clicked workspaces.
type fakeClicks struct {
	mu  sync.Mutex
	got []ids.WorkspaceID
}

// fakeChime records each play and answers a scripted error.
type fakeChime struct {
	mu    sync.Mutex
	plays int
	err   error
	// block, when set, holds Play until closed or ctx ends.
	block chan struct{}
	// playing, when set, is closed as the first Play begins.
	playing chan struct{}
}

func (c *fakeChime) Program() string { return "fake-chime" }

func (c *fakeChime) Play(ctx context.Context) error {
	c.mu.Lock()
	c.plays++
	first := c.plays == 1
	c.mu.Unlock()
	if first && c.playing != nil {
		close(c.playing)
	}
	if c.block != nil {
		select {
		case <-c.block:
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return c.err
}

func (c *fakeChime) played() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.plays
}

func (c *fakeClicks) NotificationClicked(ws ids.WorkspaceID) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.got = append(c.got, ws)
}

func (c *fakeClicks) clicked() []ids.WorkspaceID {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]ids.WorkspaceID(nil), c.got...)
}

// fakeWorkspaces answers one workspace, or one error.
type fakeWorkspaces struct {
	workspace Workspace
	err       error
}

func (w fakeWorkspaces) Workspace(context.Context, ids.WorkspaceID) (Workspace, error) {
	return w.workspace, w.err
}

// named is the fixture's workspace.
var named = fakeWorkspaces{workspace: Workspace{Name: "ws-name", Dir: "/work/ws-name"}}

type notifierFixture struct {
	notifier *Notifier
	focus    *Focus
	backend  *fakeBackend
	chime    *fakeChime
	clicks   *fakeClicks
	log      *dlog.TestLogger
}

func newNotifierFixture(t *testing.T, workspaces Workspaces) notifierFixture {
	t.Helper()
	log := dlog.NewTestLogger()
	f := notifierFixture{focus: NewFocus(log), backend: &fakeBackend{}, chime: &fakeChime{}, clicks: &fakeClicks{}, log: log}
	f.notifier = New(Deps{Focus: f.focus, Backend: f.backend, Chime: f.chime, Clicks: f.clicks, Workspaces: workspaces, Log: log})
	return f
}

func titledBy(ctx context.Context, name string) Banner {
	return Banner{Title: "title " + name, Body: "body"}
}

// hasRecord reports whether a record at level with operation and message stood.
func hasRecord(log *dlog.TestLogger, level, message string) (dlog.Record, bool) {
	return dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: level, Message: message})
}

func TestNotifierPostsWhenEmacsIsUnfocused(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	got := f.backend.banners()
	if len(got) != 1 || got[0].Title != "title ws-name" {
		t.Fatalf("posted %+v, want one banner titled by the workspace name", got)
	}
}

func TestNotifierRaisesAnAgentNotificationUnderTheWorkspaceName(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)

	// Act
	f.notifier.Raise("ws1", "permission_requested", "Run the tests?")
	f.notifier.wg.Wait()

	// Assert
	got := f.backend.banners()
	if len(got) != 1 || got[0] != (Banner{Title: "ws-name", Body: "Run the tests?"}) {
		t.Fatalf("posted %+v, want the workspace name over the notification's line", got)
	}
}

func TestNotifierPostsWithNoEmacsStream(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.backend.banners(); len(got) != 1 {
		t.Fatalf("posted %d banners with no Emacs stream, want 1", len(got))
	}
}

func TestNotifierPostsNothingWhenEmacsIsFocused(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(true)
	composed := false

	// Act
	f.notifier.Post("ws1", "turn_ended", func(ctx context.Context, name string) Banner {
		composed = true
		return Banner{}
	})
	f.notifier.wg.Wait()

	// Assert
	if got := f.backend.banners(); len(got) != 0 || composed {
		t.Fatalf("posted %d banners (composed=%v) while Emacs was focused, want none", len(got), composed)
	}
	if _, ok := hasRecord(f.log, "info", "Emacs is focused; no desktop banner"); !ok {
		t.Fatal("the suppressed banner left no record")
	}
}

func TestNotifierRechecksFocusAfterComposing(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	release := f.focus.Attach(false)
	_ = release

	// Act
	f.notifier.Post("ws1", "turn_ended", func(ctx context.Context, name string) Banner {
		// Emacs gains focus while the banner is being composed.
		if _, err := f.focus.Report(true); err != nil {
			t.Errorf("Report: %v", err)
		}
		return Banner{Title: "t"}
	})
	f.notifier.wg.Wait()

	// Assert
	if got := f.backend.banners(); len(got) != 0 {
		t.Fatalf("posted %d banners after Emacs gained focus, want none", len(got))
	}
}

func TestNotifierRelaysAClick(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.backend.outcome = Clicked

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.clicks.clicked(); len(got) != 1 || got[0] != "ws1" {
		t.Fatalf("clicks = %v, want [ws1]", got)
	}
}

func TestNotifierPostsToTheWorkspacesTarget(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.backend.postedTargets(); len(got) != 1 || got[0] != (Target{ID: "ws1", Dir: "/work/ws-name"}) {
		t.Fatalf("targets = %+v, want the workspace's id and directory", got)
	}
}

func TestNotifierRecordsAHandedBannerAndRelaysNoClick(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.backend.outcome = Handed

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.clicks.clicked(); len(got) != 0 {
		t.Fatalf("clicks = %v, want none: a handed banner's click selects the workspace itself", got)
	}
	r, ok := hasRecord(f.log, "info", "the platform holds the banner; its click selects the workspace itself")
	if !ok || r.Context["workspace"] != "ws1" || r.Context["kind"] != "turn_ended" {
		t.Fatalf("record = %+v (found=%v), want the handed banner recorded with its workspace and kind", r, ok)
	}
}

func TestNotifierPanicsOnAnUnknownOutcome(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.backend.outcome = Outcome(99)

	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unknown outcome was accepted")
		}
	}()

	// Act
	f.notifier.show("ws1", f.log, titledBy)
}

func TestNotifierRelaysNoClickForADismissal(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.clicks.clicked(); len(got) != 0 {
		t.Fatalf("clicks = %v, want none", got)
	}
}

func TestNotifierRecordsAFailedProgram(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.backend.err = errors.New("exit status 64")

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	r, ok := hasRecord(f.log, "error", "the desktop banner program failed")
	if !ok {
		t.Fatal("a failed banner program left no ERROR record")
	}
	if r.Context["cause"] != "exit status 64" || r.Context["workspace"] != "ws1" || r.Context["title"] != "title ws-name" {
		t.Fatalf("record context = %v, want the cause, workspace and title", r.Context)
	}
	if got := f.clicks.clicked(); len(got) != 0 {
		t.Fatalf("a failed banner relayed clicks %v", got)
	}
}

func TestNotifierRecordsAnUnnameableWorkspace(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, fakeWorkspaces{err: errors.New("no such workspace")})

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	r, ok := hasRecord(f.log, "error", "could not name the workspace; no desktop banner")
	if !ok || r.Context["cause"] != "no such workspace" {
		t.Fatalf("record = %+v (found %v), want the naming failure's cause", r, ok)
	}
	if got := f.backend.banners(); len(got) != 0 {
		t.Fatalf("posted %d banners for an unnameable workspace, want none", len(got))
	}
}

// ctxWorkspaces answers only once its ctx ends, with the ctx's own error: a
// workspace lookup the stand-down cut short.
type ctxWorkspaces struct{}

func (ctxWorkspaces) Workspace(ctx context.Context, _ ids.WorkspaceID) (Workspace, error) {
	<-ctx.Done()
	return Workspace{}, ctx.Err()
}

func TestNotifierCloseEndsANameLookupWithoutAnError(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, ctxWorkspaces{})
	f.notifier.Post("ws1", "turn_ended", titledBy)

	// Act
	f.notifier.Close()

	// Assert
	if _, ok := hasRecord(f.log, "info", "the daemon stood down while the workspace was named; no desktop banner"); !ok {
		t.Fatal("a name lookup cut short by Close left no stand-down record")
	}
	if _, ok := hasRecord(f.log, "error", "could not name the workspace; no desktop banner"); ok {
		t.Fatal("a name lookup cut short by Close was recorded as a naming failure")
	}
	if got := f.backend.banners(); len(got) != 0 {
		t.Fatalf("posted %d banners after the stand-down, want none", len(got))
	}
}

func TestNotifierRecordsAMissingProgram(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	n := New(Deps{
		Focus: NewFocus(log), BackendErr: errors.New("alerter is not installed"), Chime: &fakeChime{},
		Clicks: &fakeClicks{}, Workspaces: named, Log: log,
	})

	// Act
	n.Post("ws1", "turn_ended", titledBy)
	n.wg.Wait()

	// Assert
	r, ok := hasRecord(log, "error", "no desktop banner program; the banner was not posted")
	if !ok || r.Context["cause"] != "alerter is not installed" {
		t.Fatalf("record = %+v (found %v), want the missing program's cause", r, ok)
	}
}

func TestNotifierCloseEndsABannerAwaitingItsClick(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.backend.block = make(chan struct{})
	f.notifier.Post("ws1", "turn_ended", titledBy)

	// Act
	f.notifier.Close()

	// Assert
	if _, ok := hasRecord(f.log, "info", "the daemon stood down while a banner awaited its click"); !ok {
		t.Fatal("a banner cut short by Close left no record")
	}
	if _, ok := hasRecord(f.log, "error", "the desktop banner program failed"); ok {
		t.Fatal("a banner cut short by Close was recorded as a program failure")
	}
}

func TestNotifierPostsNothingAfterClose(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.notifier.Close()

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.backend.banners(); len(got) != 0 {
		t.Fatalf("posted %d banners after Close, want none", len(got))
	}
	if _, ok := hasRecord(f.log, "info", "the daemon is standing down; no desktop banner"); !ok {
		t.Fatal("a banner refused after Close left no record")
	}
}

func TestNotifierRingsTheChimeOnceTheBannerIsMade(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)
	rungBeforeMade := false
	f.backend.onPost = func() { rungBeforeMade = f.chime.played() > 0 }

	// Act
	f.notifier.Post("ws1", KindTurnEnded, titledBy)
	f.notifier.wg.Wait()

	// Assert
	if rungBeforeMade {
		t.Fatal("the chime rang before the banner was handed to the platform")
	}
	if got := f.chime.played(); got != 1 {
		t.Fatalf("played %d chimes for one made banner, want 1", got)
	}
	r, ok := hasRecord(f.log, "info", "playing the chime")
	if !ok || r.Context["workspace"] != "ws1" || r.Context["kind"] != KindTurnEnded || r.Context["program"] != "fake-chime" {
		t.Fatalf("record = %+v (found %v), want the workspace, kind and program", r, ok)
	}
	if _, ok := hasRecord(f.log, "debug", "the chime played"); !ok {
		t.Fatal("a played chime left no record of its end")
	}
}

func TestNotifierRingsTheChimeForEveryKindOfBanner(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)

	// Act
	f.notifier.Raise("ws1", "permission_requested", "Run the tests?")
	f.notifier.wg.Wait()

	// Assert
	if got := f.chime.played(); got != 1 {
		t.Fatalf("played %d chimes for an agent notification's banner, want 1", got)
	}
}

// TestNotifierRingsNothingForABannerNotMade pins that a sound is only ever a
// banner's: every way a banner is not made rings no chime.
func TestNotifierRingsNothingForABannerNotMade(t *testing.T) {
	cases := []struct {
		name       string
		workspaces Workspaces
		focused    bool
		focusLater bool
		noProgram  bool
		programErr error
		wantLevel  string
		wantRecord string
	}{
		{name: "Emacs is focused", workspaces: named, focused: true, wantLevel: "info", wantRecord: "Emacs is focused; no desktop banner"},
		{name: "Emacs gained focus while composing", workspaces: named, focusLater: true, wantLevel: "info", wantRecord: "Emacs was focused while the banner was composed; no desktop banner"},
		{name: "the workspace cannot be named", workspaces: fakeWorkspaces{err: errors.New("no such workspace")}, wantLevel: "error", wantRecord: "could not name the workspace; no desktop banner"},
		{name: "no banner program is installed", workspaces: named, noProgram: true, wantLevel: "error", wantRecord: "no desktop banner program; the banner was not posted"},
		{name: "the banner program failed", workspaces: named, programErr: errors.New("exit status 64"), wantLevel: "error", wantRecord: "the desktop banner program failed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			focus := NewFocus(log)
			focus.Attach(tc.focused)
			chime := &fakeChime{}
			deps := Deps{Focus: focus, Chime: chime, Clicks: &fakeClicks{}, Workspaces: tc.workspaces, Log: log}
			if tc.noProgram {
				deps.BackendErr = errors.New("terminal-notifier is not installed")
			} else {
				deps.Backend = &fakeBackend{err: tc.programErr}
			}
			n := New(deps)
			compose := titledBy
			if tc.focusLater {
				compose = func(ctx context.Context, name string) Banner {
					if _, err := focus.Report(true); err != nil {
						t.Errorf("Report: %v", err)
					}
					return titledBy(ctx, name)
				}
			}

			// Act
			n.Post("ws1", KindTurnEnded, compose)
			n.wg.Wait()

			// Assert
			if got := chime.played(); got != 0 {
				t.Fatalf("played %d chimes for a banner that was not made, want none", got)
			}
			if _, ok := hasRecord(log, tc.wantLevel, tc.wantRecord); !ok {
				t.Fatalf("no %s record %q of the banner not made", tc.wantLevel, tc.wantRecord)
			}
		})
	}
}

func TestNotifierPanicsOnABannerMadeTwice(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.backend.madeTwice = true

	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("a backend reporting one banner made twice was accepted")
		}
		f.notifier.Close()
	}()

	// Act
	f.notifier.show("ws1", f.log, titledBy)
}

func TestNotifierRecordsAFailedChime(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.chime.err = errors.New("exit status 1")

	// Act
	f.notifier.Post("ws1", KindTurnEnded, titledBy)
	f.notifier.wg.Wait()

	// Assert
	r, ok := hasRecord(f.log, "error", "the chime program failed")
	if !ok || r.Context["cause"] != "exit status 1" || r.Context["workspace"] != "ws1" || r.Context["program"] != "fake-chime" {
		t.Fatalf("record = %+v (found %v), want the cause, workspace and program", r, ok)
	}
}

func TestNotifierRecordsAMissingChime(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	n := New(Deps{
		Focus: NewFocus(log), Backend: &fakeBackend{}, ChimeErr: errors.New("afplay is not installed"),
		Clicks: &fakeClicks{}, Workspaces: named, Log: log,
	})

	// Act
	n.Post("ws1", KindTurnEnded, titledBy)
	n.wg.Wait()

	// Assert
	r, ok := hasRecord(log, "error", "no chime program; the chime was not played")
	if !ok || r.Context["cause"] != "afplay is not installed" || r.Context["workspace"] != "ws1" {
		t.Fatalf("record = %+v (found %v), want the missing program's cause", r, ok)
	}
}

func TestNotifierCloseEndsAPlayingChime(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.chime.block = make(chan struct{})
	f.chime.playing = make(chan struct{})
	f.notifier.Post("ws1", KindTurnEnded, titledBy)
	<-f.chime.playing

	// Act
	f.notifier.Close()

	// Assert
	if _, ok := hasRecord(f.log, "info", "the daemon stood down while the chime played"); !ok {
		t.Fatal("a chime cut short by Close left no record")
	}
	if _, ok := hasRecord(f.log, "error", "the chime program failed"); ok {
		t.Fatal("a chime cut short by Close was recorded as a program failure")
	}
}

func TestNotifierRingsNothingAfterClose(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.notifier.Close()

	// Act
	f.notifier.ring(f.log)
	f.notifier.wg.Wait()

	// Assert
	if got := f.chime.played(); got != 0 {
		t.Fatalf("played %d chimes after Close, want none", got)
	}
	if _, ok := hasRecord(f.log, "info", "the daemon is standing down; no chime"); !ok {
		t.Fatal("a chime refused after Close left no record")
	}
}

func TestNewRefusesAHalfWiredNotifier(t *testing.T) {
	cases := []struct {
		name string
		deps Deps
	}{
		{name: "no focus", deps: Deps{Backend: &fakeBackend{}, Chime: &fakeChime{}, Clicks: &fakeClicks{}, Workspaces: fakeWorkspaces{}, Log: dlog.NewTestLogger()}},
		{name: "both backend and error", deps: Deps{Focus: NewFocus(dlog.NewTestLogger()), Backend: &fakeBackend{}, BackendErr: errors.New("x"), Chime: &fakeChime{}, Clicks: &fakeClicks{}, Workspaces: fakeWorkspaces{}, Log: dlog.NewTestLogger()}},
		{name: "neither backend nor error", deps: Deps{Focus: NewFocus(dlog.NewTestLogger()), Chime: &fakeChime{}, Clicks: &fakeClicks{}, Workspaces: fakeWorkspaces{}, Log: dlog.NewTestLogger()}},
		{name: "both chime and error", deps: Deps{Focus: NewFocus(dlog.NewTestLogger()), Backend: &fakeBackend{}, Chime: &fakeChime{}, ChimeErr: errors.New("x"), Clicks: &fakeClicks{}, Workspaces: fakeWorkspaces{}, Log: dlog.NewTestLogger()}},
		{name: "neither chime nor error", deps: Deps{Focus: NewFocus(dlog.NewTestLogger()), Backend: &fakeBackend{}, Clicks: &fakeClicks{}, Workspaces: fakeWorkspaces{}, Log: dlog.NewTestLogger()}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			defer func() {
				if recover() == nil {
					t.Fatal("New accepted a half-wired notifier")
				}
			}()

			// Act
			New(tc.deps)
		})
	}
}
