package desktopnotify

import (
	"context"
	"errors"
	"os"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// fakeBackend records each posted banner and answers a scripted click.
type fakeBackend struct {
	mu      sync.Mutex
	posted  []Banner
	clicked bool
	err     error
	// block, when set, holds Post until closed or ctx ends.
	block chan struct{}
}

func (b *fakeBackend) Program() string { return "fake-banner" }

func (b *fakeBackend) Post(ctx context.Context, _ ids.WorkspaceID, banner Banner) (bool, error) {
	b.mu.Lock()
	b.posted = append(b.posted, banner)
	b.mu.Unlock()
	if b.block != nil {
		select {
		case <-b.block:
		case <-ctx.Done():
			return false, ctx.Err()
		}
	}
	return b.clicked, b.err
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

// fakeNames answers one name, or one error.
type fakeNames struct {
	name string
	err  error
}

func (n fakeNames) WorkspaceName(context.Context, ids.WorkspaceID) (string, error) {
	return n.name, n.err
}

type notifierFixture struct {
	notifier *Notifier
	focus    *Focus
	backend  *fakeBackend
	clicks   *fakeClicks
	log      *dlog.TestLogger
}

func newNotifierFixture(t *testing.T, names Names) notifierFixture {
	t.Helper()
	log := dlog.NewTestLogger()
	f := notifierFixture{focus: NewFocus(log), backend: &fakeBackend{}, clicks: &fakeClicks{}, log: log}
	f.notifier = New(Deps{Focus: f.focus, Backend: f.backend, Clicks: f.clicks, Names: names, Log: log})
	return f
}

func titledBy(ctx context.Context, name string) Banner {
	return Banner{Title: "title " + name, Body: "body"}
}

// hasRecord reports whether a record at level with operation and message stood.
func hasRecord(log *dlog.TestLogger, level, message string) (dlog.Record, bool) {
	for _, r := range log.Records() {
		if r.Level == level && r.Message == message {
			return r, true
		}
	}
	return dlog.Record{}, false
}

func TestNotifierPostsWhenEmacsIsUnfocused(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})

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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})

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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
	release := f.focus.Attach(false)
	_ = release

	// Act
	f.notifier.Post("ws1", "turn_ended", func(ctx context.Context, name string) Banner {
		// Emacs gains focus while the banner is being composed.
		if err := f.focus.Report(true); err != nil {
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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
	f.backend.clicked = true

	// Act
	f.notifier.Post("ws1", "turn_ended", titledBy)
	f.notifier.wg.Wait()

	// Assert
	if got := f.clicks.clicked(); len(got) != 1 || got[0] != "ws1" {
		t.Fatalf("clicks = %v, want [ws1]", got)
	}
}

func TestNotifierRelaysNoClickForADismissal(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})

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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
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
	f := newNotifierFixture(t, fakeNames{err: errors.New("no such workspace")})

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

func TestNotifierRecordsAMissingProgram(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	n := New(Deps{
		Focus: NewFocus(log), BackendErr: errors.New("alerter is not installed"),
		Clicks: &fakeClicks{}, Names: fakeNames{name: "ws-name"}, Log: log,
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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
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
	f := newNotifierFixture(t, fakeNames{name: "ws-name"})
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

func TestNewRefusesAHalfWiredNotifier(t *testing.T) {
	cases := []struct {
		name string
		deps Deps
	}{
		{name: "no focus", deps: Deps{Backend: &fakeBackend{}, Clicks: &fakeClicks{}, Names: fakeNames{}, Log: dlog.NewTestLogger()}},
		{name: "both backend and error", deps: Deps{Focus: NewFocus(dlog.NewTestLogger()), Backend: &fakeBackend{}, BackendErr: errors.New("x"), Clicks: &fakeClicks{}, Names: fakeNames{}, Log: dlog.NewTestLogger()}},
		{name: "neither backend nor error", deps: Deps{Focus: NewFocus(dlog.NewTestLogger()), Clicks: &fakeClicks{}, Names: fakeNames{}, Log: dlog.NewTestLogger()}},
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

func TestStoodDown(t *testing.T) {
	live := context.Background()
	ended, cancel := context.WithCancel(context.Background())
	cancel()
	failed := errors.New("signal: killed")
	cases := []struct {
		name string
		ctx  context.Context
		err  error
		want bool
	}{
		{"a failure under an ended lifetime is a stand-down", ended, failed, true},
		{"a failure under a live lifetime is a real failure", live, failed, false},
		{"no failure under an ended lifetime is no stand-down", ended, nil, false},
		{"no failure under a live lifetime is no stand-down", live, nil, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := stoodDown(tc.ctx, tc.err)

			// Assert
			if got != tc.want {
				t.Fatalf("stoodDown = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestStandDownSitesShareStoodDown fails a site that hand-rolls the
// stand-down test instead of asking stoodDown: stoodDown's own body is the
// one place the lifetime's end is read.
func TestStandDownSitesShareStoodDown(t *testing.T) {
	cases := []struct {
		file      string
		wantReads int
	}{
		{"notifier.go", 1},
		{"summary.go", 0},
	}
	for _, tc := range cases {
		t.Run(tc.file, func(t *testing.T) {
			// Arrange
			src, err := os.ReadFile(tc.file)
			if err != nil {
				t.Fatalf("read %s: %v", tc.file, err)
			}

			// Act
			asks := strings.Count(string(src), "stoodDown(")
			reads := strings.Count(string(src), "ctx.Err() != nil")

			// Assert
			if asks == 0 || reads != tc.wantReads {
				t.Fatalf("%s: stoodDown( x%d, ctx.Err() != nil x%d (want %d)", tc.file, asks, reads, tc.wantReads)
			}
		})
	}
}
