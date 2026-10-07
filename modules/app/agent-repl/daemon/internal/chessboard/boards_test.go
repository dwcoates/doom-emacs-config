package chessboard

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// boardsWait bounds a test's wait for a board to settle: every backend
// command is a fake answering at once, so a settle takes milliseconds.
const boardsWait = 5 * time.Second

var testSession = Session{ID: "agent-a", GameID: "g-1"}

// boardsFixture is a Boards over a fake runner, checkout and cee-webapp.
type boardsFixture struct {
	boards *Boards
	runner *fakeRunner
	webapp *fakeWebapp
	log    *dlog.TestLogger
	cli    string
	// settles receives every session whose resolution ended.
	settles chan Session
}

func newBoardsFixture(t *testing.T) *boardsFixture {
	t.Helper()
	checkout, cli := fakeCheckout(t)
	r := newFakeRunner()
	buildsTheWidget(r, cli)
	buildsTheWebapp(r, "webapp")
	w := newFakeWebapp(t)
	servesAt(r, w.server.URL)
	log := dlog.NewTestLogger()
	life, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	b, err := New(Deps{
		Log: log, Run: r, Getenv: envOf(map[string]string{EngineDirEnv: checkout}),
		PluginDir: t.TempDir(), HTTP: http.DefaultClient, Life: life,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	settles := make(chan Session, 64)
	b.settled = func(s Session) { settles <- s }
	return &boardsFixture{boards: b, runner: r, webapp: w, log: log, cli: cli, settles: settles}
}

// settled waits for s's next resolution to end and answers its bubble.
func (f *boardsFixture) settled(t *testing.T, s Session) *frontendv1.FeedChessBoard {
	t.Helper()
	timeout := time.NewTimer(boardsWait)
	defer timeout.Stop()
	for {
		select {
		case got := <-f.settles:
			if got != s {
				continue
			}
			f.boards.mu.Lock()
			defer f.boards.mu.Unlock()
			return bubble(headingFor(s), f.boards.boards[s].body)
		case <-timeout.C:
			t.Fatalf("board %v did not settle within %v", s, boardsWait)
		}
	}
}

func TestNewRefusesMissingDeps(t *testing.T) {
	full := Deps{Log: dlog.NewTestLogger(), Run: newFakeRunner(), Getenv: envOf(nil), PluginDir: "/p", HTTP: http.DefaultClient, Life: context.Background()}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "log", strip: func(d *Deps) { d.Log = nil }},
		{name: "run", strip: func(d *Deps) { d.Run = nil }},
		{name: "getenv", strip: func(d *Deps) { d.Getenv = nil }},
		{name: "plugin dir", strip: func(d *Deps) { d.PluginDir = "" }},
		{name: "http", strip: func(d *Deps) { d.HTTP = nil }},
		{name: "life", strip: func(d *Deps) { d.Life = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			deps := full
			tt.strip(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatalf("New without %s succeeded", tt.name)
			}
		})
	}
}

func TestAFirstDrawShowsTheBoardPreparing(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget([]byte{0x0a, 0x00})

	// Act.
	view := f.boards.Board(testSession)

	// Assert.
	if view.GetHeading().GetText() != "Chess board · CEE session agent-a" || view.GetPreparing() == nil {
		t.Fatalf("first draw = %v, want the heading and the preparing arm", view)
	}
	f.settled(t, testSession)
}

func TestABoardBecomesReadyWithTheWidgetBundleAndToken(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget([]byte{0x0a, 0x00})
	f.boards.Board(testSession)

	// Act.
	ready := f.settled(t, testSession).GetReady()

	// Assert.
	stamp, _ := widgetStamp(f.cli)
	if string(ready.GetWidget().GetCeeWebWidget()) != string([]byte{0x0a, 0x00}) ||
		ready.GetBundle().GetScriptUrl() != BundleRoute+stamp+"/"+WidgetScript ||
		ready.GetBundle().GetStylesheetUrl() != BundleRoute+stamp+"/"+WidgetStylesheet {
		t.Fatalf("ready = %v, want the widget bytes and the stamped bundle", ready)
	}
	if s, err := SessionFromToken(ready.GetSquareToken()); err != nil || s != testSession {
		t.Fatalf("square token decodes to %v, %v; want the board's session", s, err)
	}
}

func TestABoardWhoseSessionIsGoneIsUnavailable(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.refuse(procGetCeeWebWidget, "failed_precondition")
	f.boards.Board(testSession)

	// Act.
	view := f.settled(t, testSession)

	// Assert.
	if view.GetUnavailable().GetReason().GetText() != "CEE session agent-a no longer holds game g-1." {
		t.Fatalf("view = %v, want the session-gone line", view)
	}
}

func TestABoardWhoseBackendRefusesIsUnavailableAndLogged(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.refuse(procGetCeeWebWidget, "internal")
	f.boards.Board(testSession)

	// Act.
	view := f.settled(t, testSession)

	// Assert.
	if !strings.HasPrefix(view.GetUnavailable().GetReason().GetText(), "The chess widget's backend did not answer: ") {
		t.Fatalf("view = %v, want the backend line", view)
	}
	for _, rec := range f.log.Records() {
		if rec.Level == "error" && rec.Operation == opBoard && rec.Context["cee_session_id"] == "agent-a" {
			return
		}
	}
	t.Fatalf("no ERROR %s record naming the session in %+v", opBoard, f.log.Records())
}

func TestABoardWhoseBackendCannotBeReadiedIsUnavailable(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.runner.on("npm ci", func(string, []string) (string, int, error) { return "npm error E401", 1, nil })
	f.boards.Board(testSession)

	// Act.
	view := f.settled(t, testSession)

	// Assert.
	if view.GetUnavailable().GetReason().GetText() != "Building the chess widget failed: npm error E401" {
		t.Fatalf("view = %v, want the build failure", view)
	}
}

func TestARedrawOfAReadyBoardKeepsItReadyWhileReResolving(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget([]byte{0x0a, 0x00})
	f.boards.Board(testSession)
	f.settled(t, testSession)

	// Act.
	view := f.boards.Board(testSession)

	// Assert.
	if view.GetReady() == nil {
		t.Fatalf("redraw = %v, want the ready board while it is re-resolved", view)
	}
	f.settled(t, testSession)
}

func TestARedrawAfterTheSessionWentAwayTurnsTheBoardUnavailable(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget([]byte{0x0a, 0x00})
	f.boards.Board(testSession)
	f.settled(t, testSession)
	f.webapp.refuse(procGetCeeWebWidget, "failed_precondition")

	// Act.
	f.boards.Board(testSession)
	view := f.settled(t, testSession)

	// Assert.
	if view.GetUnavailable() == nil {
		t.Fatalf("view = %v, want unavailable once the session is gone", view)
	}
}

func TestChangedAnswersEveryBoardThatChangedSinceItLastAnswered(t *testing.T) {
	// Arrange. agent-a's changes are drained first, so only agent-b's remain.
	f := newBoardsFixture(t)
	f.webapp.answerWidget(nil)
	ctx, cancel := context.WithTimeout(context.Background(), boardsWait)
	defer cancel()
	f.boards.Board(testSession)
	f.settled(t, testSession)
	if _, err := f.boards.Changed(ctx); err != nil {
		t.Fatalf("drain agent-a: %v", err)
	}
	other := Session{ID: "agent-b", GameID: "g-2"}
	f.boards.Board(other)
	f.settled(t, other)

	// Act.
	got, err := f.boards.Changed(ctx)

	// Assert.
	if err != nil || len(got) != 1 || got[0] != other {
		t.Fatalf("Changed() = %v, %v; want agent-b alone", got, err)
	}
}

func TestChangedAnswersTheContextsEnd(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := f.boards.Changed(ctx)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Changed() error = %v, want context.Canceled", err)
	}
}

func TestRunHandsEveryChangeToApplyAndStopsWithItsContext(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget(nil)
	ctx, cancel := context.WithCancel(context.Background())
	applied := make(chan []Session, 8)
	done := make(chan struct{})
	go func() { f.boards.Run(ctx, func(s []Session) { applied <- s }); close(done) }()

	// Act.
	f.boards.Board(testSession)
	got := <-applied
	cancel()
	<-done

	// Assert.
	if len(got) != 1 || got[0] != testSession {
		t.Fatalf("applied %v, want the board's session", got)
	}
}

func TestFailedRequestNamesTheSessionAndWhatTheCallSaid(t *testing.T) {
	// Act.
	view := FailedRequest(&conversationv1.AgentChessBoardSession{SessionId: "agent-a", GameId: "g-1"}, "invalid input")

	// Assert.
	if view.GetHeading().GetText() != "Chess board · CEE session agent-a" ||
		view.GetUnavailable().GetReason().GetText() != "The agent's board request failed: invalid input" {
		t.Fatalf("view = %v, want the session heading and the call's words", view)
	}
}

func TestFailedRequestWithNoSessionOrWordsStillSaysItFailed(t *testing.T) {
	// Act.
	view := FailedRequest(nil, "")

	// Assert.
	if view.GetHeading().GetText() != "Chess board" ||
		view.GetUnavailable().GetReason().GetText() != "The agent's board request failed." {
		t.Fatalf("view = %v, want the bare heading and the bare failure", view)
	}
}

func TestInspectSquareAnswersCeeWebappsAnswerWhole(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answers[procGetSquareEvents] = func() (int, []byte) { return http.StatusOK, []byte{0x08, 0x01} }

	// Act.
	got, err := f.boards.InspectSquare(context.Background(), testSession, 7, 1)

	// Assert.
	if err != nil || string(got) != string([]byte{0x08, 0x01}) {
		t.Fatalf("InspectSquare() = %v, %v; want the answer whole", got, err)
	}
}

func TestInspectSquareOnAGoneSessionAnswersSessionGone(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.refuse(procGetSquareEvents, "failed_precondition")

	// Act.
	_, err := f.boards.InspectSquare(context.Background(), testSession, 7, 1)

	// Assert.
	if !errors.Is(err, ErrSessionGone) {
		t.Fatalf("InspectSquare() error = %v, want ErrSessionGone", err)
	}
}

func TestInspectSquareWithNoAnswerIsABackendErrorAndLogged(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.refuse(procGetSquareEvents, "unavailable")

	// Act.
	_, err := f.boards.InspectSquare(context.Background(), testSession, 7, 1)

	// Assert.
	if !errors.Is(err, ErrBackend) {
		t.Fatalf("InspectSquare() error = %v, want ErrBackend", err)
	}
	for _, rec := range f.log.Records() {
		if rec.Level == "error" && rec.Operation == opSquare && rec.Context["square"] == uint32(1) {
			return
		}
	}
	t.Fatalf("no ERROR %s record naming the square in %+v", opSquare, f.log.Records())
}

func TestInspectSquareWhoseBackendWontStartIsABackendError(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.runner.on("env", func(string, []string) (string, int, error) { return "no plugin", 1, nil })

	// Act.
	_, err := f.boards.InspectSquare(context.Background(), testSession, 7, 1)

	// Assert.
	if !errors.Is(err, ErrBackend) {
		t.Fatalf("InspectSquare() error = %v, want ErrBackend", err)
	}
}

// serveBundle requests path from the boards' bundle route.
func serveBundle(f *boardsFixture, path string) *httptest.ResponseRecorder {
	rec := httptest.NewRecorder()
	f.boards.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, path, nil))
	return rec
}

func TestTheBundleRouteServesTheCurrentBuildsFiles(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget(nil)
	f.boards.Board(testSession)
	url := f.settled(t, testSession).GetReady().GetBundle().GetScriptUrl()

	// Act.
	rec := serveBundle(f, url)

	// Assert.
	if rec.Code != http.StatusOK || rec.Body.String() != "built" {
		t.Fatalf("GET %s = %d %q, want the built script", url, rec.Code, rec.Body.String())
	}
}

func TestTheBundleRouteRefusesAStaleStamp(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget(nil)
	f.boards.Board(testSession)
	f.settled(t, testSession)

	// Act.
	rec := serveBundle(f, BundleRoute+"0000000000000000/"+WidgetScript)

	// Assert.
	if rec.Code != http.StatusNotFound {
		t.Fatalf("a stale stamp answered %d, want 404", rec.Code)
	}
}

func TestTheBundleRouteRefusesAnyOtherFile(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget(nil)
	f.boards.Board(testSession)
	url := f.settled(t, testSession).GetReady().GetBundle().GetScriptUrl()
	other := strings.TrimSuffix(url, WidgetScript) + widgetStampFile

	// Act.
	rec := serveBundle(f, other)

	// Assert.
	if rec.Code != http.StatusNotFound {
		t.Fatalf("GET %s answered %d, want 404", other, rec.Code)
	}
}

func TestTheBundleRouteRefusesBeforeAnyBuild(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)

	// Act.
	rec := serveBundle(f, BundleRoute+"x/"+WidgetScript)

	// Assert.
	if rec.Code != http.StatusNotFound {
		t.Fatalf("a request before any build answered %d, want 404", rec.Code)
	}
}

func TestTheBundleRouteLogsAMissingBuildFile(t *testing.T) {
	// Arrange.
	f := newBoardsFixture(t)
	f.webapp.answerWidget(nil)
	f.boards.Board(testSession)
	url := f.settled(t, testSession).GetReady().GetBundle().GetStylesheetUrl()
	removeFile(t, filepath.Join(widgetDist(f.cli), WidgetStylesheet))

	// Act.
	rec := serveBundle(f, url)

	// Assert.
	if rec.Code != http.StatusNotFound {
		t.Fatalf("a missing file answered %d, want 404", rec.Code)
	}
	for _, r := range f.log.Records() {
		if r.Level == "error" && r.Operation == opBundle {
			return
		}
	}
	t.Fatalf("no ERROR %s record in %+v", opBundle, f.log.Records())
}
