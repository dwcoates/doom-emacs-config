package desktopnotify

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// fakeEndings answers one filed ending per turn.
type fakeEndings map[ids.TurnID]feed.TurnEnding

func (e fakeEndings) TakeTurnEnding(_ ids.WorkspaceID, turn ids.TurnID) (feed.TurnEnding, bool) {
	ending, ok := e[turn]
	delete(e, turn)
	return ending, ok
}

// fakePoster composes each posted banner at once, under a fixed name.
type fakePoster struct {
	workspaces []ids.WorkspaceID
	kinds      []string
	banners    []Banner
}

func (a *fakePoster) Post(ws ids.WorkspaceID, kind string, compose Compose) {
	a.workspaces = append(a.workspaces, ws)
	a.kinds = append(a.kinds, kind)
	a.banners = append(a.banners, compose(context.Background(), "my-ws"))
}

// fakeSummaries answers a fixed summary and records the answer it was handed.
type fakeSummaries struct{ answer string }

func (s *fakeSummaries) Summarize(_ context.Context, _ ids.WorkspaceID, answer string) string {
	s.answer = answer
	return "the summary"
}

var turnEndedAt = time.Date(2026, 9, 29, 14, 7, 0, 0, time.Local)

type turnFixture struct {
	banners   *TurnBanners
	poster    *fakePoster
	summaries *fakeSummaries
	log       *dlog.TestLogger
}

func newTurnBanners(endings fakeEndings) turnFixture {
	f := turnFixture{poster: &fakePoster{}, summaries: &fakeSummaries{}, log: dlog.NewTestLogger()}
	f.banners = NewTurnBanners(TurnDeps{
		Endings: endings, Poster: f.poster, Summaries: f.summaries,
		Now: func() time.Time { return turnEndedAt }, Log: f.log,
	})
	return f
}

func TestTurnBannerForEachTurnEnd(t *testing.T) {
	cases := []struct {
		name      string
		how       wsm.TurnClose
		ending    feed.TurnEnding
		wantTitle string
		wantBody  string
	}{
		{name: "completed", how: wsm.CloseCompleted, ending: feed.TurnEnding{Answer: "answer"},
			wantTitle: "✅ my-ws turn completed 14:07", wantBody: "𝘵𝘩𝘦 𝘴𝘶𝘮𝘮𝘢𝘳𝘺"},
		{name: "interrupted", how: wsm.CloseKilled, ending: feed.TurnEnding{},
			wantTitle: "✅ my-ws turn completed 14:07", wantBody: "𝘵𝘩𝘦 𝘴𝘶𝘮𝘮𝘢𝘳𝘺"},
		{name: "expected stop", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.ExpectedStop, Error: "stopped by a hook"},
			wantTitle: "✅ my-ws turn completed 14:07", wantBody: "𝘵𝘩𝘦 𝘴𝘶𝘮𝘮𝘢𝘳𝘺"},
		{name: "failed", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.VendorFailed, Error: "rate limited: slow down"},
			wantTitle: "❌ my-ws turn errored 14:07", wantBody: "rate limited: slow down"},
		{name: "vendor blocked", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.VendorBlocked, Error: "usage limit"},
			wantTitle: "❌ my-ws turn errored 14:07", wantBody: "usage limit"},
		{name: "agent died", how: wsm.CloseAgentDied, ending: feed.TurnEnding{Error: "the agent process died"},
			wantTitle: "❌ my-ws turn errored 14:07", wantBody: "the agent process died"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newTurnBanners(fakeEndings{"turn-1": tc.ending})

			// Act
			f.banners.OnTurnEnded("ws1", "turn-1", tc.how)

			// Assert
			if len(f.poster.banners) != 1 {
				t.Fatalf("posted %d banners, want 1", len(f.poster.banners))
			}
			got := f.poster.banners[0]
			want := Banner{Title: tc.wantTitle, Body: tc.wantBody}
			if got != want || f.poster.kinds[0] != KindTurnEnded || f.poster.workspaces[0] != "ws1" {
				t.Fatalf("banner = %+v (kind %s, workspace %s), want %+v for ws1", got, f.poster.kinds[0], f.poster.workspaces[0], want)
			}
		})
	}
}

func TestTurnBannerSummarizesTheFinalAnswer(t *testing.T) {
	// Arrange
	f := newTurnBanners(fakeEndings{"turn-1": {Answer: "the final answer"}})

	// Act
	f.banners.OnTurnEnded("ws1", "turn-1", wsm.CloseCompleted)

	// Assert
	if f.summaries.answer != "the final answer" {
		t.Fatalf("summarized %q, want the final answer", f.summaries.answer)
	}
}

func TestTurnBannerRaisesNothingWithoutALiveEnding(t *testing.T) {
	// Arrange
	f := newTurnBanners(fakeEndings{})

	// Act
	f.banners.OnTurnEnded("ws1", "turn-1", wsm.CloseCompleted)

	// Assert
	if len(f.poster.banners) != 0 {
		t.Fatalf("posted %d banners for a turn with no live ending", len(f.poster.banners))
	}
	if _, ok := hasRecord(f.log, "info", "the turn drew no live ending (a /clear, or a turn this feed never saw); no banner"); !ok {
		t.Fatal("the missing ending left no record")
	}
}

func TestTurnBannerRefusesAnUnknownClose(t *testing.T) {
	// Arrange
	f := newTurnBanners(fakeEndings{"turn-1": {}})

	// Act
	f.banners.OnTurnEnded("ws1", "turn-1", wsm.TurnClose(99))

	// Assert
	if len(f.poster.banners) != 0 {
		t.Fatal("an unknown close announced a banner")
	}
	r, ok := hasRecord(f.log, "error", "a turn close has no turn end; no banner")
	if !ok || r.Context["close"] != 99 || r.Context["turn"] != "turn-1" {
		t.Fatalf("record = %+v (found %v), want the close and turn", r, ok)
	}
}

func TestEventTitle(t *testing.T) {
	cases := []struct {
		name string
		mark string
		ws   string
		what string
		at   time.Time
		want string
	}{
		{name: "an afternoon event", mark: "✅", ws: "my-ws", what: "turn completed", at: turnEndedAt, want: "✅ my-ws turn completed 14:07"},
		{name: "a morning hour is zero-padded", mark: "🔀", ws: "w", what: "merge queued", at: time.Date(2026, 1, 2, 3, 4, 0, 0, time.Local), want: "🔀 w merge queued 03:04"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := eventTitle(tc.mark, tc.ws, tc.what, tc.at)

			// Assert
			if got != tc.want {
				t.Fatalf("eventTitle = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestEveryEventTitleIsComposedByEventTitle pins the shared shape: no banner
// title in the package formats its own clock.
func TestEveryEventTitleIsComposedByEventTitle(t *testing.T) {
	// Arrange / Act
	for _, file := range sourcescan.Production(t) {
		count := strings.Count(string(file.Source), `"15:04"`)

		// Assert
		want := 0
		if file.Name == "turnbanner.go" {
			want = 1
		}
		if count != want {
			t.Errorf("%s formats a banner clock %d times, want %d: compose titles through eventTitle", file.Name, count, want)
		}
	}
}
