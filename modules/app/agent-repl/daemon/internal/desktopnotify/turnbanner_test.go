package desktopnotify

import (
	"context"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/wsm"
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
	kinds   []string
	banners []Banner
}

func (p *fakePoster) Post(_ ids.WorkspaceID, kind string, compose Compose) {
	p.kinds = append(p.kinds, kind)
	p.banners = append(p.banners, compose(context.Background(), "my-ws"))
}

// fakeSummaries answers a fixed summary and records the answer it was handed.
type fakeSummaries struct{ answer string }

func (s *fakeSummaries) Summarize(_ context.Context, _ ids.WorkspaceID, answer string) string {
	s.answer = answer
	return "the summary"
}

var turnEndedAt = time.Date(2026, 9, 29, 14, 7, 0, 0, time.Local)

func newTurnBanners(endings fakeEndings) (*TurnBanners, *fakePoster, *fakeSummaries, *dlog.TestLogger) {
	poster := &fakePoster{}
	summaries := &fakeSummaries{}
	log := dlog.NewTestLogger()
	t := NewTurnBanners(TurnDeps{
		Endings: endings, Poster: poster, Summaries: summaries,
		Now: func() time.Time { return turnEndedAt }, Log: log,
	})
	return t, poster, summaries, log
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
			wantTitle: "✅ my-ws turn completed 14:07", wantBody: "the summary"},
		{name: "interrupted", how: wsm.CloseKilled, ending: feed.TurnEnding{},
			wantTitle: "✅ my-ws turn completed 14:07", wantBody: "the summary"},
		{name: "expected stop", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.ExpectedStop, Error: "stopped by a hook"},
			wantTitle: "✅ my-ws turn completed 14:07", wantBody: "the summary"},
		{name: "failed", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.TurnFailed, Error: "rate limited: slow down"},
			wantTitle: "❌ my-ws turn errored 14:07", wantBody: "rate limited: slow down"},
		{name: "vendor blocked", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.VendorBlocked, Error: "usage limit"},
			wantTitle: "❌ my-ws turn errored 14:07", wantBody: "usage limit"},
		{name: "agent died", how: wsm.CloseAgentDied, ending: feed.TurnEnding{Error: "the agent process died"},
			wantTitle: "❌ my-ws turn errored 14:07", wantBody: "the agent process died"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			banners, poster, _, _ := newTurnBanners(fakeEndings{"turn-1": tc.ending})

			// Act
			banners.OnTurnEnded("ws1", "turn-1", tc.how)

			// Assert
			if len(poster.banners) != 1 {
				t.Fatalf("posted %d banners, want 1", len(poster.banners))
			}
			got := poster.banners[0]
			if got.Title != tc.wantTitle || got.Body != tc.wantBody || poster.kinds[0] != KindTurnEnded {
				t.Fatalf("banner = %+v (kind %s), want %q / %q", got, poster.kinds[0], tc.wantTitle, tc.wantBody)
			}
		})
	}
}

func TestTurnBannerSummarizesTheFinalAnswer(t *testing.T) {
	// Arrange
	banners, _, summaries, _ := newTurnBanners(fakeEndings{"turn-1": {Answer: "the final answer"}})

	// Act
	banners.OnTurnEnded("ws1", "turn-1", wsm.CloseCompleted)

	// Assert
	if summaries.answer != "the final answer" {
		t.Fatalf("summarized %q, want the final answer", summaries.answer)
	}
}

func TestTurnBannerRaisesNothingWithoutALiveEnding(t *testing.T) {
	// Arrange
	banners, poster, _, log := newTurnBanners(fakeEndings{})

	// Act
	banners.OnTurnEnded("ws1", "turn-1", wsm.CloseCompleted)

	// Assert
	if len(poster.banners) != 0 {
		t.Fatalf("posted %d banners for a turn with no live ending", len(poster.banners))
	}
	if _, ok := hasRecord(log, "info", "the turn drew no live ending (a /clear, or a turn this feed never saw); no banner"); !ok {
		t.Fatal("the missing ending left no record")
	}
}

func TestTurnBannerRefusesAnUnknownClose(t *testing.T) {
	// Arrange
	banners, poster, _, log := newTurnBanners(fakeEndings{"turn-1": {}})

	// Act
	banners.OnTurnEnded("ws1", "turn-1", wsm.TurnClose(99))

	// Assert
	if len(poster.banners) != 0 {
		t.Fatal("an unknown close raised a banner")
	}
	r, ok := hasRecord(log, "error", "a turn close has no turn end; no banner")
	if !ok || r.Context["close"] != 99 || r.Context["turn"] != "turn-1" {
		t.Fatalf("record = %+v (found %v), want the close and turn", r, ok)
	}
}
