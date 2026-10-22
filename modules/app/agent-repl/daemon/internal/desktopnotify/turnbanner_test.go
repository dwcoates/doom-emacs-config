package desktopnotify

import (
	"context"
	"regexp"
	"testing"

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

type turnFixture struct {
	banners   *TurnBanners
	poster    *fakePoster
	summaries *fakeSummaries
	log       *dlog.TestLogger
}

func newTurnBanners(endings fakeEndings) turnFixture {
	f := turnFixture{poster: &fakePoster{}, summaries: &fakeSummaries{}, log: dlog.NewTestLogger()}
	f.banners = NewTurnBanners(TurnDeps{
		Endings: endings, Poster: f.poster, Summaries: f.summaries, Log: f.log,
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
			wantTitle: "✅ my-ws", wantBody: "𝘵𝘩𝘦 𝘴𝘶𝘮𝘮𝘢𝘳𝘺"},
		{name: "interrupted", how: wsm.CloseKilled, ending: feed.TurnEnding{},
			wantTitle: "✅ my-ws", wantBody: "𝘵𝘩𝘦 𝘴𝘶𝘮𝘮𝘢𝘳𝘺"},
		{name: "expected stop", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.ExpectedStop, Error: "stopped by a hook"},
			wantTitle: "✅ my-ws", wantBody: "𝘵𝘩𝘦 𝘴𝘶𝘮𝘮𝘢𝘳𝘺"},
		{name: "failed", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.VendorFailed, Error: "rate limited: slow down"},
			wantTitle: "❌ my-ws", wantBody: "rate limited: slow down"},
		{name: "vendor blocked", how: wsm.CloseFailed, ending: feed.TurnEnding{Failure: ladder.VendorBlocked, Error: "usage limit"},
			wantTitle: "❌ my-ws", wantBody: "usage limit"},
		{name: "agent died", how: wsm.CloseAgentDied, ending: feed.TurnEnding{Error: "the agent process died"},
			wantTitle: "❌ my-ws", wantBody: "the agent process died"},
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
		want string
	}{
		{name: "a turn mark", mark: "✅", ws: "my-ws", want: "✅ my-ws"},
		{name: "a merge mark", mark: "🔀", ws: "w", want: "🔀 w"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := eventTitle(tc.mark, tc.ws)

			// Assert
			if got != tc.want {
				t.Fatalf("eventTitle = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestEveryEventTitleIsComposedByEventTitle pins the shared shape: every
// banner title in the package is the workspace's name, either bare (an agent
// notification) or behind its mark through eventTitle.
func TestEveryEventTitleIsComposedByEventTitle(t *testing.T) {
	titled := regexp.MustCompile(`Title: (.+?), Body`)
	allowed := regexp.MustCompile(`^(name|eventTitle\(mark, name\)|TurnTitle\(end, name\)|MergeQueuedTitle\(name\))$`)
	// Arrange / Act
	seen := 0
	for _, file := range sourcescan.Production(t) {
		for _, m := range titled.FindAllStringSubmatch(string(file.Source), -1) {
			seen++
			// Assert
			if !allowed.MatchString(m[1]) {
				t.Errorf("%s titles a banner %q: compose titles through eventTitle", file.Name, m[1])
			}
		}
	}
	if seen == 0 {
		t.Fatal("found no banner titles to check")
	}
}
