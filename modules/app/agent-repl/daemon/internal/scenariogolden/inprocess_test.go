package scenariogolden

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/integration/fakeshim/shimfacts"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/imageorigin"
	"agent-repld/internal/paint"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/sessionwatcher/watchtest"
	"agent-repld/internal/vocab"
)

// goldensDir is the scenario goldens, from this package's directory.
const goldensDir = "../../../testdata/scenario-goldens"

// vocabDir is the vocabulary the daemon's painter is built from.
const vocabDir = "../../../proto/vocab"

// inProcessBound bounds every wait of the in-process drive: a frame routes
// and draws in well under a millisecond in process (measured 2026-11-20), so
// this is the fallback for a hang, never the channel a failure takes.
const inProcessBound = 10 * time.Second

// TestEveryScenarioGoldenProjectsToItsFeedGolden is the daemon half of every
// mocked-vendor scenario (SCENARIO-MATRIX.md, ruling (a) of 2026-10-14), IN
// PROCESS: the frames the shim's fold produced for the scenario (frames.json,
// the shim half's golden) are served exactly as the fake shim serves its store
// (shimfacts, Drive's pointer rule), to the REAL session watcher routing into
// the REAL feed resolver built with the daemon's own painter, sentinel
// stripper and image resolver, and the root feed drawn must match the
// scenario's feed.json. No daemon, shim process, repository or log file is
// made: each scenario owns its own watcher and resolver (owner ruling
// 2026-11-20, daemon/AGENTS.md "Projection goldens run in process"). The
// routing seam through a real daemon is proved end to end by
// daemon/integration/scenariogoldens_test.go.
func TestEveryScenarioGoldenProjectsToItsFeedGolden(t *testing.T) {
	t.Parallel()
	write := os.Getenv(WriteEnv) == "1"
	for _, name := range Names(t, goldensDir) {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			d := newInProcess(t)
			got := Drive(t, d, goldensDir, name)
			path := filepath.Join(goldensDir, name, "feed.json")
			warnings := WarningOperations(d.warnings())
			if write {
				WriteFeedGolden(t, path, FeedGolden{Rows: got, Warnings: warnings})
				return
			}
			want := ReadFeedGolden(t, path)
			if string(got) != string(want.Rows) {
				t.Fatalf("the feed drawn for %s differs from its golden %s (rewrite with %s=1 if the change is intended):\ngot:\n%s\nwant:\n%s",
					name, path, WriteEnv, got, want.Rows)
			}
			if !equalStrings(warnings, want.Warnings) {
				t.Fatalf("drawing %s recorded warnings %v, want its golden's %v", name, warnings, want.Warnings)
			}
		})
	}
}

// equalStrings reports whether A and B hold the same strings in order, a nil
// and an empty list alike.
func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// inProcess drives one scenario through a real watcher and resolver.
type inProcess struct {
	ws      ids.WorkspaceID
	log     *dlog.TestLogger
	client  *watchtest.Client
	feed    feed.Resolver
	watcher sessionwatcher.Watcher
	session *watchtest.Stream[*shimv1.WatchSessionResponse]
	// agents is each agent's watch stream, by agent id ("" never: the main
	// agent's is under its own id).
	agents map[string]*watchtest.Stream[*shimv1.WatchAgentResponse]
	turns  int
}

func newInProcess(t *testing.T) *inProcess {
	t.Helper()
	surf := dlog.NewTestSurfaces()
	log := surf.Global().(*dlog.TestLogger)
	classes, err := vocab.LoadPaintClasses(vocabDir)
	if err != nil {
		t.Fatalf("load the paint classes: %v", err)
	}
	painter, err := paint.New(classes)
	if err != nil {
		t.Fatalf("build the painter: %v", err)
	}
	images, err := imageorigin.New(log)
	if err != nil {
		t.Fatalf("build the image origin: %v", err)
	}
	resolveImage, err := feed.PathImageResolver(images.Register, log)
	if err != nil {
		t.Fatalf("build the image resolver: %v", err)
	}
	dir := t.TempDir()
	resolver, err := feed.New(feed.Deps{
		Log:            surf,
		Instance:       "scenario-golden",
		WorkspaceDir:   func(ids.WorkspaceID) (string, error) { return dir, nil },
		Painter:        painter,
		StripSentinels: prompts.SentinelStripper(log),
		ResolveImage:   resolveImage,
	})
	if err != nil {
		t.Fatalf("build the feed resolver: %v", err)
	}
	d := &inProcess{
		ws: ids.WorkspaceID("ws-" + t.Name()), log: log, client: watchtest.NewClient(), feed: resolver,
		agents: map[string]*watchtest.Stream[*shimv1.WatchAgentResponse]{},
	}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	started := shimfacts.StartedSession("vendor-"+t.Name(), shimfacts.DefaultBuildSHA, shimfacts.DefaultModel)
	w, err := sessionwatcher.Start(ctx, d.ws, d.client, sessionwatcher.Session{Started: started, Opening: sessionwatcher.WorkspaceOpened()},
		sessionwatcher.Sinks{
			Feed: resolver, Footer: nop{}, Topbar: nop{}, Sidebar: nop{}, Holds: nop{}, Lifecycle: nop{},
			Commands: nop{}, CacheLapse: nop{}, Clock: clock.System{},
		}, log)
	if err != nil {
		t.Fatalf("start the session watcher: %v", err)
	}
	d.watcher = w
	// THE OPENING THE FAKE SHIM SERVES EVERY FRESH WATCH: the session stream's
	// diagnostics, context usage and re-announced start, and an empty floor
	// page on the main agent's stream.
	d.session = d.client.NextSessionOpen(t)
	for _, u := range []*conversationv1.SessionUpdate{shimfacts.HealthyDiagnostics(), shimfacts.DefaultContextUsage()} {
		d.session.Send(t, &shimv1.WatchSessionResponse{Frame: &shimv1.WatchSessionResponse_Update{Update: u}})
	}
	d.session.Send(t, &shimv1.WatchSessionResponse{Frame: &shimv1.WatchSessionResponse_SessionStarted{SessionStarted: started}})
	d.openAgent(t, shimfacts.MainAgentID)
	return d
}

// openAgent takes the watcher's open of AGENT's stream and serves it the
// fresh book's empty floor page.
func (d *inProcess) openAgent(t *testing.T, agent string) {
	t.Helper()
	open := d.client.NextAgentOpenFor(t, agent)
	open.Stream.Send(t, &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Page{Page: shimfacts.EmptyFloorPage()}})
	d.agents[agent] = open.Stream
}

// OpenTurn implements Driver: the turn the prompt queue would open, handed to
// the watcher as the shim's StartTurn answers it.
func (d *inProcess) OpenTurn(t *testing.T, prompt, key string) string {
	t.Helper()
	d.turns++
	turn := "turn-" + key
	d.watcher.OnTurnOpened(d.ws, &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: turn},
		Agent: &conversationv1.AgentId{Value: shimfacts.MainAgentID},
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: prompt}}},
		}}},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	}, nil)
	return turn
}

// MainAgent implements Driver.
func (d *inProcess) MainAgent() string { return shimfacts.MainAgentID }

// Workspace implements Driver.
func (d *inProcess) Workspace() string { return string(d.ws) }

// Records implements Driver.
func (d *inProcess) Records(t *testing.T) []Record {
	t.Helper()
	var out []Record
	for _, r := range d.log.Records() {
		out = append(out, Record{Operation: r.Operation, Context: r.Context})
	}
	return out
}

// AwaitRecords implements Driver.
func (d *inProcess) AwaitRecords(t *testing.T, what string, match func(Record) bool, n int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), inProcessBound)
	defer cancel()
	q := dlog.RecordQuery{Match: func(r dlog.Record) bool { return match(Record{Operation: r.Operation, Context: r.Context}) }}
	if _, err := d.log.AwaitCount(ctx, q, n); err != nil {
		t.Fatalf("waiting for %d %s: %v", n, what, err)
	}
}

// stream answers AGENT's watch stream, taking the watcher's open of it first
// when the agent is new (a subagent the watcher opened a watch for).
func (d *inProcess) stream(t *testing.T, agent string) *watchtest.Stream[*shimv1.WatchAgentResponse] {
	t.Helper()
	if s, ok := d.agents[agent]; ok {
		return s
	}
	d.openAgent(t, agent)
	return d.agents[agent]
}

// entry is the store's served entry E at STAMP's pointer, turn and place.
func entry(stamp Stamp, e *conversationv1.HistoryEntry) *shimv1.WatchAgentResponse {
	return &shimv1.WatchAgentResponse{Frame: &shimv1.WatchAgentResponse_Entry{
		Entry: shimfacts.ServedEntry(stamp.Pointer, stamp.Turn, stamp.PlaceMs, e),
	}}
}

// PushPrompt implements Driver.
func (d *inProcess) PushPrompt(t *testing.T, agent string, stamp Stamp, prompt *conversationv1.AgentPrompt) {
	t.Helper()
	d.stream(t, agent).Send(t, entry(stamp, &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: prompt}}))
}

// PushFrame implements Driver.
func (d *inProcess) PushFrame(t *testing.T, agent string, stamp Stamp, frame *conversationv1.AgentFrame) {
	t.Helper()
	d.stream(t, agent).Send(t, entry(stamp, &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame}}))
}

// PushSessionUpdate implements Driver.
func (d *inProcess) PushSessionUpdate(t *testing.T, u *conversationv1.SessionUpdate) {
	t.Helper()
	d.session.Send(t, &shimv1.WatchSessionResponse{Frame: &shimv1.WatchSessionResponse_Update{Update: u}})
}

// AwaitRow implements Driver: on the root feed's page, then on its tail.
func (d *inProcess) AwaitRow(t *testing.T, what string, match func(*frontendv1.FeedRow) bool) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), inProcessBound)
	defer cancel()
	page, token, _, err := d.feed.OpenPage(ctx, d.ws, feedid.Feed{Root: true})
	if err != nil {
		t.Fatalf("open the root feed while waiting for %s: %v", what, err)
	}
	for _, r := range page.GetSuccess().GetRows() {
		if match(r) {
			return
		}
	}
	tail, err := d.feed.Tail(ctx, d.ws, feedid.Feed{Root: true}, token)
	if err != nil {
		t.Fatalf("tail the root feed while waiting for %s: %v", what, err)
	}
	pubs, ack := tail.Publications(ctx)
	for {
		select {
		case <-ctx.Done():
			t.Fatalf("waiting for %s: %v", what, ctx.Err())
		case p, ok := <-pubs:
			if !ok {
				t.Fatalf("the root feed's tail closed while waiting for %s", what)
			}
			ack()
			if p.Row != nil && match(p.Row) {
				return
			}
		}
	}
}

// RootRows implements Driver.
func (d *inProcess) RootRows(t *testing.T) []*frontendv1.FeedRow {
	t.Helper()
	page, _, _, err := d.feed.OpenPage(context.Background(), d.ws, feedid.Feed{Root: true})
	if err != nil {
		t.Fatalf("open the root feed: %v", err)
	}
	return page.GetSuccess().GetRows()
}

// warnings answers the operation of every warning or error the drawing
// recorded.
func (d *inProcess) warnings() []string {
	var out []string
	for _, r := range d.log.Records() {
		if r.Level == "warn" || r.Level == "error" {
			out = append(out, r.Operation)
		}
	}
	return out
}
