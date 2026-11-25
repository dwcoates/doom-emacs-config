package slackwatch

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/briefprompt/briefprompttest"
	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/gnsclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

// SLACK IS NEVER CALLED: every test runs a scripted gns over the real
// in-memory store.

// t0 is the instant every rig starts at.
var t0 = time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)

// threadURL links the thread every test follows, and threadKey is its key.
const threadURL = "https://chesscom.slack.com/archives/C0439CR3S3A/p1784225803782699"

var threadKey = wsm.SlackThreadKey{Channel: "C0439CR3S3A", ThreadTS: "1784225803.782699"}

// gnsFailure is the failure the fake answers when told to fail.
var gnsFailure = &gnsclient.Failure{Call: "gns slack send", Message: "Invalid or expired JWT"}

// fakeGNS is a scripted gns.
type fakeGNS struct {
	mu        sync.Mutex
	username  string
	whoamiErr error
	rootErr   error
	head      int64
	headErr   error
	// noPipe makes PipeHead answer that GNS keeps no such pipe, as it does
	// for a direct message.
	noPipe bool
	// later is what ThreadAfter reads, then laterErr its failure; afters
	// are the cursors it was asked to read after.
	later    []gnsclient.ThreadMessage
	laterErr error
	afters   []string
	channels []string
	// stream is what Stream reads, then streamErr its end.
	stream    []gnsclient.PipeMessage
	streamEnd int64
	streamErr error
	thread    []gnsclient.ThreadMessage
	posts     []string
	postErr   error
	pages     []gnsclient.SearchPage
	// searchErr fails every search; queries are the searches asked for.
	searchErr error
	queries   []string
	reacted   []gnsclient.ReactedMessage
}

func (g *fakeGNS) Whoami(context.Context) (string, error) { return g.username, g.whoamiErr }
func (g *fakeGNS) SubscribeChannel(_ context.Context, channel string) error {
	g.channels = append(g.channels, channel)
	return nil
}

// ThreadRoot answers every message as its own thread's root.
func (g *fakeGNS) ThreadRoot(_ context.Context, _, ts string) (string, error) {
	return ts, g.rootErr
}
func (g *fakeGNS) PipeHead(context.Context, string) (int64, bool, error) {
	return g.head, !g.noPipe, g.headErr
}
func (g *fakeGNS) ThreadAfter(_ context.Context, _, _, after string) ([]gnsclient.ThreadMessage, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.afters = append(g.afters, after)
	return g.later, g.laterErr
}
func (g *fakeGNS) Stream(_ context.Context, _ string, after int64, _ string, each func(gnsclient.PipeMessage)) (int64, error) {
	for _, m := range g.stream {
		each(m)
	}
	return max(after, g.streamEnd), g.streamErr
}
func (g *fakeGNS) Thread(context.Context, string, string, int) ([]gnsclient.ThreadMessage, error) {
	return g.thread, nil
}
func (g *fakeGNS) Post(_ context.Context, _, _ string, blocks []byte) (string, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.postErr != nil {
		return "", g.postErr
	}
	g.posts = append(g.posts, string(blocks))
	return fmt.Sprintf("9.%d", len(g.posts)), nil
}
func (g *fakeGNS) Search(_ context.Context, query string, page int) (gnsclient.SearchPage, error) {
	g.queries = append(g.queries, query)
	if g.searchErr != nil {
		return gnsclient.SearchPage{}, g.searchErr
	}
	if page > len(g.pages) {
		return gnsclient.SearchPage{}, nil
	}
	return g.pages[page-1], nil
}
func (g *fakeGNS) ListReacted(context.Context, string) ([]gnsclient.ReactedMessage, error) {
	return g.reacted, nil
}

// ListMyChannels is never the watcher's: the channel list is slackchannels'.
func (g *fakeGNS) ListMyChannels(context.Context) ([]gnsclient.Channel, error) {
	panic("slackwatch listed the user's channels")
}

// submitted is one prompt the fake prompt handler took.
type submitted struct {
	ws       ids.WorkspaceID
	text     string
	origin   conversationv1.PromptOrigin
	delivery wsm.Delivery
}

type fakePrompts struct {
	got  []submitted
	fail error
}

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	if p.fail != nil {
		return prompthandler.Outcome{}, p.fail
	}
	p.got = append(p.got, submitted{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), origin: origin, delivery: delivery})
	return prompthandler.Outcome{Turn: ids.TurnID(fmt.Sprintf("turn-%d", len(p.got)))}, nil
}

// fakeHolds answers every Supersede with retired.
type fakeHolds struct{ retired bool }

func (h *fakeHolds) Supersede(context.Context, ids.WorkspaceID, ids.TurnID) (bool, error) {
	return h.retired, nil
}

// fakeIntakes records every intake asked for.
type fakeIntakes struct {
	got  []IntakeRequest
	fail error
}

func (f *fakeIntakes) CreateSlackIntake(_ context.Context, req IntakeRequest) error {
	if f.fail != nil {
		return f.fail
	}
	f.got = append(f.got, req)
	return nil
}

// fakeLinks records every workspace whose links were republished.
type fakeLinks struct {
	got  []ids.WorkspaceID
	fail error
}

func (f *fakeLinks) RepublishLinks(_ context.Context, ws ids.WorkspaceID) error {
	if f.fail != nil {
		return f.fail
	}
	f.got = append(f.got, ws)
	return nil
}

// briefs splices a brief as `name|key=value|...`, keys sorted, so a test
// reads what was composed.
func briefs(name string, values map[string]string) (string, error) {
	keys := make([]string, 0, len(values))
	for k := range values {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	parts := []string{name}
	for _, k := range keys {
		parts = append(parts, k+"="+values[k])
	}
	return strings.Join(parts, "|"), nil
}

// rig is one watcher over a real store, a scripted gns and a scripted clock.
type rig struct {
	w       *Watcher
	db      wsm.DB
	gns     *fakeGNS
	prompts *fakePrompts
	holds   *fakeHolds
	intakes *fakeIntakes
	links   *fakeLinks
	notices *briefprompttest.Meter
	clock   *clocktest.Scripted
	log     *dlog.TestLogger
	ws      Workspace
	repo    string
	serves  bool
	nonces  int
}

func newRig(t *testing.T) *rig {
	t.Helper()
	db, err := wsm.OpenInMemory(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = db.Close() })
	// A path that names no temporary folder: the store refuses those, and
	// nothing here touches the disk.
	dir := "/repos/follower"
	record, _, err := db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{Name: "follower", Branch: "feature", ParentBranch: "master", RepoDir: dir})
	if err != nil {
		t.Fatal(err)
	}
	r := &rig{db: db, gns: &fakeGNS{username: "dodge", head: 40}, prompts: &fakePrompts{}, holds: &fakeHolds{}, intakes: &fakeIntakes{}, links: &fakeLinks{}, notices: &briefprompttest.Meter{},
		clock: clocktest.New(t0), log: dlog.NewTestLogger(), ws: Workspace{ID: record.ID, Name: "follower"}, repo: string(record.Repo), serves: true}
	r.w, err = New(Deps{Store: db, GNS: r.gns, Holds: r.holds, Briefs: briefs, MintKey: func(p string) (string, error) { return p + "-key", nil },
		NewNonce: func() (string, error) { r.nonces++; return fmt.Sprintf("n%07d", r.nonces), nil },
		Serves:   func() bool { return r.serves }, Intakes: r.intakes, Links: r.links, Notices: r.notices, Clock: r.clock, Log: r.log})
	if err != nil {
		t.Fatal(err)
	}
	return r
}

// subscribe subscribes the rig's workspace to the thread in MODE.
func (r *rig) subscribe(t *testing.T, mode wsm.SlackThreadMode) wsm.SlackThreadSubscription {
	t.Helper()
	sub, _, err := r.w.Subscribe(context.Background(), r.ws, threadURL, mode, wsm.SlackThreadOriginRequested)
	if err != nil {
		t.Fatalf("Subscribe: %v", err)
	}
	return sub
}

// sub reads the rig's subscription to threadKey back.
func (r *rig) sub(t *testing.T) wsm.SlackThreadSubscription {
	t.Helper()
	return r.subOf(t, threadKey)
}

// dmURL links a thread in a direct message, and dmKey is its key: GNS keeps
// no echo pipe for its channel.
const dmURL = "https://chesscom.slack.com/archives/D01ABTZFLBA/p1795570533281099"

var dmKey = wsm.SlackThreadKey{Channel: "D01ABTZFLBA", ThreadTS: "1795570533.281099"}

// subscribeDM subscribes the rig's workspace to the direct message's thread
// in MODE, which it follows by polling.
func (r *rig) subscribeDM(t *testing.T, mode wsm.SlackThreadMode) wsm.SlackThreadSubscription {
	t.Helper()
	r.gns.noPipe = true
	sub, _, err := r.w.Subscribe(context.Background(), r.ws, dmURL, mode, wsm.SlackThreadOriginRequested)
	if err != nil {
		t.Fatalf("Subscribe: %v", err)
	}
	return sub
}

// reply is a reply in the direct message's thread at TS saying TEXT.
func reply(ts, text string) gnsclient.ThreadMessage {
	return gnsclient.ThreadMessage{TS: ts, ThreadTS: dmKey.ThreadTS, Author: "alice", Text: text}
}

// subOf reads the rig's subscription to THREAD back.
func (r *rig) subOf(t *testing.T, thread wsm.SlackThreadKey) wsm.SlackThreadSubscription {
	t.Helper()
	sub, ok, err := r.w.lookup(context.Background(), subKey{ws: r.ws.ID, thread: thread})
	if err != nil || !ok {
		t.Fatalf("lookup = %v, %v", ok, err)
	}
	return sub
}

// pipe is a pipe message at SEQ saying TEXT.
func pipe(seq int64, ts, text string) gnsclient.PipeMessage {
	return gnsclient.PipeMessage{Seq: seq, TS: ts, ThreadTS: threadKey.ThreadTS, Author: "alice", Text: text}
}

// record finds the one record at LEVEL with OP and MESSAGE.
func (r *rig) record(t *testing.T, level, op, message string) dlog.Record {
	t.Helper()
	rec, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: level, Operation: op, Message: message})
	if !ok {
		t.Fatalf("no %s record %q under %s: %+v", level, message, op, r.log.Records())
	}
	return rec
}

var errBoom = errors.New("boom")

// intakeText composes REQ's first prompt as the creator would, through the
// rig's briefs (workspace.CreateIntake).
func (r *rig) intakeText(t *testing.T, req IntakeRequest) string {
	t.Helper()
	if req.Brief != BriefIntake {
		t.Fatalf("intake brief = %q, want %q", req.Brief, BriefIntake)
	}
	text, err := r.w.deps.Briefs(req.Brief, req.Values)
	if err != nil {
		t.Fatal(err)
	}
	return text
}
