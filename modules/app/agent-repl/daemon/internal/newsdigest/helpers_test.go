package newsdigest

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/wsm"
)

// repoPromptsDir is the checked-in prompts directory: the brief the
// condensing call is composed from is asserted against the real one.
const repoPromptsDir = "../../../prompts"

// now is the fixed instant the tests run at.
var now = time.Date(2026, 10, 2, 12, 0, 0, 0, time.UTC)

// errScripted is a failure a fake was scripted to answer.
var errScripted = errors.New("scripted failure")

// fakeClock is a clock whose waits the test fires by hand: every After is
// announced on asked, and answers when the test sends on fire.
type fakeClock struct {
	mu    sync.Mutex
	at    time.Time
	asked chan time.Duration
	fire  chan time.Time
}

func newFakeClock() *fakeClock {
	return &fakeClock{at: now, asked: make(chan time.Duration, 16), fire: make(chan time.Time)}
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.at
}

func (c *fakeClock) set(t time.Time) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.at = t
}

func (c *fakeClock) After(d time.Duration) <-chan time.Time {
	c.asked <- d
	return c.fire
}

// fakeStore is the digest's store in memory, with scripted failures.
type fakeStore struct {
	mu         sync.Mutex
	state      wsm.NewsDigestState
	runs       []wsm.NewsDigestRun
	stateErr   error
	recordErr  error
	dismissErr error
	restandErr error
	// onRecord runs inside RecordNewsDigestRun, before it records.
	onRecord func()
	// reads counts NewsDigestState calls; afterRead runs after each one with
	// the count, under the store's lock.
	reads     int
	afterRead func(s *fakeStore, n int)
}

func newFakeStore() *fakeStore {
	return &fakeStore{state: wsm.NewsDigestState{Snapshots: map[string]string{}}}
}

func (s *fakeStore) NewsDigestState(context.Context) (wsm.NewsDigestState, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.stateErr != nil {
		return wsm.NewsDigestState{}, s.stateErr
	}
	s.reads++
	if s.afterRead != nil {
		defer s.afterRead(s, s.reads)
	}
	out := s.state
	out.Snapshots = map[string]string{}
	for k, v := range s.state.Snapshots {
		out.Snapshots[k] = v
	}
	return out, nil
}

func (s *fakeStore) RecordNewsDigestRun(_ context.Context, run wsm.NewsDigestRun) error {
	if s.onRecord != nil {
		s.onRecord()
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.recordErr != nil {
		return s.recordErr
	}
	s.runs = append(s.runs, run)
	s.state.LastRunEnd = run.EndedAt
	if run.Recorded {
		s.state.Baseline = run.EndedAt
		for k, v := range run.Snapshots {
			s.state.Snapshots[k] = v
		}
	}
	if run.Digest != nil {
		s.state.LatestID = run.Digest.ID
		s.state.Standing = run.Digest.Overlay
		s.state.LatestOverlay = run.Digest.Overlay
		s.state.LatestMadeAt = run.EndedAt
	}
	return nil
}

func (s *fakeStore) DismissNewsDigest(_ context.Context, id string) (bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.dismissErr != nil {
		return false, s.dismissErr
	}
	if id != s.state.LatestID || id == "" {
		return false, nil
	}
	s.state.Standing = nil
	return true, nil
}

func (s *fakeStore) RestandNewsDigest(_ context.Context, id string) (bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.restandErr != nil {
		return false, s.restandErr
	}
	if id != s.state.LatestID || id == "" || s.state.Standing != nil || s.state.LatestOverlay == nil {
		return false, nil
	}
	s.state.Standing = s.state.LatestOverlay
	return true, nil
}

func (s *fakeStore) recorded() []wsm.NewsDigestRun {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]wsm.NewsDigestRun(nil), s.runs...)
}

// fakeFetcher answers each URL's scripted body or failure.
type fakeFetcher struct {
	mu     sync.Mutex
	bodies map[string]string
	errs   map[string]error
	// block, when set, holds every fetch until it is closed.
	block chan struct{}
	// entered is told of every fetch that has begun.
	entered chan string
}

func newFakeFetcher() *fakeFetcher {
	return &fakeFetcher{bodies: map[string]string{}, errs: map[string]error{}}
}

func (f *fakeFetcher) Fetch(ctx context.Context, url string) ([]byte, error) {
	if f.entered != nil {
		f.entered <- url
	}
	if f.block != nil {
		select {
		case <-f.block:
		case <-ctx.Done():
			return nil, ctx.Err()
		}
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.errs[url]; err != nil {
		return nil, err
	}
	body, ok := f.bodies[url]
	if !ok {
		return nil, fmt.Errorf("no fixture for %s", url)
	}
	return []byte(body), nil
}

// fakeRunner answers the condensing call from a script.
type fakeRunner struct {
	mu       sync.Mutex
	text     string
	err      error
	requests []headless.Request
	// onRun runs inside Run, before it answers.
	onRun func(ctx context.Context)
}

func (r *fakeRunner) Run(ctx context.Context, req headless.Request) (headless.Response, error) {
	if r.onRun != nil {
		r.onRun(ctx)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	r.requests = append(r.requests, req)
	if r.err != nil {
		return headless.Response{}, r.err
	}
	return headless.Response{Text: r.text, Model: req.Model}, nil
}

func (r *fakeRunner) Bin() string { return "fake-claude" }

func (r *fakeRunner) asked() []headless.Request {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]headless.Request(nil), r.requests...)
}

// The fixture sources: one feed and one page.
var (
	feedSource = Source{Key: "feed", Name: "SDK releases", URL: "https://fixture.test/feed.atom",
		Home: "https://fixture.test/releases", Format: FormatAtom}
	pageSource = Source{Key: "page", Name: "News page", URL: "https://fixture.test/news",
		Home: "https://fixture.test/news", Format: FormatPage}
)

// atomWith is an Atom feed holding one entry per id, each updated at the
// given instant and linking to /releases/<id>.
func atomWith(updated time.Time, ids ...string) string {
	body := `<?xml version="1.0" encoding="UTF-8"?><feed xmlns="http://www.w3.org/2005/Atom">`
	for _, id := range ids {
		body += fmt.Sprintf(`<entry><id>%s</id><title>Release %s</title><updated>%s</updated>`+
			`<link rel="alternate" href="/releases/%s"/><content type="html">&lt;p&gt;Notes for %s&lt;/p&gt;</content></entry>`,
			id, id, updated.Format(time.RFC3339), id, id)
	}
	return body + `</feed>`
}

// world is one digester under test with every fake it was built from.
type world struct {
	t       *testing.T
	clock   *fakeClock
	store   *fakeStore
	fetcher *fakeFetcher
	runner  *fakeRunner
	log     *dlog.TestLogger
	serves  bool
	lock    string
	minted  int
	sources []Source
}

func newWorld(t *testing.T) *world {
	t.Helper()
	return &world{
		t: t, clock: newFakeClock(), store: newFakeStore(), fetcher: newFakeFetcher(),
		runner: &fakeRunner{}, log: dlog.NewTestLogger(), serves: true,
		lock: filepath.Join(t.TempDir(), "news-digest.lock"), sources: []Source{feedSource, pageSource},
	}
}

func (w *world) digester() *Digester {
	w.t.Helper()
	d, err := New(Deps{
		Sources: w.sources, Fetcher: w.fetcher, Headless: w.runner, PromptsDir: repoPromptsDir,
		ConfigDir: "/accounts/default", Store: w.store, Clock: w.clock, LockPath: w.lock,
		Serves: func() bool { return w.serves },
		MintID: func() string { w.minted++; return fmt.Sprintf("digest-%d", w.minted) },
		Every:  DefaultEvery, StartDelay: DefaultStartDelay, Recheck: DefaultRecheck, Log: w.log,
	})
	if err != nil {
		w.t.Fatalf("New: %v", err)
	}
	return d
}

// withNewFeedEntry scripts a world whose feed was read before (seen: old) and
// now carries one new entry, and whose page is unchanged.
func (w *world) withNewFeedEntry() {
	w.store.state.LastRunEnd = now.Add(-25 * time.Hour)
	w.store.state.Baseline = now.Add(-25 * time.Hour)
	w.store.state.Snapshots["feed"] = `["old"]`
	w.store.state.Snapshots["page"] = "Headline"
	w.fetcher.bodies[feedSource.URL] = atomWith(now.Add(-time.Hour), "new", "old")
	w.fetcher.bodies[pageSource.URL] = "<p>Headline</p>"
}

// answerJSON is a valid model answer: one backend item linking the new entry.
const answerJSON = `{"sections":[{"kind":"backend","items":[{"title":"SDK drops subscription billing","summary":"The SDK now needs an API key.","effective":"2026-11-01","links":[{"label":"Release new","url":"https://fixture.test/releases/new"}]}]}]}`

// records answers the records at level under operation.
func records(log *dlog.TestLogger, level, operation string) []dlog.Record {
	var out []dlog.Record
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// latestStanding is the topic's latest value, failing when there is none.
func latestStanding(t *testing.T, d *Digester) *agentreplv1.NewsDigestStanding {
	t.Helper()
	v, ok := d.Topic().Latest()
	if !ok {
		t.Fatal("the standing topic holds no value")
	}
	return v
}

// encodedOverlay is a stored overlay with id and one backend item.
func encodedOverlay(t *testing.T, id string, fromMs int64) []byte {
	t.Helper()
	overlay := &frontendv1.NewsDigestOverlay{
		Id: &frontendv1.NewsDigestId{Value: id},
		Header: &frontendv1.NewsDigestHeader{
			Title:  &frontendv1.NewsDigestTitle{Text: "Claude news · Oct 1"},
			Period: &frontendv1.NewsDigestPeriod{FromMs: fromMs, ToMs: fromMs + 1},
		},
		Sections: []*frontendv1.NewsDigestSection{{
			Heading: &frontendv1.NewsDigestSectionHeading{Text: "Affects the agent-repl backend"},
			Kind:    kinds[0].arm(),
			Items: []*frontendv1.NewsDigestItem{{
				Title:   &frontendv1.NewsDigestItemTitle{Text: "Earlier item"},
				Summary: &frontendv1.NewsDigestItemSummary{Text: "Still unread."},
				Links:   []*frontendv1.NewsDigestLink{{Label: "Earlier", Url: "https://fixture.test/earlier"}},
			}},
		}},
	}
	encoded, err := proto.Marshal(overlay)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	return encoded
}
