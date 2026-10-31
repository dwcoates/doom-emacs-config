package prwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

// GITHUB IS NEVER CALLED: fakeGH answers each path from a script, and the
// state store is a real one in memory, so every compare-and-set is the
// store's own.

var (
	t0   = time.Date(2026, 10, 30, 12, 0, 0, 0, time.UTC)
	prK  = wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 7}
	base = "repos/o/r"
)

// fakeGH answers a GET from gets[path] (a body, or a failure), answering 304
// to a read whose entity tag matches the body's, and records every call.
type fakeGH struct {
	mu    sync.Mutex
	gets  map[string]any
	posts map[string]any
	calls []string
	sent  []map[string]string
}

func newGH() *fakeGH { return &fakeGH{gets: map[string]any{}, posts: map[string]any{}} }

func etagOf(body string) string { return fmt.Sprintf(`W/"%d"`, len(body)) + digest(body) }

func (g *fakeGH) Get(_ context.Context, path, etag string) (ghclient.Response, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "GET "+path)
	answer, ok := g.gets[path]
	if !ok {
		answer = "[]"
		if strings.Contains(path, "/check-runs") {
			answer = `{"check_runs":[]}`
		}
	}
	switch a := answer.(type) {
	case error:
		return ghclient.Response{}, a
	case string:
		tag := etagOf(a)
		header := http.Header{"Etag": []string{tag}}
		if etag == tag {
			return ghclient.Response{Status: 304, Header: header}, nil
		}
		return ghclient.Response{Status: 200, Header: header, Body: []byte(a)}, nil
	}
	panic(fmt.Sprintf("fakeGH: answer %T", answer))
}

func (g *fakeGH) Post(_ context.Context, path string, fields map[string]string) (ghclient.Response, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "POST "+path)
	g.sent = append(g.sent, fields)
	switch a := g.posts[path].(type) {
	case error:
		return ghclient.Response{}, a
	case string:
		return ghclient.Response{Status: 201, Body: []byte(a)}, nil
	}
	panic("fakeGH: no scripted post for " + path)
}

func (g *fakeGH) set(path string, answer any) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.gets[path] = answer
}

// pullJSON is the pull request itself.
func pullJSON(state string, draft bool, merged bool, head string) string {
	m := map[string]any{"title": "Fix the thing", "html_url": URLOf(prK), "state": state, "draft": draft, "head": map[string]string{"sha": head}}
	if merged {
		m["merged_at"] = "2026-10-30T13:00:00Z"
		m["merge_commit_sha"] = "mergesha00001111"
		m["merged_by"] = map[string]string{"login": "dodge"}
	}
	return mustJSON(m)
}

// commentJSONOf is a conversation or line comment.
func commentJSONOf(id uint64, login, body string, created, updated time.Time, extra map[string]any) map[string]any {
	m := map[string]any{"id": id, "user": map[string]string{"login": login}, "author_association": "MEMBER", "body": body,
		"html_url": fmt.Sprintf("%s#c%d", URLOf(prK), id), "created_at": created, "updated_at": updated}
	for k, v := range extra {
		m[k] = v
	}
	return m
}

func reviewJSONOf(id uint64, login, state, body string, at time.Time) map[string]any {
	return map[string]any{"id": id, "user": map[string]string{"login": login}, "author_association": "COLLABORATOR", "state": state, "body": body,
		"html_url": fmt.Sprintf("%s#r%d", URLOf(prK), id), "submitted_at": at}
}

func mustJSON(v any) string {
	b, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	return string(b)
}

// paths are the reads one poll of prK makes.
func pathPull() string { return fmt.Sprintf("%s/pulls/%d", base, prK.Number) }
func pathComments() string {
	return fmt.Sprintf("%s/issues/%d/comments?per_page=100&page=1", base, prK.Number)
}
func pathLines() string {
	return fmt.Sprintf("%s/pulls/%d/comments?per_page=100&page=1", base, prK.Number)
}
func pathReviews() string {
	return fmt.Sprintf("%s/pulls/%d/reviews?per_page=100&page=1", base, prK.Number)
}
func pathChecks(head string) string {
	return fmt.Sprintf("%s/commits/%s/check-runs?per_page=100&page=1", base, head)
}

// fakePrompts records each submission.
type fakePrompts struct {
	mu   sync.Mutex
	got  []submitted
	fail error
}

type submitted struct {
	ws     ids.WorkspaceID
	text   string
	origin conversationv1.PromptOrigin
}

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, _ wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.fail != nil {
		return prompthandler.Outcome{}, p.fail
	}
	p.got = append(p.got, submitted{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), origin: origin})
	return prompthandler.Outcome{Turn: ids.TurnID(fmt.Sprintf("turn-%d", len(p.got)))}, nil
}

// fakeHolds answers every Supersede with retired.
type fakeHolds struct{ retired bool }

func (h *fakeHolds) Supersede(context.Context, ids.WorkspaceID, ids.TurnID) (bool, error) {
	return h.retired, nil
}

// rig is one watcher over a real store, a scripted GitHub, and a script clock.
type rig struct {
	w       *Watcher
	db      wsm.DB
	gh      *fakeGH
	prompts *fakePrompts
	holds   *fakeHolds
	clock   *clocktest.Scripted
	log     *dlog.TestLogger
	ws      ids.WorkspaceID
	serves  bool
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
	r := &rig{db: db, gh: newGH(), prompts: &fakePrompts{}, holds: &fakeHolds{}, clock: clocktest.New(t0), log: dlog.NewTestLogger(), ws: record.ID, serves: true}
	r.w, err = New(Deps{Store: db, GH: r.gh, Holds: r.holds, Briefs: briefs, MintKey: func(p string) (string, error) { return p + "-key", nil },
		Serves: func() bool { return r.serves }, Clock: r.clock, Every: DefaultEvery, Log: r.log})
	if err != nil {
		t.Fatal(err)
	}
	r.gh.set(pathPull(), pullJSON("open", false, false, "head1"))
	return r
}

// briefs splices the notice's placeholders in order, so a test reads them.
func briefs(name string, values map[string]string) (string, error) {
	return name + "|" + values["label"] + "|" + values["activity"] + "|" + values["status"], nil
}

// subscribe anchors and records a subscription of the rig's workspace to prK.
func (r *rig) subscribe(t *testing.T, events ...wsm.PullRequestEvent) {
	t.Helper()
	sub, _, err := r.w.Anchor(context.Background(), r.ws, prK, wsm.NewPullRequestEvents(events...), wsm.PullRequestOriginRequested)
	if err != nil {
		t.Fatalf("Anchor: %v", err)
	}
	if _, err := r.db.SubscribePullRequest(context.Background(), sub); err != nil {
		t.Fatalf("SubscribePullRequest: %v", err)
	}
}

func (r *rig) tick() { r.w.Tick(context.Background(), r.prompts) }

// told answers every notice text submitted so far.
func (r *rig) told() []string {
	r.prompts.mu.Lock()
	defer r.prompts.mu.Unlock()
	var out []string
	for _, s := range r.prompts.got {
		out = append(out, s.text)
	}
	return out
}

func contains(s, sub string) bool { return strings.Contains(s, sub) }
