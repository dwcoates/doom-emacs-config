package prwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"regexp"
	"strconv"
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
	"agent-repld/internal/merge"
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
	// graphql answers every GraphQL call; nil fails it.
	graphql func(query string, variables map[string]any) (ghclient.GraphQLResponse, error)
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

// GraphQL answers from graphql, which a test scripts; unscripted, no
// GraphQL call is expected and one fails the read.
func (g *fakeGH) GraphQL(ctx context.Context, query string, variables map[string]any) (ghclient.GraphQLResponse, error) {
	g.mu.Lock()
	answer := g.graphql
	g.calls = append(g.calls, "GRAPHQL "+queryName(query))
	g.mu.Unlock()
	if answer == nil {
		return ghclient.GraphQLResponse{}, fmt.Errorf("fakeGH: no scripted GraphQL answer for %s", queryName(query))
	}
	return answer(query, variables)
}

// queryName is the name a query declares (`query PullRequestQueues(...)`).
func queryName(query string) string {
	rest, ok := strings.CutPrefix(strings.TrimSpace(query), "query ")
	if !ok {
		return "anonymous"
	}
	name, _, _ := strings.Cut(rest, "(")
	name, _, _ = strings.Cut(name, " ")
	return strings.TrimSpace(name)
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
	// ownedMerged is every workspace told one of its owned pull requests
	// merged, and ownedMergedErr what the telling answers.
	ownedMerged    []ids.WorkspaceID
	ownedMergedErr error

	w       *Watcher
	db      wsm.DB
	gh      *fakeGH
	prompts *fakePrompts
	holds   *fakeHolds
	clock   *clocktest.Scripted
	log     *dlog.TestLogger
	ws      ids.WorkspaceID
	serves  bool
	deploys *fakeDeployments
}

// fakeDeployments records every whole set of GitHub deployments told.
type fakeDeployments struct {
	told [][]merge.GithubDeployment
}

func (d *fakeDeployments) SetGithubDeployments(_ context.Context, deployments []merge.GithubDeployment) {
	d.told = append(d.told, deployments)
}

// last is the latest set told; it fails the test when none was.
func (d *fakeDeployments) last(t *testing.T) []merge.GithubDeployment {
	t.Helper()
	if len(d.told) == 0 {
		t.Fatal("no GitHub deployments were told")
	}
	return d.told[len(d.told)-1]
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
	r := &rig{db: db, gh: newGH(), prompts: &fakePrompts{}, holds: &fakeHolds{}, clock: clocktest.New(t0), log: dlog.NewTestLogger(), ws: record.ID, serves: true, deploys: &fakeDeployments{}}
	r.w, err = New(Deps{Store: db, GH: r.gh, Holds: r.holds, Briefs: briefs, MintKey: func(p string) (string, error) { return p + "-key", nil },
		Serves: func() bool { return r.serves }, Clock: r.clock, Every: DefaultEvery, Deployments: r.deploys, Log: r.log,
		OwnedMerged: func(_ context.Context, ws ids.WorkspaceID) error {
			r.ownedMerged = append(r.ownedMerged, ws)
			return r.ownedMergedErr
		}})
	if err != nil {
		t.Fatal(err)
	}
	r.gh.set(pathPull(), pullJSON("open", false, false, "head1"))
	// Every subscription asking for every event reads the merge queue, so
	// by default the pull request is in none; a test scripts more with
	// withGQL.
	g := r.withGQL()
	g.setFacts(prK.Number, factsJSON("main", nil))
	return r
}

// briefs splices the notice's placeholders in order, so a test reads them.
func briefs(name string, values map[string]string) (string, error) {
	return name + "|" + values["label"] + "|" + values["activity"] + "|" + values["status"], nil
}

// subscribe anchors and records a subscription of the rig's workspace to prK.
func (r *rig) subscribe(t *testing.T, events ...wsm.PullRequestEvent) {
	t.Helper()
	r.subscribeAs(t, wsm.PullRequestOriginRequested, events...)
}

// subscribeAs subscribes the rig's workspace to the pull request under ORIGIN.
func (r *rig) subscribeAs(t *testing.T, origin wsm.PullRequestOrigin, events ...wsm.PullRequestEvent) {
	t.Helper()
	sub, _, err := r.w.Anchor(context.Background(), r.ws, prK, wsm.NewPullRequestEvents(events...), origin)
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

// gql is a scripted GraphQL GitHub: each query is answered from the facts a
// test sets, by the query's name.
type gql struct {
	mu sync.Mutex
	// prFacts answers PullRequestQueues: each pull request's raw facts. One
	// absent is nulled, with a NOT_FOUND error naming it.
	prFacts map[uint32]string
	// queues answers MergeQueue: each branch's raw `mergeQueue`, "null" for
	// none; the empty branch is the default branch.
	queues map[string]string
	// defaultBranch is the repository's default branch's name.
	defaultBranch string
	// departures answers MergeQueueDepartures, as prFacts does.
	departures map[uint32]string
	// fail fails every query of a name with its error.
	fail map[string]error
}

func newGQL() *gql {
	return &gql{prFacts: map[uint32]string{}, queues: map[string]string{}, defaultBranch: "main", departures: map[uint32]string{}, fail: map[string]error{}}
}

// aliasNumber reads the pull request numbers a batched query asks.
var aliasNumber = regexp.MustCompile(`pr(\d+): pullRequest`)

func (g *gql) answer(query string, vars map[string]any) (ghclient.GraphQLResponse, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	name := queryName(query)
	if err := g.fail[name]; err != nil {
		return ghclient.GraphQLResponse{}, err
	}
	switch name {
	case "PullRequestQueues", "MergeQueueDepartures":
		facts := g.prFacts
		if name == "MergeQueueDepartures" {
			facts = g.departures
		}
		repo := map[string]json.RawMessage{}
		var errs []ghclient.GraphQLError
		for _, m := range aliasNumber.FindAllStringSubmatch(query, -1) {
			n, _ := strconv.ParseUint(m[1], 10, 32)
			raw, ok := facts[uint32(n)]
			if !ok {
				raw = "null"
				errs = append(errs, ghclient.GraphQLError{Type: "NOT_FOUND", Message: "no pull request " + m[1], Path: []string{"repository", "pr" + m[1]}})
			}
			repo["pr"+m[1]] = json.RawMessage(raw)
		}
		return ghclient.GraphQLResponse{Data: json.RawMessage(mustJSON(map[string]any{"repository": repo})), Errors: errs}, nil
	case "MergeQueue":
		branch, _ := vars["branch"].(string)
		raw, ok := g.queues[branch]
		if !ok {
			raw = "null"
		}
		return ghclient.GraphQLResponse{Data: json.RawMessage(`{"repository":{"defaultBranchRef":{"name":"` + g.defaultBranch + `"},"mergeQueue":` + raw + `}}`)}, nil
	}
	panic("gql: no answer for " + name)
}

func (g *gql) setFacts(n uint32, raw string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.prFacts[n] = raw
}

func (g *gql) setQueue(branch, raw string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.queues[branch] = raw
}

func (g *gql) setDeparture(n uint32, raw string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.departures[n] = raw
}

// withGQL scripts the rig's GitHub's GraphQL answers.
func (r *rig) withGQL() *gql {
	g := newGQL()
	r.gh.mu.Lock()
	r.gh.graphql = g.answer
	r.gh.mu.Unlock()
	return g
}

// factsJSON is one pull request's queue facts: its base, its entry (nil for
// none) and its timeline events.
func factsJSON(base string, entry *remoteEntryFacts, events ...remoteQueueEvent) string {
	m := map[string]any{"baseRefName": base, "mergeQueueEntry": nil}
	if entry != nil {
		e := map[string]any{"state": entry.State, "headCommit": nil}
		if entry.GroupHead != "" {
			e["headCommit"] = map[string]string{"oid": entry.GroupHead}
		}
		m["mergeQueueEntry"] = e
	}
	nodes := []map[string]any{}
	for _, ev := range events {
		node := map[string]any{"__typename": "AddedToMergeQueueEvent", "id": ev.ID, "createdAt": ev.At, "actor": nil}
		if ev.Removed {
			node["__typename"], node["reason"] = "RemovedFromMergeQueueEvent", ev.Reason
		}
		if ev.Actor != "" {
			node["actor"] = map[string]string{"login": ev.Actor}
		}
		nodes = append(nodes, node)
	}
	m["timelineItems"] = map[string]any{"nodes": nodes}
	return mustJSON(m)
}

// queueJSON is a one-page `mergeQueue` holding ENTRIES, head first.
func queueJSON(entries ...remoteQueueEntry) string {
	nodes := []map[string]any{}
	for _, e := range entries {
		node := map[string]any{"state": e.State, "headCommit": nil, "pullRequest": map[string]any{"number": e.Number, "title": e.Title, "url": e.URL}}
		if e.GroupHead != "" {
			node["headCommit"] = map[string]string{"oid": e.GroupHead}
		}
		nodes = append(nodes, node)
	}
	return mustJSON(map[string]any{"url": "https://github.com/o/r/queue/main", "entries": map[string]any{"pageInfo": map[string]any{"hasNextPage": false, "endCursor": ""}, "nodes": nodes}})
}

// departureJSON is what became of a departed entry.
func departureJSON(title string, merged bool, sha string, reason *string) string {
	m := map[string]any{"title": title, "merged": merged, "mergeCommit": nil, "timelineItems": map[string]any{"nodes": []any{}}}
	if sha != "" {
		m["mergeCommit"] = map[string]string{"oid": sha}
	}
	if reason != nil {
		m["timelineItems"] = map[string]any{"nodes": []any{map[string]any{"reason": *reason}}}
	}
	return mustJSON(m)
}

// entry is a queue entry of pull request N of the rig's repository.
func entry(n uint32, state string) remoteQueueEntry {
	return remoteQueueEntry{Number: n, Title: fmt.Sprintf("Change %d", n), URL: fmt.Sprintf("https://github.com/o/r/pull/%d", n), State: state}
}

// gqlCalls counts the GraphQL calls of NAME the rig's GitHub took.
func (r *rig) gqlCalls(name string) int {
	r.gh.mu.Lock()
	defer r.gh.mu.Unlock()
	n := 0
	for _, c := range r.gh.calls {
		if c == "GRAPHQL "+name {
			n++
		}
	}
	return n
}
