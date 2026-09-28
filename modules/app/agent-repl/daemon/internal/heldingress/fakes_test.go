package heldingress

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

// fixedNow is the instant retries are judged against.
var fixedNow = time.Date(2026, 9, 28, 12, 0, 0, 0, time.UTC)

// submission is one recorded Submit call.
type submission struct {
	WS     ids.WorkspaceID
	Key    string
	Text   string
	Origin conversationv1.PromptOrigin
}

// fakeHandler is the prompt handler. Its accepted set stands for the durable
// idempotency claim: a key it already accepted answers ErrDuplicateSubmission,
// exactly as the real handler does, and the set may be shared by two
// "processes" to stand for the state that outlives a restart.
type fakeHandler struct {
	prompthandler.Handler

	calls      []submission
	accepted   map[string]bool
	deliveries map[string]int
	// refuse answers a Submit of the key with the error, without accepting it.
	refuse map[string]error
}

func newFakeHandler() *fakeHandler {
	return &fakeHandler{accepted: map[string]bool{}, deliveries: map[string]int{}, refuse: map[string]error{}}
}

func (h *fakeHandler) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *feedid.Ref) (prompthandler.Outcome, error) {
	h.calls = append(h.calls, submission{WS: ws, Key: key, Text: said.GetContent().GetBlocks()[0].GetText().GetText(), Origin: origin})
	if err, ok := h.refuse[key]; ok {
		return prompthandler.Outcome{}, err
	}
	if h.accepted[key] {
		return prompthandler.Outcome{}, prompthandler.ErrDuplicateSubmission
	}
	h.accepted[key] = true
	h.deliveries[key]++
	held := wsm.HoldSessionStarting
	return prompthandler.Outcome{Turn: ids.TurnID("turn-" + key), Disposition: promptqueue.Disposition{Held: &held}}, nil
}

// delivered counts the submissions of key the handler accepted, which is how
// many times that prompt would reach the shim.
func (h *fakeHandler) delivered(key string) int { return h.deliveries[key] }

// world is one test's ingress directory, workspace registry and collaborators.
type world struct {
	t         *testing.T
	dir       string
	handler   *fakeHandler
	log       *dlog.TestSurfaces
	byDir     map[string]wsm.Workspace
	published []ids.WorkspaceID
	// publishedSaw records, at each publish, whether the entry files it
	// followed were already gone.
	publishedSaw []int
	now          time.Time
	remove       func(string) error
}

func newWorld(t *testing.T) *world {
	t.Helper()
	return &world{
		t:       t,
		dir:     t.TempDir(),
		handler: newFakeHandler(),
		log:     dlog.NewTestSurfaces(),
		byDir: map[string]wsm.Workspace{
			"/work/one": {ID: "ws-one", Dir: "/work/one"},
			"/work/two": {ID: "ws-two", Dir: "/work/two"},
		},
		now: fixedNow,
	}
}

// ingress builds a fresh ingress over the world, as a (re)started daemon
// would.
func (w *world) ingress() Ingress {
	w.t.Helper()
	in, err := New(Deps{
		Dir:            w.dir,
		WorkspaceByDir: w.ingressLookup(),
		Prompts:        w.handler,
		PublishHost: func(ws ids.WorkspaceID) {
			w.published = append(w.published, ws)
			w.publishedSaw = append(w.publishedSaw, len(w.entries()))
		},
		Log:    w.log,
		Remove: w.remove,
		Now:    func() time.Time { return w.now },
	})
	if err != nil {
		w.t.Fatalf("New = %v", err)
	}
	return in
}

// ingressLookup resolves a directory against the world's registry.
func (w *world) ingressLookup() WorkspaceByDirFunc {
	return func(_ context.Context, dir string) (wsm.Workspace, error) {
		ws, ok := w.byDir[dir]
		if !ok {
			return wsm.Workspace{}, wsm.ErrNotFound
		}
		return ws, nil
	}
}

// write drops one entry file under its contracted name, through a temp name
// and a rename as every producer does.
func (w *world) write(name, dir, key, text string) string {
	w.t.Helper()
	said, err := json.Marshal(map[string]any{
		"content": map[string]any{"blocks": []any{map[string]any{"text": map[string]any{"text": text}}}},
	})
	if err != nil {
		w.t.Fatal(err)
	}
	body, err := json.Marshal(Entry{
		Version: FormatVersion, ProjectDir: dir, IdempotencyKey: key,
		Origin: "PROMPT_ORIGIN_USER_SENT", Said: said, QueuedAt: "2026-09-28T11:59:00Z",
	})
	if err != nil {
		w.t.Fatal(err)
	}
	return w.writeRaw(name, string(body))
}

// writeRaw drops a file with exactly these bytes.
func (w *world) writeRaw(name, body string) string {
	w.t.Helper()
	path := filepath.Join(w.dir, name)
	tmp := filepath.Join(w.dir, "."+name+".tmp")
	if err := os.WriteFile(tmp, []byte(body), 0o644); err != nil {
		w.t.Fatal(err)
	}
	if err := os.Rename(tmp, path); err != nil {
		w.t.Fatal(err)
	}
	return path
}

// entries lists the entry files still in the ingress.
func (w *world) entries() []string {
	w.t.Helper()
	matches, err := filepath.Glob(filepath.Join(w.dir, Glob))
	if err != nil {
		w.t.Fatal(err)
	}
	return matches
}

// sweep runs one sweep and fails the test on an error.
func (w *world) sweep(in Ingress) {
	w.t.Helper()
	if err := in.Sweep(context.Background()); err != nil {
		w.t.Fatalf("Sweep = %v", err)
	}
}

// keys lists the keys the handler was asked to submit, in order.
func (w *world) keys() []string {
	out := make([]string, 0, len(w.handler.calls))
	for _, c := range w.handler.calls {
		out = append(out, c.Key)
	}
	return out
}

// records lists the captured records of one operation at one level.
func (w *world) records(level, operation string) []dlog.Record {
	var out []dlog.Record
	for _, r := range w.log.Records() {
		if r.Level == level && r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}
