package heldingress

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

// fixedNow is the instant retries are judged against.
var fixedNow = time.Date(2026, 9, 28, 12, 0, 0, 0, time.UTC)

// submission is one recorded Submit call.
type submission struct {
	WS       ids.WorkspaceID
	Key      string
	Text     string
	Origin   conversationv1.PromptOrigin
	Delivery wsm.Delivery
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

func (h *fakeHandler) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	h.calls = append(h.calls, submission{WS: ws, Key: key, Text: said.GetContent().GetBlocks()[0].GetText().GetText(), Origin: origin, Delivery: delivery})
	if err, ok := h.refuse[key]; ok {
		return prompthandler.Outcome{}, err
	}
	if h.accepted[key] {
		return prompthandler.Outcome{}, prompthandler.ErrDuplicateSubmission
	}
	h.accepted[key] = true
	h.deliveries[key]++
	held := wsm.HoldReconnect
	return prompthandler.Outcome{Turn: ids.TurnID("turn-" + key), Disposition: promptqueue.Disposition{Held: &held}}, nil
}

// delivered counts the submissions of key the handler accepted, which is how
// many times that prompt would reach the shim.
func (h *fakeHandler) delivered(key string) int { return h.deliveries[key] }

// pendingCall is one call the ingress made on the held section.
type pendingCall struct {
	// Op is "set" or "drop".
	Op      string
	WS      ids.WorkspaceID
	Entries []holds.IngressEntry
	Key     string
}

// fakePending is the held section: it records every call in order.
type fakePending struct {
	calls []pendingCall
}

func (f *fakePending) SetIngressPending(ws ids.WorkspaceID, entries []holds.IngressEntry) {
	f.calls = append(f.calls, pendingCall{Op: "set", WS: ws, Entries: entries})
}

func (f *fakePending) ClaimIngress(ids.WorkspaceID, string, ids.TurnID) {
	panic("the ingress never claims: the prompt handler does")
}

func (f *fakePending) DropIngress(ws ids.WorkspaceID, key string) {
	f.calls = append(f.calls, pendingCall{Op: "drop", WS: ws, Key: key})
}

// lastSet is the last list stated for WS, and whether one was stated.
func (f *fakePending) lastSet(ws ids.WorkspaceID) ([]holds.IngressEntry, bool) {
	for i := len(f.calls) - 1; i >= 0; i-- {
		if f.calls[i].Op == "set" && f.calls[i].WS == ws {
			return f.calls[i].Entries, true
		}
	}
	return nil, false
}

// world is one test's ingress directory, workspace registry and collaborators.
type world struct {
	t         *testing.T
	dir       string
	handler   *fakeHandler
	pending   *fakePending
	log       *dlog.TestSurfaces
	byDir     map[string]wsm.Workspace
	published []ids.WorkspaceID
	// publishedSaw records, at each publish, whether the entry files it
	// followed were already gone.
	publishedSaw []int
	now          time.Time
	remove       func(string) error
	// serves is what the ingress's serving answer says.
	serves bool
}

func newWorld(t *testing.T) *world {
	t.Helper()
	return &world{
		t:       t,
		dir:     t.TempDir(),
		handler: newFakeHandler(),
		pending: &fakePending{},
		log:     dlog.NewTestSurfaces(),
		byDir: map[string]wsm.Workspace{
			"/work/one": {ID: "ws-one", Dir: "/work/one"},
			"/work/two": {ID: "ws-two", Dir: "/work/two"},
		},
		now:    fixedNow,
		serves: true,
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
		Pending:        w.pending,
		PublishHost: func(ws ids.WorkspaceID) {
			w.published = append(w.published, ws)
			w.publishedSaw = append(w.publishedSaw, len(w.entries()))
		},
		Serves: func() bool { return w.serves },
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
	return w.writeDelivered(name, dir, key, text, "")
}

// writeDelivered is write with the entry's delivery named ("" for none).
func (w *world) writeDelivered(name, dir, key, text, delivery string) string {
	w.t.Helper()
	said, err := json.Marshal(map[string]any{
		"content": map[string]any{"blocks": []any{map[string]any{"text": map[string]any{"text": text}}}},
	})
	if err != nil {
		w.t.Fatal(err)
	}
	body, err := json.Marshal(Entry{
		Version: FormatVersion, ProjectDir: dir, IdempotencyKey: key,
		Origin: "PROMPT_ORIGIN_USER_SENT", Said: said, QueuedAt: "2026-09-28T11:59:00Z", Delivery: delivery,
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
	return dlog.FilterRecords(w.log.Records(), dlog.RecordQuery{Level: level, Operation: operation})
}
