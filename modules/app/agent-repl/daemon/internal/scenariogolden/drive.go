package scenariogolden

import (
	"encoding/json"
	"strconv"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// Record is one record the daemon side wrote while drawing a scenario: its
// operation and context, which the drive counts routing records by.
type Record struct {
	Operation string
	Context   map[string]any
}

// Stamp is what the fake store stamps an entry with as it serves it: the
// pointer minted at the key's first write, the turn, and the recorded place.
type Stamp struct {
	Pointer string
	Turn    string
	PlaceMs int64
}

// Driver serves one scenario's frames to a daemon side (the real session
// watcher and feed resolver) exactly as the shim serves its store, and reads
// back what that side drew. One drive loop (Drive) runs over every driver, so
// the in-process projection and the end-to-end daemon check cannot drift.
type Driver interface {
	// OpenTurn opens the scenario's turn with the user prompt PROMPT under
	// idempotency KEY, and answers the turn's id.
	OpenTurn(t *testing.T, prompt, key string) string
	// MainAgent is the main agent's id on this driver's shim.
	MainAgent() string
	// Workspace is the id of the workspace whose root feed is drawn.
	Workspace() string
	// Records answers every record the daemon side wrote for this scenario.
	Records(t *testing.T) []Record
	// AwaitRecords waits until at least N records MATCH accepts were written,
	// failing t as its wait primitive does.
	AwaitRecords(t *testing.T, what string, match func(Record) bool, n int)
	// PushPrompt serves a user prompt on AGENT's stream.
	PushPrompt(t *testing.T, agent string, stamp Stamp, prompt *conversationv1.AgentPrompt)
	// PushFrame serves an agent frame on AGENT's stream.
	PushFrame(t *testing.T, agent string, stamp Stamp, frame *conversationv1.AgentFrame)
	// PushSessionUpdate serves a session update on the session stream.
	PushSessionUpdate(t *testing.T, u *conversationv1.SessionUpdate)
	// AwaitRow waits until the root feed holds a row MATCH accepts.
	AwaitRow(t *testing.T, what string, match func(*frontendv1.FeedRow) bool)
	// RootRows answers the root feed's newest page.
	RootRows(t *testing.T) []*frontendv1.FeedRow
}

// entryRouted is the routing record of an entry on WATCH ("" for the main
// agent's watch).
func entryRouted(watch string) func(Record) bool {
	return func(r Record) bool {
		if r.Operation != "daemon.sessionwatcher.entry_routed" {
			return false
		}
		if watch == "" {
			return r.Context["main"] == true
		}
		return r.Context["agent_id"] == watch
	}
}

// updateRouted is the routing record of a session update.
func updateRouted(r Record) bool {
	return r.Operation == "daemon.sessionwatcher.session_update_routed"
}

// count is how many of D's records MATCH accepts.
func count(t *testing.T, d Driver, match func(Record) bool) int {
	t.Helper()
	n := 0
	for _, r := range d.Records(t) {
		if match(r) {
			n++
		}
	}
	return n
}

// Drive serves scenario NAME (its frames.json under DIR) through D and answers
// its normalized root feed.
//
// Every entry and update is awaited on the watcher's own routing records
// (daemon.sessionwatcher.entry_routed, session_update_routed), never a quiet
// window. The file-plane items (residue, shell_run_claim, bash_run) are the
// sidecar's to deliver and are not pushed here; a scenario that needs them is
// a two-plane seam with its e2e wiring test.
func Drive(t *testing.T, d Driver, dir, name string) json.RawMessage {
	t.Helper()
	rows := ReadRows(t, dir, name)
	prompt := "!" + name
	turn := d.OpenTurn(t, prompt, "k-"+name)
	if turn == "" {
		t.Fatalf("opening %s's turn with %q minted no turn", name, prompt)
	}
	main := d.MainAgent()
	// THE DRIVE'S IDENTITIES ARE THIS DRIVER'S: its main agent's book (the
	// book its turn's prompt was written to) and its turn name this side's.
	ids := Identities(t, name, rows, turn, main)
	// Counted BEFORE the first push, so no routing it causes is in the base.
	routed := map[string]int{"": count(t, d, entryRouted(""))}
	updates := count(t, d, updateRouted)

	pointers := map[string]string{}
	terminal := false
	// retried is a main-agent failure agent-repl promised to retry
	// (conversation.v1.AgentFailure.retry_promised): the feed withholds its
	// errored row (resolve/feed/retriedfailure.go), so no row ends the turn.
	retried := false
	for _, r := range rows {
		// THE STORE'S POINTER RULE: minted at a key's first write and kept
		// across every later upsert of the key.
		key := r.Book + "\x00" + r.UpsertKey
		if pointers[key] == "" {
			pointers[key] = "g-" + strconv.Itoa(len(pointers)+1)
		}
		var place int64
		if r.RecordPlace != nil {
			place = r.RecordPlace.AtMs
		}
		switch r.Item.Kind {
		case "prompt":
			p := &conversationv1.AgentPrompt{}
			if err := protojson.Unmarshal(ids.Rename(t, r.Item.Prompt), p); err != nil {
				t.Fatalf("decode the prompt of %s: %v", name, err)
			}
			if place == 0 {
				// THE STORE PLACES AN UNPLACED ROW AT ITS ARRIVAL, which for the
				// turn's prompt is the instant the drive began: the goldens' own
				// clock, never this side's, so every row sorts on one timeline.
				place = ReadNowMs(t, dir, name)
			}
			d.PushPrompt(t, main, Stamp{Pointer: pointers[key], Turn: turn, PlaceMs: place}, p)
			routed[""]++
			d.AwaitRecords(t, "routing records of the delivered prompt", entryRouted(""), routed[""])
		case "frame":
			frame := &conversationv1.AgentFrame{}
			if err := protojson.Unmarshal(ids.Rename(t, r.Item.Frame), frame); err != nil {
				t.Fatalf("decode a frame of %s: %v", name, err)
			}
			agent, watch := r.Book, r.Book
			if agent == ids.mainBook {
				agent, watch = main, ""
				terminal = terminal || frame.GetSuccess() != nil || frame.GetFailure() != nil
				retried = retried || frame.GetFailure().GetRetryPromised() != nil
			}
			if _, open := routed[watch]; !open {
				// A SUBAGENT'S FRAMES ARE PUSHED ONCE ITS WATCH IS OPEN, so
				// each is served live and routed as an entry, never folded
				// into the opening page at whatever moment the watch opens.
				d.AwaitRecords(t, "the watch of "+watch+" to open", func(r Record) bool {
					return r.Operation == "daemon.sessionwatcher.history_page" && r.Context["agent_id"] == watch
				}, 1)
				routed[watch] = count(t, d, entryRouted(watch))
			}
			d.PushFrame(t, agent, Stamp{Pointer: pointers[key], Turn: turn, PlaceMs: place}, frame)
			routed[watch]++
			d.AwaitRecords(t, "routing records of a frame of "+agent, entryRouted(watch), routed[watch])
		case "session_update":
			u := &conversationv1.SessionUpdate{}
			if err := protojson.Unmarshal(ids.Rename(t, r.Item.Update), u); err != nil {
				t.Fatalf("decode a session update of %s: %v", name, err)
			}
			d.PushSessionUpdate(t, u)
			updates++
			d.AwaitRecords(t, "routing records of a session update", updateRouted, updates)
		}
	}
	switch {
	case retried:
		// A RETRIED FAILURE DRAWS NO ROW: the feed's record of withholding it
		// is the turn's end, written by the same drawing that settles its
		// prompt, so the page read after it holds every row the end touches.
		d.AwaitRecords(t, "the scenario turn's withheld failure", func(r Record) bool {
			return r.Operation == "daemon.feed.retried_failure_withheld" && r.Context["turn"] == turn
		}, 1)
	case terminal:
		// THE TURN'S END IS DRAWN ONCE THE LIFECYCLE CLOSES IT, after the
		// watcher's lock is released, so its row is awaited on its own.
		d.AwaitRow(t, "the scenario turn's ended row", func(r *frontendv1.FeedRow) bool {
			return r.GetTurnEnded() != nil && r.GetTurn().GetValue() == turn
		})
	}
	return Normalize(t, d.RootRows(t), turn, d.Workspace(), Instants(t, rows))
}
