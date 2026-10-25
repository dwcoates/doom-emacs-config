package graph

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/zettel/internal/zlog"
)

type fixture struct {
	db   *DB
	path string
	log  *bytes.Buffer
}

func newFixture(t *testing.T) fixture {
	t.Helper()
	path := filepath.Join(t.TempDir(), "zettel.db")
	var buf bytes.Buffer
	return fixture{db: openAt(t, path, &buf), path: path, log: &buf}
}

func openAt(t *testing.T, path string, buf *bytes.Buffer) *DB {
	t.Helper()
	log := zlog.New(buf, buf, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)
	ids := 0
	clock := time.UnixMilli(1_000_000)
	db, err := Open(context.Background(), path, log, Options{
		Now:   func() time.Time { clock = clock.Add(time.Millisecond); return clock },
		NewID: func() string { ids++; return fmt.Sprintf("id%03d", ids) },
	})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	return db
}

func records(buf *bytes.Buffer) []map[string]any {
	var out []map[string]any
	for _, line := range strings.Split(strings.TrimSpace(buf.String()), "\n") {
		if line == "" {
			continue
		}
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err == nil {
			out = append(out, rec)
		}
	}
	return out
}

func ctx() context.Context { return context.Background() }

// answered records one exchange, failing the test on any error.
func (f fixture) answered(t *testing.T, thread ThreadID, turn string, parent Parent, question, answer string) Node {
	t.Helper()
	node, created, err := f.db.RecordExchange(ctx(), thread, turn, question, parent, answer)
	if err != nil || !created {
		t.Fatalf("RecordExchange: created=%v err=%v", created, err)
	}
	return node
}

func askedOf(n Node) Parent { return Parent{Of: &AskedOf{Node: n.ID, Revision: 1}} }

func TestRecordExchangeRefusals(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, f fixture) (ThreadID, Parent)
		want    error
	}{
		{"a second thread start from another turn", func(t *testing.T, f fixture) (ThreadID, Parent) {
			f.answered(t, "t1", "turn1", Parent{}, "q", "a")
			return "t1", Parent{}
		}, ErrThreadAlreadyStarted},
		{"a parent that does not exist", func(t *testing.T, f fixture) (ThreadID, Parent) {
			return "t1", Parent{Of: &AskedOf{Node: "nope", Revision: 1}}
		}, ErrUnknownParent},
		{"a parent in another thread", func(t *testing.T, f fixture) (ThreadID, Parent) {
			other := f.answered(t, "t2", "turn1", Parent{}, "q", "a")
			return "t1", askedOf(other)
		}, ErrUnknownParent},
		{"a revision the parent does not have", func(t *testing.T, f fixture) (ThreadID, Parent) {
			root := f.answered(t, "t1", "turn1", Parent{}, "q", "a")
			return "t1", Parent{Of: &AskedOf{Node: root.ID, Revision: 2}}
		}, ErrUnknownParent},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			thread, parent := tc.arrange(t, f)
			before, _ := f.db.Thread(ctx(), thread)

			// Act
			_, _, err := f.db.RecordExchange(ctx(), thread, "turn-new", "question", parent, "answer")

			// Assert
			if !errors.Is(err, tc.want) {
				t.Fatalf("err = %v, want %v", err, tc.want)
			}
			after, _ := f.db.Thread(ctx(), thread)
			if len(after) != len(before) {
				t.Errorf("a refused record wrote a node: %d before, %d after", len(before), len(after))
			}
			for _, rec := range records(f.log) {
				if rec["level"] == "error" {
					t.Errorf("a refusal was logged as an error: %v", rec)
				}
			}
		})
	}
}

func TestRecordExchangeIsIdempotentByTurn(t *testing.T) {
	// Arrange
	f := newFixture(t)
	first := f.answered(t, "t1", "turn1", Parent{}, "What are quarks?", "Elementary particles.")

	// Act
	again, created, err := f.db.RecordExchange(ctx(), "t1", "turn1", "a different question", Parent{}, "a different answer")

	// Assert
	if err != nil || created {
		t.Fatalf("created=%v err=%v, want the existing node", created, err)
	}
	if again.ID != first.ID || again.Answer.Markdown != "Elementary particles." || again.Question != "What are quarks?" {
		t.Errorf("again = %+v, want the first record unchanged", again)
	}
	nodes, _ := f.db.Thread(ctx(), "t1")
	if len(nodes) != 1 {
		t.Errorf("nodes = %d, want one node for one turn", len(nodes))
	}
}

func TestRecordExchangeStoresTheQuoteAndRevisionOne(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.answered(t, "t1", "turn1", Parent{}, "What are hydrogen ions?", "A proton.")
	context := "it bonds to a water molecule and forms hydronium"
	child := Parent{Of: &AskedOf{Node: root.ID, Revision: 1, Quote: &Quote{Text: "forms hydronium", Context: &context}}}

	// Act
	node := f.answered(t, "t1", "turn2", child, "What is this?", "H3O+.")

	// Assert
	nodes, err := f.db.Thread(ctx(), "t1")
	if err != nil || len(nodes) != 2 || nodes[1].ID != node.ID {
		t.Fatalf("nodes = %v, err = %v, want root then the new node", nodes, err)
	}
	got := nodes[1]
	if got.Answer.Number != 1 || got.Answer.Markdown != "H3O+." || got.Question != "What is this?" || got.Turn != "turn2" {
		t.Errorf("node = %+v, want revision 1 of the given answer for turn2", got)
	}
	if got.Parent.Of == nil || got.Parent.Of.Node != root.ID || got.Parent.Of.Quote == nil ||
		got.Parent.Of.Quote.Text != "forms hydronium" || got.Parent.Of.Quote.Context == nil || *got.Parent.Of.Quote.Context != context {
		t.Errorf("parent = %+v, want the root with the quote and its context", got.Parent.Of)
	}
}

func TestQuoteWithoutContextRoundTrips(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.answered(t, "t1", "turn1", Parent{}, "q", "a")

	// Act
	f.answered(t, "t1", "turn2", Parent{Of: &AskedOf{Node: root.ID, Revision: 1, Quote: &Quote{Text: "a"}}}, "q2", "a2")

	// Assert
	nodes, _ := f.db.Thread(ctx(), "t1")
	if q := nodes[1].Parent.Of.Quote; q == nil || q.Text != "a" || q.Context != nil {
		t.Errorf("quote = %+v, want the text and no context", q)
	}
}

func TestRecordSightings(t *testing.T) {
	t.Run("counts each occurrence", func(t *testing.T) {
		// Arrange
		f := newFixture(t)
		n := f.answered(t, "t1", "turn1", Parent{}, "q", "a")

		// Act
		err := f.db.RecordSightings(ctx(), []NodeID{n.ID, n.ID})

		// Assert
		if err != nil {
			t.Fatal(err)
		}
		nodes, _ := f.db.Thread(ctx(), "t1")
		if nodes[0].Sightings != 2 {
			t.Errorf("sightings = %d, want 2", nodes[0].Sightings)
		}
	})
	t.Run("an unknown node refuses the whole request", func(t *testing.T) {
		// Arrange
		f := newFixture(t)
		n := f.answered(t, "t1", "turn1", Parent{}, "q", "a")

		// Act
		err := f.db.RecordSightings(ctx(), []NodeID{n.ID, "nope"})

		// Assert
		var unknown *UnknownNodeError
		if !errors.As(err, &unknown) || unknown.Node != "nope" {
			t.Fatalf("err = %v, want the unknown node named", err)
		}
		nodes, _ := f.db.Thread(ctx(), "t1")
		if nodes[0].Sightings != 0 {
			t.Errorf("sightings = %d, want the known node's count rolled back", nodes[0].Sightings)
		}
	})
}

func TestReopenKeepsTheGraph(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.answered(t, "t1", "turn1", Parent{}, "q", "a")
	if err := f.db.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	var buf bytes.Buffer
	reopened := openAt(t, f.path, &buf)

	// Assert
	nodes, err := reopened.Thread(ctx(), "t1")
	if err != nil || len(nodes) != 1 {
		t.Errorf("nodes = %v, err = %v; want the node carried forward", nodes, err)
	}
}

func TestANewerLayoutIsRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if _, err := f.db.write.Exec(fmt.Sprintf("PRAGMA user_version = %d", len(migrations)+1)); err != nil {
		t.Fatal(err)
	}
	if err := f.db.Close(); err != nil {
		t.Fatal(err)
	}
	var buf bytes.Buffer
	log := zlog.New(&buf, &buf, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)

	// Act
	_, err := Open(ctx(), f.path, log, Options{})

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("err = %v, want a storage refusal", err)
	}
	found := false
	for _, rec := range records(&buf) {
		if rec["operation"] == "zettel.db.migrate" && rec["level"] == "error" {
			found = true
		}
	}
	if !found {
		t.Errorf("no error record for the refused layout in %q", buf.String())
	}
}

func TestStorageFailureIsLoggedAndReported(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if err := f.db.write.Close(); err != nil {
		t.Fatal(err)
	}
	f.log.Reset()

	// Act
	_, _, err := f.db.RecordExchange(ctx(), "t1", "turn1", "q", Parent{}, "a")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("err = %v, want ErrStorage", err)
	}
	recs := records(f.log)
	if len(recs) == 0 || recs[0]["operation"] != "zettel.exchange.record" || recs[0]["level"] != "error" {
		t.Errorf("records = %v, want one error record for the begin", recs)
	}
	if ctx := recs[0]["context"].(map[string]any); ctx["thread"] != "t1" || ctx["turn"] != "turn1" {
		t.Errorf("context = %v, want the thread and turn named", ctx)
	}
}
