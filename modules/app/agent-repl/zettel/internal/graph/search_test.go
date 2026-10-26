package graph

import (
	"bytes"
	"errors"
	"reflect"
	"testing"
)

// threadsOf is each match's thread, in order.
func threadsOf(ms []Match) []ThreadID {
	var out []ThreadID
	for _, m := range ms {
		out = append(out, m.Thread)
	}
	return out
}

// search runs Search, failing the test on any error.
func (f fixture) search(t *testing.T, query string) []Match {
	t.Helper()
	ms, err := f.db.Search(ctx(), query)
	if err != nil {
		t.Fatalf("Search(%q): %v", query, err)
	}
	return ms
}

func TestSearchMatchesWords(t *testing.T) {
	tests := []struct {
		name  string
		query string
		want  []ThreadID
	}{
		{name: "a word of the question", query: "sky", want: []ThreadID{"t1"}},
		{name: "a word of the answer", query: "scattering", want: []ThreadID{"t1"}},
		{name: "the start of a word", query: "scatt", want: []ThreadID{"t1"}},
		{name: "every word, in any order", query: "scattering blue", want: []ThreadID{"t1"}},
		{name: "a word no node holds keeps out the match", query: "blue lifetimes", want: nil},
		{name: "case does not matter", query: "RAYLEIGH", want: []ThreadID{"t1"}},
		{name: "accents do not matter", query: "cafe", want: []ThreadID{"t2"}},
		{name: "search syntax is only words", query: `blue OR "lifetimes`, want: nil},
		{name: "a lone quote is a word like any other", query: `"`, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.answered(t, "t1", "turn1", Parent{}, "Why is the sky blue?", "Rayleigh scattering.")
			f.answered(t, "t2", "turn2", Parent{}, "Where is the café?", "Down the road.")

			// Act
			got := threadsOf(f.search(t, tt.query))

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Search(%q) = %v, want %v", tt.query, got, tt.want)
			}
		})
	}
}

func TestSearchAnswersEachThreadOnceByItsBestNode(t *testing.T) {
	// Arrange: t1 says "rust" twice, once in passing; t2 is about it.
	f := newFixture(t)
	root := f.answered(t, "t1", "turn1", Parent{}, "What about iron?", "It can rust.")
	f.answered(t, "t1", "turn2", askedOf(root), "And rust?", "Rust is iron oxide; rust flakes.")
	f.answered(t, "t2", "turn3", Parent{}, "Rust rust rust lifetimes?", "Rust lifetimes bound borrows in Rust.")

	// Act
	got := f.search(t, "rust")

	// Assert
	if threads := threadsOf(got); !reflect.DeepEqual(threads, []ThreadID{"t2", "t1"}) {
		t.Fatalf("threads = %v, want t2 (the better match) then t1, each once", threads)
	}
	if got[1].Node == root.ID {
		t.Errorf("t1 answered by its root %s, want its rust-heavy follow-up", root.ID)
	}
}

func TestSearchSnippetMarksTheMatchedWords(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.answered(t, "t1", "turn1", Parent{}, "Why is the sky blue?", "Rayleigh scattering.")

	// Act
	got := f.search(t, "sky")

	// Assert
	want := []SnippetPart{{Text: "Why is the "}, {Text: "sky", Hit: true}, {Text: " blue?"}}
	if !reflect.DeepEqual(got[0].Snippet, want) {
		t.Fatalf("snippet = %+v, want %+v", got[0].Snippet, want)
	}
}

func TestSnippetParts(t *testing.T) {
	tests := []struct {
		name   string
		marked string
		want   []SnippetPart
	}{
		{name: "no match", marked: "plain", want: []SnippetPart{{Text: "plain"}}},
		{name: "a match alone", marked: hitOpen + "hit" + hitClose, want: []SnippetPart{{Text: "hit", Hit: true}}},
		{name: "two matches apart", marked: "a " + hitOpen + "b" + hitClose + " c " + hitOpen + "d" + hitClose,
			want: []SnippetPart{{Text: "a "}, {Text: "b", Hit: true}, {Text: " c "}, {Text: "d", Hit: true}}},
		{name: "empty", marked: "", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if got := snippetParts(tt.marked); !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("snippetParts = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestSnippetPartsPanicsOnAnUnclosedMatch(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unclosed match was split, want a panic")
		}
	}()

	// Act
	snippetParts("a " + hitOpen + "b")
}

func TestSearchRefusesABlankQuery(t *testing.T) {
	for _, query := range []string{"", "   "} {
		t.Run(query, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			_, err := f.db.Search(ctx(), query)

			// Assert
			if !errors.Is(err, ErrBlankQuery) {
				t.Fatalf("err = %v, want ErrBlankQuery", err)
			}
		})
	}
}

func TestSearchStorageFailureIsLoggedAndReported(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if err := f.db.read.Close(); err != nil {
		t.Fatal(err)
	}
	f.log.Reset()

	// Act
	_, err := f.db.Search(ctx(), "sky")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("err = %v, want ErrStorage", err)
	}
	recs := records(f.log)
	if len(recs) == 0 || recs[0]["operation"] != "zettel.search" || recs[0]["level"] != "error" {
		t.Fatalf("records = %v, want the failed search at error", recs)
	}
	if c := recs[0]["context"].(map[string]any); c["query"] != "sky" {
		t.Errorf("context = %v, want the query named", c)
	}
}

func TestTheIndexFollowsANewerRevision(t *testing.T) {
	// Arrange
	f := newFixture(t)
	n := f.answered(t, "t1", "turn1", Parent{}, "q", "first words")
	if _, err := f.db.write.Exec(`INSERT INTO revision (node_id, number, markdown, written_at_ms) VALUES (?, 2, 'second words', 1)`, string(n.ID)); err != nil {
		t.Fatal(err)
	}

	// Act
	first, second := f.search(t, "first"), f.search(t, "second")

	// Assert
	if len(first) != 0 || len(second) != 1 {
		t.Fatalf("first = %v, second = %v; want only the newest answer indexed", first, second)
	}
}

func TestLayoutTwoIndexesTheNodesAlreadyStored(t *testing.T) {
	// Arrange: a file at layout 1, holding a node, with no index.
	f := newFixture(t)
	f.answered(t, "t1", "turn1", Parent{}, "Why is the sky blue?", "Rayleigh scattering.")
	for _, stmt := range []string{
		"DROP TRIGGER node_text_on_node", "DROP TRIGGER node_text_on_revision", "DROP TRIGGER node_text_on_delete",
		"DROP TABLE node_text", "ALTER TABLE node DROP COLUMN question_abbreviation", "PRAGMA user_version = 1",
	} {
		if _, err := f.db.write.Exec(stmt); err != nil {
			t.Fatalf("%s: %v", stmt, err)
		}
	}
	if err := f.db.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	var buf bytes.Buffer
	reopened := fixture{db: openAt(t, f.path, &buf), path: f.path, log: &buf}

	// Assert
	if got := threadsOf(reopened.search(t, "scattering")); !reflect.DeepEqual(got, []ThreadID{"t1"}) {
		t.Fatalf("Search = %v, want the stored node indexed", got)
	}
}

func TestDeleteThread(t *testing.T) {
	tests := []struct {
		name        string
		thread      ThreadID
		wantDeleted int
	}{
		{name: "a thread with nodes", thread: "t1", wantDeleted: 2},
		{name: "a thread zettel holds nothing of", thread: "t9", wantDeleted: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			root := f.answered(t, "t1", "turn1", Parent{}, "Why is the sky blue?", "Rayleigh scattering.")
			f.answered(t, "t1", "turn2", askedOf(root), "And sunsets?", "Longer paths.")
			f.answered(t, "t2", "turn3", Parent{}, "Is the sea blue?", "It reflects the sky.")

			// Act
			deleted, err := f.db.DeleteThread(ctx(), tt.thread)

			// Assert
			if err != nil || deleted != tt.wantDeleted {
				t.Fatalf("DeleteThread = %d, %v; want %d", deleted, err, tt.wantDeleted)
			}
			if nodes, _ := f.db.Thread(ctx(), tt.thread); len(nodes) != 0 {
				t.Errorf("nodes left = %v, want none", nodes)
			}
			var revisions int
			if err := f.db.read.QueryRow(`SELECT COUNT(*) FROM revision WHERE node_id IN ('id001', 'id002')`).Scan(&revisions); err != nil || (tt.thread == "t1" && revisions != 0) {
				t.Errorf("revisions left = %d (%v), want none of the thread's", revisions, err)
			}
			if got := threadsOf(f.search(t, "sky")); !reflect.DeepEqual(got, wantSkyThreads(tt.thread)) {
				t.Errorf("Search(sky) = %v, want %v: the index follows the delete", got, wantSkyThreads(tt.thread))
			}
			if nodes, _ := f.db.Thread(ctx(), "t2"); len(nodes) != 1 {
				t.Errorf("t2 nodes = %v, want another thread untouched", nodes)
			}
		})
	}
}

// wantSkyThreads is which threads still say "sky" after DELETED is deleted.
func wantSkyThreads(deleted ThreadID) []ThreadID {
	if deleted == "t1" {
		return []ThreadID{"t2"}
	}
	return []ThreadID{"t1", "t2"}
}

func TestDeleteThreadStorageFailureIsLoggedAndReported(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if err := f.db.write.Close(); err != nil {
		t.Fatal(err)
	}
	f.log.Reset()

	// Act
	_, err := f.db.DeleteThread(ctx(), "t1")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("err = %v, want ErrStorage", err)
	}
	recs := records(f.log)
	if len(recs) == 0 || recs[0]["operation"] != "zettel.thread.delete" || recs[0]["level"] != "error" {
		t.Fatalf("records = %v, want the failed delete at error", recs)
	}
	if c := recs[0]["context"].(map[string]any); c["thread"] != "t1" {
		t.Errorf("context = %v, want the thread named", c)
	}
}
