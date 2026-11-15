package db

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// questionLine is a page line in `book` recording the question batch `id`,
// open when `open`, else settled as answered. Start and settle share the
// upsert key, as the shim keys them.
func questionLine(id, book string, open bool) *storev1.StoreEntry {
	q := &conversationv1.AgentQuestion{Id: &conversationv1.AgentQuestionId{Value: id}}
	if open {
		q.Result = &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{}}
	} else {
		q.Result = &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{
			Outcome: &conversationv1.AgentQuestionSuccess_Unanswered{Unanswered: &conversationv1.AgentQuestionUnanswered{}},
		}}
	}
	frame := &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: book},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Question{Question: q},
		}},
	}
	return pageEntry("w-"+id+boolKey(open), "question:"+id, book, frameItem(frame))
}

// permissionLine is a page line in `book` recording the consent ask `id`,
// open when `open`, else settled as abandoned.
func permissionLine(id, book string, open bool) *storev1.StoreEntry {
	p := &conversationv1.AgentPermission{Id: &conversationv1.AgentPermissionId{Value: id}}
	if open {
		p.Result = &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{}}
	} else {
		p.Result = &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
			Decision: &conversationv1.AgentPermissionSuccess_Abandoned{Abandoned: &conversationv1.AgentPermissionAbandoned{}},
		}}
	}
	frame := &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: book},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{Permission: p},
		}},
	}
	return pageEntry("w-"+id+boolKey(open), "permission:"+id, book, frameItem(frame))
}

func boolKey(open bool) string {
	if open {
		return "-start"
	}
	return "-settle"
}

// askIDs spells the asks a live-work answer lists, as kind:id@agent.
func askIDs(asks []*conversationv1.SessionOpenAsk) string {
	var out []string
	for _, a := range asks {
		switch {
		case a.GetQuestion() != nil:
			out = append(out, "question:"+a.GetQuestion().GetId().GetValue()+"@"+a.GetAgent().GetValue())
		case a.GetPermission() != nil:
			out = append(out, "permission:"+a.GetPermission().GetId().GetValue()+"@"+a.GetAgent().GetValue())
		}
	}
	return strings.Join(out, ",")
}

func TestTheAskIndexRecordsEachAskLineAndWhetherItIsOpen(t *testing.T) {
	tests := []struct {
		name  string
		entry *storev1.StoreEntry
		want  string
	}{
		{"an open question", questionLine("q1", "agent-main", true), "agent-main/1"},
		{"a settled question", questionLine("q1", "agent-main", false), "agent-main/0"},
		{"an open consent ask", permissionLine("p1", "agent-main", true), "agent-main/1"},
		{"a settled consent ask", permissionLine("p1", "agent-main", false), "agent-main/0"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			result := writeOK(t, d, tt.entry)

			// Assert
			if got := scalar[string](t, d, `SELECT book_agent_id || '/' || open FROM ask_line WHERE position = ?`, positionOf(t, result)); got != tt.want {
				t.Fatalf("ask_line = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheAskIndexHoldsNoRowForALineThatIsNoAsk(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w-p", "prompt:1", "agent-main", promptItem("agent-main")))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM ask_line`); got != 0 {
		t.Fatalf("ask_line rows = %d, want none", got)
	}
}

func TestTheAskIndexClosesTheAskItsSettleUpserts(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, questionLine("q1", "agent-main", true))

	// Act
	writeOK(t, d, questionLine("q1", "agent-main", false))

	// Assert
	if got := scalar[string](t, d, `SELECT group_concat(open) FROM ask_line`); got != "0" {
		t.Fatalf("ask_line open flags = %q, want the one row closed", got)
	}
}

func TestLiveWorkListsEveryOpenAskInTheLineageOldestFirst(t *testing.T) {
	// Arrange: the main agent's question, a subagent's consent ask, a settled
	// ask, and another session's open ask.
	d, _ := newStore(t)
	writeOK(t, d, questionLine("q1", "agent-main", true))
	writeOK(t, d, permissionLine("p1", "toolu_child", true))
	writeOK(t, d, questionLine("q2", "agent-main", true), questionLine("q2", "agent-main", false))
	writeOK(t, d, questionLine("q3", "other-main", true))
	if _, err := d.sql.ExecContext(ctx(), `UPDATE agent SET spawned_by_agent = 'agent-main' WHERE agent_id = 'toolu_child'`); err != nil {
		t.Fatalf("stamping the lineage: %v", err)
	}

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil {
		t.Fatalf("LiveWork: %v", err)
	}
	if got := askIDs(live.GetOpenAsks()); got != "question:q1@agent-main,permission:p1@toolu_child" {
		t.Fatalf("open asks = %q, want the lineage's two open asks oldest first", got)
	}
}

func TestLiveWorkAnswersEachOpenAskAsItsStoredStart(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := questionLine("q1", "agent-main", true)
	writeOK(t, d, entry)

	// Act
	live, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if err != nil || len(live.GetOpenAsks()) != 1 {
		t.Fatalf("LiveWork = (%v, %v), want one open ask", live, err)
	}
	want := entry.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetQuestion()
	if !proto.Equal(live.GetOpenAsks()[0].GetQuestion(), want) {
		t.Fatalf("open ask = %v, want the stored start %v", live.GetOpenAsks()[0].GetQuestion(), want)
	}
}

func TestLiveWorkRefusesAnAskIndexThatNamesAClosedRowOpen(t *testing.T) {
	// Arrange: the index and its row disagree.
	d, s := newStore(t)
	writeOK(t, d, questionLine("q1", "agent-main", false))
	if _, err := d.sql.ExecContext(ctx(), `UPDATE ask_line SET open = 1`); err != nil {
		t.Fatalf("breaking the index: %v", err)
	}

	// Act
	_, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if !errors.Is(err, ErrStorage) || !errors.Is(err, errAskIndexDiverged) {
		t.Fatalf("LiveWork error = %v, want a storage failure naming the diverged index", err)
	}
	s.assertLogged(t, "error", "refused")
}

func TestLiveWorkRefusesAnOpenAskWhoseFrameCannotBeDecoded(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, questionLine("q1", "agent-main", true))
	if _, err := d.sql.ExecContext(ctx(), `UPDATE entry SET frame = x'ff' WHERE kind = ?`, kindPageLine); err != nil {
		t.Fatalf("damaging the frames: %v", err)
	}

	// Act
	_, err := d.LiveWork(ctx(), "agent-main")

	// Assert
	if !errors.Is(err, ErrStorage) || !strings.Contains(err.Error(), "cannot be decoded") {
		t.Fatalf("LiveWork error = %v, want the decode failure", err)
	}
}

// preAskLineDatabase writes an open and a settled ask into a store and drops
// ask_line — the shape a store had before the index — and returns its path,
// closed. MUTATE, when set, alters the database before it closes.
func preAskLineDatabase(t *testing.T, mutate func(*DB)) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	d, err := OpenWithOptions(path, log, Options{unsynced: true, Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, d, questionLine("q1", "agent-main", true))
	writeOK(t, d, permissionLine("p1", "agent-main", true), permissionLine("p1", "agent-main", false))
	if _, err := d.sql.Exec(`DROP TABLE ask_line`); err != nil {
		t.Fatalf("dropping ask_line: %v", err)
	}
	if mutate != nil {
		mutate(d)
	}
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	return path
}

func TestOpenIndexesEveryStoredAskWhenItBuildsTheAskIndex(t *testing.T) {
	// Arrange
	path := preAskLineDatabase(t, nil)
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	if got := scalar[string](t, d, `SELECT group_concat(open, ',') FROM (SELECT open FROM ask_line ORDER BY position)`); got != "1,0" {
		t.Fatalf("ask_line open flags = %q, want the open question and the settled consent ask", got)
	}
}

func TestOpenRefusesToBuildTheAskIndexOverAFrameItCannotDecode(t *testing.T) {
	// Arrange
	path := preAskLineDatabase(t, func(d *DB) {
		if _, err := d.sql.Exec(`UPDATE entry SET frame = x'ff' WHERE kind = ?`, kindPageLine); err != nil {
			t.Fatalf("damaging the frames: %v", err)
		}
	})
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // test teardown
		t.Fatalf("reopen succeeded; want the backfill's decode failure")
	}
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("reopen error = %v, want a storage failure", err)
	}
}
