package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// mustPutTurnAt records a turn of WS with TEXT and ORIGIN, started at AT,
// failing the test on any error.
func mustPutTurnAt(t *testing.T, s *store, ws WorkspaceID, id TurnID, text string, origin conversationv1.PromptOrigin, at time.Time) {
	t.Helper()
	if err := s.PutTurn(context.Background(), Turn{ID: id, Workspace: ws, Text: text, Origin: origin.String(), StartedAt: at}); err != nil {
		t.Fatalf("PutTurn(%s): %v", id, err)
	}
}

func TestTurnStartsAnswersEveryTurnByWorkspaceThenStart(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first, second := testWorkspace(t, s), testWorkspaceNamed(t, s, "second")
	base := time.Unix(1_700_000_000, 0).UTC()
	sent := conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT
	created := conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED
	mustPutTurnAt(t, s, second.ID, "t-late", "later", sent, base.Add(2*time.Minute))
	mustPutTurnAt(t, s, first.ID, "t-mid", "middle", created, base.Add(time.Minute))
	mustPutTurnAt(t, s, second.ID, "t-early", "earlier", sent, base)

	// Act
	got, err := s.TurnStarts(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("TurnStarts: %v", err)
	}
	byWorkspace := map[WorkspaceID][]TurnStart{
		first.ID:  {{Workspace: first.ID, StartedAt: base.Add(time.Minute), Origin: created}},
		second.ID: {{Workspace: second.ID, StartedAt: base, Origin: sent}, {Workspace: second.ID, StartedAt: base.Add(2 * time.Minute), Origin: sent}},
	}
	lo, hi := first.ID, second.ID
	if hi < lo {
		lo, hi = hi, lo
	}
	want := append(append([]TurnStart(nil), byWorkspace[lo]...), byWorkspace[hi]...)
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("TurnStarts = %+v, want %+v", got, want)
	}
}

func TestTurnStartsWithholdsOnlyTextThatBeginsWithNoCommand(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "prose is withheld", text: "fix the footer /compact", want: ""},
		{name: "an empty text is withheld", text: "", want: ""},
		{name: "a command is answered whole", text: "/compact keep the plan", want: "/compact keep the plan"},
		{name: "a command after ascii whitespace is answered whole", text: " \t\n/clear", want: " \t\n/clear"},
		{name: "a text led by non-ascii whitespace is answered whole", text: " /compact", want: " /compact"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			mustPutTurnAt(t, s, ws.ID, "t-1", tt.text, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, time.Unix(1, 0))

			// Act
			got, err := s.TurnStarts(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("TurnStarts: %v", err)
			}
			if len(got) != 1 || got[0].CommandText != tt.want {
				t.Fatalf("TurnStarts = %+v, want one start with command text %q", got, tt.want)
			}
		})
	}
}

func TestTurnStartsRefusesAnUnnamedOrigin(t *testing.T) {
	tests := []struct {
		name   string
		origin string
	}{
		{name: "an origin the vocabulary does not name", origin: "emacs"},
		{name: "the unspecified origin", origin: "PROMPT_ORIGIN_UNSPECIFIED"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			if _, err := s.db().ExecContext(context.Background(),
				`INSERT INTO turns (id, workspace_id, text, origin, displaced, started_at) VALUES ('t-bad', ?, 'hi', ?, 0, 1)`, ws.ID, tt.origin); err != nil {
				t.Fatalf("seed the turn: %v", err)
			}

			// Act
			got, err := s.TurnStarts(context.Background())

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Table != "turns" || decode.Row != "t-bad" || decode.Field != "origin" || !errors.Is(err, errUnnamedTurnOrigin) {
				t.Fatalf("TurnStarts error = %v, want a DecodeError of t-bad's origin", err)
			}
			if got != nil {
				t.Fatalf("TurnStarts answered %+v beside its error, want nothing", got)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.turn_starts"}); !ok {
				t.Fatalf("no error record of the failed read: %+v", log.Records())
			}
		})
	}
}
