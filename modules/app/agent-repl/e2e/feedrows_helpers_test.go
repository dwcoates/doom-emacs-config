package e2e

import (
	"os"
	"path/filepath"
	"regexp"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// TestEndsTurn pins the one turn-ended match every feed wait and page scan in
// this suite goes through.
func TestEndsTurn(t *testing.T) {
	t.Parallel()
	turn := &conversationv1.TurnId{Value: "t1"}
	ended := &frontendv1.FeedRow_TurnEnded{TurnEnded: &frontendv1.FeedTurnEnded{}}
	cases := []struct {
		name string
		row  *frontendv1.FeedRow
		want bool
	}{
		{"the turn's own ended row", &frontendv1.FeedRow{Turn: turn, Row: ended}, true},
		{"another turn's ended row", &frontendv1.FeedRow{Turn: &conversationv1.TurnId{Value: "t2"}, Row: ended}, false},
		{"the turn's row that is not its end", &frontendv1.FeedRow{Turn: turn}, false},
		{"a row with no turn", &frontendv1.FeedRow{Row: ended}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Act
			got := endsTurn(turn)(tc.row)
			// Assert
			if got != tc.want {
				t.Fatalf("endsTurn(t1)(%v) = %v, want %v", tc.row, got, tc.want)
			}
		})
	}
}

// handRolledTurnEnded is the match endsTurn owns, written out by hand.
var handRolledTurnEnded = regexp.MustCompile(`GetTurn\(\)\.GetValue\(\) == \w+\.GetValue\(\) && \w+\.GetTurnEnded\(\) != nil`)

// TestTurnEndedMatchIsShared fails on any test in this suite that writes the
// turn-ended match out by hand instead of calling endsTurn, so a divergent
// copy cannot drift from the one every wait shares.
func TestTurnEndedMatchIsShared(t *testing.T) {
	t.Parallel()
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob the suite's sources: %v", err)
	}
	var offenders []string
	for _, f := range files {
		src, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		n := len(handRolledTurnEnded.FindAll(src, -1))
		if f == "world_test.go" {
			n-- // endsTurn's own body
		}
		if n > 0 {
			offenders = append(offenders, f)
		}
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled turn-ended matches in %v; call endsTurn(turn) instead", offenders)
	}
}
