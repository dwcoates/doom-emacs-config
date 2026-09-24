package convert

// turn_test.go — which turn each record's entries are stamped with, by the
// transcript's own prompt and parent links, and never a guessed one.

import (
	"testing"
)

// turnExternalPrompt is a prompt typed in interactive Claude Code: the sidecar
// emits it and names its turn by the record's uuid.
func turnExternalPrompt(t *testing.T, uuid, parent, promptID, text string) string {
	t.Helper()
	fields := map[string]any{
		"type": "user", "uuid": uuid, "parentUuid": parent, "entrypoint": "cli",
		"message": map[string]any{"role": "user", "content": []any{map[string]any{"type": "text", "text": text}}},
	}
	if promptID != "" {
		fields["promptId"] = promptID
	}
	return kaLine(t, fields)
}

// lastLineTurns converts lines in file order through ONE converter and returns
// the turn each entry of the LAST line was stamped with ("" for none).
func lastLineTurns(t *testing.T, lines ...string) []string {
	t.Helper()
	c := newTestConverter(t)
	var turns []string
	for i, line := range lines {
		entries := c.Line(decode(t, line), testAttribution(int64(i*1000)), nil)
		if i != len(lines)-1 {
			continue
		}
		for _, entry := range entries {
			turns = append(turns, entry.GetTurn().GetValue())
		}
	}
	return turns
}

// TestRecordsAreStampedWithTheTurnTheirLinksName drives one edge per case: the
// SUBJECT is the last line, and every line before it is its arrangement.
func TestRecordsAreStampedWithTheTurnTheirLinksName(t *testing.T) {
	cases := []struct {
		name  string
		lines func(t *testing.T) []string
		want  string
	}{
		{
			name: "an external prompt opens its own turn",
			lines: func(t *testing.T) []string {
				return []string{turnExternalPrompt(t, "p1", "", "pid-1", "hello")}
			},
			want: "p1",
		},
		{
			name: "an assistant record follows its parent into the external turn",
			lines: func(t *testing.T) []string {
				return []string{
					turnExternalPrompt(t, "p1", "", "pid-1", "hello"),
					kaReply(t, "a1", "p1", "m1", "hi"),
				}
			},
			want: "p1",
		},
		{
			name: "a tool result names its turn by promptId whatever its parent",
			lines: func(t *testing.T) []string {
				return []string{
					turnExternalPrompt(t, "p1", "", "pid-1", "hello"),
					kaCall(t, "a1", "p1", "m1", "call-1"),
					kaResult(t, "r1", "unrelated", "pid-1", "call-1"),
				}
			},
			want: "p1",
		},
		{
			name: "an agent-repl prompt's records name no turn",
			lines: func(t *testing.T) []string {
				return []string{
					kaPrompt(t, "p1", "", "pid-1", "hello"),
					kaReply(t, "a1", "p1", "m1", "hi"),
				}
			},
			want: "",
		},
		{
			name: "an agent-repl prompt chained onto an external turn does not inherit it",
			lines: func(t *testing.T) []string {
				return []string{
					turnExternalPrompt(t, "p1", "", "pid-1", "hello"),
					kaReply(t, "a1", "p1", "m1", "hi"),
					kaPrompt(t, "p2", "a1", "pid-2", "again"),
					kaReply(t, "a2", "p2", "m2", "sure"),
				}
			},
			want: "",
		},
		{
			name: "a record whose parent predates the file's window names no turn",
			lines: func(t *testing.T) []string {
				return []string{kaReply(t, "a1", "before-the-window", "m1", "hi")}
			},
			want: "",
		},
		{
			name: "a tool result naming a prompt this file never showed names no turn",
			lines: func(t *testing.T) []string {
				return []string{
					turnExternalPrompt(t, "p1", "", "pid-1", "hello"),
					kaResult(t, "r1", "p1", "pid-unseen", "call-1"),
				}
			},
			want: "",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			lines := tc.lines(t)

			// Act
			turns := lastLineTurns(t, lines...)

			// Assert
			if len(turns) == 0 {
				t.Fatal("the subject line produced no entries to stamp")
			}
			for i, got := range turns {
				if got != tc.want {
					t.Fatalf("entry %d turn = %q, want %q (all: %q)", i, got, tc.want, turns)
				}
			}
		})
	}
}
