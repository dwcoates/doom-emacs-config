package convert

// resend_test.go — an edited or re-sent prompt is ONE row, holding the version
// the conversation continued from (resend.go).

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// resendFixture is the captured re-send: a prompt, its attachment, the same
// prompt re-sent on the same parent, its attachment, and the answer's first line.
const resendFixture = "transcript-lines/user-prompt-resend.jsonl"

// The captured fixture's two versions and the parent they share.
const (
	resendFirstUUID  = "dd2fbb61-2428-441d-a348-28a2695b0b35"
	resendSecondUUID = "126511e4-af2c-4841-9819-9dca95d1ebcd"
)

// corpusLines reads every line of a multi-line corpus fixture, verbatim.
func corpusLines(t *testing.T, rel string) []string {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(corpusFixtureDir, rel))
	if err != nil {
		t.Fatalf("read corpus fixture %s: %v", rel, err)
	}
	var lines []string
	for _, line := range strings.Split(string(raw), "\n") {
		if strings.TrimSpace(line) != "" {
			lines = append(lines, line)
		}
	}
	return lines
}

// resendPrompt is a prompt typed in interactive Claude Code on `parent`.
func resendPrompt(t *testing.T, uuid, parent, text string) string {
	t.Helper()
	return turnExternalPrompt(t, uuid, parent, "pid-"+uuid, text)
}

// resendLocalCommand is the CLI's record of a local command run while a prompt
// was pending, as observed under an abandoned version.
func resendLocalCommand(t *testing.T, uuid, parent string) string {
	t.Helper()
	return kaLine(t, map[string]any{
		"type": "system", "subtype": "local_command", "uuid": uuid, "parentUuid": parent,
		"content": "<command-name>/model</command-name>", "level": "info", "isMeta": false,
	})
}

// promptWrites is every prompt write the entries carry, in write order.
type promptWrite struct {
	key, turn, text, writeID string
}

func promptWrites(entries []*storev1.StoreEntry) []promptWrite {
	var out []promptWrite
	for _, e := range entries {
		prompt := pageLine(e).GetAgentItem().GetAgentPrompt()
		if prompt == nil {
			continue
		}
		var text string
		if blocks := prompt.GetSaid().GetContent().GetBlocks(); len(blocks) > 0 {
			text = blocks[0].GetText().GetText()
		}
		out = append(out, promptWrite{key: e.GetUpsertKey(), turn: prompt.GetId().GetValue(), text: text, writeID: e.GetWriteId()})
	}
	return out
}

// versionRows folds the writes into the rows a store holds: each key's words
// are its LAST write's, as an upsert leaves them.
func versionRows(writes []promptWrite) (keys []string, words map[string]string) {
	words = map[string]string{}
	for _, w := range writes {
		if _, seen := words[w.key]; !seen {
			keys = append(keys, w.key)
		}
		words[w.key] = w.text
	}
	return keys, words
}

// TestAReSentPromptIsOneRowHoldingTheAnsweredVersion drives one edge per case:
// the rows a whole run of lines leaves, and the words each row holds.
func TestAReSentPromptIsOneRowHoldingTheAnsweredVersion(t *testing.T) {
	cases := []struct {
		name      string
		lines     func(t *testing.T) []string
		wantKeys  []string
		wantWords map[string]string
	}{
		{
			name:     "an identical re-send, as captured, leaves the first version's one row",
			lines:    func(t *testing.T) []string { return corpusLines(t, resendFixture) },
			wantKeys: []string{PromptKey(resendFirstUUID)},
			wantWords: map[string]string{PromptKey(resendFirstUUID): "what would this look like int he gns page? Can we update our mock page " +
				"accordingly and open in my browser to i can see what it'd look like?"},
		},
		{
			name: "an edited version supersedes the unanswered version's words",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "is that not the case?"),
					kaAttachment(t, "p1-att", "p1"),
					resendPrompt(t, "p2", "anchor", "is that not the case? it does send some"),
					kaAttachment(t, "p2-att", "p2"),
					kaReply(t, "a1", "p2-att", "m1", "it does"),
				}
			},
			wantKeys:  []string{PromptKey("p1")},
			wantWords: map[string]string{PromptKey("p1"): "is that not the case? it does send some"},
		},
		{
			name: "an earlier version that got the answer is written back into the row",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "first words"),
					kaAttachment(t, "p1-att", "p1"),
					resendPrompt(t, "p2", "anchor", "second words"),
					kaAttachment(t, "p2-att", "p2"),
					kaReply(t, "a1", "p1-att", "m1", "answering the first"),
				}
			},
			wantKeys:  []string{PromptKey("p1")},
			wantWords: map[string]string{PromptKey("p1"): "first words"},
		},
		{
			name: "an abandoned version's deep branch of attachments and a local command decides nothing",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "continue"),
					kaAttachment(t, "p1-att", "p1"),
					kaAttachment(t, "p1-att-2", "p1-att"),
					resendLocalCommand(t, "p1-cmd", "p1-att-2"),
					resendPrompt(t, "p2", "anchor", "continue now"),
					kaAttachment(t, "p2-att", "p2"),
					kaReply(t, "a1", "p2-att", "m1", "continuing"),
				}
			},
			wantKeys:  []string{PromptKey("p1")},
			wantWords: map[string]string{PromptKey("p1"): "continue now"},
		},
		{
			name: "an answer reached through a deep chain still names the version it descends from",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "first words"),
					kaAttachment(t, "p1-att", "p1"),
					kaAttachment(t, "p1-att-2", "p1-att"),
					kaAttachment(t, "p1-att-3", "p1-att-2"),
					resendPrompt(t, "p2", "anchor", "second words"),
					kaReply(t, "a1", "p1-att-3", "m1", "answering the first"),
				}
			},
			wantKeys:  []string{PromptKey("p1")},
			wantWords: map[string]string{PromptKey("p1"): "first words"},
		},
		{
			name: "three versions leave one row holding the answered third",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "one"),
					resendPrompt(t, "p2", "anchor", "two"),
					resendPrompt(t, "p3", "anchor", "three"),
					kaReply(t, "a1", "p3", "m1", "answering three"),
				}
			},
			wantKeys:  []string{PromptKey("p1")},
			wantWords: map[string]string{PromptKey("p1"): "three"},
		},
		{
			name: "a version written after the first one was answered keeps a row of its own",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "first words"),
					kaReply(t, "a1", "p1", "m1", "answering the first"),
					resendPrompt(t, "p2", "anchor", "second words"),
				}
			},
			wantKeys:  []string{PromptKey("p1"), PromptKey("p2")},
			wantWords: map[string]string{PromptKey("p1"): "first words", PromptKey("p2"): "second words"},
		},
		{
			name: "a prompt on a different parent keeps a row of its own",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "anchor", "first words"),
					kaAttachment(t, "p1-att", "p1"),
					resendPrompt(t, "p2", "p1-att", "second words"),
				}
			},
			wantKeys:  []string{PromptKey("p1"), PromptKey("p2")},
			wantWords: map[string]string{PromptKey("p1"): "first words", PromptKey("p2"): "second words"},
		},
		{
			name: "two parentless prompts are never read as versions of each other",
			lines: func(t *testing.T) []string {
				return []string{
					resendPrompt(t, "p1", "", "first words"),
					resendPrompt(t, "p2", "", "second words"),
				}
			},
			wantKeys:  []string{PromptKey("p1"), PromptKey("p2")},
			wantWords: map[string]string{PromptKey("p1"): "first words", PromptKey("p2"): "second words"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			lines := tc.lines(t)

			// Act.
			keys, words := versionRows(promptWrites(convertLines(t, c, lines...)))

			// Assert.
			if strings.Join(keys, ",") != strings.Join(tc.wantKeys, ",") {
				t.Fatalf("prompt rows = %v, want %v", keys, tc.wantKeys)
			}
			for key, want := range tc.wantWords {
				if words[key] != want {
					t.Fatalf("row %s holds %q, want %q", key, words[key], want)
				}
			}
		})
	}
}

// TestALiveReSendReplacesThePendingRowInPlace asserts the LIVE case: the first
// version is read and drawn on its own poll, before the re-send exists, and the
// re-send read on a later poll is written on that same row and turn — so a feed
// holding the first bubble has it replaced, never a second one beside it.
func TestALiveReSendReplacesThePendingRowInPlace(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	first := promptWrites(c.Line(decode(t, resendPrompt(t, "p1", "anchor", "first words")), testAttribution(0), nil))
	c.Line(decode(t, kaAttachment(t, "p1-att", "p1")), testAttribution(1000), nil)

	// Act.
	second := promptWrites(c.Line(decode(t, resendPrompt(t, "p2", "anchor", "second words")), testAttribution(2000), nil))

	// Assert.
	if len(first) != 1 || len(second) != 1 {
		t.Fatalf("prompt writes = %d then %d, want one each", len(first), len(second))
	}
	if second[0].key != first[0].key || second[0].turn != first[0].turn {
		t.Fatalf("re-send wrote row %s turn %s, want the pending row %s turn %s",
			second[0].key, second[0].turn, first[0].key, first[0].turn)
	}
	if second[0].text != "second words" {
		t.Fatalf("re-send wrote %q, want its own words", second[0].text)
	}
}

// TestTheRewrittenRowCarriesAWriteIDOfItsOwn asserts the write that puts an
// earlier version back is not absorbed as a replay of any earlier write.
func TestTheRewrittenRowCarriesAWriteIDOfItsOwn(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	lines := []string{
		resendPrompt(t, "p1", "anchor", "first words"),
		kaAttachment(t, "p1-att", "p1"),
		resendPrompt(t, "p2", "anchor", "second words"),
		kaReply(t, "a1", "p1-att", "m1", "answering the first"),
	}

	// Act.
	entries := convertLines(t, c, lines...)

	// Assert.
	seen := map[string]bool{}
	for _, e := range entries {
		if seen[e.GetWriteId()] {
			t.Fatalf("write_id %s minted twice (key %s)", e.GetWriteId(), e.GetUpsertKey())
		}
		seen[e.GetWriteId()] = true
	}
}

// TestARecordUnderAReSentVersionIsStampedWithTheRowsTurn asserts the answer
// under a re-sent version belongs to the one turn the row names.
func TestARecordUnderAReSentVersionIsStampedWithTheRowsTurn(t *testing.T) {
	// Arrange.
	lines := []string{
		resendPrompt(t, "p1", "anchor", "first words"),
		resendPrompt(t, "p2", "anchor", "second words"),
		kaReply(t, "a1", "p2", "m1", "answering the second"),
	}

	// Act.
	turns := lastLineTurns(t, lines...)

	// Assert.
	if len(turns) == 0 {
		t.Fatal("the answer produced no entries to stamp")
	}
	for i, got := range turns {
		if got != "p1" {
			t.Fatalf("entry %d turn = %q, want the row's turn p1 (all: %q)", i, got, turns)
		}
	}
}
