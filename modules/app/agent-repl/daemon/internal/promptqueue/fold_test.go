package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// queuedPrompts parks each TURN with its TEXT, in order, behind a running
// turn, each judged to wait for the turn's end.
func queuedPrompts(t *testing.T, h *harness, turns ...[2]string) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	for _, turn := range turns {
		if _, err := h.q.Submit(context.Background(), submission(idsTurn(turn[0]), turn[1])); err != nil {
			t.Fatalf("Submit(%s): %v", turn[0], err)
		}
		h.q.waitForClassifications()
	}
}

// twoQueued parks t1 then t2 behind a running turn.
func twoQueued(t *testing.T, h *harness) {
	t.Helper()
	queuedPrompts(t, h, [2]string{"t1", "first words"}, [2]string{"t2", "second words"})
}

// fold folds TURN into ABOVE, failing the test on any error.
func fold(t *testing.T, h *harness, turn, above string) {
	t.Helper()
	if err := h.q.Fold(context.Background(), theWorkspace, idsTurn(turn), idsTurn(above)); err != nil {
		t.Fatalf("Fold(%s into %s): %v", turn, above, err)
	}
	h.q.waitForClassifications()
}

// snapshot is every hold the fake store holds, retired or not, so a refusal
// can be asserted to have changed none of them.
func snapshot(h *harness) map[ids.TurnID]wsm.HeldPrompt {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	out := map[ids.TurnID]wsm.HeldPrompt{}
	for turn, held := range h.db.held {
		copied := *held
		out[turn] = copied
	}
	return out
}

// unchanged fails the test when any hold differs from BEFORE in its words,
// retirement, verdict, acceptance or coalesced mark.
func unchanged(t *testing.T, h *harness, before map[ids.TurnID]wsm.HeldPrompt) {
	t.Helper()
	after := snapshot(h)
	if len(after) != len(before) {
		t.Fatalf("the store holds %d holds, want the %d it held", len(after), len(before))
	}
	for turn, was := range before {
		now := after[turn]
		if saidText(now.Said) != saidText(was.Said) || (now.Tombstone == nil) != (was.Tombstone == nil) ||
			(now.Classification == nil) != (was.Classification == nil) || now.Accepted != was.Accepted || now.Coalesced != was.Coalesced {
			t.Fatalf("hold %s changed: was %+v, now %+v", turn, was, now)
		}
	}
}

func TestFoldAppendsTheContentAfterABlankLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	twoQueued(t, h)

	// Act
	fold(t, h, "t2", "t1")

	// Assert
	blocks := h.db.hold("t1").Said.GetContent().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != "first words\n\nsecond words" {
		t.Fatalf("t1 blocks = %v, want one text block of both prompts' words, a blank line between", blocks)
	}
}

func TestFoldRetiresTheFoldedPromptAndKeepsTheEntrysPlace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedPrompts(t, h, [2]string{"t1", "first"}, [2]string{"t2", "second"}, [2]string{"t3", "third"})

	// Act
	fold(t, h, "t3", "t2")

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 2 || standing[0].Turn != "t1" || standing[1].Turn != "t2" || !standing[1].Coalesced {
		t.Fatalf("standing = (%+v, %v), want t1 then t2, t2 marked coalesced", standing, err)
	}
	if retired := h.db.retired("t3"); retired == nil || retired.Kind != tombstoneCoalesced {
		t.Fatalf("t3 tombstone = %+v, want it retired as coalesced", retired)
	}
}

func TestFoldRepublishesTheTray(t *testing.T) {
	// Arrange
	h := newHarness(t)
	twoQueued(t, h)

	// Act
	fold(t, h, "t2", "t1")

	// Assert
	if last := h.holds.last(); len(last) != 1 || last[0].Turn != "t1" {
		t.Fatalf("the tray's last push = %+v, want t1 alone", last)
	}
}

func TestFoldDiscardsTheMergedPromptsVerdictAndReclassifiesIt(t *testing.T) {
	// Arrange: t1's standing verdict was accepted; the fold must not carry it
	// onto the merged words.
	h := newHarness(t)
	twoQueued(t, h)
	if err := h.q.Accept(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Accept: %v", err)
	}
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteAfterToolCall, Reason: "adds to the work"}

	// Act
	fold(t, h, "t2", "t1")

	// Assert
	questions := h.judge.questions()
	if last := questions[len(questions)-1]; last[1] != "first words\n\nsecond words" {
		t.Fatalf("the judge was last asked about %q, want the merged words", last[1])
	}
	if got := h.db.hold("t1"); got.Accepted || got.Classification == nil || got.Classification.Arm != wsm.ArmAfterToolCall {
		t.Fatalf("t1 = %+v, want the merged words' own verdict and no acceptance", got)
	}
}

func TestAVerdictInFlightAcrossAFoldIsDiscarded(t *testing.T) {
	// Arrange: t1's verdict about its old words, and t2's about t1, are both
	// still being reached when the fold lands; every verdict would interrupt.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	release := h.judge.hold()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}
	for _, turn := range [][2]string{{"t1", "first words"}, {"t2", "second words"}} {
		if _, err := h.q.Submit(context.Background(), submission(idsTurn(turn[0]), turn[1])); err != nil {
			t.Fatalf("Submit(%s): %v", turn[0], err)
		}
	}
	if err := h.q.Fold(context.Background(), theWorkspace, "t2", "t1"); err != nil {
		t.Fatalf("Fold: %v", err)
	}

	// Act
	release()
	h.q.waitForClassifications()

	// Assert
	if killed := h.sender.killed(); len(killed) != 1 {
		t.Fatalf("killed = %v, want exactly one interrupt, from the merged words' verdict", killed)
	}
	if !logged(h.log.Records(), "info", opClassify, "the verdict is about content an edit has since replaced or a move has since superseded; it is discarded") {
		t.Fatalf("records = %+v, want the stale verdicts' discard at info", h.log.Records())
	}
}

func TestTheMergedPromptIsDeliveredAsOneTurnAtTheTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	twoQueued(t, h)
	fold(t, h, "t2", "t1")

	// Act
	turnEnds(h)

	// Assert
	h.sender.mu.Lock()
	defer h.sender.mu.Unlock()
	if len(h.sender.turns) != 1 || h.sender.turns[0] != "t1" || saidText(h.sender.said[0]) != "first words\n\nsecond words" {
		t.Fatalf("started = %v, want t1 alone, carrying the merged words", h.sender.turns)
	}
}

func TestFoldIsLoggedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	twoQueued(t, h)

	// Act
	fold(t, h, "t2", "t1")

	// Assert
	if !logged(h.log.Records(), "info", opFold, "the held prompt was folded into the one ahead; the merged prompt's verdict is discarded and it is reclassified") {
		t.Fatalf("records = %+v, want the fold at info", h.log.Records())
	}
}

func TestFoldRefuses(t *testing.T) {
	tests := []struct {
		name string
		// arrange builds the queue and answers the fold to attempt.
		arrange func(t *testing.T, h *harness) (turn, above string)
		wantErr error
		message string
	}{
		{
			name: "a turn nothing was ever held under",
			arrange: func(t *testing.T, h *harness) (string, string) {
				twoQueued(t, h)
				return "never", "t2"
			},
			wantErr: ErrNoSuchHold,
			message: "the fold is refused: no hold was ever recorded under the turn",
		},
		{
			name: "a prompt no longer held",
			arrange: func(t *testing.T, h *harness) (string, string) {
				queuedPrompts(t, h, [2]string{"t1", "first"}, [2]string{"t2", "second"}, [2]string{"t3", "third"})
				if err := h.q.Drop(context.Background(), theWorkspace, "t3"); err != nil {
					t.Fatalf("Drop: %v", err)
				}
				return "t3", "t2"
			},
			wantErr: ErrNotHeld,
			message: "the fold is refused: the prompt is no longer held",
		},
		{
			name: "a session act",
			arrange: func(t *testing.T, h *harness) (string, string) {
				queuedPrompts(t, h, [2]string{"t1", "first"}, [2]string{"t2", "/compact"})
				return "t2", "t1"
			},
			wantErr: ErrNotAPrompt,
			message: "the fold is refused: the entry is a session act, not a prompt",
		},
		{
			name: "an entry ahead that is not the one named",
			arrange: func(t *testing.T, h *harness) (string, string) {
				queuedPrompts(t, h, [2]string{"t1", "first"}, [2]string{"t2", "second"}, [2]string{"t3", "third"})
				return "t3", "t1"
			},
			wantErr: ErrAboveMoved,
			message: "the fold is refused: the entry named is no longer directly ahead",
		},
		{
			name: "an entry ahead that left the queue",
			arrange: func(t *testing.T, h *harness) (string, string) {
				twoQueued(t, h)
				if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
					t.Fatalf("Drop: %v", err)
				}
				return "t2", "t1"
			},
			wantErr: ErrAboveMoved,
			message: "the fold is refused: the entry named is no longer directly ahead",
		},
		{
			name: "a session act ahead",
			arrange: func(t *testing.T, h *harness) (string, string) {
				queuedPrompts(t, h, [2]string{"t1", "/compact"}, [2]string{"t2", "second"})
				return "t2", "t1"
			},
			wantErr: ErrAboveNotAPrompt,
			message: "the fold is refused: the entry ahead is a session act, not a prompt",
		},
		{
			name: "the entry ahead being edited",
			arrange: func(t *testing.T, h *harness) (string, string) {
				twoQueued(t, h)
				beginEdit(t, h, "t1")
				return "t2", "t1"
			},
			wantErr: ErrBeingEdited,
			message: "the fold is refused: one of the two entries is being edited",
		},
		{
			name: "the folded prompt being edited",
			arrange: func(t *testing.T, h *harness) (string, string) {
				twoQueued(t, h)
				beginEdit(t, h, "t2")
				return "t2", "t1"
			},
			wantErr: ErrBeingEdited,
			message: "the fold is refused: one of the two entries is being edited",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			turn, above := tt.arrange(t, h)
			before := snapshot(h)

			// Act
			err := h.q.Fold(context.Background(), theWorkspace, idsTurn(turn), idsTurn(above))
			h.q.waitForClassifications()

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("Fold = %v, want %v", err, tt.wantErr)
			}
			if !logged(h.log.Records(), "info", opFold, tt.message) {
				t.Fatalf("records = %+v, want %q at info", h.log.Records(), tt.message)
			}
			unchanged(t, h, before)
			if h.db.coalescences != 0 {
				t.Fatalf("coalescences = %d, want no fold attempted", h.db.coalescences)
			}
		})
	}
}

func TestFoldRefusedForAMovedEntryNamesTheEntryAheadNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedPrompts(t, h, [2]string{"t1", "first"}, [2]string{"t2", "second"}, [2]string{"t3", "third"})

	// Act
	err := h.q.Fold(context.Background(), theWorkspace, "t3", "t1")

	// Assert
	var moved *AboveMovedError
	if !errors.As(err, &moved) || moved.Current != "t2" {
		t.Fatalf("Fold = %v, want AboveMovedError naming t2", err)
	}
}

func TestFoldRefusedForAnEditNamesTheEditedEntry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	twoQueued(t, h)
	beginEdit(t, h, "t1")

	// Act
	err := h.q.Fold(context.Background(), theWorkspace, "t2", "t1")

	// Assert
	var edited *BeingEditedError
	if !errors.As(err, &edited) || edited.Turn != "t1" {
		t.Fatalf("Fold = %v, want BeingEditedError naming t1", err)
	}
}

func TestAFoldTheStoreRefusesChangesNothing(t *testing.T) {
	// Arrange: the merge and the retirement are one transaction, which fails
	// whole.
	h := newHarness(t)
	twoQueued(t, h)
	h.db.coalesceErr = errors.New("disk I/O error")
	before := snapshot(h)
	asked := len(h.judge.questions())

	// Act
	err := h.q.Fold(context.Background(), theWorkspace, "t2", "t1")
	h.q.waitForClassifications()

	// Assert
	if err == nil {
		t.Fatal("Fold succeeded through a store failure")
	}
	unchanged(t, h, before)
	if len(h.judge.questions()) != asked {
		t.Fatalf("the judge was asked again after a failed fold")
	}
	if !logged(h.log.Records(), "error", opFold, "the fold could not be recorded; both entries stand as they were") {
		t.Fatalf("records = %+v, want the failure at error", h.log.Records())
	}
}

func TestFoldedSaid(t *testing.T) {
	text := func(s string) *conversationv1.UserContentBlock {
		return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: s}}}
	}
	image := func(path string) *conversationv1.UserContentBlock {
		return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
			Location: &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: path}},
		}}}
	}
	said := func(blocks ...*conversationv1.UserContentBlock) *conversationv1.UserSaid {
		return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
	}
	// describe renders blocks as "text:<words>" or "image:<path>", in order.
	describe := func(s *conversationv1.UserSaid) []string {
		var out []string
		for _, b := range s.GetContent().GetBlocks() {
			if b.GetText() != nil {
				out = append(out, "text:"+b.GetText().GetText())
			} else {
				out = append(out, "image:"+b.GetImage().GetPath().GetPath())
			}
		}
		return out
	}
	tests := []struct {
		name string
		into *conversationv1.UserSaid
		from *conversationv1.UserSaid
		want []string
	}{
		{name: "text meets text across one blank line", into: said(text("a")), from: said(text("b")), want: []string{"text:a\n\nb"}},
		{name: "the folded prompt's image follows its words", into: said(text("a")), from: said(text("b"), image("p.png")),
			want: []string{"text:a\n\nb", "image:p.png"}},
		{name: "the entry's own image keeps its place", into: said(image("p.png"), text("a")), from: said(text("b")),
			want: []string{"image:p.png", "text:a\n\nb"}},
		{name: "an image at the seam is appended as it is", into: said(text("a"), image("p.png")), from: said(text("b")),
			want: []string{"text:a", "image:p.png", "text:b"}},
		{name: "a folded image-only prompt is appended", into: said(text("a")), from: said(image("p.png")),
			want: []string{"text:a", "image:p.png"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := describe(foldedSaid(tt.into, tt.from))

			// Assert
			if len(got) != len(tt.want) {
				t.Fatalf("blocks = %q, want %q", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("blocks = %q, want %q", got, tt.want)
				}
			}
		})
	}
}

func TestFoldedSaidLeavesBothInputsUntouched(t *testing.T) {
	// Arrange: the entry's stored words are read again if the store refuses.
	into, from := userSaid("a"), userSaid("b")

	// Act
	_ = foldedSaid(into, from)

	// Assert
	if saidText(into) != "a" || saidText(from) != "b" {
		t.Fatalf("inputs = %q, %q, want them unchanged", saidText(into), saidText(from))
	}
}
