package promptqueue

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// editorUp is the probe of a workspace whose editor's host stream stands.
func editorUp() bool { return true }

// queuedBehind parks TURNS, in order, behind a running turn, each judged to
// wait for the turn's end.
func queuedBehind(t *testing.T, h *harness, turns ...string) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	for _, turn := range turns {
		heldPrompt(t, h, turn, classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	}
}

// turnEnds ends the running turn, as the watcher reports it.
func turnEnds(h *harness) {
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
}

// beginEdit claims TURN, failing the test on a refusal.
func beginEdit(t *testing.T, h *harness, turn string) {
	t.Helper()
	if err := h.q.BeginEdit(context.Background(), theWorkspace, idsTurn(turn), editorUp); err != nil {
		t.Fatalf("BeginEdit(%s): %v", turn, err)
	}
}

// logged reports whether a record at LEVEL under OP carries MESSAGE.
func logged(records []dlog.Record, level, op, message string) bool {
	for _, r := range records {
		if r.Level == level && r.Operation == op && r.Message == message {
			return true
		}
	}
	return false
}

func TestBeginEditClaimsThePrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	// Act
	beginEdit(t, h, "t1")
	// Assert
	edit, ok := h.q.Editing(theWorkspace)
	if !ok || edit.Turn != "t1" || firstTextOf(edit) != "the held prompt" {
		t.Fatalf("Editing = (%+v, %v), want the claim on t1 with its content", edit, ok)
	}
}

func TestBeginEditMarksTheTrayAndRepublishesTheHostView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	// Act
	beginEdit(t, h, "t1")
	// Assert
	if marks := h.holds.editingMarks(); len(marks) != 1 || marks[0] != "t1" {
		t.Fatalf("tray editing marks = %v, want t1", marks)
	}
	if h.hostPublished() != 1 {
		t.Fatalf("host publishes = %d, want 1", h.hostPublished())
	}
}

func TestBeginEditIsLoggedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	// Act
	beginEdit(t, h, "t1")
	// Assert
	if !logged(h.log.Records(), "info", opEditBegin,
		"the held prompt is being edited; it and every prompt queued after it stay held") {
		t.Fatalf("records = %+v, want the begin at info", h.log.Records())
	}
}

func TestBeginEditMintsADistinctIdentityPerEdit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	first, _ := h.q.Editing(theWorkspace)
	if err := h.q.CancelEdit(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("CancelEdit: %v", err)
	}
	// Act
	beginEdit(t, h, "t1")
	// Assert
	second, _ := h.q.Editing(theWorkspace)
	if first.ID == second.ID {
		t.Fatalf("both edits carried identity %d, want distinct", first.ID)
	}
}

func TestATurnEndDeliversOnlyThePromptsAheadOfTheEdit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2", "t3")
	beginEdit(t, h, "t2")
	// Act
	turnEnds(h)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want only t1, ahead of the edit", started)
	}
}

func TestATurnEndDuringAnEditOfTheFirstPromptDeliversNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t1")
	// Act
	turnEnds(h)
	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing delivered during the edit", started)
	}
}

func TestATurnEndWithholdingPromptsSaysWhyAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t1")
	// Act
	turnEnds(h)
	// Assert
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opTurnEnded && r.Context["withheld"] == 2 {
			return
		}
	}
	t.Fatalf("records = %+v, want the withheld count at info", h.log.Records())
}

func TestAnInterjectionQueuedAfterTheEditDoesNotCarryItPast(t *testing.T) {
	// Arrange: t2 earned the semantic head, and t1 ahead of it is edited.
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	head := idsTurn("t2")
	h.q.state(theWorkspace).head = &head
	beginEdit(t, h, "t1")
	// Act
	turnEnds(h)
	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want the head withheld behind the edit", started)
	}
}

func TestAReleaseOfAPromptBehindTheEditIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t1")
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t2")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release = %v, want ErrReleaseRefused", err)
	}
}

func TestAReleaseOfThePromptAheadOfTheEditIsAllowed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t2")
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if err != nil {
		t.Fatalf("Release = %v, want the release of a prompt ahead of the edit", err)
	}
}

func TestASubmissionWhileAnEditStandsIsHeldBehindIt(t *testing.T) {
	// Arrange: the turn ended during the edit, so nothing is running.
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	turnEnds(h)
	// Act
	disposition, err := h.q.Submit(context.Background(), submission("t2", "a new prompt"))
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if disposition.Delivered || len(h.sender.started()) != 0 {
		t.Fatalf("disposition = %+v, started = %v, want the submission held", disposition, h.sender.started())
	}
}

func TestCommitEditReplacesTheContent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("the edited words")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := saidText(h.db.hold("t1").Said); got != "the edited words" {
		t.Fatalf("said = %q, want the edited words", got)
	}
}

func TestCommitEditRetiresTheClaim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("the edited words")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if _, ok := h.q.Editing(theWorkspace); ok {
		t.Fatal("the claim survived the commit")
	}
	if marks := h.holds.editingMarks(); marks[len(marks)-1] != "" {
		t.Fatalf("tray editing marks = %v, want the marker cleared last", marks)
	}
}

func TestCommitEditReclassifiesTheNewContent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("the edited words")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	questions := h.judge.questions()
	if last := questions[len(questions)-1]; last[1] != "the edited words" {
		t.Fatalf("the judge was last asked about %q, want the edited words", last[1])
	}
	if c := h.db.hold("t1").Classification; c == nil || c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("classification = %+v, want the new verdict", c)
	}
}

func TestCommitEditThenAnInterjectVerdictInterruptsTheRunningTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "a correction"}
	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("stop, do this instead")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "running-turn" {
		t.Fatalf("killed = %v, want the running turn interrupted", killed)
	}
}

func TestCommitEditThenAnInterruptVerdictAgainstAQueuedPromptCoalesces(t *testing.T) {
	// Arrange: t1's edit is ruled to interrupt t0, which is still queued.
	h := newHarness(t)
	queuedBehind(t, h, "t0", "t1")
	beginEdit(t, h, "t1")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "a correction"}
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("stop, do this instead")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()

	// Act
	turnEnds(h)

	// Assert: t0 carries t1 and is the one turn delivered.
	if started := h.sender.started(); len(started) != 1 || started[0] != "t0" {
		t.Fatalf("started = %v, want t0 carrying the folded t1", started)
	}
}
func TestCommitEditWithNothingRunningDeliversInPlace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	turnEnds(h)
	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("the edited words")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the edited prompt delivered", started)
	}
}

func TestAVerdictInFlightAcrossACommitIsDiscarded(t *testing.T) {
	// Arrange: the old content's verdict is still being reached when the
	// commit replaces the content; both verdicts would interject.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	release := h.judge.hold()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}
	if _, err := h.q.Submit(context.Background(), submission("t1", "the old words")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	beginEdit(t, h, "t1")
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("the new words")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	// Act
	release()
	h.q.waitForClassifications()
	// Assert
	if killed := h.sender.killed(); len(killed) != 1 {
		t.Fatalf("killed = %v, want exactly one interrupt, from the new content's verdict", killed)
	}
	if !logged(h.log.Records(), "info", opClassify, "the verdict is about content an edit has since replaced or a move has since superseded; it is discarded") {
		t.Fatalf("records = %+v, want the stale verdict's discard at info", h.log.Records())
	}
}

func TestCommitEditThatTheStoreRefusesLeavesTheEditStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	h.db.replaceErr = errors.New("disk full")
	// Act
	err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("the edited words"))
	// Assert
	if err == nil {
		t.Fatal("CommitEdit succeeded, want the store's refusal")
	}
	if _, ok := h.q.Editing(theWorkspace); !ok {
		t.Fatal("the claim was retired although the content was not recorded")
	}
	if !logged(h.log.Records(), "error", opEditCommit, "the edited content could not be recorded; the edit stands") {
		t.Fatalf("records = %+v, want the failure at error", h.log.Records())
	}
}

func TestCommitEditWithNoContentIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	// Act
	err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", nil)
	// Assert
	if err == nil {
		t.Fatal("CommitEdit(nil) succeeded, want a refusal")
	}
	if !logged(h.log.Records(), "error", opEditCommit, "the commit carried no content; the edit stands") {
		t.Fatalf("records = %+v, want the refusal at error", h.log.Records())
	}
}

func TestCancelEditKeepsTheContent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	// Act
	if err := h.q.CancelEdit(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("CancelEdit: %v", err)
	}
	// Assert
	if got := saidText(h.db.hold("t1").Said); got != "the held prompt" {
		t.Fatalf("said = %q, want the content unchanged", got)
	}
	if _, ok := h.q.Editing(theWorkspace); ok {
		t.Fatal("the claim survived the cancel")
	}
}

func TestCancelEditWithNothingRunningResumesTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	turnEnds(h)
	// Act
	if err := h.q.CancelEdit(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("CancelEdit: %v", err)
	}
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the queue resumed with t1", started)
	}
}

func TestEditorGoneReleasesTheClaim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	// Act
	h.q.EditorGone(theWorkspace)
	// Assert
	if _, ok := h.q.Editing(theWorkspace); ok {
		t.Fatal("the claim survived its editor's departure")
	}
	if !logged(h.log.Records(), "info", opEditRelease,
		"the editor's host stream ended; the edit is released with the content unchanged") {
		t.Fatalf("records = %+v, want the release at info", h.log.Records())
	}
}

func TestEditorGoneWithNothingRunningResumesTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	turnEnds(h)
	// Act
	h.q.EditorGone(theWorkspace)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the queue resumed with t1", started)
	}
}

func TestEditorGoneWithNoEditStandingIsANoOp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	// Act
	h.q.EditorGone(theWorkspace)
	// Assert
	if marks := h.holds.editingMarks(); len(marks) != 0 {
		t.Fatalf("tray editing marks = %v, want none", marks)
	}
}

func TestEditorGoneWithNoEditStandingReadsNothingAndRecordsNothing(t *testing.T) {
	// Arrange: the state store is already gone, as on an orderly exit.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.workspaces, theWorkspace)
	h.db.mu.Unlock()
	// Act
	h.q.EditorGone(theWorkspace)
	// Assert
	for _, r := range h.log.Records() {
		if r.Level == "error" {
			t.Fatalf("records = %+v, want no error for an editor leaving with no edit", h.log.Records())
		}
	}
}

func TestDroppingTheEditedPromptReleasesTheClaim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t1")
	// Act
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}
	// Assert
	if _, ok := h.q.Editing(theWorkspace); ok {
		t.Fatal("the claim survived its prompt's drop")
	}
}

func TestEditRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		act     func(h *harness) error
		want    error
		op      string
		message string
	}{
		{
			name:    "a begin on a turn nothing was held under",
			arrange: func(t *testing.T, h *harness) { queuedBehind(t, h, "t1") },
			act: func(h *harness) error {
				return h.q.BeginEdit(context.Background(), theWorkspace, "never-held", editorUp)
			},
			want: ErrNoSuchHold, op: opEditBegin,
			message: "the edit is refused: no hold was ever recorded under the turn",
		},
		{
			name: "a begin on a dropped prompt",
			arrange: func(t *testing.T, h *harness) {
				queuedBehind(t, h, "t1")
				if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
					t.Fatalf("Drop: %v", err)
				}
			},
			act: func(h *harness) error {
				return h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp)
			},
			want: ErrNotHeld, op: opEditBegin,
			message: "the edit is refused: the prompt is no longer held",
		},
		{
			name: "a begin on a delivered prompt",
			arrange: func(t *testing.T, h *harness) {
				queuedBehind(t, h, "t1")
				turnEnds(h)
			},
			act: func(h *harness) error {
				return h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp)
			},
			want: ErrAlreadyDelivered, op: opEditBegin,
			message: "the edit is refused: the prompt was already delivered",
		},
		{
			name: "a begin while another prompt is being edited",
			arrange: func(t *testing.T, h *harness) {
				queuedBehind(t, h, "t1", "t2")
				beginEdit(t, h, "t1")
			},
			act: func(h *harness) error {
				return h.q.BeginEdit(context.Background(), theWorkspace, "t2", editorUp)
			},
			want: ErrBeingEdited, op: opEditBegin,
			message: "the edit is refused: an edit already stands on this workspace",
		},
		{
			name:    "a begin with no editor attached",
			arrange: func(t *testing.T, h *harness) { queuedBehind(t, h, "t1") },
			act: func(h *harness) error {
				return h.q.BeginEdit(context.Background(), theWorkspace, "t1", func() bool { return false })
			},
			want: ErrNoEditor, op: opEditBegin,
			message: "the edit is refused: no editor's host stream stands for the workspace",
		},
		{
			name:    "a commit with no edit standing",
			arrange: func(t *testing.T, h *harness) { queuedBehind(t, h, "t1") },
			act: func(h *harness) error {
				return h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("words"))
			},
			want: ErrNotEditing, op: opEditCommit,
			message: "the step is refused: no edit stands on this prompt",
		},
		{
			name: "a cancel naming a prompt other than the edited one",
			arrange: func(t *testing.T, h *harness) {
				queuedBehind(t, h, "t1", "t2")
				beginEdit(t, h, "t1")
			},
			act: func(h *harness) error {
				return h.q.CancelEdit(context.Background(), theWorkspace, "t2")
			},
			want: ErrNotEditing, op: opEditCancel,
			message: "the step is refused: no edit stands on this prompt",
		},
		{
			name: "a commit after the edited prompt was delivered",
			arrange: func(t *testing.T, h *harness) {
				queuedBehind(t, h, "t1")
				turnEnds(h)
			},
			act: func(h *harness) error {
				return h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("words"))
			},
			want: ErrAlreadyDelivered, op: opEditCommit,
			message: "the edit is refused: the prompt was already delivered",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(t, h)
			// Act
			err := tt.act(h)
			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("err = %v, want %v", err, tt.want)
			}
			if !logged(h.log.Records(), "info", tt.op, tt.message) {
				t.Fatalf("records = %+v, want %q at info under %s", h.log.Records(), tt.message, tt.op)
			}
		})
	}
}

func TestBeingEditedNamesTheTurnBeingEdited(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t1")
	// Act
	err := h.q.BeginEdit(context.Background(), theWorkspace, "t2", editorUp)
	// Assert
	var being *BeingEditedError
	if !errors.As(err, &being) || being.Turn != "t1" {
		t.Fatalf("err = %v, want a BeingEditedError naming t1", err)
	}
}

func TestBeginEditThatCannotReadTheHoldIsLoggedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	h.db.byTurnErr = errors.New("database is locked")
	// Act
	err := h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp)
	// Assert
	if err == nil {
		t.Fatal("BeginEdit succeeded, want the read failure")
	}
	if !logged(h.log.Records(), "error", opEditBegin, "could not read the hold the edit names") {
		t.Fatalf("records = %+v, want the read failure at error", h.log.Records())
	}
}

// firstTextOf answers an edit's content as text.
func firstTextOf(e Edit) string { return saidText(e.Said) }

func TestCommitEditIntoASessionActNeverReachesTheClassifier(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	beginEdit(t, h, "t1")
	asked := len(h.judge.questions())
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "a correction"}
	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("/compact keep the plan")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if now := len(h.judge.questions()); now != asked {
		t.Fatalf("classifier asked %d more times, want an edited-in /compact never classified", now-asked)
	}
}
