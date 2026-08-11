package inflight

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func completedAlways(Item) (bool, string, error) {
	return true, "a terminal result is recorded", nil
}

func completedNever(Item) (bool, string, error) {
	return false, "no terminal result exists for this identity", nil
}

// TestReconcileDispositions covers each disposition on its own edge.
func TestReconcileDispositions(t *testing.T) {
	item := Item{Kind: KindTask, ID: "task-1"}
	tests := []struct {
		name   string
		before Set
		after  Set
		oracle CompletionOracle
		want   string
	}{
		{
			name:   "an identity still live is preserved",
			before: MustAnswered("/w", item),
			after:  MustAnswered("/w", item),
			oracle: completedNever,
			want:   DispositionPreserved,
		},
		{
			name:   "a vanished item with a recorded terminal result is completed",
			before: MustAnswered("/w", item),
			after:  MustAnswered("/w"),
			oracle: completedAlways,
			want:   DispositionCompleted,
		},
		{
			name:   "a vanished item with no terminal result is interrupted",
			before: MustAnswered("/w", item),
			after:  MustAnswered("/w"),
			oracle: completedNever,
			want:   DispositionInterrupted,
		},
		{
			name:   "an unknown after set judges every item unknown",
			before: MustAnswered("/w", item),
			after:  Unanswered("/w", "the shim never handshook again"),
			oracle: completedAlways,
			want:   DispositionUnknown,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := Reconcile(tt.before, tt.after, tt.oracle)
			// Assert
			if err != nil {
				t.Fatalf("Reconcile: %v", err)
			}
			if len(got) != 1 {
				t.Fatalf("Reconcile returned %d judgements, want 1", len(got))
			}
			if got[0].Disposition != tt.want {
				t.Fatalf("disposition = %s, want %s (reason %q)", got[0].Disposition, tt.want, got[0].Reason)
			}
			if got[0].Reason == "" {
				t.Fatal("a judgement carries no reason; a verdict that cannot say why is the account this mechanism refuses")
			}
		})
	}
}

// TestReconcileFoldsAnUnreadableOracleIntoUnknownNotCompleted is the rule the
// whole manifest turns on: a fate nobody could observe is never a completion.
func TestReconcileFoldsAnUnreadableOracleIntoUnknown(t *testing.T) {
	// Arrange
	item := Item{Kind: KindTurn, ID: "turn-1"}
	oracle := func(Item) (bool, string, error) {
		return false, "", errors.New("the turn ledger could not be read")
	}
	// Act
	got, err := Reconcile(MustAnswered("/w", item), MustAnswered("/w"), oracle)
	// Assert
	if err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	if got[0].Disposition != DispositionUnknown {
		t.Fatalf("disposition = %s, want UNKNOWN for an unreadable oracle", got[0].Disposition)
	}
}

// TestReconcileRefusesAnUnknownBeforeSet pins that a bounce whose pre-state was
// never observed makes NO claim, rather than an empty one that would read as
// "nothing was hurt".
func TestReconcileRefusesAnUnknownBeforeSet(t *testing.T) {
	// Arrange / Act
	_, err := Reconcile(Unanswered("/w", "the daemon had no controller"), MustAnswered("/w"), completedAlways)
	// Assert
	if !errors.Is(err, ErrBeforeUnknown) {
		t.Fatalf("Reconcile err = %v, want ErrBeforeUnknown", err)
	}
}

// TestReconcileRequiresAnOracle covers the construction refusal.
func TestReconcileRequiresAnOracle(t *testing.T) {
	// Arrange / Act
	_, err := Reconcile(MustAnswered("/w"), MustAnswered("/w"), nil)
	// Assert
	if err == nil {
		t.Fatal("Reconcile accepted a nil oracle; a vanished item could not then be told from a finished one")
	}
}

// TestTallyCountsEachDisposition covers the human summary.
func TestTallyCountsEachDisposition(t *testing.T) {
	// Arrange
	judgements := []Judgement{
		{Disposition: DispositionPreserved},
		{Disposition: DispositionInterrupted},
		{Disposition: DispositionInterrupted},
	}
	// Act
	got := Tally(judgements)
	// Assert
	if got[DispositionInterrupted] != 2 || got[DispositionPreserved] != 1 {
		t.Fatalf("Tally = %v, want two interrupted and one preserved", got)
	}
}

// TestManifestSurvivesTheBounceItDescribes is the observability requirement in
// executable form: a START record written by one process and an END record
// written by a LATER one land in the same readable file, in order.
func TestManifestSurvivesTheBounceItDescribes(t *testing.T) {
	// Arrange: an outgoing daemon's start record.
	ws := t.TempDir()
	item := Item{Kind: KindTask, ID: "task-1"}
	if err := Append(ws, Record{Phase: PhaseStart, BounceID: "b1", Cause: "deploy", Known: true, Items: []Item{item}}); err != nil {
		t.Fatalf("Append start: %v", err)
	}
	// Act: a separate later write, standing in for the incoming daemon — the
	// point being that no descriptor is held across the two.
	judgements, err := Reconcile(MustAnswered(ws, item), MustAnswered(ws), completedNever)
	if err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	if err := Append(ws, Record{Phase: PhaseEnd, BounceID: "b1", Cause: "deploy", Known: true, Judgements: judgements, Tally: Tally(judgements)}); err != nil {
		t.Fatalf("Append end: %v", err)
	}
	// Assert
	got, err := Read(ws)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("Read returned %d records, want the start and the end", len(got))
	}
	if got[0].Phase != PhaseStart || got[1].Phase != PhaseEnd {
		t.Fatalf("phases = %s,%s want start,end", got[0].Phase, got[1].Phase)
	}
	if got[1].Judgements[0].Disposition != DispositionInterrupted {
		t.Fatalf("end disposition = %s, want INTERRUPTED", got[1].Judgements[0].Disposition)
	}
}

// TestManifestPathIsNotTheRepointedSymlink pins the manifest beside the log
// links rather than through one: the daemon re-points daemon.log on every
// restart, which is exactly the event the manifest has to outlive.
func TestManifestPathIsAPlainFileBesideTheLogLinks(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	// Act
	if err := Append(ws, Record{Phase: PhaseStart, BounceID: "b", Cause: "c", Known: true}); err != nil {
		t.Fatalf("Append: %v", err)
	}
	// Assert
	info, err := os.Lstat(ManifestPath(ws))
	if err != nil {
		t.Fatalf("Lstat: %v", err)
	}
	if info.Mode()&os.ModeSymlink != 0 {
		t.Fatal("the manifest is a symlink; a re-pointed link is what makes a bounce's own record unreachable afterwards")
	}
	if filepath.Base(ManifestPath(ws)) != ManifestFileName {
		t.Fatalf("manifest basename = %q, want %q", filepath.Base(ManifestPath(ws)), ManifestFileName)
	}
}

// TestAppendRefusesToFollowASymlink covers the logging contract's rule that a
// runtime never follows a workspace-provided link as a durable sink.
func TestAppendRefusesToFollowASymlink(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	elsewhere := filepath.Join(t.TempDir(), "stolen.jsonl")
	if err := os.MkdirAll(filepath.Dir(ManifestPath(ws)), 0o700); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.Symlink(elsewhere, ManifestPath(ws)); err != nil {
		t.Fatalf("Symlink: %v", err)
	}
	// Act
	err := Append(ws, Record{Phase: PhaseStart, BounceID: "b", Cause: "c", Known: true})
	// Assert
	if err == nil {
		t.Fatal("Append followed a symlink out of the workspace")
	}
	if _, statErr := os.Stat(elsewhere); statErr == nil {
		t.Fatal("Append created the symlink's target; the sink escaped the workspace")
	}
}

// TestAppendRequiresAWorkspace covers the validation refusal.
func TestAppendRequiresAWorkspace(t *testing.T) {
	// Arrange / Act
	err := Append("", Record{Phase: PhaseStart})
	// Assert
	if err == nil {
		t.Fatal("Append accepted an empty workspace directory")
	}
}

// TestReadOfAMissingManifestIsEmptyNotAnError covers a workspace that has never
// been bounced.
func TestReadOfAMissingManifestIsEmpty(t *testing.T) {
	// Arrange / Act
	got, err := Read(t.TempDir())
	// Assert
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("Read = %v, want no records", got)
	}
}

// TestTimestampIsTheSharedRepresentation pins the six-fractional-digit,
// numeric-offset form every agent-repl runtime shares, so manifest lines sort
// lexically beside log lines.
func TestTimestampIsTheSharedRepresentation(t *testing.T) {
	// Arrange
	rec := Record{Phase: PhaseStart, BounceID: "b", Cause: "c", Known: true}
	ws := t.TempDir()
	// Act
	if err := Append(ws, rec); err != nil {
		t.Fatalf("Append: %v", err)
	}
	got, err := Read(ws)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	// Assert
	ts := got[0].Timestamp
	dot := strings.LastIndex(ts, ".")
	if dot == -1 || len(ts) < dot+7 {
		t.Fatalf("timestamp %q has no fractional field", ts)
	}
	if frac := ts[dot+1 : dot+7]; len(frac) != 6 {
		t.Fatalf("timestamp %q fractional field = %q, want exactly six digits", ts, frac)
	}
	if strings.HasSuffix(ts, "Z") {
		t.Fatalf("timestamp %q is UTC-suffixed; the shared representation uses a numeric local offset", ts)
	}
}
