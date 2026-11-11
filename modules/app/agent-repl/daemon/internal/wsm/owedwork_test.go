package wsm

import (
	"context"
	"testing"
)

// boundSession answers workspace WS's recorded session, for a bind to rewrite.
func boundSession(t *testing.T, s *store, ws Workspace) Session {
	t.Helper()
	sess, found, err := s.Session(context.Background(), ws.ID)
	if err != nil || !found {
		t.Fatalf("Session = (%v, %v)", found, err)
	}
	return sess
}

func TestABindRecordsTheWorkItOwes(t *testing.T) {
	cases := []struct {
		name string
		bind func(*store, Session) error
		want OwedStartWork
	}{
		{
			name: "an own fork owing a compaction",
			bind: func(s *store, sess Session) error {
				sess.VendorSessionID = ""
				return s.BindSessionToOwnFork(context.Background(), sess, "vendor-queue", &OwedStartWork{Compact: true})
			},
			want: OwedStartWork{Compact: true},
		},
		{
			name: "a resume owing a summary of another conversation",
			bind: func(s *store, sess Session) error {
				sess.VendorSessionID = "vendor-bypass"
				return s.BindSession(context.Background(), sess, &OwedStartWork{SummaryOf: "vendor-queue"})
			},
			want: OwedStartWork{SummaryOf: "vendor-queue"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _, ws := ownForkStore(t)

			// Act
			err := tc.bind(s, boundSession(t, s, ws))

			// Assert
			if err != nil {
				t.Fatalf("bind: %v", err)
			}
			got, found, err := s.OwedStartWork(context.Background(), ws.ID)
			if err != nil || !found || got != tc.want {
				t.Fatalf("OwedStartWork = (%+v, %v, %v), want %+v", got, found, err, tc.want)
			}
		})
	}
}

func TestABindForgetsTheWorkAndTheSummaryThePreviousBindingOwed(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	sess := boundSession(t, s, ws)
	if err := s.BindSession(ctx, sess, &OwedStartWork{SummaryOf: "vendor-queue"}); err != nil {
		t.Fatalf("BindSession: %v", err)
	}
	if err := s.SettleOwedStartWork(ctx, ws.ID, "the summary"); err != nil {
		t.Fatalf("SettleOwedStartWork: %v", err)
	}
	if err := s.BindSession(ctx, sess, &OwedStartWork{Compact: true}); err != nil {
		t.Fatalf("BindSession: %v", err)
	}

	// Act
	err := s.BindSession(ctx, sess, nil)

	// Assert
	if err != nil {
		t.Fatalf("BindSession: %v", err)
	}
	if _, found, _ := s.OwedStartWork(ctx, ws.ID); found {
		t.Fatal("the owed work outlived the binding it belonged to")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM owed_context_summaries`); n != 0 {
		t.Fatalf("owed_context_summaries holds %d rows after a rebind", n)
	}
}

func TestABindRefusesMalformedOwedWorkAndWritesNothing(t *testing.T) {
	cases := []struct {
		name string
		owed OwedStartWork
	}{
		{name: "naming no work", owed: OwedStartWork{}},
		{name: "naming both kinds of work", owed: OwedStartWork{Compact: true, SummaryOf: "vendor-queue"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs, ws := ownForkStore(t)
			sess := boundSession(t, s, ws)
			sess.VendorSessionID = "vendor-bypass"

			// Act
			err := s.BindSession(context.Background(), sess, &tc.owed)

			// Assert
			if err == nil {
				t.Fatal("BindSession accepted it")
			}
			if got := scalar[string](t, s, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, ws.ID); got != "vendor-queue" {
				t.Fatalf("the session names %q after a refusal, want the binding it had", got)
			}
			if !loggedError(logs.Records(), "daemon.wsm.bind_session") {
				t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
			}
		})
	}
}

// owingSummary answers a store whose one workspace's binding owes a summary.
func owingSummary(t *testing.T) (*store, Workspace) {
	t.Helper()
	s, _, ws := ownForkStore(t)
	if err := s.BindSession(context.Background(), boundSession(t, s, ws), &OwedStartWork{SummaryOf: "vendor-queue"}); err != nil {
		t.Fatalf("BindSession: %v", err)
	}
	return s, ws
}

func TestSettlingOwedWorkRetiresItAndRecordsTheSummary(t *testing.T) {
	// Arrange
	s, ws := owingSummary(t)
	ctx := context.Background()

	// Act
	err := s.SettleOwedStartWork(ctx, ws.ID, "the summary")

	// Assert
	if err != nil {
		t.Fatalf("SettleOwedStartWork: %v", err)
	}
	if _, found, _ := s.OwedStartWork(ctx, ws.ID); found {
		t.Fatal("the settled work is still owed")
	}
	if got := scalar[string](t, s, `SELECT summary FROM owed_context_summaries WHERE workspace_id = ?`, ws.ID); got != "the summary" {
		t.Fatalf("owed summary = %q, want the settled one", got)
	}
}

func TestSettlingOwedWorkWithNoSummaryRecordsNone(t *testing.T) {
	// Arrange
	s, ws := owingSummary(t)

	// Act
	err := s.SettleOwedStartWork(context.Background(), ws.ID, "")

	// Assert
	if err != nil {
		t.Fatalf("SettleOwedStartWork: %v", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM owed_context_summaries`); n != 0 {
		t.Fatalf("owed_context_summaries holds %d rows, want none for a compaction", n)
	}
}

func TestSettlingWorkThatIsNotOwedIsRefusedAndRecordsNothing(t *testing.T) {
	// Arrange
	s, logs, ws := ownForkStore(t)

	// Act
	err := s.SettleOwedStartWork(context.Background(), ws.ID, "the summary")

	// Assert
	if err == nil {
		t.Fatal("SettleOwedStartWork settled work nobody owed")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM owed_context_summaries`); n != 0 {
		t.Fatalf("owed_context_summaries holds %d rows after a refusal", n)
	}
	if !loggedError(logs.Records(), "daemon.wsm.settle_owed_start_work") {
		t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
	}
}

// owingContextSummary answers a store whose one workspace's next prompt
// delivers SUMMARY.
func owingContextSummary(t *testing.T, summary string) (*store, Workspace) {
	t.Helper()
	s, ws := owingSummary(t)
	if err := s.SettleOwedStartWork(context.Background(), ws.ID, summary); err != nil {
		t.Fatalf("SettleOwedStartWork: %v", err)
	}
	return s, ws
}

func TestTakingTheContextSummaryAnswersItOnce(t *testing.T) {
	// Arrange
	s, ws := owingContextSummary(t, "the summary")
	ctx := context.Background()

	// Act
	first, firstFound, firstErr := s.TakeOwedContextSummary(ctx, ws.ID)
	_, secondFound, secondErr := s.TakeOwedContextSummary(ctx, ws.ID)

	// Assert
	if firstErr != nil || !firstFound || first != "the summary" {
		t.Fatalf("first take = (%q, %v, %v), want the summary", first, firstFound, firstErr)
	}
	if secondErr != nil || secondFound {
		t.Fatalf("second take = (%v, %v), want nothing left", secondFound, secondErr)
	}
}

func TestRestoringATakenSummaryPutsItBack(t *testing.T) {
	// Arrange
	s, ws := owingContextSummary(t, "the summary")
	ctx := context.Background()
	taken, _, _ := s.TakeOwedContextSummary(ctx, ws.ID)

	// Act
	err := s.RestoreOwedContextSummary(ctx, ws.ID, taken)

	// Assert
	if err != nil {
		t.Fatalf("RestoreOwedContextSummary: %v", err)
	}
	if got, found, _ := s.TakeOwedContextSummary(ctx, ws.ID); !found || got != "the summary" {
		t.Fatalf("after the restore, take = (%q, %v), want the summary", got, found)
	}
}

func TestRestoringASummaryKeepsANewerOne(t *testing.T) {
	// Arrange
	s, ws := owingContextSummary(t, "the older summary")
	ctx := context.Background()
	taken, _, _ := s.TakeOwedContextSummary(ctx, ws.ID)
	if err := s.BindSession(ctx, boundSession(t, s, ws), &OwedStartWork{SummaryOf: "vendor-queue"}); err != nil {
		t.Fatalf("BindSession: %v", err)
	}
	if err := s.SettleOwedStartWork(ctx, ws.ID, "the newer summary"); err != nil {
		t.Fatalf("SettleOwedStartWork: %v", err)
	}

	// Act
	err := s.RestoreOwedContextSummary(ctx, ws.ID, taken)

	// Assert
	if err != nil {
		t.Fatalf("RestoreOwedContextSummary: %v", err)
	}
	if got := scalar[string](t, s, `SELECT summary FROM owed_context_summaries WHERE workspace_id = ?`, ws.ID); got != "the newer summary" {
		t.Fatalf("owed summary = %q, want the newer one kept", got)
	}
}

func TestRestoringAnEmptySummaryIsRefused(t *testing.T) {
	// Arrange
	s, logs, ws := ownForkStore(t)

	// Act
	err := s.RestoreOwedContextSummary(context.Background(), ws.ID, "")

	// Assert
	if err == nil {
		t.Fatal("RestoreOwedContextSummary accepted an empty summary")
	}
	if !loggedError(logs.Records(), "daemon.wsm.restore_owed_context_summary") {
		t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
	}
}
