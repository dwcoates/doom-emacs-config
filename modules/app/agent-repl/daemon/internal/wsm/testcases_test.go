package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"slices"
	"testing"

	"google.golang.org/protobuf/proto"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/testcases"
)

const casePkg = "modules/app/agent-repl/daemon/internal/merge"

// mustRecordTestCases records CASES for WS, failing the test on any error.
func mustRecordTestCases(t *testing.T, s *store, ws WorkspaceID, at int64, cases ...*testrunv1.TestSelection) {
	t.Helper()
	if err := s.RecordTestCases(context.Background(), ws, cases, at); err != nil {
		t.Fatalf("RecordTestCases: %v", err)
	}
}

// testCasesOf loads WS's record, failing the test on an error.
func testCasesOf(t *testing.T, s *store, ws WorkspaceID) []*testrunv1.TestSelection {
	t.Helper()
	got, err := s.TestCasesOf(context.Background(), ws)
	if err != nil {
		t.Fatalf("TestCasesOf: %v", err)
	}
	return got
}

func sameCases(a, b []*testrunv1.TestSelection) bool {
	return slices.EqualFunc(a, b, func(x, y *testrunv1.TestSelection) bool { return proto.Equal(x, y) })
}

func TestACaseRecordedTwiceIsOneCaseInTheOrderFirstRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	a, b := testcases.GoPackage(casePkg, []string{"TestA"}), testcases.Suite("daemon")
	mustRecordTestCases(t, s, ws.ID, 1, a)
	mustRecordTestCases(t, s, ws.ID, 2, b, a)

	// Act
	got := testCasesOf(t, s, ws.ID)

	// Assert
	if want := []*testrunv1.TestSelection{a, b}; !sameCases(got, want) {
		t.Fatalf("TestCasesOf = %v, want %v", got, want)
	}
}

func TestAWorkspacesCasesAreItsOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws, other := testWorkspace(t, s), testWorkspaceNamed(t, s, "other")
	mustRecordTestCases(t, s, other.ID, 1, testcases.Suite("daemon"))

	// Act
	got := testCasesOf(t, s, ws.ID)

	// Assert
	if len(got) != 0 {
		t.Fatalf("TestCasesOf = %v, want none of another workspace's", got)
	}
}

func TestTheRecordOutlivesARestart(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatal(err)
	}
	s := handle.(*store)
	ws := testWorkspace(t, s)
	c := testcases.ErtFile("test-wire.el", []string{"test-wire-decodes"})
	mustRecordTestCases(t, s, ws.ID, 1, c)
	if err := handle.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	reopened, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatal(err)
	}
	defer reopened.Close()
	got := testCasesOf(t, reopened.(*store), ws.ID)

	// Assert
	if want := []*testrunv1.TestSelection{c}; !sameCases(got, want) {
		t.Fatalf("TestCasesOf after reopening = %v, want %v", got, want)
	}
}

func TestTheRecordFollowsItsWorkspacesLifetime(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustRecordTestCases(t, s, ws.ID, 1, testcases.Suite("daemon"))

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_test_cases`); n != 0 {
		t.Fatalf("%d case rows outlived their workspace, want none", n)
	}
}

func TestForgettingCasesAnswersHowManyStood(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	a, b := testcases.GoPackage(casePkg, []string{"TestA"}), testcases.GoPackage(casePkg, []string{"TestB"})
	mustRecordTestCases(t, s, ws.ID, 1, a, b)

	// Act
	forgot, err := s.ForgetTestCases(context.Background(), ws.ID, []*testrunv1.TestSelection{a, testcases.Suite("never-recorded")})

	// Assert
	if err != nil || forgot != 1 {
		t.Fatalf("ForgetTestCases = %d, %v; want 1 forgotten", forgot, err)
	}
	if got := testCasesOf(t, s, ws.ID); !sameCases(got, []*testrunv1.TestSelection{b}) {
		t.Fatalf("TestCasesOf = %v, want only %v left", got, b)
	}
}

func TestRecordingForAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RecordTestCases(context.Background(), "ws-gone", []*testrunv1.TestSelection{testcases.Suite("daemon")}, 1)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RecordTestCases = %v, want ErrNotFound", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_test_cases`); n != 0 {
		t.Fatalf("%d case rows stand, want none", n)
	}
}

func TestRecordingOrForgettingNoCaseIsRefused(t *testing.T) {
	tests := []struct {
		name string
		op   string
		call func(s *store, ws WorkspaceID) error
	}{
		{name: "recording none", op: "daemon.wsm.record_test_cases", call: func(s *store, ws WorkspaceID) error {
			return s.RecordTestCases(context.Background(), ws, nil, 1)
		}},
		{name: "forgetting none", op: "daemon.wsm.forget_test_cases", call: func(s *store, ws WorkspaceID) error {
			_, err := s.ForgetTestCases(context.Background(), ws, nil)
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := tt.call(s, ws.ID)

			// Assert
			if !errors.Is(err, errNoCases) {
				t.Fatalf("err = %v, want errNoCases", err)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: tt.op})
			if !ok || rec.Context["workspace"] != string(ws.ID) {
				t.Fatalf("no error record naming the workspace: %+v", log.Records())
			}
		})
	}
}

func TestReadingTheRecordFailsWholeOnACorruptRow(t *testing.T) {
	// Arrange
	s, sink := testStore(t)
	ws := testWorkspace(t, s)
	mustRecordTestCases(t, s, ws.ID, 1, testcases.Suite("daemon"), testcases.Suite("ert"))
	corrupt(t, s, `UPDATE workspace_test_cases SET selection = x'ff' WHERE case_key LIKE 'suite%ert'`)

	// Act
	got, err := s.TestCasesOf(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "workspace_test_cases" || got != nil {
		t.Fatalf("TestCasesOf = %v, %v; want a DecodeError on workspace_test_cases and nothing read", got, err)
	}
	if !loggedOperation(sink, "daemon.wsm.test_cases_of", "error") {
		t.Fatalf("the failed read was not recorded at ERROR: %v", sink.Records())
	}
}

func TestTheTestCasesStepIsAdditiveAndCreatesItsTable(t *testing.T) {
	// Arrange
	step := migrationTo(t, 190)
	path := fixtureAt(t, 189)

	// Act
	execRaw(t, path, step.DDL)

	// Assert
	if step.Name != "workspace_test_cases" || step.Kind != MigrationAdditive {
		t.Fatalf("migration 190 = %+v, want the workspace test cases, additive", step)
	}
	if got := rawScalar[int](t, path, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'workspace_test_cases'`); got != 1 {
		t.Fatalf("table workspace_test_cases exists %d times, want once", got)
	}
}
