package wsm

import (
	"context"
	"database/sql"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// githubKinds is every GitHub subscription kind the shared helpers serve. A
// kind added without its entry here goes unchecked by
// TestEveryGithubKindsTablesFitTheSharedHelpers.
var githubKinds = []githubTables{pullRequestTables, githubMergeQueueTables}

func TestKeyWhereMatchesEveryKeyColumn(t *testing.T) {
	// Act
	got := pullRequestTables.keyWhere()

	// Assert
	if want := "owner = ? AND repo = ? AND number = ?"; got != want {
		t.Fatalf("keyWhere = %q, want %q", got, want)
	}
}

// columnsOf answers TABLE's columns in a fresh store.
func columnsOf(t *testing.T, s *store, table string) map[string]bool {
	t.Helper()
	names, err := listRows(context.Background(), s.q(context.Background()), `SELECT name FROM pragma_table_info(?)`,
		func(row interface{ Scan(...any) error }) (string, error) {
			var name string
			return name, row.Scan(&name)
		}, table)
	if err != nil || len(names) == 0 {
		t.Fatalf("columns of %s = %v, %v", table, names, err)
	}
	out := map[string]bool{}
	for _, n := range names {
		out[n] = true
	}
	return out
}

func TestEveryGithubKindsTablesFitTheSharedHelpers(t *testing.T) {
	for _, kind := range githubKinds {
		t.Run(kind.noun, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			wantSubs := append([]string{"subscriber", "told", "last_read", "failing_since", "failure_kind", "failure_detail", "failure_resets_at"}, kind.key...)
			wantNotices := []string{"subscriber", kind.subject, "turn_id", "sentences", "pending", "status"}

			// Act
			subs, notices := columnsOf(t, s, kind.subscriptions), columnsOf(t, s, kind.notices)

			// Assert
			for _, c := range wantSubs {
				if !subs[c] {
					t.Fatalf("%s has no column %s: %v", kind.subscriptions, c, subs)
				}
			}
			for _, c := range wantNotices {
				if !notices[c] {
					t.Fatalf("%s has no column %s: %v", kind.notices, c, notices)
				}
			}
		})
	}
}

func TestOwedGithubNoticesCarryWhatWasAppendedUntilSubmitted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	appendOwed := func(sentences []string, status string) {
		if err := s.write(ctx, "test", nil, func(ctx context.Context, tx *sql.Tx) error {
			return appendOwedInTx(ctx, tx, pullRequestTables, ws.ID, "o/r#1", sentences, status)
		}); err != nil {
			t.Fatal(err)
		}
	}
	appendOwed([]string{"one."}, "open")
	appendOwed([]string{"two."}, "")

	// Act
	before, errBefore := s.owedGithubNotices(ctx, "test", pullRequestTables)
	errSubmit := s.recordGithubNoticeSubmitted(ctx, "test", pullRequestTables, ws.ID, "o/r#1", "turn-1", []string{"one.", "two."}, 2)
	after, errAfter := s.owedGithubNotices(ctx, "test", pullRequestTables)

	// Assert
	want := []githubOwed{{Subscriber: ws.ID, Subject: "o/r#1", Sentences: []string{}, Pending: []string{"one.", "two."}, Status: "open"}}
	if errBefore != nil || !reflect.DeepEqual(before, want) {
		t.Fatalf("owed before = %+v, %v; want %+v", before, errBefore, want)
	}
	if errSubmit != nil || errAfter != nil || len(after) != 0 {
		t.Fatalf("submitted = %v, owed after = %+v, %v; want nothing owed", errSubmit, after, errAfter)
	}
}

func TestRecordGithubNoticeSubmittedRefuses(t *testing.T) {
	tests := []struct {
		name      string
		sentences []string
		took      int
		wantError string
		wantLog   string
	}{
		{name: "a notice saying nothing", sentences: nil, took: 0, wantError: "says no change", wantLog: "refused an untold pull-request notice that says no change"},
		{name: "a notice taking more than is pending", sentences: []string{"one."}, took: 2, wantError: "took 2 pending sentences of 1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			ctx := context.Background()
			if err := s.write(ctx, "test", nil, func(ctx context.Context, tx *sql.Tx) error {
				return appendOwedInTx(ctx, tx, pullRequestTables, ws.ID, "o/r#1", []string{"one."}, "")
			}); err != nil {
				t.Fatal(err)
			}

			// Act
			err := s.recordGithubNoticeSubmitted(ctx, "daemon.wsm.test", pullRequestTables, ws.ID, "o/r#1", "turn-1", tt.sentences, tt.took)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantError) {
				t.Fatalf("err = %v, want one saying %q", err, tt.wantError)
			}
			if tt.wantLog != "" {
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.test", Message: tt.wantLog}); !ok {
					t.Fatalf("no record %q: %+v", tt.wantLog, log.Records())
				}
			}
			owed, _ := s.owedGithubNotices(ctx, "test", pullRequestTables)
			if len(owed) != 1 || !reflect.DeepEqual(owed[0].Pending, []string{"one."}) {
				t.Fatalf("owed = %+v, want the pending sentence untouched", owed)
			}
		})
	}
}

func TestScanGithubHealth(t *testing.T) {
	at := time.Unix(1_800_000_000, 0).UTC()
	tests := []struct {
		name        string
		lastRead    sql.NullInt64
		since       sql.NullInt64
		kind        sql.NullString
		resets      sql.NullInt64
		wantRead    time.Time
		wantFailing *PullRequestFailure
	}{
		{name: "never read and never failing"},
		{name: "read and healthy", lastRead: sql.NullInt64{Int64: nanos(at), Valid: true}, wantRead: at},
		{name: "failing on a spent rate limit", since: sql.NullInt64{Int64: nanos(at), Valid: true}, kind: sql.NullString{String: "rate_limited", Valid: true},
			resets:      sql.NullInt64{Int64: nanos(at.Add(time.Hour)), Valid: true},
			wantFailing: &PullRequestFailure{Since: at, Kind: PullRequestFailureRateLimited, ResetsAt: at.Add(time.Hour)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			read, failing := scanGithubHealth(tt.lastRead, tt.since, tt.kind, sql.NullString{}, tt.resets)

			// Assert
			if !read.Equal(tt.wantRead) || !reflect.DeepEqual(failing, tt.wantFailing) {
				t.Fatalf("health = %v, %+v; want %v, %+v", read, failing, tt.wantRead, tt.wantFailing)
			}
		})
	}
}
