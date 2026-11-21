package wsm

import (
	"context"
	"database/sql"
	"reflect"
	"strconv"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// statePR is the pull request the state tests link and read.
var statePR = PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 7}

// stateReadAt is the instant the state tests' reads are made at.
var stateReadAt = time.Date(2026, 11, 18, 12, 0, 0, 0, time.UTC)

// linkPR stores a reference link on WS naming PR, keyed as the verb keys it.
func linkPR(t *testing.T, s *store, ws WorkspaceID, pr PullRequestKey) {
	t.Helper()
	key := pr
	url := "https://github.com/" + pr.Owner + "/" + pr.Repo + "/pull/" + strconv.FormatUint(uint64(pr.Number), 10)
	if _, err := s.SetWorkspaceLink(context.Background(), ws, LinkSet{URL: url, Label: "The pull request", PullRequest: &key}); err != nil {
		t.Fatalf("SetWorkspaceLink: %v", err)
	}
}

// recordState records STATE for PR and fails the test on an error.
func recordState(t *testing.T, s *store, pr PullRequestKey, state PullRequestState) []WorkspaceID {
	t.Helper()
	got, err := s.RecordPullRequestState(context.Background(), pr, state, stateReadAt)
	if err != nil {
		t.Fatalf("RecordPullRequestState(%s, %s): %v", pr, state, err)
	}
	return got
}

func TestRecordPullRequestStateAnswersTheWorkspacesToRedraw(t *testing.T) {
	other := PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 8}
	tests := []struct {
		name  string
		prior []PullRequestState
		state PullRequestState
		want  func(linking, closed, unrelated Workspace) []WorkspaceID
	}{
		{name: "a first read answers every open workspace linking it", state: PullRequestStateOpen,
			want: func(linking, _, _ Workspace) []WorkspaceID { return []WorkspaceID{linking.ID} }},
		{name: "a state that stands answers none", prior: []PullRequestState{PullRequestStateOpen}, state: PullRequestStateOpen,
			want: func(Workspace, Workspace, Workspace) []WorkspaceID { return nil }},
		{name: "a changed state answers them again", prior: []PullRequestState{PullRequestStateDraft}, state: PullRequestStateMerged,
			want: func(linking, _, _ Workspace) []WorkspaceID { return []WorkspaceID{linking.ID} }},
		{name: "a reopened pull request answers them", prior: []PullRequestState{PullRequestStateOpen, PullRequestStateClosed}, state: PullRequestStateOpen,
			want: func(linking, _, _ Workspace) []WorkspaceID { return []WorkspaceID{linking.ID} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			linking := testWorkspaceNamed(t, s, "linking")
			closed := testWorkspaceNamed(t, s, "closed")
			unrelated := testWorkspaceNamed(t, s, "unrelated")
			linkPR(t, s, linking.ID, statePR)
			linkPR(t, s, closed.ID, statePR)
			linkPR(t, s, unrelated.ID, other)
			if err := s.MarkClosed(context.Background(), closed.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			for _, prior := range tt.prior {
				recordState(t, s, statePR, prior)
			}

			// Act
			got := recordState(t, s, statePR, tt.state)

			// Assert
			if want := tt.want(linking, closed, unrelated); !reflect.DeepEqual(got, want) {
				t.Fatalf("RecordPullRequestState = %v, want %v", got, want)
			}
			if stored := scalar[string](t, s, `SELECT state FROM pull_request_states WHERE number = 7`); stored != string(tt.state) {
				t.Fatalf("stored state = %q, want %q", stored, tt.state)
			}
		})
	}
}

func TestRecordPullRequestStateRefusesWhatNoReadFinds(t *testing.T) {
	tests := []struct {
		name  string
		pr    PullRequestKey
		state PullRequestState
	}{
		{name: "a state never read", pr: statePR, state: ""},
		{name: "a state GitHub has none of", pr: statePR, state: "locked"},
		{name: "a key missing its number", pr: PullRequestKey{Owner: "ChessCom", Repo: "doom"}, state: PullRequestStateOpen},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)

			// Act
			_, err := s.RecordPullRequestState(context.Background(), tt.pr, tt.state, stateReadAt)

			// Assert
			if err == nil {
				t.Fatal("RecordPullRequestState accepted it")
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.record_pull_request_state"}); !ok {
				t.Fatalf("no error record of the refusal: %+v", logs.Records())
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM pull_request_states`); n != 0 {
				t.Fatalf("%d states stored, want none", n)
			}
		})
	}
}

func TestLinkedPullRequestsAreTheOnesWhoseStateCanChange(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	a, b, gone := testWorkspaceNamed(t, s, "a"), testWorkspaceNamed(t, s, "b"), testWorkspaceNamed(t, s, "gone")
	unread, open, closed, merged, onlyGone := statePR, PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 8},
		PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 9}, PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 10},
		PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 11}
	for _, pr := range []PullRequestKey{unread, open, closed, merged} {
		linkPR(t, s, a.ID, pr)
	}
	linkPR(t, s, b.ID, open)
	linkPR(t, s, gone.ID, onlyGone)
	if _, err := s.SetWorkspaceLink(context.Background(), a.ID, LinkSet{URL: "https://example.com/doc", Label: "The design doc"}); err != nil {
		t.Fatalf("SetWorkspaceLink: %v", err)
	}
	if err := s.MarkClosed(context.Background(), gone.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	recordState(t, s, open, PullRequestStateOpen)
	recordState(t, s, closed, PullRequestStateClosed)
	recordState(t, s, merged, PullRequestStateMerged)

	// Act
	got, err := s.LinkedPullRequests(context.Background())

	// Assert: once each, merged and closed-workspace-only ones left out.
	if want := []PullRequestKey{unread, open, closed}; err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("LinkedPullRequests = %v, %v; want %v", got, err, want)
	}
}

func TestWorkspaceLinksCarryTheirPullRequestsState(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	unread := PullRequestKey{Owner: "ChessCom", Repo: "doom", Number: 8}
	linkPR(t, s, ws.ID, statePR)
	linkPR(t, s, ws.ID, unread)
	if _, err := s.SetWorkspaceLink(context.Background(), ws.ID, LinkSet{URL: "https://example.com/doc", Label: "The design doc"}); err != nil {
		t.Fatalf("SetWorkspaceLink: %v", err)
	}
	recordState(t, s, statePR, PullRequestStateMerged)

	// Act
	links, err := s.WorkspaceLinks(context.Background(), ws.ID)

	// Assert: most recently set first.
	if err != nil {
		t.Fatalf("WorkspaceLinks: %v", err)
	}
	var got []PullRequestState
	for _, l := range links {
		got = append(got, l.State)
	}
	if want := []PullRequestState{"", "", PullRequestStateMerged}; !reflect.DeepEqual(got, want) {
		t.Fatalf("states = %q, want %q", got, want)
	}
}

func TestTheLayout176RewriteKeysOldLinksAndSeedsEndedStates(t *testing.T) {
	// Arrange: a link stored before links recorded their pull request, one
	// that names none, and three ended subscriptions, two to one pull request.
	s, logs := testStore(t)
	ws, other := testWorkspaceNamed(t, s, "a"), testWorkspaceNamed(t, s, "b")
	const oldURL = "https://github.com/ChessCom/doom/pull/7/files"
	for _, set := range []LinkSet{{URL: oldURL, Label: "The old pull request"}, {URL: "https://example.com/doc", Label: "The design doc"}} {
		if _, err := s.SetWorkspaceLink(context.Background(), ws.ID, set); err != nil {
			t.Fatalf("SetWorkspaceLink: %v", err)
		}
	}
	ended := func(sub WorkspaceID, number int, origin, end string, lastRead any) {
		corrupt(t, s, `INSERT INTO pull_request_subscriptions (subscriber, owner, repo, number, events, origin, subscribed_at, told, ended, last_read)
			VALUES (?, 'ChessCom', 'doom', ?, 2, ?, 5, '', ?, ?)`, sub, number, origin, end, lastRead)
	}
	ended(ws.ID, 7, "owned_link", "closed", 20)
	ended(other.ID, 7, "requested", "merged", 10)
	ended(ws.ID, 9, "owned_link", "closed", nil)

	// Act
	err := s.write(context.Background(), "test", nil, func(ctx context.Context, tx *sql.Tx) error {
		return backfillPullRequestStates(ctx, tx, s.log)
	})

	// Assert
	if err != nil {
		t.Fatalf("backfillPullRequestStates: %v", err)
	}
	t.Run("the old link is keyed from its url", func(t *testing.T) {
		links, err := s.WorkspaceLinks(context.Background(), ws.ID)
		if err != nil {
			t.Fatalf("WorkspaceLinks: %v", err)
		}
		byURL := map[string]*PullRequestKey{}
		for _, l := range links {
			byURL[l.URL] = l.PullRequest
		}
		if got := byURL[oldURL]; got == nil || *got != statePR {
			t.Fatalf("old link's pull request = %v, want %v", got, statePR)
		}
		if got := byURL["https://example.com/doc"]; got != nil {
			t.Fatalf("the doc link names pull request %v, want none", got)
		}
	})
	t.Run("each ended pull request is seeded once, a merge winning", func(t *testing.T) {
		type seeded struct {
			Number int
			State  string
			ReadAt int64
		}
		got, err := listRows(context.Background(), s.q(context.Background()), `SELECT number, state, read_at FROM pull_request_states ORDER BY number`,
			func(row interface{ Scan(...any) error }) (seeded, error) {
				var r seeded
				err := row.Scan(&r.Number, &r.State, &r.ReadAt)
				return r, err
			})
		if want := []seeded{{7, "merged", 20}, {9, "closed", 5}}; err != nil || !reflect.DeepEqual(got, want) {
			t.Fatalf("seeded = %+v, %v; want %+v", got, err, want)
		}
	})
	t.Run("the rewrite records what it did", func(t *testing.T) {
		rec, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.wsm.migrate"})
		if !ok || rec.Context["links_keyed"] != 1 || rec.Context["states_seeded"] != int64(2) {
			t.Fatalf("record = %+v, %v; want 1 link keyed and 2 states seeded", rec, ok)
		}
	})
}
