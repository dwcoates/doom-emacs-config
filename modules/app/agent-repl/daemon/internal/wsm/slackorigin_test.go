package wsm

import (
	"context"
	"database/sql"
	"errors"
	"reflect"
	"testing"
)

const originURL = "https://w.slack.com/archives/C1/p1730000000123456"

func TestWorkspaceSlackOriginIsReadBack(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetWorkspaceSlackOrigin(context.Background(), ws.ID, originURL)

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceSlackOrigin: %v", err)
	}
	got, found, err := s.WorkspaceSlackOrigin(context.Background(), ws.ID)
	if err != nil || !found || got != originURL {
		t.Fatalf("WorkspaceSlackOrigin = %q, %v, %v; want the recorded thread", got, found, err)
	}
}

func TestWorkspaceSlackOriginOfAWorkspaceCreatedForNoneIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, found, err := s.WorkspaceSlackOrigin(context.Background(), ws.ID)

	// Assert
	if err != nil || found || got != "" {
		t.Fatalf("WorkspaceSlackOrigin = %q, %v, %v; want none", got, found, err)
	}
}

func TestWorkspaceSlackOriginRefusals(t *testing.T) {
	tests := []struct {
		name     string
		act      func(s *store, ws WorkspaceID) error
		notFound bool
	}{
		{name: "an origin with no thread is a defect", act: func(s *store, ws WorkspaceID) error {
			return s.SetWorkspaceSlackOrigin(context.Background(), ws, "")
		}},
		{name: "a second origin is refused", act: func(s *store, ws WorkspaceID) error {
			if err := s.SetWorkspaceSlackOrigin(context.Background(), ws, originURL); err != nil {
				t.Fatalf("first SetWorkspaceSlackOrigin: %v", err)
			}
			return s.SetWorkspaceSlackOrigin(context.Background(), ws, originURL+"9")
		}},
		{name: "an origin of an unknown workspace is not found", act: func(s *store, _ WorkspaceID) error {
			return s.SetWorkspaceSlackOrigin(context.Background(), WorkspaceID("absent"), originURL)
		}, notFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := tt.act(s, ws.ID)

			// Assert
			if err == nil {
				t.Fatal("err = nil, want a refusal")
			}
			if tt.notFound != errors.Is(err, ErrNotFound) {
				t.Fatalf("err = %v, want not-found %v", err, tt.notFound)
			}
		})
	}
}

func TestWorkspaceSlackOriginGoesWithItsWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.SetWorkspaceSlackOrigin(context.Background(), ws.ID, originURL); err != nil {
		t.Fatalf("SetWorkspaceSlackOrigin: %v", err)
	}

	// Act
	_, err := s.q(context.Background()).ExecContext(context.Background(), `DELETE FROM workspaces WHERE id = ?`, ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("delete the workspace: %v", err)
	}
	var count int
	if err := s.q(context.Background()).QueryRowContext(context.Background(), `SELECT COUNT(*) FROM workspace_slack_origins`).Scan(&count); err != nil || count != 0 {
		t.Fatalf("origins left = %d, %v; want none", count, err)
	}
}

const postedLink = "https://github.com/a/b/pull/7"

// originStore is a store holding one workspace created for originURL.
func originStore(t *testing.T) (*store, WorkspaceID) {
	t.Helper()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.SetWorkspaceSlackOrigin(context.Background(), ws.ID, originURL); err != nil {
		t.Fatalf("SetWorkspaceSlackOrigin: %v", err)
	}
	return s, ws.ID
}

func TestClaimSlackOpeningIsTakenOnce(t *testing.T) {
	// Arrange
	s, ws := originStore(t)

	// Act
	firstURL, first, err1 := s.ClaimSlackOpening(context.Background(), ws)
	secondURL, second, err2 := s.ClaimSlackOpening(context.Background(), ws)

	// Assert
	if err1 != nil || !first || firstURL != originURL {
		t.Fatalf("first claim = %q, %v, %v; want the thread claimed", firstURL, first, err1)
	}
	if err2 != nil || second || secondURL != "" {
		t.Fatalf("second claim = %q, %v, %v; want nothing claimed", secondURL, second, err2)
	}
}

func TestClaimSlackOpeningOfAWorkspaceCreatedForNoThreadClaimsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	url, claimed, err := s.ClaimSlackOpening(context.Background(), ws.ID)

	// Assert
	if err != nil || claimed || url != "" {
		t.Fatalf("claim = %q, %v, %v; want nothing claimed", url, claimed, err)
	}
}

func TestClaimSlackLinkPostIsTakenOncePerLink(t *testing.T) {
	// Arrange
	s, ws := originStore(t)

	// Act
	firstURL, first, err1 := s.ClaimSlackLinkPost(context.Background(), ws, postedLink)
	againURL, again, err2 := s.ClaimSlackLinkPost(context.Background(), ws, postedLink)
	otherURL, other, err3 := s.ClaimSlackLinkPost(context.Background(), ws, postedLink+"/files")

	// Assert
	if err1 != nil || !first || firstURL != originURL {
		t.Fatalf("first claim = %q, %v, %v; want the thread claimed", firstURL, first, err1)
	}
	if err2 != nil || again || againURL != "" {
		t.Fatalf("second claim = %q, %v, %v; want nothing claimed", againURL, again, err2)
	}
	if err3 != nil || !other || otherURL != originURL {
		t.Fatalf("another link's claim = %q, %v, %v; want it claimed", otherURL, other, err3)
	}
}

func TestClaimSlackLinkPostOfAWorkspaceCreatedForNoThreadClaimsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	url, claimed, err := s.ClaimSlackLinkPost(context.Background(), ws.ID, postedLink)

	// Assert
	if err != nil || claimed || url != "" {
		t.Fatalf("claim = %q, %v, %v; want nothing claimed", url, claimed, err)
	}
	if posts, err := s.WorkspaceSlackLinkPosts(context.Background(), ws.ID); err != nil || len(posts) != 0 {
		t.Fatalf("posts = %+v, %v; want none", posts, err)
	}
}

func TestTheMergedReactionFallsToWhicheverWriteComesSecond(t *testing.T) {
	tests := []struct {
		name       string
		mergeFirst bool
		wantMarkTS string
		wantMerged bool
	}{
		{name: "the ts recorded first: the mark answers it", wantMarkTS: "1730000001.000100"},
		{name: "the merge marked first: the record answers it", mergeFirst: true, wantMerged: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, ws := originStore(t)
			if _, _, err := s.ClaimSlackLinkPost(context.Background(), ws, postedLink); err != nil {
				t.Fatalf("ClaimSlackLinkPost: %v", err)
			}

			// Act
			var (
				ts     string
				merged bool
				err    error
			)
			if tt.mergeFirst {
				ts, err = s.MarkSlackLinkMerged(context.Background(), ws, postedLink)
				if err == nil {
					merged, err = s.RecordSlackLinkPost(context.Background(), ws, postedLink, "1730000001.000100")
				}
			} else {
				merged, err = s.RecordSlackLinkPost(context.Background(), ws, postedLink, "1730000001.000100")
				if err == nil {
					ts, err = s.MarkSlackLinkMerged(context.Background(), ws, postedLink)
				}
			}

			// Assert
			if err != nil || ts != tt.wantMarkTS || merged != tt.wantMerged {
				t.Fatalf("mark ts = %q, record merged = %v, err = %v; want %q and %v", ts, merged, err, tt.wantMarkTS, tt.wantMerged)
			}
			want := []SlackLinkPost{{URL: postedLink, MessageTS: "1730000001.000100", Merged: true}}
			if got, err := s.WorkspaceSlackLinkPosts(context.Background(), ws); err != nil || !reflect.DeepEqual(got, want) {
				t.Fatalf("posts = %+v, %v; want %+v", got, err, want)
			}
		})
	}
}

func TestMarkSlackLinkMergedAnswersOnce(t *testing.T) {
	// Arrange
	s, ws := originStore(t)
	if _, _, err := s.ClaimSlackLinkPost(context.Background(), ws, postedLink); err != nil {
		t.Fatalf("ClaimSlackLinkPost: %v", err)
	}
	if _, err := s.RecordSlackLinkPost(context.Background(), ws, postedLink, "1730000001.000100"); err != nil {
		t.Fatalf("RecordSlackLinkPost: %v", err)
	}
	if _, err := s.MarkSlackLinkMerged(context.Background(), ws, postedLink); err != nil {
		t.Fatalf("first MarkSlackLinkMerged: %v", err)
	}

	// Act
	ts, err := s.MarkSlackLinkMerged(context.Background(), ws, postedLink)

	// Assert
	if err != nil || ts != "" {
		t.Fatalf("second mark = %q, %v; want nothing to react to", ts, err)
	}
}

func TestMarkSlackLinkMergedOfALinkNeverPostedAnswersNothing(t *testing.T) {
	// Arrange
	s, ws := originStore(t)

	// Act
	ts, err := s.MarkSlackLinkMerged(context.Background(), ws, postedLink)

	// Assert
	if err != nil || ts != "" {
		t.Fatalf("mark = %q, %v; want nothing to react to", ts, err)
	}
}

func TestRecordSlackLinkPostRefusals(t *testing.T) {
	tests := []struct {
		name string
		act  func(s *store, ws WorkspaceID) error
	}{
		{name: "an empty ts", act: func(s *store, ws WorkspaceID) error {
			_, _, _ = s.ClaimSlackLinkPost(context.Background(), ws, postedLink)
			_, err := s.RecordSlackLinkPost(context.Background(), ws, postedLink, "")
			return err
		}},
		{name: "a link never claimed", act: func(s *store, ws WorkspaceID) error {
			_, err := s.RecordSlackLinkPost(context.Background(), ws, postedLink, "1730000001.000100")
			return err
		}},
		{name: "a ts already recorded", act: func(s *store, ws WorkspaceID) error {
			_, _, _ = s.ClaimSlackLinkPost(context.Background(), ws, postedLink)
			if _, err := s.RecordSlackLinkPost(context.Background(), ws, postedLink, "1730000001.000100"); err != nil {
				t.Fatalf("first RecordSlackLinkPost: %v", err)
			}
			_, err := s.RecordSlackLinkPost(context.Background(), ws, postedLink, "1730000001.000200")
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, ws := originStore(t)

			// Act
			err := tt.act(s, ws)

			// Assert
			if err == nil {
				t.Fatal("err = nil, want a refusal")
			}
		})
	}
}

func TestSlackLinkPostsGoWithTheirWorkspace(t *testing.T) {
	// Arrange
	s, ws := originStore(t)
	if _, _, err := s.ClaimSlackLinkPost(context.Background(), ws, postedLink); err != nil {
		t.Fatalf("ClaimSlackLinkPost: %v", err)
	}

	// Act
	_, err := s.q(context.Background()).ExecContext(context.Background(), `DELETE FROM workspaces WHERE id = ?`, ws)

	// Assert
	if err != nil {
		t.Fatalf("delete the workspace: %v", err)
	}
	var count int
	if err := s.q(context.Background()).QueryRowContext(context.Background(), `SELECT COUNT(*) FROM workspace_slack_link_posts`).Scan(&count); err != nil || count != 0 {
		t.Fatalf("posts left = %d, %v; want none", count, err)
	}
}

// TestTheMigrationMarksEveryRecordedOriginOpened pins the layout-100 step: an
// origin recorded before it had its opening posted at creation, so it is
// never posted again, and the posts table starts empty.
func TestTheMigrationMarksEveryRecordedOriginOpened(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 99)
	withRawDB(t, path, func(db *sql.DB) {
		if _, err := db.Exec(`INSERT INTO workspace_slack_origins (workspace_id, thread_url) VALUES (?, ?)`, string(fixtureWorkspaceID), originURL); err != nil {
			t.Fatalf("seed the origin: %v", err)
		}
	})

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-99 database: %v", err)
	}
	defer handle.Close()

	// Assert
	url, claimed, err := handle.ClaimSlackOpening(context.Background(), fixtureWorkspaceID)
	if err != nil || claimed || url != "" {
		t.Fatalf("claim on a migrated origin = %q, %v, %v; want it already opened", url, claimed, err)
	}
	if got := scalar[int](t, handle.(*store), `SELECT count(*) FROM workspace_slack_link_posts`); got != 0 {
		t.Fatalf("%d migrated posts, want none", got)
	}
}
