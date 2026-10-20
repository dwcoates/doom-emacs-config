package wsm

import (
	"context"
	"database/sql"
	"errors"
	"slices"
	"testing"
	"time"
)

// family is a parent with one open child, both in one repository.
type family struct {
	s       *store
	repoDir string
	parent  Workspace
	child   Workspace
}

// newFamily registers a parent and an open child of it.
func newFamily(t *testing.T) family {
	t.Helper()
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	parent := registerIn(t, s, repoDir, "parent", nil)
	child := registerChild(t, s, repoDir, "child", parent.ID)
	return family{s: s, repoDir: repoDir, parent: parent, child: child}
}

// workspaceNow reads ID back from the store.
func workspaceNow(t *testing.T, s *store, id WorkspaceID) Workspace {
	t.Helper()
	ws, err := s.Workspace(context.Background(), id)
	if err != nil {
		t.Fatalf("Workspace(%s): %v", id, err)
	}
	return ws
}

// requireHasOpenChildren fails unless ERR refuses PARENT for exactly CHILDREN.
func requireHasOpenChildren(t *testing.T, err error, parent Workspace, children ...Workspace) {
	t.Helper()
	var refused *HasOpenChildrenError
	if !errors.As(err, &refused) {
		t.Fatalf("error = %v, want a HasOpenChildrenError", err)
	}
	want := make([]FamilyRef, len(children))
	for i, child := range children {
		want[i] = FamilyRef{ID: child.ID, Dir: child.Dir}
	}
	if refused.Parent != (FamilyRef{ID: parent.ID, Dir: parent.Dir}) || !slices.Equal(refused.Children, want) {
		t.Fatalf("refusal = %+v, want parent %s with children %v", refused, parent.ID, want)
	}
}

// requireParentNotOpen fails unless ERR refuses for PARENT in STATE.
func requireParentNotOpen(t *testing.T, err error, parent Workspace, state ParentState) {
	t.Helper()
	var refused *ParentNotOpenError
	if !errors.As(err, &refused) {
		t.Fatalf("error = %v, want a ParentNotOpenError", err)
	}
	if refused.Parent != (FamilyRef{ID: parent.ID, Dir: parent.Dir}) || refused.State != state {
		t.Fatalf("refusal = %+v, want parent %s %s", refused, parent.ID, state)
	}
}

func mustClose(t *testing.T, s *store, id WorkspaceID, by CloseCause) {
	t.Helper()
	if err := s.MarkClosed(context.Background(), id, Closure{At: time.Unix(10, 0), By: by}); err != nil {
		t.Fatalf("MarkClosed(%s): %v", id, err)
	}
}

func mustHide(t *testing.T, s *store, id WorkspaceID) {
	t.Helper()
	if err := s.SetHidden(context.Background(), id, true, time.Unix(10, 0)); err != nil {
		t.Fatalf("SetHidden(%s): %v", id, err)
	}
}

// queueClosingMergeOf records an own-branch merge of ID, whose landing closes it.
func queueClosingMergeOf(t *testing.T, s *store, id WorkspaceID) {
	t.Helper()
	if err := s.RequestMerge(context.Background(), RepoKey(t.TempDir()), id, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(10, 0)); err != nil {
		t.Fatalf("RequestMerge(%s): %v", id, err)
	}
}

// legacyHide hides ID behind the family rules' back, as a row from before them.
func legacyHide(t *testing.T, s *store, id WorkspaceID) {
	t.Helper()
	if _, err := s.db().Exec(`UPDATE workspaces SET hidden = 1, hidden_at = 1 WHERE id = ?`, id); err != nil {
		t.Fatalf("hide %s: %v", id, err)
	}
}

// legacyQueueClosingMergeOf records a merge closing ID behind the family
// rules' back, as a row from before them.
func legacyQueueClosingMergeOf(t *testing.T, s *store, id WorkspaceID) {
	t.Helper()
	err := s.write(context.Background(), "test.legacy_merge", nil, func(ctx context.Context, tx *sql.Tx) error {
		return insertRequest(ctx, tx, RepoKey(t.TempDir()), id, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(10, 0))
	})
	if err != nil {
		t.Fatalf("queue a merge of %s: %v", id, err)
	}
}

func TestMarkClosedRefusesAGestureOnAParentWithOpenChildren(t *testing.T) {
	for _, by := range []CloseCause{CloseByUser, CloseByAgent} {
		t.Run(string(by), func(t *testing.T) {
			// Arrange
			f := newFamily(t)

			// Act
			err := f.s.MarkClosed(context.Background(), f.parent.ID, Closure{At: time.Unix(10, 0), By: by})

			// Assert
			requireHasOpenChildren(t, err, f.parent, f.child)
			if workspaceNow(t, f.s, f.parent.ID).Closed {
				t.Fatalf("the parent was closed by a refused close")
			}
		})
	}
}

func TestMarkClosedRecordsTheDaemonsOwnCloseOverOpenChildren(t *testing.T) {
	for _, by := range []CloseCause{CloseByMerge, CloseByDaemon} {
		t.Run(string(by), func(t *testing.T) {
			// Arrange
			f := newFamily(t)

			// Act
			err := f.s.MarkClosed(context.Background(), f.parent.ID, Closure{At: time.Unix(10, 0), By: by})

			// Assert
			if err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			if !workspaceNow(t, f.s, f.parent.ID).Closed {
				t.Fatalf("the parent is open after the daemon's close")
			}
		})
	}
}

func TestMarkClosedCountsAHiddenChildAsOpen(t *testing.T) {
	// Arrange
	f := newFamily(t)
	mustHide(t, f.s, f.child.ID)

	// Act
	err := f.s.MarkClosed(context.Background(), f.parent.ID, Closure{At: time.Unix(10, 0), By: CloseByUser})

	// Assert
	requireHasOpenChildren(t, err, f.parent, f.child)
}

func TestMarkClosedNamesAnOpenGrandchildUnderAClosedChild(t *testing.T) {
	// Arrange -- a family that broke the rule before it existed.
	f := newFamily(t)
	grandchild := registerChild(t, f.s, f.repoDir, "grandchild", f.child.ID)
	mustClose(t, f.s, f.child.ID, CloseByDaemon)

	// Act
	err := f.s.MarkClosed(context.Background(), f.parent.ID, Closure{At: time.Unix(10, 0), By: CloseByUser})

	// Assert
	requireHasOpenChildren(t, err, f.parent, grandchild)
}

func TestMarkClosedClosesAParentWhoseChildrenAreClosed(t *testing.T) {
	// Arrange
	f := newFamily(t)
	mustClose(t, f.s, f.child.ID, CloseByUser)

	// Act
	err := f.s.MarkClosed(context.Background(), f.parent.ID, Closure{At: time.Unix(10, 0), By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
}

func TestMarkClosedIgnoresAHiddenMergeWorkspace(t *testing.T) {
	// Arrange -- an async merge's workspace is its machinery, drawn nowhere.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())

	// Act
	err := s.MarkClosed(context.Background(), requester.ID, Closure{At: time.Unix(10, 0), By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
}

func TestMarkClosedRestampsAClosedParentOverALegacyOpenChild(t *testing.T) {
	// Arrange -- a closed row is no close at all, so it is not refused.
	f := newFamily(t)
	mustClose(t, f.s, f.parent.ID, CloseByDaemon)

	// Act
	err := f.s.MarkClosed(context.Background(), f.parent.ID, Closure{At: time.Unix(20, 0), By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
}

func TestSetHiddenRefusesHidingAParentWithOpenChildren(t *testing.T) {
	// Arrange
	f := newFamily(t)

	// Act
	err := f.s.SetHidden(context.Background(), f.parent.ID, true, time.Unix(10, 0))

	// Assert
	requireHasOpenChildren(t, err, f.parent, f.child)
	if workspaceNow(t, f.s, f.parent.ID).Hidden {
		t.Fatalf("the parent was hidden by a refused hide")
	}
}

func TestSetHiddenHidesAFamilyChildFirst(t *testing.T) {
	// Arrange
	f := newFamily(t)
	mustHide(t, f.s, f.child.ID)
	mustClose(t, f.s, f.child.ID, CloseByUser)

	// Act
	err := f.s.SetHidden(context.Background(), f.parent.ID, true, time.Unix(10, 0))

	// Assert
	if err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
}

func TestSetHiddenRestampsAHiddenParentOverALegacyOpenChild(t *testing.T) {
	// Arrange -- the parent was hidden before its child was made.
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	parent := registerIn(t, s, repoDir, "parent", nil)
	registerChild(t, s, repoDir, "child", parent.ID)
	legacyHide(t, s, parent.ID)

	// Act
	err := s.SetHidden(context.Background(), parent.ID, true, time.Unix(10, 0))

	// Assert
	if err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
}

func TestShowingAHiddenChildRequiresAnOpenParent(t *testing.T) {
	tests := []struct {
		name  string
		setUp func(t *testing.T, f family)
		state ParentState
	}{
		{name: "a hidden parent", state: ParentHidden, setUp: func(t *testing.T, f family) { legacyHide(t, f.s, f.parent.ID) }},
		{name: "a closed parent", state: ParentClosed, setUp: func(t *testing.T, f family) { mustClose(t, f.s, f.parent.ID, CloseByDaemon) }},
		{name: "a closing parent", state: ParentClosing, setUp: func(t *testing.T, f family) { legacyQueueClosingMergeOf(t, f.s, f.parent.ID) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFamily(t)
			mustHide(t, f.s, f.child.ID)
			tt.setUp(t, f)

			// Act
			err := f.s.SetHidden(context.Background(), f.child.ID, false, time.Unix(20, 0))

			// Assert
			requireParentNotOpen(t, err, f.parent, tt.state)
			if !workspaceNow(t, f.s, f.child.ID).Hidden {
				t.Fatalf("the child was shown by a refused show")
			}
		})
	}
}

func TestShowingAHiddenChildOfAnOpenParentSucceeds(t *testing.T) {
	// Arrange
	f := newFamily(t)
	mustHide(t, f.s, f.child.ID)

	// Act
	err := f.s.SetHidden(context.Background(), f.child.ID, false, time.Unix(20, 0))

	// Assert
	if err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
}

func TestReopenRefusesAChildWhoseParentIsNotOpen(t *testing.T) {
	tests := []struct {
		name  string
		setUp func(t *testing.T, f family)
		state ParentState
	}{
		{name: "a closed parent", state: ParentClosed, setUp: func(t *testing.T, f family) { mustClose(t, f.s, f.parent.ID, CloseByUser) }},
		{name: "a hidden parent", state: ParentHidden, setUp: func(t *testing.T, f family) { mustHide(t, f.s, f.parent.ID) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFamily(t)
			mustClose(t, f.s, f.child.ID, CloseByUser)
			tt.setUp(t, f)

			// Act
			err := f.s.Reopen(context.Background(), f.child.ID)

			// Assert
			requireParentNotOpen(t, err, f.parent, tt.state)
			if !workspaceNow(t, f.s, f.child.ID).Closed {
				t.Fatalf("the child was re-opened by a refused re-open")
			}
		})
	}
}

func TestReopenOfAnOpenLegacyChildIsNoReopen(t *testing.T) {
	// Arrange -- an open child of a closed parent, from before the rules.
	f := newFamily(t)
	mustClose(t, f.s, f.parent.ID, CloseByDaemon)

	// Act
	err := f.s.Reopen(context.Background(), f.child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Reopen: %v", err)
	}
}

func TestRegisterWorkspaceRefusesAChildOfAParentThatIsNotOpen(t *testing.T) {
	tests := []struct {
		name  string
		setUp func(t *testing.T, s *store, parent Workspace)
		state ParentState
	}{
		{name: "a hidden parent", state: ParentHidden, setUp: func(t *testing.T, s *store, p Workspace) { mustHide(t, s, p.ID) }},
		{name: "a closed parent", state: ParentClosed, setUp: func(t *testing.T, s *store, p Workspace) { mustClose(t, s, p.ID, CloseByUser) }},
		{name: "a closing parent", state: ParentClosing, setUp: func(t *testing.T, s *store, p Workspace) { queueClosingMergeOf(t, s, p.ID) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			parent := registerIn(t, s, repoDir, "parent", nil)
			tt.setUp(t, s, parent)

			// Act
			_, _, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{
				Name: "child", Branch: "child", ParentBranch: "master", RepoDir: repoDir, Parent: &parent.ID,
			})

			// Assert
			requireParentNotOpen(t, err, parent, tt.state)
			if all, _ := s.ListWorkspaces(context.Background()); len(all) != 1 {
				t.Fatalf("workspaces = %d after a refused registration, want only the parent", len(all))
			}
		})
	}
}

func TestRegisterWorkspaceForksAMergeWorkspaceUnderAHiddenRequester(t *testing.T) {
	// Arrange -- a merge workspace is drawn nowhere, so it is no open child.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	mustHide(t, s, requester.ID)

	// Act
	ws := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())

	// Assert
	if ws.Parent == nil || *ws.Parent != requester.ID {
		t.Fatalf("merge workspace parent = %v, want %s", ws.Parent, requester.ID)
	}
}

func TestSetParentRefusesAParentThatIsNotOpen(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := canonicalTemp(t)
	ws := registerInRepo(t, s, repoDir, "ws", true)
	parent := registerInRepo(t, s, repoDir, "parent", true)
	mustHide(t, s, parent.ID)

	// Act
	_, err := s.SetParent(context.Background(), ws.ID, &parent.ID)

	// Assert
	requireParentNotOpen(t, err, parent, ParentHidden)
	if got := workspaceNow(t, s, ws.ID).Parent; got != nil {
		t.Fatalf("parent = %s after a refused retarget, want none", *got)
	}
}

func TestRequestMergeRefusesAMergeThatClosesAParentWithOpenChildren(t *testing.T) {
	// Arrange
	f := newFamily(t)
	other := registerIn(t, f.s, f.repoDir, "other", nil)
	repo := RepoKey(t.TempDir())
	tests := []struct {
		name      string
		requester WorkspaceID
		source    MergeSource
	}{
		{name: "the parent's own in-place merge", requester: f.parent.ID, source: MergeSource{Kind: MergeSourceOwnBranch}},
		{name: "an async merge whose one-shot is the parent", requester: other.ID, source: MergeSource{
			Kind: MergeSourceBranch, Branch: "feature", Detach: NewDetachedMergeID(), DetachName: "m", CloseOnLanding: f.parent.ID}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := f.s.RequestMerge(context.Background(), repo, tt.requester, tt.source, time.Unix(10, 0))

			// Assert
			requireHasOpenChildren(t, err, f.parent, f.child)
			if queue, _ := f.s.MergeQueue(context.Background(), repo); len(queue) != 0 {
				t.Fatalf("queue = %+v after a refused request, want it empty", queue)
			}
		})
	}
}

func TestRequestMergeQueuesAMergeThatKeepsTheParentOpen(t *testing.T) {
	// Arrange
	f := newFamily(t)

	// Act
	err := f.s.RequestMerge(context.Background(), RepoKey(t.TempDir()), f.parent.ID, MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true}, time.Unix(10, 0))

	// Assert
	if err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
}

func TestRequireOpenParentAnswersTheRefusalWithoutLoggingAFailure(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	parent := testWorkspace(t, s)
	mustClose(t, s, parent.ID, CloseByUser)

	// Act
	err := s.RequireOpenParent(context.Background(), parent.ID)

	// Assert
	requireParentNotOpen(t, err, parent, ParentClosed)
	for _, rec := range log.Records() {
		if rec.Level == "ERROR" {
			t.Fatalf("a refused parent was logged at ERROR: %+v", rec)
		}
	}
}

func TestRequireOpenParentAnswersNilForAnOpenParent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent := testWorkspace(t, s)

	// Act / Assert
	if err := s.RequireOpenParent(context.Background(), parent.ID); err != nil {
		t.Fatalf("RequireOpenParent: %v", err)
	}
}

func TestRequireOpenParentRefusesAnUnknownParent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act / Assert
	if err := s.RequireOpenParent(context.Background(), "ws-missing"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("RequireOpenParent = %v, want ErrNotFound", err)
	}
}

func TestMergeSourceLandingCloses(t *testing.T) {
	origin := &MergeOrigin{Requester: "req", Request: "r1", CloseOnLanding: "one-shot"}
	tests := []struct {
		name         string
		source       MergeSource
		origin       *MergeOrigin
		closes, also WorkspaceID
	}{
		{name: "an own-branch merge closes its requester", source: MergeSource{Kind: MergeSourceOwnBranch}, closes: "ws"},
		{name: "an own-branch merge kept open closes nothing", source: MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true}},
		{name: "a merge workspace's merge closes its one-shot too", source: MergeSource{Kind: MergeSourceOwnBranch}, origin: origin, closes: "ws", also: "one-shot"},
		{name: "a branch merged upstream closes its requester", source: MergeSource{Kind: MergeSourceMergedUpstream}, closes: "ws"},
		{name: "a workspace source closes that workspace", source: MergeSource{Kind: MergeSourceWorkspace, Workspace: "other"}, closes: "other"},
		{name: "a named branch closes nothing", source: MergeSource{Kind: MergeSourceBranch, Branch: "b"}},
		{name: "a detached request closes nothing in place", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachName: "m", CloseOnLanding: "x"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			closes, also := tt.source.LandingCloses("ws", tt.origin)

			// Assert
			if closes != tt.closes || also != tt.also {
				t.Fatalf("LandingCloses = (%q, %q), want (%q, %q)", closes, also, tt.closes, tt.also)
			}
		})
	}
}

func TestMergeSourceClosesNamesADetachedRequestsOneShot(t *testing.T) {
	// Arrange
	source := MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachName: "m", CloseOnLanding: "x"}

	// Act / Assert
	if got := source.Closes("ws", nil); !slices.Equal(got, []WorkspaceID{"x"}) {
		t.Fatalf("Closes = %v, want [x]", got)
	}
}

func TestClosingSQLAgreesWithMergeSourceCloses(t *testing.T) {
	// Arrange -- every source shape the queue stores, each in its own store.
	tests := []struct {
		name   string
		source func(other, oneShot WorkspaceID) MergeSource
		merge  bool
	}{
		{name: "own branch", source: func(_, _ WorkspaceID) MergeSource { return MergeSource{Kind: MergeSourceOwnBranch} }},
		{name: "own branch kept open", source: func(_, _ WorkspaceID) MergeSource { return MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true} }},
		{name: "merged upstream", source: func(_, _ WorkspaceID) MergeSource { return MergeSource{Kind: MergeSourceMergedUpstream} }},
		{name: "workspace", source: func(other, _ WorkspaceID) MergeSource {
			return MergeSource{Kind: MergeSourceWorkspace, Workspace: other}
		}},
		{name: "branch", source: func(_, _ WorkspaceID) MergeSource { return MergeSource{Kind: MergeSourceBranch, Branch: "b"} }},
		{name: "detached with a one-shot", source: func(_, oneShot WorkspaceID) MergeSource {
			return MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: NewDetachedMergeID(), DetachName: "m", CloseOnLanding: oneShot}
		}},
		{name: "a merge workspace's own branch", merge: true, source: func(_, _ WorkspaceID) MergeSource { return MergeSource{Kind: MergeSourceOwnBranch} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s, _ := testStore(t)
			ctx := context.Background()
			repoDir := arrangeRepo(t, "repo")
			requester := registerIn(t, s, repoDir, "requester", nil)
			other := registerIn(t, s, repoDir, "other", nil)
			oneShot := registerIn(t, s, repoDir, "one-shot", nil)
			if tt.merge {
				dir := t.TempDir()
				ws, _, err := s.RegisterWorkspace(ctx, dir, RegisterFacts{
					Name: "m", Branch: "m", ParentBranch: "master", Parent: &other.ID, RepoDir: repoDir,
					MergeOf: &MergeOrigin{Requester: other.ID, Request: NewDetachedMergeID(), CloseOnLanding: oneShot.ID},
				})
				if err != nil {
					t.Fatalf("RegisterWorkspace: %v", err)
				}
				requester = ws
			}
			source := tt.source(other.ID, oneShot.ID)
			if err := s.RequestMerge(ctx, RepoKey(t.TempDir()), requester.ID, source, time.Unix(10, 0)); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			want := source.Closes(requester.ID, requester.MergeOf)

			for _, ws := range []Workspace{requester, other, oneShot} {
				// Act
				got, err := isClosing(ctx, s.db(), ws.ID)

				// Assert
				if err != nil {
					t.Fatalf("isClosing(%s): %v", ws.Name, err)
				}
				if got != slices.Contains(want, ws.ID) {
					t.Fatalf("isClosing(%s) = %v, want %v (Closes = %v)", ws.Name, got, !got, want)
				}
			}
		})
	}
}

func TestMergeHiddenSQLAgreesWithMergeHidden(t *testing.T) {
	// Arrange -- a merge workspace hidden, revealed and landed, and an ordinary one.
	s, _ := testStore(t)
	ctx := context.Background()
	requester := testWorkspace(t, s)
	hidden := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	revealed := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	if _, err := s.RevealMergeWorkspace(ctx, revealed.ID); err != nil {
		t.Fatalf("RevealMergeWorkspace: %v", err)
	}
	landed := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	if err := s.SetMergedAt(ctx, landed.ID, time.Unix(10, 0), nil); err != nil {
		t.Fatalf("SetMergedAt: %v", err)
	}
	for _, ws := range []Workspace{requester, hidden, revealed, landed} {
		// Act
		var got bool
		if err := s.db().QueryRow(`SELECT `+mergeHiddenSQL+` FROM workspaces w WHERE w.id = ?`, ws.ID).Scan(&got); err != nil {
			t.Fatalf("mergeHiddenSQL(%s): %v", ws.ID, err)
		}

		// Assert
		if want := workspaceNow(t, s, ws.ID).MergeHidden(); got != want {
			t.Fatalf("mergeHiddenSQL(%s) = %v, want %v", ws.ID, got, want)
		}
	}
}
