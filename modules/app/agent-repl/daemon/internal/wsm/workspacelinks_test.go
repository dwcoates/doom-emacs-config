package wsm

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

// linkStep is one change a links test applies, in order.
type linkStep struct {
	set    *WorkspaceLink
	remove string
	show   []string
	clear  bool
}

func setLink(url, label string) linkStep {
	return linkStep{set: &WorkspaceLink{URL: url, Label: label}}
}

func setOwnedLink(url, label string) linkStep {
	return linkStep{set: &WorkspaceLink{URL: url, Label: label, Owned: true}}
}

// applyLinkSteps runs STEPS against WS, failing on any error, and answers the
// links the last step answered (a clear answers a fresh read).
func applyLinkSteps(t *testing.T, s *store, ws WorkspaceID, steps []linkStep) []WorkspaceLink {
	t.Helper()
	ctx := context.Background()
	var got []WorkspaceLink
	var err error
	for _, step := range steps {
		switch {
		case step.set != nil:
			got, err = s.SetWorkspaceLink(ctx, ws, LinkSet{URL: step.set.URL, Label: step.set.Label, Owned: step.set.Owned})
		case step.remove != "":
			got, err = s.RemoveWorkspaceLink(ctx, ws, step.remove)
		case step.show != nil:
			got, err = s.ShowWorkspaceLinks(ctx, ws, step.show)
		case step.clear:
			if err = s.ClearWorkspaceLinks(ctx, ws); err == nil {
				got, err = s.WorkspaceLinks(ctx, ws)
			}
		}
		if err != nil {
			t.Fatalf("step %+v: %v", step, err)
		}
	}
	return got
}

// linkURL is the i'th sample url.
func linkURL(i int) string { return fmt.Sprintf("https://example.com/%d", i) }

func TestWorkspaceLinksChanges(t *testing.T) {
	five := []linkStep{setLink(linkURL(1), "one a"), setLink(linkURL(2), "two a"), setLink(linkURL(3), "three a"),
		setLink(linkURL(4), "four a"), setLink(linkURL(5), "five a")}
	tests := []struct {
		name  string
		steps []linkStep
		want  []WorkspaceLink
	}{
		{name: "a set is read back, shown", steps: five[:1],
			want: []WorkspaceLink{{URL: linkURL(1), Label: "one a", Shown: true}}},
		{name: "links read most recently set first", steps: five[:2],
			want: []WorkspaceLink{{URL: linkURL(2), Label: "two a", Shown: true}, {URL: linkURL(1), Label: "one a", Shown: true}}},
		{name: "a relabel moves the link to the front", steps: append(slices.Clone(five[:2]), setLink(linkURL(1), "one b")),
			want: []WorkspaceLink{{URL: linkURL(1), Label: "one b", Shown: true}, {URL: linkURL(2), Label: "two a", Shown: true}}},
		{name: "a sixth link is stored unshown", steps: append(slices.Clone(five), setLink(linkURL(6), "six a")),
			want: []WorkspaceLink{{URL: linkURL(6), Label: "six a"}, {URL: linkURL(5), Label: "five a", Shown: true},
				{URL: linkURL(4), Label: "four a", Shown: true}, {URL: linkURL(3), Label: "three a", Shown: true},
				{URL: linkURL(2), Label: "two a", Shown: true}, {URL: linkURL(1), Label: "one a", Shown: true}}},
		{name: "a relabel keeps an unshown link unshown",
			steps: append(slices.Clone(five), setLink(linkURL(6), "six a"), setLink(linkURL(6), "six b"), linkStep{remove: linkURL(1)},
				linkStep{remove: linkURL(2)}, linkStep{remove: linkURL(3)}, linkStep{remove: linkURL(4)}),
			want: []WorkspaceLink{{URL: linkURL(6), Label: "six b"}, {URL: linkURL(5), Label: "five a", Shown: true}}},
		{name: "a removal frees a slot for the next new link",
			steps: append(slices.Clone(five), linkStep{remove: linkURL(1)}, setLink(linkURL(6), "six a")),
			want: []WorkspaceLink{{URL: linkURL(6), Label: "six a", Shown: true}, {URL: linkURL(5), Label: "five a", Shown: true},
				{URL: linkURL(4), Label: "four a", Shown: true}, {URL: linkURL(3), Label: "three a", Shown: true},
				{URL: linkURL(2), Label: "two a", Shown: true}}},
		{name: "a show names exactly the shown links",
			steps: append(slices.Clone(five[:3]), linkStep{show: []string{linkURL(1)}}),
			want: []WorkspaceLink{{URL: linkURL(3), Label: "three a"}, {URL: linkURL(2), Label: "two a"},
				{URL: linkURL(1), Label: "one a", Shown: true}}},
		{name: "an owned set is read back owned", steps: []linkStep{setOwnedLink(linkURL(1), "one a")},
			want: []WorkspaceLink{{URL: linkURL(1), Label: "one a", Shown: true, Owned: true}}},
		{name: "a relabel sets the ownership anew", steps: []linkStep{setOwnedLink(linkURL(1), "one a"), setLink(linkURL(1), "one b")},
			want: []WorkspaceLink{{URL: linkURL(1), Label: "one b", Shown: true}}},
		{name: "a clear leaves none", steps: append(slices.Clone(five[:2]), linkStep{clear: true}), want: nil},
		{name: "clearing a workspace with none is no change", steps: []linkStep{{clear: true}}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			got := applyLinkSteps(t, s, ws.ID, tt.steps)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("links = %+v, want %+v", got, tt.want)
			}
			read, err := s.WorkspaceLinks(context.Background(), ws.ID)
			if err != nil || !slices.Equal(read, tt.want) {
				t.Fatalf("a later read = %+v, %v; want the change's own answer %+v", read, err, tt.want)
			}
		})
	}
}

func TestWorkspaceLinksAreTheirWorkspacesOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first, second := testWorkspace(t, s), testWorkspaceNamed(t, s, "other")
	applyLinkSteps(t, s, first.ID, []linkStep{setLink(linkURL(1), "one a")})

	// Act
	got, err := s.WorkspaceLinks(context.Background(), second.ID)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("the other workspace's links = %+v, %v; want none", got, err)
	}
}

func TestWorkspaceLinksOutliveTheWorkspacesClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	applyLinkSteps(t, s, ws.ID, []linkStep{setLink(linkURL(1), "one a")})

	// Act
	err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	got, err := s.WorkspaceLinks(context.Background(), ws.ID)
	if err != nil || len(got) != 1 || got[0].URL != linkURL(1) {
		t.Fatalf("after close: %+v, %v; want the link standing", got, err)
	}
}

func TestWorkspaceLinksRefusals(t *testing.T) {
	standing := []WorkspaceLink{{URL: linkURL(1), Label: "one a", Shown: true}}
	tests := []struct {
		name     string
		act      func(s *store, ws WorkspaceID) error
		unknown  string
		errorOp  string
		isDefect bool
		notFound bool
	}{
		{name: "removing a url the workspace does not hold is an unknown link",
			act: func(s *store, ws WorkspaceID) error {
				_, err := s.RemoveWorkspaceLink(context.Background(), ws, linkURL(9))
				return err
			}, unknown: linkURL(9)},
		{name: "showing a url the workspace does not hold is an unknown link and shows nothing new",
			act: func(s *store, ws WorkspaceID) error {
				_, err := s.ShowWorkspaceLinks(context.Background(), ws, []string{linkURL(9)})
				return err
			}, unknown: linkURL(9)},
		{name: "a show naming more than the limit is a defect",
			act: func(s *store, ws WorkspaceID) error {
				_, err := s.ShowWorkspaceLinks(context.Background(), ws, []string{linkURL(1), linkURL(2), linkURL(3), linkURL(4), linkURL(5), linkURL(6)})
				return err
			}, isDefect: true, errorOp: "daemon.wsm.show_workspace_links"},
		{name: "a show naming a url twice is a defect",
			act: func(s *store, ws WorkspaceID) error {
				_, err := s.ShowWorkspaceLinks(context.Background(), ws, []string{linkURL(1), linkURL(1)})
				return err
			}, isDefect: true, errorOp: "daemon.wsm.show_workspace_links"},
		{name: "an empty show is a defect",
			act: func(s *store, ws WorkspaceID) error {
				_, err := s.ShowWorkspaceLinks(context.Background(), ws, nil)
				return err
			}, isDefect: true, errorOp: "daemon.wsm.show_workspace_links"},
		{name: "a set with no label is a defect",
			act: func(s *store, ws WorkspaceID) error {
				_, err := s.SetWorkspaceLink(context.Background(), ws, LinkSet{URL: linkURL(2)})
				return err
			}, isDefect: true, errorOp: "daemon.wsm.set_workspace_link"},
		{name: "a set on an unknown workspace is not found",
			act: func(s *store, _ WorkspaceID) error {
				_, err := s.SetWorkspaceLink(context.Background(), WorkspaceID("absent"), LinkSet{URL: linkURL(2), Label: "two a"})
				return err
			}, notFound: true},
		{name: "a clear on an unknown workspace is not found",
			act: func(s *store, _ WorkspaceID) error {
				return s.ClearWorkspaceLinks(context.Background(), WorkspaceID("absent"))
			}, notFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			applyLinkSteps(t, s, ws.ID, []linkStep{setLink(linkURL(1), "one a")})

			// Act
			err := tt.act(s, ws.ID)

			// Assert
			var unknown *UnknownLinkError
			switch {
			case tt.unknown != "":
				if !errors.As(err, &unknown) || unknown.URL != tt.unknown {
					t.Fatalf("err = %v, want an unknown link naming %q", err, tt.unknown)
				}
			case tt.notFound:
				if !errors.Is(err, ErrNotFound) {
					t.Fatalf("err = %v, want ErrNotFound", err)
				}
			case tt.isDefect:
				if err == nil || errors.As(err, &unknown) {
					t.Fatalf("err = %v, want a defect refusal", err)
				}
				if !hasErrorRecord(log.Records(), tt.errorOp, string(ws.ID)) {
					t.Errorf("no error record of %s names the workspace: %v", tt.errorOp, log.Records())
				}
			}
			got, _ := s.WorkspaceLinks(context.Background(), ws.ID)
			if !slices.Equal(got, standing) {
				t.Errorf("links = %+v, want the standing %+v untouched", got, standing)
			}
		})
	}
}

// assertRefusalRecordedAtDebug fails T unless RECORDS hold a DEBUG record of
// OP and no ERROR record of it naming WORKSPACE: a links refusal is the store
// answering what was asked, never a failed write (isLinkRefusal).
func assertRefusalRecordedAtDebug(t *testing.T, records []dlog.Record, op, workspace string) {
	t.Helper()
	if _, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: dlog.LevelDebug, Operation: op}); !ok {
		t.Errorf("no DEBUG record of %s: %v", op, records)
	}
	if hasErrorRecord(records, op, workspace) {
		t.Errorf("the refusal of %s was recorded as an ERROR: %v", op, records)
	}
}

// hasErrorRecord reports whether an error record of OP names WORKSPACE.
func hasErrorRecord(records []dlog.Record, op, workspace string) bool {
	for _, r := range dlog.FilterRecords(records, dlog.RecordQuery{Level: "error", Operation: op}) {
		if r.Context["workspace"] == workspace {
			return true
		}
	}
	return false
}

func TestOwnedPullRequestsMerged(t *testing.T) {
	pr := func(n uint32, owned bool, sub *PullRequestSubscription) WorkspaceLink {
		key := PullRequestKey{Owner: "o", Repo: "r", Number: n}
		return WorkspaceLink{URL: key.String(), Owned: owned, PullRequest: &key, Follow: sub}
	}
	ended := func(merged bool) *PullRequestSubscription {
		return &PullRequestSubscription{Ended: &PullRequestEnd{Merged: merged}}
	}
	tests := []struct {
		name       string
		links      []WorkspaceLink
		wantOwns   bool
		wantMerged bool
	}{
		{name: "no links own nothing", wantOwns: false, wantMerged: false},
		{name: "a referenced pull request is not owned", links: []WorkspaceLink{pr(1, false, ended(true))}, wantOwns: false, wantMerged: false},
		{name: "an owned link that is no pull request is not counted", links: []WorkspaceLink{{URL: "https://example.com", Owned: true}}, wantOwns: false, wantMerged: false},
		{name: "every owned pull request merged", links: []WorkspaceLink{pr(1, true, ended(true)), pr(2, true, ended(true))}, wantOwns: true, wantMerged: true},
		{name: "one still open", links: []WorkspaceLink{pr(1, true, ended(true)), pr(2, true, &PullRequestSubscription{})}, wantOwns: true, wantMerged: false},
		{name: "one closed unmerged", links: []WorkspaceLink{pr(1, true, ended(false))}, wantOwns: true, wantMerged: false},
		{name: "one no longer followed", links: []WorkspaceLink{pr(1, true, nil)}, wantOwns: true, wantMerged: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			owns, merged := OwnedPullRequestsMerged(tt.links)

			// Assert
			if owns != tt.wantOwns || merged != tt.wantMerged {
				t.Fatalf("OwnedPullRequestsMerged = (%v, %v), want (%v, %v)", owns, merged, tt.wantOwns, tt.wantMerged)
			}
		})
	}
}

func TestOwnedPullRequestOpen(t *testing.T) {
	pr := func(n uint32, owned bool, sub *PullRequestSubscription) WorkspaceLink {
		key := PullRequestKey{Owner: "o", Repo: "r", Number: n}
		return WorkspaceLink{URL: key.String(), Owned: owned, PullRequest: &key, Follow: sub}
	}
	ended := func(merged bool) *PullRequestSubscription {
		return &PullRequestSubscription{Ended: &PullRequestEnd{Merged: merged}}
	}
	tests := []struct {
		name  string
		links []WorkspaceLink
		want  bool
	}{
		{name: "no links hold no open pull request", want: false},
		{name: "an owned followed pull request with no end is open", links: []WorkspaceLink{pr(1, true, &PullRequestSubscription{})}, want: true},
		{name: "a referenced open pull request is not owned", links: []WorkspaceLink{pr(1, false, &PullRequestSubscription{})}, want: false},
		{name: "an owned link that is no pull request is not counted", links: []WorkspaceLink{{URL: "https://example.com", Owned: true}}, want: false},
		{name: "a merged pull request is not open", links: []WorkspaceLink{pr(1, true, ended(true))}, want: false},
		{name: "a closed unmerged pull request is not open", links: []WorkspaceLink{pr(1, true, ended(false))}, want: false},
		{name: "a pull request no longer followed is not open", links: []WorkspaceLink{pr(1, true, nil)}, want: false},
		{name: "one open among merged ones is open", links: []WorkspaceLink{pr(1, true, ended(true)), pr(2, true, &PullRequestSubscription{})}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := OwnedPullRequestOpen(tt.links)

			// Assert
			if got != tt.want {
				t.Fatalf("OwnedPullRequestOpen = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestOwnedPullRequestLinksAreTheOwnedPullRequests(t *testing.T) {
	key := PullRequestKey{Owner: "o", Repo: "r", Number: 1}
	owned := WorkspaceLink{URL: key.String(), Owned: true, PullRequest: &key}
	tests := []struct {
		name  string
		links []WorkspaceLink
		want  []WorkspaceLink
	}{
		{name: "no links"},
		{name: "a referenced pull request is not owned", links: []WorkspaceLink{{URL: key.String(), PullRequest: &key}}},
		{name: "an owned link that is no pull request", links: []WorkspaceLink{{URL: "https://example.com", Owned: true}}},
		{name: "an owned pull request", links: []WorkspaceLink{{URL: "https://example.com", Owned: true}, owned}, want: []WorkspaceLink{owned}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := OwnedPullRequestLinks(tt.links)

			// Assert
			if len(got) != len(tt.want) || (len(got) == 1 && got[0].URL != tt.want[0].URL) {
				t.Fatalf("OwnedPullRequestLinks = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestDeployablePullRequestsLeaveOutTheClosedUnmerged(t *testing.T) {
	pr := func(n uint32, sub *PullRequestSubscription) WorkspaceLink {
		key := PullRequestKey{Owner: "o", Repo: "r", Number: n}
		return WorkspaceLink{URL: key.String(), Owned: true, PullRequest: &key, Follow: sub}
	}
	tests := []struct {
		name  string
		links []WorkspaceLink
		want  []string
	}{
		{name: "an open one", links: []WorkspaceLink{pr(1, &PullRequestSubscription{})}, want: []string{"o/r#1"}},
		{name: "a merged one", links: []WorkspaceLink{pr(1, &PullRequestSubscription{Ended: &PullRequestEnd{Merged: true}})}, want: []string{"o/r#1"}},
		{name: "one no longer followed", links: []WorkspaceLink{pr(1, nil)}, want: []string{"o/r#1"}},
		{name: "a closed unmerged one", links: []WorkspaceLink{pr(1, &PullRequestSubscription{Ended: &PullRequestEnd{}})}},
		{name: "a referenced one", links: []WorkspaceLink{{URL: "o/r#2", PullRequest: &PullRequestKey{Owner: "o", Repo: "r", Number: 2}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := DeployablePullRequests(tt.links)

			// Assert
			if len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("DeployablePullRequests = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestWorkspaceLinksGNSSubpages(t *testing.T) {
	const (
		page    = "https://gns.example.com/key/teams.core"
		subpage = "https://gns.example.com/app/teams.core.review.app?s=v"
	)
	tests := []struct {
		name string
		// held is the links stored before the set.
		held []linkStep
		set  LinkSet
		// refusedBy is the owned page a refusal names; empty when the set
		// is stored.
		refusedBy string
	}{
		{name: "an owned subpage of an owned page is refused", held: []linkStep{setOwnedLink(page, "The team's page")},
			set: LinkSet{URL: subpage, Label: "The review app", Owned: true}, refusedBy: page},
		{name: "a deeper owned subpage is refused", held: []linkStep{setOwnedLink(page, "The team's page")},
			set: LinkSet{URL: "https://gns.example.com/key/teams.core.notes.2026", Label: "This year's notes", Owned: true}, refusedBy: page},
		{name: "an owned subpage of a referenced page is stored", held: []linkStep{setLink(page, "The team's page")},
			set: LinkSet{URL: subpage, Label: "The review app", Owned: true}},
		{name: "a subpage stored as a reference is stored", held: []linkStep{setOwnedLink(page, "The team's page")},
			set: LinkSet{URL: subpage, Label: "The review app"}},
		{name: "an owned page that is no subpage is stored", held: []linkStep{setOwnedLink(page, "The team's page")},
			set: LinkSet{URL: "https://gns.example.com/key/teams.core-chess", Label: "Another team's page", Owned: true}},
		{name: "relabeling the owned page itself is stored", held: []linkStep{setOwnedLink(page, "The team's page")},
			set: LinkSet{URL: page, Label: "The team's main page", Owned: true}},
		{name: "an owned parent of an owned page is stored", held: []linkStep{setOwnedLink(subpage, "The review app")},
			set: LinkSet{URL: page, Label: "The team's page", Owned: true}},
		{name: "an owned url that is no GNS page is stored", held: []linkStep{setOwnedLink(page, "The team's page")},
			set: LinkSet{URL: "https://example.com/key/teams.core.notes", Label: "A lookalike page", Owned: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			standing := applyLinkSteps(t, s, ws.ID, tt.held)

			// Act
			got, err := s.SetWorkspaceLink(context.Background(), ws.ID, tt.set)

			// Assert
			read, _ := s.WorkspaceLinks(context.Background(), ws.ID)
			if tt.refusedBy == "" {
				if err != nil || got[0].URL != tt.set.URL || got[0].Owned != tt.set.Owned {
					t.Fatalf("set = %+v, %v; want the link stored at the front", got, err)
				}
				return
			}
			var subpageErr *SubpageOfOwnedLinkError
			if !errors.As(err, &subpageErr) || subpageErr.URL != tt.set.URL || subpageErr.OwnedPageURL != tt.refusedBy {
				t.Fatalf("err = %v, want a subpage refusal naming %q", err, tt.refusedBy)
			}
			if !slices.Equal(read, standing) {
				t.Errorf("links = %+v, want the standing %+v untouched", read, standing)
			}
			assertRefusalRecordedAtDebug(t, log.Records(), "daemon.wsm.set_workspace_link", string(ws.ID))
		})
	}
}

// ownedLinkURL is the i'th sample url of an owned link.
func ownedLinkURL(i int) string { return fmt.Sprintf("https://example.com/owned/%d", i) }

func TestWorkspaceLinksOwnedLinksComeFirstInTheMenu(t *testing.T) {
	// fiveReferences fills the menu with references, linkURL(1) the oldest.
	fiveReferences := []linkStep{setLink(linkURL(1), "one a"), setLink(linkURL(2), "two a"), setLink(linkURL(3), "three a"),
		setLink(linkURL(4), "four a"), setLink(linkURL(5), "five a")}
	fiveOwned := []linkStep{setOwnedLink(ownedLinkURL(1), "owned one"), setOwnedLink(ownedLinkURL(2), "owned two"), setOwnedLink(ownedLinkURL(3), "owned three"),
		setOwnedLink(ownedLinkURL(4), "owned four"), setOwnedLink(ownedLinkURL(5), "owned five")}
	tests := []struct {
		name  string
		steps []linkStep
		// shown is the urls the menu shows afterwards, in any order.
		shown []string
	}{
		{name: "a new owned link takes the oldest shown reference's place",
			steps: append(slices.Clone(fiveReferences), setOwnedLink(ownedLinkURL(1), "owned one")),
			shown: []string{ownedLinkURL(1), linkURL(2), linkURL(3), linkURL(4), linkURL(5)}},
		{name: "a second owned link takes the next oldest reference's place",
			steps: append(slices.Clone(fiveReferences), setOwnedLink(ownedLinkURL(1), "owned one"), setOwnedLink(ownedLinkURL(2), "owned two")),
			shown: []string{ownedLinkURL(1), ownedLinkURL(2), linkURL(3), linkURL(4), linkURL(5)}},
		{name: "the oldest reference is the one least recently set, not stored",
			steps: append(slices.Clone(fiveReferences), setLink(linkURL(1), "one b"), setOwnedLink(ownedLinkURL(1), "owned one")),
			shown: []string{ownedLinkURL(1), linkURL(1), linkURL(3), linkURL(4), linkURL(5)}},
		{name: "a new owned link is stored unshown when the menu shows only owned links",
			steps: append(slices.Clone(fiveOwned), setOwnedLink(ownedLinkURL(6), "owned six")),
			shown: []string{ownedLinkURL(1), ownedLinkURL(2), ownedLinkURL(3), ownedLinkURL(4), ownedLinkURL(5)}},
		{name: "a new reference is stored unshown when the menu is full",
			steps: append(slices.Clone(fiveReferences), setLink(linkURL(6), "six a")),
			shown: []string{linkURL(1), linkURL(2), linkURL(3), linkURL(4), linkURL(5)}},
		{name: "a relabel making an unshown reference owned takes a reference's place",
			steps: append(slices.Clone(fiveReferences), setLink(linkURL(6), "six a"), setOwnedLink(linkURL(6), "six b")),
			shown: []string{linkURL(6), linkURL(2), linkURL(3), linkURL(4), linkURL(5)}},
		{name: "a relabel making a shown reference owned keeps it shown",
			steps: append(slices.Clone(fiveReferences), setOwnedLink(linkURL(3), "three b")),
			shown: []string{linkURL(1), linkURL(2), linkURL(3), linkURL(4), linkURL(5)}},
		{name: "a relabel making a shown owned link a reference keeps it shown",
			steps: append(slices.Clone(fiveOwned), setLink(ownedLinkURL(1), "owned one b")),
			shown: []string{ownedLinkURL(1), ownedLinkURL(2), ownedLinkURL(3), ownedLinkURL(4), ownedLinkURL(5)}},
		{name: "an owned link left out by a show is shown again by its relabel in a reference's place",
			steps: append(slices.Clone(fiveReferences[:4]), setOwnedLink(ownedLinkURL(7), "owned seven"), setOwnedLink(ownedLinkURL(8), "owned eight"),
				setOwnedLink(ownedLinkURL(1), "owned one"), setOwnedLink(ownedLinkURL(2), "owned two"), setOwnedLink(ownedLinkURL(3), "owned three"),
				setOwnedLink(ownedLinkURL(4), "owned four"), linkStep{show: []string{ownedLinkURL(1), ownedLinkURL(2), ownedLinkURL(3), ownedLinkURL(4), ownedLinkURL(7)}},
				setOwnedLink(ownedLinkURL(8), "owned eight b")),
			shown: []string{ownedLinkURL(1), ownedLinkURL(2), ownedLinkURL(3), ownedLinkURL(4), ownedLinkURL(7)}},
		{name: "a show of every owned link and references after them stands",
			steps: append(slices.Clone(fiveReferences), setOwnedLink(ownedLinkURL(1), "owned one"), linkStep{show: []string{ownedLinkURL(1), linkURL(1), linkURL(2)}}),
			shown: []string{ownedLinkURL(1), linkURL(1), linkURL(2)}},
		{name: "a show of owned links alone, leaving some out, stands",
			steps: append(slices.Clone(fiveOwned[:3]), linkStep{show: []string{ownedLinkURL(1)}}),
			shown: []string{ownedLinkURL(1)}},
		{name: "a show choosing a reference over an owned link stands once the owned links alone exceed the menu",
			steps: append(slices.Clone(fiveOwned), setOwnedLink(ownedLinkURL(6), "owned six"), setLink(linkURL(1), "one a"),
				linkStep{show: []string{linkURL(1), ownedLinkURL(2), ownedLinkURL(3), ownedLinkURL(4), ownedLinkURL(5)}}),
			shown: []string{linkURL(1), ownedLinkURL(2), ownedLinkURL(3), ownedLinkURL(4), ownedLinkURL(5)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			got := applyLinkSteps(t, s, ws.ID, tt.steps)

			// Assert
			var shown []string
			for _, link := range got {
				if link.Shown {
					shown = append(shown, link.URL)
				}
			}
			slices.Sort(shown)
			want := slices.Sorted(slices.Values(tt.shown))
			if !slices.Equal(shown, want) {
				t.Fatalf("shown = %v, want %v", shown, want)
			}
		})
	}
}

func TestWorkspaceLinksRefuseAShowLeavingAnOwnedLinkOut(t *testing.T) {
	tests := []struct {
		name string
		show []string
		// wantOwned and wantReference are what the refusal names.
		wantOwned, wantReference string
	}{
		{name: "a reference shown in an owned link's place", show: []string{linkURL(1), ownedLinkURL(1)},
			wantOwned: ownedLinkURL(2), wantReference: linkURL(1)},
		{name: "the most recently set owned link left out is named", show: []string{linkURL(2), linkURL(1)},
			wantOwned: ownedLinkURL(2), wantReference: linkURL(2)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			standing := applyLinkSteps(t, s, ws.ID, []linkStep{setLink(linkURL(1), "one a"), setLink(linkURL(2), "two a"),
				setOwnedLink(ownedLinkURL(1), "owned one"), setOwnedLink(ownedLinkURL(2), "owned two")})

			// Act
			_, err := s.ShowWorkspaceLinks(context.Background(), ws.ID, tt.show)

			// Assert
			var unshown *OwnedLinkUnshownError
			if !errors.As(err, &unshown) || unshown.OwnedURL != tt.wantOwned || unshown.ReferenceURL != tt.wantReference {
				t.Fatalf("err = %v, want an owned-link-unshown refusal naming %q and %q", err, tt.wantOwned, tt.wantReference)
			}
			read, _ := s.WorkspaceLinks(context.Background(), ws.ID)
			if !slices.Equal(read, standing) {
				t.Errorf("links = %+v, want the standing %+v untouched", read, standing)
			}
			assertRefusalRecordedAtDebug(t, log.Records(), "daemon.wsm.show_workspace_links", string(ws.ID))
		})
	}
}

// TestWorkspaceLinksRefusalsAreRecordedAtDebug pins every refusal a links
// write answers (isLinkRefusal) to a DEBUG record, never an ERROR: each is
// the store answering what was asked, which the verb records as the rpc's
// refusal.
func TestWorkspaceLinksRefusalsAreRecordedAtDebug(t *testing.T) {
	tests := []struct {
		name string
		op   string
		act  func(t *testing.T, s *store, ws WorkspaceID) error
	}{
		{name: "an unknown link", op: "daemon.wsm.remove_workspace_link", act: func(t *testing.T, s *store, ws WorkspaceID) error {
			_, err := s.RemoveWorkspaceLink(context.Background(), ws, linkURL(9))
			return err
		}},
		{name: "an announcement of an unheld link", op: "daemon.wsm.set_workspace_link", act: func(t *testing.T, s *store, ws WorkspaceID) error {
			_, err := s.SetWorkspaceLink(context.Background(), ws, LinkSet{URL: linkURL(2), Label: "the post", Owned: true, Announces: linkURL(9)})
			return err
		}},
		{name: "a pull request another workspace owns", op: "daemon.wsm.set_workspace_link", act: func(t *testing.T, s *store, ws WorkspaceID) error {
			other := testWorkspaceNamed(t, s, "other")
			mustSetLink(t, s, other.ID, ownedSet(other.ID, ownedURL, prKey))
			_, err := s.SetWorkspaceLink(context.Background(), ws, ownedSet(ws, ownedURL, prKey))
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := tt.act(t, s, ws.ID)

			// Assert
			if !isLinkRefusal(err) {
				t.Fatalf("err = %v, want a links refusal", err)
			}
			assertRefusalRecordedAtDebug(t, log.Records(), tt.op, string(ws.ID))
		})
	}
}

func TestAddWorkspaceLinkIfAbsent(t *testing.T) {
	tests := []struct {
		name      string
		steps     []linkStep
		wantAdded bool
		want      []WorkspaceLink
	}{
		{name: "a new url is stored as a shown reference", wantAdded: true,
			want: []WorkspaceLink{{URL: linkURL(1), Label: "auto label", Shown: true}}},
		{name: "a url held as a reference keeps its label", steps: []linkStep{setLink(linkURL(1), "mine a")},
			want: []WorkspaceLink{{URL: linkURL(1), Label: "mine a", Shown: true}}},
		{name: "a url held as owned stays owned", steps: []linkStep{setOwnedLink(linkURL(1), "mine a")},
			want: []WorkspaceLink{{URL: linkURL(1), Label: "mine a", Shown: true, Owned: true}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			applyLinkSteps(t, s, ws.ID, tt.steps)

			// Act.
			added, err := s.AddWorkspaceLinkIfAbsent(context.Background(), ws.ID, linkURL(1), "auto label", nil)

			// Assert.
			if err != nil || added != tt.wantAdded {
				t.Fatalf("AddWorkspaceLinkIfAbsent = %v, %v; want %v", added, err, tt.wantAdded)
			}
			got, err := s.WorkspaceLinks(context.Background(), ws.ID)
			if err != nil || !slices.EqualFunc(got, tt.want, func(a, b WorkspaceLink) bool {
				return a.URL == b.URL && a.Label == b.Label && a.Shown == b.Shown && a.Owned == b.Owned
			}) {
				t.Fatalf("links = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}

func TestAnAutoAddedLinkIsPromotedByAnOwnedSet(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if _, err := s.AddWorkspaceLinkIfAbsent(ctx, ws.ID, linkURL(1), "auto label", nil); err != nil {
		t.Fatalf("AddWorkspaceLinkIfAbsent: %v", err)
	}

	// Act.
	got, err := s.SetWorkspaceLink(ctx, ws.ID, LinkSet{URL: linkURL(1), Label: "mine a", Owned: true})

	// Assert.
	if err != nil || len(got) != 1 || !got[0].Owned || got[0].Label != "mine a" || !got[0].Shown {
		t.Fatalf("links = %+v, %v; want the one link promoted to owned", got, err)
	}
}

func TestAnAutoAddedPullRequestLinkNamesItsPullRequestAndIsNotFollowed(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	pr := PullRequestKey{Owner: "o", Repo: "r", Number: 7}

	// Act.
	_, err := s.AddWorkspaceLinkIfAbsent(context.Background(), ws.ID, "https://github.com/o/r/pull/7", "auto label", &pr)

	// Assert.
	got, rerr := s.WorkspaceLinks(context.Background(), ws.ID)
	if err != nil || rerr != nil || len(got) != 1 || got[0].PullRequest == nil || *got[0].PullRequest != pr || got[0].Follow != nil {
		t.Fatalf("links = %+v, %v, %v; want an unfollowed reference naming the pull request", got, err, rerr)
	}
}

func TestAddWorkspaceLinkIfAbsentRefusals(t *testing.T) {
	tests := []struct {
		name       string
		ws         func(s *store) WorkspaceID
		url, label string
		notFound   bool
	}{
		{name: "an unregistered workspace is not found", ws: func(*store) WorkspaceID { return "nope" }, url: linkURL(1), label: "auto label", notFound: true},
		{name: "no url is a defect", ws: func(s *store) WorkspaceID { return testWorkspace(t, s).ID }, url: "", label: "auto label"},
		{name: "no label is a defect", ws: func(s *store) WorkspaceID { return testWorkspace(t, s).ID }, url: linkURL(1), label: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, _ := testStore(t)
			id := tt.ws(s)

			// Act.
			added, err := s.AddWorkspaceLinkIfAbsent(context.Background(), id, tt.url, tt.label, nil)

			// Assert.
			if added || err == nil || (tt.notFound && !errors.Is(err, ErrNotFound)) {
				t.Fatalf("AddWorkspaceLinkIfAbsent = %v, %v; want a refusal and nothing stored", added, err)
			}
		})
	}
}
