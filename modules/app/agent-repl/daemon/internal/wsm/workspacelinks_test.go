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
			got, err = s.SetWorkspaceLink(ctx, ws, step.set.URL, step.set.Label, step.set.Owned)
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
				_, err := s.SetWorkspaceLink(context.Background(), ws, linkURL(2), "", false)
				return err
			}, isDefect: true, errorOp: "daemon.wsm.set_workspace_link"},
		{name: "a set on an unknown workspace is not found",
			act: func(s *store, _ WorkspaceID) error {
				_, err := s.SetWorkspaceLink(context.Background(), WorkspaceID("absent"), linkURL(2), "two a", false)
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

// hasErrorRecord reports whether an error record of OP names WORKSPACE.
func hasErrorRecord(records []dlog.Record, op, workspace string) bool {
	for _, r := range dlog.FilterRecords(records, dlog.RecordQuery{Level: "error", Operation: op}) {
		if r.Context["workspace"] == workspace {
			return true
		}
	}
	return false
}
