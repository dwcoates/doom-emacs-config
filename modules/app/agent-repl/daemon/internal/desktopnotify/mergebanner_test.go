package desktopnotify

import (
	"testing"

	"agent-repld/internal/dlog"
)

func newMergeBanners() (*MergeBanners, *fakePoster, *dlog.TestLogger) {
	poster := &fakePoster{}
	log := dlog.NewTestLogger()
	return NewMergeBanners(poster, log), poster, log
}

func TestMergeBannersResolvedPostsEachEndThatDidNotLand(t *testing.T) {
	cases := []struct {
		name      string
		end       MergeEnd
		wantTitle string
	}{
		{name: "failed", end: MergeFailed, wantTitle: "❌ my-ws"},
		{name: "abandoned", end: MergeAbandoned, wantTitle: "⛔ my-ws"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, poster, _ := newMergeBanners()

			// Act
			m.Resolved("ws1", tc.end, "the summary")

			// Assert
			want := Banner{Title: tc.wantTitle, Body: "the summary"}
			if len(poster.banners) != 1 || poster.banners[0] != want || poster.kinds[0] != KindMergeResolved || poster.workspaces[0] != "ws1" || poster.regardless[0] {
				t.Fatalf("posted %+v (kinds %v), want %+v as %s", poster.banners, poster.kinds, want, KindMergeResolved)
			}
		})
	}
}

func TestMergeBannersResolvedPanicsOnAnUnknownEnd(t *testing.T) {
	// Arrange
	m, poster, _ := newMergeBanners()

	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unknown merge end was accepted")
		}
		if len(poster.banners) != 0 {
			t.Fatalf("posted %d banners for an unknown end, want none", len(poster.banners))
		}
	}()

	// Act
	m.Resolved("ws1", MergeEnd(99), "the summary")
}

func TestMergeBannersResolvedRaisesNothingForALandedMerge(t *testing.T) {
	// Arrange
	m, poster, log := newMergeBanners()

	// Act
	m.Resolved("ws1", MergeLanded, "the summary")

	// Assert
	if len(poster.banners) != 0 {
		t.Fatalf("posted %+v for a landed merge, want nothing", poster.banners)
	}
	if r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "a landed merge raises no desktop banner"}); !ok || r.Context["workspace"] != "ws1" {
		t.Fatalf("record = %+v (found=%v), want the landed merge's silence recorded for ws1", r, ok)
	}
}

func TestNewMergeBannersRefusesAMissingCollaborator(t *testing.T) {
	cases := []struct {
		name   string
		poster Poster
		log    dlog.Logger
	}{
		{name: "no poster", log: dlog.NewTestLogger()},
		{name: "no logger", poster: &fakePoster{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			defer func() {
				if recover() == nil {
					t.Fatal("NewMergeBanners accepted a missing collaborator")
				}
			}()

			// Act
			NewMergeBanners(tc.poster, tc.log)
		})
	}
}

func TestMergeEndString(t *testing.T) {
	cases := []struct {
		end  MergeEnd
		want string
	}{
		{end: MergeLanded, want: "landed"},
		{end: MergeFailed, want: "failed"},
		{end: MergeAbandoned, want: "abandoned"},
		{end: MergeEnd(7), want: "MergeEnd(7)"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act
			got := tc.end.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestMergeBannersMediatedPostsTheMediatorsBanner(t *testing.T) {
	// Arrange
	m, poster, _ := newMergeBanners()

	// Act
	m.Mediated("ws1", "the deploy's page boots regressed")

	// Assert
	want := Banner{Title: "🩺 my-ws", Body: "the deploy's page boots regressed"}
	if len(poster.banners) != 1 || poster.banners[0] != want || poster.kinds[0] != KindDeployMediated || poster.workspaces[0] != "ws1" || !poster.regardless[0] {
		t.Fatalf("posted %+v (kinds %v, workspaces %v, regardless %v), want %+v as %s for ws1, regardless of focus", poster.banners, poster.kinds, poster.workspaces, poster.regardless, want, KindDeployMediated)
	}
}

func TestDeployMediatedTitleLeadsWithTheMediationMark(t *testing.T) {
	// Act
	got := DeployMediatedTitle("deploy-mediator")

	// Assert
	if got != "🩺 deploy-mediator" {
		t.Fatalf("DeployMediatedTitle = %q, want the mediation mark before the mediator's name", got)
	}
}
