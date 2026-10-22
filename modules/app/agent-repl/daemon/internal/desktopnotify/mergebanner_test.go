package desktopnotify

import (
	"testing"
)

func newMergeBanners() (*MergeBanners, *fakePoster) {
	poster := &fakePoster{}
	return NewMergeBanners(poster), poster
}

func TestMergeBannersQueuedPostsTheQueuedBanner(t *testing.T) {
	// Arrange
	m, poster := newMergeBanners()

	// Act
	m.Queued("ws1", "feature → master")

	// Assert
	want := Banner{Title: "🔀 my-ws", Body: "feature → master"}
	if len(poster.banners) != 1 || poster.banners[0] != want || poster.kinds[0] != KindMergeQueued || poster.workspaces[0] != "ws1" {
		t.Fatalf("posted %+v (kinds %v, workspaces %v), want %+v as %s for ws1", poster.banners, poster.kinds, poster.workspaces, want, KindMergeQueued)
	}
}

func TestMergeBannersResolvedPostsEachEnd(t *testing.T) {
	cases := []struct {
		name      string
		end       MergeEnd
		wantTitle string
	}{
		{name: "landed", end: MergeLanded, wantTitle: "🎉 my-ws"},
		{name: "failed", end: MergeFailed, wantTitle: "❌ my-ws"},
		{name: "abandoned", end: MergeAbandoned, wantTitle: "⛔ my-ws"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, poster := newMergeBanners()

			// Act
			m.Resolved("ws1", tc.end, "the summary")

			// Assert
			want := Banner{Title: tc.wantTitle, Body: "the summary"}
			if len(poster.banners) != 1 || poster.banners[0] != want || poster.kinds[0] != KindMergeResolved || poster.workspaces[0] != "ws1" {
				t.Fatalf("posted %+v (kinds %v), want %+v as %s", poster.banners, poster.kinds, want, KindMergeResolved)
			}
		})
	}
}

func TestMergeBannersResolvedPanicsOnAnUnknownEnd(t *testing.T) {
	// Arrange
	m, poster := newMergeBanners()

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

func TestNewMergeBannersRefusesAMissingPoster(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("NewMergeBanners accepted a missing poster")
		}
	}()

	// Act
	NewMergeBanners(nil)
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
