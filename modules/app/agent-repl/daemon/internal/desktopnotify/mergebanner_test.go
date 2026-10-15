package desktopnotify

import (
	"testing"
	"time"
)

var mergeAt = time.Date(2026, 10, 12, 9, 41, 0, 0, time.Local)

func newMergeBanners() (*MergeBanners, *fakePoster) {
	poster := &fakePoster{}
	return NewMergeBanners(poster, func() time.Time { return mergeAt }), poster
}

func TestMergeBannersQueuedPostsTheQueuedBanner(t *testing.T) {
	// Arrange
	m, poster := newMergeBanners()

	// Act
	m.Queued("ws1", "feature → master")

	// Assert
	want := Banner{Title: "🔀 my-ws merge queued 09:41", Body: "feature → master"}
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
		{name: "landed", end: MergeLanded, wantTitle: "🎉 my-ws merge landed 09:41"},
		{name: "failed", end: MergeFailed, wantTitle: "❌ my-ws merge failed 09:41"},
		{name: "abandoned", end: MergeAbandoned, wantTitle: "⛔ my-ws merge abandoned 09:41"},
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

func TestNewMergeBannersRefusesAMissingCollaborator(t *testing.T) {
	cases := []struct {
		name   string
		poster Poster
		now    func() time.Time
	}{
		{name: "no poster", now: time.Now},
		{name: "no clock", poster: &fakePoster{}},
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
			NewMergeBanners(tc.poster, tc.now)
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
