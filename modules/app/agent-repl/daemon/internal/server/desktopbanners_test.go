package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/desktopnotify"
)

// bannerStream opens the notifier app's banner stream and reads its first
// frame, which must be `attached`.
func bannerStream(t *testing.T, h *harness, ctx context.Context, pid int32) *connect.ServerStreamForClient[agentreplv1.WatchDesktopBannersResponse] {
	t.Helper()
	stream, err := h.Client.WatchDesktopBanners(ctx, connect.NewRequest(&agentreplv1.WatchDesktopBannersRequest{
		Process: &agentreplv1.DesktopNotifierProcess{Pid: pid},
		Build:   &agentreplv1.DesktopNotifierBuild{Sha256: "build-1"},
	}))
	if err != nil {
		t.Fatalf("open the banner stream: %v", err)
	}
	if !stream.Receive() || stream.Msg().GetAttached() == nil {
		t.Fatalf("first frame = %v (err %v), want attached", stream.Msg(), stream.Err())
	}
	return stream
}

// postOne posts one banner through the harness's notifier app on its own
// goroutine, answering what Post came to.
func postOne(h *harness) <-chan error {
	done := make(chan error, 1)
	go func() {
		_, err := h.NotifierApp.Post(context.Background(),
			desktopnotify.Target{ID: testWorkspaceID, Dir: ref().GetDir()},
			desktopnotify.Banner{Title: "✅ ws", Body: "done"}, func() {})
		done <- err
	}()
	return done
}

func reportPosted(h *harness, postID uint64, outcome *agentreplv1.DesktopBannerPostOutcome) (*connect.Response[agentreplv1.ReportDesktopBannerPostedResponse], error) {
	return h.Client.ReportDesktopBannerPosted(context.Background(), connect.NewRequest(&agentreplv1.ReportDesktopBannerPostedRequest{
		PostId: &agentreplv1.DesktopBannerPostId{Value: postID}, Outcome: outcome,
	}))
}

func postedOutcome() *agentreplv1.DesktopBannerPostOutcome {
	return &agentreplv1.DesktopBannerPostOutcome{Outcome: &agentreplv1.DesktopBannerPostOutcome_Posted{Posted: &agentreplv1.DesktopBannerPostOutcomePosted{}}}
}

func TestWatchDesktopBannersHandsEachBannerAndItsAnswerSettlesThePost(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := bannerStream(t, h, ctx, 4242)

	// Act
	done := postOne(h)
	if !stream.Receive() {
		t.Fatalf("no banner frame: %v", stream.Err())
	}
	banner := stream.Msg().GetBanner()
	resp, err := reportPosted(h, banner.GetPostId().GetValue(), postedOutcome())

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportDesktopBannerPosted = %v, %v; want success", resp, err)
	}
	if err := <-done; err != nil {
		t.Fatalf("Post = %v, want the reported post settled", err)
	}
	if banner.GetTitle().GetText() != "✅ ws" || banner.GetBody().GetText() != "done" {
		t.Fatalf("banner text = %q / %q", banner.GetTitle().GetText(), banner.GetBody().GetText())
	}
	if banner.GetClick().GetWorkspace().GetId() != string(testWorkspaceID) || banner.GetGroup().GetId() == "" {
		t.Fatalf("banner = %v, want its click token naming the workspace and a group", banner)
	}
}

func TestWatchDesktopBannersRefusesAStreamWithNoPid(t *testing.T) {
	for _, process := range []*agentreplv1.DesktopNotifierProcess{nil, {Pid: 0}, {Pid: -1}} {
		// Arrange
		h := newHarness(t)

		// Act
		stream, err := h.Client.WatchDesktopBanners(context.Background(), connect.NewRequest(&agentreplv1.WatchDesktopBannersRequest{
			Process: process, Build: &agentreplv1.DesktopNotifierBuild{Sha256: "b"},
		}))
		if err == nil {
			stream.Receive()
			err = stream.Err()
		}

		// Assert
		if connect.CodeOf(err) != connect.CodeInvalidArgument {
			t.Fatalf("process %v: err = %v, want InvalidArgument", process, err)
		}
	}
}

func TestWatchDesktopBannersRefusesAStreamWithNoBuild(t *testing.T) {
	for _, build := range []*agentreplv1.DesktopNotifierBuild{nil, {Sha256: ""}} {
		// Arrange
		h := newHarness(t)

		// Act
		stream, err := h.Client.WatchDesktopBanners(context.Background(), connect.NewRequest(&agentreplv1.WatchDesktopBannersRequest{
			Process: &agentreplv1.DesktopNotifierProcess{Pid: 1}, Build: build,
		}))
		if err == nil {
			stream.Receive()
			err = stream.Err()
		}

		// Assert
		if connect.CodeOf(err) != connect.CodeInvalidArgument {
			t.Fatalf("build %v: err = %v, want InvalidArgument", build, err)
		}
	}
}

func TestWatchDesktopBannersStandsTheReportedBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	bannerStream(t, h, ctx, 9)

	// Assert
	if build, attached := h.NotifierApp.RunningBuild(); !attached || build != "build-1" {
		t.Fatalf("RunningBuild = %q, %v; want the stream's build-1", build, attached)
	}
}

func TestWatchDesktopBannersTellsASupersededStreamSo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	first := bannerStream(t, h, ctx, 1)

	// Act
	bannerStream(t, h, ctx, 2)

	// Assert
	if !first.Receive() || first.Msg().GetSuperseded() == nil {
		t.Fatalf("first stream's frame = %v (err %v), want superseded", first.Msg(), first.Err())
	}
	if first.Receive() {
		t.Fatalf("the superseded stream carried a frame after superseded: %v", first.Msg())
	}
}

func TestReportDesktopBannerPostedMapsEachOutcome(t *testing.T) {
	cases := []struct {
		name    string
		outcome *agentreplv1.DesktopBannerPostOutcome
		check   func(error) bool
	}{
		{name: "posted", outcome: postedOutcome(), check: func(err error) bool { return err == nil }},
		{name: "not allowed", outcome: &agentreplv1.DesktopBannerPostOutcome{Outcome: &agentreplv1.DesktopBannerPostOutcome_NotAllowed{NotAllowed: &agentreplv1.DesktopBannerPostOutcomeNotAllowed{}}},
			check: func(err error) bool { return errors.Is(err, desktopnotify.ErrNotAllowed) }},
		{name: "failed", outcome: &agentreplv1.DesktopBannerPostOutcome{Outcome: &agentreplv1.DesktopBannerPostOutcome_Failed{Failed: &agentreplv1.DesktopBannerPostOutcomeFailed{PlatformError: "boom"}}},
			check: func(err error) bool {
				return err != nil && errors.Unwrap(err) == nil && err.Error() == "macOS refused the banner: boom"
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := bannerStream(t, h, ctx, 7)
			done := postOne(h)
			if !stream.Receive() {
				t.Fatalf("no banner frame: %v", stream.Err())
			}

			// Act
			resp, err := reportPosted(h, stream.Msg().GetBanner().GetPostId().GetValue(), tc.outcome)

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("ReportDesktopBannerPosted = %v, %v", resp, err)
			}
			if got := <-done; !tc.check(got) {
				t.Fatalf("Post = %v, want the %s outcome", got, tc.name)
			}
		})
	}
}

func TestReportDesktopBannerPostedRefusesAnUnknownPost(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportPosted(h, 99, postedOutcome())

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownPost() == nil {
		t.Fatalf("ReportDesktopBannerPosted = %v, %v; want unknown_post", resp, err)
	}
}

func TestReportDesktopBannerPostedRefusesAMalformedRequest(t *testing.T) {
	cases := []struct {
		name    string
		postID  uint64
		outcome *agentreplv1.DesktopBannerPostOutcome
	}{
		{name: "no post id", postID: 0, outcome: postedOutcome()},
		{name: "no outcome", postID: 1, outcome: nil},
		{name: "a failure naming no error", postID: 1, outcome: &agentreplv1.DesktopBannerPostOutcome{Outcome: &agentreplv1.DesktopBannerPostOutcome_Failed{Failed: &agentreplv1.DesktopBannerPostOutcomeFailed{}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := reportPosted(h, tc.postID, tc.outcome)

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
		})
	}
}

func reportClicked(h *harness, workspace *workspacev1.WorkspaceRef) (*connect.Response[agentreplv1.ReportDesktopBannerClickedResponse], error) {
	return h.Client.ReportDesktopBannerClicked(context.Background(), connect.NewRequest(&agentreplv1.ReportDesktopBannerClickedRequest{
		Click: &agentreplv1.DesktopBannerClickToken{Workspace: workspace},
	}))
}

func TestReportDesktopBannerClickedRelaysTheClickForTheTokensWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportClicked(h, ref())

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportDesktopBannerClicked = %v, %v; want success", resp, err)
	}
	if got := h.Banners.clicks(); len(got) != 1 || got[0] != testWorkspaceID {
		t.Fatalf("clicks = %v, want [%s]", got, testWorkspaceID)
	}
}

func TestReportDesktopBannerClickedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportClicked(h, &workspacev1.WorkspaceRef{Id: "0000000000000000", Dir: "/nowhere"})

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("ReportDesktopBannerClicked = %v, %v; want unknown_workspace", resp, err)
	}
	if got := h.Banners.clicks(); len(got) != 0 {
		t.Fatalf("clicks = %v, want none relayed", got)
	}
}

func TestReportDesktopBannerClickedRefusesATokenWithNoWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := reportClicked(h, nil)

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
}
