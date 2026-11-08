package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opDesktopBanners is the operation the notifier app's endpoints carry.
const opDesktopBanners = "daemon.server.desktop_banners"

// NotifierApp is the macOS notifier app's side of the daemon
// (desktopnotify.App): the stream its banners ride, and the posts awaiting
// its answers.
type NotifierApp interface {
	// Attach stands pid's stream, superseding any earlier one, answering the
	// release its handler calls when the stream ends.
	Attach(pid int) (*desktopnotify.AppStream, func())
	// Answer settles one post with the app's report.
	Answer(postID uint64, outcome desktopnotify.PostOutcome) error
}

// WatchDesktopBanners serves the notifier app's standing stream
// (endpoint_watch_desktop_banners.proto): `attached` first, then each banner
// the notifier hands it, until the app goes away, a later app supersedes it,
// or the daemon stands down.
func (s *server) WatchDesktopBanners(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchDesktopBannersRequest],
	out *connect.ServerStream[agentreplv1.WatchDesktopBannersResponse],
) error {
	return serveThenEnd(s, "WatchDesktopBanners", out, desktopBannersEnding(), func() error {
		return s.watchDesktopBanners(ctx, req.Msg, out)
	})
}

// desktopBannersEnding is the banner stream's planned ending frame.
func desktopBannersEnding() *agentreplv1.WatchDesktopBannersResponse {
	return &agentreplv1.WatchDesktopBannersResponse{
		Frame: &agentreplv1.WatchDesktopBannersResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
	}
}

// watchDesktopBanners is the body.
func (s *server) watchDesktopBanners(
	ctx context.Context,
	msg *agentreplv1.WatchDesktopBannersRequest,
	out streamSink[agentreplv1.WatchDesktopBannersResponse],
) error {
	const rpc = "WatchDesktopBanners"
	pid := msg.GetProcess().GetPid()
	if msg.GetProcess() == nil || pid <= 0 {
		return invalid("process.pid", "the notifier app's positive pid is required")
	}
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()
	stream, release := s.deps.NotifierApp.Attach(int(pid))
	defer release()
	s.acceptStream(ctx, rpc)
	log := s.log.With(dlog.Context{"rpc": rpc, "pid": pid})
	if err := out.Send(&agentreplv1.WatchDesktopBannersResponse{
		Frame: &agentreplv1.WatchDesktopBannersResponse_Attached{Attached: &agentreplv1.WatchDesktopBannersAttached{}},
	}); err != nil {
		log.Info(opDesktopBanners, "the notifier app went away before its stream was acknowledged", dlog.Context{"cause": err.Error()})
		return nil
	}
	for {
		select {
		case <-streamCtx.Done():
			log.Debug(opDesktopBanners, "the banner stream ended on cancellation", nil)
			return nil
		case <-stream.Superseded():
			log.Info(opDesktopBanners, "a later notifier app superseded this banner stream; telling it so", nil)
			if err := out.Send(&agentreplv1.WatchDesktopBannersResponse{
				Frame: &agentreplv1.WatchDesktopBannersResponse_Superseded{Superseded: &agentreplv1.WatchDesktopBannersSuperseded{}},
			}); err != nil {
				log.Info(opDesktopBanners, "the superseded notifier app went away before it was told", dlog.Context{"cause": err.Error()})
			}
			return nil
		case banner := <-stream.Banners():
			if err := out.Send(&agentreplv1.WatchDesktopBannersResponse{
				Frame: &agentreplv1.WatchDesktopBannersResponse_Banner{Banner: desktopBannerView(banner)},
			}); err != nil {
				log.Info(opDesktopBanners, "the notifier app went away while a banner was sent", dlog.Context{
					"post": banner.PostID, "cause": err.Error(),
				})
				return nil
			}
		}
	}
}

// desktopBannerView is the wire form of one banner handed to the app.
func desktopBannerView(b desktopnotify.AppBanner) *agentreplv1.DesktopBanner {
	return &agentreplv1.DesktopBanner{
		PostId: &agentreplv1.DesktopBannerPostId{Value: b.PostID},
		Title:  &agentreplv1.DesktopBannerTitle{Text: b.Banner.Title},
		Body:   &agentreplv1.DesktopBannerBody{Text: b.Banner.Body},
		Group:  &agentreplv1.DesktopBannerGroup{Id: b.Group},
		Click:  &agentreplv1.DesktopBannerClickToken{Workspace: wsm.FamilyRef{ID: b.Target.ID, Dir: b.Target.Dir}.Ref()},
	}
}

// ReportDesktopBannerPosted settles one post with what the platform did with
// it (endpoint_report_desktop_banner_posted.proto).
func (s *server) ReportDesktopBannerPosted(
	_ context.Context,
	req *connect.Request[agentreplv1.ReportDesktopBannerPostedRequest],
) (*connect.Response[agentreplv1.ReportDesktopBannerPostedResponse], error) {
	postID := req.Msg.GetPostId().GetValue()
	if postID == 0 {
		return nil, invalid("post_id", "the post's id, as the stream handed it, is required")
	}
	outcome, err := postOutcome(req.Msg.GetOutcome())
	if err != nil {
		return nil, err
	}
	resp := &agentreplv1.ReportDesktopBannerPostedResponse{}
	if err := s.deps.NotifierApp.Answer(postID, outcome); err != nil {
		if !errors.Is(err, desktopnotify.ErrUnknownPost) {
			panic("server: ReportDesktopBannerPosted: an unrecognized answer refusal: " + err.Error())
		}
		resp.Result = &agentreplv1.ReportDesktopBannerPostedResponse_Error{Error: &agentreplv1.ReportDesktopBannerPostedError{
			Cause: &agentreplv1.ReportDesktopBannerPostedError_UnknownPost{UnknownPost: &agentreplv1.ReportDesktopBannerPostedUnknownPost{}},
		}}
		return connect.NewResponse(resp), nil
	}
	resp.Result = &agentreplv1.ReportDesktopBannerPostedResponse_Success{Success: &agentreplv1.ReportDesktopBannerPostedSuccess{}}
	return connect.NewResponse(resp), nil
}

// postOutcome reads the outcome arm.
func postOutcome(outcome *agentreplv1.DesktopBannerPostOutcome) (desktopnotify.PostOutcome, *connect.Error) {
	switch arm := outcome.GetOutcome().(type) {
	case *agentreplv1.DesktopBannerPostOutcome_Posted:
		return desktopnotify.PostOutcome{Posted: true}, nil
	case *agentreplv1.DesktopBannerPostOutcome_NotAllowed:
		return desktopnotify.PostOutcome{NotAllowed: true}, nil
	case *agentreplv1.DesktopBannerPostOutcome_Failed:
		if arm.Failed.GetPlatformError() == "" {
			return desktopnotify.PostOutcome{}, invalid("outcome.failed.platform_error", "a refused post names the platform's error")
		}
		return desktopnotify.PostOutcome{PlatformError: arm.Failed.GetPlatformError()}, nil
	case nil:
		return desktopnotify.PostOutcome{}, invalid("outcome", "what the platform did with the banner is required")
	default:
		panic(fmt.Sprintf("server: ReportDesktopBannerPosted: an unknown outcome arm %T", arm))
	}
}

// ReportDesktopBannerClicked relays a banner's click
// (endpoint_report_desktop_banner_clicked.proto): every held banner is let go
// and Emacs is told to select the token's workspace.
func (s *server) ReportDesktopBannerClicked(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportDesktopBannerClickedRequest],
) (*connect.Response[agentreplv1.ReportDesktopBannerClickedResponse], error) {
	const rpc = "ReportDesktopBannerClicked"
	ref := req.Msg.GetClick().GetWorkspace()
	if err := validateWorkspaceRef("click.workspace", ref); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ReportDesktopBannerClickedResponse{}
	subject, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, ref, nil)
	if done {
		return answer(resp, refused)
	}
	s.deps.Banners.Clicked(subject.Record.ID)
	resp.Result = &agentreplv1.ReportDesktopBannerClickedResponse_Success{Success: &agentreplv1.ReportDesktopBannerClickedSuccess{}}
	return connect.NewResponse(resp), nil
}
