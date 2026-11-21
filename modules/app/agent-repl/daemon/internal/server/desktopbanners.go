package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// ReportDesktopBannerClicked relays a banner's click, the one call the
// launcher (Zettel.app) makes on the daemon
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
