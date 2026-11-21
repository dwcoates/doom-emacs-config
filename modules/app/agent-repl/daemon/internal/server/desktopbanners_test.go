package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

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
