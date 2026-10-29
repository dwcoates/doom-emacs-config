package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/memorypopup"
)

func residencyNotLoaded() *agentreplv1.PageResidency {
	return &agentreplv1.PageResidency{State: &agentreplv1.PageResidency_NotLoaded{NotLoaded: &agentreplv1.PageResidencyNotLoaded{}}}
}

func TestReportPageResidencyRecordsTheWorkspacesResidency(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.ReportPageResidency(context.Background(), connect.NewRequest(&agentreplv1.ReportPageResidencyRequest{
		Workspace: ref(), Residency: residencyNotLoaded(),
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportPageResidency = (%v, %v), want success", resp, err)
	}
	if h.Memory.reports[testWorkspaceID].GetNotLoaded() == nil {
		t.Fatalf("recorded = %v, want the not-loaded residency", h.Memory.reports)
	}
}

func TestReportPageResidencyRefusesAMalformedResidency(t *testing.T) {
	tests := []struct {
		name      string
		residency *agentreplv1.PageResidency
	}{
		{"no residency", nil},
		{"no state", &agentreplv1.PageResidency{}},
		{"loaded with no release rule", &agentreplv1.PageResidency{State: &agentreplv1.PageResidency_Loaded{Loaded: &agentreplv1.PageResidencyLoaded{}}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.ReportPageResidency(context.Background(), connect.NewRequest(&agentreplv1.ReportPageResidencyRequest{
				Workspace: ref(), Residency: tc.residency,
			}))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Memory.reports) != 0 {
				t.Fatalf("err = %v, reports %v: want InvalidArgument and nothing recorded", err, h.Memory.reports)
			}
		})
	}
}

func TestReportPageResidencyRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.ReportPageResidency(context.Background(), connect.NewRequest(&agentreplv1.ReportPageResidencyRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}, Residency: residencyNotLoaded(),
	}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("ReportPageResidency = (%v, %v), want unknown_workspace", resp, err)
	}
}

func TestReportPageResidencyRefusedByThePopupsIsInternal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Memory.reportErr = errors.New("refused")

	// Act
	_, err := h.Client.ReportPageResidency(context.Background(), connect.NewRequest(&agentreplv1.ReportPageResidencyRequest{
		Workspace: ref(), Residency: residencyNotLoaded(),
	}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("err = %v, want Internal", err)
	}
}

func TestGetMemoryOverviewAnswersTheMeasuredOverview(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Memory.overview = &frontendv1.SidebarMemoryOverview{Total: &frontendv1.SidebarMemoryTotal{FootprintBytes: 7}}

	// Act
	resp, err := h.Client.GetMemoryOverview(context.Background(), connect.NewRequest(&agentreplv1.GetMemoryOverviewRequest{}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetMeasured().GetTotal().GetFootprintBytes() != 7 {
		t.Fatalf("GetMemoryOverview = (%v, %v), want the measured overview", resp, err)
	}
}

func TestGetMemoryOverviewBeforeAnySampleIsNotYetMeasured(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.GetMemoryOverview(context.Background(), connect.NewRequest(&agentreplv1.GetMemoryOverviewRequest{}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetNotYetMeasured() == nil {
		t.Fatalf("GetMemoryOverview = (%v, %v), want not_yet_measured", resp, err)
	}
}

func TestGetMemoryOverviewAFailedSampleIsTheMeasurementFailedArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Memory.overErr = memorypopup.ErrMeasurementFailed

	// Act
	resp, err := h.Client.GetMemoryOverview(context.Background(), connect.NewRequest(&agentreplv1.GetMemoryOverviewRequest{}))

	// Assert
	if err != nil || resp.Msg.GetError().GetMeasurementFailed() == nil {
		t.Fatalf("GetMemoryOverview = (%v, %v), want measurement_failed", resp, err)
	}
}

func TestGetMemoryOverviewAnyOtherFailureIsInternal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Memory.overErr = errors.New("broken")

	// Act
	_, err := h.Client.GetMemoryOverview(context.Background(), connect.NewRequest(&agentreplv1.GetMemoryOverviewRequest{}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("err = %v, want Internal", err)
	}
}

func TestGetMemoryOverviewCountsOnlyRegisteredOpenWorkspaces(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.Client.GetMemoryOverview(context.Background(), connect.NewRequest(&agentreplv1.GetMemoryOverviewRequest{})); err != nil {
		t.Fatalf("GetMemoryOverview: %v", err)
	}

	// Assert
	if !h.Memory.open(testWorkspaceID) || h.Memory.open("ws-nope") {
		t.Fatalf("the open set did not name exactly the registered open workspace")
	}
}

func TestGetWorkspaceMemoryAnswersTheWorkspacesLines(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Memory.workspace = &frontendv1.SidebarWorkspaceMemory{Agent: &frontendv1.SidebarWorkspaceMemoryAgent{
		State: &frontendv1.SidebarWorkspaceMemoryAgent_NotRunning{NotRunning: &frontendv1.SidebarWorkspaceMemoryAgentNotRunning{}},
	}}

	// Act
	resp, err := h.Client.GetWorkspaceMemory(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceMemoryRequest{Workspace: ref()}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetMemory().GetAgent().GetNotRunning() == nil {
		t.Fatalf("GetWorkspaceMemory = (%v, %v), want the workspace's lines", resp, err)
	}
}

func TestGetWorkspaceMemoryRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.GetWorkspaceMemory(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceMemoryRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
	}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("GetWorkspaceMemory = (%v, %v), want unknown_workspace", resp, err)
	}
}

func TestGetWorkspaceMemoryRefusesNoWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.GetWorkspaceMemory(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceMemoryRequest{}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
}
