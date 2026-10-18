package server

import (
	"context"
	"math"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

const (
	switchRepeat = agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_SWITCH_REPEAT
	pageBoot     = agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_PAGE_BOOT
)

func latency(kind agentreplv1.ReportedLatencyKind, ws *workspacev1.WorkspaceRef, seconds float64) *agentreplv1.ReportedLatency {
	return &agentreplv1.ReportedLatency{Kind: kind, Workspace: ws, Seconds: seconds}
}

func reportLatencies(t *testing.T, h *harness, latencies ...*agentreplv1.ReportedLatency) (*agentreplv1.ReportLatenciesResponse, error) {
	t.Helper()
	resp, err := h.Client.ReportLatencies(context.Background(), connect.NewRequest(&agentreplv1.ReportLatenciesRequest{Latencies: latencies}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestReportLatenciesRecordsEveryLatencyOfAKnownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportLatencies(t, h, latency(switchRepeat, ref(), 0.08), latency(pageBoot, ref(), 0.4))

	// Assert
	if err != nil || resp.GetSuccess().GetRecorded().GetRecorded() != 2 {
		t.Fatalf("ReportLatencies = %v, %v; want both recorded", resp, err)
	}
	got := h.Latencies.all()
	if len(got) != 2 || got[0] != (observedLatency{testWorkspaceID, "switch_repeat", 0.08}) || got[1].kind != "page_boot" {
		t.Fatalf("observed = %+v", got)
	}
}

func TestReportLatenciesDropsAndCountsAWorkspaceTheRegistryNoLongerHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	gone := &workspacev1.WorkspaceRef{Id: "closed-and-forgotten"}

	// Act
	resp, err := reportLatencies(t, h, latency(switchRepeat, ref(), 0.08), latency(switchRepeat, gone, 0.09), latency(pageBoot, gone, 1))

	// Assert
	partly := resp.GetSuccess().GetPartlyRecorded()
	if err != nil || partly.GetRecorded() != 1 || partly.GetDroppedUnknownWorkspace() != 2 {
		t.Fatalf("ReportLatencies = %v, %v; want 1 recorded and 2 dropped", resp, err)
	}
	if got := h.Latencies.all(); len(got) != 1 || got[0].workspace != testWorkspaceID {
		t.Fatalf("observed = %+v, want only the known workspace's", got)
	}
}

func TestReportLatenciesRefusesTheWholeBatchOnARefWhoseDirDisagrees(t *testing.T) {
	// Arrange
	h := newHarness(t)
	wrong := &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"}

	// Act
	resp, err := reportLatencies(t, h, latency(pageBoot, wrong, 0.4))

	// Assert
	mismatch := resp.GetError().GetWorkspaceRefMismatch()
	if err != nil || mismatch.GetRegistryDir() != testWorkspaceDir || mismatch.GetReportedDir() != "/elsewhere" || mismatch.GetWorkspaceId() != string(testWorkspaceID) {
		t.Fatalf("ReportLatencies = %v, %v; want the mismatch refusal with its evidence", resp, err)
	}
	if got := h.Latencies.all(); len(got) != 0 {
		t.Fatalf("a refused batch recorded %+v", got)
	}
}

func TestReportLatenciesSaysSoWhenTheMetricsSurfaceIsOff(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Latencies.enabled = false

	// Act
	resp, err := reportLatencies(t, h, latency(pageBoot, ref(), 0.4))

	// Assert
	if err != nil || resp.GetSuccess().GetMetricsOff() == nil {
		t.Fatalf("ReportLatencies = %v, %v; want metrics_off", resp, err)
	}
	if got := h.Latencies.all(); len(got) != 0 {
		t.Fatalf("the off surface recorded %+v", got)
	}
}

func TestReportLatenciesAcceptsAnEmptyBatch(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportLatencies(t, h)

	// Assert
	if err != nil || resp.GetSuccess().GetRecorded() == nil || resp.GetSuccess().GetRecorded().GetRecorded() != 0 {
		t.Fatalf("ReportLatencies = %v, %v; want an empty recorded answer", resp, err)
	}
}

func TestReportLatenciesRefusesAMalformedLatency(t *testing.T) {
	cases := []struct {
		name    string
		latency *agentreplv1.ReportedLatency
	}{
		{name: "no workspace", latency: latency(pageBoot, nil, 0.4)},
		{name: "a blank workspace id", latency: latency(pageBoot, &workspacev1.WorkspaceRef{}, 0.4)},
		{name: "an unspecified kind", latency: latency(agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_UNSPECIFIED, ref(), 0.4)},
		{name: "a negative duration", latency: latency(pageBoot, ref(), -0.1)},
		{name: "a non-finite duration", latency: latency(pageBoot, ref(), math.Inf(1))},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := reportLatencies(t, h, latency(pageBoot, ref(), 0.2), tc.latency)

			// Assert
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
			if got := h.Latencies.all(); len(got) != 0 {
				t.Fatalf("a malformed batch recorded %+v", got)
			}
		})
	}
}
