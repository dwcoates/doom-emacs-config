//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

func degradedWindowWarning(v *frontendv1.TopbarView, closed bool, began int64) *frontendv1.TopbarDegradedWindowWarningDetail {
	for _, warning := range v.GetWarnings().GetWarnings() {
		window := warning.GetDegradedWindow()
		if window == nil || window.GetBeganAtMs() != began {
			continue
		}
		if (window.GetClosed() != nil) == closed {
			return window
		}
	}
	return nil
}

func TestADegradedWindowTheShimOpensIsDrawnOpenAndThenClosedWithItsDroppedCount(t *testing.T) {
	t.Parallel()
	// Arrange: the open fault the unhealthy push raises is the subject.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.health.open_fault")
	topbar := f.d.WatchTopbar(f.ws)
	const began = int64(1_700_000_000_000)
	window := func(extent func(*conversationv1.SessionDegradedWindow)) *conversationv1.SessionDegradedWindow {
		w := &conversationv1.SessionDegradedWindow{Component: "converter", Reason: "could not model a record", BeganAtMs: began}
		extent(w)
		return w
	}

	// Act: the shim reports the window open while unhealthy.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Unhealthy{Unhealthy: &conversationv1.SessionUnhealthy{Faults: []*conversationv1.SessionFault{{
			Component: "converter",
			Detail:    "could not model a record",
			Kind:      &conversationv1.SessionFault_ConverterDefect{ConverterDefect: &conversationv1.SessionFaultConverterDefect{}},
		}}}},
		DegradedWindows: []*conversationv1.SessionDegradedWindow{window(func(w *conversationv1.SessionDegradedWindow) {
			w.Extent = &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}}
		})},
	}}})

	// Assert: the warning is drawn as an OPEN window.
	awaitTopbar(t, f, topbar, "the open degraded window", func(v *frontendv1.TopbarView) bool {
		return degradedWindowWarning(v, false, began) != nil
	})

	// Act: the next clean turn returns the shim to healthy with the window closed.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
		DegradedWindows: []*conversationv1.SessionDegradedWindow{window(func(w *conversationv1.SessionDegradedWindow) {
			w.Extent = &conversationv1.SessionDegradedWindow_Closed{Closed: &conversationv1.SessionDegradedClosed{EndedAtMs: began + 5, DroppedCount: 1}}
		})},
	}}})

	// Assert: THAT window (same component and start) is drawn closed, with its figures.
	view := awaitTopbar(t, f, topbar, "the same degraded window closed", func(v *frontendv1.TopbarView) bool {
		return degradedWindowWarning(v, true, began) != nil
	})
	closed := degradedWindowWarning(view, true, began).GetClosed()
	if closed.GetEndedAtMs() != began+5 || closed.GetDroppedCount() != 1 {
		t.Fatalf("closed degraded window = ended %d dropped %d, want ended %d dropped 1", closed.GetEndedAtMs(), closed.GetDroppedCount(), began+5)
	}
}
