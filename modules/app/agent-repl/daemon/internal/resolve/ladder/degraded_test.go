package ladder

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestDegradedWindowOpen(t *testing.T) {
	open := &conversationv1.SessionDegradedWindow{Extent: &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}}}
	closed := &conversationv1.SessionDegradedWindow{Extent: &conversationv1.SessionDegradedWindow_Closed{Closed: &conversationv1.SessionDegradedClosed{}}}
	cases := []struct {
		name string
		d    *conversationv1.SessionDiagnostics
		want bool
	}{
		{name: "no diagnostics have no open window", d: nil, want: false},
		{name: "diagnostics with no windows have no open window", d: &conversationv1.SessionDiagnostics{}, want: false},
		{name: "only closed windows are not degraded", d: &conversationv1.SessionDiagnostics{DegradedWindows: []*conversationv1.SessionDegradedWindow{closed}}, want: false},
		{name: "an open window among closed ones is degraded", d: &conversationv1.SessionDiagnostics{DegradedWindows: []*conversationv1.SessionDegradedWindow{closed, open}}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := DegradedWindowOpen(tc.d)

			// Assert
			if got != tc.want {
				t.Fatalf("DegradedWindowOpen = %v, want %v", got, tc.want)
			}
		})
	}
}
