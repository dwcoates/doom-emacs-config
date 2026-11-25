//go:build integration

package integration

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// support_merge_test.go holds helpers shared only by this suite's newly added
// tests (audit1-c), kept separate from merge_test.go's own long-standing
// merge-prefixed helpers at that file's bottom.

// isMergeTerminal reports whether a feed row is a merge bubble's terminal:
// its head landed (success) or ended in error. It is the one predicate every
// test awaiting a merge's end reads.
func isMergeTerminal(row *frontendv1.FeedRow) bool {
	m := row.GetActivity().GetMerge()
	return m.GetSuccess() != nil || m.GetError() != nil
}

func TestIsMergeTerminalReadsALandingOrAnErrorAndNothingElse(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
		want bool
	}{
		{name: "landed", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Success{Success: &frontendv1.FeedMergeSuccess{}}}}}}}, want: true},
		{name: "errored", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Error{Error: &frontendv1.FeedMergeError{}}}}}}}, want: true},
		{name: "running", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{}}}}}, want: false},
		{name: "no merge", row: &frontendv1.FeedRow{}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert.
			if got := isMergeTerminal(tt.row); got != tt.want {
				t.Fatalf("isMergeTerminal = %v, want %v", got, tt.want)
			}
		})
	}
}
