package wsm

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// storedHeadRow is a stored merge head row whose head is HEAD.
func storedHeadRow(head *frontendv1.FeedMergeHead) []byte {
	return marshalledOrPanic(&frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
		Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{Head: head}},
	}}})
}

// retiredLabelHead is a merge head carrying the retired label TEXT.
func retiredLabelHead(text string) *frontendv1.FeedMergeHead {
	return withRetiredBytes(&frontendv1.FeedMergeHead{}, retiredMergeHeadLabelTag, marshalledOrPanic(&frontendv1.FeedFixGateLabel{Text: text}))
}

func TestCarryRetiredMergeHeadLabelCarriesTheLine(t *testing.T) {
	tests := []struct {
		name string
		text string
		want *frontendv1.FeedMergeBranchLine
	}{
		{name: "a line with an arrow is a source and a target", text: "ABC/fix → /Users/x/repo",
			want: &frontendv1.FeedMergeBranchLine{Source: &frontendv1.FeedMergeBranchLineSource{Text: "ABC/fix"}, Target: &frontendv1.FeedMergeBranchLineTarget{Text: "/Users/x/repo"}}},
		{name: "a line with no arrow is a source alone", text: "c0ffee",
			want: &frontendv1.FeedMergeBranchLine{Source: &frontendv1.FeedMergeBranchLineSource{Text: "c0ffee"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			out, changed, err := CarryRetiredMergeHeadLabel(storedHeadRow(retiredLabelHead(tt.text)))

			// Assert
			if err != nil || !changed {
				t.Fatalf("carry = (%v, %v), want a carried row", changed, err)
			}
			row := &frontendv1.FeedRow{}
			if err := proto.Unmarshal(out, row); err != nil {
				t.Fatalf("Unmarshal: %v", err)
			}
			head := row.GetActivity().GetMerge().GetHead()
			if !proto.Equal(head.GetBranchLine(), tt.want) || len(head.ProtoReflect().GetUnknown()) != 0 {
				t.Fatalf("head = %+v, want %+v and no retired field left", head, tt.want)
			}
		})
	}
}

func TestCarryRetiredMergeHeadLabelLeavesTodaysRowsAlone(t *testing.T) {
	// Arrange
	raw := storedHeadRow(&frontendv1.FeedMergeHead{BranchLine: &frontendv1.FeedMergeBranchLine{Source: &frontendv1.FeedMergeBranchLineSource{Text: "feature"}}})

	// Act
	out, changed, err := CarryRetiredMergeHeadLabel(raw)

	// Assert
	if err != nil || changed || string(out) != string(raw) {
		t.Fatalf("carry = (%v, %v), want the row answered as it is", changed, err)
	}
}

func TestCarryRetiredMergeHeadLabelRefuses(t *testing.T) {
	both := retiredLabelHead("feature → master")
	both.BranchLine = &frontendv1.FeedMergeBranchLine{Source: &frontendv1.FeedMergeBranchLineSource{Text: "feature"}}
	tests := []struct {
		name string
		head *frontendv1.FeedMergeHead
	}{
		{name: "a head carrying the label and its successor", head: both},
		{name: "a label that does not decode", head: withRetiredBytes(&frontendv1.FeedMergeHead{}, retiredMergeHeadLabelTag, []byte{0xff})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, changed, err := CarryRetiredMergeHeadLabel(storedHeadRow(tt.head))

			// Assert
			if err == nil || changed {
				t.Fatalf("carry = (%v, %v), want an error and nothing carried", changed, err)
			}
		})
	}
}
