package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
)

// LAYOUT 175 CARRIES THE STORED MERGE HEADS' RETIRED LABEL ONTO THEIR BRANCH
// LINE. A merge head drew its branch line as one string, `label` (2), until
// the target became an element of its own that opens the pull request a merge
// deploys (2026-11-19): FeedMergeHead reserves the field, and `branch_line`
// (8) holds the source and the target. Every stored head still carrying the
// label would be refused by each read of its bubble, so the label is split at
// its arrow into the line's source and target; a label with no arrow is the
// merging workspace's id standing in, and becomes a source alone.
//
// IT IS ADDITIVE, as every row rewrite is (TestAContractStepOnlyRemoves). A
// build before it that draws a rewritten head afresh, in a handover's span,
// refuses it as carrying a field it cannot describe; this build carries a
// stored head's label as it reads the row (CarryRetiredMergeHeadLabel), so it
// draws every head whether or not the step has run.

// The retired merge head field this step carries.
const retiredMergeHeadLabelTag protowire.Number = 2 // FeedMergeHead.label -> branch_line

// retiredMergeLabelArrow is the separator the retired label spelled between
// the source and the target.
const retiredMergeLabelArrow = " → "

// carryRetiredMergeHeadLabelRows is layout 175's row rewrite.
func carryRetiredMergeHeadLabelRows(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	return rewriteDurableRows(ctx, tx, log, "carried the stored merge heads' retired labels onto their branch lines", CarryRetiredMergeHeadLabel)
}

// CarryRetiredMergeHeadLabel re-encodes one stored frontend.v1.FeedRow whose
// merge head carries the retired label with the label carried onto its branch
// line, and reports whether it did. Every other row is answered as it is. A
// head carrying both the label and a branch line, or a label that does not
// decode, is an error: no build wrote either.
func CarryRetiredMergeHeadLabel(raw []byte) ([]byte, bool, error) {
	return editFeedRow(raw, func(row *frontendv1.FeedRow) (bool, error) {
		return walkMessages(row.ProtoReflect(), func(m protoreflect.Message) (bool, error) {
			head, ok := m.Interface().(*frontendv1.FeedMergeHead)
			if !ok {
				return false, nil
			}
			return carryUnknown(m, retiredMergeHeadLabelTag, head.GetBranchLine() != nil, func(v []byte) error {
				line, err := retiredMergeLabelLine(v)
				if err != nil {
					return err
				}
				head.BranchLine = line
				return nil
			})
		})
	})
}

// retiredMergeLabelLine reads the retired label's bytes, a message whose one
// field is the line's text (1), as today's branch line.
func retiredMergeLabelLine(v []byte) (*frontendv1.FeedMergeBranchLine, error) {
	// THE RETIRED LABEL'S SHAPE IS THE FIX GATE LABEL'S: one string at tag 1.
	label := &frontendv1.FeedFixGateLabel{}
	if err := proto.Unmarshal(v, label); err != nil {
		return nil, fmt.Errorf("a merge head's retired label does not decode: %w", err)
	}
	source, target, read := strings.Cut(label.GetText(), retiredMergeLabelArrow)
	line := &frontendv1.FeedMergeBranchLine{Source: &frontendv1.FeedMergeBranchLineSource{Text: source}}
	if read {
		line.Target = &frontendv1.FeedMergeBranchLineTarget{Text: target}
	}
	return line, nil
}
