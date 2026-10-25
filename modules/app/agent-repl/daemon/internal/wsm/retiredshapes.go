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
	"agent-repld/internal/feedmarker"
	"agent-repld/internal/prose"
)

// LAYOUT 77 CARRIES THE RETIRED ROW SHAPES STILL STORED ONTO TODAY'S. Each
// boot refused 1001 stored rows that carried a field the proto had since
// reserved, recording each at ERROR and drawing none (2026-10-24): the
// retirements landed with no step re-encoding the rows already written. A
// retired field whose content has a successor is CARRIED to it; one whose
// content is drawn elsewhere now, or not at all, is dropped. A row with a
// retired field and its successor both set is refused: no build wrote both.
//
// THE RETIRED MERGE TAB KINDS ARE NOT THIS STEP'S. A stored `conflicts` (5)
// or `fixes` (7) tab has no arm of today's oneof to land in without deciding
// how a merge bubble recorded before the nested tabs is drawn, so those rows
// are left as they are (224 of the 1001 on the 2026-10-24 store).

// The retired fields this step carries or drops, by message.
const (
	retiredResponseMarkdownTag  protowire.Number = 1 // FeedResponseProse.markdown -> prose
	retiredAddressTextTag       protowire.Number = 1 // FeedAgentPromptAddress.text -> name, direction
	retiredNoticeHeadingTag     protowire.Number = 1 // FeedResponseNotice.heading -> marker
	retiredHeadWorkspaceSwitch  protowire.Number = 5 // FeedMergeHead.workspace_switch -> mode.async
	retiredRebaseCommitTag      protowire.Number = 5 // FeedMergeTabRebaseCommit.commit -> FeedMergeTab.label.commit
	retiredTestsSuitesTag       protowire.Number = 3 // FeedMergeTabTests.suites: a run's own subtab now
	retiredTestsLogTag          protowire.Number = 4 // FeedMergeTabTests.log: a run's own subtab now
	retiredPermissionAtMsTag    protowire.Number = 1 // FeedPermissionAnswered.at_ms: drawn by nothing
	retiredToolReturnRuntimeTag protowire.Number = 9 // FeedToolCallReturned.runtime: the card head's clock
	retiredAddressSentArrow                      = "→ "
)

// rewriteRetiredShapeRows is layout 77's row rewrite.
func rewriteRetiredShapeRows(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	return rewriteDurableRows(ctx, tx, log, "carried the stored rows' retired fields onto today's shapes",
		func(raw []byte) ([]byte, bool, error) { return rewriteRetiredShapes(log, raw) })
}

// rewriteRetiredShapes re-encodes one stored frontend.v1.FeedRow carrying any
// retired field this step knows, and reports whether it did.
func rewriteRetiredShapes(log dlog.Logger, raw []byte) ([]byte, bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return nil, false, err
	}
	changed, err := walkMessages(row.ProtoReflect(), func(m protoreflect.Message) (bool, error) {
		return carryRetiredShape(log, m)
	})
	if err != nil || !changed {
		return raw, false, err
	}
	out, err := proto.Marshal(row)
	if err != nil {
		return nil, false, err
	}
	return out, true, nil
}

// carryRetiredShape carries or drops M's retired fields, as its message's own
// retirement says.
func carryRetiredShape(log dlog.Logger, m protoreflect.Message) (bool, error) {
	switch msg := m.Interface().(type) {
	case *frontendv1.FeedResponseProse:
		return carryUnknown(m, retiredResponseMarkdownTag, msg.GetProse() != nil, func(v []byte) error {
			msg.Prose = prose.Parse(log, string(v), prose.Settled)
			return nil
		})
	case *frontendv1.FeedAgentPromptAddress:
		return carryUnknown(m, retiredAddressTextTag, msg.GetName() != nil || msg.GetDirection() != nil, func(v []byte) error {
			return carryAddressText(msg, string(v))
		})
	case *frontendv1.FeedResponseNotice:
		return carryUnknown(m, retiredNoticeHeadingTag, msg.GetMarker() != nil, func([]byte) error {
			// THE HEADING NAMED NO SUBJECT, so the notice is today's
			// unclassified one: the neutral marker under its label.
			msg.Marker = feedmarker.Neutral(feedmarker.VendorNoticeLabel, "")
			return nil
		})
	case *frontendv1.FeedMergeHead:
		return carryUnknown(m, retiredHeadWorkspaceSwitch, msg.GetMode() != nil, func(v []byte) error {
			// THE SWITCH EXISTED ONLY FOR AN ASYNC MERGE, so it names the
			// mode the head was drawn in, and its bytes are today's arm's.
			sw := &frontendv1.FeedMergeWorkspaceSwitch{}
			if err := proto.Unmarshal(v, sw); err != nil {
				return fmt.Errorf("a merge head's retired workspace switch does not decode: %w", err)
			}
			msg.Mode = &frontendv1.FeedMergeMode{Mode: &frontendv1.FeedMergeMode_Async{Async: &frontendv1.FeedMergeModeAsync{WorkspaceSwitch: sw}}}
			return nil
		})
	case *frontendv1.FeedMergeTab:
		return carryRebaseCommit(msg)
	case *frontendv1.FeedMergeTabTests:
		return dropRetired(m, retiredTestsSuitesTag, retiredTestsLogTag)
	case *frontendv1.FeedPermissionAnswered:
		return dropRetired(m, retiredPermissionAtMsTag)
	case *frontendv1.FeedToolCallReturned:
		return dropRetired(m, retiredToolReturnRuntimeTag)
	}
	return false, nil
}

// dropRetired removes M's retired fields NUMS, whose content nothing draws
// from the row now.
func dropRetired(m protoreflect.Message, nums ...protowire.Number) (bool, error) {
	changed := false
	for _, num := range nums {
		found, err := dropUnknown(m, num)
		if err != nil {
			return false, err
		}
		changed = changed || found
	}
	return changed, nil
}

// carryAddressText reads a retired address line ("→ name", arrow and all)
// into its name and its direction. Only a SENT address can be carried: the
// line never said which kind of agent it reached, and today's unclassified
// recipient is exactly that. A received one never said whether an agent or a
// workspace sent it, and is refused.
func carryAddressText(addr *frontendv1.FeedAgentPromptAddress, text string) error {
	name, sent := strings.CutPrefix(text, retiredAddressSentArrow)
	if !sent || name == "" {
		return fmt.Errorf("a retired prompt address %q is not a sent one this step can carry", text)
	}
	addr.Name = &frontendv1.FeedAgentPromptAddressName{Text: name}
	addr.Direction = &frontendv1.FeedAgentPromptAddress_Sent{Sent: &frontendv1.FeedAgentPromptAddressSent{
		Recipient: &frontendv1.FeedAgentPromptAddressSent_Unclassified{Unclassified: &frontendv1.FeedAgentPromptAddressUnclassified{}},
	}}
	return nil
}

// carryRebaseCommit moves a rebase commit tab's retired commit identity into
// the tab's label, where today's draws it (FeedMergeTabLabel.commit). The
// label it replaces was that same identity spelled as text.
func carryRebaseCommit(tab *frontendv1.FeedMergeTab) (bool, error) {
	commit := tab.GetRebaseCommit()
	if commit == nil {
		return false, nil
	}
	return carryUnknown(commit.ProtoReflect(), retiredRebaseCommitTag, tab.GetLabel().GetCommit() != nil, func(v []byte) error {
		identity := &frontendv1.FeedMergeRebaseCommitIdentity{}
		if err := proto.Unmarshal(v, identity); err != nil {
			return fmt.Errorf("a rebase commit tab's retired commit does not decode: %w", err)
		}
		if tab.Label == nil {
			tab.Label = &frontendv1.FeedMergeTabLabel{}
		}
		tab.Label.Words = &frontendv1.FeedMergeTabLabel_Commit{Commit: identity}
		return nil
	})
}
