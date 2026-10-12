package feed

import (
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// theMergeWorkspace is the merge workspace the switches below name.
const theMergeWorkspace = ids.WorkspaceID("ws-merge")

// switchingHeadRow is a merge head carrying an available switch to TARGET.
func switchingHeadRow(target ids.WorkspaceID) *frontendv1.FeedRow {
	row := mergeHeadRow("feature → master")
	row.GetActivity().GetMerge().GetHead().WorkspaceSwitch = &frontendv1.FeedMergeWorkspaceSwitch{
		Label: &frontendv1.FeedMergeWorkspaceSwitchLabel{Text: "open merge workspace"},
		Availability: &frontendv1.FeedMergeWorkspaceSwitch_Available{Available: &frontendv1.FeedMergeWorkspaceSwitchAvailable{
			Target: &workspacev1.WorkspaceRef{Id: string(target), Dir: "/tmp/ws-merge"},
		}},
	}
	return row
}

// switchingConflictsRow is a conflicts tab on lease-7's sub-feed carrying an
// available switch to TARGET.
func switchingConflictsRow(target ids.WorkspaceID) (feedid.Feed, *frontendv1.FeedRow) {
	lease := ids.LeaseID("lease-7")
	feed := feedid.Feed{Merge: &lease}
	return feed, &frontendv1.FeedRow{
		Id: testEncode(feedid.Ref{WS: testWorkspace, Feed: feed, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "conflicts", Sub: "1"}}),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
			Label: &frontendv1.FeedMergeTabLabel{Text: "conflicts", Round: 1},
			Kind: &frontendv1.FeedMergeTab_Conflicts{Conflicts: &frontendv1.FeedMergeTabConflicts{
				WorkspaceSwitch: &frontendv1.FeedMergeTabWorkspaceSwitch{
					Label: &frontendv1.FeedMergeTabWorkspaceSwitchLabel{Text: "resolving conflicts in the merge workspace"},
					Availability: &frontendv1.FeedMergeTabWorkspaceSwitch_Available{Available: &frontendv1.FeedMergeTabWorkspaceSwitchAvailable{
						Target: &workspacev1.WorkspaceRef{Id: string(target), Dir: "/tmp/ws-merge"},
					}},
				},
			}},
		}},
	}
}

func TestPromptRowDrawsAMergeHandbackAsItsBadge(t *testing.T) {
	// Arrange
	said := SaidText("merge of feature landed\n\nA merge you asked for ran detached.")

	// Act
	row := PromptRow(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK, said, nil)

	// Assert
	if row.GetUserPrompt() != nil || row.GetMergeHandback().GetLabel().GetText() != "merge of feature landed" {
		t.Fatalf("row = %+v, want the handback badge labelled with the first line", row)
	}
}

func TestPromptRowDrawsEveryOtherOriginAsAUserPrompt(t *testing.T) {
	// Arrange
	blocks := []*frontendv1.FeedUserPromptBlock{{}}

	// Act
	row := PromptRow(conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT, SaidText("hello"), blocks)

	// Assert
	if row.GetMergeHandback() != nil || row.GetUserPrompt().GetAuthor().GetLabel() != "You" ||
		len(row.GetUserPrompt().GetSuccess().GetBody().GetBlocks()) != 1 {
		t.Fatalf("row = %+v, want the user's prompt bubble with its blocks", row)
	}
}

func TestMergeHandbackLabelIsTheFirstLineOfTheFirstText(t *testing.T) {
	tests := []struct {
		name string
		said *conversationv1.UserSaid
		want string
	}{
		{name: "several lines", said: SaidText("  merge of x failed  \nmore"), want: "merge of x failed"},
		{name: "one line", said: SaidText("merge of x did not run"), want: "merge of x did not run"},
		{name: "no text", said: &conversationv1.UserSaid{}, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := MergeHandbackLabel(tt.said)
			// Assert
			if got != tt.want {
				t.Fatalf("MergeHandbackLabel = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheHandbackBriefLeadsWithItsLabel(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile("../../../../prompts/merge-detached-handback.md")
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}

	// Act: the first line after the header comment.
	lines := strings.Split(string(raw), "\n")

	// Assert: MergeHandbackLabel reads the label off exactly this line.
	if len(lines) < 2 || !strings.HasPrefix(lines[0], "<!--") || lines[1] != "{{label}}" {
		t.Fatalf("brief starts %q, want the header then {{label}} alone on the first line", lines[:2])
	}
}

func TestThePromptQueueMirrorDrawsThroughPromptRow(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile("../../promptqueue/deliver.go")
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	source := string(raw)

	// Act / Assert: the mirror composes no prompt row of its own.
	if !strings.Contains(source, "feed.PromptRow(") || strings.Contains(source, "FeedRow_UserPrompt{") {
		t.Fatal("the prompt queue's mirror does not draw its row through feed.PromptRow")
	}
}

func TestMarkMergeWorkspaceRemovedRestatesTheHeadAndTheAgenticTab(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	r.UpsertDurable(testWorkspace, rootFeed(), switchingHeadRow(theMergeWorkspace))
	tabFeed, tab := switchingConflictsRow(theMergeWorkspace)
	r.UpsertDurable(testWorkspace, tabFeed, tab)

	// Act
	marked := r.MarkMergeWorkspaceRemoved(testWorkspace, theMergeWorkspace, 1234)

	// Assert: both rows restated, drawn and recorded as removed.
	if marked != 2 {
		t.Fatalf("marked = %d, want the head and the tab", marked)
	}
	head := feedRows(r, rootFeed())[0].GetActivity().GetMerge().GetHead().GetWorkspaceSwitch()
	if head.GetRemoved().GetRemovedAtMs() != 1234 || head.GetLabel().GetText() != removedSwitchLabel {
		t.Fatalf("head switch = %+v, want removed at 1234", head)
	}
	conflicts := feedRows(r, tabFeed)[0].GetMergeTab().GetConflicts().GetWorkspaceSwitch()
	if conflicts.GetRemoved().GetRemovedAtMs() != 1234 {
		t.Fatalf("tab switch = %+v, want removed at 1234", conflicts)
	}
	recorded, _ := store.recorded(testWorkspace, tab.GetId().GetValue())
	decoded := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, decoded); err != nil || decoded.GetMergeTab().GetConflicts().GetWorkspaceSwitch().GetRemoved() == nil {
		t.Fatalf("recorded tab = %+v, %v; want the removal recorded", decoded, err)
	}
}

func TestMarkMergeWorkspaceRemovedLeavesOtherSwitchesAlone(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	r.UpsertDurable(testWorkspace, rootFeed(), switchingHeadRow("ws-other"))

	// Act
	marked := r.MarkMergeWorkspaceRemoved(testWorkspace, theMergeWorkspace, 1234)

	// Assert
	if marked != 0 || feedRows(r, rootFeed())[0].GetActivity().GetMerge().GetHead().GetWorkspaceSwitch().GetAvailable() == nil {
		t.Fatalf("marked = %d, want another workspace's switch left available", marked)
	}
}

func TestMarkMergeWorkspaceRemovedReachesARestartedDaemonsRestoredRows(t *testing.T) {
	// Arrange: the first daemon drew the head; the second has only the record.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), switchingHeadRow(theMergeWorkspace))
	second, _ := daemonOver(t, store)

	// Act
	marked := second.MarkMergeWorkspaceRemoved(testWorkspace, theMergeWorkspace, 99)

	// Assert
	if marked != 1 || feedRows(second, rootFeed())[0].GetActivity().GetMerge().GetHead().GetWorkspaceSwitch().GetRemoved() == nil {
		t.Fatalf("marked = %d, want the restored head restated as removed", marked)
	}
}
