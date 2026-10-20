package feed

import (
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/ids"
)

// theMergeWorkspace is the merge workspace the switches below name.
const theMergeWorkspace = ids.WorkspaceID("ws-merge")

// switchingHeadRow is an async merge head carrying an available switch to
// TARGET.
func switchingHeadRow(target ids.WorkspaceID) *frontendv1.FeedRow {
	row := mergeHeadRow("feature → master")
	row.GetActivity().GetMerge().GetHead().Mode = &frontendv1.FeedMergeMode{Mode: &frontendv1.FeedMergeMode_Async{Async: &frontendv1.FeedMergeModeAsync{
		WorkspaceSwitch: &frontendv1.FeedMergeWorkspaceSwitch{
			Label: &frontendv1.FeedMergeWorkspaceSwitchLabel{Text: "open merge workspace"},
			Availability: &frontendv1.FeedMergeWorkspaceSwitch_Available{Available: &frontendv1.FeedMergeWorkspaceSwitchAvailable{
				Target: &workspacev1.WorkspaceRef{Id: string(target), Dir: "/tmp/ws-merge"},
			}},
		},
	}}}
	return row
}

func TestPromptRowDrawsAMergeHandbackAsItsBadge(t *testing.T) {
	// Arrange
	said := SaidText("merge of feature landed\n\nA merge you asked for ran detached.")

	// Act
	row := PromptRow(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK, nil, said, nil)

	// Assert
	if row.GetUserPrompt() != nil || row.GetMergeHandback().GetLabel().GetText() != "merge of feature landed" {
		t.Fatalf("row = %+v, want the handback badge labelled with the first line", row)
	}
}

func TestPromptRowDrawsEveryOtherOriginAsAUserPrompt(t *testing.T) {
	// Arrange
	blocks := []*frontendv1.FeedUserPromptBlock{{}}

	// Act
	row := PromptRow(conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT, nil, SaidText("hello"), blocks)

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

func TestMarkMergeWorkspaceRemovedRestatesTheHead(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	head := switchingHeadRow(theMergeWorkspace)
	r.UpsertDurable(testWorkspace, rootFeed(), head)

	// Act
	marked := r.MarkMergeWorkspaceRemoved(testWorkspace, theMergeWorkspace, 1234)

	// Assert: the head restated, drawn and recorded as removed.
	if marked != 1 {
		t.Fatalf("marked = %d, want the head", marked)
	}
	drawn := feedRows(r, rootFeed())[0].GetActivity().GetMerge().GetHead().GetMode().GetAsync().GetWorkspaceSwitch()
	if drawn.GetRemoved().GetRemovedAtMs() != 1234 || drawn.GetLabel().GetText() != removedSwitchLabel {
		t.Fatalf("head switch = %+v, want removed at 1234", drawn)
	}
	recorded, _ := store.recorded(testWorkspace, head.GetId().GetValue())
	decoded := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, decoded); err != nil || decoded.GetActivity().GetMerge().GetHead().GetMode().GetAsync().GetWorkspaceSwitch().GetRemoved() == nil {
		t.Fatalf("recorded head = %+v, %v; want the removal recorded", decoded, err)
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
	if marked != 0 || feedRows(r, rootFeed())[0].GetActivity().GetMerge().GetHead().GetMode().GetAsync().GetWorkspaceSwitch().GetAvailable() == nil {
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
	if marked != 1 || feedRows(second, rootFeed())[0].GetActivity().GetMerge().GetHead().GetMode().GetAsync().GetWorkspaceSwitch().GetRemoved() == nil {
		t.Fatalf("marked = %d, want the restored head restated as removed", marked)
	}
}
