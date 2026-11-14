package feed

import (
	"errors"
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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

// recordedSwitch decodes the switch WS's durable record holds for ROW.
func recordedSwitch(t *testing.T, store *fakeDurableRows, ws ids.WorkspaceID, row string) *frontendv1.FeedMergeWorkspaceSwitch {
	t.Helper()
	recorded, ok := store.recorded(ws, row)
	if !ok {
		t.Fatalf("no durable record of %s", row)
	}
	decoded := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, decoded); err != nil {
		t.Fatalf("decode %s: %v", row, err)
	}
	return decoded.GetActivity().GetMerge().GetHead().GetMode().GetAsync().GetWorkspaceSwitch()
}

func TestMarkMergeWorkspaceRemovedRestatesAnUnheldWorkspacesRecordWithoutWakingItsFeed(t *testing.T) {
	// Arrange: the first daemon drew the head; the second holds no feed state
	// for the workspace, and its directory is gone.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	head := switchingHeadRow(theMergeWorkspace)
	first.UpsertDurable(testWorkspace, rootFeed(), head)
	first.UpsertDurable(testWorkspace, rootFeed(), func() *frontendv1.FeedRow {
		other := switchingHeadRow("ws-other")
		other.Id = &frontendv1.FeedId{Value: head.GetId().GetValue() + "-other"}
		return other
	}())
	lookedUp := 0
	second, log := daemonOverWith(t, store, func(d *Deps) {
		d.WorkspaceDir = func(ids.WorkspaceID) (string, error) { lookedUp++; return "/gone", nil }
	})

	// Act
	marked := second.MarkMergeWorkspaceRemoved(testWorkspace, theMergeWorkspace, 77)

	// Assert
	if marked != 1 {
		t.Fatalf("marked = %d, want the one switch to the removed workspace", marked)
	}
	if got := recordedSwitch(t, store, testWorkspace, head.GetId().GetValue()); got.GetRemoved().GetRemovedAtMs() != 77 || got.GetLabel().GetText() != removedSwitchLabel {
		t.Fatalf("recorded switch = %+v, want removed at 77", got)
	}
	if got := recordedSwitch(t, store, testWorkspace, head.GetId().GetValue()+"-other"); got.GetAvailable() == nil {
		t.Fatalf("other switch = %+v, want it left available", got)
	}
	if _, held := second.workspaces[testWorkspace]; held {
		t.Fatal("the workspace's feed state was brought up; a feed nobody reads was woken")
	}
	if lookedUp != 0 {
		t.Fatalf("workspace directory looked up %d times, want none", lookedUp)
	}
	if !hasRecordIn(log, "info", "daemon.feed.merge_workspace_removed") {
		t.Fatalf("records = %+v, want the restatement recorded", log.Records())
	}
}

func TestMarkMergeWorkspaceRemovedInAnUnheldRecordFailsLoudly(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(store *fakeDurableRows)
		op      string
	}{
		{name: "the record is unreadable", arrange: func(s *fakeDurableRows) { s.readErr = errors.New("disk I/O error") }, op: "daemon.feed.merge_workspace_removed_unread"},
		{name: "the restated row cannot be recorded", arrange: func(s *fakeDurableRows) { s.putErr = errors.New("disk I/O error") }, op: "daemon.feed.merge_workspace_removed_unrecorded"},
		{name: "a recorded row cannot be decoded", arrange: func(s *fakeDurableRows) {
			s.rows[testWorkspace]["row|garbage"] = wsm.DurableFeedRow{Workspace: testWorkspace, RowID: "row|garbage", OrderKey: "2zzzz", Row: []byte{0xff, 0xff, 0xff}}
		}, op: "daemon.feed.merge_workspace_removed_undecoded"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			store := newFakeDurableRows()
			first, _ := daemonOver(t, store)
			head := switchingHeadRow(theMergeWorkspace)
			first.UpsertDurable(testWorkspace, rootFeed(), head)
			tc.arrange(store)
			second, log := daemonOver(t, store)

			// Act
			marked := second.MarkMergeWorkspaceRemoved(testWorkspace, theMergeWorkspace, 77)

			// Assert
			if !hasRecordIn(log, "error", tc.op) {
				t.Fatalf("records = %+v, want %s at ERROR", log.Records(), tc.op)
			}
			if tc.op != "daemon.feed.merge_workspace_removed_undecoded" && marked != 0 {
				t.Fatalf("marked = %d, want none", marked)
			}
			if tc.op == "daemon.feed.merge_workspace_removed_undecoded" && marked != 1 {
				t.Fatalf("marked = %d, want the decodable switch still restated", marked)
			}
		})
	}
}
