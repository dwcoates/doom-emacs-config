package discover

import (
	"path/filepath"
	"testing"
)

// The vendor writes ONE file name for TWO documents. Both bodies below are
// verbatim shapes observed on the owner's machine under
// ~/.claude/projects/<slug>/<session>/subagents/ (the subagent shape) and
// .../subagents/workflows/wf_<id>/ (the workflow shape).
const (
	subagentMetaBody = `{"agentType":"opus-medium","description":"Tool converters batch B",` +
		`"toolUseId":"toolu_01LaW8HwuB3zUaU8bUmqVSGT","parentAgentId":"a699887424d695c83","spawnDepth":3}`
	workflowMetaBody = `{"agentType":"workflow-subagent",` +
		`"worktreePath":"/work/.config/doom/.claude/worktrees/wf_0297f159-ca1-1",` +
		`"spawnedWithWorktree":true,"spawnDepth":1,"model":"opus"}`
	workflowMetaWithoutWorktreeBody = `{"agentType":"workflow-subagent","spawnDepth":1,"model":"opus"}`
	namelessMetaBody                = `{"description":"names nothing","spawnDepth":1}`
)

// readMetaFixture writes one meta body and reads it back.
func readMetaFixture(t *testing.T, body string) (Meta, error) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "agent-abc.meta.json")
	writeRaw(t, path, []byte(body))
	return ReadMeta(path)
}

func TestReadMetaShapes(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		body string
		want Meta
	}{
		{
			name: "subagent shape is identified by its spawning call",
			body: subagentMetaBody,
			want: Meta{
				Shape: ShapeSubagent, AgentType: "opus-medium",
				Description: "Tool converters batch B", ToolUseID: "toolu_01LaW8HwuB3zUaU8bUmqVSGT",
				SpawnDepth: 3, ParentAgentID: "a699887424d695c83",
			},
		},
		{
			name: "workflow shape carries no spawning call and no description",
			body: workflowMetaBody,
			want: Meta{
				Shape: ShapeWorkflow, AgentType: "workflow-subagent", SpawnDepth: 1,
				Model:        "opus",
				WorktreePath: "/work/.config/doom/.claude/worktrees/wf_0297f159-ca1-1",

				SpawnedWithWorktree: true,
			},
		},
		{
			name: "workflow shape without a worktree is still the workflow shape",
			body: workflowMetaWithoutWorktreeBody,
			want: Meta{Shape: ShapeWorkflow, AgentType: "workflow-subagent", SpawnDepth: 1, Model: "opus"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := readMetaFixture(t, tc.body)

			// Assert.
			if err != nil {
				t.Fatalf("ReadMeta: %v", err)
			}
			if got != tc.want {
				t.Fatalf("meta = %+v, want %+v", got, tc.want)
			}
		})
	}
}

// A file naming NEITHER an id nor a type names no agent at all, and that is the
// only shape ReadMeta refuses.
func TestReadMetaRefusesAMetaThatNamesNothing(t *testing.T) {
	// Arrange + Act.
	_, err := readMetaFixture(t, namelessMetaBody)

	// Assert.
	if err == nil {
		t.Fatal("a meta stating neither toolUseId nor agentType was accepted")
	}
}

func TestReadMetaRefusesUnparsableJSON(t *testing.T) {
	// Arrange + Act.
	_, err := readMetaFixture(t, "not json at all")

	// Assert.
	if err == nil {
		t.Fatal("an unparsable meta file was accepted")
	}
}
