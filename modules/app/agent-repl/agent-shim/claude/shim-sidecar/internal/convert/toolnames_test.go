package convert

// toolnames_test.go — the tool vocabulary: agent-repl's own tools classify to
// their own kinds, and their names agree with the cross-language vocabulary.

import (
	"encoding/json"
	"os"
	"testing"
)

// agentReplToolsVocab is proto/vocab/agent-repl-tools.json's shape.
type agentReplToolsVocab struct {
	Server string `json:"server"`
	Tools  []struct {
		Tool      string `json:"tool"`
		Qualified string `json:"qualified"`
	} `json:"tools"`
}

func readAgentReplToolsVocab(t *testing.T) agentReplToolsVocab {
	t.Helper()
	raw, err := os.ReadFile("../../../../../proto/vocab/agent-repl-tools.json")
	if err != nil {
		t.Fatalf("read the agent-repl tools vocabulary: %v", err)
	}
	var vocab agentReplToolsVocab
	if err := json.Unmarshal(raw, &vocab); err != nil {
		t.Fatalf("decode the agent-repl tools vocabulary: %v", err)
	}
	return vocab
}

func TestAgentReplToolNamesMatchTheVocabulary(t *testing.T) {
	// Arrange.
	vocab := readAgentReplToolsVocab(t)
	want := map[string]bool{}
	for _, tool := range vocab.Tools {
		want[tool.Qualified] = true
	}

	// Act.
	got := map[string]bool{}
	for name := range agentReplTools {
		got[name] = true
	}

	// Assert.
	if len(got) != len(want) {
		t.Fatalf("agent-repl tools = %v, want exactly the vocabulary's %v", got, want)
	}
	for name := range want {
		if !got[name] {
			t.Fatalf("the vocabulary's %q has no conversion here", name)
		}
	}
}

func TestClassifyToolReadsAgentReplsOwnBoardTool(t *testing.T) {
	// Arrange, Act.
	kind, known := classifyTool(showChessBoardToolName)

	// Assert.
	if !known || kind != kindChessBoard {
		t.Fatalf("classifyTool(%q) = %v, %t; want kindChessBoard, true", showChessBoardToolName, kind, known)
	}
}

func TestClassifyToolLeavesAnotherMcpToolUnknown(t *testing.T) {
	// Arrange, Act.
	_, known := classifyTool("mcp__agent-repl__unknown_tool")

	// Assert.
	if known {
		t.Fatal("an agent-repl server tool with no conversion must stay unknown, for the MCP card")
	}
}
