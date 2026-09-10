package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// fullUsage is a rich get_context_usage answer, so one arrangement can exercise
// every section of the panel.
func fullUsage() *conversationv1.SessionContextUsage {
	loaded := true
	threshold := int64(160_000)
	return &conversationv1.SessionContextUsage{
		TotalTokens:  142_300,
		MaxTokens:    200_000,
		RawMaxTokens: 200_000,
		Percentage:   71,
		Model:        "claude-sonnet-4-5",
		Categories: []*conversationv1.SessionContextCategory{
			{Label: "messages", Tokens: 38_100, Color: "#8888ff"},
		},
		MemoryFiles: []*conversationv1.SessionContextMemoryFile{
			{Path: "webapp/CLAUDE.md", Type: "project", Tokens: 2_100},
		},
		McpTools: []*conversationv1.SessionContextMcpTool{
			{Name: "list_prs", ServerName: "github", Tokens: 900, IsLoaded: &loaded},
		},
		DeferredBuiltinTools: []*conversationv1.SessionContextDeferredBuiltinTool{
			{Name: "NotebookEdit", Tokens: 300, IsLoaded: false},
		},
		SystemTools: []*conversationv1.SessionContextSystemTool{
			{Name: "Bash", Tokens: 1_200},
		},
		SystemPromptSections: []*conversationv1.SessionContextSystemPromptSection{
			{Name: "tone", Tokens: 400},
		},
		Agents: []*conversationv1.SessionContextAgent{
			{AgentType: "Explore", Source: "builtin", Tokens: 250},
		},
		SlashCommands: &conversationv1.SessionContextSlashCommands{
			TotalCommands: 31, IncludedCommands: 12, Tokens: 4_200,
		},
		Skills: &conversationv1.SessionContextSkills{
			TotalSkills: 19, IncludedSkills: 8, Tokens: 6_400,
			SkillFrontmatter: []*conversationv1.SessionContextSkillFrontmatter{
				{Name: "graphify", Source: "user", Tokens: 800},
			},
		},
		AutoCompactThreshold: &threshold,
		IsAutoCompactEnabled: true,
		MessageBreakdown: &conversationv1.SessionContextMessageBreakdown{
			ToolCallTokens: 9_000, ToolResultTokens: 21_000, AttachmentTokens: 1_000,
			AssistantMessageTokens: 5_000, UserMessageTokens: 2_000,
			RedirectedContextTokens: 100, UnattributedTokens: 50,
			ToolCallsByType: []*conversationv1.SessionContextToolCallsByType{
				{Name: "Bash", CallTokens: 1_200, ResultTokens: 3_400},
			},
			AttachmentsByType: []*conversationv1.SessionContextAttachmentsByType{
				{Name: "image", Tokens: 1_000},
			},
		},
	}
}

// panelOf installs a usage answer and resolves the panel.
func panelOf(t *testing.T, h *harness, usage *conversationv1.SessionContextUsage) *frontendv1.ContextPanelView {
	t.Helper()
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: usage},
	})
	panel, ok := h.r.ContextPanel(testWS)
	if !ok {
		t.Fatalf("ContextPanel reported no answer after a context usage arrived")
	}
	return panel
}

func TestThePanelIsUnresolvedBeforeTheVendorAnswers(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, ok := h.r.ContextPanel(testWS)

	// Assert
	if ok {
		t.Fatalf("a panel resolved before the vendor stated a context usage")
	}
}

func TestThePanelHeaderCarriesTheVendorsOwnPercentage(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	want := "142.3k of 200k (71%) · claude-sonnet-4-5"
	if panel.GetHeader() != want {
		t.Fatalf("header = %q, want %q", panel.GetHeader(), want)
	}
}

func TestACategoryCarriesItsComposedFigureAndVendorColor(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	category := panel.GetCategories()[0]
	if category.GetFigure() != "38.1k · 19%" {
		t.Fatalf("figure = %q, want the composed tokens and share", category.GetFigure())
	}
	if category.GetColor() != "#8888ff" {
		t.Fatalf("color = %q, want the vendor's own", category.GetColor())
	}
}

func TestAMemoryFileRowNamesItsPathAndType(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	row := panel.GetMemoryFiles()[0]
	if row.GetLabel() != "webapp/CLAUDE.md · project" || row.GetFigure() != "2.1k" {
		t.Fatalf("row = %+v, want the composed label and figure", row)
	}
}

func TestAnMcpToolRowNamesItsToolServerAndLoadState(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if got := panel.GetMcpTools()[0].GetLabel(); got != "list_prs · github · loaded" {
		t.Fatalf("label = %q, want the tool, its server and its load state", got)
	}
}

func TestAnUnstatedLoadStateSaysNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.McpTools[0].IsLoaded = nil

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	if got := panel.GetMcpTools()[0].GetLabel(); got != "list_prs · github" {
		t.Fatalf("label = %q, want no claim where the vendor made none", got)
	}
}

func TestADeferredBuiltinToolSaysItIsDeferred(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if got := panel.GetDeferredBuiltinTools()[0].GetLabel(); got != "NotebookEdit · deferred" {
		t.Fatalf("label = %q, want the deferred state stated", got)
	}
}

func TestAnAgentRowNamesItsTypeAndSource(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if got := panel.GetAgents()[0].GetLabel(); got != "Explore · builtin" {
		t.Fatalf("label = %q, want the type and the source", got)
	}
}

func TestTheSlashCommandRollupIsComposed(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if got := panel.GetSlashCommands().GetLine(); got != "12 of 31 commands · 4.2k" {
		t.Fatalf("rollup = %q, want the composed sentence", got)
	}
}

func TestAnAbsentSlashCommandFactLeavesTheRollupUnset(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.SlashCommands = nil

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	if panel.SlashCommands != nil {
		t.Fatalf("rollup = %+v, want UNSET where the vendor omitted the fact", panel.SlashCommands)
	}
}

func TestTheSkillsRollupCarriesItsPerSkillRows(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	skills := panel.GetSkills()
	if skills.GetLine() != "8 of 19 skills · 6.4k" {
		t.Fatalf("rollup = %q, want the composed sentence", skills.GetLine())
	}
	if got := skills.GetSkills()[0].GetLabel(); got != "graphify · user" {
		t.Fatalf("skill row = %q, want the name and the source", got)
	}
}

func TestTheMessageBreakdownCarriesEveryPlane(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if got := len(panel.GetMessageBreakdown().GetPlanes()); got != 7 {
		t.Fatalf("planes = %d, want all seven", got)
	}
}

func TestAToolCallRowSplitsCallFromResult(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	row := panel.GetMessageBreakdown().GetToolCalls()[0]
	if row.GetFigure() != "call 1.2k · result 3.4k" {
		t.Fatalf("figure = %q, want the composed split", row.GetFigure())
	}
}

func TestAnAbsentMessageBreakdownIsUnset(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.MessageBreakdown = nil

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	if panel.MessageBreakdown != nil {
		t.Fatalf("breakdown = %+v, want UNSET where the vendor omitted the fact", panel.MessageBreakdown)
	}
}

func TestAutoCompactStatesItsThreshold(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if panel.GetAutoCompactLine() != "auto-compact at 160k" {
		t.Fatalf("line = %q, want the threshold stated", panel.GetAutoCompactLine())
	}
}

func TestAutoCompactOffIsStatedAsOff(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.IsAutoCompactEnabled = false

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	if panel.GetAutoCompactLine() != "auto-compact off" {
		t.Fatalf("line = %q, want off", panel.GetAutoCompactLine())
	}
}

func TestAutoCompactOnWithNoThresholdSaysOnlyThat(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.AutoCompactThreshold = nil

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	if panel.GetAutoCompactLine() != "auto-compact on" {
		t.Fatalf("line = %q, want on with no invented threshold", panel.GetAutoCompactLine())
	}
}

func TestTheChipAndThePanelResolveFromTheSameFact(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if h.view(t).GetContext().GetText() != "142.3k" {
		t.Fatalf("chip = %q, want the same total the panel's header states",
			h.view(t).GetContext().GetText())
	}
	if !contains(panel.GetHeader(), "142.3k") {
		t.Fatalf("header = %q, want the same total", panel.GetHeader())
	}
}

func TestACategoryShareFallsBackToTheTotalWhenNoWindowIsStated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.MaxTokens = 0
	usage.TotalTokens = 100_000
	usage.Categories[0].Tokens = 25_000

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	if got := panel.GetCategories()[0].GetFigure(); got != "25k · 25%" {
		t.Fatalf("figure = %q, want the share against the total when no window is stated", got)
	}
}
