package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// fullUsage is a rich get_context_usage answer, so one arrangement can exercise
// every section of the panel. Its single category ("Messages") pairs with the
// message breakdown; the remaining detail lists trail as their own sections.
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
			{Label: "Messages", Tokens: 38_100, Color: "#8888ff"},
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

// sectionByLabel finds a top-level section by its label, failing the test when
// none matches — the panel's sections are label-keyed by design.
func sectionByLabel(t *testing.T, panel *frontendv1.ContextPanelView, label string) *frontendv1.ContextPanelSection {
	t.Helper()
	for _, s := range panel.GetSections() {
		if s.GetLabel() == label {
			return s
		}
	}
	t.Fatalf("no section labeled %q; sections = %v", label, sectionLabels(panel))
	return nil
}

func sectionLabels(panel *frontendv1.ContextPanelView) []string {
	labels := make([]string, 0, len(panel.GetSections()))
	for _, s := range panel.GetSections() {
		labels = append(labels, s.GetLabel())
	}
	return labels
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

func TestTheHeaderCarriesTheVendorsParts(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	header := panelOf(t, h, fullUsage()).GetHeader()

	// Assert
	if header.GetUsed() != "142.3k" || header.GetTotal() != "200k" ||
		header.GetPercent() != 71 || header.GetModel() != "claude-sonnet-4-5" {
		t.Fatalf("header = %+v, want the split used/total/percent/model", header)
	}
}

func TestTheHeaderPercentIsTheVendorsOwnFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.Percentage = 6

	// Act
	header := panelOf(t, h, usage).GetHeader()

	// Assert. The percent is verbatim, never re-derived from used/total.
	if header.GetPercent() != 6 {
		t.Fatalf("percent = %d, want the vendor's own 6", header.GetPercent())
	}
}

func TestAPairedCategoryKeepsItsVendorLabelAndFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.Categories = []*conversationv1.SessionContextCategory{
		{Label: "System prompt", Tokens: 38_100, Color: "#8888ff"},
	}

	// Act
	section := sectionByLabel(t, panelOf(t, h, usage), "System prompt")

	// Assert. The paired section takes the CATEGORY figure, not the detail sum.
	if section.GetFigure() != "38.1k · 19%" {
		t.Fatalf("figure = %q, want the composed tokens and share", section.GetFigure())
	}
	if got := section.GetItems()[0].GetLabel(); got != "tone" {
		t.Fatalf("first item = %q, want the paired system-prompt section", got)
	}
}

func TestTheMessagesSectionLaysEveryPlaneAsAnItem(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Messages")

	// Assert
	if got := len(section.GetItems()); got != 7 {
		t.Fatalf("plane items = %d, want all seven", got)
	}
}

func TestTheToolCallListIsANestedSection(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Messages")

	// Assert
	sub := section.GetSections()[0]
	if sub.GetLabel() != "tool calls" {
		t.Fatalf("nested section = %q, want the tool-call fold", sub.GetLabel())
	}
	if got := sub.GetItems()[0].GetFigure(); got != "call 1.2k · result 3.4k" {
		t.Fatalf("tool-call figure = %q, want the composed call/result split", got)
	}
}

func TestTheAttachmentsAreANestedSection(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Messages")

	// Assert
	sub := section.GetSections()[1]
	if sub.GetLabel() != "attachments" || sub.GetItems()[0].GetLabel() != "image" {
		t.Fatalf("nested section = %+v, want the per-attachment fold", sub)
	}
}

func TestASystemPromptSectionPairsItsSections(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "System prompt")

	// Assert
	if got := section.GetItems()[0].GetLabel(); got != "tone" {
		t.Fatalf("item = %q, want the system-prompt section name", got)
	}
}

func TestAMemoryFileRowNamesItsPathAndType(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Memory files")

	// Assert
	row := section.GetItems()[0]
	if row.GetLabel() != "webapp/CLAUDE.md · project" || row.GetFigure() != "2.1k" {
		t.Fatalf("row = %+v, want the composed label and figure", row)
	}
}

func TestAnMcpToolRowNamesItsToolServerAndLoadState(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "MCP tools")

	// Assert
	if got := section.GetItems()[0].GetLabel(); got != "list_prs · github · loaded" {
		t.Fatalf("label = %q, want the tool, its server and its load state", got)
	}
}

func TestAnUnstatedLoadStateSaysNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.McpTools[0].IsLoaded = nil

	// Act
	section := sectionByLabel(t, panelOf(t, h, usage), "MCP tools")

	// Assert
	if got := section.GetItems()[0].GetLabel(); got != "list_prs · github" {
		t.Fatalf("label = %q, want no claim where the vendor made none", got)
	}
}

func TestADeferredBuiltinToolSaysItIsDeferred(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "System tools (deferred)")

	// Assert
	if got := section.GetItems()[0].GetLabel(); got != "NotebookEdit · deferred" {
		t.Fatalf("label = %q, want the deferred state stated", got)
	}
}

func TestADeferredCategoryIsNotSwallowedBySystemTools(t *testing.T) {
	// Arrange. A "System tools (deferred)" category must pair with the deferred
	// built-in tools, not the plain system tools — the label match tests
	// "deferred" first.
	h := newHarness(t)
	usage := fullUsage()
	usage.Categories = []*conversationv1.SessionContextCategory{
		{Label: "System tools (deferred)", Tokens: 300},
	}

	// Act
	section := sectionByLabel(t, panelOf(t, h, usage), "System tools (deferred)")

	// Assert
	if got := section.GetItems()[0].GetLabel(); got != "NotebookEdit · deferred" {
		t.Fatalf("item = %q, want the deferred built-in tool, not a system tool", got)
	}
}

func TestAnAgentRowNamesItsTypeAndSource(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Custom agents")

	// Assert
	if got := section.GetItems()[0].GetLabel(); got != "Explore · builtin" {
		t.Fatalf("label = %q, want the type and the source", got)
	}
}

func TestTheSlashCommandsFigureIsTheRollup(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Slash commands")

	// Assert. The roll-up sentence is the section FIGURE, and there are no items.
	if section.GetFigure() != "12 of 31 commands · 4.2k" {
		t.Fatalf("figure = %q, want the composed sentence", section.GetFigure())
	}
	if len(section.GetItems()) != 0 {
		t.Fatalf("items = %v, want none for the slash-command roll-up", section.GetItems())
	}
}

func TestTheSkillsFigureIsTheRollupAndItsRowsAreItems(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	section := sectionByLabel(t, panelOf(t, h, fullUsage()), "Skills")

	// Assert
	if section.GetFigure() != "8 of 19 skills · 6.4k" {
		t.Fatalf("figure = %q, want the composed sentence", section.GetFigure())
	}
	if got := section.GetItems()[0].GetLabel(); got != "graphify · user" {
		t.Fatalf("skill row = %q, want the name and the source", got)
	}
}

func TestAnAbsentSkillsFactOmitsTheSkillsSection(t *testing.T) {
	// Arrange
	h := newHarness(t)
	usage := fullUsage()
	usage.Skills = nil

	// Act
	panel := panelOf(t, h, usage)

	// Assert
	for _, s := range panel.GetSections() {
		if s.GetLabel() == "Skills" {
			t.Fatalf("a Skills section stood where the vendor omitted the fact")
		}
	}
}

func TestAFreeSpaceCategoryHasNoDetail(t *testing.T) {
	// Arrange. An unrecognized category (Free space) is a bare row: no items,
	// no nested sub-folds, so the webapp draws it without a chevron.
	h := newHarness(t)
	usage := fullUsage()
	usage.Categories = []*conversationv1.SessionContextCategory{
		{Label: "Free space", Tokens: 57_700},
	}

	// Act
	section := sectionByLabel(t, panelOf(t, h, usage), "Free space")

	// Assert
	if len(section.GetItems()) != 0 || len(section.GetSections()) != 0 {
		t.Fatalf("Free space = %+v, want a childless row", section)
	}
	if section.GetFigure() != "57.7k · 29%" {
		t.Fatalf("figure = %q, want the composed share", section.GetFigure())
	}
}

func TestTheSectionsFollowTheVendorsCategoryOrder(t *testing.T) {
	// Arrange. The top-level rows lead with the vendor's categories in served
	// order; any unpaired detail trails afterwards.
	h := newHarness(t)
	usage := fullUsage()
	usage.Categories = []*conversationv1.SessionContextCategory{
		{Label: "System prompt", Tokens: 400},
		{Label: "Messages", Tokens: 38_100},
	}

	// Act
	labels := sectionLabels(panelOf(t, h, usage))

	// Assert
	if labels[0] != "System prompt" || labels[1] != "Messages" {
		t.Fatalf("leading sections = %v, want the served category order", labels[:2])
	}
}

func TestAnUnpairedDetailTrailsAsItsOwnSection(t *testing.T) {
	// Arrange. The vendor named only a Messages category but stated memory
	// files too; the memory files must not be dropped.
	h := newHarness(t)
	usage := fullUsage()
	usage.Categories = []*conversationv1.SessionContextCategory{
		{Label: "Messages", Tokens: 38_100},
	}

	// Act
	section := sectionByLabel(t, panelOf(t, h, usage), "Memory files")

	// Assert. Its figure is composed from the detail's own sum, since no
	// category stated one.
	if section.GetFigure() != "2.1k · 1%" {
		t.Fatalf("figure = %q, want the summed detail figure and share", section.GetFigure())
	}
}

func TestTheAutoCompactLineStatesItsThreshold(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	panel := panelOf(t, h, fullUsage())

	// Assert
	if panel.GetAutoCompactLine() != "auto-compact at 160k" {
		t.Fatalf("auto-compact = %q, want the stated threshold", panel.GetAutoCompactLine())
	}
}
