package topbar

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// contextPanel resolves the /context panel from the SAME SessionContextUsage
// fact the context chip resolves from — the vendor's own get_context_usage
// answer, never an estimate and never derived from usage frames.
//
// Every label and every figure is COMPOSED HERE: the client does no arithmetic,
// so the resolver states the header's percentage, each category's share, and
// each roll-up's sentence.
func (r *resolver) contextPanel(usage *conversationv1.SessionContextUsage) *frontendv1.ContextPanelView {
	basis := usage.GetMaxTokens()
	if basis <= 0 {
		basis = usage.GetTotalTokens()
	}
	out := &frontendv1.ContextPanelView{
		Header:          contextHeader(usage),
		AutoCompactLine: autoCompactLine(usage),
	}
	for _, category := range usage.GetCategories() {
		out.Categories = append(out.Categories, &frontendv1.ContextPanelCategory{
			Label:  category.GetLabel(),
			Figure: joinNonEmpty(" · ", formatTokens(category.GetTokens()), percentOf(category.GetTokens(), basis)),
			Color:  category.GetColor(),
		})
	}
	for _, file := range usage.GetMemoryFiles() {
		out.MemoryFiles = append(out.MemoryFiles, item(
			joinNonEmpty(" · ", file.GetPath(), file.GetType()), file.GetTokens()))
	}
	for _, tool := range usage.GetMcpTools() {
		out.McpTools = append(out.McpTools, item(
			joinNonEmpty(" · ", tool.GetName(), tool.GetServerName(), loadedWord(tool.IsLoaded)),
			tool.GetTokens()))
	}
	for _, tool := range usage.GetDeferredBuiltinTools() {
		loaded := tool.GetIsLoaded()
		out.DeferredBuiltinTools = append(out.DeferredBuiltinTools, item(
			joinNonEmpty(" · ", tool.GetName(), loadedWord(&loaded)), tool.GetTokens()))
	}
	for _, tool := range usage.GetSystemTools() {
		out.SystemTools = append(out.SystemTools, item(tool.GetName(), tool.GetTokens()))
	}
	for _, section := range usage.GetSystemPromptSections() {
		out.SystemPromptSections = append(out.SystemPromptSections,
			item(section.GetName(), section.GetTokens()))
	}
	for _, agent := range usage.GetAgents() {
		out.Agents = append(out.Agents, item(
			joinNonEmpty(" · ", agent.GetAgentType(), agent.GetSource()), agent.GetTokens()))
	}
	if commands := usage.GetSlashCommands(); commands != nil {
		out.SlashCommands = &frontendv1.ContextPanelRollup{
			Line: fmt.Sprintf("%d of %d commands · %s",
				commands.GetIncludedCommands(), commands.GetTotalCommands(),
				formatTokens(commands.GetTokens())),
		}
	}
	if skills := usage.GetSkills(); skills != nil {
		panel := &frontendv1.ContextPanelSkills{
			Line: fmt.Sprintf("%d of %d skills · %s",
				skills.GetIncludedSkills(), skills.GetTotalSkills(), formatTokens(skills.GetTokens())),
		}
		for _, skill := range skills.GetSkillFrontmatter() {
			panel.Skills = append(panel.Skills, item(
				joinNonEmpty(" · ", skill.GetName(), skill.GetSource()), skill.GetTokens()))
		}
		out.Skills = panel
	}
	if breakdown := usage.GetMessageBreakdown(); breakdown != nil {
		out.MessageBreakdown = messageBreakdown(breakdown)
	}
	return out
}

// contextHeader composes the panel's one-line summary. The PERCENTAGE IS THE
// VENDOR'S OWN figure, never re-derived.
func contextHeader(usage *conversationv1.SessionContextUsage) string {
	return joinNonEmpty(" · ",
		fmt.Sprintf("%s of %s (%d%%)",
			formatTokens(usage.GetTotalTokens()),
			formatTokens(usage.GetMaxTokens()),
			usage.GetPercentage()),
		usage.GetModel())
}

// autoCompactLine states where auto-compaction stands. Three readings, not two:
// off, on with a threshold, and on with none stated.
func autoCompactLine(usage *conversationv1.SessionContextUsage) string {
	if !usage.GetIsAutoCompactEnabled() {
		return "auto-compact off"
	}
	if usage.AutoCompactThreshold == nil {
		return "auto-compact on"
	}
	return "auto-compact at " + formatTokens(usage.GetAutoCompactThreshold())
}

// messageBreakdown composes the message-plane rows, the per-tool call/result
// rows the webapp folds, and the per-attachment rows.
func messageBreakdown(breakdown *conversationv1.SessionContextMessageBreakdown) *frontendv1.ContextPanelMessageBreakdown {
	out := &frontendv1.ContextPanelMessageBreakdown{
		Planes: []*frontendv1.ContextPanelItem{
			item("tool calls", breakdown.GetToolCallTokens()),
			item("tool results", breakdown.GetToolResultTokens()),
			item("attachments", breakdown.GetAttachmentTokens()),
			item("assistant messages", breakdown.GetAssistantMessageTokens()),
			item("user messages", breakdown.GetUserMessageTokens()),
			item("redirected context", breakdown.GetRedirectedContextTokens()),
			item("unattributed", breakdown.GetUnattributedTokens()),
		},
	}
	for _, tool := range breakdown.GetToolCallsByType() {
		out.ToolCalls = append(out.ToolCalls, &frontendv1.ContextPanelItem{
			Label: tool.GetName(),
			Figure: fmt.Sprintf("call %s · result %s",
				formatTokens(tool.GetCallTokens()), formatTokens(tool.GetResultTokens())),
		})
	}
	for _, attachment := range breakdown.GetAttachmentsByType() {
		out.Attachments = append(out.Attachments, item(attachment.GetName(), attachment.GetTokens()))
	}
	return out
}

// item is one composed label-and-figure row.
func item(label string, tokens int64) *frontendv1.ContextPanelItem {
	return &frontendv1.ContextPanelItem{
		Label:  truncate(label, DefaultLineWidth),
		Figure: formatTokens(tokens),
	}
}

// loadedWord states whether a tool's schema is in context. UNSET means the
// vendor said nothing, and the row then says nothing either rather than
// claiming one state.
func loadedWord(loaded *bool) string {
	if loaded == nil {
		return ""
	}
	if *loaded {
		return "loaded"
	}
	return "deferred"
}
