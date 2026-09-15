package topbar

import (
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// contextPanel resolves the /context panel from the SAME SessionContextUsage
// fact the context chip resolves from — the vendor's own get_context_usage
// answer, never an estimate and never derived from usage frames.
//
// The panel is a COLLAPSIBLE SECTION TREE: each of the vendor's top-level
// categories, in served order, becomes a section paired with its detail as leaf
// items (and, for Messages, a nested sub-fold for the tool-call list). Every
// label and every figure is COMPOSED HERE — the client does no arithmetic and
// only decides presentation (chevrons, palette colors, the header's percent
// gradient), so the resolver states the header's parts, each category's share,
// and each roll-up's sentence.
//
// A detail collection the vendor stated but did NOT surface as a category (the
// thin fake carries only four categories yet all detail lists) is not dropped:
// it is appended as a trailing section so the panel never loses a fact. In real
// vendor output every detail belongs to a category and nothing trails.
func (r *resolver) contextPanel(usage *conversationv1.SessionContextUsage) *frontendv1.ContextPanelView {
	basis := usage.GetMaxTokens()
	if basis <= 0 {
		basis = usage.GetTotalTokens()
	}
	out := &frontendv1.ContextPanelView{
		Header:          contextHeader(usage),
		AutoCompactLine: autoCompactLine(usage),
	}
	// Which detail kinds a category consumed, so the trailing safety net does
	// not re-emit them.
	consumed := map[detailKind]bool{}
	for _, category := range usage.GetCategories() {
		section := &frontendv1.ContextPanelSection{
			Label:  category.GetLabel(),
			Figure: joinNonEmpty(" · ", formatTokens(category.GetTokens()), percentOf(category.GetTokens(), basis)),
		}
		kind := classifyCategory(category)
		if kind != detailNone {
			consumed[kind] = true
			applyDetail(section, usage, kind)
		}
		out.Sections = append(out.Sections, section)
	}
	// Safety net: a detail the vendor stated but never named as a category is
	// surfaced as its own section rather than silently dropped. Its figure is
	// composed from the detail's own sum, since no category stated one.
	for _, kind := range detailOrder {
		if consumed[kind] || detailEmpty(usage, kind) {
			continue
		}
		sum := detailSum(usage, kind)
		section := &frontendv1.ContextPanelSection{
			Label:  detailLabel(kind),
			Figure: joinNonEmpty(" · ", formatTokens(sum), percentOf(sum, basis)),
		}
		// applyDetail may still override the figure with a roll-up sentence
		// (slash commands, skills), which is the more informative figure.
		applyDetail(section, usage, kind)
		out.Sections = append(out.Sections, section)
	}
	return out
}

// detailKind names a detail collection the panel can drill into. It is the
// bridge between a vendor category label and the structured list that details
// it, so the pairing lives in one place.
type detailKind int

const (
	detailNone detailKind = iota
	detailSystemPrompt
	detailSystemTools
	detailDeferredTools
	detailMcpTools
	detailAgents
	detailMemoryFiles
	detailSlashCommands
	detailSkills
	detailMessages
)

// detailOrder is the canonical order the safety net emits any unpaired detail
// in — the vendor's own broad ordering, so a thin fixture still reads sensibly.
var detailOrder = []detailKind{
	detailSystemPrompt, detailSystemTools, detailDeferredTools, detailMcpTools,
	detailAgents, detailMemoryFiles, detailSlashCommands, detailSkills, detailMessages,
}

// classifyCategory maps a vendor category to the detail it drills into, by its
// label (the vendor's own vocabulary). A category with no known detail — Free
// space, or anything unrecognized — pairs with detailNone and draws as a bare
// row.
//
// The DEFERRED test comes before the plain tools test: a "System tools
// (deferred)" category must not be swallowed by the "system tools" match. The
// category's own is_deferred flag is NOT used here — the fake marks "MCP tools"
// deferred, so the flag does not single out the deferred BUILT-IN tools.
func classifyCategory(category *conversationv1.SessionContextCategory) detailKind {
	label := strings.ToLower(strings.TrimSpace(category.GetLabel()))
	switch {
	case strings.Contains(label, "deferred"):
		return detailDeferredTools
	case strings.Contains(label, "mcp"):
		return detailMcpTools
	case strings.Contains(label, "system prompt"), strings.Contains(label, "prompt"):
		return detailSystemPrompt
	case strings.Contains(label, "tool"):
		return detailSystemTools
	case strings.Contains(label, "agent"):
		return detailAgents
	case strings.Contains(label, "memory"):
		return detailMemoryFiles
	case strings.Contains(label, "slash"), strings.Contains(label, "command"):
		return detailSlashCommands
	case strings.Contains(label, "skill"):
		return detailSkills
	case strings.Contains(label, "message"):
		return detailMessages
	default:
		return detailNone
	}
}

// detailLabel is the section label the safety net gives an unpaired detail. A
// paired detail keeps its vendor category label instead.
func detailLabel(kind detailKind) string {
	switch kind {
	case detailSystemPrompt:
		return "System prompt"
	case detailSystemTools:
		return "System tools"
	case detailDeferredTools:
		return "System tools (deferred)"
	case detailMcpTools:
		return "MCP tools"
	case detailAgents:
		return "Custom agents"
	case detailMemoryFiles:
		return "Memory files"
	case detailSlashCommands:
		return "Slash commands"
	case detailSkills:
		return "Skills"
	case detailMessages:
		return "Messages"
	default:
		return ""
	}
}

// applyDetail fills a section's items (and, for Messages, nested sub-folds) from
// the detail the kind names, and overrides the figure where the detail carries
// its own roll-up sentence (slash commands, skills).
func applyDetail(section *frontendv1.ContextPanelSection, usage *conversationv1.SessionContextUsage, kind detailKind) {
	switch kind {
	case detailSystemPrompt:
		for _, s := range usage.GetSystemPromptSections() {
			section.Items = append(section.Items, item(s.GetName(), s.GetTokens()))
		}
	case detailSystemTools:
		for _, t := range usage.GetSystemTools() {
			section.Items = append(section.Items, item(t.GetName(), t.GetTokens()))
		}
	case detailDeferredTools:
		for _, t := range usage.GetDeferredBuiltinTools() {
			loaded := t.GetIsLoaded()
			section.Items = append(section.Items, item(
				joinNonEmpty(" · ", t.GetName(), loadedWord(&loaded)), t.GetTokens()))
		}
	case detailMcpTools:
		for _, t := range usage.GetMcpTools() {
			section.Items = append(section.Items, item(
				joinNonEmpty(" · ", t.GetName(), t.GetServerName(), loadedWord(t.IsLoaded)), t.GetTokens()))
		}
	case detailAgents:
		for _, a := range usage.GetAgents() {
			section.Items = append(section.Items, item(
				joinNonEmpty(" · ", a.GetAgentType(), a.GetSource()), a.GetTokens()))
		}
	case detailMemoryFiles:
		for _, f := range usage.GetMemoryFiles() {
			section.Items = append(section.Items, item(
				joinNonEmpty(" · ", f.GetPath(), f.GetType()), f.GetTokens()))
		}
	case detailSlashCommands:
		if c := usage.GetSlashCommands(); c != nil {
			section.Figure = fmt.Sprintf("%d of %d commands · %s",
				c.GetIncludedCommands(), c.GetTotalCommands(), formatTokens(c.GetTokens()))
		}
	case detailSkills:
		if s := usage.GetSkills(); s != nil {
			// The roll-up sentence IS the section figure, as ruled — the count
			// and the total belong on the row, not hidden in a child.
			section.Figure = fmt.Sprintf("%d of %d skills · %s",
				s.GetIncludedSkills(), s.GetTotalSkills(), formatTokens(s.GetTokens()))
			for _, sk := range s.GetSkillFrontmatter() {
				section.Items = append(section.Items, item(
					joinNonEmpty(" · ", sk.GetName(), sk.GetSource()), sk.GetTokens()))
			}
		}
	case detailMessages:
		applyMessages(section, usage.GetMessageBreakdown())
	}
}

// applyMessages lays the message-plane rows as leaf items and folds the
// per-tool call/result list and the per-attachment list into NESTED sub-sections
// — the tool-call list especially is the longest, least-wanted detail, so it
// ships as its own collapsible child the webapp closes by default.
func applyMessages(section *frontendv1.ContextPanelSection, breakdown *conversationv1.SessionContextMessageBreakdown) {
	if breakdown == nil {
		return
	}
	section.Items = []*frontendv1.ContextPanelItem{
		item("tool calls", breakdown.GetToolCallTokens()),
		item("tool results", breakdown.GetToolResultTokens()),
		item("attachments", breakdown.GetAttachmentTokens()),
		item("assistant messages", breakdown.GetAssistantMessageTokens()),
		item("user messages", breakdown.GetUserMessageTokens()),
		item("redirected context", breakdown.GetRedirectedContextTokens()),
		item("unattributed", breakdown.GetUnattributedTokens()),
	}
	if calls := breakdown.GetToolCallsByType(); len(calls) > 0 {
		sub := &frontendv1.ContextPanelSection{Label: "tool calls"}
		for _, t := range calls {
			sub.Items = append(sub.Items, &frontendv1.ContextPanelItem{
				Label: t.GetName(),
				Figure: fmt.Sprintf("call %s · result %s",
					formatTokens(t.GetCallTokens()), formatTokens(t.GetResultTokens())),
			})
		}
		section.Sections = append(section.Sections, sub)
	}
	if attachments := breakdown.GetAttachmentsByType(); len(attachments) > 0 {
		sub := &frontendv1.ContextPanelSection{Label: "attachments"}
		for _, a := range attachments {
			sub.Items = append(sub.Items, item(a.GetName(), a.GetTokens()))
		}
		section.Sections = append(section.Sections, sub)
	}
}

// detailEmpty reports whether the detail a kind names carries nothing, so the
// safety net emits only sections that would show something.
func detailEmpty(usage *conversationv1.SessionContextUsage, kind detailKind) bool {
	switch kind {
	case detailSystemPrompt:
		return len(usage.GetSystemPromptSections()) == 0
	case detailSystemTools:
		return len(usage.GetSystemTools()) == 0
	case detailDeferredTools:
		return len(usage.GetDeferredBuiltinTools()) == 0
	case detailMcpTools:
		return len(usage.GetMcpTools()) == 0
	case detailAgents:
		return len(usage.GetAgents()) == 0
	case detailMemoryFiles:
		return len(usage.GetMemoryFiles()) == 0
	case detailSlashCommands:
		return usage.GetSlashCommands() == nil
	case detailSkills:
		return usage.GetSkills() == nil
	case detailMessages:
		return usage.GetMessageBreakdown() == nil
	default:
		return true
	}
}

// detailSum totals the tokens of the detail a kind names, so a trailing section
// the vendor never gave a category figure still states an honest share.
func detailSum(usage *conversationv1.SessionContextUsage, kind detailKind) int64 {
	var total int64
	switch kind {
	case detailSystemPrompt:
		for _, s := range usage.GetSystemPromptSections() {
			total += s.GetTokens()
		}
	case detailSystemTools:
		for _, t := range usage.GetSystemTools() {
			total += t.GetTokens()
		}
	case detailDeferredTools:
		for _, t := range usage.GetDeferredBuiltinTools() {
			total += t.GetTokens()
		}
	case detailMcpTools:
		for _, t := range usage.GetMcpTools() {
			total += t.GetTokens()
		}
	case detailAgents:
		for _, a := range usage.GetAgents() {
			total += a.GetTokens()
		}
	case detailMemoryFiles:
		for _, f := range usage.GetMemoryFiles() {
			total += f.GetTokens()
		}
	case detailSlashCommands:
		total = usage.GetSlashCommands().GetTokens()
	case detailSkills:
		total = usage.GetSkills().GetTokens()
	case detailMessages:
		b := usage.GetMessageBreakdown()
		total = b.GetToolCallTokens() + b.GetToolResultTokens() + b.GetAttachmentTokens() +
			b.GetAssistantMessageTokens() + b.GetUserMessageTokens() +
			b.GetRedirectedContextTokens() + b.GetUnattributedTokens()
	}
	return total
}

// contextHeader composes the panel's structured header. The PERCENTAGE IS THE
// VENDOR'S OWN figure, never re-derived; the client composes the one-line form
// and colors only the percent.
func contextHeader(usage *conversationv1.SessionContextUsage) *frontendv1.ContextPanelHeader {
	return &frontendv1.ContextPanelHeader{
		Used:    formatTokens(usage.GetTotalTokens()),
		Total:   formatTokens(usage.GetMaxTokens()),
		Percent: uint32(usage.GetPercentage()),
		Model:   usage.GetModel(),
	}
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
