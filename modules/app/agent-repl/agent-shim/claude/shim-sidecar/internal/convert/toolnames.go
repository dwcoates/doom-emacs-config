package convert

// toolnames.go — the vendor's tool vocabulary, in one table.
//
// ASKUSERQUESTION IS ABSENT ON PURPOSE, and its absence is not a modelling gap:
// the stream plane authors that unit whole (see streamowned.go), so the name is
// dropped before it ever reaches this table.
//
// A NAME NOT LISTED HERE IS NOT AUTOMATICALLY UNMODELED. An MCP server's tool is
// AgentMcpToolCall (mcp.go); AgentUnmodeled means "a tool whose schema genuinely
// cannot be known" — one the vendor added after this schema was written. A recognizable built-in
// arriving there is a PRODUCER DEFECT, so the table below is what keeps the two
// apart, and the golden corpus is what keeps the table honest.

// toolKind classifies a tool name into the conversion that handles it.
type toolKind int

const (
	kindUnmodeled toolKind = iota
	kindRead
	kindWrite
	kindEdit
	kindGrep
	kindGlob
	kindBash
	kindSubagent
	kindSkill
	kindSendMessage
	kindTaskAct
	kindWebFetch
	kindWebSearch
	kindMonitor
	kindScheduleWakeup
	kindArtifact
	kindPlanMode
	kindReportFindings
	kindWorktree
	kindCron
	kindPushNotification
)

// builtinTools maps the vendor's tool names onto the unit kind each becomes.
var builtinTools = map[string]toolKind{
	"Read":             kindRead,
	"Write":            kindWrite,
	"Edit":             kindEdit,
	"Grep":             kindGrep,
	"Glob":             kindGlob,
	"Bash":             kindBash,
	"Agent":            kindSubagent,
	"Task":             kindSubagent,
	"Skill":            kindSkill,
	"SendMessage":      kindSendMessage,
	"TaskCreate":       kindTaskAct,
	"TaskUpdate":       kindTaskAct,
	"WebFetch":         kindWebFetch,
	"WebSearch":        kindWebSearch,
	"Monitor":          kindMonitor,
	"ScheduleWakeup":   kindScheduleWakeup,
	"Artifact":         kindArtifact,
	"EnterPlanMode":    kindPlanMode,
	"ExitPlanMode":     kindPlanMode,
	"ReportFindings":   kindReportFindings,
	"EnterWorktree":    kindWorktree,
	"ExitWorktree":     kindWorktree,
	"CronCreate":       kindCron,
	"CronDelete":       kindCron,
	"CronList":         kindCron,
	"PushNotification": kindPushNotification,
}

// classifyTool resolves a tool name to its conversion, and whether the name is a
// recognized built-in at all.
func classifyTool(name string) (toolKind, bool) {
	kind, ok := builtinTools[name]
	return kind, ok
}
