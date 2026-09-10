package convert

// exempt.go — THE EXEMPT SET: built-ins deliberately NOT carried.
//
// A DROP IS NOT RESIDUE. These are tools this system knows perfectly well and
// has decided the feed does not show — a task-status poll, a tool-schema search.
// Filing them as `unknown` would say "we do not model this" (false, and it would
// pollute the very query that finds real modelling gaps); filing them as
// AgentUnmodeled would say "no schema can exist for this" (also false). So they
// are dropped entirely, and the drop is logged so the decision stays visible.
//
// ONE CARVE-OUT, ruled: the TaskStop CALL is dropped, but its RESULT is CONSUMED
// as the owning task's CANCELLED terminal before the drop — deliberately-stopped
// work must resolve cancelled, never LOST.

// exemptTools are dropped at the call and at the result alike.
var exemptTools = map[string]bool{
	// The background-task control surface: polling and listing are machinery,
	// not conversation.
	"TaskStop":   true,
	"TaskOutput": true,
	"TaskGet":    true,
	"TaskList":   true,
	// The agent looking up its own tool schemas.
	"ToolSearch": true,
	// Not modeled by decision rather than by ignorance; no feed draws notebooks.
	"NotebookEdit": true,
	"REPL":         true,
	// The MCP resource family: server-side resource plumbing.
	"ListMcpResources": true,
	"ReadMcpResource":  true,
	// Feedback to the vendor is not part of this conversation.
	"SendFeedback": true,
}

// IsExempt reports whether a tool is dropped entirely.
func IsExempt(tool string) bool { return exemptTools[tool] }

// taskStopTool is the one exempt tool whose RESULT is consumed before the drop.
const taskStopTool = "TaskStop"

// settlesLater names the kinds whose RESULT deliberately produces no entry,
// distinct from a result this converter FAILED to settle.
//
// THE DISTINCTION IS LOAD-BEARING. Both cases produce no activity, and treating
// them alike files a perfectly-handled record as residue — which then shows up as
// a mapping gap in exactly the query built to find real ones.
//
//   - A SKILL's own return is a bare acknowledgement restating the name. The unit
//     settles when its DOCUMENT lands, joined by sourceToolUseID.
//   - A MONITOR is always detached; arming it does not end it. The result only
//     acknowledges the arm, so the announcement stands and the watch's own end is
//     what settles it.
//   - A SHELL THAT MOVED to the background did not end. The vendor returns the
//     SAME receipt for a command that finished and for one it launched — empty
//     output and a `backgroundTaskId` — so this one is decided from the RESULT
//     rather than from the kind, and the detached-work frames naming the unit
//     are what settle it. It is the stream plane's rule too
//     (convert/tools/bash.ts): both planes write this unit under one upsert key,
//     and a terminal from either of them says the work concluded when it had
//     not.
func settlesLater(kind toolKind, result map[string]any) bool {
	return kind == kindSkill || kind == kindMonitor || (kind == kindBash && bashMovedToBackground(result))
}
