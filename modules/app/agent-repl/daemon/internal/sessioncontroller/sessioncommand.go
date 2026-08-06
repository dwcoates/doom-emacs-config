package sessioncontroller

import (
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// THE SESSION COMMANDS: the slash commands the CLI answers ITSELF, the one
// table that names them, and the conversation item the daemon pushes in place
// of each one's prompt bubble.
//
// A session command is not a prompt. `/model` never reaches the model, and
// neither does `/cost`, `/context`, or any other name below — the CLI resolves
// them inside itself and the vendor is not in the loop. Echoing one as a
// purple user bubble therefore states something false twice over: that the
// user said it to the agent, and that the agent received it.
//
// WHAT REPLACES THE BUBBLE is `frontend.v1.SessionCommandItem`, which carries
// the command's IDENTITY and no text at all. That absence is the whole design:
// the item has no field a prompt could be put in, so no consumer can render
// the submitted text and no producer can leak an argument the user typed
// (`/model opus`) onto a surface with no business showing it. See the message's
// own comment in frontend.proto.
//
// WHY AN ALLOWLIST AND NOT A `/`-PREFIX RULE. A custom command — a skill, a
// project command — EXPANDS into a prompt for the agent, so the text the user
// typed really is that turn's opening and really does belong in the feed.
// Suppressing every slash-prefixed submit would silently delete the opening
// line of the majority of this workspace's turns. The set below is closed, is
// mirrored one-for-one by the `SessionCommand` enum on the wire, and is the
// only thing that can suppress a bubble.

// sessionCommandSpec is ONE recognized session command: the enum the wire
// names it by, the literal the user types, and whether an argument may follow.
type sessionCommandSpec struct {
	command frontendv1.SessionCommand
	literal string
	// takesArgs allows `/<literal> <anything>` to be recognized as this
	// command. FALSE IS THE DEFAULT AND THE SAFE SIDE: a command marked as
	// taking no arguments is recognized only when it is the ENTIRE prompt, so
	// "/status of the build" stays a prompt and keeps its bubble. Marking a
	// command that takes none as taking some is the one way this table can
	// swallow something a user genuinely wrote, which is why the flag is set
	// only for commands with a documented argument form.
	takesArgs bool
}

// sessionCommandSpecs is THE table. Ordering is the enum's, not significance.
var sessionCommandSpecs = []sessionCommandSpec{
	// `/clear` takes no argument on purpose, and the exactness matters more
	// here than anywhere else in the table: "/clear the build cache" is a
	// prompt, and mistaking it for the command that DISCARDS THE CONVERSATION
	// would destroy the context the user was speaking into.
	{frontendv1.SessionCommand_SESSION_COMMAND_CLEAR, "/clear", false},
	// `/compact <instructions>` steers what the summary keeps.
	{frontendv1.SessionCommand_SESSION_COMMAND_COMPACT, "/compact", true},
	// `/model <name>` selects the model inline.
	{frontendv1.SessionCommand_SESSION_COMMAND_MODEL, "/model", true},
	{frontendv1.SessionCommand_SESSION_COMMAND_COST, "/cost", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_USAGE, "/usage", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_STATUS, "/status", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_CONTEXT, "/context", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_CONFIG, "/config", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_HELP, "/help", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_DOCTOR, "/doctor", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_LOGIN, "/login", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_LOGOUT, "/logout", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_MEMORY, "/memory", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_PERMISSIONS, "/permissions", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_AGENTS, "/agents", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_MCP, "/mcp", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_HOOKS, "/hooks", false},
	// `/output-style <name>` selects a style inline.
	{frontendv1.SessionCommand_SESSION_COMMAND_OUTPUT_STYLE, "/output-style", true},
	{frontendv1.SessionCommand_SESSION_COMMAND_RELEASE_NOTES, "/release-notes", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_TODOS, "/todos", false},
	// `/export <file>` names where the transcript goes.
	{frontendv1.SessionCommand_SESSION_COMMAND_EXPORT, "/export", true},
	// `/add-dir <path>` is the whole point of the command.
	{frontendv1.SessionCommand_SESSION_COMMAND_ADD_DIR, "/add-dir", true},
	// `/resume <session>` names the session to resume.
	{frontendv1.SessionCommand_SESSION_COMMAND_RESUME, "/resume", true},
	{frontendv1.SessionCommand_SESSION_COMMAND_EXIT, "/exit", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_PRIVACY_SETTINGS, "/privacy-settings", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_STATUSLINE, "/statusline", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_TERMINAL_SETUP, "/terminal-setup", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_VIM, "/vim", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_REWIND, "/rewind", false},
	{frontendv1.SessionCommand_SESSION_COMMAND_BUG, "/bug", false},
}

// lookupSessionCommand reports which session command a submitted prompt IS —
// or UNSPECIFIED when it is an ordinary prompt — together with the ARGUMENT
// that followed it, empty for the bare form.
//
// Matched on the TRIMMED submitted text, which is where the daemon sees it: the
// CLI recognizes these itself and never yields the command back on the stream,
// so by the time anything is on the file plane the command has already run.
//
// An argument is admitted only for a command whose table entry allows one, and
// only behind whitespace: `/models` must never match `/model`, and `/modelfoo`
// must never match it either.
//
// THE ARGUMENT IS RETURNED, NOT DISCARDED, because for `/model <name>` it is
// the whole operation: the daemon performs that command itself through
// Manager.SetModel rather than forwarding the text (promptdispatch.go), so the
// name has to survive the reading. It still never reaches the wire — the
// invocation item has no field to put it in.
func lookupSessionCommand(text string) (frontendv1.SessionCommand, string) {
	trimmed := strings.TrimSpace(text)
	for _, spec := range sessionCommandSpecs {
		if trimmed == spec.literal {
			return spec.command, ""
		}
		if !spec.takesArgs {
			continue
		}
		rest, ok := strings.CutPrefix(trimmed, spec.literal)
		if ok && rest != "" && strings.TrimLeft(rest, " \t") != rest {
			return spec.command, strings.TrimSpace(rest)
		}
	}
	return frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, ""
}

// sessionCommandUUID is the item identity one invocation is pushed under.
// Derived from the submit's request id so a resync re-push REPLACES the
// standing item rather than adding a second one, exactly as a prompt receipt's
// uuid does.
func sessionCommandUUID(requestID string) string { return "session-command:" + requestID }

// sessionCommandItem composes THE invocation item, and is the ONE construction
// of it in the daemon.
//
// It carries the command and nothing else. There is deliberately no `text`
// parameter to forget to omit: the submitted prompt does not reach this
// function, so it cannot reach the wire.
func sessionCommandItem(requestID string, command frontendv1.SessionCommand, tsMs int64) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:      sessionCommandUUID(requestID),
		TsMs:      tsMs,
		RequestId: requestID,
		Item: &frontendv1.ConversationItem_SessionCommand{
			SessionCommand: &frontendv1.SessionCommandItem{Command: command},
		},
	}
}

// pushSessionCommand retains and pushes the invocation item for one recognized
// session command.
//
// RETAINED AND REPLAYED, on the same footing as a permission item, a failure
// card and a prompt receipt, and for the same reason: it carries no store seq,
// so no from_seq a resync names could ever cover it. It is also the ONLY record
// of the invocation a frontend will ever get — the CLI's own transcript
// bookkeeping for the command is withheld as machinery (machinery.go), and the
// receipt that would otherwise stand in for it was deliberately not pushed —
// so losing it on a reconnect would leave the feed with no account of why the
// session's model changed.
func (c *consumer) pushSessionCommand(requestID string, command frontendv1.SessionCommand) {
	item := sessionCommandItem(requestID, command, c.now())
	c.mu.Lock()
	if c.cmdItems == nil {
		c.cmdItems = map[string]*frontendv1.ConversationItem{}
	}
	if _, seen := c.cmdItems[item.GetUuid()]; !seen {
		c.cmdOrder = append(c.cmdOrder, item.GetUuid())
	}
	c.cmdItems[item.GetUuid()] = item
	c.mu.Unlock()
	c.logf("session-controller: session command %s invoked ws=%q session=%s request_id=%s — pushed as a SessionCommandItem, NOT as a prompt bubble (a session command is not a prompt, and the item carries no prompt text)",
		command.String(), c.workspace, c.sessionID, requestID)
	c.pushLocalItem(item)
}

// snapshotCommandItems returns the retained invocation items in first-seen
// order, taken under the lock so a concurrent pushSessionCommand cannot race
// the read.
func (c *consumer) snapshotCommandItems() []*frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.ConversationItem, 0, len(c.cmdOrder))
	for _, id := range c.cmdOrder {
		out = append(out, c.cmdItems[id])
	}
	return out
}

// dropCommandItems discards every retained invocation item, reporting how many
// went.
//
// Called from the same context cut that drops the prompt receipts
// (noteClearOrCompact), and for the identical reason: these carry no seq, so
// nothing else would ever floor them, and an invocation from BELOW the cut
// replayed above it would sit in a feed the cut exists to open.
func (c *consumer) dropCommandItems() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	n := len(c.cmdOrder)
	c.cmdItems, c.cmdOrder = nil, nil
	return n
}
