package sessioncontroller

import (
	"sort"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/protocmd"
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
// line of the majority of this workspace's turns. The set below is closed, IS
// the `SessionCommand` enum on the wire rather than a mirror of it, and is the
// only thing that can suppress a bubble.

// THE TABLE IS THE SCHEMA'S, NOT THIS FILE'S. The literal each command is
// typed as, and whether an argument may follow it, are carried as options on
// the `SessionCommand` enum values themselves and read back off the generated
// descriptor (protocmd). This file used to hold its own copy, the webapp held
// two more — its command list and its label table — and nothing compared the
// three: a corrected literal here left the frontend's chip rendering the old
// spelling, with each side's tests passing against its own copy.
//
// Ordered by enum number so recognition is deterministic. The descriptor read
// returns a map, and a table whose iteration order changed between runs would
// make which command a prompt matched depend on the map's seed.
var sessionCommandSpecs = orderedSessionCommandSpecs()

// sessionCommandSpec is ONE recognized session command, as this file needs it:
// the enum the wire names it by, beside the schema facts protocmd read back.
type sessionCommandSpec struct {
	command frontendv1.SessionCommand
	protocmd.Spec
}

// orderedSessionCommandSpecs sorts the schema's specs by enum number.
func orderedSessionCommandSpecs() []sessionCommandSpec {
	specs := protocmd.SessionCommandSpecs()
	out := make([]sessionCommandSpec, 0, len(specs))
	for command, spec := range specs {
		out = append(out, sessionCommandSpec{command: command, Spec: spec})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].command < out[j].command })
	return out
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
		if trimmed == spec.Literal {
			return spec.command, ""
		}
		if !spec.TakesArgs {
			continue
		}
		rest, ok := strings.CutPrefix(trimmed, spec.Literal)
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
