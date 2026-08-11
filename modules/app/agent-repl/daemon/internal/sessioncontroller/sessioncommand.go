package sessioncontroller

import (
	"sort"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
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
// WHAT REPLACES THE DETACHED WORK is `frontend.v1.DaemonInterceptedCommandItem`, which carries
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
// only thing that can suppress a work.

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
//
// THE DURABILITY ARM IS AN INPUT, NOT A DERIVATION. Whether a record for this
// invocation exists is a fact about WHO ANSWERED the command — the CLI, which
// writes a transcript record for it, or the daemon alone, which writes nothing
// anywhere — and that is known at the dispatch site and nowhere else. Reading
// it back off the command enum here would be a second copy of the routing
// decision, free to disagree with the routing itself.
func sessionCommandItem(requestID string, command frontendv1.SessionCommand, tsMs int64, ephemeral bool) *frontendv1.Message {
	item := &frontendv1.Message{
		Uuid:      sessionCommandUUID(requestID),
		TsMs:      tsMs,
		RequestId: requestID,
		Lineage:   frontend.FeedRowLineage(sessionCommandUUID(requestID)),
		Payload: &frontendv1.Message_DaemonInterceptedCommand{
			DaemonInterceptedCommand: &frontendv1.DaemonInterceptedCommandItem{Command: command},
		},
	}
	if ephemeral {
		item.Durability = &frontendv1.Message_Ephemeral{Ephemeral: &frontendv1.MessageEphemeral{}}
		return item
	}
	item.Durability = &frontendv1.Message_Durable{Durable: &frontendv1.MessageDurable{}}
	return item
}

// pushSessionCommand retains and pushes the invocation item for one recognized
// session command.
//
// RETENTION IS CONDITIONAL ON THE EPHEMERAL ARM, and on nothing else. The
// branch reads the item's own durability oneof rather than inferring the class
// from the command, the payload kind or who called: the arm IS the claim that
// no record exists, and a second reading of that claim is a second chance to
// get it wrong.
//
// An EPHEMERAL invocation — one the daemon answered alone, which therefore
// reached no CLI and left no transcript record — is retained and replayed, on
// the same footing as a permission item and a failure card and for the same
// reason: it carries no store seq, so no from_seq a resync names could ever
// cover it, and it is the ONLY account of the invocation a frontend will ever
// get. Losing it on a reconnect would leave the feed silent about why the
// session's model changed.
//
// A DURABLE invocation is NOT retained. The CLI wrote a record for it, so the
// store serves it on reconnect; retaining it here would make one invocation
// arrive from two sources at once and draw it twice. Its account comes from the
// store, which is the single source that can also be paged.
//
// The live push happens either way: retention decides what a RECONNECT
// replays, never whether the frontend sees the invocation when it happens.
//
// outcome is what the daemon RESOLVED the command to, for the log only. It is
// empty for every command that resolves to nothing, and it never reaches the
// item: the item has no field to put it in, which is what keeps the argument
// the user typed off every frontend surface.
//
// `/model` fills it. An operator reading "session command SESSION_COMMAND_MODEL
// invoked" could not tell what the session was switched TO, or that a switch
// was the reason the picker disagreed with the session — the one line about the
// command named no model at all.
func (c *consumer) pushSessionCommand(requestID string, command frontendv1.SessionCommand, ephemeral bool, outcome string) {
	item := sessionCommandItem(requestID, command, c.now(), ephemeral)
	retained := item.GetEphemeral() != nil
	if retained {
		c.mu.Lock()
		if c.cmdItems == nil {
			c.cmdItems = map[string]*frontendv1.Message{}
		}
		if _, seen := c.cmdItems[item.GetUuid()]; !seen {
			c.cmdOrder = append(c.cmdOrder, item.GetUuid())
		}
		c.cmdItems[item.GetUuid()] = item
		c.mu.Unlock()
	}
	c.logf("session-controller: session command %s invoked ws=%q session=%s request_id=%s ephemeral=%t retained_for_replay=%t%s — pushed as a DaemonInterceptedCommandItem, NOT as a prompt bubble (a session command is not a prompt, and the item carries no prompt text)",
		command.String(), c.workspace, c.sessionID, requestID, item.GetEphemeral() != nil, retained, outcome)
	c.pushLocalItem(item)
}

// snapshotCommandItems returns the retained EPHEMERAL invocation items in
// first-seen order, taken under the lock so a concurrent pushSessionCommand
// cannot race the read.
//
// Every item here is ephemeral by construction — pushSessionCommand retains
// nothing else — so a replay of this snapshot can never double a command the
// store is already serving.
func (c *consumer) snapshotCommandItems() []*frontendv1.Message {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.Message, 0, len(c.cmdOrder))
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
//
// UNCHANGED BY THE DURABILITY SPLIT. Retention now holds ephemeral invocations
// only, and the cut's reason applies to exactly those: a durable invocation is
// floored by the store like everything else with a seq, while an ephemeral one
// has nothing but this to floor it.
func (c *consumer) dropCommandItems() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	n := len(c.cmdOrder)
	c.cmdItems, c.cmdOrder = nil, nil
	return n
}
