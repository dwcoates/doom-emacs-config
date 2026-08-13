package sessioncontroller

import (
	"sort"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"

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
// IT IS ALWAYS EPHEMERAL, AND THERE IS NO PARAMETER TO SAY OTHERWISE. This
// function is now reached only for a command the DAEMON answered itself
// (noteSessionCommand), which by definition never reached the CLI and left no
// transcript record anywhere, so no durable account of it can exist. A
// CLI-handled command's account comes from the record the CLI wrote, and
// nothing here produces a second one — with no durable arm reachable from this
// constructor, a daemon-produced twin of a stored invocation cannot be built at
// all, which is a stronger guarantee than a caller passing the right flag.
//
// THE ARM IS STATED THROUGH THE CONSTRUCTOR, never assigned here. It writes the
// class and the lineage that class requires in ONE act, so an item cannot claim
// a class and carry lineage that contradicts it. A hand-assigned arm beside a
// hand-written lineage is two statements of one fact, which is one more than can
// be kept true.
//
// IT RETURNS AN ERROR because the constructors refuse rather than repair, and
// the caller's job on a refusal is to keep the unclassified item OFF the wire:
// an item with no durability arm would later read as "the store lost this",
// which is the exact confusion the class exists to end.
func sessionCommandItem(requestID string, command frontendv1.SessionCommand, tsMs int64) (*frontendv1.Message, error) {
	body := &frontendv1.Message{
		Uuid:      sessionCommandUUID(requestID),
		TsMs:      tsMs,
		RequestId: requestID,
		Payload: &frontendv1.Message_DaemonInterceptedCommand{
			DaemonInterceptedCommand: &frontendv1.DaemonInterceptedCommandItem{Command: command},
		},
	}
	return frontend.NewEphemeralFeedRow(body)
}

// pushSessionCommand retains and pushes the invocation item for one recognized
// session command.
//
// EVERY INVOCATION THAT REACHES HERE IS EPHEMERAL, so every one is retained.
// The caller (noteSessionCommand) admits only the commands the daemon answered
// alone: those reached no CLI and left no transcript record, so this item is the
// ONLY account of the invocation a frontend will ever get. It is retained and
// replayed on the same footing as a permission item and a failure card, and for
// the same reason — it carries no store seq, so no from_seq a resync names
// could ever cover it. Losing it on a reconnect would leave the feed silent
// about why the session's model changed.
//
// A DURABLE invocation never gets here at all. The CLI wrote a record for it,
// the store serves that record on reconnect, and machinery.go turns it into the
// one item drawn for it. Producing a second here would make one invocation
// arrive from two sources at once and draw it twice.
//
// THE RETENTION CHECK STILL READS THE ITEM'S OWN ARM rather than assuming the
// class the caller intended. The constructor is what states the class, and a
// retention decision made on anything other than the stated arm is a second
// reading of one fact.
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
func (c *consumer) pushSessionCommand(requestID string, command frontendv1.SessionCommand, outcome string) {
	item, err := sessionCommandItem(requestID, command, c.now())
	if err != nil {
		// THE ITEM IS WITHHELD, LOUDLY. The constructor refuses rather than
		// repairs, and pushing the unclassified item anyway would put a message
		// with no durability arm on the wire — which a later reader cannot tell
		// apart from a durable message the store lost. The invocation is
		// reported in full instead, with the command and request that produced
		// it, because the refusal is a daemon defect and not a user-visible
		// condition anything downstream can act on.
		c.warn("session-controller: session command %s NOT pushed ws=%q session=%s request_id=%s — the message constructor refused it, so no DaemonInterceptedCommandItem could be classified and none is delivered: %v",
			command.String(), c.workspace, c.sessionID, requestID, err)
		return
	}
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
