// asyncsplit.go holds THE ONE CURATION POINT at which the daemon decides
// whether an agent's output belongs to the TOP-LEVEL FEED or to a DETACHED
// WORK BUBBLE.
//
// THE DEFECT THIS REPAIRS. A detached agent writes its whole conversation into
// the same transcript the main agent does, flagged `isSidechain` and linked to
// the tool call that launched it. Curating that transcript without reading the
// flag put a subagent's responses into the top-level feed, interleaved with the
// main agent's, as though the user's own agent had said them.
//
// WHY THE SPLIT IS SINGLE. It would be possible to filter sidechain records at
// each push site instead — the live push, the durable replay, the reconnect
// snapshot. That is three rules, and the moment two of them disagree the same
// conversation renders differently depending on how a client arrived at it.
// CurateEvent is the only exported route from an event to conversation content:
// conversationDeltaFromEvent is UNEXPORTED, so no caller outside this package
// can obtain an unsplit delta at all. The split is therefore a property of the
// call graph rather than a discipline every push site has to remember.
package frontend

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Curation is one event's whole verdict: what lands in the top-level feed, what
// lands in a detached agent's bubble, and the daemon-internal facts the session
// controller's own curators need afterwards.
type Curation struct {
	// Feed is the top-level ConversationDelta, carrying ONLY items that belong
	// to the main conversation.
	//
	// It is non-nil whenever the event curated to anything at all, EVEN IF every
	// item it produced was routed to a bubble. The delta carries through_seq,
	// which is a frontend's replay cursor: swallowing an all-detached event
	// would leave every client's cursor stuck behind it and make the next resync
	// re-deliver the conversation from before it.
	Feed *frontendv1.ConversationDelta
	// Detached is the content routed away from the feed, grouped by the
	// detached agent that produced it.
	Detached []DetachedFold
	// Envelopes are the file-plane record envelopes of the FEED's items, keyed
	// by item uuid, for the curators that run after this one. A detached item's
	// envelope is not here: it left the feed, and the feed's curators must not
	// see it.
	Envelopes map[string]RecordEnvelope
	// ToolNames maps a tool_use id to the tool the agent named, harvested from
	// this event's own tool_use blocks. It is what lets a later detachment on
	// that call be reported as the EXPLICIT unclassified kind — naming the tool
	// it could not classify — rather than as an anonymous unknown.
	ToolNames map[string]string
	// Outcomes are the TYPED tool outcomes this event carried, correlated to
	// the calls they answer. They are the daemon's launch and retrieval
	// evidence for detached work — a background shell's task id, an async
	// agent's launch, a workflow's run — and they are harvested HERE rather
	// than by a second pass over the event elsewhere, so the classification and
	// the conversation are read from one traversal of the same record.
	Outcomes []ToolOutcome
	// WithheldDetached names the detached records this curation had no
	// AgentEmission arm to carry. It is REPORTED rather than dropped silently:
	// the caller logs it, because a bubble quietly missing records looks exactly
	// like a quiet agent.
	WithheldDetached []string
	// SuppressedInternalResumes names the re-drive request ids whose internal
	// instruction this curation removed from the conversation
	// (internalresume.go).
	//
	// It is the ONLY confirmation the daemon has that a re-drive reached the
	// vendor conversation: the instruction is in the transcript, so the submit
	// that carried it landed. The session controller discharges the owed
	// resumption off exactly this, which is why the fact is reported by the
	// curator rather than re-derived from the event somewhere else.
	//
	// It is populated even when the curation feeds nothing — an event carrying
	// only the instruction is precisely the ordinary case.
	SuppressedInternalResumes []string
}

// DetachedFold is one detached agent's contribution from a single event, in
// emission order and in EXACTLY the vocabulary the top-level feed uses.
//
// The emissions are the SAME *frontendv1.AgentEmission values the feed carries,
// produced by the SAME curation above — not re-derived from a second parse.
// That is the wire-level guarantee behind AsyncAgentUpdate: a frontend's
// renderer for a response bubble, a thinking block or a tool card is the same
// code in both places because the daemon produced the same message for both.
type DetachedFold struct {
	// SourceToolUseID is the tool_use id of the call that launched the agent,
	// as the transcript's own envelope records it. It is the primary handle a
	// bubble is addressed by, because it is the same id the launching tool call
	// carries.
	SourceToolUseID string
	// AgentID is the harness's own id for the detached agent, the secondary
	// handle for a record whose envelope names no source call.
	AgentID string
	// Emissions are the agent's output from this event.
	Emissions []*frontendv1.AgentEmission
	// TsMs is when the record was written, unix millis.
	TsMs int64
}

// ToolOutcome is one typed tool result together with the call it answers.
//
// The correlation is POSITIONAL on disk — a transcript's `toolUseResult` sits
// beside the tool_result block it belongs to and carries no id of its own — so
// it is resolved here, once, while the record is still whole. Downstream the
// association would be unrecoverable.
type ToolOutcome struct {
	// ToolUseID is the call this outcome answers.
	ToolUseID string
	// Result is the typed outcome, verbatim durable evidence.
	Result *datav1.ToolUseResult
	// FromDetachedAgent marks an outcome produced INSIDE a detached agent's
	// conversation. A detachment it launches is a NESTED dispatch, and this is
	// what lets the daemon set the new bubble's parent pointer rather than
	// hanging it at the top level.
	FromDetachedAgent bool
	// SourceToolUseID is the launching call of the detached agent that produced
	// the outcome, empty for a top-level one. It names the parent bubble.
	SourceToolUseID string
	// AgentID is the detached agent that produced the outcome, empty at the top
	// level.
	AgentID string
}

// CurateEvent is the ONE route from a store event to conversation content.
//
// It runs the ordinary curation and then partitions its result: a record the
// transcript flags `isSidechain` belongs to the detached agent that wrote it
// and is routed to that agent's fold; everything else is the main conversation
// and stays on the feed delta. Nothing reaches a frontend as a top-level item
// without passing this partition.
func CurateEvent(workspace, fence string, ev *corev1.Event) (Curation, error) {
	cd, envs, err := conversationDeltaFromEvent(workspace, fence, ev)
	if err != nil || cd == nil {
		return Curation{}, err
	}
	// THE DAEMON'S OWN RE-DRIVE HAS NO PROMPT (internalresume.go), and this is
	// the choke point every route from a store event to conversation content
	// passes through — the live push, a connect snapshot's backfill, a resync
	// replay and a store re-pull alike.
	//
	// It runs BEFORE the sidechain partition and before the empty check, so an
	// event carrying ONLY the internal instruction curates to nothing at all
	// rather than to an empty delta a client would still be handed.
	items, suppressed := dropInternalResumePrompt(cd.GetItems())
	cd.Items = items
	if len(items) == 0 {
		return Curation{SuppressedInternalResumes: suppressed}, nil
	}
	c := Curation{Feed: cd, Outcomes: toolOutcomes(ev), SuppressedInternalResumes: suppressed}
	var feed []*frontendv1.ConversationItem
	folds := map[string]*DetachedFold{}
	var order []string
	for _, item := range cd.GetItems() {
		env, hasEnv := envs[item.GetUuid()]
		if !hasEnv || !env.IsSidechain {
			feed = append(feed, item)
			if hasEnv {
				if c.Envelopes == nil {
					c.Envelopes = map[string]RecordEnvelope{}
				}
				c.Envelopes[item.GetUuid()] = env
			}
			c.ToolNames = harvestToolNames(c.ToolNames, item)
			continue
		}
		ems := detachedEmissions(item)
		if len(ems) == 0 {
			// A sidechain record with no emission arm to carry it — the
			// launching prompt the harness writes as the subagent's first user
			// record is the usual case, and the bubble's label already states
			// it. Withheld, never promoted to the feed: putting it there is the
			// very defect this split exists to repair.
			c.WithheldDetached = append(c.WithheldDetached, item.GetUuid())
			continue
		}
		key := env.SourceToolUseID
		if key == "" {
			key = "agent:" + env.AgentID
		}
		fold, seen := folds[key]
		if !seen {
			fold = &DetachedFold{SourceToolUseID: env.SourceToolUseID, AgentID: env.AgentID, TsMs: item.GetTsMs()}
			folds[key] = fold
			order = append(order, key)
		}
		fold.Emissions = append(fold.Emissions, ems...)
	}
	// The delta keeps its envelope (workspace, fence, through_seq) and loses
	// only the items that left it.
	cd.Items = feed
	for _, key := range order {
		c.Detached = append(c.Detached, *folds[key])
	}
	return c, nil
}

// toolOutcomes reads an event's typed tool outcomes off the file plane.
//
// A transcript user line carries ONE `toolUseResult`, positionally associated
// with the tool_result block in its own message. When the line carries exactly
// one such block the correlation is unambiguous and the outcome is claimed;
// when it carries several there is no evidence which one the result belongs to,
// and the outcome is left uncorrelated rather than attributed to a guess — a
// launch attributed to the wrong call would open a bubble under the wrong card
// and stamp the wrong tool's id on it.
func toolOutcomes(ev *corev1.Event) []ToolOutcome {
	vendor := ev.GetVendor()
	if vendor == nil {
		return nil
	}
	msg, err := vendor.UnmarshalNew()
	if err != nil {
		// The unmarshal failure is already surfaced, loudly and once, by
		// conversationDeltaFromEvent on the same Any.
		return nil
	}
	tl, ok := msg.(*datav1.TranscriptLine)
	if !ok {
		return nil
	}
	ul := tl.GetUser()
	if ul == nil || !ul.GetHasToolUseResult() {
		return nil
	}
	var ids []string
	for _, block := range ul.GetMessage().GetContentBlocks().GetBlocks() {
		if tr := block.GetToolResult(); tr.GetToolUseId() != "" {
			ids = append(ids, tr.GetToolUseId())
		}
	}
	if len(ids) != 1 {
		return nil
	}
	env := ul.GetEnvelope()
	return []ToolOutcome{{
		ToolUseID:         ids[0],
		Result:            ul.GetToolUseResult(),
		FromDetachedAgent: env.GetIsSidechain(),
		SourceToolUseID:   env.GetSourceToolUseId(),
		AgentID:           env.GetAgentId(),
	}}
}

// detachedEmissions converts one curated item into the emissions a bubble can
// carry.
//
// An agent item IS an emission and passes through unchanged. A sidechain USER
// record is the harness handing tool results back to the subagent, so its
// tool_result blocks become tool-result emissions — the same arm the feed's own
// tool cards use. A sidechain user record carrying prose and no tool results is
// the launch prompt, which the bubble's label already states; it yields nothing
// and its caller reports it withheld.
func detachedEmissions(item *frontendv1.ConversationItem) []*frontendv1.AgentEmission {
	switch it := item.GetItem().(type) {
	case *frontendv1.ConversationItem_Agent:
		return []*frontendv1.AgentEmission{it.Agent}
	case *frontendv1.ConversationItem_UserMessage:
		var out []*frontendv1.AgentEmission
		for _, block := range it.UserMessage.GetContentBlocks().GetBlocks() {
			if tr := block.GetToolResult(); tr != nil {
				out = append(out, &frontendv1.AgentEmission{
					Emission: &frontendv1.AgentEmission_ToolResult{
						ToolResult: &frontendv1.AgentToolResult{Result: tr},
					},
				})
			}
		}
		return out
	default:
		return nil
	}
}

// harvestToolNames records the tool each tool_use block in a FEED item named,
// keyed by its tool_use id.
//
// Only feed items are harvested, and that is deliberate: a detachment is
// launched by a call the MAIN agent made, so the tool whose name a bubble may
// have to report is always on the top-level conversation. Harvesting a
// subagent's own calls would let a nested tool name be attributed to the outer
// launch.
func harvestToolNames(into map[string]string, item *frontendv1.ConversationItem) map[string]string {
	var blocks []*datav1.ContentBlock
	switch it := item.GetItem().(type) {
	case *frontendv1.ConversationItem_Agent:
		blocks = it.Agent.GetResponse().GetBody().GetContent()
	default:
		return into
	}
	for _, block := range blocks {
		use := block.GetToolUse()
		if use.GetId() == "" || use.GetName() == "" {
			continue
		}
		if into == nil {
			into = map[string]string{}
		}
		into[use.GetId()] = use.GetName()
	}
	return into
}
