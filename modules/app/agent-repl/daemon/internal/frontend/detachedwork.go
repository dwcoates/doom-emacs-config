// detachedwork.go is the daemon's ONE detached-work apparatus: the single site
// that mints a detached-work message, the single constructor that turns a
// resolved classification into that message, and the update producers that
// derive their wire arm FROM the work's own kind rather than from a caller's
// claim about it.
//
// DETACHED WORK IS A MESSAGE, AND ONLY A MESSAGE. It has identity, provenance,
// containment, a timestamp and content that accumulates, which is what a message
// is. So this file produces a *frontendv1.Message whose payload arm is
// DetachedWork: the message's uuid IS the work's id, and the message's lineage
// IS the work's place in the feed. There is no second id space and no second
// containment tree for anything here to bridge.
//
// AN UPDATE IS NOT A MESSAGE. Emissions folded into a detached agent and bytes
// appended to a spool are that ONE message's accumulating payload; they are
// pushed as DetachedWorkUpdate addressed to the message's id and never become
// messages of their own. A shell producing a megabyte would otherwise mint
// thousands of feed rows.
//
// THE INVARIANTS THIS FILE ENFORCES STRUCTURALLY
//
//   - ONE MINTING SITE. mintDetachedWorkID is unexported and OpenDetachedWork is
//     its only caller. Nothing outside this file can name it, so no second site
//     can invent an id and no id can be resolved twice with two answers. A caller
//     that needs the id reads it back off the message it opened.
//
//   - LINEAGE IS SET WHERE THE ID IS MINTED. top_level_message_id is never empty
//     because the only site that can produce a detached-work message is the same
//     site that stamps it, and it is self-referential exactly when there is no
//     parent. A caller cannot supply half of a lineage: a parent id without its
//     top-level root, or a root without its parent, is refused by name.
//
//   - ARMS MATCH THE KIND BY CONSTRUCTION. Every update producer takes the
//     MESSAGE, finds the kind arm on its payload, and selects the update arm from
//     that same switch. There is no code path that accepts an arm choice from a
//     caller, so a journal update cannot be addressed to shell work even by
//     mistake — the producer returns an error naming both kinds instead.
//
//   - from_offset COMES FROM THE SPOOL. AppendDetachedOutput reads the cursor off
//     DetachedWorkOutputSpool.through_offset, stamps it on the append, and
//     advances the same field, in one function. The spool IS the cursor; there is
//     no second counter that could drift from it.
//
//   - THE FOLD AND THE PUSH ARE ONE OPERATION. Every producer mutates the
//     folded-to-date message (what StateSnapshot.detached_work serves) and
//     returns the incremental update (what DetachedWorkDelta pushes) from the
//     same call. A snapshot and a delta cannot disagree about what the fold
//     contains because neither is produced without the other.
//
//   - THE CAP IS A DAEMON FACT. StreamItemCap is applied here and reported on
//     DetachedWorkFold, so a fold that dropped its oldest entries is
//     distinguishable from a complete one, and two frontends cannot disagree
//     about what the user is being shown.
package frontend

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
)

// StreamItemCap is the tail cap the daemon applies to the item-counted folds
// (an agent's emissions, a workflow's rows). A detached agent can run to
// thousands of emissions and a fold is a glance at what the work is doing, not
// a second feed — so the TAIL is kept and the drop is REPORTED on
// DetachedWorkFold rather than being silent.
const StreamItemCap = 200

// DetachKind is the daemon's RESOLVED classification of one detachment: which
// DetachedWork kind arm the work is, decided once and then carried. It is a
// daemon-internal vocabulary and never reaches the wire; its only job is to
// make "the kind" a single value that both the payload's arm and every later
// update's arm are derived from.
//
// There is no zero-valued "unknown" member that means "decide later".
// DetachUnrecognized is the EXPLICIT verdict for a tool the daemon does not
// know, and it carries the tool's name; the zero value is refused at
// construction.
type DetachKind int

const (
	// DetachUnresolved is the zero value and is never a valid classification.
	DetachUnresolved DetachKind = iota
	// DetachAgent is a detached agent: a whole conversation happening elsewhere.
	DetachAgent
	// DetachShell is a backgrounded shell command: an opaque byte spool.
	DetachShell
	// DetachWorkflow is a Workflow run: a journal.jsonl step log.
	DetachWorkflow
	// DetachUnrecognized is a spawn whose TOOL the daemon does not recognize.
	// An explicit arm on the contract, never a fallback: the daemon states that
	// it could not classify the tool, names it, and streams the output anyway.
	DetachUnrecognized
	// DetachMerge is a merge run: the conversation a merge drives through the
	// workspace's OWN session. No tool spawns it — the daemon opens it when it
	// classifies the merge skill's invocation (see MergeSkillCall) — and its
	// content is the same emission vocabulary a detached agent's is.
	DetachMerge
	// DetachSkill is a skill invocation: the card the feed used to render flat,
	// now work that owns its window. It is the merge kind's sibling and not its
	// special case — detached-work.proto says "`merge` is the one skill with an
	// arm of its own … every other skill arrives as `skill`" — and it carries
	// two facts a merge run has no room for: the skill file's body, and the
	// name and arguments the call was made with.
	DetachSkill
)

// String names the kind for log records and for the error a mismatched update
// producer returns.
func (k DetachKind) String() string {
	switch k {
	case DetachAgent:
		return "agent"
	case DetachShell:
		return "shell"
	case DetachWorkflow:
		return "workflow"
	case DetachUnrecognized:
		return "unclassified"
	case DetachMerge:
		return "merge"
	case DetachSkill:
		return "skill"
	default:
		return "unresolved"
	}
}

// DetachKindFromTaskKind translates the shim's typed core.v1 TaskKind into the
// daemon's classification. UNSPECIFIED does NOT map to DetachUnrecognized: the
// enum being unset says the shim named no kind, which is a different fact from
// "the daemon recognizes no such tool", and conflating them would let a shim
// omission be reported to the user as an unknown tool. It returns
// DetachUnresolved, and the caller decides — with the tool name in hand —
// whether this is the unclassified arm or a fault.
func DetachKindFromTaskKind(k protocolv1.TaskKind) DetachKind {
	switch k {
	case protocolv1.TaskKind_TASK_KIND_AGENT:
		return DetachAgent
	case protocolv1.TaskKind_TASK_KIND_SHELL:
		return DetachShell
	case protocolv1.TaskKind_TASK_KIND_WORKFLOW:
		return DetachWorkflow
	default:
		return DetachUnresolved
	}
}

// DetachedWorkSpec is everything OpenDetachedWork needs to resolve one
// detachment into a message. It carries no id: the id is minted inside, which is
// what makes the minting site single.
type DetachedWorkSpec struct {
	// TaskID is the detachment's identity in the shim's task vocabulary. It is
	// what the message id is derived from, so the same detachment resolves to
	// the same message across a replay instead of accumulating twins.
	TaskID string
	// Workspace is the workspace the work runs under. Required: it is the only
	// routing key a snapshot has for detached work, and work that carried none
	// would be delivered to every scoped client — which is the leak the
	// contract's workspace field exists to close.
	Workspace string
	// Kind is the resolved classification. DetachUnresolved is refused.
	Kind DetachKind
	// OriginToolUseID is the tool_use id of the call that detached the work.
	// Required unless NoSpawningCall states there was no such call: a detachment
	// the daemon cannot attribute to a call it believes exists is a daemon fault
	// its caller surfaces as a failure card, never blank-origin work.
	//
	// IT IS PROVENANCE, NOT CONTAINMENT. It says which call STARTED the work;
	// what CONTAINS the work is ParentMessageID below. Reading one as the other
	// is what made "is this a top-level message" unanswerable.
	OriginToolUseID string
	// NoSpawningCall states that NO tool call spawned this work, which the
	// contract admits by design (detached-work.proto origin_tool_use_id: "Empty
	// only for work that no tool call spawned"). It must be set DELIBERATELY, by
	// a caller holding evidence that the launch announcement named no call at
	// all — never as a way to get past a missing attribution.
	//
	// IT IS WHY THE BLANK-ORIGIN REFUSAL BELOW SURVIVES. Without this flag the
	// only way to admit announcement-born work was to drop that refusal, and
	// then every genuinely unattributable detachment — one whose spawning call
	// exists but could not be found — would quietly open work whose origin
	// points nowhere instead of raising the fault it is.
	NoSpawningCall bool
	// Parent is the MESSAGE this work is contained by, nil when the work sits
	// directly in the feed. Detached agents dispatch detached agents, so the
	// containment tree is real — but it is the ONE message tree
	// (MessageLineage.parent_message_id) rather than a second tree walking only
	// detached work.
	//
	// IT IS THE MESSAGE, NOT ITS ID, for the same reason NewDurableChild takes
	// one: an id cannot be asked what its durability class or its root is, so a
	// spec carrying ids could name a parent whose class makes this child
	// unreachable and nothing could tell. Carrying the message also removes the
	// half-lineage case entirely — a parent and its root arrive together or not
	// at all, so there is no pairing left to keep in step.
	Parent *frontendv1.Message
	// Label is the face the collapsed fold shows. Empty is allowed and means
	// the daemon had no label; the client then shows the id.
	Label string
	// ToolName is the tool the agent named, REQUIRED for DetachUnrecognized and
	// ignored otherwise. It is the fact that makes the unclassified arm useful
	// rather than merely honest.
	ToolName string
	// Command is the backgrounded command line, for DetachShell only. Empty
	// means the daemon has no reconstructible command line.
	Command string
	// SkillName is the skill as the call named it, verbatim, REQUIRED for
	// DetachSkill and ignored otherwise. It is refused when empty for the same
	// reason DetachUnrecognized refuses an anonymous tool: the name is the whole
	// of what identifies the invocation, and nameless skill work is a fold
	// nobody can say anything about.
	SkillName string
	// Args are the invocation's arguments, verbatim, for DetachSkill. Empty is
	// legitimate and means the call carried none.
	Args string
	// StartedAtMs is when the work was launched, unix millis.
	StartedAtMs int64
}

// mintDetachedWorkID is THE ONE SITE in the daemon that produces a
// detached-work message id.
//
// It is unexported and OpenDetachedWork is its only caller, so "the id is
// resolved once" is a property of the call graph rather than a convention. It
// is DERIVED from the task id rather than random: the same detachment replayed
// out of the store must land on the same message, and a random id would open a
// second one on every resync.
func mintDetachedWorkID(taskID string) string { return "detached-work:" + taskID }

// FeedRowLineage is the lineage of a message that sits DIRECTLY in the feed:
// no parent, and a self-referential root.
//
// EVERY Message the daemon produces goes through this or through
// OpenDetachedWork, so top_level_message_id being never empty is a property of
// the two constructors rather than a rule each of the daemon's construction
// sites has to remember. A reader that had to walk parent pointers to find the
// root would be performing exactly the unbounded traversal the field removes.
func FeedRowLineage(uuid string) *frontendv1.MessageLineage {
	return &frontendv1.MessageLineage{TopLevelMessageId: uuid}
}

// OpenDetachedWork resolves one classified detachment into the MESSAGE that IS
// that work: identity, lineage and opening payload, live from the moment it
// opens.
//
// It REFUSES rather than degrades. A missing task id, a missing originating
// call, an unresolved kind, an unclassified spawn with no tool name, or half a
// lineage each return an error naming what was absent: every one of them would
// otherwise produce a message the contract calls unrepresentable (a blank id, a
// kindless body, an "unknown" the maintainer cannot act on, an empty root), and
// the caller's job on that error is to surface a failure card.
//
// "MISSING ORIGINATING CALL" MEANS MISSING, NOT ABSENT BY DESIGN. Work that no
// tool call spawned is legitimate — the contract says origin_tool_use_id is
// "Empty only for work that no tool call spawned" — and such a caller says so
// with DetachedWorkSpec.NoSpawningCall. What stays refused is the case the fault
// arm was written for: a detachment the daemon believes a call spawned, whose
// call it could not find.
func OpenDetachedWork(spec DetachedWorkSpec) (*frontendv1.Message, error) {
	if spec.TaskID == "" {
		return nil, fmt.Errorf("frontend: detached work refused — the detachment carried no task id to mint an id from, and a blank-id message is unrepresentable")
	}
	if spec.OriginToolUseID == "" && !spec.NoSpawningCall {
		return nil, fmt.Errorf("frontend: detached work refused for task %q — the detachment could not be attributed to any tool call, which is a daemon fault surfaced as a failure card rather than unattributed work", spec.TaskID)
	}
	if spec.OriginToolUseID != "" && spec.NoSpawningCall {
		return nil, fmt.Errorf("frontend: detached work refused for task %q — it claims no tool call spawned it while naming call %q as its origin, and work cannot be both announcement-born and call-spawned", spec.TaskID, spec.OriginToolUseID)
	}
	if spec.Kind == DetachUnresolved {
		return nil, fmt.Errorf("frontend: detached work refused for task %q — nothing in the event stream resolved a kind for it, and kindless work carries no body a renderer can draw", spec.TaskID)
	}
	if spec.Kind == DetachUnrecognized && spec.ToolName == "" {
		return nil, fmt.Errorf("frontend: detached work refused for task %q — an unclassified spawn must name the tool it could not classify, and an anonymous one tells a maintainer nothing", spec.TaskID)
	}
	if spec.Kind == DetachSkill && spec.SkillName == "" {
		return nil, fmt.Errorf("frontend: detached work refused for task %q — a skill invocation must name the skill it invoked, and nameless skill work carries neither a label nor anything a reader could act on", spec.TaskID)
	}
	if spec.Workspace == "" {
		return nil, fmt.Errorf("frontend: detached work refused for task %q — it named no workspace, which is the only routing key a snapshot has; workspace-less work would be delivered to every scoped client", spec.TaskID)
	}
	// HALF A LINEAGE IS NO LONGER REPRESENTABLE. The parent arrives as the
	// message itself, so its root comes with it; the refusals that used to
	// police a parent-without-root and a root-without-parent are now the shape
	// of the field. What a bad parent still needs policing for — an ephemeral
	// one, an unclassified one, one with no root of its own — is checked by
	// NewDurableChild below, which is the single place that rule lives.
	id := mintDetachedWorkID(spec.TaskID)
	w := &frontendv1.DetachedWork{
		Workspace:       spec.Workspace,
		OriginToolUseId: spec.OriginToolUseID,
		Label:           spec.Label,
		StartedAtMs:     spec.StartedAtMs,
		Liveness: &frontendv1.DetachedWorkLiveness{
			State: &frontendv1.DetachedWorkLiveness_Live{Live: &frontendv1.DetachedWorkLive{}},
		},
	}
	// THE ONE PLACE A KIND ARM IS SET. Every update producer below re-reads the
	// arm from the message rather than being told which one to use, so this
	// switch is the only decision about what kind the work is.
	switch spec.Kind {
	case DetachAgent:
		w.Kind = &frontendv1.DetachedWork_Agent{Agent: &frontendv1.DetachedWorkAgent{
			Fold: &frontendv1.DetachedWorkFold{TailCap: StreamItemCap},
		}}
	case DetachShell:
		w.Kind = &frontendv1.DetachedWork_Shell{Shell: &frontendv1.DetachedWorkShell{
			Command: spec.Command,
			Output:  &frontendv1.DetachedWorkOutputSpool{},
		}}
	case DetachWorkflow:
		w.Kind = &frontendv1.DetachedWork_Journal{Journal: &frontendv1.DetachedWorkJournal{
			Fold: &frontendv1.DetachedWorkFold{TailCap: StreamItemCap},
		}}
	case DetachUnrecognized:
		w.Kind = &frontendv1.DetachedWork_Unclassified{Unclassified: &frontendv1.DetachedWorkUnclassified{
			ToolName: spec.ToolName,
			Output:   &frontendv1.DetachedWorkOutputSpool{},
		}}
	case DetachMerge:
		w.Kind = &frontendv1.DetachedWork_Merge{Merge: &frontendv1.DetachedWorkMerge{
			Fold: &frontendv1.DetachedWorkFold{TailCap: StreamItemCap},
		}}
	case DetachSkill:
		// The body opens EMPTY and stays empty until resolution delivers it —
		// detached-work.proto: "Empty until resolution delivers it (see
		// DetachedWorkSkillUpdate.body)". Nothing is guessed at here from the call.
		w.Kind = &frontendv1.DetachedWork_Skill{Skill: &frontendv1.DetachedWorkSkill{
			SkillName: spec.SkillName,
			Args:      spec.Args,
			Fold:      &frontendv1.DetachedWorkFold{TailCap: StreamItemCap},
		}}
	}
	body := &frontendv1.Message{
		Uuid: id,
		TsMs: spec.StartedAtMs,
		// The launch FOLLOWED FROM a user turn, which is what
		// CONVERSATION_SOURCE_USER states (feed.proto ConversationSource);
		// UNSPECIFIED is a malformed frame a receiver must reject.
		Source:  frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Payload: &frontendv1.Message_DetachedWork{DetachedWork: w},
	}
	// LINEAGE AND CLASS, STATED IN ONE ACT. Contained work copies its parent's
	// root down; uncontained work IS a feed row and names itself, which is what
	// keeps a page of ten messages ten bounded things. Both go through the
	// durability constructors rather than writing the lineage here, so the work
	// cannot leave with a class its lineage contradicts — nor, as it used to,
	// with no class at all.
	//
	// IT IS DURABLE EITHER WAY. Detached work is opened over a TaskStarted the
	// store holds, so a record for it exists and a reload serves it again. The
	// contract names this case explicitly: the daemon minting the id is not what
	// decides the class, and "detached-work:" + taskID stays durable.
	if spec.Parent != nil {
		return NewDurableChild(spec.Parent, body)
	}
	return NewDurableFeedRow(body)
}

// DetachedWorkKind reports detached work's resolved kind by reading its arm
// back. It is the same vocabulary OpenDetachedWork set the arm from, so the kind
// survives a round trip through the wire.
func DetachedWorkKind(m *frontendv1.Message) DetachKind {
	switch m.GetDetachedWork().GetKind().(type) {
	case *frontendv1.DetachedWork_Agent:
		return DetachAgent
	case *frontendv1.DetachedWork_Shell:
		return DetachShell
	case *frontendv1.DetachedWork_Journal:
		return DetachWorkflow
	case *frontendv1.DetachedWork_Unclassified:
		return DetachUnrecognized
	case *frontendv1.DetachedWork_Merge:
		return DetachMerge
	case *frontendv1.DetachedWork_Skill:
		return DetachSkill
	default:
		return DetachUnresolved
	}
}

// AppendDetachedEmissions folds new emissions into AGENT work and returns the
// matching incremental update.
//
// The arm is not a parameter: it is selected from the work's own kind, so a
// caller holding shell work — or MERGE work, which advances by its OWN arm (see
// AppendWindowEmissions) — gets an error naming both kinds rather than a coerced
// agent update. The kinds stay distinct on the wire: `agent` and `merge` carry
// the same message precisely so the ARM can name the kind, and an agent update
// addressed to merge work is the mismatch a receiver rejects. The emissions are
// the SAME AgentEmission the top-level feed carries — they come from
// translate.go's curation, not from a second parse — which is the wire-level
// guarantee that a detached agent renders through a frontend's ordinary feed
// renderers.
//
// THE EMISSIONS ARE PAYLOAD, NOT MESSAGES. They accumulate on this one message
// and cost no feed row of their own, which is the whole reason an hour-long
// agent does not page like an hour of conversation.
func AppendDetachedEmissions(m *frontendv1.Message, ems []*frontendv1.AgentEmission, atMs int64) (*frontendv1.DetachedWorkUpdate, error) {
	if m.GetDetachedWork().GetAgent() == nil {
		return nil, kindMismatch(m, DetachAgent)
	}
	fold, err := foldEmissions(m, ems, atMs)
	if err != nil || fold == nil {
		return nil, err
	}
	return &frontendv1.DetachedWorkUpdate{
		MessageId: m.GetUuid(),
		Update: &frontendv1.DetachedWorkUpdate_Agent{Agent: &frontendv1.DetachedWorkAgentUpdate{
			Emissions: ems,
			// RESTATED, not deltaed: a dropped-count that drifts is worse than
			// one that is re-sent, and the fold is small.
			Fold: cloneFold(fold),
		}},
	}, nil
}

// AppendWindowEmissions folds new emissions into WINDOW work — work whose
// membership is the temporal span of the session it owns — and returns the
// matching incremental update, or nil when nothing folded.
//
// IT IS AN UPDATE, NOT A RE-DELIVERY. DetachedWorkUpdate's own comment forbids
// re-sending the whole message ("Never a re-send of the whole message either: an
// agent running for an hour would otherwise re-transmit its entire transcript on
// every new line"), and the contract carries the arm that makes an incremental
// push representable for both window kinds: `merge = 15`, a
// DetachedWorkAgentUpdate because a merge run's emissions arrive precisely as a
// detached agent's do, and `skill = 16`, whose emissions arm carries that same
// DetachedWorkAgentUpdate. So the arm names the kind while the payload has no
// axis to evolve apart on. An earlier shape advanced the window through
// DetachedWorkDelta.opened because no such arm existed yet; it does now, so the
// window advances by APPEND like every other conversation-shaped kind.
//
// THE ARM IS STILL NOT A PARAMETER. It is selected from the work's own kind in
// the switch below, so merge work can only ever receive a merge update and skill
// work a skill update, and work of any other kind is refused by name rather than
// coerced.
//
// The fold itself is the SAME operation a detached agent's is — same cap, same
// drop accounting, same emission vocabulary (foldEmissions) — because
// DetachedWorkMerge and DetachedWorkSkill carry the identical emissions/fold
// pair by design.
func AppendWindowEmissions(m *frontendv1.Message, ems []*frontendv1.AgentEmission, atMs int64) (*frontendv1.DetachedWorkUpdate, error) {
	switch DetachedWorkKind(m) {
	case DetachMerge, DetachSkill:
	default:
		return nil, kindMismatch(m, DetachMerge)
	}
	fold, err := foldEmissions(m, ems, atMs)
	if err != nil || fold == nil {
		return nil, err
	}
	// RESTATED, not deltaed, exactly as an agent update's fold is: a
	// dropped-count that drifts is worse than one that is re-sent.
	appended := &frontendv1.DetachedWorkAgentUpdate{Emissions: ems, Fold: cloneFold(fold)}
	up := &frontendv1.DetachedWorkUpdate{MessageId: m.GetUuid()}
	switch m.GetDetachedWork().GetKind().(type) {
	case *frontendv1.DetachedWork_Merge:
		up.Update = &frontendv1.DetachedWorkUpdate_Merge{Merge: appended}
	case *frontendv1.DetachedWork_Skill:
		up.Update = &frontendv1.DetachedWorkUpdate_Skill{Skill: &frontendv1.DetachedWorkSkillUpdate{
			Update: &frontendv1.DetachedWorkSkillUpdate_Emissions{Emissions: appended},
		}}
	}
	return up, nil
}

// ResolveSkillBody delivers a skill file's resolved contents as the SKILL
// message's own body, and returns the update that carries them.
//
// THE BODY HAS EXACTLY ONE HOME. detached-work.proto puts the skill file's
// contents on DetachedWorkSkill.body and retires their old rendering in the same
// breath — "they are the SKILL's content, not a response of the conversation".
// Writing the field and producing the update happen HERE, in one call, so a
// snapshot and a delta cannot disagree about what the body is.
//
// It replaces the body WHOLE rather than appending, which is what the arm says:
// resolution delivers the file once, and a replayed resolution overwrites with
// the same bytes instead of doubling them.
func ResolveSkillBody(m *frontendv1.Message, contents string) (*frontendv1.DetachedWorkUpdate, error) {
	sb := m.GetDetachedWork().GetSkill()
	if sb == nil {
		return nil, kindMismatch(m, DetachSkill)
	}
	sb.Body = contents
	return &frontendv1.DetachedWorkUpdate{
		MessageId: m.GetUuid(),
		Update: &frontendv1.DetachedWorkUpdate_Skill{Skill: &frontendv1.DetachedWorkSkillUpdate{
			Update: &frontendv1.DetachedWorkSkillUpdate_Body{Body: &frontendv1.DetachedWorkSkillBodyResolved{Contents: contents}},
		}},
	}, nil
}

// foldEmissions is the ONE fold both conversation-shaped kinds go through: it
// appends, applies the tail cap, records the drop and stamps the activity
// clock. It reports a nil fold when there was nothing to append, so a caller
// produces no wire traffic for an empty batch.
func foldEmissions(m *frontendv1.Message, ems []*frontendv1.AgentEmission, atMs int64) (*frontendv1.DetachedWorkFold, error) {
	emissions, fold, err := emissionFold(m)
	if err != nil {
		return nil, err
	}
	if len(ems) == 0 {
		return nil, nil
	}
	setEmissionFold(m, capTail(append(emissions, ems...), fold))
	touchDetachedActivity(m, atMs)
	return fold, nil
}

// emissionFold finds the emission list and fold accounting of
// conversation-shaped work, and refuses work of any other kind. It is the ONE
// lookup AppendDetachedEmissions goes through, exactly as outputSpool is for the
// byte-spool kinds.
func emissionFold(m *frontendv1.Message) ([]*frontendv1.AgentEmission, *frontendv1.DetachedWorkFold, error) {
	switch k := m.GetDetachedWork().GetKind().(type) {
	// The nil checks are the same refusal an absent arm earns, not a
	// convenience: a kind arm present but carrying no body has no emission list
	// to append to, and writing one back would be a nil dereference rather than
	// a fold.
	case *frontendv1.DetachedWork_Agent:
		if k.Agent == nil {
			return nil, nil, kindMismatch(m, DetachAgent)
		}
		return k.Agent.GetEmissions(), k.Agent.GetFold(), nil
	case *frontendv1.DetachedWork_Merge:
		if k.Merge == nil {
			return nil, nil, kindMismatch(m, DetachMerge)
		}
		return k.Merge.GetEmissions(), k.Merge.GetFold(), nil
	case *frontendv1.DetachedWork_Skill:
		if k.Skill == nil {
			return nil, nil, kindMismatch(m, DetachSkill)
		}
		return k.Skill.GetEmissions(), k.Skill.GetFold(), nil
	default:
		return nil, nil, kindMismatch(m, DetachAgent)
	}
}

// setEmissionFold writes the capped emission list back onto whichever
// conversation-shaped arm the work carries. Only emissionFold's own kinds reach
// it, so an unhandled arm here is unreachable rather than silent.
func setEmissionFold(m *frontendv1.Message, ems []*frontendv1.AgentEmission) {
	switch k := m.GetDetachedWork().GetKind().(type) {
	case *frontendv1.DetachedWork_Agent:
		k.Agent.Emissions = ems
	case *frontendv1.DetachedWork_Merge:
		k.Merge.Emissions = ems
	case *frontendv1.DetachedWork_Skill:
		k.Skill.Emissions = ems
	}
}

// AppendDetachedJournalRows folds new journal rows into WORKFLOW work and
// returns the matching incremental update. Rows are append-only and are never
// revised in place: a step that starts running and later completes appends a
// running row and then a done row.
func AppendDetachedJournalRows(m *frontendv1.Message, rows []*frontendv1.DetachedWorkJournalRow, atMs int64) (*frontendv1.DetachedWorkUpdate, error) {
	jb := m.GetDetachedWork().GetJournal()
	if jb == nil {
		return nil, kindMismatch(m, DetachWorkflow)
	}
	if len(rows) == 0 {
		return nil, nil
	}
	jb.Rows = capTail(append(jb.GetRows(), rows...), jb.GetFold())
	touchDetachedActivity(m, atMs)
	return &frontendv1.DetachedWorkUpdate{
		MessageId: m.GetUuid(),
		Update: &frontendv1.DetachedWorkUpdate_Journal{Journal: &frontendv1.DetachedWorkJournalUpdate{
			Rows: rows,
			Fold: cloneFold(jb.GetFold()),
		}},
	}, nil
}

// AppendDetachedOutput folds new bytes onto byte-spool work's spool and returns
// the matching incremental update.
//
// THE BYTES ARE AN UPDATE, NEVER A MESSAGE. They are appended to the payload of
// the message named by MessageId; a shell producing a megabyte changes one
// message many times rather than producing thousands of feed rows.
//
// THE OFFSET IS THE SPOOL'S OWN. from_offset is read off
// DetachedWorkOutputSpool.through_offset, stamped on the append, and the same
// field is advanced — all here, in one function. There is no second cursor
// anywhere in the daemon that could drift from it, so a client's gap check
// compares the daemon's cursor against itself one push earlier.
//
// `shell` and `unclassified` are distinct wire arms carrying the same message;
// which one is produced is decided by the work's kind in the same switch that
// found the spool.
func AppendDetachedOutput(m *frontendv1.Message, text string, atMs int64) (*frontendv1.DetachedWorkUpdate, error) {
	spool, err := outputSpool(m)
	if err != nil {
		return nil, err
	}
	if text == "" {
		return nil, nil
	}
	from := spool.GetThroughOffset()
	spool.Text += text
	spool.ThroughOffset = from + uint64(len(text))
	touchDetachedActivity(m, atMs)
	chunk := &frontendv1.DetachedWorkOutputAppend{Text: text, FromOffset: from}
	up := &frontendv1.DetachedWorkUpdate{MessageId: m.GetUuid()}
	switch m.GetDetachedWork().GetKind().(type) {
	case *frontendv1.DetachedWork_Shell:
		up.Update = &frontendv1.DetachedWorkUpdate_Shell{Shell: chunk}
	case *frontendv1.DetachedWork_Unclassified:
		up.Update = &frontendv1.DetachedWorkUpdate_Unclassified{Unclassified: chunk}
	}
	return up, nil
}

// AppendDetachedOutputThrough folds a CUMULATIVE output snapshot onto byte-spool
// work: the source hands over everything the work has written so far, and the
// new bytes are whatever lies past the spool's own cursor.
//
// It exists because the daemon's evidence for a backgrounded shell is a
// TaskOutputResult retrieval, which restates the whole output rather than a
// delta. Slicing that restatement at the SPOOL'S cursor — inside this function,
// which then hands the suffix to AppendDetachedOutput — is what keeps the single
// cursor single: there is still exactly one number in the daemon that decides
// where the next append starts, and a caller cannot supply a second one.
//
// A snapshot SHORTER than the cursor means the source rewound: the spool was
// truncated, rotated, or replaced by a different task's output. That is a gap,
// and it is refused loudly rather than applied — re-appending from zero would
// silently duplicate everything the client already holds.
func AppendDetachedOutputThrough(m *frontendv1.Message, whole string, atMs int64) (*frontendv1.DetachedWorkUpdate, error) {
	spool, err := outputSpool(m)
	if err != nil {
		return nil, err
	}
	through := spool.GetThroughOffset()
	if uint64(len(whole)) < through {
		return nil, &DetachedGapError{
			MessageID: m.GetUuid(),
			Gap:       DetachedGapSpoolRewind,
			Detail: fmt.Sprintf("frontend: detached work %q output REWOUND — the source restated %d bytes where its spool cursor already stands at %d, which is a gap rather than an append and is refused",
				m.GetUuid(), len(whole), through),
		}
	}
	return AppendDetachedOutput(m, whole[through:], atMs)
}

// DetachedVerdict is one settlement, resolved by the daemon before it is
// anything on the wire.
type DetachedVerdict struct {
	// Status is the shim's typed terminal status. UNSPECIFIED is refused:
	// settled work with no outcome is unrepresentable on the contract, and a
	// substituted stand-in ending is a fabrication nobody could tell from a
	// real one.
	Status protocolv1.TerminalStatus
	// AtMs is when the work finished, unix millis.
	AtMs int64
	// ExitCode is the process exit status, for work that IS a process. Nil for
	// work with no exit status of its own (an agent, a workflow), and that
	// absence is the only reading of "this work did not exit, it concluded".
	ExitCode *int32
	// Message is the failure, resolved for display. Empty when the source
	// reported failure without a reason — never filled with a manufactured one.
	Message string
	// Reason is who or what stopped the work, for a killed outcome. Empty when
	// unattributed.
	Reason string
}

// SettleDetachedWork moves work from live to settled and returns the liveness
// update that says so. Liveness is the one kind-INDEPENDENT update on the
// contract, so this producer takes no kind arm at all.
//
// The outcome is resolved from the EXIT CODE when there is one and from the
// terminal status otherwise. That mapping is the daemon's, never a client's:
// the code stays on the wire beside the verdict so a shell's card can show
// "exited 137" rather than an unexplained red dot.
func SettleDetachedWork(m *frontendv1.Message, v DetachedVerdict) (*frontendv1.DetachedWorkUpdate, error) {
	settled := &frontendv1.DetachedWorkSettled{SettledAtMs: v.AtMs}
	if v.ExitCode != nil {
		settled.ShellExit = &frontendv1.DetachedWorkShellExit{Code: *v.ExitCode}
	}
	switch {
	// A KILL IS A KILL WHATEVER THE CODE SAYS. A stopped process still exits
	// nonzero, and reading that code as "it failed" would report a user's own
	// interrupt back to them as an error.
	case v.Status == protocolv1.TerminalStatus_TERMINAL_STATUS_KILLED ||
		v.Status == protocolv1.TerminalStatus_TERMINAL_STATUS_STOPPED ||
		v.Status == protocolv1.TerminalStatus_TERMINAL_STATUS_LOST:
		settled.Outcome = &frontendv1.DetachedWorkSettled_Killed{Killed: &frontendv1.DetachedWorkOutcomeKilled{Reason: v.Reason}}
	case v.ExitCode != nil:
		if *v.ExitCode == 0 {
			settled.Outcome = &frontendv1.DetachedWorkSettled_Done{Done: &frontendv1.DetachedWorkOutcomeDone{}}
		} else {
			settled.Outcome = &frontendv1.DetachedWorkSettled_Error{Error: &frontendv1.DetachedWorkOutcomeError{Message: v.Message}}
		}
	case v.Status == protocolv1.TerminalStatus_TERMINAL_STATUS_DONE:
		settled.Outcome = &frontendv1.DetachedWorkSettled_Done{Done: &frontendv1.DetachedWorkOutcomeDone{}}
	case v.Status == protocolv1.TerminalStatus_TERMINAL_STATUS_ERROR:
		settled.Outcome = &frontendv1.DetachedWorkSettled_Error{Error: &frontendv1.DetachedWorkOutcomeError{Message: v.Message}}
	default:
		return nil, fmt.Errorf("frontend: detached work %q settlement refused — the terminal status was left unspecified and no exit code resolved one, and settled work with no outcome is unrepresentable", m.GetUuid())
	}
	m.GetDetachedWork().Liveness = &frontendv1.DetachedWorkLiveness{State: &frontendv1.DetachedWorkLiveness_Settled{Settled: settled}}
	return &frontendv1.DetachedWorkUpdate{
		MessageId: m.GetUuid(),
		Update: &frontendv1.DetachedWorkUpdate_Liveness{Liveness: &frontendv1.DetachedWorkLivenessUpdate{
			Liveness: m.GetDetachedWork().GetLiveness(),
		}},
	}, nil
}

// StampSpawnedMessageIDs publishes the daemon's detachment verdict onto the tool
// card, on BOTH messages that carry it.
//
// resolve maps a tool_use id to the id of the MESSAGE that call detached, and
// returns "" for a call that detached nothing — which is the only reading of an
// empty spawned_message_id. It is the SAME resolver behind both stamps, so the
// two fields are the same string by construction rather than by two lookups that
// could disagree; the contract's "the daemon resolves the id once and stamps it
// on both" is that shared closure.
//
// IT STAMPS PROVENANCE, NOT CONTAINMENT. The named message is a feed row in its
// own right; this says which call produced it, and nothing about what contains
// it.
//
// It walks messages rather than being called per emission so that the stamp
// happens at the one curation point, on the way out, for every route — live push
// and replay alike.
func StampSpawnedMessageIDs(items []*frontendv1.Message, resolve func(toolUseID string) string) {
	if resolve == nil {
		return
	}
	for _, it := range items {
		switch em := it.GetAgent().GetEmission().(type) {
		case *frontendv1.AgentEmission_ToolCall:
			if id := resolve(em.ToolCall.GetCall().GetId()); id != "" {
				em.ToolCall.SpawnedMessageId = id
			}
		case *frontendv1.AgentEmission_ToolOutcome:
			if id := resolve(em.ToolOutcome.GetToolUseId()); id != "" {
				em.ToolOutcome.SpawnedMessageId = id
			}
		}
	}
}

// DetachedWorkDeltaFrame wraps a detached-work push in its frame arm.
func DetachedWorkDeltaFrame(d *frontendv1.DetachedWorkDelta) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_DetachedWorkDelta{DetachedWorkDelta: d}}
}

// --- internals -------------------------------------------------------------

// outputSpool finds the byte spool of spool-kinded work, and refuses work of any
// other kind. It is the ONE lookup both append producers go through, so "the
// cursor lives on the spool" holds for every route into it.
func outputSpool(m *frontendv1.Message) (*frontendv1.DetachedWorkOutputSpool, error) {
	var spool *frontendv1.DetachedWorkOutputSpool
	switch k := m.GetDetachedWork().GetKind().(type) {
	case *frontendv1.DetachedWork_Shell:
		spool = k.Shell.GetOutput()
	case *frontendv1.DetachedWork_Unclassified:
		spool = k.Unclassified.GetOutput()
	default:
		return nil, kindMismatch(m, DetachShell)
	}
	if spool == nil {
		return nil, fmt.Errorf("frontend: detached work %q is kind %s but carries no output spool — its cursor is the only source of from_offset and there is nothing to read it from", m.GetUuid(), DetachedWorkKind(m))
	}
	return spool, nil
}

// kindMismatch is the refusal an update producer returns when the message it was
// handed is not the kind that update belongs to. It names BOTH kinds, because
// the useful fact is the disagreement, not either half of it.
func kindMismatch(m *frontendv1.Message, want DetachKind) error {
	return &DetachedGapError{
		MessageID: m.GetUuid(),
		Gap:       DetachedGapKindMismatch,
		Detail: fmt.Sprintf("frontend: detached work %q is kind %s, so a %s update addressed to it is a daemon bug and is rejected rather than coerced",
			m.GetUuid(), DetachedWorkKind(m), want),
	}
}

// capTail applies the tail cap to an item-counted fold, keeping the NEWEST
// entries and accumulating what it dropped onto the fold so the drop is
// reported rather than silent. It also restates tail_cap, so a complete fold is
// distinguishable from one capped at exactly the limit.
func capTail[T any](items []T, fold *frontendv1.DetachedWorkFold) []T {
	if fold == nil {
		return items
	}
	fold.TailCap = StreamItemCap
	if len(items) <= StreamItemCap {
		return items
	}
	drop := len(items) - StreamItemCap
	fold.DroppedBefore += int64(drop)
	return items[drop:]
}

// cloneFold copies the fold accounting onto an update. The update must not
// alias the message's own fold: the message keeps folding after the update is
// queued, and an aliased dropped-count would be rewritten under a frame already
// handed to the outbox.
func cloneFold(f *frontendv1.DetachedWorkFold) *frontendv1.DetachedWorkFold {
	if f == nil {
		return nil
	}
	return &frontendv1.DetachedWorkFold{DroppedBefore: f.GetDroppedBefore(), TailCap: f.GetTailCap()}
}

// touchDetachedActivity records that the work produced something, on the LIVE
// arm only. Settled work has no activity clock, and writing one would be a
// second, contradictory account of an ending the outcome already states.
func touchDetachedActivity(m *frontendv1.Message, atMs int64) {
	if live, ok := m.GetDetachedWork().GetLiveness().GetState().(*frontendv1.DetachedWorkLiveness_Live); ok {
		live.Live.LastActivityMs = atMs
	}
}
