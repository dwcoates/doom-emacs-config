// asyncbubble.go is the daemon's ONE async-bubble apparatus: the single site
// that mints a bubble id, the single constructor that turns a resolved
// classification into a bubble, and the update producers that derive their
// wire arm FROM the bubble's own kind rather than from a caller's claim about
// it.
//
// THE INVARIANTS THIS FILE ENFORCES STRUCTURALLY
//
//   - ONE MINTING SITE. mintBubbleID is unexported and OpenAsyncBubble is its
//     only caller. Nothing outside this file can name it, so no second site can
//     invent an id and no id can be resolved twice with two answers. A caller
//     that needs the id reads it back off the bubble it opened.
//
//   - ARMS MATCH THE KIND BY CONSTRUCTION. Every update producer takes the
//     BUBBLE, finds the kind arm on it, and selects the update arm from that
//     same switch. There is no code path that accepts an arm choice from a
//     caller, so a journal update cannot be addressed to a shell bubble even by
//     mistake — the producer returns an error naming both kinds instead.
//
//   - from_offset COMES FROM THE SPOOL. AppendAsyncOutput reads the cursor off
//     AsyncOutputSpool.through_offset, stamps it on the append, and advances the
//     same field, in one function. The spool IS the cursor; there is no second
//     counter that could drift from it.
//
//   - THE FOLD AND THE PUSH ARE ONE OPERATION. Every producer mutates the
//     folded-to-date bubble (what StateSnapshot.async_bubbles serves) and
//     returns the incremental update (what AsyncBubbleDelta pushes) from the
//     same call. A snapshot and a delta cannot disagree about what the fold
//     contains because neither is produced without the other.
//
//   - THE CAP IS A DAEMON FACT. StreamItemCap is applied here and reported on
//     AsyncFold, so a fold that dropped its oldest entries is distinguishable
//     from a complete one, and two frontends cannot disagree about what the
//     user is being shown.
package frontend

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// StreamItemCap is the tail cap the daemon applies to the item-counted folds
// (an agent's emissions, a workflow's rows). A detached agent can run to
// thousands of emissions and a fold is a glance at what the work is doing, not
// a second feed — so the TAIL is kept and the drop is REPORTED on AsyncFold
// rather than being silent.
const StreamItemCap = 200

// DetachKind is the daemon's RESOLVED classification of one detachment: which
// AsyncBubble kind arm the work is, decided once and then carried. It is a
// daemon-internal vocabulary and never reaches the wire; its only job is to
// make "the kind" a single value that both the bubble's arm and every later
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
func DetachKindFromTaskKind(k corev1.TaskKind) DetachKind {
	switch k {
	case corev1.TaskKind_TASK_KIND_AGENT:
		return DetachAgent
	case corev1.TaskKind_TASK_KIND_SHELL:
		return DetachShell
	case corev1.TaskKind_TASK_KIND_WORKFLOW:
		return DetachWorkflow
	default:
		return DetachUnresolved
	}
}

// BubbleSpec is everything OpenAsyncBubble needs to resolve one detachment into
// a bubble. It carries no id: the id is minted inside, which is what makes the
// minting site single.
type BubbleSpec struct {
	// TaskID is the detachment's identity in the shim's task vocabulary. It is
	// what the bubble id is derived from, so the same detachment resolves to
	// the same bubble across a replay instead of accumulating twins.
	TaskID string
	// Workspace is the workspace the work runs under. Required: it is the only
	// routing key a snapshot has for a bubble, and a bubble that carried none
	// would be delivered to every scoped client — which is the leak the
	// contract's workspace field exists to close.
	Workspace string
	// Kind is the resolved classification. DetachUnresolved is refused.
	Kind DetachKind
	// OriginToolUseID is the tool_use id of the call that detached the work.
	// Required unless NoSpawningCall states there was no such call: a detachment
	// the daemon cannot attribute to a call it believes exists is a daemon fault
	// its caller surfaces as a failure card, never a blank-origin bubble.
	OriginToolUseID string
	// NoSpawningCall states that NO tool call spawned this work, which the
	// contract admits by design (async-bubble.proto origin_tool_use_id: "Empty
	// only for work that no tool call spawned"). It must be set DELIBERATELY, by
	// a caller holding evidence that the launch announcement named no call at
	// all — never as a way to get past a missing attribution.
	//
	// IT IS WHY THE BLANK-ORIGIN REFUSAL BELOW SURVIVES. Without this flag the
	// only way to admit announcement-born work was to drop that refusal, and
	// then every genuinely unattributable detachment — one whose spawning call
	// exists but could not be found — would quietly open a bubble whose origin
	// points nowhere instead of raising the fault it is.
	NoSpawningCall bool
	// ParentBubbleID is the bubble this one was dispatched FROM, empty at the
	// top level. Detached agents dispatch detached agents.
	ParentBubbleID string
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
	// StartedAtMs is when the work was launched, unix millis.
	StartedAtMs int64
}

// mintBubbleID is THE ONE SITE in the daemon that produces an async bubble id.
//
// It is unexported and OpenAsyncBubble is its only caller, so "the id is
// resolved once" is a property of the call graph rather than a convention. It
// is DERIVED from the task id rather than random: the same detachment replayed
// out of the store must land on the same bubble, and a random id would open a
// second bubble on every resync.
func mintBubbleID(taskID string) string { return "bubble:" + taskID }

// OpenAsyncBubble resolves one classified detachment into a bubble, anchored at
// its launch point and live from the moment it opens.
//
// It REFUSES rather than degrades. A missing task id, a missing originating
// call, an unresolved kind, or an unclassified spawn with no tool name each
// return an error naming what was absent: every one of them would otherwise
// produce a bubble the contract calls unrepresentable (a blank id, a kindless
// body, an "unknown" the maintainer cannot act on), and the caller's job on
// that error is to surface a failure card.
//
// "MISSING ORIGINATING CALL" MEANS MISSING, NOT ABSENT BY DESIGN. Work that no
// tool call spawned is legitimate — the contract says origin_tool_use_id is
// "Empty only for work that no tool call spawned" — and such a caller says so
// with BubbleSpec.NoSpawningCall. What stays refused is the case the fault arm
// was written for: a detachment the daemon believes a call spawned, whose call
// it could not find.
func OpenAsyncBubble(spec BubbleSpec) (*frontendv1.AsyncBubble, error) {
	if spec.TaskID == "" {
		return nil, fmt.Errorf("frontend: async bubble refused — the detachment carried no task id to mint an id from, and a blank-id bubble is unrepresentable")
	}
	if spec.OriginToolUseID == "" && !spec.NoSpawningCall {
		return nil, fmt.Errorf("frontend: async bubble refused for task %q — the detachment could not be attributed to any tool call, which is a daemon fault surfaced as a failure card rather than an unattributed bubble", spec.TaskID)
	}
	if spec.OriginToolUseID != "" && spec.NoSpawningCall {
		return nil, fmt.Errorf("frontend: async bubble refused for task %q — it claims no tool call spawned it while naming call %q as its origin, and a bubble cannot be both announcement-born and call-spawned", spec.TaskID, spec.OriginToolUseID)
	}
	if spec.Kind == DetachUnresolved {
		return nil, fmt.Errorf("frontend: async bubble refused for task %q — nothing in the event stream resolved a kind for it, and a kindless bubble carries no body a renderer can draw", spec.TaskID)
	}
	if spec.Kind == DetachUnrecognized && spec.ToolName == "" {
		return nil, fmt.Errorf("frontend: async bubble refused for task %q — an unclassified spawn must name the tool it could not classify, and an anonymous one tells a maintainer nothing", spec.TaskID)
	}
	if spec.Workspace == "" {
		return nil, fmt.Errorf("frontend: async bubble refused for task %q — it named no workspace, which is the only routing key a snapshot has for a bubble; a workspace-less bubble would be delivered to every scoped client", spec.TaskID)
	}
	b := &frontendv1.AsyncBubble{
		Id:              mintBubbleID(spec.TaskID),
		Workspace:       spec.Workspace,
		OriginToolUseId: spec.OriginToolUseID,
		ParentBubbleId:  spec.ParentBubbleID,
		Label:           spec.Label,
		StartedAtMs:     spec.StartedAtMs,
		Liveness: &frontendv1.AsyncLiveness{
			State: &frontendv1.AsyncLiveness_Live{Live: &frontendv1.AsyncLive{}},
		},
	}
	// THE ONE PLACE A KIND ARM IS SET. Every update producer below re-reads the
	// arm from the bubble rather than being told which one to use, so this
	// switch is the only decision about what kind the work is.
	switch spec.Kind {
	case DetachAgent:
		b.Kind = &frontendv1.AsyncBubble_Agent{Agent: &frontendv1.AsyncAgentBubble{
			Fold: &frontendv1.AsyncFold{TailCap: StreamItemCap},
		}}
	case DetachShell:
		b.Kind = &frontendv1.AsyncBubble_Shell{Shell: &frontendv1.AsyncShellBubble{
			Command: spec.Command,
			Output:  &frontendv1.AsyncOutputSpool{},
		}}
	case DetachWorkflow:
		b.Kind = &frontendv1.AsyncBubble_Journal{Journal: &frontendv1.AsyncWorkflowJournal{
			Fold: &frontendv1.AsyncFold{TailCap: StreamItemCap},
		}}
	case DetachUnrecognized:
		b.Kind = &frontendv1.AsyncBubble_Unclassified{Unclassified: &frontendv1.AsyncUnclassifiedBubble{
			ToolName: spec.ToolName,
			Output:   &frontendv1.AsyncOutputSpool{},
		}}
	}
	return b, nil
}

// AsyncBubbleKind reports a bubble's resolved kind by reading its arm back. It
// is the same vocabulary OpenAsyncBubble set the arm from, so a bubble's kind
// survives a round trip through the wire.
func AsyncBubbleKind(b *frontendv1.AsyncBubble) DetachKind {
	switch b.GetKind().(type) {
	case *frontendv1.AsyncBubble_Agent:
		return DetachAgent
	case *frontendv1.AsyncBubble_Shell:
		return DetachShell
	case *frontendv1.AsyncBubble_Journal:
		return DetachWorkflow
	case *frontendv1.AsyncBubble_Unclassified:
		return DetachUnrecognized
	default:
		return DetachUnresolved
	}
}

// AppendAsyncEmissions folds new emissions into an AGENT bubble and returns the
// matching incremental update.
//
// The arm is not a parameter: it is selected from the bubble's own kind, so a
// caller holding a shell bubble gets an error naming both kinds rather than a
// coerced agent update. The emissions are the SAME AgentEmission the top-level
// feed carries — they come from translate.go's curation, not from a second
// parse — which is the wire-level guarantee that a detached agent renders
// through a frontend's ordinary feed renderers.
func AppendAsyncEmissions(b *frontendv1.AsyncBubble, ems []*frontendv1.AgentEmission, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	ab := b.GetAgent()
	if ab == nil {
		return nil, kindMismatch(b, DetachAgent)
	}
	if len(ems) == 0 {
		return nil, nil
	}
	ab.Emissions = capTail(append(ab.GetEmissions(), ems...), ab.GetFold())
	touchAsyncActivity(b, atMs)
	return &frontendv1.AsyncBubbleUpdate{
		BubbleId: b.GetId(),
		Update: &frontendv1.AsyncBubbleUpdate_Agent{Agent: &frontendv1.AsyncAgentUpdate{
			Emissions: ems,
			// RESTATED, not deltaed: a dropped-count that drifts is worse than
			// one that is re-sent, and the fold is small.
			Fold: cloneFold(ab.GetFold()),
		}},
	}, nil
}

// AppendAsyncJournalRows folds new journal rows into a WORKFLOW bubble and
// returns the matching incremental update. Rows are append-only and are never
// revised in place: a step that starts running and later completes appends a
// running row and then a done row.
func AppendAsyncJournalRows(b *frontendv1.AsyncBubble, rows []*frontendv1.AsyncWorkflowJournalRow, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	jb := b.GetJournal()
	if jb == nil {
		return nil, kindMismatch(b, DetachWorkflow)
	}
	if len(rows) == 0 {
		return nil, nil
	}
	jb.Rows = capTail(append(jb.GetRows(), rows...), jb.GetFold())
	touchAsyncActivity(b, atMs)
	return &frontendv1.AsyncBubbleUpdate{
		BubbleId: b.GetId(),
		Update: &frontendv1.AsyncBubbleUpdate_Journal{Journal: &frontendv1.AsyncWorkflowJournalUpdate{
			Rows: rows,
			Fold: cloneFold(jb.GetFold()),
		}},
	}, nil
}

// AppendAsyncOutput folds new bytes onto a byte-spool bubble's spool and returns
// the matching incremental update.
//
// THE OFFSET IS THE SPOOL'S OWN. from_offset is read off
// AsyncOutputSpool.through_offset, stamped on the append, and the same field is
// advanced — all here, in one function. There is no second cursor anywhere in
// the daemon that could drift from it, so a client's gap check compares the
// daemon's cursor against itself one push earlier.
//
// `shell` and `unclassified` are distinct wire arms carrying the same message;
// which one is produced is decided by the bubble's kind in the same switch that
// found the spool.
func AppendAsyncOutput(b *frontendv1.AsyncBubble, text string, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	spool, err := outputSpool(b)
	if err != nil {
		return nil, err
	}
	if text == "" {
		return nil, nil
	}
	from := spool.GetThroughOffset()
	spool.Text += text
	spool.ThroughOffset = from + uint64(len(text))
	touchAsyncActivity(b, atMs)
	chunk := &frontendv1.AsyncOutputAppend{Text: text, FromOffset: from}
	up := &frontendv1.AsyncBubbleUpdate{BubbleId: b.GetId()}
	switch b.GetKind().(type) {
	case *frontendv1.AsyncBubble_Shell:
		up.Update = &frontendv1.AsyncBubbleUpdate_Shell{Shell: chunk}
	case *frontendv1.AsyncBubble_Unclassified:
		up.Update = &frontendv1.AsyncBubbleUpdate_Unclassified{Unclassified: chunk}
	}
	return up, nil
}

// AppendAsyncOutputThrough folds a CUMULATIVE output snapshot onto a byte-spool
// bubble: the source hands over everything the work has written so far, and the
// new bytes are whatever lies past the spool's own cursor.
//
// It exists because the daemon's evidence for a backgrounded shell is a
// TaskOutputResult retrieval, which restates the whole output rather than a
// delta. Slicing that restatement at the SPOOL'S cursor — inside this function,
// which then hands the suffix to AppendAsyncOutput — is what keeps the single
// cursor single: there is still exactly one number in the daemon that decides
// where the next append starts, and a caller cannot supply a second one.
//
// A snapshot SHORTER than the cursor means the source rewound: the spool was
// truncated, rotated, or replaced by a different task's output. That is a gap,
// and it is refused loudly rather than applied — re-appending from zero would
// silently duplicate everything the client already holds.
func AppendAsyncOutputThrough(b *frontendv1.AsyncBubble, whole string, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	spool, err := outputSpool(b)
	if err != nil {
		return nil, err
	}
	through := spool.GetThroughOffset()
	if uint64(len(whole)) < through {
		return nil, &AsyncGapError{
			BubbleID: b.GetId(),
			Gap:      AsyncGapSpoolRewind,
			Detail: fmt.Sprintf("frontend: async bubble %q output REWOUND — the source restated %d bytes where its spool cursor already stands at %d, which is a gap rather than an append and is refused",
				b.GetId(), len(whole), through),
		}
	}
	return AppendAsyncOutput(b, whole[through:], atMs)
}

// AsyncVerdict is one settlement, resolved by the daemon before it is anything
// on the wire.
type AsyncVerdict struct {
	// Status is the shim's typed terminal status. UNSPECIFIED is refused: a
	// settled bubble with no outcome is unrepresentable on the contract, and a
	// substituted stand-in ending is a fabrication nobody could tell from a
	// real one.
	Status corev1.TerminalStatus
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

// SettleAsyncBubble moves a bubble from live to settled and returns the liveness
// update that says so. Liveness is the one kind-INDEPENDENT update on the
// contract, so this producer takes no kind arm at all.
//
// The outcome is resolved from the EXIT CODE when there is one and from the
// terminal status otherwise. That mapping is the daemon's, never a client's:
// the code stays on the wire beside the verdict so a shell's card can show
// "exited 137" rather than an unexplained red dot.
func SettleAsyncBubble(b *frontendv1.AsyncBubble, v AsyncVerdict) (*frontendv1.AsyncBubbleUpdate, error) {
	settled := &frontendv1.AsyncSettled{SettledAtMs: v.AtMs}
	if v.ExitCode != nil {
		settled.ShellExit = &frontendv1.AsyncShellExit{Code: *v.ExitCode}
	}
	switch {
	// A KILL IS A KILL WHATEVER THE CODE SAYS. A stopped process still exits
	// nonzero, and reading that code as "it failed" would report a user's own
	// interrupt back to them as an error.
	case v.Status == corev1.TerminalStatus_TERMINAL_STATUS_KILLED ||
		v.Status == corev1.TerminalStatus_TERMINAL_STATUS_STOPPED ||
		v.Status == corev1.TerminalStatus_TERMINAL_STATUS_LOST:
		settled.Outcome = &frontendv1.AsyncSettled_Killed{Killed: &frontendv1.AsyncOutcomeKilled{Reason: v.Reason}}
	case v.ExitCode != nil:
		if *v.ExitCode == 0 {
			settled.Outcome = &frontendv1.AsyncSettled_Done{Done: &frontendv1.AsyncOutcomeDone{}}
		} else {
			settled.Outcome = &frontendv1.AsyncSettled_Error{Error: &frontendv1.AsyncOutcomeError{Message: v.Message}}
		}
	case v.Status == corev1.TerminalStatus_TERMINAL_STATUS_DONE:
		settled.Outcome = &frontendv1.AsyncSettled_Done{Done: &frontendv1.AsyncOutcomeDone{}}
	case v.Status == corev1.TerminalStatus_TERMINAL_STATUS_ERROR:
		settled.Outcome = &frontendv1.AsyncSettled_Error{Error: &frontendv1.AsyncOutcomeError{Message: v.Message}}
	default:
		return nil, fmt.Errorf("frontend: async bubble %q settlement refused — the terminal status was left unspecified and no exit code resolved one, and a settled bubble with no outcome is unrepresentable", b.GetId())
	}
	b.Liveness = &frontendv1.AsyncLiveness{State: &frontendv1.AsyncLiveness_Settled{Settled: settled}}
	return &frontendv1.AsyncBubbleUpdate{
		BubbleId: b.GetId(),
		Update: &frontendv1.AsyncBubbleUpdate_Liveness{Liveness: &frontendv1.AsyncLivenessUpdate{
			Liveness: b.GetLiveness(),
		}},
	}, nil
}

// StampSpawnedBubbleIDs publishes the daemon's classification verdict onto the
// tool card, on BOTH messages that carry it.
//
// resolve maps a tool_use id to the bubble that call detached, and returns ""
// for a call that detached nothing — which is the only reading of an empty
// spawned_bubble_id. It is the SAME resolver behind both stamps, so the two
// fields are the same string by construction rather than by two lookups that
// could disagree; the contract's "the daemon resolves the id once and stamps it
// on both" is that shared closure.
//
// It walks conversation items rather than being called per emission so that the
// stamp happens at the one curation point, on the way out, for every route —
// live push and replay alike.
func StampSpawnedBubbleIDs(items []*frontendv1.ConversationItem, resolve func(toolUseID string) string) {
	if resolve == nil {
		return
	}
	for _, it := range items {
		switch em := it.GetAgent().GetEmission().(type) {
		case *frontendv1.AgentEmission_ToolCall:
			if id := resolve(em.ToolCall.GetCall().GetId()); id != "" {
				em.ToolCall.SpawnedBubbleId = id
			}
		case *frontendv1.AgentEmission_ToolOutcome:
			if id := resolve(em.ToolOutcome.GetToolUseId()); id != "" {
				em.ToolOutcome.SpawnedBubbleId = id
			}
		}
	}
}

// AsyncBubbleDeltaFrame wraps an async push in its frame arm.
func AsyncBubbleDeltaFrame(d *frontendv1.AsyncBubbleDelta) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_AsyncBubbleDelta{AsyncBubbleDelta: d}}
}

// --- internals -------------------------------------------------------------

// outputSpool finds the byte spool of a spool-kinded bubble, and refuses a
// bubble of any other kind. It is the ONE lookup both append producers go
// through, so "the cursor lives on the spool" holds for every route into it.
func outputSpool(b *frontendv1.AsyncBubble) (*frontendv1.AsyncOutputSpool, error) {
	var spool *frontendv1.AsyncOutputSpool
	switch k := b.GetKind().(type) {
	case *frontendv1.AsyncBubble_Shell:
		spool = k.Shell.GetOutput()
	case *frontendv1.AsyncBubble_Unclassified:
		spool = k.Unclassified.GetOutput()
	default:
		return nil, kindMismatch(b, DetachShell)
	}
	if spool == nil {
		return nil, fmt.Errorf("frontend: async bubble %q is kind %s but carries no output spool — its cursor is the only source of from_offset and there is nothing to read it from", b.GetId(), AsyncBubbleKind(b))
	}
	return spool, nil
}

// kindMismatch is the refusal an update producer returns when the bubble it was
// handed is not the kind that update belongs to. It names BOTH kinds, because
// the useful fact is the disagreement, not either half of it.
func kindMismatch(b *frontendv1.AsyncBubble, want DetachKind) error {
	return &AsyncGapError{
		BubbleID: b.GetId(),
		Gap:      AsyncGapKindMismatch,
		Detail: fmt.Sprintf("frontend: async bubble %q is kind %s, so a %s update addressed to it is a daemon bug and is rejected rather than coerced",
			b.GetId(), AsyncBubbleKind(b), want),
	}
}

// capTail applies the tail cap to an item-counted fold, keeping the NEWEST
// entries and accumulating what it dropped onto the fold so the drop is
// reported rather than silent. It also restates tail_cap, so a complete fold is
// distinguishable from one capped at exactly the limit.
func capTail[T any](items []T, fold *frontendv1.AsyncFold) []T {
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
// alias the bubble's own fold: the bubble keeps folding after the update is
// queued, and an aliased dropped-count would be rewritten under a frame already
// handed to the outbox.
func cloneFold(f *frontendv1.AsyncFold) *frontendv1.AsyncFold {
	if f == nil {
		return nil
	}
	return &frontendv1.AsyncFold{DroppedBefore: f.GetDroppedBefore(), TailCap: f.GetTailCap()}
}

// touchAsyncActivity records that the work produced something, on the LIVE arm
// only. A settled bubble has no activity clock, and writing one would be a
// second, contradictory account of an ending the outcome already states.
func touchAsyncActivity(b *frontendv1.AsyncBubble, atMs int64) {
	if live, ok := b.GetLiveness().GetState().(*frontendv1.AsyncLiveness_Live); ok {
		live.Live.LastActivityMs = atMs
	}
}
