// asyncbubbles.go is one session's detached-work apparatus: the store of every
// bubble it has opened, the classification that opens them, and the routing
// that folds each work kind's output into the right one.
//
// WHAT LIVES HERE AND WHAT DOES NOT. Every construction decision — minting an
// id, choosing a kind arm, choosing an update arm, advancing a spool cursor,
// applying the tail cap, resolving a settlement outcome — lives in
// internal/frontend's async-bubble apparatus, which this file only calls. What
// lives here is the SESSION's part: which detachments exist, which call each
// one came from, which bubble a record belongs to, and when a push goes out.
//
// THE FEED-VERSUS-BUBBLE SPLIT IS NOT HERE EITHER. It is decided once, in
// frontend.CurateEvent, and this file consumes its verdict. That is deliberate:
// this store is per-consumer and there are several consumers per session (the
// live one, the durable-replay one), so a split decided here would be a rule
// with as many copies as there are consumers.
package sessioncontroller

import (
	"fmt"
	"sort"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
)

// asyncBubbleStore holds one session's open bubbles and the indexes that route
// evidence to them.
//
// The indexes are all secondary keys onto byID. There is exactly one bubble
// object per detachment, and every route — a sidechain record, a task
// lifecycle event, a tool outcome, the reconnect snapshot — reaches the SAME
// object, which is why a snapshot and a delta cannot describe different folds.
type asyncBubbleStore struct {
	// workspace is stamped on every bubble this store opens. It is held here
	// rather than passed per call so that a bubble and the AsyncBubbleDelta
	// that carries it cannot name different workspaces: both read this one
	// value, which the consumer set once.
	workspace string
	mu        sync.Mutex
	byID      map[string]*frontendv1.AsyncBubble
	// order preserves launch order, so a snapshot lists bubbles as they opened
	// rather than in map order.
	order []string
	// idByToolUse routes by the launching call's tool_use id — the primary
	// handle, because it is the same id the tool card carries.
	idByToolUse map[string]string
	// idByTask routes by the shim's task id and by a detached agent's agent id,
	// which is what a sidechain record names when its envelope carries no
	// source call.
	idByTask map[string]string
	// parentByToolUse names, for a call MADE INSIDE a bubble, the bubble it was
	// made in. A detachment launched by such a call is a nested dispatch and
	// takes that bubble as its parent.
	parentByToolUse map[string]string
	// toolNames maps a tool_use id to the tool the agent named. It is the only
	// source for AsyncUnclassifiedBubble.tool_name.
	toolNames map[string]string
	// journalThrough is a workflow bubble's byte cursor over its journal text.
	// The contract gives a journal no output spool — it carries rows — so this
	// cursor has no wire home and lives here, still exactly one number per
	// bubble with exactly one owner.
	journalThrough map[string]uint64
}

func newAsyncBubbleStore(workspace string) *asyncBubbleStore {
	return &asyncBubbleStore{
		workspace:       workspace,
		byID:            map[string]*frontendv1.AsyncBubble{},
		idByToolUse:     map[string]string{},
		idByTask:        map[string]string{},
		parentByToolUse: map[string]string{},
		toolNames:       map[string]string{},
		journalThrough:  map[string]uint64{},
	}
}

// asyncPush is everything one event produced for the async plane: bubbles that
// opened, updates to bubbles already open, and the daemon faults that could not
// be turned into either.
type asyncPush struct {
	// Opened are bubbles opening for the first time.
	Opened []*frontendv1.AsyncBubble
	// Updates are incremental pushes to bubbles already open, in order.
	Updates []*frontendv1.AsyncBubbleUpdate
	// Faults are detachments the daemon could not attribute or classify. They
	// are FAILURE CARDS, not bubbles: the contract says a detachment the daemon
	// cannot attribute to a tool call is a daemon fault, never a bubble with a
	// blank id, and this is where that ruling is honoured.
	Faults []asyncFault
}

// asyncFault is one detachment that produced a failure card instead of a
// bubble.
type asyncFault struct {
	// UUID is the card's conversation address, derived from the detachment so
	// the card is stable across a resync rather than accumulating twins.
	UUID string
	// Card is the classified failure.
	Card *frontendv1.FailureCardView
	// Detail is the record the caller logs. It is the same sentence the card's
	// detail carries, so the log and the screen cannot disagree.
	Detail string
}

func (p *asyncPush) empty() bool {
	return len(p.Opened) == 0 && len(p.Updates) == 0 && len(p.Faults) == 0
}

// spawnedBubbleID answers "did this call detach work, and which bubble is it".
//
// It is the resolver behind frontend.StampSpawnedBubbleIDs, and therefore the
// ONE lookup behind BOTH AgentToolCall.spawned_bubble_id and
// AgentToolOutcome.spawned_bubble_id. The two fields are the same string
// because they are the same call's answer, not because two sites agreed to
// write the same thing.
//
// An empty result means the call detached nothing, which is the only reading of
// an empty spawned_bubble_id.
func (s *asyncBubbleStore) spawnedBubbleID(toolUseID string) string {
	if toolUseID == "" {
		return ""
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.idByToolUse[toolUseID]
}

// snapshot returns every bubble the session holds, folded to date, in launch
// order.
//
// The bubbles are returned by POINTER and are the store's own objects: they are
// the same values the deltas were produced from, which is precisely why a
// reconnecting client's snapshot and the pushes it then receives describe one
// fold rather than two.
func (s *asyncBubbleStore) snapshot() []*frontendv1.AsyncBubble {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]*frontendv1.AsyncBubble, 0, len(s.order))
	for _, id := range s.order {
		if b := s.byID[id]; b != nil {
			out = append(out, b)
		}
	}
	return out
}

// observeCuration folds one curated event's detached content into its bubbles.
//
// The Curation it takes has ALREADY had the feed-versus-bubble question
// answered (frontend.CurateEvent). This method never revisits it: it receives
// content that is detached by construction and only has to decide WHICH bubble
// each piece belongs to.
func (s *asyncBubbleStore) observeCuration(c frontend.Curation, atMs int64) (asyncPush, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	for id, name := range c.ToolNames {
		s.toolNames[id] = name
	}
	var push asyncPush
	var errs []error
	for _, fold := range c.Detached {
		b, opened, err := s.resolveAgentBubbleLocked(fold, atMs)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if opened {
			push.Opened = append(push.Opened, b)
		}
		// A CALL A DETACHED AGENT MAKES IS A CALL MADE INSIDE THIS BUBBLE. Its
		// own detachment, if it has one, is a nested dispatch, and this is the
		// record that lets the child be given a parent pointer rather than
		// being hung at the top level.
		s.indexCallsLocked(fold.Emissions, b.GetId())
		up, err := frontend.AppendAsyncEmissions(b, fold.Emissions, atMs)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if up != nil {
			push.Updates = append(push.Updates, up)
		}
	}
	for _, outcome := range c.Outcomes {
		opened, updates, fault, err := s.observeOutcomeLocked(outcome, atMs)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if fault != nil {
			push.Faults = append(push.Faults, *fault)
		}
		if opened != nil {
			push.Opened = append(push.Opened, opened)
		}
		push.Updates = append(push.Updates, updates...)
	}
	return push, joinAsyncErrors(errs)
}

// resolveAgentBubbleLocked finds the bubble a detached agent's records belong
// to, opening it if this is the first evidence of the detachment.
//
// OPENING HERE IS NOT A SECOND MINTING SITE. It calls the same
// frontend.OpenAsyncBubble every other route calls, and the id is minted inside
// that function as always. What this handles is ORDER: a subagent's first
// transcript record can reach the daemon before the task-lifecycle event that
// announces the launch, and refusing to fold it until the announcement arrives
// would silently lose the beginning of every detached conversation.
//
// The lookup is by the LAUNCHING CALL first. That is what makes an out-of-order
// open safe: when the announcement does arrive it names the same tool_use id,
// finds this bubble, and enriches it instead of opening a twin.
func (s *asyncBubbleStore) resolveAgentBubbleLocked(fold frontend.DetachedFold, atMs int64) (*frontendv1.AsyncBubble, bool, error) {
	if b := s.lookupLocked(fold.SourceToolUseID, fold.AgentID); b != nil {
		return b, false, nil
	}
	if fold.SourceToolUseID == "" {
		return nil, false, fmt.Errorf("session-controller: async fold REFUSED for agent_id=%q — the record names neither a source tool call nor any bubble already open, so there is nothing to attribute the detachment to and a blank-origin bubble is unrepresentable", fold.AgentID)
	}
	taskID := fold.AgentID
	if taskID == "" {
		taskID = fold.SourceToolUseID
	}
	b, err := frontend.OpenAsyncBubble(frontend.BubbleSpec{
		TaskID:          taskID,
		Workspace:       s.workspace,
		Kind:            frontend.DetachAgent,
		OriginToolUseID: fold.SourceToolUseID,
		ParentBubbleID:  s.parentByToolUse[fold.SourceToolUseID],
		Label:           s.toolNames[fold.SourceToolUseID],
		StartedAtMs:     atMs,
	})
	if err != nil {
		return nil, false, err
	}
	s.adoptLocked(b, fold.SourceToolUseID, taskID, fold.AgentID)
	return b, true, nil
}

// observeOutcomeLocked reads one typed tool outcome for detachment evidence.
//
// EVERY ARM IT HANDLES IS A LAUNCH OR A RETRIEVAL, and each is handled by the
// fact that identifies it rather than by the tool's name: a BashResult carrying
// a background_task_id IS a background launch, an AgentAsyncLaunch IS an async
// agent, a WorkflowLaunchResult IS a workflow run. An arm this switch does not
// name detached nothing, and that is not a fallback — it is the absence of
// launch evidence.
func (s *asyncBubbleStore) observeOutcomeLocked(o frontend.ToolOutcome, atMs int64) (*frontendv1.AsyncBubble, []*frontendv1.AsyncBubbleUpdate, *asyncFault, error) {
	switch r := o.Result.GetResult().(type) {
	case *datav1.ToolUseResult_Bash:
		if r.Bash.GetBackgroundTaskId() == "" {
			return nil, nil, nil, nil // a foreground shell detached nothing
		}
		return s.openFromOutcomeLocked(o, frontend.BubbleSpec{
			TaskID:      r.Bash.GetBackgroundTaskId(),
			Kind:        frontend.DetachShell,
			Command:     r.Bash.GetBackgroundCwdHint(),
			Label:       s.toolNames[o.ToolUseID],
			StartedAtMs: atMs,
		})
	case *datav1.ToolUseResult_AgentAsyncLaunch:
		return s.openFromOutcomeLocked(o, frontend.BubbleSpec{
			TaskID:      r.AgentAsyncLaunch.GetAgentId(),
			Kind:        frontend.DetachAgent,
			Label:       r.AgentAsyncLaunch.GetDescription(),
			StartedAtMs: atMs,
		})
	case *datav1.ToolUseResult_WorkflowLaunch:
		return s.openFromOutcomeLocked(o, frontend.BubbleSpec{
			TaskID:      r.WorkflowLaunch.GetTaskId(),
			Kind:        frontend.DetachWorkflow,
			Label:       r.WorkflowLaunch.GetWorkflowName(),
			StartedAtMs: atMs,
		})
	case *datav1.ToolUseResult_TaskOutput:
		updates, err := s.foldRetrievalLocked(r.TaskOutput, atMs)
		return nil, updates, nil, err
	case *datav1.ToolUseResult_TaskStop:
		up, err := s.settleByTaskLocked(r.TaskStop.GetTaskId(), frontend.AsyncVerdict{
			Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED,
			AtMs:   atMs,
			Reason: r.TaskStop.GetMessage(),
		})
		if up == nil {
			return nil, nil, nil, err
		}
		return nil, []*frontendv1.AsyncBubbleUpdate{up}, nil, err
	default:
		return nil, nil, nil, nil
	}
}

// openFromOutcomeLocked opens the bubble one launch outcome announced, or
// returns the fault that stopped it.
//
// The originating call is the outcome's own tool_use id, which is why a launch
// with no correlated call becomes a FAILURE CARD here: the contract's ruling is
// that an unattributable detachment is a daemon fault, and the card is how the
// user learns that work is running which the daemon cannot show them.
func (s *asyncBubbleStore) openFromOutcomeLocked(o frontend.ToolOutcome, spec frontend.BubbleSpec) (*frontendv1.AsyncBubble, []*frontendv1.AsyncBubbleUpdate, *asyncFault, error) {
	if b := s.lookupLocked(o.ToolUseID, spec.TaskID); b != nil {
		// Already opened — by an out-of-order sidechain record, or by a replay
		// of this same outcome. Enrich rather than mint a twin: a label the
		// launch names is better than the tool name the record guessed at.
		if b.GetLabel() == "" && spec.Label != "" {
			b.Label = spec.Label
		}
		s.idByTask[spec.TaskID] = b.GetId()
		return nil, nil, nil, nil
	}
	spec.OriginToolUseID = o.ToolUseID
	spec.Workspace = s.workspace
	if o.FromDetachedAgent {
		if parent := s.lookupLocked(o.SourceToolUseID, o.AgentID); parent != nil {
			spec.ParentBubbleID = parent.GetId()
		}
	} else if parent := s.parentByToolUse[o.ToolUseID]; parent != "" {
		spec.ParentBubbleID = parent
	}
	// AN UNRECOGNIZED TOOL IS THE EXPLICIT unclassified ARM, not a fault and
	// not a silent shell. The launch evidence says work detached; the tool
	// naming it is simply one the daemon has no classification for, and the
	// contract has an arm that says exactly that and carries the name.
	if spec.Kind == frontend.DetachUnresolved {
		name := s.toolNames[o.ToolUseID]
		if name == "" {
			return nil, nil, s.faultLocked(spec.TaskID, fmt.Sprintf("a tool outcome announced detached work for task %q on call %q, but nothing in the conversation named the tool that launched it, so the work can be neither classified nor honestly reported as unclassified", spec.TaskID, o.ToolUseID)), nil
		}
		spec.Kind = frontend.DetachUnrecognized
		spec.ToolName = name
	}
	b, err := frontend.OpenAsyncBubble(spec)
	if err != nil {
		return nil, nil, s.faultLocked(spec.TaskID, err.Error()), nil
	}
	s.adoptLocked(b, o.ToolUseID, spec.TaskID, "")
	return b, nil, nil, nil
}

// foldRetrievalLocked folds a task-output retrieval into its bubble.
//
// THE FOLD IS CHOSEN BY THE BUBBLE'S KIND, never by the retrieval's arm. The
// retrieval says what was read; the bubble says what the work IS, and what the
// work is decides how its output is modeled. That is what keeps a workflow's
// journal from being folded as a byte spool because it happened to arrive
// through the same retrieval shape.
func (s *asyncBubbleStore) foldRetrievalLocked(out *datav1.TaskOutputResult, atMs int64) ([]*frontendv1.AsyncBubbleUpdate, error) {
	taskID, text, verdict := retrievalFacts(out)
	if taskID == "" {
		return nil, nil
	}
	b := s.lookupLocked("", taskID)
	if b == nil {
		// A retrieval for work no launch announced. Nothing is invented from
		// it: a bubble opened here would have no originating call and therefore
		// no blank-free id, which is the case the contract routes to a fault
		// rather than to a bubble. The retrieval is simply not evidence of a
		// launch.
		return nil, nil
	}
	var updates []*frontendv1.AsyncBubbleUpdate
	switch frontend.AsyncBubbleKind(b) {
	case frontend.DetachShell, frontend.DetachUnrecognized:
		up, err := frontend.AppendAsyncOutputThrough(b, text, atMs)
		if err != nil {
			return nil, err
		}
		if up != nil {
			updates = append(updates, up)
		}
	case frontend.DetachWorkflow:
		up, err := s.foldJournalLocked(b, text, atMs)
		if err != nil {
			return nil, err
		}
		if up != nil {
			updates = append(updates, up)
		}
	case frontend.DetachAgent:
		// A detached agent folds from its SIDECHAIN TRANSCRIPT, which is the
		// structured, durable linkage the store already carries, and never from
		// a retrieval's flattened text. Two sources for one fold would
		// interleave the same conversation with itself.
	}
	if verdict != nil {
		up, err := frontend.SettleAsyncBubble(b, *verdict)
		if err != nil {
			return updates, err
		}
		updates = append(updates, up)
	}
	return updates, nil
}

// foldJournalLocked folds the newly-arrived tail of a workflow's journal into
// rows.
//
// The cursor is this store's journalThrough — one number per bubble, advanced
// only here — for the same reason a spool's cursor lives on the spool: the
// slice point and the advance must be one operation or a restated retrieval
// duplicates rows.
func (s *asyncBubbleStore) foldJournalLocked(b *frontendv1.AsyncBubble, text string, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	through := s.journalThrough[b.GetId()]
	if uint64(len(text)) < through {
		return nil, fmt.Errorf("session-controller: workflow journal for bubble %q REWOUND — the retrieval restated %d bytes where the fold already stands at %d, which is a gap rather than an append and is refused", b.GetId(), len(text), through)
	}
	tail := text[through:]
	if tail == "" {
		return nil, nil
	}
	// The cursor advances by what was CONSUMED, and a trailing partial record
	// is not consumed: it is re-read whole on the next retrieval. Advancing
	// past it would drop the step it describes.
	consumed := completeJournalPrefix(tail)
	rows, _ := frontend.ParseJournalRows(tail[:consumed])
	s.journalThrough[b.GetId()] = through + uint64(consumed)
	return frontend.AppendAsyncJournalRows(b, rows, atMs)
}

// observeTaskEnded settles the bubble a finished detachment belongs to.
func (s *asyncBubbleStore) observeTaskEnded(te *corev1.TaskEnded, atMs int64) (asyncPush, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	up, err := s.settleByTaskLocked(te.GetTaskId(), frontend.AsyncVerdict{
		Status:  te.GetStatus(),
		AtMs:    atMs,
		Message: te.GetSummary(),
		Reason:  te.GetInference(),
	})
	if up == nil {
		return asyncPush{}, err
	}
	return asyncPush{Updates: []*frontendv1.AsyncBubbleUpdate{up}}, err
}

// observeTaskStarted enriches the bubble a launch announcement names, and
// reports the fault when the announcement can be attributed to no call.
//
// It does not open bubbles for kinds it recognizes but has no launch outcome
// for: the outcome plane is where a launch's identity is complete (its task id,
// its label, its originating call), and opening from both planes would be two
// sites deciding the same thing.
func (s *asyncBubbleStore) observeTaskStarted(ts *corev1.TaskStarted, atMs int64) (asyncPush, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if ts.GetToolUseId() == "" {
		return asyncPush{Faults: []asyncFault{*s.faultLocked(ts.GetTaskId(),
			fmt.Sprintf("task %q started as detached work but the announcement named no tool call, so the detachment can be attributed to nothing on screen; the contract routes this to a failure card rather than to a bubble with a blank origin", ts.GetTaskId()))}}, nil
	}
	if b := s.lookupLocked(ts.GetToolUseId(), ts.GetTaskId()); b != nil {
		if b.GetLabel() == "" {
			b.Label = ts.GetDescription()
		}
		s.idByTask[ts.GetTaskId()] = b.GetId()
		return asyncPush{}, nil
	}
	spec := frontend.BubbleSpec{
		TaskID:          ts.GetTaskId(),
		Workspace:       s.workspace,
		Kind:            frontend.DetachKindFromTaskKind(ts.GetKind()),
		OriginToolUseID: ts.GetToolUseId(),
		ParentBubbleID:  s.parentByToolUse[ts.GetToolUseId()],
		Label:           ts.GetDescription(),
		StartedAtMs:     atMs,
	}
	if spec.Kind == frontend.DetachUnresolved {
		name := s.toolNames[ts.GetToolUseId()]
		if name == "" {
			return asyncPush{Faults: []asyncFault{*s.faultLocked(ts.GetTaskId(),
				fmt.Sprintf("task %q started on call %q with no kind the daemon recognizes, and nothing in the conversation named the tool either, so the work can be neither classified nor honestly reported as unclassified", ts.GetTaskId(), ts.GetToolUseId()))}}, nil
		}
		spec.Kind = frontend.DetachUnrecognized
		spec.ToolName = name
	}
	b, err := frontend.OpenAsyncBubble(spec)
	if err != nil {
		return asyncPush{Faults: []asyncFault{*s.faultLocked(ts.GetTaskId(), err.Error())}}, nil
	}
	s.adoptLocked(b, ts.GetToolUseId(), ts.GetTaskId(), "")
	return asyncPush{Opened: []*frontendv1.AsyncBubble{b}}, nil
}

// --- store internals -------------------------------------------------------

// lookupLocked resolves a bubble by either handle, the launching call first.
func (s *asyncBubbleStore) lookupLocked(toolUseID string, taskIDs ...string) *frontendv1.AsyncBubble {
	if toolUseID != "" {
		if id := s.idByToolUse[toolUseID]; id != "" {
			return s.byID[id]
		}
	}
	for _, taskID := range taskIDs {
		if taskID == "" {
			continue
		}
		if id := s.idByTask[taskID]; id != "" {
			return s.byID[id]
		}
	}
	return nil
}

// adoptLocked files a freshly opened bubble under every handle it can be
// reached by.
func (s *asyncBubbleStore) adoptLocked(b *frontendv1.AsyncBubble, toolUseID string, taskIDs ...string) {
	s.byID[b.GetId()] = b
	s.order = append(s.order, b.GetId())
	if toolUseID != "" {
		s.idByToolUse[toolUseID] = b.GetId()
	}
	for _, taskID := range taskIDs {
		if taskID != "" {
			s.idByTask[taskID] = b.GetId()
		}
	}
}

// settleByTaskLocked settles the bubble a task id names, and reports nothing
// for a task that opened no bubble — a task the session tracks in its catalog
// but never detached work for is not a missing bubble.
func (s *asyncBubbleStore) settleByTaskLocked(taskID string, v frontend.AsyncVerdict) (*frontendv1.AsyncBubbleUpdate, error) {
	b := s.lookupLocked("", taskID)
	if b == nil {
		return nil, nil
	}
	return frontend.SettleAsyncBubble(b, v)
}

// indexCallsLocked records every tool call a detached agent made, so a
// detachment launched by one of them can be given this bubble as its parent.
func (s *asyncBubbleStore) indexCallsLocked(ems []*frontendv1.AgentEmission, bubbleID string) {
	for _, em := range ems {
		for _, block := range em.GetResponse().GetBody().GetContent() {
			if id := block.GetToolUse().GetId(); id != "" {
				s.parentByToolUse[id] = bubbleID
				if name := block.GetToolUse().GetName(); name != "" {
					s.toolNames[id] = name
				}
			}
		}
	}
}

// faultLocked builds the failure card for a detachment that could not become a
// bubble. Its uuid is DERIVED from the detachment so the card is stable across
// a resync instead of accumulating a twin per replay.
func (s *asyncBubbleStore) faultLocked(taskID, detail string) *asyncFault {
	return &asyncFault{
		UUID:   "async-fault:" + taskID,
		Card:   errclass.Card(errclass.TypeInternalUnclassified, detail),
		Detail: detail,
	}
}

// --- pure helpers ----------------------------------------------------------

// retrievalFacts reads the task id, the cumulative output, and the settlement
// (when the work has ended) off a task-output retrieval.
//
// A shell task settles on its EXIT CODE being set together with a status that
// is no longer running — the exit code is the fact, the status is the
// confirmation that it is final. A running task's absent exit code is not a
// zero, and reading it as one would report every in-flight command as a clean
// success.
func retrievalFacts(out *datav1.TaskOutputResult) (taskID, text string, verdict *frontend.AsyncVerdict) {
	switch t := out.GetTask().(type) {
	case *datav1.TaskOutputResult_LocalBash:
		taskID, text = t.LocalBash.GetTaskId(), t.LocalBash.GetOutput()
		status := t.LocalBash.GetStatus()
		if t.LocalBash.GetExitCodeSet() && status != datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING {
			code := t.LocalBash.GetExitCode()
			verdict = &frontend.AsyncVerdict{
				Status:   terminalStatusFromRaw(status),
				ExitCode: &code,
			}
		}
	case *datav1.TaskOutputResult_LocalAgent:
		taskID, text = t.LocalAgent.GetTaskId(), t.LocalAgent.GetOutput()
		if status := t.LocalAgent.GetStatus(); isRawTerminal(status) {
			verdict = &frontend.AsyncVerdict{
				Status:  terminalStatusFromRaw(status),
				Message: t.LocalAgent.GetResult(),
			}
		}
	}
	return taskID, text, verdict
}

// isRawTerminal reports whether a retrieval's raw status says the work has
// ended. A launched or running task has not.
func isRawTerminal(s datav1.RawTaskStatus) bool {
	switch s {
	case datav1.RawTaskStatus_RAW_TASK_STATUS_COMPLETED,
		datav1.RawTaskStatus_RAW_TASK_STATUS_FAILED,
		datav1.RawTaskStatus_RAW_TASK_STATUS_KILLED,
		datav1.RawTaskStatus_RAW_TASK_STATUS_STOPPED:
		return true
	default:
		return false
	}
}

// terminalStatusFromRaw translates the retrieval's raw status vocabulary into
// the shim's typed terminal status, which is the ONE vocabulary
// frontend.SettleAsyncBubble resolves an outcome from. A status this table does
// not name leaves the result UNSPECIFIED, which the settler refuses rather than
// standing in for.
func terminalStatusFromRaw(s datav1.RawTaskStatus) corev1.TerminalStatus {
	switch s {
	case datav1.RawTaskStatus_RAW_TASK_STATUS_COMPLETED:
		return corev1.TerminalStatus_TERMINAL_STATUS_DONE
	case datav1.RawTaskStatus_RAW_TASK_STATUS_FAILED:
		return corev1.TerminalStatus_TERMINAL_STATUS_ERROR
	case datav1.RawTaskStatus_RAW_TASK_STATUS_KILLED:
		return corev1.TerminalStatus_TERMINAL_STATUS_KILLED
	case datav1.RawTaskStatus_RAW_TASK_STATUS_STOPPED:
		return corev1.TerminalStatus_TERMINAL_STATUS_STOPPED
	default:
		return corev1.TerminalStatus_TERMINAL_STATUS_UNSPECIFIED
	}
}

// completeJournalPrefix returns the length of the prefix of text that ends on a
// record boundary — everything up to and including the last newline. A journal
// is read while it is being written, so the bytes after that newline are a
// partial record and are left for the next read rather than parsed as a whole
// one.
func completeJournalPrefix(text string) int {
	for i := len(text) - 1; i >= 0; i-- {
		if text[i] == '\n' {
			return i + 1
		}
	}
	return 0
}

// joinAsyncErrors folds a batch of per-bubble failures into one error, so a
// single bad fold neither aborts the rest of the batch nor disappears.
func joinAsyncErrors(errs []error) error {
	switch len(errs) {
	case 0:
		return nil
	case 1:
		return errs[0]
	default:
		msgs := make([]string, 0, len(errs))
		for _, err := range errs {
			msgs = append(msgs, err.Error())
		}
		sort.Strings(msgs)
		return fmt.Errorf("session-controller: %d async bubble failures: %v", len(errs), msgs)
	}
}
