package convert

// detached.go — work that LEFT the turn and now runs alongside it.
//
// A detachment is announced by a TOOL RESULT: the harness reports the launch
// back to the agent, and that report is what names the thing that detached.
//
// THE LIFECYCLE CONVERSION IS UNPORTED. Its target — conversation.v1's
// DetachedWorkStarted / DetachedWorkProgressed / DetachedWorkEnded family with
// its DetachedWorkKind and outcome arms — was deleted by the redesign. The
// successor (AgentDetachedWork over DetachableWork, carried on an AgentFrame)
// is a DIFFERENT structure keyed by DetachedWorkId and AgentActivityId, not a
// rename of the old one, and populating it from Claude's JSONL is a design
// decision this reconciliation is not entitled to take.
//
// So the vendor-side CLASSIFICATION is kept verbatim — it is JSON reading, not
// protocol — and every record it classifies is stored whole through
// UnportedEntry, loudly, rather than converted onto a shape nobody agreed.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// launch is one detachment read out of a tool result.
type launch struct {
	taskID string
	label  string
	// kind names the sort of work that detached, as the vendor's own signature
	// keys reveal it. It was a conversation.v1 DetachedWorkKind; that type is
	// gone, so the classification survives as the plain name it always was.
	kind string
	// skillName is set only for a skill invocation, so the body that arrives
	// later can be resolved onto this card by name.
	skillName string
}

// launchRecords classifies a user record's `toolUseResult` as the detached-work
// lifecycle it implies, and stores each such record unported.
//
// A tool result that is not a launch produces nothing here.
func (c *Converter) launchRecords(record map[string]any, at Attribution, env envelope, container string) []*storev1.StoreEntry {
	_ = container
	result := obj(record["toolUseResult"])
	if result == nil {
		return nil
	}
	if stop := c.taskStop(result, at, record); stop != nil {
		return []*storev1.StoreEntry{stop}
	}
	found := classifyLaunch(result, env)
	if found == nil {
		return nil
	}
	if found.taskID == "" {
		// A launch the harness did not name. The card would have no identity for
		// its own output to route to, so the record is stored whole instead of
		// opening a card nothing can ever update.
		c.log.With(logging.Context{Operation: "detached-launch", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("detached launch at offset=%d carries no task identity; stored unconverted rather than opening a card nothing can update", at.Offset)
		return []*storev1.StoreEntry{UnknownEntry(at, "launch", "toolUseResult", record)}
	}

	messageID := DetachedWorkMessageID(found.taskID)
	if found.skillName != "" {
		c.skillMessage[found.skillName] = messageID
	}
	c.log.With(logging.Context{Operation: "detached-launch", Path: at.Path, VendorSessionID: at.SessionID, TaskID: found.taskID, Level: "error"}).
		Log("detached-work START (kind=%s label=%q message_id=%s origin_tool_call_id=%q) has NO conversion under the redesigned conversation.v1: "+
			"DetachedWorkStarted was deleted and AgentDetachedWork is a different structure. Record stored unported at offset=%d",
			found.kind, found.label, messageID, env.toolUseID, at.Offset)
	return []*storev1.StoreEntry{UnportedEntry(at, "detached_started:"+found.kind, record)}
}

// classifyLaunch identifies which kind of work detached, by the signature keys
// the harness's launch results carry. Order is most-specific first, and an
// object matching nothing is not a launch at all.
func classifyLaunch(result map[string]any, env envelope) *launch {
	switch {
	case has(result, "isAsync"):
		return &launch{
			taskID: str(result["agentId"]),
			label:  str(result["description"]),
			kind:   "agent",
		}
	case has(result, "runId"):
		return &launch{
			taskID: firstNonEmpty(str(result["runId"]), str(result["taskId"])),
			label:  firstNonEmpty(str(result["summary"]), str(result["workflowName"])),
			kind:   "workflow",
		}
	case str(result["backgroundTaskId"]) != "":
		return &launch{
			taskID: str(result["backgroundTaskId"]),
			label:  str(result["backgroundCwdHint"]),
			kind:   "shell",
		}
	case has(result, "commandName"):
		name := str(result["commandName"])
		// A skill owns its own window, and the tool call that invoked it is the
		// only identity it has — the harness mints no task id for one.
		return &launch{
			taskID:    env.toolUseID,
			label:     name,
			skillName: name,
			kind:      "skill",
		}
	default:
		return nil
	}
}

// taskStop recognizes a TaskStop result — the end of work someone stopped
// deliberately — and stores it unported.
func (c *Converter) taskStop(result map[string]any, at Attribution, record map[string]any) *storev1.StoreEntry {
	if !has(result, "command") || !has(result, "taskType") {
		return nil
	}
	taskID := str(result["taskId"])
	if taskID == "" {
		return nil
	}
	c.log.With(logging.Context{Operation: "detached-stop", Path: at.Path, VendorSessionID: at.SessionID, TaskID: taskID, Level: "error"}).
		Log("detached-work CANCELLED (message_id=%s) has NO conversion under the redesigned conversation.v1: "+
			"DetachedWorkEnded and its Cancelled arm were deleted. Record stored unported at offset=%d",
			DetachedWorkMessageID(taskID), at.Offset)
	return UnportedEntry(at, "detached_cancelled", record)
}

// DetachedProgress records output appended to detached work already open.
//
// UNPORTED. DetachedWorkProgressed was deleted and nothing in the Agent* model
// spells "more output arrived on work already open" as a producer-written
// record, so the delta is stored whole and the loss is stated.
func DetachedProgress(at Attribution, taskID, output string) *storev1.StoreEntry {
	return UnportedEntry(at, "detached_progress", map[string]any{
		"task_id":            taskID,
		"detached_work_id":   DetachedWorkMessageID(taskID),
		"output":             output,
		"__unported_because": "conversation.v1 DetachedWorkProgressed was deleted with no producer-side successor",
	})
}

// DetachedExited records detached work that told us how it exited.
//
// UNPORTED. The exit code is still READ and carried verbatim — losing it would
// leave a task that plainly finished sitting as running until a staleness sweep
// called it LOST — but DetachedWorkEnded's Succeeded/Failed arms are gone, so
// the outcome is stored rather than asserted in the protocol.
func DetachedExited(at Attribution, taskID string, code int) *storev1.StoreEntry {
	return UnportedEntry(at, "detached_exited", map[string]any{
		"task_id":            taskID,
		"detached_work_id":   DetachedWorkMessageID(taskID),
		"exit_code":          float64(code),
		"summary":            exitSummary(code),
		"__unported_because": "conversation.v1 DetachedWorkEnded (Succeeded/Failed) was deleted with no producer-side successor",
	})
}

// DetachedLost records detached work we stopped being able to see.
//
// UNPORTED, but STILL A SEPARATE OUTCOME FROM FAILURE. Folding it into failure
// would have this system assert something it never observed: that the work
// died. The inference is carried so "we watched it exit" stays distinguishable
// from "we stopped hearing from it".
//
// The verdict is stable for a task however many sweeps observe it, so the write
// identity is too and a re-emission is a no-op at the store.
func DetachedLost(at Attribution, taskID, inference string) *storev1.StoreEntry {
	return SyntheticUnportedEntry(at, "detached_lost:"+at.SessionID+":"+taskID, "detached_lost", map[string]any{
		"task_id":            taskID,
		"detached_work_id":   DetachedWorkMessageID(taskID),
		"inference":          inference,
		"__unported_because": "conversation.v1 DetachedWorkEnded (Lost) was deleted with no producer-side successor",
	})
}

func exitSummary(code int) string {
	return "exited with status " + itoa(code)
}

// itoa avoids pulling strconv in for one call in a hot path.
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	negative := n < 0
	if negative {
		n = -n
	}
	var digits [20]byte
	i := len(digits)
	for n > 0 {
		i--
		digits[i] = byte('0' + n%10)
		n /= 10
	}
	if negative {
		i--
		digits[i] = '-'
	}
	return string(digits[i:])
}

// has reports whether an object carries a key, matching the vendor's camelCase
// and snake_case spellings of one name.
func has(o map[string]any, key string) bool {
	if _, ok := o[key]; ok {
		return true
	}
	want := canon(key)
	for k := range o {
		if canon(k) == want {
			return true
		}
	}
	return false
}

// canon folds a field name to its case- and separator-insensitive form, so the
// disk's `toolUseID`, `tool_use_id` and `toolUseId` all collide.
func canon(s string) string {
	var b []byte
	for i := 0; i < len(s); i++ {
		ch := s[i]
		switch {
		case ch == '_' || ch == '-':
			continue
		case ch >= 'A' && ch <= 'Z':
			b = append(b, ch+('a'-'A'))
		default:
			b = append(b, ch)
		}
	}
	return string(b)
}
