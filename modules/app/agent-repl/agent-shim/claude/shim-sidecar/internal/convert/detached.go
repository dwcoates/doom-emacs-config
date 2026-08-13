package convert

// detached.go — work that LEFT the turn and now runs alongside it.
//
// A detachment is announced by a TOOL RESULT: the harness reports the launch
// back to the agent, and that report is what names the thing that detached. So
// the card is opened from a user record (where the vendor files tool results),
// and it is opened as a FEED ROW naming itself — "a page of ten rows is ten
// bounded things rather than ten trees".
//
// EVERY LATER RECORD ABOUT THE WORK NAMES THE SAME CARD, and none of them needs
// to remember it: DetachedWorkMessageID is a pure function of the task id, so
// the spool that carries the output, the sidechain that carries the subagent's
// conversation, and the staleness sweep that declares it lost all derive the
// same message id from the identity they already hold. Nothing is correlated
// across files and nothing has to survive a restart.
//
// THE MERGE KIND IS NEVER PRODUCED HERE, and that is per the contract: no tool
// spawns a merge run — the daemon opens it when it classifies the merge skill's
// invocation — so this reader has nothing to observe.

import (
	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// launch is one detachment read out of a tool result.
type launch struct {
	taskID string
	label  string
	kind   *conversationv1.DetachedWorkKind
	// skillName is set only for a skill invocation, so the body that arrives
	// later can be resolved onto this card by name.
	skillName string
}

// launchRecords converts a user record's `toolUseResult` into the detached-work
// lifecycle it implies: a launch opens a card, a stop closes one.
//
// A tool result that is not a launch produces nothing here and is carried by
// toolReturns as the tool's ordinary output.
func (c *Converter) launchRecords(record map[string]any, at Attribution, env envelope, container string) []*agentshimv1.Entry {
	result := obj(record["toolUseResult"])
	if result == nil {
		return nil
	}
	if stop := c.taskStop(result, at, container); stop != nil {
		return []*agentshimv1.Entry{stop}
	}
	found := classifyLaunch(result, env)
	if found == nil {
		return nil
	}
	if found.taskID == "" {
		// A launch the harness did not name. The card would have no identity for
		// its own output to route to, so the record is stored whole instead of
		// opening a card nothing can ever update.
		c.log.With(logging.Context{Operation: "detached-launch", Path: at.Path, Session: at.SessionID, Level: "warn"}).
			Log("detached launch at offset=%d carries no task identity; stored unconverted rather than opening a card nothing can update", at.Offset)
		return []*agentshimv1.Entry{UnknownEntry(at, "launch", "toolUseResult", record)}
	}

	messageID := DetachedWorkMessageID(found.taskID)
	if found.skillName != "" {
		c.skillMessage[found.skillName] = messageID
	}
	m := &conversationv1.MessageEntry{Author: authorAgent(container)}
	// A detached-work card is a FEED ROW, so it names itself even when the
	// record that announced it was read inside another card.
	lineage(m, messageID, messageID)
	m.Payload = &conversationv1.MessageEntry_DetachedWorkStarted{DetachedWorkStarted: &conversationv1.DetachedWorkStarted{
		OriginToolCallId: env.toolUseID,
		Label:            found.label,
		Kind:             found.kind,
	}}
	c.log.With(logging.Context{Operation: "detached-launch", Path: at.Path, Session: at.SessionID, Task: found.taskID}).
		Log("detached work opened message_id=%s origin_tool_call_id=%q", messageID, env.toolUseID)
	return []*agentshimv1.Entry{MessageEntry(at, "detached_started:"+found.taskID, m)}
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
			kind: &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Agent{
				Agent: &conversationv1.DetachedAgent{},
			}},
		}
	case has(result, "runId"):
		return &launch{
			taskID: firstNonEmpty(str(result["runId"]), str(result["taskId"])),
			label:  firstNonEmpty(str(result["summary"]), str(result["workflowName"])),
			kind: &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Workflow{
				Workflow: &conversationv1.DetachedWorkflow{},
			}},
		}
	case str(result["backgroundTaskId"]) != "":
		return &launch{
			taskID: str(result["backgroundTaskId"]),
			label:  str(result["backgroundCwdHint"]),
			kind: &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Shell{
				Shell: &conversationv1.DetachedShell{},
			}},
		}
	case has(result, "commandName"):
		name := str(result["commandName"])
		// A skill owns its own window, and the tool call that invoked it is the
		// only identity it has — the harness mints no task id for one.
		return &launch{
			taskID:    env.toolUseID,
			label:     name,
			skillName: name,
			kind: &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Skill{
				Skill: &conversationv1.DetachedSkill{SkillName: name},
			}},
		}
	default:
		return nil
	}
}

// taskStop converts a TaskStop result into the end of the work it stopped.
//
// It is CANCELLED rather than succeeded or failed: someone stopped it
// deliberately, which is a different thing from either outcome it might have
// reached on its own.
func (c *Converter) taskStop(result map[string]any, at Attribution, container string) *agentshimv1.Entry {
	if !has(result, "command") || !has(result, "taskType") {
		return nil
	}
	taskID := str(result["taskId"])
	if taskID == "" {
		return nil
	}
	messageID := DetachedWorkMessageID(taskID)
	m := &conversationv1.MessageEntry{Author: authorAgent(container)}
	lineage(m, messageID, messageID)
	m.Payload = &conversationv1.MessageEntry_DetachedWorkEnded{DetachedWorkEnded: &conversationv1.DetachedWorkEnded{
		Outcome: &conversationv1.DetachedWorkEnded_Cancelled{Cancelled: &conversationv1.DetachedCancelled{}},
	}}
	c.log.With(logging.Context{Operation: "detached-stop", Path: at.Path, Session: at.SessionID, Task: taskID}).
		Log("detached work cancelled message_id=%s", messageID)
	return MessageEntry(at, "detached_cancelled:"+taskID, m)
}

// DetachedProgress appends output to detached work already open.
//
// A DELTA rather than the whole spool: re-sending everything on every update is
// how a long-running shell costs more to watch than it did to run.
func DetachedProgress(at Attribution, taskID, output string) *agentshimv1.Entry {
	messageID := DetachedWorkMessageID(taskID)
	m := &conversationv1.MessageEntry{Author: authorAgent(messageID)}
	lineage(m, messageID, messageID)
	m.Payload = &conversationv1.MessageEntry_DetachedWorkProgressed{DetachedWorkProgressed: &conversationv1.DetachedWorkProgressed{
		Output: output,
	}}
	return MessageEntry(at, "detached_progress", m)
}

// DetachedExited ends detached work that told us how it exited.
//
// The exit code is the ONLY structured byte a shell spool has, and reading it is
// what keeps a task that plainly finished — and said so, on disk — from sitting
// as running until a staleness sweep eventually calls it LOST. That would be the
// wrong verdict as well as a late one: LOST means we never found out, and here
// we did.
func DetachedExited(at Attribution, taskID string, code int) *agentshimv1.Entry {
	messageID := DetachedWorkMessageID(taskID)
	m := &conversationv1.MessageEntry{Author: authorAgent(messageID)}
	lineage(m, messageID, messageID)
	ended := &conversationv1.DetachedWorkEnded{}
	if code == 0 {
		ended.Outcome = &conversationv1.DetachedWorkEnded_Succeeded{Succeeded: &conversationv1.DetachedSucceeded{}}
	} else {
		ended.Outcome = &conversationv1.DetachedWorkEnded_Failed{Failed: &conversationv1.DetachedFailed{
			Summary: exitSummary(code),
		}}
	}
	m.Payload = &conversationv1.MessageEntry_DetachedWorkEnded{DetachedWorkEnded: ended}
	return MessageEntry(at, "detached_exited", m)
}

// DetachedLost ends detached work we stopped being able to see.
//
// A SEPARATE OUTCOME FROM FAILURE, deliberately. Folding it into failure would
// have this system assert something it never observed: that the work died. The
// inference is carried so "we watched it exit" stays distinguishable from "we
// stopped hearing from it".
func DetachedLost(at Attribution, taskID, inference string) *agentshimv1.Entry {
	messageID := DetachedWorkMessageID(taskID)
	m := &conversationv1.MessageEntry{Author: authorAgent(messageID)}
	lineage(m, messageID, messageID)
	m.Payload = &conversationv1.MessageEntry_DetachedWorkEnded{DetachedWorkEnded: &conversationv1.DetachedWorkEnded{
		Outcome: &conversationv1.DetachedWorkEnded_Lost{Lost: &conversationv1.DetachedLost{Inference: inference}},
	}}
	// The verdict is stable for a task however many sweeps observe it, so the
	// write identity is too and a re-emission is a no-op at the store.
	return SyntheticMessageEntry(at, "detached_lost:"+at.SessionID+":"+taskID, m)
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
