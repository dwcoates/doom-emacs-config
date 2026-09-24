package convert

// subagent.go — THE SPAWN, and the agent it created.
//
// A SPAWN IS A UNIT OF THE CALLER'S WORK; the agent it produced is a different
// thing with its own identity. `AgentSubagentStart.created_agent_id` is the join
// key the whole flat model rests on — every later frame carrying that identity
// routes into the container a consumer drew on seeing this unit.
//
// THE ANNOUNCEMENT IS MINTED AT THE RESULT, NOT AT THE CALL, and this is a
// deliberate override of "announce at issue": created_agent_id is NOT OPTIONAL
// and the vendor names the created agent only in the launch's answer. Announcing
// with it unset would break the presence rule; announcing with an invented value
// would break the identity rule. So the unit appears when the id does, carrying
// the ORIGINAL call instant so a drawn clock measures the spawn, not the answer.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// subagentSettled converts an Agent tool result into the spawn's unit.
//
// The vendor's own split is NOT reflected: an awaited spawn returns a full
// report, a backgrounded one returns a bare launch acknowledgement, and both map
// onto this one lifecycle. A consumer that had to know which the producer used
// would be learning a calling convention in order to draw a bubble.
func (c *Converter) subagentSettled(call openCall, result map[string]any, failed bool, failure *conversationv1.AgentToolFailure, ts int64, at Attribution) *conversationv1.AgentActivity {
	// THE CREATED AGENT'S IDENTITY IS THIS CALL. The cross-plane minting rule
	// (conversation/v1 AgentId) binds every producer to one id for one agent: a
	// subagent's AgentId is the tool_use_id of the call that spawned it. The
	// vendor's own `agentId` is a LOCATOR — it names the sidechain records and
	// the a* spool on disk — and is kept for owner resolution, never used as an
	// identity. The two spaces stay distinct even though the bytes coincide with
	// this call's activity id.
	created := call.activityID
	vendorAgentID := str(pick(result, "agentId", "agent_id"))
	prompt := subagentPrompt(call, result)

	if failed {
		f := &conversationv1.AgentSubagentFailure{Error: failure}
		if boolean(pick(result, "stoppedByUser", "stopped_by_user")) {
			f.Cause = &conversationv1.AgentSubagentFailure_StoppedByUser{StoppedByUser: &conversationv1.AgentSubagentStoppedByUser{}}
		}
		// BENIGN CONVERSION OF A RECORDED FAILURE — debug, not warn. The sidecar
		// is a copier: `failed` is the vendor's own is_error on a past Agent tool
		// result, and this branch converts it FAITHFULLY into the Failure arm.
		// That arm is the coverage the consumer renders — the subagent failure is
		// preserved, not lost. This is conversation CONTENT, not a sidecar fault,
		// and warn conflated "the transcript recorded a failure" with "the
		// sidecar had a problem", so every historical subagent failure flooded a
		// cold re-scan's strict harvest. The genuine conversion defects nearby
		// keep their severity: an async launch with no spawning-call id is error
		// (the spawn cannot be announced), and one with no vendor agent id is
		// warn (its spool cannot be attributed).
		c.log.With(at.ctxFor("subagent")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID), BookAgentID: created}).
			LogVerbose("subagent spawn failed")
		return item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: f},
		}})
	}

	// AN ASYNC LAUNCH IS AN ANNOUNCEMENT, NOT A CONCLUSION. The vendor answers a
	// backgrounded spawn with a bare acknowledgement, so the unit STARTS here and
	// its own stream carries it from now on.
	if boolean(result["isAsync"]) {
		if created == "" {
			c.log.With(at.ctxError("subagent")).With(logging.Context{ActivityID: call.activityID}).
				Log("async subagent launch carries no spawning-call id; the spawn has no identity and cannot be announced")
			return nil
		}
		if vendorAgentID == "" {
			// The identity is safe (it is this call), but nothing will attribute
			// the agent's own a* spool without the vendor's locator.
			c.log.With(at.ctxWarn("subagent")).With(logging.Context{ActivityID: call.activityID}).
				Log("async subagent launch names no vendor agent id; its transcript spool cannot be attributed to this spawn")
		}
		c.log.With(at.ctxFor("subagent")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID), BookAgentID: created}).
			Log("async subagent launch announced; its own stream carries it from here")
		return item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId:       agentID(created),
				Prompt:               prompt,
				StartedAt:            startedAt(call.startedAt),
				SpawnDepth:           optionalUint32(result, "spawnDepth"),
				WorkingDir:           optionalString(pick(result, "workingDir", "cwd")),
				TranscriptSuppressed: boolean(pick(result, "transcriptSuppressed", "transcript_suppressed")),
			}},
		}})
	}

	c.log.With(at.ctxFor("subagent")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID), BookAgentID: created}).
		LogVerbose("subagent spawn settled")

	success := &conversationv1.AgentSubagentSuccess{
		// THE SAME IDENTITY THE START STATES. A sidecar delivery is exactly the
		// case the field exists for: a transcript read after the fact carries
		// this conclusion and no start, and a consumer that could not name the
		// created agent here would draw a bubble addressing nothing. `agentID`
		// answers nil for an empty id, so a call whose own id is unknown leaves
		// this UNSET rather than inventing one.
		CreatedAgentId:       agentID(created),
		Prompt:               prompt,
		Report:               subagentReport(result),
		Totals:               subagentTotals(result),
		ResolvedSubagentType: optionalString(pick(result, "agentType", "resolvedSubagentType")),
		SettledAt:            settledAt(ts, call.startedAt),
	}
	if model := str(pick(result, "resolvedModel", "model")); model != "" {
		success.ModelsUsed = []*conversationv1.AgentModel{{Name: model}}
	}
	if worktree := subagentWorktree(result); worktree != nil {
		success.Worktree = worktree
	}
	return item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
		Result: &conversationv1.AgentSubagent_Success{Success: success},
	}})
}

// subagentPrompt reads WHAT THE SUBAGENT WAS ASKED TO DO. Carried on every frame
// of the spawn, so each frame stands alone.
func subagentPrompt(call openCall, result map[string]any) *conversationv1.AgentSubagentPrompt {
	prompt := &conversationv1.AgentSubagentPrompt{
		Description:      optionalString(pick(call.input, "description")),
		Text:             firstNonEmpty(str(call.input["prompt"]), str(result["prompt"])),
		SubagentType:     optionalString(pick(call.input, "subagent_type", "subagentType")),
		RequestedName:    optionalString(pick(call.input, "name", "requested_name")),
		ForkedFromCaller: str(pick(call.input, "subagent_type", "subagentType")) == "fork",
	}
	if model := str(pick(call.input, "model", "requested_model")); model != "" {
		prompt.RequestedModel = &conversationv1.AgentModel{Name: model}
	}
	switch str(pick(call.input, "isolation")) {
	case "worktree":
		prompt.Isolation = &conversationv1.AgentSubagentPrompt_Worktree{Worktree: &conversationv1.AgentSubagentIsolationWorktree{}}
	case "remote":
		prompt.Isolation = &conversationv1.AgentSubagentPrompt_Remote{Remote: &conversationv1.AgentSubagentIsolationRemote{
			SessionUrl:   optionalString(pick(result, "sessionUrl", "session_url")),
			RemoteTaskId: optionalString(pick(result, "remoteTaskId", "remote_task_id")),
		}}
	default:
		prompt.Isolation = &conversationv1.AgentSubagentPrompt_None{None: &conversationv1.AgentSubagentIsolationNone{}}
	}
	return prompt
}

// subagentReport carries the subagent's own words RENDERED AND NEVER PARSED, and
// any structured result it wrote alongside them.
func subagentReport(result map[string]any) *conversationv1.AgentSubagentReport {
	report := &conversationv1.AgentSubagentReport{
		Prose: &conversationv1.AgentResponseProse{Markdown: flattenResultText(result["content"])},
	}
	if structured := obj(pick(result, "structuredResult", "structured_result")); structured != nil {
		report.StructuredResult = rawStruct(structured)
	}
	return report
}

// subagentTotals: THE SET ARM IS THE SPAWN PATH'S HONESTY. A sync run carries the
// full billed breakdown; an async one carries at most a total-tokens scalar, so a
// full breakdown on an async run is unproducible and unrepresentable.
func subagentTotals(result map[string]any) *conversationv1.AgentSubagentTotals {
	totals := &conversationv1.AgentSubagentTotals{
		DurationMs:   uint64(number(pick(result, "totalDurationMs", "durationMs"))),
		ToolUseCount: uint32(number(pick(result, "totalToolUseCount", "toolUseCount"))),
	}
	if usage := readUsage(obj(result["usage"])); usage != nil {
		totals.Usage = &conversationv1.AgentSubagentTotals_Full{Full: usage}
	} else {
		totals.Usage = &conversationv1.AgentSubagentTotals_TotalOnly{TotalOnly: &conversationv1.AgentSubagentAsyncUsage{
			TotalTokens: optionalUint64(result, "totalTokens"),
		}}
	}
	if stats := obj(pick(result, "toolStats", "tool_stats")); stats != nil {
		totals.ToolStats = &conversationv1.AgentSubagentToolStats{
			ReadCount:      uint32(number(stats["readCount"])),
			SearchCount:    uint32(number(stats["searchCount"])),
			BashCount:      uint32(number(stats["bashCount"])),
			EditFileCount:  uint32(number(stats["editFileCount"])),
			LinesAdded:     uint32(number(stats["linesAdded"])),
			LinesRemoved:   uint32(number(stats["linesRemoved"])),
			OtherToolCount: uint32(number(stats["otherToolCount"])),
			FrameCount:     uint32(number(stats["frameCount"])),
		}
	}
	return totals
}

// subagentWorktree states what was ACTUALLY USED, which is only knowable now —
// beside the prompt's isolation arm, which states what was ASKED FOR at issue.
func subagentWorktree(result map[string]any) *conversationv1.AgentSubagentWorktree {
	w := obj(pick(result, "worktree"))
	if w == nil {
		return nil
	}
	worktree := &conversationv1.AgentSubagentWorktree{
		Path:   str(w["path"]),
		Branch: str(w["branch"]),
	}
	if boolean(w["inherited"]) {
		worktree.Provenance = &conversationv1.AgentSubagentWorktree_Inherited{Inherited: &conversationv1.AgentSubagentWorktreeInherited{}}
	} else {
		worktree.Provenance = &conversationv1.AgentSubagentWorktree_Created{Created: &conversationv1.AgentSubagentWorktreeCreated{}}
	}
	if boolean(w["removed"]) {
		worktree.Cleanup = &conversationv1.AgentSubagentWorktree_Removed{Removed: &conversationv1.AgentSubagentWorktreeRemoved{}}
	} else if boolean(w["retained"]) {
		worktree.Cleanup = &conversationv1.AgentSubagentWorktree_Retained{Retained: &conversationv1.AgentSubagentWorktreeRetained{}}
	}
	return worktree
}

// optionalUint64 returns a pointer for a present numeric value, so an
// unreported total stays UNSET rather than becoming a zero a consumer would draw.
func optionalUint64(o map[string]any, key string) *uint64 {
	raw, ok := o[key]
	if !ok || raw == nil {
		return nil
	}
	f, ok := raw.(float64)
	if !ok {
		return nil
	}
	v := uint64(f)
	return &v
}
