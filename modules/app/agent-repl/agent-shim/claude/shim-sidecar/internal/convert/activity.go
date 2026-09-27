package convert

// activity.go — THE CALL SIDE: a `tool_use` content block becomes the unit that
// announces the work.
//
// `start` means "this stream now carries this unit", not "this work began". The
// unit's identity is the vendor's tool_use_id, held from here to its settle, so
// the result that arrives later UPSERTS this row rather than appending a child.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// noUnitReason names WHY a block produced no unit, for the one record that has
// to say so truthfully. An empty reason means the block produced units.
type noUnitReason string

const (
	// reasonExempt: the tool is in the exempt set and is dropped at the call
	// and at the result alike.
	reasonExempt noUnitReason = "the tool is in the exempt set"
	// reasonDeferredAnnounce: the unit is real and appears at the call's
	// RESULT, because something the announcement needs is not knowable until
	// the launch answers.
	reasonDeferredAnnounce noUnitReason = "the tool announces at its result rather than at its call"
	// reasonStreamOwned: the stream plane authors this unit whole, because the
	// transcript's rendering of it is lossy (streamowned.go); the file plane
	// deliberately writes no part of it.
	reasonStreamOwned noUnitReason = "the tool is stream-owned and not converted from the transcript"
)

// toolCallBlock converts one `tool_use` block into its announcement, and
// remembers the call so its result can settle it. A block that produced no
// unit answers WHY, so the record that reports it does not have to guess.
func (c *Converter) toolCallBlock(block map[string]any, index int, messageID string, at Attribution, env envelope, agent string) ([]*storev1.StoreEntry, noUnitReason) {
	name := str(block["name"])
	id := firstNonEmpty(str(block["id"]), BlockActivityID(messageID, index))
	input := obj(block["input"])
	if input == nil {
		input = map[string]any{}
	}

	if IsExempt(name) {
		// Dropped entirely — never residue, never unmodeled. The call is still
		// remembered, because the RESULT of one exempt tool (TaskStop) is
		// consumed as a terminal before its own drop.
		c.rememberCall(id, openCall{name: name, input: input, startedAt: env.timestampMs, activityID: id, agentID: agent})
		c.log.With(at.ctxFor("exempt-drop")).With(logging.Context{ActivityID: id}).
			LogVerbose("tool call name=%q is in the exempt set; dropped entirely", name)
		return nil, reasonExempt
	}

	mcp := mcpTool(name, block)
	c.rememberCall(id, openCall{name: name, input: input, startedAt: env.timestampMs, activityID: id, agentID: agent, mcp: mcp})

	if IsStreamOwned(name) {
		// The stream plane authors this unit whole; see streamowned.go. The call
		// is still remembered so its result is dropped as this same unit rather
		// than filed as an orphan settle.
		c.log.With(at.ctxFor("stream-owned-drop")).With(logging.Context{ActivityID: id}).
			LogVerbose("tool call name=%q is authored by the stream plane; not converted here", name)
		return nil, reasonStreamOwned
	}

	kind, known := classifyTool(name)
	if !known && mcp != nil {
		return []*storev1.StoreEntry{c.mcpToolCall(mcp, id, index, input, at, env, agent)}, ""
	}
	if !known {
		return []*storev1.StoreEntry{c.unmodeledCall(name, id, index, input, at, env, agent)}, ""
	}

	activity := c.callItem(kind, name, input, env.timestampMs, at, id)
	if activity == nil {
		// A recognized built-in whose ANNOUNCEMENT this converter does not mint
		// — today only the subagent spawn, whose created_agent_id is not knowable
		// until the launch answers. The unit appears at its settle instead.
		c.log.With(at.ctxFor("deferred-announce")).With(logging.Context{ActivityID: id}).
			LogVerbose("tool call name=%q announces at its result, not at its call", name)
		return nil, reasonDeferredAnnounce
	}

	c.log.With(at.ctxFor("tool-call")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
		LogVerbose("tool call name=%q announced", name)
	activity.ActivityId = activityID(id)
	return []*storev1.StoreEntry{c.activityEntry(at, env, agent, id, index, activity)}, ""
}

// rememberCall indexes one OPEN call. Bounded by concurrent in-flight calls: the
// entry is deleted the moment the call settles.
func (c *Converter) rememberCall(id string, call openCall) {
	if id == "" {
		return
	}
	c.openCalls[id] = call
}

// unmodeledCall announces a tool whose schema genuinely cannot be known. An MCP
// server's tool never reaches here (mcp.go).
func (c *Converter) unmodeledCall(name, id string, index int, input map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	c.log.With(at.ctxFor("unmodeled-call")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
		Log("tool call name=%q has no modeled schema; carried as AgentUnmodeled", name)
	return c.activityEntry(at, env, agent, id, index, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{
			Result: &conversationv1.AgentUnmodeled_Start{Start: &conversationv1.AgentUnmodeledStart{
				ToolName:  name,
				Arguments: rawStruct(input),
				StartedAt: startedAt(env.timestampMs),
			}},
		}},
	})
}

// callItem builds the announcement arm for a recognized built-in. It returns nil
// for a kind whose announcement cannot be minted from the call alone.
func (c *Converter) callItem(kind toolKind, name string, input map[string]any, ts int64, at Attribution, id string) *conversationv1.AgentActivity {
	switch kind {
	case kindRead:
		return item(&conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{
			Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
				Path:      requestedPath(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindWrite:
		return item(&conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{
			Result: &conversationv1.AgentWrite_Start{Start: &conversationv1.AgentWriteStart{
				Path:      requestedPath(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindEdit:
		return item(&conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{
			Result: &conversationv1.AgentEdit_Start{Start: &conversationv1.AgentEditStart{
				Path:      requestedPath(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindGrep:
		return item(&conversationv1.AgentActivity_Grep{Grep: &conversationv1.AgentGrep{
			Result: &conversationv1.AgentGrep_Start{Start: &conversationv1.AgentGrepStart{
				Query:     grepQuery(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindGlob:
		return item(&conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{
			Result: &conversationv1.AgentGlob_Start{Start: &conversationv1.AgentGlobStart{
				Query:     globQuery(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindBash:
		return item(&conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
				Command:   bashCommand(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindSubagent:
		// THE SPAWN'S ANNOUNCEMENT NEEDS created_agent_id, which the call does
		// not carry: the vendor names the created agent only in the launch's
		// answer. Announcing with it unset would violate the presence rule, so
		// the unit appears at its settle, carrying this original instant.
		return nil
	case kindSkill:
		return item(&conversationv1.AgentActivity_SkillUse{SkillUse: &conversationv1.AgentSkillUse{
			Result: &conversationv1.AgentSkillUse_Start{Start: &conversationv1.AgentSkillUseStart{
				Skill:     &conversationv1.AgentSkillName{Name: requestedSkill(input)},
				Args:      optionalString(pick(input, "args", "arguments")),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindSendMessage:
		return item(&conversationv1.AgentActivity_SendMessage{SendMessage: &conversationv1.AgentSendMessage{
			Result: &conversationv1.AgentSendMessage_Start{Start: &conversationv1.AgentSendMessageStart{
				AddressedTo: sendAddressedTo(input),
				Summary:     sendMessageSummary(input),
				Body:        &conversationv1.AgentSendMessageBody{Text: str(pick(input, "message", "body"))},
				StartedAt:   startedAt(ts),
			}},
		}})
	case kindSubagentHandback:
		return item(&conversationv1.AgentActivity_SubagentHandback{SubagentHandback: &conversationv1.AgentSubagentHandback{
			Result: &conversationv1.AgentSubagentHandback_Start{Start: &conversationv1.AgentSubagentHandbackStart{
				Report:    handbackReport(input),
				StartedAt: startedAt(ts),
			}},
		}})
	case kindTaskAct:
		// A task act is INSTANTANEOUS at this tier: what the tracker did and
		// where it left the task both come from the RESULT, so the announcement
		// has nothing truthful to say on its own.
		return nil
	case kindWebFetch:
		return item(&conversationv1.AgentActivity_WebFetch{WebFetch: &conversationv1.AgentWebFetch{
			Result: &conversationv1.AgentWebFetch_Start{Start: &conversationv1.AgentWebFetchStart{
				Target:      &conversationv1.AgentWebFetchTarget{Url: str(input["url"])},
				StartedAtMs: ts,
			}},
		}})
	case kindWebSearch:
		return item(&conversationv1.AgentActivity_WebSearch{WebSearch: &conversationv1.AgentWebSearch{
			Result: &conversationv1.AgentWebSearch_Start{Start: &conversationv1.AgentWebSearchStart{
				Query:       &conversationv1.AgentWebSearchQuery{Terms: str(pick(input, "query", "terms"))},
				StartedAtMs: ts,
			}},
		}})
	case kindMonitor:
		return item(&conversationv1.AgentActivity_Monitor{Monitor: &conversationv1.AgentMonitor{
			Result: &conversationv1.AgentMonitor_Start{Start: monitorStart(input, ts)},
		}})
	case kindScheduleWakeup:
		return item(&conversationv1.AgentActivity_ScheduleWakeup{ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
			Result: &conversationv1.AgentScheduleWakeup_Start{Start: wakeupStart(input, ts)},
		}})
	case kindArtifact:
		return item(&conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: artifactStart(input, ts)},
		}})
	case kindPlanMode:
		return item(&conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{
			State: &conversationv1.AgentPlanMode_Start{Start: planModeStart(name, ts)},
		}})
	case kindReportFindings:
		return item(&conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Start{Start: &conversationv1.AgentReportFindingsStart{
				StartedAt: startedAt(ts),
			}},
		}})
	case kindWorktree:
		return item(&conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Start{Start: worktreeStart(name, input, ts)},
		}})
	case kindCron:
		return item(&conversationv1.AgentActivity_Cron{Cron: &conversationv1.AgentCron{
			State: &conversationv1.AgentCron_Start{Start: cronStart(name, input, ts)},
		}})
	case kindPushNotification:
		return item(&conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Start{Start: &conversationv1.AgentPushNotificationStart{
				Message:   str(pick(input, "message", "text")),
				StartedAt: startedAt(ts),
			}},
		}})
	default:
		return nil
	}
}

// ---------------------------------------------------------------------------
// per-kind input readers
//
// ONE READER PER FACT, shared by a unit's start AND its settle. A settled frame
// RESTATES what its start carried so it stands alone: the start and the settle
// upsert one unit, so once the call settles its start is gone from the store,
// and a replay drawing the settle alone must still name what the call acted on.
// Reading it through the same function the start used is what keeps the two
// from drifting.
// ---------------------------------------------------------------------------

// requestedPath is the file a Read, Write or Edit call named.
func requestedPath(input map[string]any) *conversationv1.ReadPath {
	return &conversationv1.ReadPath{Path: str(pick(input, "file_path", "path"))}
}

// sendAddressedTo is WHO a send's caller addressed, exactly as written.
func sendAddressedTo(input map[string]any) string {
	return str(pick(input, "to", "agent", "recipient"))
}

func grepQuery(input map[string]any) *conversationv1.AgentGrepQuery {
	return &conversationv1.AgentGrepQuery{
		Pattern:         str(pick(input, "pattern", "query")),
		Path:            optionalString(input["path"]),
		Glob:            optionalString(input["glob"]),
		FileType:        optionalString(pick(input, "type", "file_type")),
		CaseInsensitive: boolean(pick(input, "-i", "case_insensitive")),
		Multiline:       boolean(pick(input, "multiline")),
	}
}

func globQuery(input map[string]any) *conversationv1.AgentGlobQuery {
	return &conversationv1.AgentGlobQuery{
		Pattern: str(input["pattern"]),
		Path:    optionalString(input["path"]),
	}
}

// bashCommand reads the command line and, load-bearing for consent, WHETHER THE
// SANDBOX WAS DISABLED. Absence is never read as "sandboxed": the arm is left
// unset when the vendor said nothing either way.
func bashCommand(input map[string]any) *conversationv1.AgentBashCommand {
	command := &conversationv1.AgentBashCommand{
		Line:        str(pick(input, "command", "line")),
		Description: optionalString(input["description"]),
	}
	if raw, ok := input["sandbox"]; ok {
		if enabled, isBool := raw.(bool); isBool {
			if enabled {
				command.Sandbox = &conversationv1.AgentBashCommand_Sandboxed{Sandboxed: &conversationv1.AgentBashSandboxed{}}
			} else {
				command.Sandbox = &conversationv1.AgentBashCommand_SandboxDisabled{SandboxDisabled: &conversationv1.AgentBashSandboxDisabled{}}
			}
		}
	}
	if boolean(input["dangerouslyDisableSandbox"]) {
		command.Sandbox = &conversationv1.AgentBashCommand_SandboxDisabled{SandboxDisabled: &conversationv1.AgentBashSandboxDisabled{}}
	}
	return command
}

func sendMessageSummary(input map[string]any) *conversationv1.AgentSendMessageSummary {
	text := str(input["summary"])
	if text == "" {
		return nil
	}
	return &conversationv1.AgentSendMessageSummary{Text: text}
}

// handbackReport is a subagent's final report, read off the CALL: the call's
// own result is a bare acknowledgement, so the input is the only place the
// report exists. Read by the start and by both settle arms, so the restated
// report cannot drift from the announced one.
func handbackReport(input map[string]any) *conversationv1.AgentSubagentHandbackReport {
	return &conversationv1.AgentSubagentHandbackReport{Text: str(input["message"])}
}

// monitorStart reads a watcher's arming. The lifetime arms are exclusive by the
// vendor's own rule: the timeout is ignored when the watch is persistent.
func monitorStart(input map[string]any, ts int64) *conversationv1.AgentMonitorStart {
	start := &conversationv1.AgentMonitorStart{
		Description: str(pick(input, "description", "summary")),
		StartedAtMs: ts,
	}
	if boolean(input["persistent"]) {
		start.Lifetime = &conversationv1.AgentMonitorStart_Persistent{Persistent: &conversationv1.AgentMonitorPersistent{}}
	} else {
		start.Lifetime = &conversationv1.AgentMonitorStart_Deadline{Deadline: &conversationv1.AgentMonitorDeadline{
			TimeoutMs: uint64(number(pick(input, "timeout", "timeoutMs"))),
		}}
	}
	if url := str(input["url"]); url != "" {
		start.Source = &conversationv1.AgentMonitorStart_Websocket{Websocket: &conversationv1.AgentMonitorWebsocket{Url: url}}
	} else {
		start.Source = &conversationv1.AgentMonitorStart_Command{Command: &conversationv1.AgentMonitorCommand{
			Command: str(input["command"]),
		}}
	}
	return start
}

// wakeupStart reads a self-pacing tick. The arms are exclusive by the vendor's
// own rule: every schedule field is ignored when stop is set.
func wakeupStart(input map[string]any, ts int64) *conversationv1.AgentScheduleWakeupStart {
	start := &conversationv1.AgentScheduleWakeupStart{StartedAtMs: ts}
	if boolean(input["stop"]) {
		start.Act = &conversationv1.AgentScheduleWakeupStart_Stop{Stop: &conversationv1.AgentScheduleWakeupStop{}}
		return start
	}
	start.Act = &conversationv1.AgentScheduleWakeupStart_Schedule{Schedule: &conversationv1.AgentScheduleWakeupSchedule{
		DelaySeconds: uint32(number(pick(input, "delay_seconds", "delaySeconds", "seconds"))),
		Reason:       str(input["reason"]),
		Prompt:       str(input["prompt"]),
	}}
	return start
}

// artifactStart reads a publish or a listing. Each action reads its own fields
// and ignores the other's, which is why the arms are exclusive.
func artifactStart(input map[string]any, ts int64) *conversationv1.AgentArtifactStart {
	start := &conversationv1.AgentArtifactStart{StartedAtMs: ts}
	if publish, list := artifactAct(input); list != nil {
		start.Act = &conversationv1.AgentArtifactStart_List{List: list}
	} else {
		start.Act = &conversationv1.AgentArtifactStart_Publish{Publish: publish}
	}
	return start
}

// artifactAct is the ONE reader of which act an artifact call asked for, shared
// by the start and the failure that restates it so the two cannot disagree.
// Exactly one of the two is non-nil.
func artifactAct(input map[string]any) (*conversationv1.AgentArtifactPublish, *conversationv1.AgentArtifactList) {
	if action := str(input["action"]); action == "list" {
		return nil, &conversationv1.AgentArtifactList{
			Limit: optionalUint32(input, "limit"),
			Scope: optionalString(input["scope"]),
		}
	}
	return &conversationv1.AgentArtifactPublish{
		FilePath:    str(pick(input, "file_path", "filePath")),
		Favicon:     optionalString(input["favicon"]),
		Title:       optionalString(input["title"]),
		UpdatesUrl:  optionalString(input["url"]),
		Label:       optionalString(input["label"]),
		Description: optionalString(input["description"]),
		Force:       boolean(input["force"]),
	}, nil
}

// artifactFailure restates the act a failed artifact call asked for, read
// through the start's own reader, so a replay serving the failure alone still
// draws a failed publish's card.
func artifactFailure(input map[string]any, failure *conversationv1.AgentToolFailure) *conversationv1.AgentArtifactFailure {
	f := &conversationv1.AgentArtifactFailure{Failure: failure}
	if publish, list := artifactAct(input); list != nil {
		f.Act = &conversationv1.AgentArtifactFailure_List{List: list}
	} else {
		f.Act = &conversationv1.AgentArtifactFailure_Publish{Publish: publish}
	}
	return f
}

// planModeStart states WHICH act the call is. An exit with no enter is legal: a
// session started in the plan permission mode never calls EnterPlanMode at all.
func planModeStart(name string, ts int64) *conversationv1.AgentPlanModeStart {
	start := &conversationv1.AgentPlanModeStart{StartedAt: startedAt(ts)}
	if name == "ExitPlanMode" {
		start.Act = &conversationv1.AgentPlanModeStart_Exit{Exit: &conversationv1.AgentPlanModeExit{}}
	} else {
		start.Act = &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}}
	}
	return start
}

// worktreeStart states which half of the worktree pair this call is. Unlike plan
// mode there is no coalescing anywhere: the two moments can be far apart and
// everything between them happened inside the tree.
func worktreeStart(name string, input map[string]any, ts int64) *conversationv1.AgentWorktreeStart {
	start := &conversationv1.AgentWorktreeStart{StartedAt: startedAt(ts)}
	if name == "ExitWorktree" {
		exit := &conversationv1.AgentWorktreeExit{}
		if boolean(pick(input, "remove", "delete")) {
			exit.Action = &conversationv1.AgentWorktreeExit_Remove{Remove: &conversationv1.AgentWorktreeExitRemove{
				DiscardChanges: boolean(pick(input, "discard_changes", "discardChanges")),
			}}
		} else {
			exit.Action = &conversationv1.AgentWorktreeExit_Keep{Keep: &conversationv1.AgentWorktreeExitKeep{}}
		}
		start.Act = &conversationv1.AgentWorktreeStart_Exit{Exit: exit}
		return start
	}
	start.Act = &conversationv1.AgentWorktreeStart_Enter{Enter: &conversationv1.AgentWorktreeEnter{
		Name: optionalString(input["name"]),
		Path: optionalString(input["path"]),
	}}
	return start
}

// cronStart states which scheduled-jobs act the call is.
func cronStart(name string, input map[string]any, ts int64) *conversationv1.AgentCronStart {
	start := &conversationv1.AgentCronStart{StartedAt: startedAt(ts)}
	switch name {
	case "CronDelete":
		start.Act = &conversationv1.AgentCronStart_Delete{Delete: &conversationv1.AgentCronDelete{
			JobId: str(pick(input, "job_id", "jobId", "id")),
		}}
	case "CronList":
		start.Act = &conversationv1.AgentCronStart_List{List: &conversationv1.AgentCronList{}}
	default:
		start.Act = &conversationv1.AgentCronStart_Create{Create: &conversationv1.AgentCronCreate{
			Cron:      str(input["cron"]),
			Prompt:    str(input["prompt"]),
			Recurring: boolean(input["recurring"]),
			Durable:   boolean(input["durable"]),
		}}
	}
	return start
}

// item wraps one built arm as the activity carrying it. The generated oneof
// interface is unexported, so a builder outside the proto package returns the
// message rather than the arm.
func item(arm any) *conversationv1.AgentActivity {
	activity := &conversationv1.AgentActivity{}
	switch a := arm.(type) {
	case *conversationv1.AgentActivity_Read:
		activity.Item = a
	case *conversationv1.AgentActivity_Write:
		activity.Item = a
	case *conversationv1.AgentActivity_Edit:
		activity.Item = a
	case *conversationv1.AgentActivity_Grep:
		activity.Item = a
	case *conversationv1.AgentActivity_Glob:
		activity.Item = a
	case *conversationv1.AgentActivity_Bash:
		activity.Item = a
	case *conversationv1.AgentActivity_SkillUse:
		activity.Item = a
	case *conversationv1.AgentActivity_SendMessage:
		activity.Item = a
	case *conversationv1.AgentActivity_SubagentHandback:
		activity.Item = a
	case *conversationv1.AgentActivity_WebFetch:
		activity.Item = a
	case *conversationv1.AgentActivity_WebSearch:
		activity.Item = a
	case *conversationv1.AgentActivity_Monitor:
		activity.Item = a
	case *conversationv1.AgentActivity_ScheduleWakeup:
		activity.Item = a
	case *conversationv1.AgentActivity_Artifact:
		activity.Item = a
	case *conversationv1.AgentActivity_PlanMode:
		activity.Item = a
	case *conversationv1.AgentActivity_ReportFindings:
		activity.Item = a
	case *conversationv1.AgentActivity_Worktree:
		activity.Item = a
	case *conversationv1.AgentActivity_Cron:
		activity.Item = a
	case *conversationv1.AgentActivity_PushNotification:
		activity.Item = a
	case *conversationv1.AgentActivity_Subagent:
		activity.Item = a
	case *conversationv1.AgentActivity_TaskAct:
		activity.Item = a
	case *conversationv1.AgentActivity_Hook:
		activity.Item = a
	case *conversationv1.AgentActivity_ContextInjected:
		activity.Item = a
	case *conversationv1.AgentActivity_Unmodeled:
		activity.Item = a
	case *conversationv1.AgentActivity_McpToolCall:
		activity.Item = a
	case *conversationv1.AgentActivity_Thinking:
		activity.Item = a
	case *conversationv1.AgentActivity_Response:
		activity.Item = a
	default:
		// Every arm this converter can build is listed above. Reaching here
		// means a new kind was added without a case, which must fail loudly
		// rather than produce an activity with no item at all.
		panic("convert: unhandled activity arm")
	}
	return activity
}
