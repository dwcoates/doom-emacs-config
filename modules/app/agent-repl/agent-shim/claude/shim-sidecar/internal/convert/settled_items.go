package convert

// settled_items.go — the per-kind settled arms.
//
// EVERY SETTLED ARM STANDS ALONE: it restates what its start carried, read
// through the same per-kind reader the start used (activity.go), because the
// start and the settle upsert one unit and the start is gone once this lands.
//
// EMPTY RESULTS ARE SUCCESS, NOT FAILURE: a search that matched nothing answered
// the question it was asked. A NON-ZERO SHELL EXIT IS COMPLETED, not a failure
// arm: the command ran, and the code is its own verdict on itself. The failure
// arm is reserved for a call that could not be PERFORMED.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// settledItem builds the settled arm for a recognized built-in.
func (c *Converter) settledItem(kind toolKind, call openCall, result, block map[string]any, failed bool, ts int64, at Attribution) *conversationv1.AgentActivity {
	failure := toolFailure(block, ts)
	switch kind {
	case kindRead:
		if failed {
			return item(&conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{
				Result: &conversationv1.AgentRead_Failure{Failure: &conversationv1.AgentReadFailure{
					Error: failure,
					Path:  requestedPath(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{
			Result: &conversationv1.AgentRead_Success{Success: readSuccess(call, result, ts)},
		}})
	case kindWrite:
		if failed {
			return item(&conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{
				Result: &conversationv1.AgentWrite_Failure{Failure: &conversationv1.AgentWriteFailure{
					Error: failure,
					Path:  requestedPath(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{
			Result: &conversationv1.AgentWrite_Success{Success: writeSuccess(call, result, ts)},
		}})
	case kindEdit:
		if failed {
			return item(&conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{
				Result: &conversationv1.AgentEdit_Failure{Failure: &conversationv1.AgentEditFailure{
					Error: failure,
					Path:  requestedPath(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{
			Result: &conversationv1.AgentEdit_Success{Success: editSuccess(call, result, ts)},
		}})
	case kindGrep:
		if failed {
			return item(&conversationv1.AgentActivity_Grep{Grep: &conversationv1.AgentGrep{
				Result: &conversationv1.AgentGrep_Failure{Failure: &conversationv1.AgentGrepFailure{
					Error: failure,
					Query: grepQuery(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_Grep{Grep: &conversationv1.AgentGrep{
			Result: &conversationv1.AgentGrep_Success{Success: grepSuccess(call, result, block, ts)},
		}})
	case kindGlob:
		if failed {
			return item(&conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{
				Result: &conversationv1.AgentGlob_Failure{Failure: &conversationv1.AgentGlobFailure{
					Error: failure,
					Query: globQuery(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{
			Result: &conversationv1.AgentGlob_Success{Success: globSuccess(call, result, block, ts)},
		}})
	case kindBash:
		// A NONZERO EXIT IS THE COMMAND'S VERDICT ON ITSELF, NOT A FAILED CALL.
		// The vendor marks such a result an error for the model, so `is_error`
		// alone drew every failing test run as a broken shell. A result that
		// states an exit ran; only one that states none — a tool error, a killed
		// call — could not be performed.
		// A BACKGROUNDED COMMAND DID NOT END, IT MOVED, and this plane owes the
		// same silence the stream plane keeps. The vendor returns the SAME
		// receipt for a command that finished and for one it launched into the
		// background -- empty output and a `backgroundTaskId` -- so settling on
		// it says the work concluded when it had not. The detached-work frames
		// naming this unit are what say where it went.
		//
		// THE TWO PLANES MUST AGREE, and that is why this is here rather than
		// only in convert/tools/bash.ts. Both write this unit under one upsert
		// key, so a file-plane terminal arriving second REPLACED the stream
		// plane's live card with a settled one carrying no output at all: a
		// timed-out `sleep 600` drew as a command that ran and printed nothing
		// while its own detached row was still ticking beside it.
		if bashMovedToBackground(result) {
			c.log.With(at.ctxFor("bash")).With(logging.Context{ActivityID: call.activityID}).
				LogVerbose("the command MOVED rather than ended; its detached-work frames settle it, not this receipt")
			return nil
		}
		exit := bashExitCode(result, block, failed)
		if failed && exit == nil {
			return item(&conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{
				Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{
					Error:   failure,
					Command: bashCommand(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: bashSuccess(call, result, block, exit, ts)},
		}})
	case kindSubagent:
		return c.subagentSettled(call, result, failed, failure, ts, at)
	case kindSkill:
		// A VENDOR-STATED ERROR IS THE UNIT'S END. Nothing further arrives for a
		// skill that did not resolve — no document will ever land — so an error
		// result settles the invocation as a failure here, exactly as the shim's
		// own converter does (convert/tools/skill-use.ts settle). Swallowing it
		// left this plane's START row as the last word on the unit, which
		// overwrote the stream plane's failed card back to running.
		if failed {
			return c.skillFailed(call, failure)
		}
		// A SKILL'S OWN RETURN IS WORTHLESS TO DRAW: the producer answers with a
		// bare acknowledgement restating the name. The unit settles when the
		// DOCUMENT lands, as a separate injected-context record linked back to
		// this call — so nothing is emitted here. THE ALLOWANCES RIDE THIS
		// ACKNOWLEDGEMENT AND NOTHING ELSE, so they are retained onto the call
		// the document will settle.
		call.retainedAllowedTools = skillAllowedTools(result)
		c.rememberSkillCall(call)
		c.log.With(at.ctxFor("skill")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID)}).
			LogVerbose("skill call acknowledged; the unit settles when its document lands")
		return nil
	case kindSendMessage:
		if failed {
			return item(&conversationv1.AgentActivity_SendMessage{SendMessage: &conversationv1.AgentSendMessage{
				Result: &conversationv1.AgentSendMessage_Failure{Failure: &conversationv1.AgentSendMessageFailure{
					Error:       failure,
					AddressedTo: sendAddressedTo(call.input),
					Summary:     sendMessageSummary(call.input),
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_SendMessage{SendMessage: &conversationv1.AgentSendMessage{
			Result: &conversationv1.AgentSendMessage_Success{Success: sendMessageSuccess(call, result, ts)},
		}})
	case kindTaskAct:
		return taskActSettled(call, result, failed, failure)
	case kindWebFetch:
		if failed {
			return item(&conversationv1.AgentActivity_WebFetch{WebFetch: &conversationv1.AgentWebFetch{
				Result: &conversationv1.AgentWebFetch_Failure{Failure: &conversationv1.AgentWebFetchFailure{
					Target:  &conversationv1.AgentWebFetchTarget{Url: str(call.input["url"])},
					Failure: failure,
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_WebFetch{WebFetch: &conversationv1.AgentWebFetch{
			Result: &conversationv1.AgentWebFetch_Success{Success: webFetchSuccess(call, result)},
		}})
	case kindWebSearch:
		if failed {
			return item(&conversationv1.AgentActivity_WebSearch{WebSearch: &conversationv1.AgentWebSearch{
				Result: &conversationv1.AgentWebSearch_Failure{Failure: &conversationv1.AgentWebSearchFailure{
					Query:   &conversationv1.AgentWebSearchQuery{Terms: str(pick(call.input, "query", "terms"))},
					Failure: failure,
				}},
			}})
		}
		return item(&conversationv1.AgentActivity_WebSearch{WebSearch: &conversationv1.AgentWebSearch{
			Result: &conversationv1.AgentWebSearch_Success{Success: c.webSearchSuccess(call, result, at)},
		}})
	case kindMonitor:
		if failed {
			return item(&conversationv1.AgentActivity_Monitor{Monitor: &conversationv1.AgentMonitor{
				Result: &conversationv1.AgentMonitor_Failure{Failure: &conversationv1.AgentMonitorFailure{Failure: failure}},
			}})
		}
		// A monitor is ALWAYS DETACHED — arming it does not end it. The result
		// only acknowledges the arm, so the announcement stands and nothing is
		// upserted here; the watch's own end is what settles it.
		return nil
	case kindScheduleWakeup:
		if failed {
			return item(&conversationv1.AgentActivity_ScheduleWakeup{ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
				Result: &conversationv1.AgentScheduleWakeup_Failure{Failure: &conversationv1.AgentScheduleWakeupFailure{Failure: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_ScheduleWakeup{ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
			Result: &conversationv1.AgentScheduleWakeup_Success{Success: wakeupSuccess(call, result)},
		}})
	case kindArtifact:
		if failed {
			return item(&conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
				Result: &conversationv1.AgentArtifact_Failure{Failure: &conversationv1.AgentArtifactFailure{Failure: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Success{Success: artifactSuccess(call, result)},
		}})
	case kindPlanMode:
		if failed {
			return item(&conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{
				State: &conversationv1.AgentPlanMode_Failure{Failure: &conversationv1.AgentPlanModeFailure{Error: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{
			State: &conversationv1.AgentPlanMode_Success{Success: planModeSuccess(call, result, ts)},
		}})
	case kindReportFindings:
		if failed {
			return item(&conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
				State: &conversationv1.AgentReportFindings_Failure{Failure: &conversationv1.AgentReportFindingsFailure{Error: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Success{Success: findingsSuccess(call, result, ts)},
		}})
	case kindWorktree:
		if failed {
			return item(&conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
				State: &conversationv1.AgentWorktree_Failure{Failure: &conversationv1.AgentWorktreeFailure{Error: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: worktreeSuccess(call, result, ts)},
		}})
	case kindCron:
		if failed {
			return item(&conversationv1.AgentActivity_Cron{Cron: &conversationv1.AgentCron{
				State: &conversationv1.AgentCron_Failure{Failure: &conversationv1.AgentCronFailure{Error: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_Cron{Cron: &conversationv1.AgentCron{
			State: &conversationv1.AgentCron_Success{Success: cronSuccess(call, result, ts)},
		}})
	case kindPushNotification:
		if failed {
			return item(&conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
				State: &conversationv1.AgentPushNotification_Failure{Failure: &conversationv1.AgentPushNotificationFailure{Error: failure}},
			}})
		}
		return item(&conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Success{Success: pushSuccess(result, ts)},
		}})
	default:
		return nil
	}
}

// settleUnmodeled settles a tool whose schema genuinely cannot be known.
func (c *Converter) settleUnmodeled(call openCall, block map[string]any, failed bool, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	content := resultContent(block["content"])
	if content == nil {
		content = &conversationv1.ToolResultContent{}
	}
	var activity *conversationv1.AgentActivity
	if failed {
		activity = item(&conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{
			Result: &conversationv1.AgentUnmodeled_Failure{Failure: &conversationv1.AgentUnmodeledFailure{
				ToolName:  call.name,
				Content:   content,
				SettledAt: settledAt(env.timestampMs),
			}},
		}})
	} else {
		activity = item(&conversationv1.AgentActivity_Unmodeled{Unmodeled: &conversationv1.AgentUnmodeled{
			Result: &conversationv1.AgentUnmodeled_Success{Success: &conversationv1.AgentUnmodeledSuccess{
				ToolName:  call.name,
				Content:   content,
				SettledAt: settledAt(env.timestampMs),
			}},
		}})
	}
	activity.ActivityId = activityID(call.activityID)
	c.log.With(at.ctxFor("unmodeled-return")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID)}).
		LogVerbose("unmodeled tool name=%q settled failed=%t", call.name, failed)
	return c.settledEntry(at, agent, call.activityID, activity)
}
