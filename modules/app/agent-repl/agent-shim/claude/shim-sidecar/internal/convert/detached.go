package convert

// detached.go — THE DETACHED SHELL SPOOL: bytes on disk becoming a run's frames.
//
// A foreground shell call has NO output anywhere until it returns, so the update
// arm is structurally detach-only: every delta here comes from a `b*.output`
// spool the vendor writes for a backgrounded command. The spool is a delta stream
// terminated by its own `EXIT=<code>` line.
//
// LOST IS ITS OWN WORD — "we stopped seeing it", not "known failed". The wire has
// no DetachedLost message this wave, so a lost run resolves as
// AgentBash.success.interrupted with NO cause (the producer states none) and the
// LOST arm is named in the log. Folding it into a failure would have this system
// assert something it never observed: that the work died.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// LostReason is HOW we concluded a detached run was lost.
//
// IT RIDES THE WIRE. conversation.v1 carries DetachedLost with exactly these
// three arms (file_vanished / went_silent / swept_up), reached through
// AgentBashInterrupted.cause.lost and AgentSubagentFailure.cause.lost, so the
// account of how we stopped seeing a run is a fact a READER can draw — not
// something recoverable only by grepping this process's log. DetachedLostArm
// below is the mapping, and it PANICS on a reason it does not know rather than
// leaving the oneof unset, because an unset oneof is illegal here and a lost
// run with no arm says nothing at all.
//
// It rides the log as well, in the `reason` key, so a terminal on the wire and
// the record that concluded it can be joined.
type LostReason string

const (
	// LostFileVanished: the spool we were tailing is no longer on disk.
	LostFileVanished LostReason = "file_vanished"
	// LostWentSilent: the spool stopped growing past the staleness window.
	LostWentSilent LostReason = "went_silent"
	// LostSweptUp: a boot sweep found the run open with nothing still writing.
	LostSweptUp LostReason = "swept_up"
)

// BashDelta converts a batch of spool bytes into the run's update frame.
//
// `fromOffset` is a GAP DETECTOR, not addressing: it MUST equal the number of
// bytes the consumer has already accumulated for this unit, and anything else
// means bytes were lost and the consumer refuses the frame rather than
// concatenating across a hole.
func (c *Converter) BashDelta(at Attribution, run, output string, fromOffset int64) *storev1.StoreEntry {
	c.log.With(at.ctxFor("bash-delta")).With(logging.Context{
		ActivityID: run, UpsertKey: BashDeltaKey(run, fromOffset), Offset: logging.Off(fromOffset),
	}).LogVerbose("spool delta bytes=%d from_offset=%d", len(output), fromOffset)
	return BashRun(at, "bash_delta:"+itoa(int(fromOffset)), BashDeltaKey(run, fromOffset), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Update{Update: &conversationv1.AgentBashUpdate{
			NewOutput:  output,
			FromOffset: uint64(fromOffset),
		}},
	})
}

// BashExited converts the spool's `EXIT=<code>` terminator into the run's
// terminal.
//
// THE CODE IS THE COMMAND'S VERDICT ON ITSELF, never a failure of the call: a
// non-zero exit is still the COMPLETED arm. Ending the run on this evidence is
// also what keeps a task that plainly finished from sitting open until a
// staleness sweep eventually — and wrongly — calls it LOST.
func (c *Converter) BashExited(at Attribution, run, output string, omitted uint64, code int) *storev1.StoreEntry {
	c.log.With(at.ctxFor("bash-exit")).With(logging.Context{ActivityID: run, UpsertKey: BashTerminalKey(run)}).
		Log("EXIT=%d observed on disk; the run ends on evidence rather than on a silence timeout", code)
	return BashRun(at, "bash_terminal", BashTerminalKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: at.TaskID},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: spoolOutput(output, omitted),
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: int32(code)}},
				},
			}},
		}},
	})
}

// BashLost converts a run we STOPPED BEING ABLE TO SEE into its terminal.
//
// THE CAUSE IS `lost`, AND THE ARM IS HOW WE CONCLUDED IT. Landing 3 gave
// DetachedLost a home on AgentBashInterrupted.cause, so "we stopped seeing it"
// is now a statement the wire carries rather than a fact that survived only in
// this reader's log. It is deliberately NOT `by_user` or `timed_out`: those name
// decisions, and no decision was observed — which is exactly why `lost` draws as
// its own word downstream and never as a cancel or a failure.
func (c *Converter) BashLost(at Attribution, run, output string, omitted uint64, reason LostReason) *storev1.StoreEntry {
	c.log.With(at.ctxWarn("bash-lost")).With(logging.Context{ActivityID: run, UpsertKey: BashTerminalKey(run)}).
		Log("the detached run is LOST (%s); it resolves interrupted with cause=lost naming that arm", reason)
	return BashLostEntry(at, run, output, omitted, reason)
}

// BashLostEntry is BashLost without a converter, for the staleness policy in the
// root package, which owns its own logging and holds no per-file converter.
//
// THE WRITE IDENTITY IS STABLE FOR THE VERDICT: a run is lost once however many
// sweeps observe it, so a re-emission is absorbed at the store rather than
// appending a second terminal.
func BashLostEntry(at Attribution, run, output string, omitted uint64, reason LostReason) *storev1.StoreEntry {
	return BashRun(at, "bash_terminal", BashTerminalKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: at.TaskID},
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: spoolOutput(output, omitted),
				Cause:  &conversationv1.AgentBashInterrupted_Lost{Lost: DetachedLostArm(reason)},
			}},
		}},
	})
}

// BashCancelled converts a person's stop into the run's terminal, carrying what
// the run had said by the time it was cut.
//
// A CANCEL IS A DECISION AND SAYS SO: `by_user` is the one cause here that IS
// evidence — a TaskStop result is a person's act, recorded by the vendor. It is
// deliberately not `lost`: we did not stop seeing this run, we were told it was
// stopped.
func (c *Converter) BashCancelled(at Attribution, run, output string, omitted uint64, settledAtMs int64) *storev1.StoreEntry {
	c.log.With(at.ctxFor("bash-cancelled")).With(logging.Context{ActivityID: run, UpsertKey: BashTerminalKey(run)}).
		Log("the detached run was stopped by a person; it resolves interrupted with cause=by_user carrying the %d byte(s) it had produced", len(output))
	return BashRun(at, "bash_terminal", BashTerminalKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command:   &conversationv1.AgentBashCommand{Line: at.TaskID},
			SettledAt: settledAt(settledAtMs),
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: spoolOutput(output, omitted),
				Cause:  &conversationv1.AgentBashInterrupted_ByUser{ByUser: &conversationv1.AgentBashInterruptedByUser{}},
			}},
		}},
	})
}

// DetachedLostArm spells a reader's LOST vocabulary as the wire's arm.
//
// AN UNRECOGNIZED REASON IS A PROGRAMMING ERROR, NOT A DEFAULT. The three arms
// ARE the reader's three ways of stopping seeing a run, so a fourth string means
// this package and the staleness policy have drifted apart — and picking an arm
// to keep going would have the wire assert something nobody observed. It fails
// hard instead.
func DetachedLostArm(reason LostReason) *conversationv1.DetachedLost {
	switch reason {
	case LostFileVanished:
		return &conversationv1.DetachedLost{
			How: &conversationv1.DetachedLost_FileVanished{FileVanished: &conversationv1.DetachedLostFileVanished{}},
		}
	case LostWentSilent:
		return &conversationv1.DetachedLost{
			How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}},
		}
	case LostSweptUp:
		return &conversationv1.DetachedLost{
			How: &conversationv1.DetachedLost_SweptUp{SweptUp: &conversationv1.DetachedLostSweptUp{}},
		}
	default:
		panic("convert: unknown LostReason " + string(reason) + " — DetachedLost's arms are the reader's whole vocabulary and an unset oneof is illegal")
	}
}

// taskStopTerminal consumes a TaskStop RESULT as the owning task's CANCELLED
// terminal, before the call itself is dropped.
//
// DELIBERATELY-STOPPED WORK MUST RESOLVE CANCELLED, NEVER LOST. An AGENT task
// settles here, because the spawn unit is a line in THIS stream's book and this
// converter owns it. A SHELL task does not: its terminal owes the output the
// spool holds, so the fact is reported and the spool's reader mints it.
func (c *Converter) taskStopTerminal(result map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	taskID := str(pick(result, "task_id", "taskId"))
	taskType := str(pick(result, "task_type", "taskType"))
	if taskID == "" {
		c.log.With(at.ctxWarn("task-stop")).
			Log("TaskStop result names no task; the stop cannot be attributed and the record is stored as vendor_specific")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "task_stop/unattributed", result)}
	}

	switch taskType {
	case "agent":
		// THE SPAWN UNIT IS KEYED BY THE CALL THAT SPAWNED IT, never by the
		// vendor task id: the unit being settled is the Agent CALL in this
		// stream's book, and the task id names the harness's bookkeeping for it.
		run, launched := c.spawnedRuns[taskID]
		if !launched {
			c.log.With(at.ctxWarn("task-stop")).With(logging.Context{TaskID: taskID}).
				Log("TaskStop names an agent task no launch on this stream opened; the spawn unit it settles cannot be identified and the record is stored as vendor_specific")
			return []*storev1.StoreEntry{VendorSpecificEntry(at, "task_stop/unlaunched", result)}
		}
		c.log.With(at.ctxFor("task-stop")).With(logging.Context{TaskID: taskID, ActivityID: run, UpsertKey: ActivityKey(run)}).
			Log("TaskStop consumed as the CANCELLED terminal of an agent task")
		activity := item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
				Cause: &conversationv1.AgentSubagentFailure_StoppedByUser{StoppedByUser: &conversationv1.AgentSubagentStoppedByUser{}},
			}},
		}})
		activity.ActivityId = activityID(run)
		return []*storev1.StoreEntry{c.settledEntry(at, agent, run, activity)}
	default:
		// A SHELL TASK'S CANCELLED TERMINAL IS MINTED BY THE SPOOL'S READER, not
		// here. The terminal must carry what the run had said, and those bytes
		// are in the spool this converter never reads — so the fact is reported
		// and the reader mints the terminal through the spool handler, exactly as
		// it does for a LOST conclusion. Reporting is the whole conversion: the
		// TaskStop call is dropped either way, so this record produces no entry.
		c.log.With(at.ctxFor("task-stop")).With(logging.Context{TaskID: taskID}).
			Log("TaskStop reported to owner resolution as the CANCELLED terminal of a shell run; the spool's reader mints it with the output it holds")
		c.observer.TaskStopped(taskID)
		return nil
	}
}

// wholeStdout wraps output the producer knows to be complete.
func wholeStdout(output string) *conversationv1.AgentBashOutput {
	return spoolOutput(output, 0)
}

// spoolOutput wraps a spool's accumulated bytes. ALWAYS SET, even for a command
// that said nothing: an empty output still draws its header, so a reader can tell
// "ran and was silent" from "has not run".
//
// THE EXTENT IS STATED, NEVER ASSUMED. A producer that kept only part of what a
// run said says so on the partial arm with the count it dropped, rather than
// claiming `whole` over a prefix — a consumer offered "whole" has no way to find
// out it was lied to.
func spoolOutput(output string, omitted uint64) *conversationv1.AgentBashOutput {
	text := &conversationv1.AgentBashOutputText{Stdout: output}
	if omitted == 0 {
		text.Extent = &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}}
	} else {
		text.Extent = &conversationv1.AgentBashOutputText_Partial{Partial: &conversationv1.AgentBashOutputPartial{
			BytesOmitted: omitted,
		}}
	}
	return &conversationv1.AgentBashOutput{
		Form: &conversationv1.AgentBashOutput_Text{Text: text},
	}
}
