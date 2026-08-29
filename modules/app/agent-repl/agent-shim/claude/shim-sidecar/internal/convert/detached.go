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

// LostReason is HOW we concluded a detached run was lost. It rides the log, not
// the wire.
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
	c.log.With(logging.Context{Operation: "bash-delta", Path: at.Path, Task: at.TaskID}).
		LogVerbose("spool delta run=%s upsert_key=%s bytes=%d from_offset=%d", run, BashKey(run), len(output), fromOffset)
	return BashRun(at, "bash_delta:"+itoa(int(fromOffset)), BashKey(run), run, &conversationv1.AgentBash{
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
func (c *Converter) BashExited(at Attribution, run, output string, code int) *storev1.StoreEntry {
	c.log.With(logging.Context{Operation: "bash-exit", Path: at.Path, Task: at.TaskID}).
		Log("EXIT=%d observed on disk for run=%s upsert_key=%s; the run ends on evidence rather than on a silence timeout", code, run, BashKey(run))
	return BashRun(at, "bash_terminal", BashKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: at.TaskID},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: wholeStdout(output),
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: int32(code)}},
				},
			}},
		}},
	})
}

// BashLost converts a run we STOPPED BEING ABLE TO SEE into its terminal.
//
// NO CAUSE ARM IS SET. AgentBashInterrupted's causes are `by_user` and
// `timed_out`, and neither is what happened: we simply stopped observing. Setting
// either would be an accusation with no evidence, so the cause stays UNSET and
// the reason is stated loudly in the log instead.
func (c *Converter) BashLost(at Attribution, run, output string, reason LostReason) *storev1.StoreEntry {
	c.log.With(logging.Context{Operation: "bash-lost", Path: at.Path, Task: at.TaskID, Level: "warn"}).
		Log("detached run=%s upsert_key=%s is LOST (%s); it resolves interrupted with no cause because the wire carries no DetachedLost this wave", run, BashKey(run), reason)
	return BashLostEntry(at, run, output, reason)
}

// BashLostEntry is BashLost without a converter, for the staleness policy in the
// root package, which owns its own logging and holds no per-file converter.
//
// THE WRITE IDENTITY IS STABLE FOR THE VERDICT: a run is lost once however many
// sweeps observe it, so a re-emission is absorbed at the store rather than
// appending a second terminal.
func BashLostEntry(at Attribution, run, output string, reason LostReason) *storev1.StoreEntry {
	return BashRun(at, "bash_terminal", BashKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: at.TaskID},
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: wholeStdout(output),
			}},
		}},
	})
}

// taskStopTerminal consumes a TaskStop RESULT as the owning task's CANCELLED
// terminal, before the call itself is dropped.
//
// DELIBERATELY-STOPPED WORK MUST RESOLVE CANCELLED, NEVER LOST. A bash task
// becomes the run's interrupted-by-user terminal; an agent task becomes the
// spawn unit's stopped-by-user failure.
func (c *Converter) taskStopTerminal(result map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	taskID := str(pick(result, "task_id", "taskId"))
	taskType := str(pick(result, "task_type", "taskType"))
	if taskID == "" {
		c.log.With(logging.Context{Operation: "task-stop", Path: at.Path, Level: "warn"}).
			Log("TaskStop result at offset=%d names no task; the stop cannot be attributed and the record is stored as vendor_specific", at.Offset)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "task_stop/unattributed", result)}
	}

	switch taskType {
	case "agent":
		c.log.With(logging.Context{Operation: "task-stop", Path: at.Path, Task: taskID}).
			Log("TaskStop consumed as the CANCELLED terminal of agent task=%s upsert_key=%s", taskID, ActivityKey(taskID))
		activity := item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
				Cause: &conversationv1.AgentSubagentFailure_StoppedByUser{StoppedByUser: &conversationv1.AgentSubagentStoppedByUser{}},
			}},
		}})
		activity.ActivityId = activityID(taskID)
		return []*storev1.StoreEntry{c.settledEntry(at, agent, taskID, activity)}
	default:
		c.log.With(logging.Context{Operation: "task-stop", Path: at.Path, Task: taskID}).
			Log("TaskStop consumed as the CANCELLED terminal of shell run=%s upsert_key=%s", taskID, BashKey(taskID))
		return []*storev1.StoreEntry{BashRun(at, "bash_terminal", BashKey(taskID), taskID, &conversationv1.AgentBash{
			Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
				Command:   &conversationv1.AgentBashCommand{Line: taskID},
				SettledAt: settledAt(env.timestampMs),
				Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
					Output: wholeStdout(""),
					Cause:  &conversationv1.AgentBashInterrupted_ByUser{ByUser: &conversationv1.AgentBashInterruptedByUser{}},
				}},
			}},
		})}
	}
}

// wholeStdout wraps a spool's accumulated bytes. ALWAYS SET, even for a command
// that said nothing: an empty output still draws its header, so a reader can tell
// "ran and was silent" from "has not run".
func wholeStdout(output string) *conversationv1.AgentBashOutput {
	return &conversationv1.AgentBashOutput{
		Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
			Stdout: output,
			Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
		}},
	}
}
