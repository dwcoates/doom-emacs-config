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

// THE COMMAND LINE IS NOT KNOWN HERE, SO IT IS LEFT UNSET. A spool terminal is
// minted from bytes on disk and a run handle; the command that produced them is
// not in the spool, and shim-store's detached_work row holds THE JOIN AND
// NOTHING ELSE (handle, kind, origin unit, owner, terminals) — never the line.
// AgentBashSuccess.command is a RESTATEMENT of what was run, so filling it with
// the vendor TASK id would have this producer assert a command nobody ran. The
// field is optional precisely for this case: the origin unit's own call carries
// the true line, and the daemon fills a blank terminal from it rather than the
// reverse.

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

// BashTail converts the run's rendered output so far into its tail frame: the
// most recent bytes, bounded by conversation.v1 AgentBashTailCap, and how many
// bytes and lines came before them.
//
// A SNAPSHOT THAT SUPERSEDES THE LAST WHOLE, under the run's one tail key, so
// the store holds exactly what a reader is drawn and nothing before it. There
// is no offset: the gap detector the delta model needed is retired, because a
// snapshot that states its own omitted count cannot have a hole.
func (c *Converter) BashTail(at Attribution, run, text string, bytesOmitted, linesOmitted uint64) *storev1.StoreEntry {
	c.log.With(at.ctxFor("bash-tail")).With(logging.Context{
		ActivityID: run, UpsertKey: BashTailKey(run), Offset: logging.Off(at.Offset),
	}).LogVerbose("spool tail bytes=%d bytes_omitted=%d lines_omitted=%d", len(text), bytesOmitted, linesOmitted)
	return BashRun(at, "bash_tail", BashTailKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Tail{Tail: &conversationv1.AgentBashTail{
			Text:         text,
			BytesOmitted: bytesOmitted,
			LinesOmitted: linesOmitted,
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
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: spoolOutput(output, omitted),
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: int32(code)}},
				},
			}},
		}},
	})
}

// BashKilled converts the spool's `[killed]` terminator into the run's terminal.
//
// A KILL IS AN ENDING, NOT A LOSS. The wrapper wrote the line, so the run's own
// file says it ended — which is why this is the COMPLETED arm carrying the
// `killed` termination and never `interrupted{lost}`: we did not stop seeing
// it, we read how it stopped. It is not `by_user` either: the wrapper's line
// names no actor, and a run the harness killed for its own reasons is not a
// person's decision.
func (c *Converter) BashKilled(at Attribution, run, output string, omitted uint64) *storev1.StoreEntry {
	c.log.With(at.ctxFor("bash-killed")).With(logging.Context{ActivityID: run, UpsertKey: BashTerminalKey(run)}).
		Log("[killed] observed on disk; the run ends on evidence rather than on a silence timeout, with no status the shell reported")
	return BashRun(at, "bash_terminal", BashTerminalKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: spoolOutput(output, omitted),
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Killed{Killed: &conversationv1.AgentBashKilled{}},
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
func (c *Converter) BashLost(at Attribution, run, output string, omitted uint64, reason LostReason, observed, catchup bool) *storev1.StoreEntry {
	bound := c.log.With(lostCtx(at, "bash-lost", catchup, reason)).With(logging.Context{
		ActivityID: run, UpsertKey: BashTerminalKey(run), Reason: string(reason),
	})
	line := "the detached run is LOST (%s); it resolves interrupted with cause=lost naming that arm, output_observed=%t"
	if catchup {
		bound.LogVerbose(line, reason, observed)
	} else {
		bound.Log(line, reason, observed)
	}
	return BashLostEntry(at, run, output, omitted, reason, observed)
}

// lostCtx picks the rung a LOST run's terminal is recorded on, and it is the
// SAME classification the policy one layer up already made.
//
// A STARTUP CATCH-UP CONCLUSION IS NOT A NEW EVENT. A restarted sidecar
// re-derives every historical run from the spools still on disk — 1,189 of them
// sat unterminated under the temporary root on 2026-09-12 — and concludes each
// one LOST again, with the store absorbing the re-emitted terminal on its stable
// upsert key. stale.Tracker.state already refuses to state those one by one and
// rolls them into ONE informational summary per class for exactly that reason;
// this record used to be written at WARN regardless, so the flood came straight
// back under a different operation name and realtest 5's harvest was 161
// `bash-lost` warnings out of 475 records, every one of them backlog. A catch-up
// item is therefore recorded on the DEBUG rung — the same rung the policy's own
// per-item catch-up statement uses, and the totals still ride the INFO summary,
// so nothing is silenced.
//
// A conclusion reached about a run that VANISHED while we watched is a
// newly-arising condition and stays at WARN, which is the whole point of keeping
// the two apart.
//
// WENT_SILENT IS THE ONE REASON THAT IS NOT A FAULT, and it is recorded at INFO
// for the reason stale.Tracker.state gives at the layer above: the file plane
// cannot tell a quiet dead run from a quiet live one — no pid is written, no
// heartbeat, and a terminator is the only end the vendor states — so a silence
// window expiring asks an operator to act on something that may need nothing.
// The two layers classify identically ON PURPOSE; a `bash-lost` warn under an
// info `lost-policy` would put the flood straight back one level down, which is
// exactly how the catch-up flood came back before it.
func lostCtx(at Attribution, operation string, catchup bool, reason LostReason) logging.Context {
	if catchup {
		ctx := at.ctxFor(operation)
		ctx.Level = "debug"
		return ctx
	}
	if reason == LostWentSilent {
		return at.ctxFor(operation)
	}
	return at.ctxWarn(operation)
}

// BashLostEntry is BashLost without a converter, for the staleness policy in the
// root package, which owns its own logging and holds no per-file converter.
//
// THE WRITE IDENTITY IS STABLE FOR THE VERDICT: a run is lost once however many
// sweeps observe it, so a re-emission is absorbed at the store rather than
// appending a second terminal.
func BashLostEntry(at Attribution, run, output string, omitted uint64, reason LostReason, observed bool) *storev1.StoreEntry {
	return BashRun(at, "bash_terminal", BashTerminalKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				// A run concluded from the ABSENCE of a file states
				// not_observed: we do not know what it printed, and "it printed
				// nothing" would put words in its mouth.
				Output: terminalOutput(output, omitted, observed),
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
func (c *Converter) BashCancelled(at Attribution, run, output string, omitted uint64, settledAtMs int64, observed bool) *storev1.StoreEntry {
	c.log.With(at.ctxFor("bash-cancelled")).With(logging.Context{ActivityID: run, UpsertKey: BashTerminalKey(run)}).
		Log("the detached run was stopped by a person; it resolves interrupted with cause=by_user, output_observed=%t carrying %d byte(s)", observed, len(output))
	return BashRun(at, "bash_terminal", BashTerminalKey(run), run, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			// NO START IS RESTATED: this is the detached run's own terminal,
			// and its start instant rides the run's own start frame.
			SettledAt: settledAt(settledAtMs, 0),
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				// A stop for a run whose spool was never readable states
				// not_observed: the stop is evidence about the PERSON's
				// decision, never about what the command printed.
				Output: terminalOutput(output, omitted, observed),
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
		// BENIGN UNATTRIBUTABLE — debug, not warn. A TaskStop that names no task
		// carries nothing to attribute the stop to; it is classified whole as
		// residue, which is the correct outcome, not data loss. It recurs across
		// history and would flood a cold re-scan's strict harvest at warn. The
		// classification stays; only the severity drops. (An agent TaskStop that
		// DOES name a task but whose launch this stream never opened stays warn
		// below — that is a pointier "expected-but-absent launch" signal.)
		c.log.With(at.ctxFor("task-stop")).
			LogVerbose("TaskStop result names no task; the stop cannot be attributed and the record is classified as vendor_specific residue")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "task_stop/unattributed", result)}
	}

	// BOTH SPELLINGS THE VENDOR USES FOR AN AGENT TASK. The captured stop record
	// (testdata/corpus/tool-results/task_stop.jsonl) states `task_type:
	// "local_agent"`, which is what the harness writes for a spawned Agent;
	// "agent" is kept because it is the spelling the earlier ruling was written
	// against. Matching only one of them routed a real agent stop into the SHELL
	// branch below, where it was reported to a spool that does not exist and the
	// spawn unit was never settled at all.
	switch taskType {
	case "agent", "local_agent":
		// THE SPAWN UNIT IS KEYED BY THE CALL THAT SPAWNED IT, never by the
		// vendor task id: the unit being settled is the Agent CALL in this
		// stream's book, and the task id names the harness's bookkeeping for it.
		run, launched := c.spawnedRuns[taskID]
		if !launched {
			// AN ABSENT LAUNCH IS ONLY A SIGNAL IF THE LAUNCH COULD HAVE BEEN
			// SEEN, and there are TWO ways it could not have been.
			//
			// THE LAUNCH WAS WRITTEN TO A DIFFERENT FILE. The vendor's
			// background agents outlive the transcript that launched them: a
			// `/clear` rotates the session id and opens a new file while the
			// harness keeps every running agent, so the first thing the new
			// file says about such a run is the stop that settles it. The
			// vendor states where the launch lives — the run's spool sits under
			// the LAUNCHING session's directory — and foreignspawn.go reads
			// that off the notifications this stream did carry.
			//
			// THE LAUNCH LIES BEHIND THIS READER'S CURSOR. A converter that
			// RESUMED mid-file has no way to have seen a launch before its
			// window; that is the ordinary shape of a restart.
			//
			// A converter that read the file FROM BYTE 0, with no foreign owner
			// on record, IS looking at a gap and keeps the warning. The residue
			// is identical in all three cases; only the severity moves, and the
			// record names which case it is.
			switch owner, foreign := c.foreignSpawns[taskID]; {
			case foreign:
				c.log.With(at.ctxFor("task-stop")).With(logging.Context{TaskID: taskID}).
					LogVerbose("TaskStop names an agent task session %s launched, so its launch was written to that session's transcript and never to this one; the spawn unit it settles cannot be identified here and the record is classified as vendor_specific residue", owner)
			case c.resumedMidFile():
				c.log.With(at.ctxFor("task-stop")).With(logging.Context{TaskID: taskID, Offset: logging.Off(c.joinedOffset)}).
					LogVerbose("TaskStop names an agent task whose launch lies before this reader joined the file, so the spawn unit it settles cannot be identified here and the record is classified as vendor_specific residue")
			default:
				c.log.With(at.ctxWarn("task-stop")).With(logging.Context{TaskID: taskID}).
					Log("TaskStop names an agent task no launch on this stream opened; the spawn unit it settles cannot be identified and the record is classified as vendor_specific residue")
			}
			return []*storev1.StoreEntry{VendorSpecificEntry(at, "task_stop/unlaunched", result)}
		}
		c.log.With(at.ctxFor("task-stop")).With(logging.Context{TaskID: taskID, ActivityID: run, UpsertKey: ActivityKey(run)}).
			Log("TaskStop consumed as the CANCELLED terminal of an agent task")
		c.observer.TaskConcluded(taskID)
		activity := item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
				Cause: &conversationv1.AgentSubagentFailure_StoppedByUser{StoppedByUser: &conversationv1.AgentSubagentStoppedByUser{}},
				// RESTATED so the stopped spawn a replay serves alone still
				// draws its label and addresses its sub-feed: the commission
				// its launch recorded, and the created agent the minting rule
				// names (the spawning call itself).
				Prompt:         c.spawnPrompt(run),
				CreatedAgentId: agentID(run),
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

// unobservedOutput states that the producer HOLDS NO BYTES for this run.
//
// IT IS A THIRD THING, not a spelling of empty. `text{stdout: "", whole{}}`
// asserts that the command printed nothing — a claim about the COMMAND — and
// `partial{bytes_omitted: 0}` asserts that nothing was cut, which is a claim
// about the CARRIER. A run swept up at boot, or one whose spool was never
// readable, supports neither: we do not know what it printed, and saying "it
// printed nothing" would put words in its mouth. `not_observed` is the arm that
// says exactly that, and the producer owes the reader the distinction.
func unobservedOutput() *conversationv1.AgentBashOutput {
	return &conversationv1.AgentBashOutput{
		Form: &conversationv1.AgentBashOutput_NotObserved{
			NotObserved: &conversationv1.AgentBashOutputNotObserved{},
		},
	}
}

// terminalOutput picks the arm a terminal owes: what the run said when the
// producer READ it, and `not_observed` when it never did.
func terminalOutput(output string, omitted uint64, observed bool) *conversationv1.AgentBashOutput {
	if !observed {
		return unobservedOutput()
	}
	return spoolOutput(output, omitted)
}

// SubagentLost converts a BACKGROUNDED SUBAGENT we stopped being able to see
// into its spawn unit's settled state.
//
// THE SPAWN UNIT IS THE LINE THAT GOES OPEN FOREVER. A backgrounded agent is
// delivered through an `a*` task spool, and when that spool vanishes or goes
// quiet the only thing downstream holding the run is the SPAWNING CALL's unit in
// the parent's book — `activity:<tool_use_id>`. Without this the reader
// concluded LOST, said so in its log, and settled nothing: the spawn drew as
// still running in every consumer for the life of the store.
//
// THE CAUSE IS `lost`, AND THE ARM IS HOW WE CONCLUDED IT.
// AgentSubagentFailure.cause.lost carries the same DetachedLost the shell run's
// terminal carries (landing 3), so the two detached kinds state the SAME fact
// the SAME way. It is deliberately not `stopped_by_user`: that names a decision,
// and no decision was observed.
//
// The failure carries no `error`: we observed no error, only silence, and
// inventing one would have this producer assert the run died.
func (c *Converter) SubagentLost(at Attribution, run, ownerAgent string, reason LostReason, catchup bool) *storev1.StoreEntry {
	bound := c.log.With(lostCtx(at, "subagent-lost", catchup, reason)).With(logging.Context{
		ActivityID: run, UpsertKey: ActivityKey(run), BookAgentID: ownerAgent, Reason: string(reason),
	})
	line := "the backgrounded subagent is LOST (%s); its spawn unit settles failed with cause=lost naming that arm"
	if catchup {
		bound.LogVerbose(line, reason)
	} else {
		bound.Log(line, reason)
	}
	return SubagentLostEntry(at, run, ownerAgent, reason, c.spawnPrompt(run))
}

// SubagentLostEntry is SubagentLost without a converter, so a caller that holds
// no per-file converter can mint the same entry.
//
// THE WRITE IDENTITY IS STABLE FOR THE VERDICT, exactly as the bash terminal's
// is: a run is lost once however many sweeps observe it, so a re-emission is
// absorbed at the store rather than appending a second settle.
//
// THE LOSS IS A SETTLED FRAME AND RESTATES THE SPAWN: `prompt` is the
// commission the launch recorded (empty when the caller never saw it), and the
// created agent is the run itself by the minting rule.
func SubagentLostEntry(at Attribution, run, ownerAgent string, reason LostReason, prompt *conversationv1.AgentSubagentPrompt) *storev1.StoreEntry {
	activity := item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
		Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
			Cause:          &conversationv1.AgentSubagentFailure_Lost{Lost: DetachedLostArm(reason)},
			Prompt:         prompt,
			CreatedAgentId: agentID(run),
		}},
	}})
	activity.ActivityId = activityID(run)
	return PageLine(at, "settle:"+run, ActivityKey(run), ownerAgent, activityFrame(ownerAgent, activity))
}
