package commandfile

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/intakegate"
	"agent-repld/internal/merge"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// The operation names this package's records carry.
const (
	opRun        = "daemon.commandfile.run"
	opApply      = "daemon.commandfile.apply_file"
	opClaim      = "daemon.commandfile.claim"
	opQuarantine = "daemon.commandfile.quarantine"
	opEntry      = "daemon.commandfile.entry"
	opGate       = "daemon.commandfile.gate"
	// opMergeQueue is the outcome record of a merge-queue control entry
	// (merge_evict, merge_move_up, merge_move_down, merge_pause, merge_resume,
	// merge_hold_rollouts, merge_release_rollouts, merge_subscribe,
	// merge_unsubscribe): the answer the requesting
	// agent reads back, keyed by the file's path like every other record here.
	opMergeQueue = "daemon.commandfile.merge_queue"
)

// ErrQuarantined marks the error a malformed file yields: the file was warned
// about and moved aside, so the failure is already reported where it happened.
var ErrQuarantined = errors.New("commandfile: the file was quarantined")

// ErrClaimedElsewhere marks a file that was gone before this ingress could
// claim it: another daemon on the same state root (a handover's other half,
// finishing a sweep it began) claimed it first. The rename is the claim, so
// losing it is the exclusivity working, never a fault.
var ErrClaimedElsewhere = errors.New("commandfile: the file was claimed by another sweeper")

// commandFileOrigin is the prompt origin every command-file prompt carries: the
// channel is a host-written file, not a composer, and the origin says so on the
// durable turn record.
const commandFileOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_LEGACY_HOST_PROMPT

// ingress is the command-file ingress. It owns no verb of its own: every entry
// is mapped onto THE SAME internal path as the equivalent rpc, so there is
// never a second implementation of a verb to keep in step.
type ingress struct {
	deps Deps
	// gate answers whether this daemon takes the intake at all.
	gate *intakegate.Gate
}

// Run polls the ingress directory until ctx is cancelled.
//
// The directory is POLLED rather than watched: it holds small files written by
// shell scripts, a poll cannot miss one the way a dropped inotify watch can,
// and the poll interval doubles as the settling window a half-written file is
// judged against.
func (i *ingress) Run(ctx context.Context) error {
	log := i.deps.Log.Global().With(dlog.Context{"dir": i.deps.Dir})
	ticker := time.NewTicker(i.deps.Interval)
	defer ticker.Stop()
	log.Info(opRun, "watching the command-file ingress", dlog.Context{
		"glob": i.deps.Glob, "interval_ms": i.deps.Interval.Milliseconds(),
	})
	for {
		if err := i.sweep(ctx, log); err != nil {
			// A sweep that fails is logged and retried: the directory is an
			// ingress, and one unreadable pass is not a reason to stop
			// draining it.
			log.Error(opRun, "a sweep of the ingress failed", dlog.Context{"cause": err.Error()})
		}
		select {
		case <-ctx.Done():
			log.Info(opRun, "stopped watching the command-file ingress", nil)
			return ctx.Err()
		case <-ticker.C:
		}
	}
}

// sweep applies every claimable file once, oldest name first so a producer that
// drops two files gets them in the order it wrote them.
//
// ONLY THE DAEMON THAT SERVES SWEEPS (intakegate): a joining successor and an
// incumbent whose handover has begun apply nothing, because the verbs and the
// prompt body the entries map onto act on workspaces such a daemon does not
// serve.
func (i *ingress) sweep(ctx context.Context, log dlog.Logger) error {
	if !i.gate.Admits() {
		return nil
	}
	matches, err := filepath.Glob(filepath.Join(i.deps.Dir, i.deps.Glob))
	if err != nil {
		return fmt.Errorf("glob %q: %w", i.deps.Glob, err)
	}
	sort.Strings(matches)
	for _, path := range matches {
		if settled, err := i.settled(path); errors.Is(err, os.ErrNotExist) {
			log.Debug(opRun, "a command file was gone before it was judged; another sweeper claimed it", dlog.Context{"path": path})
			continue
		} else if err != nil {
			log.Warn(opRun, "could not judge whether a command file has settled", dlog.Context{
				"path": path, "cause": err.Error(),
			})
			continue
		} else if !settled {
			log.Debug(opRun, "leaving a command file that is still being written", dlog.Context{"path": path})
			continue
		}
		switch err := i.ApplyFile(ctx, path); {
		case err == nil:
		case errors.Is(err, ErrQuarantined):
			log.Debug(opRun, "a malformed command file was quarantined", dlog.Context{
				"path": path, "cause": err.Error(),
			})
		case errors.Is(err, ErrClaimedElsewhere):
			log.Debug(opRun, "a command file was claimed by another sweeper", dlog.Context{"path": path})
		default:
			log.Error(opRun, "a command file did not apply", dlog.Context{"path": path, "cause": err.Error()})
		}
	}
	return nil
}

// settled reports whether a file may be claimed. A file older than one poll
// interval has settled by age; a YOUNGER one has settled only if it already
// parses as a complete document, which is what keeps a half-written file — one
// still mid-token — out of the daemon.
func (i *ingress) settled(path string) (bool, error) {
	info, err := os.Stat(path)
	if err != nil {
		return false, err
	}
	if i.deps.Now().Sub(info.ModTime()) >= i.deps.Interval {
		return true, nil
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return false, err
	}
	if _, err := parse(data); err != nil {
		return false, nil
	}
	return true, nil
}

// ApplyFile claims one command file, applies it, and retires it.
//
// The claim is a RENAME into the claimed directory, which is atomic and
// exclusive: two daemons sweeping one ingress cannot both apply a file, and a
// producer cannot rewrite a file out from under an application in progress.
//
// A file that does not parse APPLIES NOTHING and is quarantined with a WARNING:
// the array is one request, and half of it is not a smaller request.
func (i *ingress) ApplyFile(ctx context.Context, path string) error {
	log := i.deps.Log.Global().With(dlog.Context{"path": path})

	claimed, err := i.claim(path)
	if errors.Is(err, os.ErrNotExist) {
		log.Debug(opClaim, "the command file was gone before it was claimed; another sweeper claimed it", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("claim %q: %w: %w", path, ErrClaimedElsewhere, err)
	}
	if err != nil {
		log.Error(opClaim, "could not claim the command file", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("claim %q: %w", path, err)
	}
	log.Debug(opClaim, "claimed the command file", dlog.Context{"claimed": claimed})

	data, err := os.ReadFile(claimed)
	if err != nil {
		log.Error(opApply, "could not read the claimed command file", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("read %q: %w", claimed, err)
	}
	entries, err := decode(data, i.deps.Home)
	if err != nil {
		log.Warn(opQuarantine, "quarantining a malformed command file", dlog.Context{"cause": err.Error()})
		if qErr := i.quarantine(claimed); qErr != nil {
			log.Error(opQuarantine, "could not quarantine the command file", dlog.Context{"cause": qErr.Error()})
			return errors.Join(err, qErr)
		}
		// The file IS handled: it was warned about and retired to
		// quarantine. The error still reaches a direct caller of ApplyFile,
		// but the sweep recognizes the sentinel and does not report the same
		// file a second time at ERROR on a path that has nothing left to do.
		return fmt.Errorf("parse %q: %w: %w", path, ErrQuarantined, err)
	}

	base := filepath.Base(path)
	var failures []error
	// faulted is whether any failure was a FAULT rather than an answer; it
	// sets the level the file's quarantine is recorded at.
	faulted := false
	for index, entry := range entries {
		if err := i.apply(ctx, log, base, index, entry); err != nil {
			// A refused entry is HANDLED — recorded here and retired to
			// quarantine below. A TYPED REFUSAL is the verb's ANSWER to what
			// the command asked (a temporary repository, an unknown
			// workspace, an unmergeable branch), so it is recorded at INFO;
			// anything else is a failure to act and stays a warning.
			fields := dlog.Context{"index": index, "type": entry.Type, "cause": err.Error()}
			if answered(err) {
				log.Info(opEntry, "a command-file entry was refused", fields)
			} else {
				faulted = true
				log.Warn(opEntry, "a command-file entry failed", fields)
			}
			failures = append(failures, fmt.Errorf("entry %d (%s): %w", index, entry.Type, err))
			continue
		}
		log.Debug(opEntry, "applied a command-file entry", dlog.Context{"index": index, "type": entry.Type})
	}
	if len(failures) > 0 {
		// AN APPLY-TIME REFUSAL IS THE FILE ROUTE'S QUARANTINE (daemon.md,
		// batch-1 triage: "file route: quarantine"). The rpc route answers a
		// refusal to the caller who made it; a file has no caller to answer,
		// so the refusal is recorded and the file is retired where a human can
		// still read what was asked. Left in the claimed directory it would be
		// invisible — neither applied, nor swept again, nor anywhere a person
		// would look.
		joined := errors.Join(failures...)
		fields := dlog.Context{"entries": len(entries), "refused": len(failures), "cause": joined.Error()}
		if faulted {
			log.Warn(opQuarantine, "quarantining a command file whose entries failed", fields)
		} else {
			log.Info(opQuarantine, "quarantining a command file whose entries were refused", fields)
		}
		if qErr := i.quarantine(claimed); qErr != nil {
			log.Error(opQuarantine, "could not quarantine the command file", dlog.Context{"cause": qErr.Error()})
			return errors.Join(joined, qErr)
		}
		return fmt.Errorf("apply %q: %w: %w", path, ErrQuarantined, joined)
	}
	// AN APPLIED FILE IS RETIRED TO THE APPLIED DIRECTORY, so its fate is a
	// fact on disk a producer can read: claimed is "being applied", applied
	// and quarantine are the two ends. `agent-repld merge-queue` reads it,
	// because an agent's merge request reports nothing else before the
	// requesting turn ends.
	if err := i.retire(claimed, i.deps.AppliedDir); err != nil {
		log.Error(opApply, "applied a command file but could not retire it to the applied directory", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("retire applied %q: %w", claimed, err)
	}
	log.Info(opApply, "applied a command file", dlog.Context{"entries": len(entries)})
	return nil
}

// answered reports whether an entry's failure is a TYPED REFUSAL -- the
// verb's or the merge orchestrator's answer to what the command asked, the
// same refusals the rpc route answers on an arm -- rather than a fault.
func answered(err error) bool {
	if _, ok := workspace.AsRefusal(err); ok {
		return true
	}
	// A SESSION PARKED AT ITS COLD GATE refuses a prompt by the gate's own
	// name: the queue's answer, which the rpc route answers on its cold_gate
	// arm.
	var coldGate *promptqueue.ColdGateRefusal
	if errors.As(err, &coldGate) {
		return true
	}
	_, ok := merge.Refused(err)
	return ok
}

// claim renames the file into the claimed directory. The rename is the claim:
// nothing else marks a file as taken, so there is no window in which two
// sweepers both believe they own it.
func (i *ingress) claim(path string) (string, error) {
	if err := os.MkdirAll(i.deps.ClaimedDir, 0o755); err != nil {
		return "", fmt.Errorf("create %q: %w", i.deps.ClaimedDir, err)
	}
	claimed := filepath.Join(i.deps.ClaimedDir, filepath.Base(path))
	if err := os.Rename(path, claimed); err != nil {
		return "", err
	}
	return claimed, nil
}

// quarantine moves a malformed file aside so the sweep does not meet it again
// and a human can still read what was written.
func (i *ingress) quarantine(claimed string) error {
	return i.retire(claimed, i.deps.QuarantineDir)
}

// retire moves a claimed file into the directory that names its end.
func (i *ingress) retire(claimed, dir string) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("create %q: %w", dir, err)
	}
	return os.Rename(claimed, filepath.Join(dir, filepath.Base(claimed)))
}

// apply maps ONE entry onto the same internal path as the equivalent rpc.
func (i *ingress) apply(ctx context.Context, log dlog.Logger, file string, index int, entry Entry) error {
	switch entry.Type {
	case TypeCreate:
		return i.applyCreate(ctx, entry)
	case TypePrompt, TypeSend:
		return i.applyPrompt(ctx, file, index, entry)
	case TypeMerge:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		source, err := i.mergeSource(ctx, ws, entry)
		if err != nil {
			return err
		}
		// Validate already refused a malformed increment, so this reads it.
		if source.Version, err = entry.VersionIncrement(); err != nil {
			return err
		}
		// A COMMAND-FILE MERGE IS AN AGENT'S ASK, made from inside its own
		// turn: it is put in line once that turn ends, and never displaces
		// the turn in flight (merge.Requester).
		var detached *merge.DetachedRun
		if entry.AsyncName != "" {
			closes, err := i.closeOnLanding(ctx, ws, entry)
			if err != nil {
				return err
			}
			detached = merge.NewDetachedRun(entry.AsyncName, entry.Instruction, entry.CompactAfter, closes, entry.CleanupWorktree, entry.MergeActions())
		}
		return i.deps.Merge.Enqueue(ctx, merge.Request{Workspace: ws, Source: source, By: merge.RequestedByAgent, Detached: detached, OneShot: entry.OneShot})
	case TypeMergeEvict:
		return i.applyMergeEvict(ctx, log, entry)
	case TypeMergeMoveUp, TypeMergeMoveDown:
		return i.applyMergeMove(ctx, log, entry)
	case TypeMergePause, TypeMergeResume:
		return i.applyMergePause(ctx, log, entry)
	case TypeMergeHoldRollouts, TypeMergeReleaseRollouts, TypeMergeDisableRacing, TypeMergeEnableRacing:
		return i.applyDaemonWideSwitch(ctx, log, entry)
	case TypeMergeSubscribe, TypeMergeUnsubscribe:
		return i.applyMergeSubscription(ctx, log, entry)
	case TypeRetarget:
		return i.applyRetarget(ctx, log, entry)
	case TypeClose:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		return i.deps.Verbs.Close(ctx, ws, wsm.CloseByAgent)
	case TypeForget:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		return i.deps.Verbs.Forget(ctx, ws)
	case TypeOpen:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		return i.deps.Verbs.Open(ctx, ws, nil)
	case TypeSwitch:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		return i.deps.Verbs.Select(ctx, ws)
	case TypeTaskCreate:
		task, err := i.deps.Verbs.CreateTask(ctx, entry.Title)
		if err != nil {
			return err
		}
		log.Debug(opEntry, "created a task from a command file", dlog.Context{"task": string(task.ID)})
		return nil
	case TypeTaskToggleDone:
		return i.deps.Verbs.UpdateTask(ctx, ids.TaskID(entry.ID), wsm.TaskChange{Done: entry.Done})
	case TypeTaskAddWorkspace:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		task := ids.TaskID(entry.ID)
		return i.deps.Verbs.AssignTask(ctx, ws, &task)
	default:
		// Validate already refused every unknown type, so reaching here means
		// the two disagree — which is a defect, not a bad input.
		return fmt.Errorf("entry type %q passed validation but has no mapping", entry.Type)
	}
}

// mergeSource reads a merge entry's source, and whether it asked for the
// front of its queue, which any source may.
func (i *ingress) mergeSource(ctx context.Context, requester ids.WorkspaceID, entry Entry) (wsm.MergeSource, error) {
	source, err := i.mergeSourceArm(ctx, requester, entry)
	if err != nil {
		return wsm.MergeSource{}, err
	}
	source.Front = entry.Front
	return source, nil
}

// mergeSourceArm reads a merge entry's source: an async merge's named branch,
// or a one-shot's own branch, already merged upstream when pr_was_merged says
// so.
func (i *ingress) mergeSourceArm(_ context.Context, _ ids.WorkspaceID, entry Entry) (wsm.MergeSource, error) {
	switch {
	case entry.AsyncName != "":
		return wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: strings.TrimSpace(entry.Branch)}, nil
	case entry.PRWasMerged:
		return wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, nil
	}
	return wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, nil
}

// closeOnLanding resolves an async entry's close_dir to the workspace
// registered there, empty when the entry names none. A directory no
// workspace is registered at is the contract's unknown_one_shot_workspace
// refusal.
func (i *ingress) closeOnLanding(ctx context.Context, requester ids.WorkspaceID, entry Entry) (ids.WorkspaceID, error) {
	if entry.CloseDir == "" {
		return "", nil
	}
	if i.deps.DB == nil {
		return "", fmt.Errorf("this entry names a workspace to close and the ingress has no state client to resolve it with")
	}
	record, err := i.deps.DB.WorkspaceByDir(ctx, entry.CloseDir)
	if err != nil {
		return "", &merge.RefusalError{Arm: merge.ArmUnknownOneShotWorkspace, Workspace: requester,
			Reason: fmt.Sprintf("no workspace is registered at %q: %v", entry.CloseDir, err)}
	}
	return record.ID, nil
}

// applyMergeEvict maps a merge_evict entry onto the orchestrator's Evict, the
// entry point UpdateMergeQueue's evict arm calls. The requester's own merge is
// evicted unless evict_dir names another workspace's.
//
// A WORKSPACE WITH NOTHING ON THE QUEUE IS AN ANSWER, NOT A FAILURE. The rpc
// answers it as `no_such_queued_merge` to an operator who asked for a change;
// an agent asking to take a merge out is satisfied when none is in, so the
// file is applied and the outcome record says nothing was queued.
func (i *ingress) applyMergeEvict(ctx context.Context, log dlog.Logger, entry Entry) error {
	requester, evicted, err := i.queueSubject(ctx, entry, "evict_dir", entry.EvictDir)
	if err != nil {
		return err
	}
	outcome := dlog.Context{
		"type": entry.Type, "requester": string(requester), "workspace": string(evicted),
		"own": evicted == requester,
	}
	err = i.deps.Merge.Evict(ctx, evicted)
	if answeredNotQueued(log, err, outcome, "nothing to evict: the workspace has no merge on the queue") {
		return nil
	}
	if err != nil {
		return err
	}
	outcome["outcome"] = "evicted"
	log.Info(opMergeQueue, "evicted the workspace's merge from the queue", outcome)
	return nil
}

// queueSubject resolves a queue control's two workspaces: the REQUESTER the
// entry targets, and the SUBJECT whose merge it addresses -- the requester's
// own unless DIR (the entry's FIELD) names another workspace's worktree.
func (i *ingress) queueSubject(ctx context.Context, entry Entry, field, dir string) (requester, subject wsm.WorkspaceID, err error) {
	requester, err = i.target(ctx, entry)
	if err != nil {
		return "", "", err
	}
	if dir == "" {
		return requester, requester, nil
	}
	if i.deps.DB == nil {
		return "", "", fmt.Errorf("this entry names a %s and the ingress has no state client to resolve it with", field)
	}
	record, err := i.deps.DB.WorkspaceByDir(ctx, dir)
	if err != nil {
		return "", "", fmt.Errorf("unknown_workspace: no workspace is registered at %s %q: %w", field, dir, err)
	}
	return requester, record.ID, nil
}

// answeredNotQueued records ERR as the `not_queued` outcome when it is the
// orchestrator's no_such_queued_merge refusal, answering whether it was: a
// queue control addressing a merge that is not there is satisfied, not
// failed. Any other ERR is left to the caller.
func answeredNotQueued(log dlog.Logger, err error, outcome dlog.Context, message string) bool {
	refusal, ok := merge.Refused(err)
	if !ok || refusal.Arm != merge.ArmNoSuchQueuedMerge {
		return false
	}
	outcome["outcome"] = "not_queued"
	log.Info(opMergeQueue, message, outcome)
	return true
}

// applyMergeMove maps a merge_move_up or merge_move_down entry onto the
// orchestrator's Move, the entry point UpdateMergeQueue's move arms call. The
// requester's own merge moves unless move_dir names another workspace's.
//
// A MERGE THAT DOES NOT MOVE IS AN ANSWER, NOT A FAILURE, exactly as an evict
// of nothing is: a merge with no waiting merge to pass is applied with the
// outcome `unmoved`, and a workspace with nothing on the queue with
// `not_queued`.
func (i *ingress) applyMergeMove(ctx context.Context, log dlog.Logger, entry Entry) error {
	requester, subject, err := i.queueSubject(ctx, entry, "move_dir", entry.MoveDir)
	if err != nil {
		return err
	}
	move := wsm.MergeMoveUp
	if entry.Type == TypeMergeMoveDown {
		move = wsm.MergeMoveDown
	}
	outcome := dlog.Context{
		"type": entry.Type, "requester": string(requester), "workspace": string(subject),
		"own": subject == requester, "move": move.String(),
	}
	moved, err := i.deps.Merge.Move(ctx, subject, move)
	if answeredNotQueued(log, err, outcome, "nothing to move: the workspace has no merge on the queue") {
		return nil
	}
	if err != nil {
		return err
	}
	if !moved {
		outcome["outcome"] = "unmoved"
		log.Info(opMergeQueue, "the merge has no waiting merge to pass that way, and stays where it is", outcome)
		return nil
	}
	outcome["outcome"] = "moved_" + move.String()
	log.Info(opMergeQueue, "moved the workspace's merge one place in its queue", outcome)
	return nil
}

// applyMergePause maps a merge_pause or merge_resume entry onto the
// orchestrator's Pause or Unpause, the entry points UpdateMergeQueue's pause
// and resume arms call, scoped the way the rpc scopes them: an unset
// repository_dir is every repository. A refusal (already_paused, not_paused,
// unknown_repository) is the rpc's refusal, and quarantines the file.
func (i *ingress) applyMergePause(ctx context.Context, log dlog.Logger, entry Entry) error {
	requester, err := i.target(ctx, entry)
	if err != nil {
		return err
	}
	scope := entry.queueScope()
	outcome := dlog.Context{
		"type": entry.Type, "requester": string(requester), "repository_dir": entry.RepositoryDir,
		"scoped": scope != nil,
	}
	if entry.Type == TypeMergePause {
		if err := i.deps.Merge.Pause(ctx, scope); err != nil {
			return err
		}
		outcome["outcome"] = "paused"
		log.Info(opMergeQueue, "paused the merge queue", outcome)
		return nil
	}
	if err := i.deps.Merge.Unpause(ctx, scope); err != nil {
		return err
	}
	outcome["outcome"] = "resumed"
	log.Info(opMergeQueue, "resumed the merge queue", outcome)
	return nil
}

// daemonWideSwitch is one daemon-wide merge-queue switch entry type: the
// orchestrator verb its UpdateMergeQueue arm calls, the outcome the requesting
// agent reads back, and the record that says so.
type daemonWideSwitch struct {
	apply   func(merge.Orchestrator, context.Context) error
	outcome string
	message string
}

// daemonWideSwitches maps each daemon-wide switch entry type onto its switch.
var daemonWideSwitches = map[string]daemonWideSwitch{
	TypeMergeHoldRollouts: {apply: merge.Orchestrator.HoldRollouts, outcome: "held",
		message: "held the merge queue's rollouts: merges still land, but a landing no longer redeploys"},
	TypeMergeReleaseRollouts: {apply: merge.Orchestrator.ReleaseRollouts, outcome: "released",
		message: "released the merge queue's rollouts: the next landing redeploys"},
	TypeMergeDisableRacing: {apply: merge.Orchestrator.DisableRacing, outcome: "racing_disabled",
		message: "disabled the merge queue's racing: every queue works its head alone"},
	TypeMergeEnableRacing: {apply: merge.Orchestrator.EnableRacing, outcome: "racing_enabled",
		message: "enabled the merge queue's racing: on AC power each queue works its top two merges"},
}

// applyDaemonWideSwitch maps a daemon-wide switch entry onto its orchestrator
// verb, the entry point its UpdateMergeQueue arm calls. The switch is
// daemon-wide, so the entry's workspace only names who asked. A refusal (the
// flip changes nothing) is the rpc's refusal, and quarantines the file.
func (i *ingress) applyDaemonWideSwitch(ctx context.Context, log dlog.Logger, entry Entry) error {
	sw, ok := daemonWideSwitches[entry.Type]
	if !ok {
		panic(fmt.Sprintf("commandfile: %s is dispatched as a daemon-wide switch but names none", entry.Type))
	}
	requester, err := i.target(ctx, entry)
	if err != nil {
		return err
	}
	if err := sw.apply(i.deps.Merge, ctx); err != nil {
		return err
	}
	log.Info(opMergeQueue, sw.message, dlog.Context{"type": entry.Type, "requester": string(requester), "outcome": sw.outcome})
	return nil
}

// applyMergeSubscription maps a merge_subscribe or merge_unsubscribe entry
// onto the orchestrator's Subscribe or Unsubscribe, the entry points
// UpdateMergeQueue's subscribe and unsubscribe arms call, scoped the way the
// rpc scopes them: an unset repository_dir is every repository. The
// requester is the subscriber: an agent subscribes its own workspace.
//
// AN UNSUBSCRIBE OF NOTHING IS AN ANSWER, NOT A FAILURE, exactly as an evict
// of nothing is: the rpc answers it as `not_subscribed`, and an agent asking
// to stop being told is satisfied when it was not, so the file is applied
// with that outcome. Any other refusal (subscriber_closed,
// unknown_repository) quarantines the file.
func (i *ingress) applyMergeSubscription(ctx context.Context, log dlog.Logger, entry Entry) error {
	requester, err := i.target(ctx, entry)
	if err != nil {
		return err
	}
	scope := entry.queueScope()
	outcome := dlog.Context{
		"type": entry.Type, "requester": string(requester), "repository_dir": entry.RepositoryDir, "scoped": scope != nil,
	}
	if entry.Type == TypeMergeSubscribe {
		// Validate already refused an undeclared detail, so this reads it.
		detail, err := wsm.ParseMergeQueueDetail(entry.Detail)
		if err != nil {
			return err
		}
		outcome["detail"] = detail.String()
		replaced, err := i.deps.Merge.Subscribe(ctx, requester, scope, detail)
		if err != nil {
			return err
		}
		outcome["outcome"] = "subscribed"
		if replaced {
			outcome["outcome"] = "resubscribed"
		}
		log.Info(opMergeQueue, "subscribed the requester's agent to the merge queue's changes", outcome)
		return nil
	}
	err = i.deps.Merge.Unsubscribe(ctx, requester, scope)
	if refusal, ok := merge.Refused(err); ok && refusal.Arm == merge.ArmNotSubscribed {
		outcome["outcome"] = "not_subscribed"
		log.Info(opMergeQueue, "nothing to unsubscribe: the requester holds no merge queue subscription in that scope", outcome)
		return nil
	}
	if err != nil {
		return err
	}
	outcome["outcome"] = "unsubscribed"
	log.Info(opMergeQueue, "ended the requester's merge queue subscription", outcome)
	return nil
}

// applyRetarget maps a retarget entry onto the retarget operation
// SetWorkspaceParent runs, so the file's retarget tells the workspace's
// session exactly as the rpc's does. The parent is keyed by its worktree
// root; null, or the repository's own main checkout, is top-level.
func (i *ingress) applyRetarget(ctx context.Context, log dlog.Logger, entry Entry) error {
	ws, err := i.target(ctx, entry)
	if err != nil {
		return err
	}
	parent, err := i.retargetParent(ctx, log, ws, entry.Parent.Dir)
	if err != nil {
		return err
	}
	return i.deps.Retarget.Retarget(ctx, ws, parent)
}

// retargetParent answers the parent a retarget's directory names: none for no
// directory or for the workspace's repository's main checkout, else the
// registered workspace there. A directory nothing is registered at is the
// unknown_parent refusal; a parent of another repository is left to the
// retarget, which refuses it by name.
func (i *ingress) retargetParent(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, dir string) (*ids.WorkspaceID, error) {
	if dir == "" {
		return nil, nil
	}
	record, err := i.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the retargeted workspace %q: %w", ws, err)
	}
	repositories, err := i.deps.DB.ListRepositories(ctx)
	if err != nil {
		return nil, fmt.Errorf("read the repository registry: %w", err)
	}
	repo, err := workspaceRepository(repositories, record, record.Dir)
	if err != nil {
		return nil, err
	}
	parent, main, err := i.parentAt(ctx, dir, repo)
	var unregistered *unregisteredParentError
	if errors.As(err, &unregistered) && errors.Is(err, wsm.ErrNotFound) {
		return nil, workspace.UnknownParentRefusal(log, dir, err)
	}
	if err != nil {
		return nil, fmt.Errorf("parent: %w", err)
	}
	if main {
		return nil, nil
	}
	return &parent.ID, nil
}

// applyCreate maps a create entry onto the ordinary creation verb. A one-shot
// create from this channel carries no finish field because NO one-shot does:
// what happens on completion is the repository's own directive, appended to the
// commission by the daemon and carried out by the agent.
func (i *ingress) applyCreate(ctx context.Context, entry Entry) error {
	spec, err := i.createSpec(ctx, entry)
	if err != nil {
		return err
	}
	_, err = i.deps.Verbs.Create(ctx, spec)
	return err
}

// applyPrompt maps a prompt entry onto SubmitPrompt's own body, so a
// command-file prompt is recognized, mirrored and queued exactly as a typed one
// is. The idempotency key is the file and the entry's index, so a file dropped
// twice cannot run one prompt twice.
func (i *ingress) applyPrompt(ctx context.Context, file string, index int, entry Entry) error {
	ws, err := i.target(ctx, entry)
	if err != nil {
		return err
	}
	key := fmt.Sprintf("%s:%d", file, index)
	_, err = i.deps.Prompts.Submit(ctx, ws, workspace.SaidText(entry.Prompt), key, commandFileOrigin, wsm.DeliveryOrdinary, nil)
	return err
}

// target resolves an entry's workspace. An entry naming BOTH an id and a dir
// goes through the verbs' own ref resolution, so the command-file channel is
// held to the same dir-mismatch refusal as the wire.
func (i *ingress) target(ctx context.Context, entry Entry) (ids.WorkspaceID, error) {
	// THE DIRECTORY IS THE KEY, whenever the entry carries one. `workspace`
	// beside it is the producer's display name and is never resolved: reading
	// it as an id is what refused every skill-dispatched merge.
	if dir := entry.TargetDir(); dir != "" {
		if i.deps.DB == nil {
			return "", fmt.Errorf("this entry names a directory and the ingress has no state client to resolve it with")
		}
		record, err := i.deps.DB.WorkspaceByDir(ctx, dir)
		if err != nil {
			return "", fmt.Errorf("no workspace is registered at %q: %w", dir, err)
		}
		// A `workspace` THAT IS ANOTHER WORKSPACE'S ID IS STILL A MISMATCH. A
		// display name resolves to nothing and is ignored, as the contract
		// says; but this module's own producers write an ID there, and an id
		// that names a DIFFERENT workspace than the directory does is a
		// request nobody can act on without guessing which half was meant.
		if entry.Workspace != "" && entry.Workspace != string(record.ID) {
			if other, err := i.deps.Verbs.Resolve(ctx, &workspacev1.WorkspaceRef{Id: entry.Workspace}); err == nil && other.ID != record.ID {
				return "", fmt.Errorf("the entry's directory %q is workspace %q, but its workspace field names the id of %q", dir, record.ID, other.ID)
			}
		}
		return record.ID, nil
	}
	record, err := i.deps.Verbs.Resolve(ctx, &workspacev1.WorkspaceRef{Id: entry.Workspace})
	if err != nil {
		return "", err
	}
	return record.ID, nil
}
