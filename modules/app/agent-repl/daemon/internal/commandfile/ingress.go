package commandfile

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/intakegate"
	"claude-repld/internal/merge"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// The operation names this package's records carry.
const (
	opRun        = "daemon.commandfile.run"
	opApply      = "daemon.commandfile.apply_file"
	opClaim      = "daemon.commandfile.claim"
	opQuarantine = "daemon.commandfile.quarantine"
	opEntry      = "daemon.commandfile.entry"
	opGate       = "daemon.commandfile.gate"
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
	entries, err := parse(data)
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
	for index, entry := range entries {
		if err := i.apply(ctx, log, base, index, entry); err != nil {
			// A refused entry is HANDLED — recorded here and retired to
			// quarantine below — so it is a warning, not an unhandled error.
			log.Warn(opEntry, "a command-file entry was refused", dlog.Context{
				"index": index, "type": entry.Type, "cause": err.Error(),
			})
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
		log.Warn(opQuarantine, "quarantining a command file whose entries were refused", dlog.Context{
			"entries": len(entries), "refused": len(failures), "cause": joined.Error(),
		})
		if qErr := i.quarantine(claimed); qErr != nil {
			log.Error(opQuarantine, "could not quarantine the command file", dlog.Context{"cause": qErr.Error()})
			return errors.Join(joined, qErr)
		}
		return fmt.Errorf("apply %q: %w: %w", path, ErrQuarantined, joined)
	}
	log.Info(opApply, "applied a command file", dlog.Context{"entries": len(entries)})
	return nil
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
	if err := os.MkdirAll(i.deps.QuarantineDir, 0o755); err != nil {
		return fmt.Errorf("create %q: %w", i.deps.QuarantineDir, err)
	}
	return os.Rename(claimed, filepath.Join(i.deps.QuarantineDir, filepath.Base(claimed)))
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
		// A COMMAND-FILE MERGE IS AN AGENT'S ASK, made from inside its own
		// turn, so it never displaces the turn in flight (merge.Requester).
		return i.deps.Merge.Enqueue(ctx, ws, merge.RequestedByAgent)
	case TypeClose:
		ws, err := i.target(ctx, entry)
		if err != nil {
			return err
		}
		return i.deps.Verbs.Close(ctx, ws)
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

// applyCreate maps a create entry onto the ordinary creation verb. A one-shot
// create from this channel carries no finish field because NO one-shot does:
// what happens on completion is the repository's own directive, appended to the
// commission by the daemon and carried out by the agent.
func (i *ingress) applyCreate(ctx context.Context, entry Entry) error {
	spec := workspace.CreateSpec{
		RepoDir:       entry.GitRoot,
		InitialPrompt: entry.Prompt,
		Name:          entry.Name,
		BaseRef:       entry.BaseRef,
		OneShot:       entry.OneShot,
	}
	_, err := i.deps.Verbs.Create(ctx, spec)
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
