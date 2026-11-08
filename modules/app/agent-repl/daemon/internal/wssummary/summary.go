// Package wssummary keeps the workspace summary the topbar draws: the one
// imperative sentence a workspace's own agent sets to state its task
// (agentrepl.v1.SetWorkspaceSummary).
//
// THE AGENT OWNS THE SUMMARY; THE DAEMON DISTILLS NOTHING. The keeper stores
// what the agent set, installs it in the topbar, draws it again at every
// registration so a restarted daemon shows it at once, and drops it at a
// /clear, which ends the conversation it described.
//
// NO SUMMARY IS A SUMMARY OWED. While none stands — a new workspace, or one
// whose /clear dropped it — every user's or agent's prompt to the workspace
// is closed by the BriefOwed note asking the agent to set one (OwedNote). The absence of the stored row IS the owed state, so the
// two can never disagree.
package wssummary

import (
	"context"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/textline"
)

// BriefOwed is the note a prompt to a workspace with no summary closes with.
const BriefOwed = "workspace-summary-owed"

// The bounds of a summary, in words.
const (
	MinWords = 2
	MaxWords = 15
)

// The keeper's operations, one per logical branch.
const (
	opRestore = "daemon.wssummary.restore"
	opSet     = "daemon.wssummary.set"
	opReset   = "daemon.wssummary.context_reset"
	opOwed    = "daemon.wssummary.owed"
)

// clauseMarks are the marks a summary never carries: each joins what would
// otherwise be several clauses, and a summary is one plain sentence.
var clauseMarks = []string{",", ";", ":", "(", ")", "—", "–", " - "}

// Validate answers why SUMMARY is not one the title line draws: not one line
// holding one sentence of MinWords to MaxWords words (textline.OneSentence),
// or carrying a clause-joining mark. Nil for a summary the title line draws.
func Validate(summary string) error {
	if err := textline.OneSentence("summary", summary, MinWords, MaxWords); err != nil {
		return err
	}
	for _, mark := range clauseMarks {
		if strings.Contains(summary, mark) {
			return fmt.Errorf("a summary is one plain sentence, so it carries no %q joining clauses", strings.TrimSpace(mark))
		}
	}
	return nil
}

// Store keeps each workspace's summary (wsm).
type Store interface {
	// PutWorkspaceSummary records the summary, replacing any it had.
	PutWorkspaceSummary(ctx context.Context, ws ids.WorkspaceID, summary string) error
	// DropWorkspaceSummary deletes the summary, reporting whether one stood.
	DropWorkspaceSummary(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// WorkspaceSummaryOf loads the summary; the bool is false when none
	// stands.
	WorkspaceSummaryOf(ctx context.Context, ws ids.WorkspaceID) (string, bool, error)
}

// Topbar installs the summary the title line draws.
type Topbar interface {
	// SetSummary installs the summary; empty retracts it.
	SetSummary(ws ids.WorkspaceID, summary string)
}

// Deps are the keeper's collaborators.
type Deps struct {
	// Store keeps the summaries.
	Store Store
	// Topbar draws them.
	Topbar Topbar
	// PromptsDir is where BriefOwed is read from AT USE TIME, so an edit
	// takes effect without a daemon bounce.
	PromptsDir string
	// Log is the keeper's canonical logger.
	Log dlog.Logger
}

// Keeper keeps every workspace's summary.
type Keeper struct {
	deps Deps
}

// New builds a keeper. It panics on a missing collaborator, because a
// half-wired keeper is a boot defect, not a runtime condition.
func New(deps Deps) *Keeper {
	if deps.Store == nil || deps.Topbar == nil || deps.Log == nil || deps.PromptsDir == "" {
		panic("wssummary: New requires Store, Topbar, PromptsDir and Log")
	}
	return &Keeper{deps: deps}
}

// Restore installs the workspace's stored summary into the topbar. The daemon
// calls it at the workspace's registration, so a restart draws the summary at
// once, however long the session stays down. A failed read is recorded and
// returned.
func (k *Keeper) Restore(ctx context.Context, ws ids.WorkspaceID) error {
	fields := dlog.Context{"workspace": string(ws)}
	summary, found, err := k.deps.Store.WorkspaceSummaryOf(ctx, ws)
	if err != nil {
		k.deps.Log.Error(opRestore, "the stored workspace summary could not be read", dlog.WithCause(fields, err))
		return fmt.Errorf("read the summary of %q: %w", ws, err)
	}
	if !found {
		k.deps.Log.Debug(opRestore, "the workspace has no summary to restore; it owes one", fields)
		return nil
	}
	k.deps.Topbar.SetSummary(ws, summary)
	k.deps.Log.Debug(opRestore, "restored the workspace summary", fields)
	return nil
}

// Set records SUMMARY as the workspace's and installs it in the topbar. The
// summary's format is the caller's to have validated (Validate); a write that
// fails is recorded and returned, with the topbar left on the summary it had.
func (k *Keeper) Set(ctx context.Context, ws ids.WorkspaceID, summary string) error {
	fields := dlog.Context{"workspace": string(ws), "summary": summary}
	if err := k.deps.Store.PutWorkspaceSummary(ctx, ws, summary); err != nil {
		k.deps.Log.Error(opSet, "the workspace summary could not be recorded", dlog.WithCause(fields, err))
		return fmt.Errorf("record the summary of %q: %w", ws, err)
	}
	k.deps.Topbar.SetSummary(ws, summary)
	k.deps.Log.Info(opSet, "the workspace set its summary", fields)
	return nil
}

// OnContextReset drops the workspace's summary at a /clear, which ends the
// conversation it described, and retracts it from the topbar: the workspace
// owes a new one, which its next prompt asks for. A drop that fails is
// recorded and leaves the summary standing.
func (k *Keeper) OnContextReset(ws ids.WorkspaceID) {
	fields := dlog.Context{"workspace": string(ws)}
	dropped, err := k.deps.Store.DropWorkspaceSummary(context.Background(), ws)
	if err != nil {
		k.deps.Log.Error(opReset, "the summary a /clear ended could not be dropped; it stands until the workspace sets another", dlog.WithCause(fields, err))
		return
	}
	k.deps.Topbar.SetSummary(ws, "")
	fields["dropped"] = dropped
	k.deps.Log.Info(opReset, "a /clear ended the summarized conversation; the workspace owes a new summary", fields)
}

// OwedNote answers the meta-wrapped BriefOwed note a prompt to the workspace
// closes with while it has no summary standing, and false when one stands. A
// read or a brief that fails is recorded and returned, so a prompt is never
// sent without the note it owes.
func (k *Keeper) OwedNote(ctx context.Context, ws ids.WorkspaceID) (string, bool, error) {
	fields := dlog.Context{"workspace": string(ws), "brief": BriefOwed}
	_, found, err := k.deps.Store.WorkspaceSummaryOf(ctx, ws)
	if err != nil {
		k.deps.Log.Error(opOwed, "could not read whether the workspace owes a summary", dlog.WithCause(fields, err))
		return "", false, fmt.Errorf("read the summary of %q: %w", ws, err)
	}
	if found {
		return "", false, nil
	}
	note, err := k.note()
	if err != nil {
		k.deps.Log.Error(opOwed, "could not compose the note asking for a summary", dlog.WithCause(fields, err))
		return "", false, err
	}
	k.deps.Log.Debug(opOwed, "the workspace owes a summary; the prompt asks for one", fields)
	return prompts.Wrap("\n\n" + note), true, nil
}

// note reads and splices BriefOwed.
func (k *Keeper) note() (string, error) {
	brief, err := prompts.Load(k.deps.PromptsDir, BriefOwed)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefOwed, err)
	}
	note, err := brief.Splice(nil)
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefOwed, err)
	}
	return note, nil
}
