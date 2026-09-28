package workspace

import (
	"context"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// RequestCommandSupport answers the add-support offer on an unsupported slash
// command's refusal card: it composes the brief from
// prompts/add-support-slash-command.md and creates a SUPPORT WORKSPACE through
// the ORDINARY standard creation form with that brief as its initial prompt.
//
// There is no second creation path — the support workspace is an ordinary one,
// so everything the standard form guarantees (layout facts, registration after
// materialization, the queue's delivery) holds for it too.
//
// A missing brief is LOUD: the workspace is not created with an empty prompt,
// because an agent with nothing to investigate would sit idle in a worktree
// nobody asked for.
func (v *verbs) RequestCommandSupport(ctx context.Context, ws ids.WorkspaceID, command string) (wsm.Workspace, error) {
	record, log, err := v.owned(ctx, "RequestCommandSupport", ws)
	if err != nil {
		return wsm.Workspace{}, err
	}

	command = strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(command), "/"))
	if command == "" {
		return wsm.Workspace{}, refuse(log, "RequestCommandSupport", ArmBlankCommand,
			"no command was named", false)
	}

	brief, err := v.load(v.deps.PromptsDir, BriefAddSupport)
	if err != nil {
		log.Error(opCommandSupport, "could not read the add-support brief", dlog.Context{
			"brief": BriefAddSupport, "cause": err.Error(),
		})
		v.raisePromptsFault(ctx, log, fmt.Sprintf("the %s brief is unreadable: %v", BriefAddSupport, err))
		return wsm.Workspace{}, refuse(log, "RequestCommandSupport", ArmBriefMissing,
			fmt.Sprintf("the %s brief is unreadable: %v", BriefAddSupport, err), false)
	}
	prompt, err := v.splice(brief, map[string]string{
		"command":     command,
		"config_root": v.deps.Accounts.ConfigDirFor(record.Dir),
	})
	if err != nil {
		log.Error(opCommandSupport, "could not splice the add-support brief", dlog.Context{
			"brief": BriefAddSupport, "cause": err.Error(),
		})
		v.raisePromptsFault(ctx, log, fmt.Sprintf("the %s brief will not splice: %v", BriefAddSupport, err))
		return wsm.Workspace{}, refuse(log, "RequestCommandSupport", ArmBriefMissing,
			fmt.Sprintf("the %s brief will not splice: %v", BriefAddSupport, err), false)
	}

	// THE FAULT CLOSES HERE. The brief just read and spliced, so the
	// condition prompts_dir_missing records — this daemon's prompts directory
	// cannot furnish a brief — no longer holds. A fault that only ever opens
	// is not a record of a condition; it is a one-way trip.
	v.clearPromptsFault(ctx, log)

	repoDir, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opCommandSupport, "could not resolve the repository to create in", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("request command support for %q: %w", command, err)
	}

	log.Info(opCommandSupport, "creating a support workspace", dlog.Context{
		"command": command, "repo_dir": repoDir,
	})
	created, err := v.Create(ctx, CreateSpec{
		RepoDir:       repoDir,
		InitialPrompt: prompt,
		// The name is the daemon's, not the brief's: a slug derived from a
		// thousand-word brief would name the brief rather than the command.
		Name: "support-" + command,
	})
	if err != nil {
		return wsm.Workspace{}, fmt.Errorf("request command support for %q: %w", command, err)
	}
	return created, nil
}

// raisePromptsFault records a prompts directory that cannot furnish a brief as
// a DAEMON-SCOPED FAULT, so DaemonHealth answers unhealthy rather than healthy.
//
// A brief_missing refusal is not one caller's bad luck: every composed brief
// this daemon owes — the merge briefs, the one-shot finish hook, this one —
// reads from the same directory, so the refusal is EVIDENCE ABOUT THE DAEMON.
// The refusal still travels to the caller; the fault is what makes the
// operator's health check see it.
//
// At most ONE such fault stands at a time: a second identical row would tell
// the operator nothing the first did not, and faults stay open until closed.
func (v *verbs) raisePromptsFault(ctx context.Context, log dlog.Logger, detail string) {
	standing, err := v.deps.Health.OpenFaults(ctx, wsm.FaultScope{Kind: health.KindPromptsDirMissing})
	if err != nil {
		// The read failing does not excuse leaving the fault unrecorded: it is
		// surfaced, and the fault is opened anyway.
		log.Error(opCommandSupport, "could not check for a standing prompts-directory fault",
			dlog.Context{"cause": err.Error()})
	}
	for _, f := range standing {
		if f.Workspace == nil {
			log.Debug(opCommandSupport, "a prompts-directory fault already stands",
				dlog.Context{"fault": string(f.ID)})
			return
		}
	}
	if _, err := v.deps.Health.OpenFault(ctx, wsm.Fault{
		Kind:     health.KindPromptsDirMissing,
		Detail:   detail,
		Evidence: map[string]string{"path": v.deps.PromptsDir},
		OpenedAt: v.now(),
	}); err != nil {
		log.Error(opCommandSupport, "could not record the prompts-directory fault",
			dlog.Context{"cause": err.Error()})
	}
}

// clearPromptsFault closes every standing DAEMON-SCOPED prompts-directory
// fault. It is the symmetric half of raisePromptsFault, called on the
// successful read+splice of a brief: that read IS the health probe, so the
// repair is observed by the same path the breakage was.
func (v *verbs) clearPromptsFault(ctx context.Context, log dlog.Logger) {
	health.CloseOnEdge(ctx, health.ReporterFaults(v.deps.Health), log, health.EdgePromptsDirServed,
		health.EdgeScope{DaemonOnly: true}, v.now())
}
