package workspace

import (
	"context"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
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
		return wsm.Workspace{}, refuse(log, "RequestCommandSupport", ArmBriefMissing,
			fmt.Sprintf("the %s brief will not splice: %v", BriefAddSupport, err), false)
	}

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
