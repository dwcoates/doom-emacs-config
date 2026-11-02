package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
)

const opCreationCatalog = "daemon.workspace.creation_catalog"

// The arms a create naming a model the creating login does not know is
// refused with (agentrepl.v1.CreateWorkspaceError).
const (
	// ArmModelNotInCatalog refuses a model that is not an option of the
	// creating login's catalog. It carries `model`, `config_dir` and the
	// catalog's `options`.
	ArmModelNotInCatalog = "model_not_in_catalog"
	// ArmModelCatalogUnknown refuses any model on a login no session has
	// served a catalog for. It carries `model` and `config_dir`.
	ArmModelCatalogUnknown = "model_catalog_unknown"
)

// CreationCatalog is what the daemon knows of the models a new workspace in a
// repository may be created under (agentrepl.v1.ListCreationModels).
type CreationCatalog struct {
	// ConfigDir is the account root (login) the new workspace spends as.
	ConfigDir string
	// Options is the catalog the last session on that login served, in the
	// vendor's order. Meaningful only when Known.
	Options []*conversationv1.ModelOption
	// Known is false when no session on the login has served a catalog yet.
	Known bool
}

// Names answers the options' model names, the strings a create may name.
func (c CreationCatalog) Names() []string {
	names := make([]string, 0, len(c.Options))
	for _, option := range c.Options {
		names = append(names, option.GetModel().GetName())
	}
	return names
}

// CreationCatalog answers the login a create in REPO_DIR spends as and the
// catalog that login last served.
//
// ONE LOGIN FOR THE LIST AND THE CHECK. The login is the path routing of the
// directory the repository's new worktrees are made in (WorktreeParent).
// Every new worktree is a direct child of it, and routing is directory
// containment, so it answers the login every new workspace's own directory
// routes to. The listing and checkCreateModel both read it here, so the
// picker never offers a model the create refuses.
func (v *verbs) CreationCatalog(ctx context.Context, repoDir string) (CreationCatalog, error) {
	log := v.deps.Log.Global().With(dlog.Context{"repo_dir": repoDir})
	parent, err := WorktreeParent(repoDir)
	if err != nil {
		log.Error(opCreationCatalog, "could not resolve where the repository's worktrees are made", dlog.Context{"cause": err.Error()})
		return CreationCatalog{}, fmt.Errorf("resolve the worktree parent of %q: %w", repoDir, err)
	}
	configDir := v.deps.Accounts.ConfigDirFor(parent)
	catalog, known, err := v.deps.DB.AccountModelCatalog(ctx, configDir)
	if err != nil {
		log.Error(opCreationCatalog, "could not read the login's model catalog", dlog.Context{
			"config_dir": configDir, "cause": err.Error(),
		})
		return CreationCatalog{}, fmt.Errorf("read the model catalog of login %q: %w", configDir, err)
	}
	log.Debug(opCreationCatalog, "resolved the creating login's model catalog", dlog.Context{
		"config_dir": configDir, "known": known, "options": len(catalog.GetOptions()),
	})
	return CreationCatalog{ConfigDir: configDir, Options: catalog.GetOptions(), Known: known}, nil
}

// checkCreateModel refuses a create whose requested model the creating login
// does not know, before anything is named or built. A create naming no model
// runs under the daemon's default and is never refused here.
func (v *verbs) checkCreateModel(ctx context.Context, log dlog.Logger, spec CreateSpec, repoDir string) error {
	if spec.Model == "" {
		return nil
	}
	catalog, err := v.CreationCatalog(ctx, repoDir)
	if err != nil {
		return fmt.Errorf("create: check the model %q: %w", spec.Model, err)
	}
	if !catalog.Known {
		return refuseWith(log, "CreateWorkspace", ArmModelCatalogUnknown,
			fmt.Sprintf("the model %q cannot be checked: no session on login %q has served a model catalog", spec.Model, catalog.ConfigDir),
			false, map[string]any{"model": spec.Model, "config_dir": catalog.ConfigDir})
	}
	if contains(catalog.Names(), spec.Model) {
		return nil
	}
	options := make([]proto.Message, 0, len(catalog.Options))
	for _, option := range catalog.Options {
		options = append(options, option)
	}
	return refuseWith(log, "CreateWorkspace", ArmModelNotInCatalog,
		fmt.Sprintf("the model %q is not in the catalog of login %q %v", spec.Model, catalog.ConfigDir, catalog.Names()),
		false, map[string]any{"model": spec.Model, "config_dir": catalog.ConfigDir, "options": options})
}
