package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

const opSetDefaultModel = "daemon.workspace.set_default_model"

// builtInDefaultModel is the default of a login nobody made one for: the work
// account's WorkDefaultLaunchModel, every other login's DefaultLaunchModel.
func builtInDefaultModel(accounts account.Resolver, configDir string) string {
	if accounts.IsMultiRepo(configDir) {
		return WorkDefaultLaunchModel
	}
	return DefaultLaunchModel
}

// loginDefaultModel answers the model a NEW session on the login CONFIG_DIR
// starts under when nobody names one: the default the reader made with a
// selector row's "Make default", else the login's built-in default.
//
// ONE ANSWER FOR BOTH READERS. The session start launches under it and the
// topbar marks it, so the row drawn as the default is the model a new session
// on that login actually gets.
func loginDefaultModel(ctx context.Context, db wsm.DB, accounts account.Resolver, configDir string) (string, error) {
	chosen, found, err := db.AccountDefaultModel(ctx, configDir)
	if err != nil {
		return "", fmt.Errorf("read the default model of login %q: %w", configDir, err)
	}
	if found {
		return chosen, nil
	}
	return builtInDefaultModel(accounts, configDir), nil
}

// SetDefaultModel makes MODEL the default of the login the workspace's session
// spends as (owner ruling, 2026-10-27: "the default model update should be
// specific to the login ... for all new agents within that login").
//
// THE CATALOG IS THE CONTRACT, exactly as for SetModel: only a model the
// selector served can be made the default, so the default is always a model a
// session can start under.
//
// THE LOGIN, NOT THE WORKSPACE. The root is the one the account cell names
// (accountRoot), the choice is filed against it, and every strip on that root
// is republished: the mark moves on all of them at once. No running session
// changes model.
func (v *verbs) SetDefaultModel(ctx context.Context, ws ids.WorkspaceID, model string) error {
	record, log, err := v.owned(ctx, "SetDefaultModel", ws)
	if err != nil {
		return err
	}
	served, ok := v.deps.Cards.Models(ws)
	if !ok {
		return refuse(log, "SetDefaultModel", ArmNoSession,
			fmt.Sprintf("workspace %q has served no model catalog", ws), false)
	}
	if !contains(served, model) {
		return refuse(log, "SetDefaultModel", ArmNotInCatalog,
			fmt.Sprintf("the model %q is not in the served catalog %v", model, served), false)
	}
	configDir, err := v.accountRoot(ctx, record)
	if err != nil {
		log.Error(opSetDefaultModel, "the workspace's login could not be resolved", dlog.Context{
			"model": model, "cause": err.Error(),
		})
		return fmt.Errorf("set the default model from %q: %w", ws, err)
	}
	if err := v.deps.DB.PutAccountDefaultModel(ctx, configDir, model); err != nil {
		log.Error(opSetDefaultModel, "could not record the login's default model", dlog.Context{
			"config_dir": configDir, "model": model, "cause": err.Error(),
		})
		return fmt.Errorf("set the default model of login %q: %w", configDir, err)
	}
	v.deps.Topbar.SetLoginDefaultModel(configDir, model)
	log.Info(opSetDefaultModel, "made the model the login's default", dlog.Context{
		"config_dir": configDir, "model": model,
	})
	return nil
}
