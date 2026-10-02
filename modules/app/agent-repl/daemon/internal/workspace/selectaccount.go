package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// SelectAccount makes a workspace's session spend as one of the account roots
// the daemon knows (owner ruling, 2026-09-13: the topbar's account cell offers
// them all and picking one switches the workspace).
//
// IT RECORDS THE CHOICE, THEN BOUNCES. The choice lives on the session row —
// `SelectedConfigDir`, which outranks the path routing at every start — and
// the switch itself is the ORDINARY RESTART: the relaunch engine spawns the
// replacement shim under the chosen root, carries the vendor transcript into
// it, and resumes there. Nothing here reimplements any of that, so the cold
// gate, the restart hold and the prompt drain all behave exactly as they do
// for a restart asked for by name.
//
// A LOGGED-OUT ROOT IS STILL HONORED. It is a root the daemon knows, the
// reader chose it deliberately, and refusing would leave them no way to log a
// known root in. The answer says the root is logged out and the caller opens
// its login flow.
func (v *verbs) SelectAccount(ctx context.Context, ws ids.WorkspaceID, configDir string) (bool, error) {
	record, log, err := v.owned(ctx, "SelectAccount", ws)
	if err != nil {
		return false, err
	}

	// THE ROOT IS VALIDATED AGAINST THE ROSTER THAT WAS SERVED, not against
	// the filesystem: the options the topbar drew came from this same roster,
	// so a root outside it is one no client could have been offered.
	roster, err := v.deps.Accounts.Roster(ctx)
	if err != nil {
		log.Error(opSelectAccount, "could not read the account roster", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("select account for %q: read the account roster: %w", ws, err)
	}
	chosen, known := findAccount(roster, configDir)
	if !known {
		return false, refuse(log, "SelectAccount", ArmUnknownAccount,
			fmt.Sprintf("%q is not one of the account roots this daemon knows", configDir), false)
	}

	session, exists, err := v.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opSelectAccount, "could not read the session record", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("select account for %q: read the session record: %w", ws, err)
	}
	if !exists {
		// A workspace registered but never brought up has no row yet. The
		// choice still has to survive to its first start, so the row is filed
		// exactly as creation files one — identity minted with the row,
		// because the host view is withheld for a row that names none.
		session = wsm.Session{Workspace: ws, HostSessionID: wsm.NewHostSessionID(), StartedAt: v.now()}
	}
	if session.SelectedConfigDir == configDir && session.ConfigDir == configDir {
		log.Info(opSelectAccount, "the workspace already spends as that account; nothing to switch",
			dlog.Context{"config_dir": configDir, "logged_in": chosen.LoggedIn})
		return chosen.LoggedIn, nil
	}
	session.SelectedConfigDir = configDir
	if err := v.deps.DB.PutSession(ctx, session); err != nil {
		log.Error(opSelectAccount, "could not record the chosen account root",
			dlog.Context{"config_dir": configDir, "cause": err.Error()})
		return false, fmt.Errorf("select account for %q: record the chosen root: %w", ws, err)
	}
	log.Info(opSelectAccount, "recorded the account root the reader chose", dlog.Context{
		"config_dir": configDir, "previous_config_dir": session.ConfigDir, "logged_in": chosen.LoggedIn,
	})

	// THE TOPBAR MOVES BEFORE THE BOUNCE DOES. The restart is asynchronous and
	// the cell's `current` mark is a durable fact, not a session fact, so
	// republishing here is what makes the click visibly land.
	if err := v.publishAccount(ctx, log, record, configDir); err != nil {
		log.Warn(opSelectAccount, "could not republish the topbar's account cell after the switch",
			dlog.Context{"cause": err.Error()})
	}
	// THE ROOT DECIDES WHICH SETTINGS FILE THE VENDOR READS, so the
	// selector's starting level is re-read from the root the session now
	// spends as.
	v.publishEffortSettings(log, record, configDir)

	if err := v.Restart(ctx, ws, false); err != nil {
		return false, err
	}
	return chosen.LoggedIn, nil
}
