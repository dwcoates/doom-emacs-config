package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/wsm"
)

// Register records a workspace Emacs announced. It is IDEMPOTENT by normalized
// directory: announcing the same tree twice yields the same workspace, with the
// same id, and mints nothing the second time.
//
// The facts the announcement omits are DERIVED FROM GIT rather than guessed:
// the repository is the tree's canonicalized common dir, the branch is what is
// checked out, and the parent branch is the repository's default branch. A
// directory that is not a git worktree at all is REFUSED — registering it would
// put a row in the roster that no git verb could ever act on.
func (v *verbs) Register(ctx context.Context, dir string, facts wsm.RegisterFacts) (wsm.Workspace, error) {
	record, _, err := v.register(ctx, dir, facts)
	return record, namedRefusal(err, "RegisterWorkspace")
}

// register is THE registration, and it is the only one. Both RegisterWorkspace
// and RegisterRepository run it -- the second for the repository's own main
// worktree (owner ruling, 2026-09-14) -- because one directory must not have
// two registration behaviors depending on which rpc announced it. A near-copy
// in the repository verb would have had to re-derive the naming, re-bind the
// resolvers, re-claim the serving ownership and re-decide the revival, and the
// two would have drifted the first time either changed.
//
// It answers `created` in addition to the record, which is what tells the
// repository verb whether it MINTED the workspace or adopted one the registry
// already held. Register itself has no field to put that in
// (RegisterWorkspaceSuccess carries the ref and nothing else) and drops it.
func (v *verbs) register(ctx context.Context, dir string, facts wsm.RegisterFacts) (wsm.Workspace, bool, error) {
	global := v.deps.Log.Global().With(dlog.Context{"dir": dir})

	normalized, err := normalizeDir(dir)
	if err != nil {
		global.Error(opRegister, "the announced directory cannot be normalized", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, false, fmt.Errorf("register %q: %w", dir, err)
	}
	if !IsWorktree(normalized) {
		// THE RPC IS THE CALLER'S TO NAME. This body serves two of them, so
		// it raises the refusal unnamed and each verb stamps its own through
		// namedRefusal.
		return wsm.Workspace{}, false, refuse(global, "", ArmNotAWorktree,
			fmt.Sprintf("%q is not a git worktree", normalized), false)
	}

	// THE WORKSPACE SINK IS RESOLVED ONLY ONCE THE ROW EXISTS. A record's
	// `workspace_id` is the daemon-minted id, and until RegisterWorkspace
	// mints it there is no id to attribute these derivations to: they belong
	// to the announcement, so they go to the run log with the directory on
	// them, and every record from the mint onwards goes to the workspace's
	// own sink.
	if facts.RepoDir == "" {
		// THE REPOSITORY IS ITS MAIN WORKTREE, not its common dir.
		// workspace.v1's RepositoryRef.dir is "the repository's normalized
		// main-worktree directory", and the merge geometry a top-level
		// workspace targets is that same directory: keyed by the common dir,
		// every merge ran `git -C <repo>/.git` and the self-repo comparison
		// the rollout trigger keys on could never match.
		main, err := v.deps.Git.MainWorktree(ctx, normalized)
		if err != nil {
			global.Error(opRegister, "could not resolve the repository's main worktree", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, false, fmt.Errorf("register %q: repository main worktree: %w", normalized, err)
		}
		facts.RepoDir = main
		global.Debug(opRegister, "derived the repository from git", dlog.Context{"repo_dir": main})
	}
	if facts.Branch == "" {
		branch, err := v.deps.Git.CurrentBranch(ctx, normalized)
		if err != nil {
			global.Error(opRegister, "could not resolve the checked-out branch", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, false, fmt.Errorf("register %q: current branch: %w", normalized, err)
		}
		facts.Branch = branch
		global.Debug(opRegister, "derived the branch from git", dlog.Context{"branch": branch})
	}
	if facts.ParentBranch == "" {
		parent, err := v.deps.Git.DefaultBranch(ctx, facts.RepoDir)
		if err != nil {
			global.Error(opRegister, "could not resolve the repository default branch", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, false, fmt.Errorf("register %q: default branch: %w", normalized, err)
		}
		facts.ParentBranch = parent
		global.Debug(opRegister, "derived the parent branch from git", dlog.Context{"parent_branch": parent})
	}
	if facts.DefaultBranch == "" {
		branch, err := v.deps.Git.DefaultBranch(ctx, facts.RepoDir)
		if err != nil {
			global.Error(opRegister, "could not resolve the repository default branch", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, false, fmt.Errorf("register %q: default branch: %w", normalized, err)
		}
		facts.DefaultBranch = branch
		global.Debug(opRegister, "derived the repository default branch from git", dlog.Context{
			"default_branch": branch,
		})
	}
	if facts.Name == "" {
		facts.Name = filepath.Base(normalized)
		global.Debug(opRegister, "derived the display name from the directory", dlog.Context{"name": facts.Name})
	}

	record, created, err := v.deps.DB.RegisterWorkspace(ctx, normalized, facts)
	if err != nil {
		global.Error(opRegister, "could not record the workspace", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, false, fmt.Errorf("register %q: %w", normalized, err)
	}
	log, err := v.deps.Log.Workspace(normalized)
	if err != nil {
		global.Error(opRegister, "could not resolve the workspace log sink", dlog.Context{
			"workspace": string(record.ID), "cause": err.Error(),
		})
		return wsm.Workspace{}, false, fmt.Errorf("register %q: resolve log sink: %w", normalized, err)
	}
	if created {
		log.Info(opRegister, "registered a new workspace", dlog.Context{
			"workspace": string(record.ID), "branch": record.Branch, "repo": string(record.Repo),
		})
	} else {
		log.Debug(opRegister, "the workspace was already registered", dlog.Context{
			"workspace": string(record.ID),
		})
	}
	// A REGISTERED WORKSPACE IS AN OPEN WORKSPACE. Registration is idempotent
	// by dir, and the row it answers with is the row that is already there --
	// including a row a previous CLOSE marked closed. Handing that row back
	// untouched is how announcing a directory produced a workspace with NO
	// TAB: `closed = true` is the roster's whole tab-membership rule
	// (lisp/roster.el's `agent-repl-roster-desired-tabs'), so the editor drew
	// nothing, the minted ref's landing waited on a tab that was never coming,
	// and the workspace could not even be resolved by name to close it again.
	//
	// So the announcement RE-OPENS it. Emacs announces a directory because it
	// means to hold that workspace -- `SPC TAB C-n' onboarding one, and the
	// link-up walk re-announcing the ones it already holds tabs for, which are
	// open by construction -- and the standing rule is that a workspace's
	// panels default to open whenever it comes into the editor's hands.
	//
	// It is the FLAG that is cleared here and nothing else: OpenWorkspace's
	// session bring-up stays OpenWorkspace's, because registration has never
	// spawned anything and the mount is what spawns.
	if !created && record.Closed {
		if err := v.deps.DB.SetClosed(ctx, record.ID, false); err != nil {
			log.Error(opRegister, "could not re-open the announced workspace", dlog.Context{
				"workspace": string(record.ID), "cause": err.Error(),
			})
			return wsm.Workspace{}, false, fmt.Errorf("register %q: re-open the closed workspace: %w", normalized, err)
		}
		record.Closed = false
		log.Info(opRegister, "the announcement re-opened a closed workspace", dlog.Context{
			"workspace": string(record.ID), "dir": record.Dir,
		})
	}
	// THE RESOLVERS ARE BOUND HERE. The footer, the topbar and the hold tray
	// each write their records to the workspace's own log sink and resolve
	// nothing themselves, so a frame for a workspace they were never told the
	// directory of is an invariant violation they report and cannot serve.
	// Registration is the one moment every workspace passes through.
	if err := v.bindResolvers(log, record.ID, normalized); err != nil {
		return wsm.Workspace{}, false, fmt.Errorf("register %q: %w", normalized, err)
	}
	// THE FOOTER IS PRIMED HERE, as the topbar's naming is below. The footer
	// topic is PER-WORKSPACE and empty after a daemon restart rebuilds the
	// resolver, unlike the editor-global roster that a reconnecting subscriber
	// always replays; an idle session produces no fresh live edge to publish
	// a first view, so without this prime serveTopic and the adoption
	// Republish have nothing to hand a reconnecting client and the footer
	// stays blank. Registration is the reconnect edge every workspace passes
	// through (Emacs re-announces every workspace it holds), so priming here
	// keeps the topic current. Bind first: Prime writes to the workspace's own
	// log sink, which bindResolvers just resolved.
	v.deps.Footer.Prime(record.ID)
	if err := v.publishNaming(ctx, log, record, facts.DefaultBranch); err != nil {
		return wsm.Workspace{}, false, fmt.Errorf("register %q: %w", normalized, err)
	}
	// THIS DAEMON SERVES IT from here. The claim is what a handover hands
	// over, and what tells a joining successor which workspaces are still the
	// incumbent's; a workspace nobody claimed is transferred by nobody.
	if v.deps.Instance != "" {
		if err := v.deps.DB.ClaimServing(ctx, record.ID, v.deps.Instance); err != nil {
			log.Error(opRegister, "could not claim the workspace's serving ownership", dlog.Context{
				"workspace": string(record.ID), "instance": string(v.deps.Instance), "cause": err.Error(),
			})
			return wsm.Workspace{}, false, fmt.Errorf("register %q: claim serving: %w", normalized, err)
		}
	}

	v.reviveRecordedConversation(ctx, log, record, created)

	v.republishRegistry(ctx, log, opRegister)
	return record, created, nil
}

// The session terminals registration reads. `deleted` refuses resurrection
// outright; `hibernated` is a stand-down the daemon performed ON PURPOSE and
// a prompt is what revives it, so neither is revived by an announcement.
const (
	terminalDeleted    = "deleted"
	terminalHibernated = "hibernated"
)

// reviveRecordedConversation brings a KNOWN workspace's recorded conversation
// back up when Emacs announces it again.
//
// A RELAUNCHED DAEMON MUST NEVER LOSE A CONVERSATION. Emacs re-announces every
// workspace it holds the moment the link comes up, and after a daemon restart
// that announcement is the ONLY edge a workspace whose panel is ALREADY
// mounted ever gets: the mount happened against the daemon that died, so no
// OpenWorkspace follows it, no session comes up, no watcher opens, and the
// feed serves zero rows for a conversation the store still holds whole.
//
// SPAWN ON MOUNT still stands. What is revived here is not "every registered
// workspace" but a workspace whose DURABLE RECORD NAMES A CONVERSATION: a
// first registration (`created`), a closed workspace, and a workspace that
// never had a vendor session mint nothing and spawn nothing. The revival runs
// through Sessions.Start, which is the one transcript-aware classifier, so the
// conversation RESUMES rather than starting fresh — and the resumed session's
// opening history page is what puts the store's own rows back in the feed and
// reconciles the footer to the terminal the last turn recorded.
//
// A failed revival is REPORTED, never absorbed and never fatal to the
// announcement: the roster row is a durable fact that must land regardless,
// and the workspace's next mount takes the bring-up again.
//
// AND IT IS NOT PART OF THE ANSWER. The start runs on its OWN goroutine
// through Sessions.StartDetached, because Sessions.Start takes the workspace's
// start gate and can wait behind a start that is already in flight -- the
// boot's own bring-up, on a relaunched daemon, for exactly the workspaces
// Emacs is re-announcing. Held inline, that wait was the register's: realtest
// run 2026-09-13T16:20:34 timed RegisterWorkspace out at Emacs's 10s bound for
// all three open workspaces across three daemon generations, each time for a
// roster row the daemon had already written. Everything this function decides
// before the start -- the terminals, the liveness read, the session record --
// stays on the answer's goroutine, so a workspace that is NOT revived is
// settled before the register returns and only the start itself is detached.
func (v *verbs) reviveRecordedConversation(ctx context.Context, log dlog.Logger, record wsm.Workspace, created bool) {
	if created || record.Closed {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "created || record.Closed"})
		return
	}
	if v.deps.Sessions.Live(record.ID) {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "v.deps.Sessions.Live(record.ID)"})
		return
	}
	session, exists, err := v.deps.DB.Session(ctx, record.ID)
	if err != nil {
		log.Error(opRegister, "could not read the announced workspace's session record", dlog.Context{
			"workspace": string(record.ID), "cause": err.Error(),
		})
		return
	}
	if !exists || session.VendorSessionID == "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!exists || session.VendorSessionID == \"\""})
		return
	}
	if session.Terminal != nil && (session.Terminal.Kind == terminalDeleted || session.Terminal.Kind == terminalHibernated) {
		log.Debug(opRegister, "the announced workspace's session is not revived by an announcement", dlog.Context{
			"workspace": string(record.ID), "terminal": session.Terminal.Kind,
		})
		return
	}
	log.Info(opRegister, "the announcement revives the workspace's recorded conversation", dlog.Context{
		"workspace": string(record.ID), "vendor_session_id": session.VendorSessionID,
	})
	v.deps.Sessions.StartDetached(record.ID, func(err error) {
		if err == nil {
			log.Debug(opRegister, "the announced workspace's recorded conversation is back up", dlog.Context{
				"workspace": string(record.ID), "vendor_session_id": session.VendorSessionID,
			})
			return
		}
		// A CANCELLED BRING-UP IS THE DAEMON LEAVING, not a session that
		// failed to come up, and a daemon standing down is the same answer
		// reached from the other side (the bring-up refused before it
		// spawned). Nothing is left to serve the revived conversation; the
		// next boot, or the successor, revives it from the same record.
		if why, ended := startEndedByDaemon(err); ended {
			log.Info(opRegister, "the announced workspace's revival "+why, dlog.Context{
				"workspace": string(record.ID), "vendor_session_id": session.VendorSessionID, "cause": err.Error(),
			})
			return
		}
		log.Error(opRegister, "the announced workspace's recorded conversation did not come back up", dlog.Context{
			"workspace": string(record.ID), "vendor_session_id": session.VendorSessionID, "cause": err.Error(),
		})
	})
}

// bindResolvers binds one workspace's directory on every resolver that needs
// telling. A failure is the caller's, never absorbed: a resolver that does not
// know where a workspace is cannot serve its views at all.
func (v *verbs) bindResolvers(log dlog.Logger, id wsm.WorkspaceID, dir string) error {
	binds := []struct {
		name string
		bind func(wsm.WorkspaceID, string) error
	}{
		{"footer", v.deps.Footer.SetWorkspaceDir},
		{"topbar", v.deps.Topbar.SetWorkspaceDir},
		{"holds", v.deps.Holds.SetWorkspaceDir},
	}
	for _, b := range binds {
		if err := b.bind(id, dir); err != nil {
			log.Error(opRegister, "a resolver could not be bound to the workspace's directory", dlog.Context{
				"resolver": b.name, "workspace": string(id), "cause": err.Error(),
			})
			return fmt.Errorf("bind the %s resolver to %q: %w", b.name, dir, err)
		}
	}
	log.Debug(opRegister, "bound the workspace's directory on every resolver that needs it", dlog.Context{
		"workspace": string(id),
	})
	return nil
}

// publishNaming installs the topbar's naming and account lines: both are
// DAEMON facts (the registry's name and branch, the repo-under-root account
// rule) and no vendor frame carries either, so nothing else can state them.
// Until they are installed the topbar is not complete and publishes NO view at
// all, so this is part of registration rather than of a session's bring-up.
func (v *verbs) publishNaming(ctx context.Context, log dlog.Logger, record wsm.Workspace, defaultBranch string) error {
	configDir, err := v.accountRoot(ctx, record)
	if err != nil {
		return err
	}
	if err := v.publishAccount(ctx, log, record, configDir); err != nil {
		return err
	}
	v.deps.Topbar.SetNaming(record.ID, topbar.Naming{
		Slug:          record.Name,
		Title:         record.Name,
		Branch:        record.Branch,
		DefaultBranch: defaultBranch,
		ConfigDir:     configDir,
	})
	log.Debug(opRegister, "installed the topbar's naming and account lines", dlog.Context{
		"workspace": string(record.ID), "config_dir": configDir, "default_branch": defaultBranch,
	})
	return nil
}

// accountRoot answers the root this workspace's session spends as right now:
// the reader's choice, then the root the session is filed under, then the path
// routing. It is what the topbar's account cell reports and what the session
// line names.
func (v *verbs) accountRoot(ctx context.Context, record wsm.Workspace) (string, error) {
	session, _, err := v.deps.DB.Session(ctx, record.ID)
	if err != nil {
		return "", fmt.Errorf("read the session record of %q: %w", record.ID, err)
	}
	return spawnRootFor(v.deps.Accounts, record.Dir, session), nil
}

// publishAccount installs the topbar's account cell: the root in force, and
// EVERY root the daemon knows as an option beside it, with that one marked.
//
// THE WHOLE ROSTER, ALWAYS. The cell's click is a dropdown of the login
// options (owner ruling, 2026-09-13), so the options are what makes the click
// mean anything — a cell published without them is the empty reveal the ruling
// was about.
func (v *verbs) publishAccount(ctx context.Context, log dlog.Logger, record wsm.Workspace, configDir string) error {
	roster, err := v.deps.Accounts.Roster(ctx)
	if err != nil {
		log.Error(opRegister, "the account roster could not be read", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("read the account roster: %w", err)
	}
	current, known := findAccount(roster, configDir)
	if !known {
		// A root the roster does not hold is still the root this session
		// spends from, so it is READ on its own rather than dropped: the cell
		// must never draw an account the workspace is not actually running as.
		current, err = v.deps.Accounts.Read(ctx, configDir)
		if err != nil {
			log.Error(opRegister, "the account root could not be read", dlog.Context{
				"config_dir": configDir, "cause": err.Error(),
			})
			return fmt.Errorf("read the account root %q: %w", configDir, err)
		}
	}
	options := make([]topbar.AccountOption, 0, len(roster))
	for _, acct := range roster {
		options = append(options, topbar.AccountOption{
			ConfigDir: acct.ConfigDir, Email: acct.Email, Current: acct.ConfigDir == configDir,
		})
	}
	// An EMPTY email is the logged-out arm, which the topbar draws as a
	// warning rather than a blank label: it is an answer, never a gap.
	v.deps.Topbar.SetAccount(record.ID, topbar.Account{Email: current.Email, Options: options})
	log.Debug(opRegister, "installed the topbar's account cell", dlog.Context{
		"workspace": string(record.ID), "config_dir": configDir,
		"logged_in": current.LoggedIn, "options": len(options),
	})
	return nil
}

// findAccount picks one root out of the roster.
func findAccount(roster []account.Account, configDir string) (account.Account, bool) {
	for _, acct := range roster {
		if acct.ConfigDir == configDir {
			return acct, true
		}
	}
	return account.Account{}, false
}

// BindViews binds every registered workspace whose directory exists, before
// the boot reconciliation publishes anything for it.
//
// IT WRITES NOTHING, so a joining successor's read-only handle can run it: a
// workspace whose directory is gone is passed over here, and closing its row is
// left to the boot's own missing-directory step (or PublishRegistry, for a
// successor). A stat that does not say "not exist" is never read as gone.
func (v *verbs) BindViews(ctx context.Context) error {
	global := v.deps.Log.Global()
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		global.Error(opRegister, "could not list the workspaces to bind their views", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("bind the views: list the workspaces: %w", err)
	}
	bound := 0
	for _, ws := range workspaces {
		if _, err := os.Stat(ws.Dir); errors.Is(err, fs.ErrNotExist) {
			global.Debug(opRegister, "the workspace's directory is gone; its views are not bound", dlog.Context{
				"workspace": string(ws.ID), "dir": ws.Dir,
			})
			continue
		}
		if err := v.bindResolvers(global, ws.ID, ws.Dir); err != nil {
			return fmt.Errorf("bind the views: %w", err)
		}
		bound++
	}
	global.Debug(opRegister, "bound every registered workspace's views before the boot reconciliation", dlog.Context{
		"workspaces": len(workspaces), "bound": bound,
	})
	return nil
}

// PublishRegistry publishes the roster's durable half once, from what the
// registry holds right now.
//
// It exists because the roster is EDITOR-GLOBAL and is otherwise published
// only as a side effect of a verb: a daemon that has just booted and has been
// asked for nothing yet would leave `WatchWorkspaceRoster` with no value to
// deliver, and the first client would wait for a workspace to change before it
// saw the roster at all — including the EMPTY roster, which is a roster.
func (v *verbs) PublishRegistry(ctx context.Context) error {
	log := v.deps.Log.Global()
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opRegister, "could not list the workspaces for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: list the workspaces: %w", err)
	}
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(opRegister, "could not list the repositories for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: list the repositories: %w", err)
	}
	tasks, err := v.deps.DB.Tasks(ctx)
	if err != nil {
		log.Error(opRegister, "could not list the tasks for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: list the tasks: %w", err)
	}
	current, err := v.deps.DB.Current(ctx)
	if err != nil {
		log.Error(opRegister, "could not read the current workspace for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: read the current workspace: %w", err)
	}
	// Every workspace the registry already holds is bound too: a daemon that
	// has just restarted did not run Register for the rows it inherited, and
	// an unbound resolver cannot serve their views.
	global := v.deps.Log.Global()
	defaults := make(map[wsm.RepoID]string, len(repositories))
	for _, repo := range repositories {
		defaults[repo.ID] = repo.DefaultBranch
	}
	for i := range workspaces {
		ws := &workspaces[i]
		// A WORKSPACE WHOSE WORKTREE IS GONE binds nothing: a merged or nuked
		// workspace keeps its registry row (the roster draws it under
		// recently_merged) while its directory has been removed, and its log
		// sink lives inside that directory. It is an ordinary state, not a
		// reason to refuse the whole roster -- and refusing it failed the BOOT
		// of every daemon that inherited one, a handover's successor included.
		//
		// AND AN OPEN ONE IS CLOSED HERE. The boot reconciliation closes these
		// rows (internal/boot/sequence.go closeMissingDirs), but a JOINING
		// SUCCESSOR RECONCILES NOTHING -- it takes ownership workspace by
		// workspace and never runs that step -- so this walk, which is the one
		// that publishes the opening roster, is what keeps a successor from
		// handing Emacs a live row for a directory that is not there. An
		// already-closed row is left alone, which is every merged and nuked
		// one.
		if _, err := os.Stat(ws.Dir); errors.Is(err, fs.ErrNotExist) {
			if !ws.Closed {
				if err := v.deps.DB.SetClosed(ctx, ws.ID, true); err != nil {
					global.Error(opRegister, "a workspace whose directory is gone could not be closed", dlog.Context{
						"workspace": string(ws.ID), "dir": ws.Dir, "cause": err.Error(),
					})
					return fmt.Errorf("publish the opening roster: close the missing-directory workspace %q: %w", ws.ID, err)
				}
				ws.Closed = true
				global.Warn(opRegister, "the workspace directory is gone; the workspace is closed", dlog.Context{
					dlog.KeyWorkspaceID: string(ws.ID), dlog.KeyWorkspaceDir: ws.Dir,
					"error": err.Error(),
				})
			}
			global.Debug(opRegister, "the workspace's directory is gone; its views are not bound", dlog.Context{
				"workspace": string(ws.ID), "dir": ws.Dir,
			})
			continue
		}
		if err := v.bindResolvers(global, ws.ID, ws.Dir); err != nil {
			return fmt.Errorf("publish the opening roster: %w", err)
		}
		if err := v.publishNaming(ctx, global, *ws, defaults[ws.Repo]); err != nil {
			return fmt.Errorf("publish the opening roster: %w", err)
		}
	}
	sessions, err := sessionRecords(ctx, v.deps.DB, workspaces)
	if err != nil {
		log.Error(opRegister, "could not read the session records for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: %w", err)
	}
	v.deps.Sidebar.SetRegistry(sidebarRegistry(log, workspaces, repositories, tasks, sessions, current))
	log.Debug(opRegister, "published the opening roster", dlog.Context{
		"workspaces": len(workspaces), "repositories": len(repositories), "tasks": len(tasks),
		"sessions": len(sessions),
	})
	return nil
}
