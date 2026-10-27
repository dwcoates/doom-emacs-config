package workspace

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/titlesynth"
	"agent-repld/internal/wsm"
)

// UngatedPermissionModes are the permission modes that DISABLE the consent
// gate: the agent acts with no permission card reaching any decider at all. A
// creation asking for one of them without the consent flag is REFUSED — the
// gate is the user's, and it is never dropped by inference.
//
// `auto` is deliberately NOT one of them (ruled): it KEEPS a gate, with a
// classifier deciding each ask instead of the user, so it needs no creation
// consent.
//
// The spellings are the vendor's own, in both the camel-case form the CLI uses
// and the snake-case form the mode oneof's arm names spell, because a mode
// reaches the daemon as a bare string.
// DefaultPermissionMode is the mode a session is MINTED under when the
// creation names none (owner ruling 2026-09-14: "the default permission mode
// should be auto for the SDK/shim"). It is the mode the topbar's picker leads
// with, and the vendor's `default` is offered nowhere.
const DefaultPermissionMode = "auto"

var UngatedPermissionModes = map[string]bool{
	"bypassPermissions": true,
	"bypass":            true,
	"dontAsk":           true,
	"dont_ask":          true,
}

// The merge-layout origins, which name the verb that recorded the geometry.
const (
	// OriginCreateStandard is the ordinary creation form.
	OriginCreateStandard = "workspace.create.standard"
	// OriginCreateOneShot is the one-shot creation form.
	OriginCreateOneShot = "workspace.create.one_shot"
)

// Create materializes a new workspace and brings its session up.
//
// The order is the ruled one and is not negotiable:
//
//  1. validate the form, including the ungated-mode consent check;
//     1a. resolve and REQUIRE a one-shot's repository policy — before step 2,
//     because step 2 may SPEND A MODEL CALL. A repository that states no
//     one-shot policy is refused with no naming call made;
//  2. derive the slug (supplied name, else the initial prompt by the naming
//     rule), the branch, the worktree directory and the resolved base ref;
//  3. record the CREATION JOB — merge geometry, configured actions and
//     consent — BEFORE anything is materialized, so a crash leaves
//     evidence of what was being built rather than an unexplained worktree;
//  4. materialize the worktree through git;
//  5. REGISTER only once the worktree exists;
//  6. bring the session up — for a fork, recording first the fork it owes, which
//     the bring-up makes through the vendor before the session resumes;
//  7. submit the initial prompt through the QUEUE, with origin
//     WORKSPACE_CREATED, only after the session is up.
//
// reportCreateStage relays one stage to the spec's progress reporter, if it
// set one. A create with no reporter (the synchronous form) emits nothing.
func reportCreateStage(spec CreateSpec, stage CreateStage) {
	if spec.Progress != nil {
		spec.Progress.Stage(stage)
	}
}

func (v *verbs) create(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	global := v.deps.Log.Global().With(dlog.Context{"repo_dir": spec.RepoDir, "one_shot": spec.OneShot})

	if err := v.validateCreate(global, spec); err != nil {
		return wsm.Workspace{}, err
	}
	global = global.With(dlog.Context{"owner": ids.Optional(spec.Parent), "fork_from": ids.Optional(spec.ForkFrom),
		"requester": ids.Optional(spec.Requester), "owner_lands": spec.OwnerLands()})

	repoDir, err := normalizeDir(spec.RepoDir)
	if err != nil {
		global.Error(opCreate, "the repository directory cannot be normalized", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create: repository %q: %w", spec.RepoDir, err)
	}

	// A REPOSITORY THAT IS NOT ON DISK IS REFUSED, NOT FAILED. The registry may
	// still hold the row -- a worktree removed underneath it, a scratch
	// repository a run cleaned up -- and the roster already stops offering such
	// a repository, so reaching here means the client's roster was stale. That
	// is a refusal the client renders, on the arm the server already answers a
	// repository ref that matches nothing with.
	//
	// WITHOUT IT the create ran on to `WorktreeDir`, whose stat of
	// "<repo>/.git" failed with a bare filesystem error: an ERROR from the
	// verb and a second `the rpc failed` ERROR from the boundary, for a
	// refusal the contract has an arm for.
	if _, statErr := os.Stat(repoDir); statErr != nil {
		return wsm.Workspace{}, refuse(global, "CreateWorkspace", ArmUnknownRepository,
			fmt.Sprintf("the repository directory %q is not on disk: %v", repoDir, statErr), true)
	}

	// A REPOSITORY THE REGISTRY DOES NOT HOLD IS REFUSED HERE, at the verb, and
	// not only at the rpc boundary. A workspace whose repository is
	// unregistered is an invariant violation (owner ruling, 2026-09-13): it
	// must be impossible, and the write path is where that is decided.
	//
	// The CreateWorkspace endpoint has always resolved its RepositoryRef
	// against the registry and refused a miss on this same arm, but it is only
	// ONE of three ways into this verb. The COMMAND-FILE channel supplies a
	// bare `git_root` an agent wrote into a file (internal/commandfile), and
	// the support-workspace verb derives its dir from a record; neither passed
	// through that lookup, so a create naming any directory on disk minted a
	// brand-new repository row for it. Putting the check on the verb makes the
	// endpoint's lookup a redundancy rather than the only guard.
	//
	// It is a REFUSAL and not a mint because the roster's repository sections
	// are the create targets: a repository nothing has registered is not one
	// the user chose, and registering the directory is what puts it there.
	// A TEMPORARY REPOSITORY IS REFUSED BY NAME, ahead of the registry miss
	// it would otherwise be: the registry refuses every temporary directory
	// (owner ruling, 2026-10-06), so no such repository is registered, and a
	// command-file create naming a scratch folder is told WHY rather than
	// that nothing is registered there. The check is the registry's own.
	if err := v.deps.DB.RefuseTemporary(repoDir); err != nil {
		if refused := temporaryRefusal(global, "CreateWorkspace", err); refused != nil {
			return wsm.Workspace{}, refused
		}
		global.Error(opCreate, "could not judge whether the repository directory is temporary", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create: repository %q: %w", repoDir, err)
	}

	registered, err := v.repositoryRegisteredAt(ctx, repoDir)
	if err != nil {
		global.Error(opCreate, "could not read the repository registry", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create: repository %q: %w", repoDir, err)
	}
	if !registered {
		return wsm.Workspace{}, refuse(global, "CreateWorkspace", ArmUnknownRepository,
			fmt.Sprintf("no repository is registered at %q", repoDir), true)
	}

	// THE DERIVATION IS CHECKED BEFORE ANYTHING IS SPENT: an owner, a fork
	// source or a requester this repository does not hold is refused before
	// the naming call and before git is touched.
	if err := v.checkDerivation(ctx, global, spec, repoDir); err != nil {
		return wsm.Workspace{}, err
	}

	// A ONE-SHOT RUNS THE REPOSITORY'S OWN POLICY, so the policy is resolved
	// and required HERE — before the id is minted, before the NAMING CALL is
	// made, before the creation job is recorded, before git is touched. A
	// repository that states none is refused with nothing built and nothing
	// minted.
	//
	// THE POLICY CHECK PRECEDES `branchFor` DELIBERATELY. `branchFor` is where
	// an unnamed create spends a headless model call to mint its name, and a
	// create that is going to be refused for a missing policy must not pay for
	// one. TestCreateRefusesAOneShotWithNoPolicyBeforeSpendingANamingCall pins
	// the ordering.
	var policy prompts.Source
	if spec.OneShot {
		policy, err = v.requireOneShotPolicy(global, repoDir)
		if err != nil {
			return wsm.Workspace{}, err
		}
	}

	// The workspace id is minted HERE, before anything is named: it is the
	// creation job's key, and it is also what an unnamed, promptless create is
	// named after — there is nothing else to derive a name from, and the
	// naming rule never invents free text.
	workspaceID := wsm.NewWorkspaceID()

	// A MERGE WORKSPACE LANDS WHERE THE QUEUE DECIDED (spec.MergeOf, set by
	// merge.orchestrator.handOffDetached): its name's holder, its target, the
	// branch it lands on and its merge actions are all read off the origin,
	// and nothing is derived from its requester.
	origin := spec.MergeOf

	branch, release, err := v.branchFor(ctx, global, spec, repoDir, workspaceID, origin)
	if err != nil {
		return wsm.Workspace{}, err
	}
	// THE NAME IS CLAIMED UNTIL THE CREATE IS OVER: registered (the registry
	// holds it from then on) or failed (it is free again). See mergenames.go.
	defer release()
	worktreeDir, err := WorktreeDir(repoDir, branch)
	if err != nil {
		global.Error(opCreate, "could not derive the worktree directory", dlog.Context{
			"branch": branch, "cause": err.Error(),
		})
		return wsm.Workspace{}, fmt.Errorf("create %q: worktree directory: %w", branch, err)
	}

	defaultBranch, err := v.deps.Git.DefaultBranch(ctx, repoDir)
	if err != nil {
		global.Error(opCreate, "could not resolve the repository default branch", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: default branch: %w", branch, err)
	}
	// A WORKSPACE IS CUT FROM WHAT IT MERGES INTO. A child starts from its
	// owner's tip and inherits the owner's priority; a top-level workspace
	// starts from the primary branch's tip.
	parent := spec.parentWorkspace()
	priority := spec.Priority
	baseRef := spec.BaseRef
	if parent != nil {
		parentRecord, err := v.deps.DB.Workspace(ctx, *parent)
		if err != nil {
			global.Error(opCreate, "could not read the parent workspace", dlog.Context{"parent": string(*parent), "cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("create %q: parent workspace %q: %w", branch, *parent, err)
		}
		// A CHILD IS MADE ONLY UNDER AN OPEN PARENT (wsm/family.go). Asked
		// before the worktree is materialized, so a refused create leaves
		// nothing behind; registration checks again in its own transaction,
		// which is the guarantee. A merge workspace is no open child, so its
		// requester is not asked.
		if origin == nil {
			if err := v.requireOpenParent(ctx, global, opCreate, *parent); err != nil {
				return wsm.Workspace{}, fmt.Errorf("create %q: %w", branch, err)
			}
		}
		if baseRef == "" {
			baseRef = parentRecord.Branch
		}
		if priority == nil && origin == nil {
			priority = parentRecord.Priority
		}
	}
	if baseRef == "" {
		baseRef = defaultBranch
	}
	// A WORKSPACE CUT FROM ORIGIN starts at origin's primary branch, fetched
	// now, and lands onto the local primary branch like any primary
	// workspace.
	if spec.FromOrigin {
		if parent != nil {
			panic(fmt.Sprintf("workspace: create %q is a child of %q and cut from origin; only a primary workspace is cut from the primary branch", branch, *parent))
		}
		if err := v.deps.Git.Fetch(ctx, repoDir, originRemote); err != nil {
			return wsm.Workspace{}, refuseWith(global, "CreateWorkspace", ArmOriginFetchFailed,
				fmt.Sprintf("origin's primary branch could not be fetched in %s", repoDir), false,
				map[string]any{"detail": err.Error()})
		}
		baseRef = originRemote + "/" + defaultBranch
	}
	// A base ref that does not resolve is REFUSED before anything is recorded
	// or materialized: git would otherwise fail halfway through `worktree add`
	// and leave the creation job standing for a workspace that cannot exist.
	if _, err := v.resolveBase(ctx, global, repoDir, baseRef); err != nil {
		return wsm.Workspace{}, err
	}

	// A MERGE WORKSPACE RUNS ONLY THE HOOKS ITS REQUEST NAMED (owner
	// ruling): never its requester's configured ones.
	targetDir, actions, parentBranch := "", spec.DeployHooks, baseRef
	if origin != nil {
		targetDir, actions, parentBranch = origin.TargetDir, origin.Hooks, origin.ParentBranch
	} else {
		targetDir, err = v.mergeTargetDir(ctx, repoDir, parent)
		if err != nil {
			global.Error(opCreate, "could not resolve the merge target directory", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("create %q: merge target: %w", branch, err)
		}
	}

	jobOrigin := OriginCreateStandard
	if spec.OneShot {
		jobOrigin = OriginCreateOneShot
	}
	job := wsm.CreationJob{
		// The pre-minted workspace id lets the geometry be recorded BEFORE
		// materialization; registration mints the registry's own id, and the
		// job is re-keyed onto it below.
		Workspace: workspaceID,
		Layout: wsm.MergeLayout{
			SourceBranch: branch,
			SourceDir:    worktreeDir,
			TargetDir:    targetDir,
			Origin:       jobOrigin,
		},
		Hooks:                actions,
		BaseRef:              baseRef,
		Materialized:         false,
		OneShot:              spec.OneShot,
		InitialPrompt:        spec.InitialPrompt,
		ConsentedUngatedMode: spec.ConsentedUngatedMode,
		CreatedAt:            v.now(),
	}
	if err := v.deps.DB.PutCreationJob(ctx, job); err != nil {
		global.Error(opCreate, "could not record the creation job", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: record the creation job: %w", branch, err)
	}
	global.Debug(opCreate, "recorded the creation job before materialization", dlog.Context{
		"branch": branch, "worktree_dir": worktreeDir, "target_dir": targetDir,
		"base_ref": baseRef,
		"parent":   ids.Optional(parent),
	})

	// A MERGE WORKSPACE IS CUT AT ITS FORK HEAD, never at wherever its
	// requester's branch has moved to since: the head it records is then
	// exactly the commit it lands, which is what its requester's landing
	// names (wsm.Landing). Its base ref stays the branch, which is what the
	// handback names.
	materializeAt := baseRef
	if spec.MergeOf != nil {
		materializeAt = spec.MergeOf.ForkHead
	}
	reportCreateStage(spec, CreateStageCreatingWorktree)
	// THE WORKTREE STEP IS RECORDED AT BOTH EDGES. It is git, and git runs the
	// repository's hooks: a create once stood here for good behind a hook
	// that never returned, and the log said nothing between the name and the
	// silence (2026-10-23). A start with no end now names the stuck step, and
	// each end's duration is what a bound on the step would be measured from.
	global.Info(opCreate, "materializing the worktree", dlog.Context{
		"branch": branch, "worktree_dir": worktreeDir, "base_ref": materializeAt,
	})
	materializing := v.now()
	worktreeCtx, cancelWorktree := context.WithTimeout(ctx, v.worktreeBound)
	err = v.deps.Git.CreateWorktree(worktreeCtx, repoDir, branch, materializeAt, worktreeDir)
	overran := errors.Is(worktreeCtx.Err(), context.DeadlineExceeded)
	cancelWorktree()
	if err != nil {
		fields := dlog.Context{
			"branch": branch, "worktree_dir": worktreeDir, "cause": err.Error(),
			"duration_ms": v.now().Sub(materializing).Milliseconds(),
		}
		if overran {
			fields["bound_ms"] = v.worktreeBound.Milliseconds()
			global.Error(opCreate, "the worktree step overran its bound; git was stopped and the create fails", fields)
			return wsm.Workspace{}, fmt.Errorf("create %q: materialize %q: git ran past %s: %w", branch, worktreeDir, v.worktreeBound, err)
		}
		global.Error(opCreate, "could not materialize the worktree", fields)
		return wsm.Workspace{}, fmt.Errorf("create %q: materialize %q: %w", branch, worktreeDir, err)
	}
	global.Info(opCreate, "materialized the worktree", dlog.Context{
		"branch": branch, "worktree_dir": worktreeDir,
		"duration_ms": v.now().Sub(materializing).Milliseconds(),
	})

	// THE WORKTREE EXISTS; everything from here to the terminal outcome —
	// registration, a fork's record, the session bring-up and the
	// initial prompt's submission (steps 5-7) — is the session stage. Reported
	// before registration so a watching client stops showing the worktree line
	// the moment git is done.
	reportCreateStage(spec, CreateStageStartingSession)
	record, err := v.Register(ctx, worktreeDir, wsm.RegisterFacts{
		Name:          branch,
		Branch:        branch,
		ParentBranch:  parentBranch,
		Parent:        (*wsm.WorkspaceID)(parent),
		RepoDir:       repoDir,
		DefaultBranch: defaultBranch,
		MergeOf:       spec.MergeOf,
		OwnerLands:    spec.OwnerLands(),
		ContextFrom:   (*wsm.WorkspaceID)(spec.ForkFrom),
		RequestedBy:   (*wsm.WorkspaceID)(spec.Requester),
		// The priority rides the insert, because it decides the place the
		// workspace lands on.
		Priority: priority,
	})
	if err != nil {
		return wsm.Workspace{}, fmt.Errorf("create %q: register: %w", branch, err)
	}

	job.Workspace = record.ID
	job.Materialized = true
	if err := v.deps.DB.PutCreationJob(ctx, job); err != nil {
		global.Error(opCreate, "could not re-key the creation job onto the registered id", dlog.Context{
			"workspace": string(record.ID), "cause": err.Error(),
		})
		return wsm.Workspace{}, fmt.Errorf("create %q: record the materialized creation job: %w", branch, err)
	}

	log, err := v.deps.Log.Workspace(record.Dir)
	if err != nil {
		global.Error(opCreate, "could not resolve the workspace log sink", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: resolve log sink: %w", branch, err)
	}
	log = log.With(dlog.Context{"workspace": string(record.ID)})

	// The spawn facts the fleet reads at bring-up. The model and permission
	// mode are recorded HERE, before any session exists, because
	// StartSession(fresh) carries them and nothing else knows what the user
	// asked for.
	//
	// THE HOST IDENTITY IS MINTED WITH THE ROW, not later at bring-up. This is
	// where the session record is CREATED, and the host view is composed from
	// the record the instant it exists — so a row filed without an identity
	// withholds the view and, if the bring-up below never succeeds, keeps
	// withholding it for the life of the workspace. Bring-up rotates the
	// identity when it starts a fresh conversation, exactly as it does for any
	// other record; what it must never have to do is invent the first one.
	session := wsm.Session{
		Workspace:      record.ID,
		HostSessionID:  wsm.NewHostSessionID(),
		ConfigDir:      v.deps.Accounts.ConfigDirFor(record.Dir),
		Model:          spec.Model,
		PermissionMode: mintedPermissionMode(spec.PermissionMode),
		StartedAt:      v.now(),
	}
	if spec.ForkFrom != nil {
		// A FORK IS REFUSED HERE, before anything else is written for it, when
		// its parent holds no conversation.
		if _, err := v.forkableParentSession(ctx, log, *spec.ForkFrom, spec.forkRefusalArm()); err != nil {
			return wsm.Workspace{}, err
		}
	}
	if err := v.deps.DB.PutSession(ctx, session); err != nil {
		log.Error(opCreate, "could not record the spawn facts", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: record the spawn facts: %w", branch, err)
	}
	if spec.ForkFrom != nil {
		// THE FORK IS OWED, NOT MADE: the bring-up makes it through the
		// vendor, while the parent starts no turn, and resumes it (forking.go).
		if err := v.deps.DB.PutPendingFork(ctx, wsm.PendingFork{Workspace: record.ID, Parent: *spec.ForkFrom}); err != nil {
			log.Error(opCreate, "could not record the fork the workspace owes", dlog.Context{
				"parent": string(*spec.ForkFrom), "cause": err.Error(),
			})
			return wsm.Workspace{}, fmt.Errorf("create %q: record the fork: %w", branch, err)
		}
	}

	// A MERGE WORKSPACE'S SESSION COMES UP ONLY WHEN A TURN FIRST NEEDS IT
	// (its pick at the head of the queue, or a repair): the bring-up is what
	// makes the fork it owes, so its context is its maker's as it stands when
	// the merge has started, never as it stood when the merge was asked for.
	// A merge that needs no turn never forks at all.
	if spec.MergeOf == nil {
		if err := v.deps.Sessions.Start(ctx, record.ID); err != nil {
			log.Error(opCreate, "the session did not come up", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("create %q: start the session: %w", branch, err)
		}
	} else {
		log.Info(opCreate, "created a merge workspace whose session comes up when its merge first needs a turn", dlog.Context{
			"requester": string(spec.MergeOf.Requester), "maker": string(spec.MergeOf.Maker)})
	}

	if err := v.submitInitialPrompt(ctx, log, record, spec, policy); err != nil {
		return wsm.Workspace{}, err
	}

	log.Info(opCreate, "created the workspace", dlog.Context{
		"branch": branch, "dir": record.Dir, "one_shot": spec.OneShot,
	})
	v.republishRegistry(ctx, log, opCreate)
	v.deps.Derived.ReportCreated(record, spec.OwnerNotice)
	return record, nil
}

// mintedPermissionMode is the mode the new session's row carries: what the
// creation asked for, or DefaultPermissionMode when it asked for nothing. The
// row is written rather than left empty so the stored fact and the mode the
// session actually runs under are the same string, and the topbar reads the
// mode in force off that row without inferring anything.
func mintedPermissionMode(requested string) string {
	if requested == "" {
		return DefaultPermissionMode
	}
	return requested
}

// validateCreate refuses the forms that cannot be built, before anything is
// minted or written.
func (v *verbs) validateCreate(log dlog.Logger, spec CreateSpec) error {
	// A BASE IS A MERGE WORKSPACE'S ALONE: every other workspace is cut from
	// its source's tip, which no request names.
	if spec.BaseRef != "" && spec.MergeOf == nil {
		log.Error(opCreate, "a create spec names a base outside a merge workspace", dlog.Context{"base_ref": spec.BaseRef})
		return fmt.Errorf("create: only a merge workspace is cut from a named base, and this create is not one (base %q)", spec.BaseRef)
	}
	// A MERGE WORKSPACE IS ONLY EVER ASKED FOR BY THE DAEMON'S OWN DETACHED
	// MERGE, so a spec that contradicts itself is a defect in that caller, not
	// a request to refuse. It nests under its requester and forks its maker.
	if spec.MergeOf != nil && (spec.ForkFrom == nil || spec.Parent == nil || *spec.Parent != spec.MergeOf.Requester || spec.MergeOf.ForkHead == "" ||
		spec.OwnerNotice != "" ||
		spec.OneShot || strings.TrimSpace(spec.InitialPrompt) != "" || strings.TrimSpace(spec.Name) != "") {
		log.Error(opCreate, "a merge workspace's create spec contradicts itself", dlog.Context{
			"requester": string(spec.MergeOf.Requester), "request": string(spec.MergeOf.Request), "fork_head": spec.MergeOf.ForkHead,
			"fork_from": ids.Optional(spec.ForkFrom), "parent": ids.Optional(spec.Parent), "one_shot": spec.OneShot,
			"has_prompt": strings.TrimSpace(spec.InitialPrompt) != "", "name": spec.Name,
		})
		return fmt.Errorf("create: a merge workspace forks from its requester %s and is named, prompted and targeted by the daemon alone", spec.MergeOf.Requester)
	}
	// A MERGE WORKSPACE LANDS BY THE LANDING THE QUEUE DECIDED, which always
	// names a target and a branch: an origin without them is a defect in the
	// hand-off, never a landing to derive here.
	if spec.MergeOf != nil && (spec.MergeOf.TargetDir == "" || spec.MergeOf.ParentBranch == "") {
		log.Error(opCreate, "a merge workspace's origin carries no landing", dlog.Context{
			"requester": string(spec.MergeOf.Requester), "request": string(spec.MergeOf.Request),
			"target_dir": spec.MergeOf.TargetDir, "parent_branch": spec.MergeOf.ParentBranch,
		})
		return fmt.Errorf("create: the merge workspace of %s for request %s has no landing (target %q, parent branch %q)",
			spec.MergeOf.Requester, spec.MergeOf.Request, spec.MergeOf.TargetDir, spec.MergeOf.ParentBranch)
	}
	// ONLY A ONE-SHOT NAMES A CONTEXT WORKSPACE: the rpc carries it inside the
	// one-shot arm, so a spec with one on any other form is a defect in its
	// caller, not a request to refuse.
	if spec.ContextWorkspace != nil && !spec.OneShot {
		log.Error(opCreate, "a create spec names a context workspace outside a one-shot", dlog.Context{
			"context_workspace": string(*spec.ContextWorkspace),
		})
		return fmt.Errorf("create: only a one-shot names a context workspace, and this create is not one (context %s)", *spec.ContextWorkspace)
	}
	if spec.OneShot && strings.TrimSpace(spec.InitialPrompt) == "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "spec.OneShot && strings.TrimSpace(spec.InitialPrompt) == \"\""})
		return refuse(log, "CreateWorkspace", ArmNoSlug,
			"a one-shot creation must carry the prompt it runs", false)
	}
	if UngatedPermissionModes[spec.PermissionMode] && spec.ConsentedUngatedMode != spec.PermissionMode {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "UngatedPermissionModes[spec.PermissionMode] && spec.ConsentedUngatedMode != spec.PermissionMode"})
		return refuse(log, "CreateWorkspace", ArmUngatedWithoutConsent,
			fmt.Sprintf("permission mode %q disables the consent gate and no consent was recorded", spec.PermissionMode), false)
	}
	return nil
}

// branchFor derives the workspace's branch, which is also its name and its
// worktree directory component: the supplied name when there is one, else the
// name the MODEL mints for the initial prompt.
//
// A supplied name that already carries a prefix component is taken as it
// stands; the prefix is applied only to a name this daemon derived. A name the
// daemon derived is also DISAMBIGUATED against existing branches and
// workspaces, which a supplied name is not — the user typed that one and is
// owed git's own refusal if it is taken.
func (v *verbs) branchFor(ctx context.Context, log dlog.Logger, spec CreateSpec, repoDir string, minted wsm.WorkspaceID, origin *wsm.MergeOrigin) (string, func(), error) {
	// A MERGE WORKSPACE IS NAMED BY ITS REQUESTING AGENT, with no naming call
	// and no disambiguation: its request checked the name and reserved it, so
	// it is free to this fork and to nothing else.
	if origin != nil {
		return v.claimSupplied(ctx, log, repoDir, spec.MergeName, origin.Requester)
	}
	if supplied := strings.TrimSpace(spec.Name); supplied != "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "supplied := strings.TrimSpace(spec.Name); supplied != \"\""})
		// A SUPPLIED NAME IS CHECKED BEFORE IT IS CLAIMED. It becomes the
		// branch, the worktree directory and the tab label as it stands, so a
		// name that breaks the naming rule is refused here, naming the part
		// that broke it, rather than handed to `git` to fail on.
		if err := refuseSuppliedName(log, "CreateWorkspace", supplied); err != nil {
			return "", nil, err
		}
		return v.claimSupplied(ctx, log, repoDir, supplied, "")
	}
	// A FORK IS NAMED FROM ITS PROMPT PLUS THE CONVERSATION IT CONTINUES (owner
	// ruling, 2026-09-27), through the same naming call, so a fork whose prompt
	// is blank still gets a name that says what it carries on with.
	conversation, err := v.forkNamingConversation(ctx, log, spec)
	if err != nil {
		return "", nil, err
	}
	// An initial prompt is OPTIONAL on the standard form: an unset one is an
	// empty workspace, which is a legal create. With no prompt and no
	// conversation there is nothing to name the workspace AFTER, and the
	// naming call is not asked to invent one, so the branch is named after the
	// workspace's own minted id.
	if strings.TrimSpace(spec.InitialPrompt) == "" && conversation == "" {
		branch := Name(Prefix(), UnnamedSlugPrefix+string(minted))
		log.Debug(opCreate, "named the branch after the minted workspace id", dlog.Context{"branch": branch})
		return branch, func() {}, nil
	}
	// A NAMING CALL IS ABOUT TO RUN — the slow, model-backed step. Reported
	// before it starts so a watching client shows "deriving name" while it
	// waits, not after. Reached only on this branch: a supplied name, or a
	// create with neither a prompt nor a conversation, never derives one, so
	// neither reports the stage.
	reportCreateStage(spec, CreateStageDerivingName)
	slug, err := v.mintName(ctx, log, repoDir, spec.InitialPrompt, conversation)
	if err != nil {
		var failure *namingFailure
		if errors.As(err, &failure) {
			// THE REFUSAL AND THE ERROR RECORD ARE ONE. mintName already wrote
			// the ERROR line; this states the same failure as the contract's
			// own arm, with the fields a client renders.
			return "", nil, refuseWith(log, "CreateWorkspace", ArmNamingFailed,
				fmt.Sprintf("no name was supplied and the naming call could not mint one: %s", failure.Detail), false,
				map[string]any{
					"model":    headless.ModelHaiku,
					"cause":    failure.Cause,
					"attempts": failure.Attempts,
					"answer":   failure.Answer,
				})
		}
		return "", nil, err
	}
	branch, release, err := v.freeName(ctx, log, repoDir, Name(Prefix(), slug))
	if err != nil {
		log.Error(opCreate, "could not find a free name for the minted workspace name", dlog.Context{
			"slug": slug, "cause": err.Error(),
		})
		return "", nil, fmt.Errorf("create: name %q: %w", slug, err)
	}
	log.Debug(opCreate, "the model named the workspace", dlog.Context{
		"branch": branch, "fork": spec.ForkFrom != nil,
	})
	return branch, release, nil
}

// claimSupplied resolves a SUPPLIED name to its branch and claims it. A name a
// standing async merge reserved is refused (ArmNameReserved), unless the
// reservation is HOLDER's own (the fork it was made for); a name another
// allocation claims is refused the same way. A supplied name is otherwise not
// disambiguated: the user typed it, and is owed git's own refusal if it is
// already a branch.
func (v *verbs) claimSupplied(ctx context.Context, log dlog.Logger, repoDir, name string, holder wsm.WorkspaceID) (string, func(), error) {
	if strings.TrimSpace(name) == "" {
		err := errors.New("create: a merge workspace is named by the request it was forked for, and this one carries no name")
		log.Error(opCreate, "a merge workspace's create carries no name", dlog.Context{"holder": string(holder)})
		return "", nil, err
	}
	branch := suppliedBranch(strings.TrimSpace(name))
	v.names.mu.Lock()
	defer v.names.mu.Unlock()
	reserved, err := v.deps.DB.ReservedMergeNames(ctx)
	if err != nil {
		log.Error(opCreate, "could not read the reserved merge names", dlog.Context{"branch": branch, "cause": err.Error()})
		return "", nil, fmt.Errorf("create %q: %w", branch, err)
	}
	for reservedName, reservedBy := range reserved {
		if suppliedBranch(reservedName) == branch && reservedBy != holder {
			return "", nil, refuseWith(log, "CreateWorkspace", ArmNameReserved,
				fmt.Sprintf("the name %q is reserved by a pending async merge", branch), false, map[string]any{"branch": branch})
		}
	}
	if v.names.claimed[branch] {
		return "", nil, refuseWith(log, "CreateWorkspace", ArmNameReserved,
			fmt.Sprintf("the name %q is being claimed by another create or async merge", branch), false, map[string]any{"branch": branch})
	}
	return branch, v.claimLocked(branch), nil
}

// forkNamingConversation answers the summary of the conversation a FORK
// continues, for the naming call; empty for a create that forks nothing.
//
// THE SOURCE IS THE DAEMON'S OWN RECORD OF THE PARENT CONVERSATION: the
// parent's own turns and, through its fork lineage, the turns it inherited
// (wsm.RecentConversationPrompts). It needs no live parent shim, so a
// hibernated parent is named from as readily as a running one. It is composed
// by the title synthesizer's one digest composition (the most recent requests,
// each bounded), so "what this conversation is about" has one spelling. The
// digest never quotes more than titlesynth.MaxPrompts requests, so only that
// bounded tail is read, however deep the lineage.
//
// A parent with no conversation is REFUSED HERE, before the naming call is
// paid for, on the same arm the create refuses it with.
func (v *verbs) forkNamingConversation(ctx context.Context, log dlog.Logger, spec CreateSpec) (string, error) {
	if spec.ForkFrom == nil {
		return "", nil
	}
	parent := *spec.ForkFrom
	if _, err := v.forkableParentSession(ctx, log, parent, spec.forkRefusalArm()); err != nil {
		return "", err
	}
	rows, err := v.deps.DB.RecentConversationPrompts(ctx, parent, titlesynth.MaxPrompts)
	if err != nil {
		log.Error(opCreate, "could not read the parent conversation to name the fork", dlog.Context{
			"parent": string(parent), "cause": err.Error(),
		})
		return "", fmt.Errorf("fork from %q: read the conversation to name it: %w", parent, err)
	}
	var said []string
	for _, row := range rows {
		if strings.TrimSpace(row.Text) != "" {
			said = append(said, row.Text)
		}
	}
	if len(said) == 0 {
		log.Info(opCreate, "the fork's parent holds no recorded request to name the fork from", dlog.Context{
			"parent": string(parent), "rows": len(rows),
		})
		return "", nil
	}
	return titlesynth.ComposeDigest("", said), nil
}

// forkableParentSession answers the parent's session when it holds a
// conversation to fork (recordedSession.hasConversation), and the refusal ARM
// names (forkRefusalArm) when it does not. Read by the naming step (before a
// model call is paid for) and by the create itself.
func (v *verbs) forkableParentSession(ctx context.Context, log dlog.Logger, parent ids.WorkspaceID, arm string) (wsm.Session, error) {
	parentConv, err := readRecordedSession(ctx, v.deps.DB, parent)
	if err != nil {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "err != nil"})
		return wsm.Session{}, fmt.Errorf("fork from %q: read the parent session: %w", parent, err)
	}
	if !parentConv.hasConversation() {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!parentConv.hasConversation()"})
		return wsm.Session{}, refuseWith(log, "CreateWorkspace", arm,
			fmt.Sprintf("workspace %q has no conversation to fork", parent), false,
			map[string]any{"workspace": &workspacev1.WorkspaceRef{Id: string(parent)}})
	}
	return parentConv.session, nil
}

// mergeTargetDir answers where this workspace's merge will land: the PARENT
// workspace's worktree when it was cut from one, otherwise the repository's
// main worktree. It is recorded at creation and never inferred later.
func (v *verbs) mergeTargetDir(ctx context.Context, repoDir string, parent *ids.WorkspaceID) (string, error) {
	if parent == nil {
		return repoDir, nil
	}
	record, err := v.deps.DB.Workspace(ctx, *parent)
	if err != nil {
		return "", fmt.Errorf("parent workspace %q: %w", *parent, err)
	}
	return record.Dir, nil
}

// forkRefusalArm is the arm a fork whose source has no conversation is refused
// on, whichever workspace the fork names.
func (s CreateSpec) forkRefusalArm() string {
	return ArmForkSourceHasNoConversation
}

// parentWorkspace answers the workspace this create's branch is cut from and
// lands into, nil for the primary branch. It is Parent alone: a fork never
// decides the branch.
func (s CreateSpec) parentWorkspace() *ids.WorkspaceID {
	return s.Parent
}

// OwnerLands reports whether this create is a child whose context is not
// forked from its owner, which only the owner (or the user) lands. A merge
// workspace never is: its landing is the queue's.
func (s CreateSpec) OwnerLands() bool {
	return s.MergeOf == nil && s.Parent != nil && (s.ForkFrom == nil || *s.ForkFrom != *s.Parent)
}

// forkedElsewhere reports whether the conversation is forked from a workspace
// other than the branch source, which the first message's note names. A merge
// workspace's own brief says what it is, so it carries no note.
func (s CreateSpec) forkedElsewhere() bool {
	return s.MergeOf == nil && s.ForkFrom != nil && (s.Parent == nil || *s.ForkFrom != *s.Parent)
}

// filesItself reports whether the created workspace is told to file itself
// into one of its repository's subsections (BriefSubsectionPlacement). Only a
// ROOT is: a family never splits across subsections, so a child sits wherever
// its root does, and placing it would move its whole family; a merge
// workspace's own brief says what it is.
func (s CreateSpec) filesItself() bool {
	return filesItself(s.Parent, s.MergeOf != nil)
}

// filesItself is the one rule of which workspace is asked to file itself
// into a subsection: a ROOT (PARENT nil) that is no merge workspace. A
// create and a workspace leaving its one-shot state both ask it.
func filesItself(parent *ids.WorkspaceID, merge bool) bool {
	return parent == nil && !merge
}

// submitInitialPrompt sends the workspace's first message down the ONE delivery
// path, with origin WORKSPACE_CREATED. A root's prompt is followed by the
// subsection placement note when its repository has subsections. A one-shot
// prompt is DECORATED last: the autonomous preamble, the composed message,
// and the repository's completion directive.
func (v *verbs) submitInitialPrompt(ctx context.Context, log dlog.Logger, record wsm.Workspace, spec CreateSpec, policy prompts.Source) error {
	if strings.TrimSpace(spec.InitialPrompt) == "" {
		log.Debug(opCreate, "created without an initial prompt", nil)
		return nil
	}
	text := spec.InitialPrompt
	if spec.forkedElsewhere() {
		decorated, err := v.decorateForkedElsewhere(ctx, log, text, *spec.ForkFrom, spec.Parent)
		if err != nil {
			return refuse(log, "CreateWorkspace", ArmBriefMissing, err.Error(), false)
		}
		text = decorated
	}
	if spec.ContextWorkspace != nil {
		decorated, err := v.decorateOneShotContext(ctx, log, text, *spec.ContextWorkspace)
		if err != nil {
			return refuse(log, "CreateWorkspace", ArmBriefMissing, err.Error(), false)
		}
		text = decorated
	}
	if spec.filesItself() {
		owed, err := v.placementOwed(ctx, log, record)
		if err != nil {
			return fmt.Errorf("create %q: %w", record.Name, err)
		}
		if owed {
			decorated, err := v.decorateSubsectionPlacement(log, text, record)
			if err != nil {
				return refuse(log, "CreateWorkspace", ArmBriefMissing, err.Error(), false)
			}
			text = decorated
		}
	}
	if spec.OneShot {
		decorated, err := v.decorateOneShot(text, policy)
		if err != nil {
			log.Error(opCreate, "could not compose the one-shot prompt", dlog.Context{"cause": err.Error()})
			return refuse(log, "CreateWorkspace", ArmBriefMissing, err.Error(), false)
		}
		text = decorated
	}

	turn := wsm.NewTurnID()
	said := SaidText(text)
	if err := v.deps.DB.PutTurn(ctx, wsm.Turn{
		ID:        turn,
		Workspace: record.ID,
		Text:      text,
		Origin:    conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED.String(),
		StartedAt: v.now(),
	}); err != nil {
		log.Error(opCreate, "could not record the initial turn", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("create %q: record the initial turn: %w", record.Name, err)
	}
	disposition, err := v.deps.Queue.Submit(ctx, promptSubmission(record.ID, turn, said))
	if err != nil {
		log.Error(opCreate, "the initial prompt was not accepted", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("create %q: submit the initial prompt: %w", record.Name, err)
	}
	log.Debug(opCreate, "submitted the initial prompt", dlog.Context{
		"turn": string(turn), "delivered": disposition.Delivered, "refused_arm": disposition.RefusedArm,
	})
	return nil
}

// SaidText composes the one canonical prompt form from plain text. It is
// exported because the command-file ingress composes prompts the same way, and
// two spellings of "the user said this" would drift.
//
// THE COMPOSITION ITSELF LIVES IN THE FEED PACKAGE, which draws a fork's
// ported prompt rows from the same shape and cannot import this one. This is
// the name the daemon's ingress paths already code against.
func SaidText(text string) *conversationv1.UserSaid { return feed.SaidText(text) }

// CreateMergeWorkspace creates the merge workspace a detached merge runs in
// (CreateSpec.MergeOf), in the requester's own repository.
func (v *verbs) CreateMergeWorkspace(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, name, base string) (wsm.Workspace, error) {
	repoDir, err := v.repoDirOf(ctx, requester)
	if err != nil {
		v.deps.Log.Global().Error(opCreate, "could not resolve the repository a merge workspace is forked in", dlog.Context{
			"requester": string(requester.ID), "request": string(origin.Request), "cause": err.Error(),
		})
		return wsm.Workspace{}, fmt.Errorf("create the merge workspace of %s: %w", requester.ID, err)
	}
	// THE FORK HEAD IS READ ONCE, HERE, and the worktree is cut at it: a
	// commit the requester makes after this is not part of the merge.
	head, err := v.deps.Git.ResolveRef(ctx, repoDir, "refs/heads/"+base)
	if err != nil {
		v.deps.Log.Global().Error(opCreate, "could not resolve the branch a merge workspace is forked from", dlog.Context{
			"requester": string(requester.ID), "request": string(origin.Request), "branch": base, "repo": repoDir, "cause": err.Error(),
		})
		return wsm.Workspace{}, fmt.Errorf("create the merge workspace of %s: resolve %s: %w", requester.ID, base, err)
	}
	origin.ForkHead = head
	// THE MERGE WORKSPACE NESTS UNDER ITS REQUESTER and forks its MAKER: the
	// workspace whose context the work was done in (the subagent's spawning
	// workspace, or the workspace that cut the branch).
	if origin.Maker == "" {
		v.deps.Log.Global().Error(opCreate, "a merge workspace's origin names no maker to fork", dlog.Context{
			"requester": string(requester.ID), "request": string(origin.Request)})
		return wsm.Workspace{}, fmt.Errorf("create the merge workspace of %s: its origin names no maker", requester.ID)
	}
	parent, maker := requester.ID, origin.Maker
	return v.Create(ctx, CreateSpec{
		RepoDir:   repoDir,
		BaseRef:   base,
		Parent:    &parent,
		ForkFrom:  &maker,
		Requester: &parent,
		MergeOf:   &origin,
		MergeName: name,
	})
}

// StartMergeWorkspace brings up merge workspace WS's session when none is
// live (Sessions.Start is a no-op on a live one), making the fork it still
// owes (forking.go). A restart that cut the create short after registration
// leaves the merge workspace with its fork owed and no session, and the boot
// brings no hidden workspace up, so the hand-off that finds it starts it here
// rather than submitting its pick to a session that does not exist.
func (v *verbs) StartMergeWorkspace(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, "StartMergeWorkspace", ws)
	if err != nil {
		return err
	}
	if record.MergeOf == nil {
		err := fmt.Errorf("start merge workspace %q: it is no merge workspace", ws)
		log.Error(opCreate, "a merge workspace start named a workspace that is none", dlog.Context{"cause": err.Error()})
		return err
	}
	if err := v.deps.Sessions.Start(ctx, ws); err != nil {
		log.Error(opCreate, "the merge workspace's session did not come up", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("start merge workspace %q: %w", ws, err)
	}
	return nil
}

// BriefForkIntoPrimary is the note a fork into the primary branch opens with:
// the conversation came from another workspace, the worktree did not.
const BriefForkIntoPrimary = "workspace-fork-into-primary-preamble"

// BriefForkedElsewhere is the note a workspace whose conversation was forked
// from a workspace other than its branch source opens with: the conversation
// came from that workspace, the worktree did not.
const BriefForkedElsewhere = "workspace-forked-elsewhere-preamble"

// decorateForkedElsewhere opens the first message with the BriefForkedElsewhere
// note naming the workspace the conversation came from and what the worktree
// was cut from instead (the primary branch, or the owner's branch),
// meta-wrapped so the drawn bubble is the user's words alone. A brief that will
// not load or splice fails the create loudly rather than sending the prompt
// undecorated.
func (v *verbs) decorateForkedElsewhere(ctx context.Context, log dlog.Logger, raw string, from ids.WorkspaceID, owner *ids.WorkspaceID) (string, error) {
	source := "the primary branch"
	if owner != nil {
		record, err := v.deps.DB.Workspace(ctx, *owner)
		if err != nil {
			log.Error(opCreate, "could not read the owner a forked child's note names", dlog.WithCause(dlog.Context{"owner": string(*owner)}, err))
			return "", fmt.Errorf("read the owner %q the forked-elsewhere note names: %w", *owner, err)
		}
		source = "the branch of the workspace `" + record.Name + "`"
	}
	return v.decorateWithWorkspaceNote(ctx, log, raw, BriefForkedElsewhere, from,
		func(origin wsm.Workspace) map[string]string {
			return map[string]string{"origin": origin.Name, "source": source}
		})
}

// BriefSubsectionPlacement is the note a root workspace's first message closes
// with when its repository has subsections: read them, and file this
// workspace into the one it most belongs in, if any
// (`agent-repld workspace subsections` and `workspace place`).
const BriefSubsectionPlacement = "workspace-subsection-placement"

// opSubsectionPlacement is the operation the create records its placement
// note's decision under: whether the note was owed, and why not.
const opSubsectionPlacement = "daemon.workspace.subsection_placement"

// placementOwed reports whether RECORD's first message owes the
// BriefSubsectionPlacement note: its repository has a subsection to file it
// into, or at least placementGatherMin other open top-level workspaces in no
// subsection that it could be gathered with into a new one
// (ArrangeCreateAgentSubsection). Otherwise the agent is not asked. The
// registry is read as ONE snapshot (wsm.Arrangement), and a read that fails
// fails the create rather than guessing either way.
func (v *verbs) placementOwed(ctx context.Context, log dlog.Logger, record wsm.Workspace) (bool, error) {
	fields := dlog.Context{"repo_id": string(record.Repo)}
	arrangement, err := v.deps.DB.ReadArrangement(ctx)
	if err != nil {
		log.Error(opSubsectionPlacement, "could not read the arrangement a new workspace could file itself into", dlog.WithCause(fields, err))
		return false, fmt.Errorf("read the arrangement of repository %q: %w", record.Repo, err)
	}
	subsections, unsectioned := 0, 0
	for _, sub := range arrangement.Subsections {
		if sub.Repo == record.Repo {
			subsections++
		}
	}
	for _, ws := range arrangement.Workspaces {
		if ws.Repo == record.Repo && ws.ID != record.ID && ws.Parent == nil && ws.Subsection == nil && !ws.Closed && ws.MergeOf == nil {
			unsectioned++
		}
	}
	fields["subsections"], fields["unsectioned"] = subsections, unsectioned
	if subsections == 0 && unsectioned < placementGatherMin {
		log.Debug(opSubsectionPlacement, "the repository has no subsection and too few unsectioned workspaces, so the new workspace is not asked to file itself", fields)
		return false, nil
	}
	return true, nil
}

// placementGatherMin is how many other open top-level workspaces in no
// subsection make a new workspace worth asking to gather them into a
// subsection of its own (owner request, 2026-10-26: "2 or more related
// workspaces without subsections").
const placementGatherMin = 2

// decorateSubsectionPlacement closes the first message with the meta-wrapped
// BriefSubsectionPlacement note. The brief is the daemon's own, the same for
// every repository, since the verbs it names are the agent API's. A brief
// that will not load or splice fails the create loudly rather than sending
// the prompt without the note it owes.
func (v *verbs) decorateSubsectionPlacement(log dlog.Logger, raw string, record wsm.Workspace) (string, error) {
	fields := dlog.Context{"brief": BriefSubsectionPlacement, "repo_id": string(record.Repo)}
	note, err := v.corpusNote(log, opSubsectionPlacement, fields, BriefSubsectionPlacement, nil)
	if err != nil {
		return "", err
	}
	log.Debug(opSubsectionPlacement, "asked the new workspace to file itself into a subsection", fields)
	return raw + prompts.Wrap("\n\n"+note), nil
}

// BriefOneShotContext is the note a one-shot asked for from inside another
// workspace opens its commission with: the cited issue is that workspace's.
const BriefOneShotContext = "workspace-oneshot-context-preamble"

// decorateOneShotContext opens a one-shot's commission with the
// BriefOneShotContext note naming the workspace the commission concerns, by
// name, worktree and branch. It runs before the one-shot decoration, so the
// note sits between the autonomous preamble and the user's words.
func (v *verbs) decorateOneShotContext(ctx context.Context, log dlog.Logger, raw string, about ids.WorkspaceID) (string, error) {
	return v.decorateWithWorkspaceNote(ctx, log, raw, BriefOneShotContext, about,
		func(concerned wsm.Workspace) map[string]string {
			return map[string]string{"context": concerned.Name, "context_dir": concerned.Dir, "context_branch": concerned.Branch}
		})
}

// decorateWithWorkspaceNote opens raw with the meta-wrapped note BRIEF, spliced
// from the registered workspace SUBJECT the note names. It is the one
// composition every create note about another workspace shares: the record is
// read, the brief is loaded from the daemon's own corpus, the values are
// spliced, and any failure is logged once here and returned, so a create never
// sends its prompt without the note it owes.
func (v *verbs) decorateWithWorkspaceNote(
	ctx context.Context,
	log dlog.Logger,
	raw, brief string,
	subject ids.WorkspaceID,
	values func(wsm.Workspace) map[string]string,
) (string, error) {
	fields := dlog.Context{"brief": brief, "workspace": string(subject)}
	record, err := v.deps.DB.Workspace(ctx, subject)
	if err != nil {
		log.Error(opCreate, "could not read the workspace a create's note names", dlog.WithCause(fields, err))
		return "", fmt.Errorf("read the workspace %q the %s brief names: %w", subject, brief, err)
	}
	note, err := v.corpusNote(log, opCreate, fields, brief, values(record))
	if err != nil {
		return "", err
	}
	return prompts.Wrap(note) + raw, nil
}

// corpusNote reads BRIEF from the daemon's own corpus and splices VALUES into
// it: the one composition of every note a create adds to a first message, and
// of the placement brief a workspace leaving its one-shot state is sent. A
// brief that will not load or splice is logged once here, at ERROR under OP
// with FIELDS, and returned, so a create never sends its prompt without the
// note it owes.
func (v *verbs) corpusNote(log dlog.Logger, op string, fields dlog.Context, brief string, values map[string]string) (string, error) {
	loaded, err := v.load(v.deps.PromptsDir, brief)
	if err != nil {
		log.Error(op, "could not read a create note's brief", dlog.WithCause(fields, err))
		return "", fmt.Errorf("read the %s brief: %w", brief, err)
	}
	note, err := v.splice(loaded, values)
	if err != nil {
		log.Error(op, "could not splice a create note's brief", dlog.WithCause(fields, err))
		return "", fmt.Errorf("splice the %s brief: %w", brief, err)
	}
	return note, nil
}

// checkDerivation refuses a create whose owner, forked workspace or requester
// the registry does not hold in this repository, before anything is minted,
// and REOPENS a closed owner that is to arbitrate the child, so the work it
// must land has a session to land it. A merge workspace's derivation is the
// queue's, decided at hand-off, and is not re-checked here.
func (v *verbs) checkDerivation(ctx context.Context, log dlog.Logger, spec CreateSpec, repoDir string) error {
	if spec.MergeOf != nil {
		return nil
	}
	if spec.Requester != nil {
		if _, err := v.deps.DB.Workspace(ctx, *spec.Requester); err != nil {
			if errors.Is(err, wsm.ErrNotFound) {
				return refuseWith(log, "CreateWorkspace", ArmUnknownRequester,
					fmt.Sprintf("no workspace %q is registered to have asked", *spec.Requester), true,
					map[string]any{"workspace": &workspacev1.WorkspaceRef{Id: string(*spec.Requester)}})
			}
			log.Error(opCreate, "could not read the create's requester", dlog.WithCause(dlog.Context{"requester": string(*spec.Requester)}, err))
			return fmt.Errorf("create: requester %q: %w", *spec.Requester, err)
		}
	}
	if spec.Parent != nil {
		owner, err := v.inRepository(ctx, log, *spec.Parent, repoDir, "owner")
		if err != nil {
			return err
		}
		if owner == nil {
			return refuseWith(log, "CreateWorkspace", ArmUnknownOwner,
				fmt.Sprintf("no workspace %q of this repository owns a branch to cut from", *spec.Parent), true,
				map[string]any{"owner": &workspacev1.WorkspaceRef{Id: string(*spec.Parent)}})
		}
	}
	if spec.ForkFrom != nil {
		source, err := v.inRepository(ctx, log, *spec.ForkFrom, repoDir, "fork source")
		if err != nil {
			return err
		}
		if source == nil {
			return refuseWith(log, "CreateWorkspace", ArmUnknownForkSource,
				fmt.Sprintf("no workspace %q of this repository holds a conversation to fork", *spec.ForkFrom), true,
				map[string]any{"workspace": &workspacev1.WorkspaceRef{Id: string(*spec.ForkFrom)}})
		}
	}
	// WHO LANDS A CHILD: a child that does not carry its owner's context is
	// the owner's to land, so it must tell the owner what is coming, and it
	// cannot be a one-shot, which lands itself.
	if spec.OwnerLands() && spec.OneShot {
		return refuseWith(log, "CreateWorkspace", ArmOneShotOwnerLands,
			"a one-shot lands itself, and a child that does not fork its owner's context is landed by its owner", false,
			map[string]any{"owner": &workspacev1.WorkspaceRef{Id: string(*spec.Parent)}})
	}
	if spec.OwnerLands() && strings.TrimSpace(spec.OwnerNotice) == "" {
		return refuse(log, "CreateWorkspace", ArmOwnerNoticeRequired,
			"a child that does not fork its owner's context must carry a notice to its owner", false)
	}
	if !spec.OwnerLands() && spec.OwnerNotice != "" {
		return refuse(log, "CreateWorkspace", ArmOwnerNoticeUnexpected,
			"only a child its owner lands carries a notice to its owner", false)
	}
	// THE OWNER IS REOPENED ONLY ONCE THE CREATE IS KNOWN TO BE BUILDABLE, so
	// a refused create never opens anything.
	if spec.OwnerLands() {
		owner, err := v.deps.DB.Workspace(ctx, *spec.Parent)
		if err != nil {
			log.Error(opCreate, "could not read the owner that is to land the child", dlog.WithCause(dlog.Context{"owner": string(*spec.Parent)}, err))
			return fmt.Errorf("create: owner %q: %w", *spec.Parent, err)
		}
		if owner.Closed {
			log.Info(opCreate, "reopening the closed owner that is to land the child", dlog.Context{"owner": string(owner.ID)})
			if err := v.Open(ctx, owner.ID, nil); err != nil {
				log.Error(opCreate, "could not reopen the owner that is to land the child", dlog.WithCause(dlog.Context{"owner": string(owner.ID)}, err))
				return fmt.Errorf("create: reopen the owner %q: %w", owner.ID, err)
			}
		}
	}
	return nil
}

// inRepository answers the registered workspace ID when it belongs to the
// repository at repoDir, nil when it is unregistered or of another
// repository, and an error recorded at ERROR when the registry cannot say.
func (v *verbs) inRepository(ctx context.Context, log dlog.Logger, id ids.WorkspaceID, repoDir, role string) (*wsm.Workspace, error) {
	fields := dlog.Context{"role": role, "workspace": string(id), "repo_dir": repoDir}
	record, err := v.deps.DB.Workspace(ctx, id)
	if errors.Is(err, wsm.ErrNotFound) {
		return nil, nil
	}
	if err != nil {
		log.Error(opCreate, "could not read a workspace the create names", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("create: %s %q: %w", role, id, err)
	}
	dir, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opCreate, "could not resolve the repository of a workspace the create names", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("create: %s %q: %w", role, id, err)
	}
	if !samePath(dir, repoDir) {
		fields["its_repo_dir"] = dir
		log.Info(opCreate, "a workspace the create names is of another repository", fields)
		return nil, nil
	}
	return &record, nil
}

// originRemote is the remote a workspace cut from origin is fetched from.
const originRemote = "origin"

// resolveBase answers the commit baseRef names. Every base is the owner's
// branch, the default branch, or a merge workspace's branch its hand-off just
// read, so one that does not resolve is a failure recorded at ERROR before
// anything is recorded or materialized: git would otherwise fail halfway
// through `worktree add` and leave the creation job standing for a workspace
// that cannot exist.
func (v *verbs) resolveBase(ctx context.Context, log dlog.Logger, repoDir, baseRef string) (string, error) {
	commit, err := v.deps.Git.ResolveRef(ctx, repoDir, baseRef)
	if err != nil {
		log.Error(opCreate, "the base a workspace is cut from does not resolve", dlog.WithCause(dlog.Context{"base_ref": baseRef, "repo_dir": repoDir}, err))
		return "", fmt.Errorf("create: the base %q does not resolve in %q: %w", baseRef, repoDir, err)
	}
	return commit, nil
}

// DefaultWorktreeBound bounds a create's worktree step: the one `git worktree
// add` that cuts the branch and checks it out.
//
// THE STEP RUNS THE REPOSITORY'S HOOKS, which are arbitrary commands, and a
// hook that never returned once held four creates for good with nothing to end
// them (2026-10-23). MEASURED from the editor's stage records over 102 creates,
// 2026-10-20 to 2026-10-23: median 1.4s, p90 6.8s, p95 10.7s, and the two
// slowest, 48.5s and 51.4s, were healthy checkouts of the explanation-engine
// repository. Two minutes is just over twice the slowest healthy step. An
// overrun stops git, is ERROR naming the bound, and fails the create.
const DefaultWorktreeBound = 2 * time.Minute
