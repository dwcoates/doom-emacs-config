package workspace

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/titlesynth"
	"claude-repld/internal/wsm"
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
//  6. bring the session up, forking the parent's transcript first when asked;
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

func (v *verbs) Create(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	global := v.deps.Log.Global().With(dlog.Context{"repo_dir": spec.RepoDir, "one_shot": spec.OneShot})

	if err := v.validateCreate(global, spec); err != nil {
		return wsm.Workspace{}, err
	}

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

	branch, err := v.branchFor(ctx, global, spec, repoDir, workspaceID)
	if err != nil {
		return wsm.Workspace{}, err
	}
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
	baseRef := spec.BaseRef
	if baseRef == "" {
		baseRef = defaultBranch
	}
	// A base ref that does not resolve is REFUSED before anything is recorded
	// or materialized: git would otherwise fail halfway through `worktree add`
	// and leave the creation job standing for a workspace that cannot exist.
	if _, err := v.deps.Git.ResolveRef(ctx, repoDir, baseRef); err != nil {
		// The arm spells `ref`, so the ref TRAVELS AS THE FIELD and not only
		// inside the sentence: a client rendering the arm names the bad ref.
		return wsm.Workspace{}, refuseWith(global, "CreateWorkspace", ArmBaseRefUnresolved,
			fmt.Sprintf("the base ref %q does not resolve in %q: %v", baseRef, repoDir, err), false,
			map[string]any{"ref": baseRef})
	}

	parent := spec.parentWorkspace()
	targetDir, err := v.mergeTargetDir(ctx, repoDir, parent)
	if err != nil {
		global.Error(opCreate, "could not resolve the merge target directory", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: merge target: %w", branch, err)
	}

	origin := OriginCreateStandard
	if spec.OneShot {
		origin = OriginCreateOneShot
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
			Origin:       origin,
		},
		Actions:              spec.MergeActions,
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
		"parent":   parentID(parent),
	})

	reportCreateStage(spec, CreateStageCreatingWorktree)
	if err := v.deps.Git.CreateWorktree(ctx, repoDir, branch, baseRef, worktreeDir); err != nil {
		global.Error(opCreate, "could not materialize the worktree", dlog.Context{
			"branch": branch, "worktree_dir": worktreeDir, "cause": err.Error(),
		})
		return wsm.Workspace{}, fmt.Errorf("create %q: materialize %q: %w", branch, worktreeDir, err)
	}

	// THE WORKTREE EXISTS; everything from here to the terminal outcome —
	// registration, a fork's transcript copy, the session bring-up and the
	// initial prompt's submission (steps 5-7) — is the session stage. Reported
	// before registration so a watching client stops showing the worktree line
	// the moment git is done.
	reportCreateStage(spec, CreateStageStartingSession)
	record, err := v.Register(ctx, worktreeDir, wsm.RegisterFacts{
		Name:          branch,
		Branch:        branch,
		ParentBranch:  baseRef,
		Parent:        (*wsm.WorkspaceID)(parent),
		RepoDir:       repoDir,
		DefaultBranch: defaultBranch,
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

	if spec.Priority != nil {
		if err := v.deps.DB.SetPriority(ctx, record.ID, spec.Priority); err != nil {
			log.Error(opCreate, "could not record the creation priority", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("create %q: priority: %w", branch, err)
		}
	}

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
		vendorSessionID, err := v.forkTranscript(ctx, log, *spec.ForkFrom, record, session.ConfigDir)
		if err != nil {
			return wsm.Workspace{}, err
		}
		// A fork RESUMES the ported conversation; it never starts a fresh one.
		session.VendorSessionID = vendorSessionID
	}
	if err := v.deps.DB.PutSession(ctx, session); err != nil {
		log.Error(opCreate, "could not record the spawn facts", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: record the spawn facts: %w", branch, err)
	}

	if err := v.deps.Sessions.Start(ctx, record.ID); err != nil {
		log.Error(opCreate, "the session did not come up", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create %q: start the session: %w", branch, err)
	}

	if err := v.submitInitialPrompt(ctx, log, record, spec, policy); err != nil {
		return wsm.Workspace{}, err
	}

	log.Info(opCreate, "created the workspace", dlog.Context{
		"branch": branch, "dir": record.Dir, "one_shot": spec.OneShot,
	})
	v.republishRegistry(ctx, log, opCreate)
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
func (v *verbs) branchFor(ctx context.Context, log dlog.Logger, spec CreateSpec, repoDir string, minted wsm.WorkspaceID) (string, error) {
	if supplied := strings.TrimSpace(spec.Name); supplied != "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "supplied := strings.TrimSpace(spec.Name); supplied != \"\""})
		if strings.Contains(supplied, "/") {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "strings.Contains(supplied, \"/\")"})
			return supplied, nil
		}
		return Name(Prefix(), supplied), nil
	}
	// A FORK IS NAMED FROM ITS PROMPT PLUS THE CONVERSATION IT CONTINUES (owner
	// ruling, 2026-09-27), through the same naming call, so a fork whose prompt
	// is blank still gets a name that says what it carries on with.
	conversation, err := v.forkNamingConversation(ctx, log, spec)
	if err != nil {
		return "", err
	}
	// An initial prompt is OPTIONAL on the standard form: an unset one is an
	// empty workspace, which is a legal create. With no prompt and no
	// conversation there is nothing to name the workspace AFTER, and the
	// naming call is not asked to invent one, so the branch is named after the
	// workspace's own minted id.
	if strings.TrimSpace(spec.InitialPrompt) == "" && conversation == "" {
		branch := Name(Prefix(), UnnamedSlugPrefix+string(minted))
		log.Debug(opCreate, "named the branch after the minted workspace id", dlog.Context{"branch": branch})
		return branch, nil
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
			return "", refuseWith(log, "CreateWorkspace", ArmNamingFailed,
				fmt.Sprintf("no name was supplied and the naming call could not mint one: %s", failure.Detail), false,
				map[string]any{
					"model":    headless.ModelHaiku,
					"cause":    failure.Cause,
					"attempts": failure.Attempts,
					"answer":   failure.Answer,
				})
		}
		return "", err
	}
	branch, err := v.freeName(ctx, log, repoDir, Name(Prefix(), slug))
	if err != nil {
		log.Error(opCreate, "could not find a free name for the minted workspace name", dlog.Context{
			"slug": slug, "cause": err.Error(),
		})
		return "", fmt.Errorf("create: name %q: %w", slug, err)
	}
	log.Debug(opCreate, "the model named the workspace", dlog.Context{
		"branch": branch, "fork": spec.ForkFrom != nil,
	})
	return branch, nil
}

// forkNamingConversation answers the summary of the conversation a FORK
// continues, for the naming call; empty for a create that forks nothing.
//
// THE SOURCE IS THE DAEMON'S OWN RECORD OF THE PARENT CONVERSATION: the
// prompt rows ConversationPrompts would answer — the parent's own turns and
// every row IT inherited from a fork of its own — which is exactly what the
// fork ports to the child. It needs no live parent shim, so a hibernated
// parent is named from as readily as a running one. It is composed by the
// title synthesizer's one digest composition (the most recent requests, each
// bounded), so "what this conversation is about" has one spelling.
//
// It reads RecentConversationPrompts, not ConversationPrompts: the digest
// never quotes more than titlesynth.MaxPrompts requests, so naming a fork
// reads only that bounded tail rather than the parent's whole history — a
// parent forked many generations deep, or with a long conversation of its
// own, costs this call the same either way. THE FORK'S OWN PORTED COPY is
// unaffected: it is written from ConversationPrompts, in forkconversation.go,
// which this function never touches.
//
// A parent with no conversation is REFUSED HERE, before the naming call is
// paid for, on the same arm the transcript port refuses it with.
func (v *verbs) forkNamingConversation(ctx context.Context, log dlog.Logger, spec CreateSpec) (string, error) {
	if spec.ForkFrom == nil {
		return "", nil
	}
	parent := *spec.ForkFrom
	if _, err := v.forkableParentSession(ctx, log, parent); err != nil {
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
// conversation to fork, and the ArmForkParentHasNoConversation refusal when it
// does not. It is the ONE statement of "this workspace can be forked", read by
// the naming step (before a model call is paid for) and by the transcript port.
func (v *verbs) forkableParentSession(ctx context.Context, log dlog.Logger, parent ids.WorkspaceID) (wsm.Session, error) {
	parentSession, ok, err := v.deps.DB.Session(ctx, parent)
	if err != nil {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "err != nil"})
		return wsm.Session{}, fmt.Errorf("fork from %q: read the parent session: %w", parent, err)
	}
	if !ok || parentSession.VendorSessionID == "" {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!ok || parentSession.VendorSessionID == \"\""})
		return wsm.Session{}, refuse(log, "CreateWorkspace", ArmForkParentHasNoConversation,
			fmt.Sprintf("workspace %q has no conversation to fork", parent), false)
	}
	return parentSession, nil
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

// parentID spells a parent for the record, empty when the create was spawned
// from no workspace.
func parentID(parent *ids.WorkspaceID) string {
	if parent == nil {
		return ""
	}
	return string(*parent)
}

// parentWorkspace answers the workspace this create was spawned from, nil when
// it was spawned from none. A fork names its parent by construction, so a spec
// carrying only ForkFrom still has one.
func (s CreateSpec) parentWorkspace() *ids.WorkspaceID {
	if s.Parent != nil {
		return s.Parent
	}
	return s.ForkFrom
}

// forkTranscript ports the parent's conversation into the CHILD's config root
// before any session starts, which is what makes the forked workspace
// resumable. It answers the vendor session id the child resumes.
//
// THE CHILD NEVER RESUMES THE PARENT'S OWN ID. shim.v1 StartSession has no
// fork arm, and a vendor session id is single-occupancy: the shim takes
// session-<vendor session id>.lock inside StartSession, so a child resuming a
// live parent's id would block on that lock forever. The daemon mints a fresh
// id, files the copy under it, and resumes that. The conversation's CONTENT is
// untouched -- its original main-agent id included -- and the parent keeps its
// own conversation, which is the whole point of a fork.
func (v *verbs) forkTranscript(ctx context.Context, log dlog.Logger, parent ids.WorkspaceID, child wsm.Workspace, childConfigDir string) (string, error) {
	parentRecord, err := v.deps.DB.Workspace(ctx, parent)
	if err != nil {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "err != nil"})
		return "", fmt.Errorf("fork from %q: %w", parent, err)
	}
	parentSession, err := v.forkableParentSession(ctx, log, parent)
	if err != nil {
		return "", err
	}
	transcript, err := v.deps.Accounts.FindTranscript(ctx, parentRecord.Dir, parentSession.VendorSessionID)
	if err != nil {
		log.Error(opCreate, "could not locate the parent transcript", dlog.Context{
			"parent": string(parent), "cause": err.Error(),
		})
		return "", fmt.Errorf("fork from %q: locate the transcript: %w", parent, err)
	}
	forked := wsm.NewVendorSessionID()
	minted, err := v.deps.Accounts.PortTranscript(ctx, transcript.Path, childConfigDir, child.Dir, forked)
	if err != nil {
		log.Error(opCreate, "could not port the parent transcript", dlog.Context{
			"parent": string(parent), "transcript": transcript.Path,
			"from_config_dir": transcript.ConfigDir, "cause": err.Error(),
		})
		return "", fmt.Errorf("fork from %q: port the transcript: %w", parent, err)
	}
	log.Debug(opCreate, "ported the parent transcript into the child config root under a fresh vendor session id", dlog.Context{
		"parent": string(parent), "config_dir": childConfigDir,
		"parent_vendor_session_id": parentSession.VendorSessionID,
		"child_vendor_session_id":  forked,
	})
	if err := v.forkConversation(ctx, log, parent, child.ID, minted); err != nil {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "err := v.forkConversation(ctx, log, parent, child.ID, minted); err != nil"})
		return "", err
	}
	return forked, nil
}

// submitInitialPrompt sends the workspace's first message down the ONE delivery
// path, with origin WORKSPACE_CREATED. A one-shot prompt is DECORATED first:
// the autonomous preamble, the user's words, and the repository's completion
// directive.
func (v *verbs) submitInitialPrompt(ctx context.Context, log dlog.Logger, record wsm.Workspace, spec CreateSpec, policy prompts.Source) error {
	if strings.TrimSpace(spec.InitialPrompt) == "" {
		log.Debug(opCreate, "created without an initial prompt", nil)
		return nil
	}
	text := spec.InitialPrompt
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
