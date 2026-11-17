package merge

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE STANDING DEPLOY CHECKS (owner ruling, 2026-11-17). A repository whose
// queue checks its deploys keeps ONE intake, ONE remediation and ONE
// escalation workspace across every deploy, never one per check: each role's
// conversation carries what the earlier checks found, tried and settled, and
// is never cleared, only compacted (compactStanding). The daemon prompts the
// standing workspace for each check; it makes one only for a role the queue
// has none in, and reopens one that was closed (a remediation's fix deploys
// and closes it; the user may close any).

// standingNames is each role's workspace name. A repository holds each name
// once; a closed standing workspace keeps it, and is reopened.
var standingNames = map[DeployCheckRole]string{
	DeployCheckIntake:      "deploy-intake",
	DeployCheckRemediation: "deploy-remediation",
	DeployCheckEscalation:  "deploy-escalation",
}

// standingRole is ROLE as the store spells it. A role outside the three is a
// caller's defect and panics.
func standingRole(role DeployCheckRole) wsm.DeployCheckRoleName {
	switch role {
	case DeployCheckIntake:
		return wsm.StandingIntake
	case DeployCheckRemediation:
		return wsm.StandingFixer
	case DeployCheckEscalation:
		return wsm.StandingFixer
	}
	panic(fmt.Sprintf("merge: no standing role for %v", role))
}

// standing is a queue's workspace in one role, as a check finds it.
type standing struct {
	ws ids.WorkspaceID
	// briefed is whether it was prompted in its role before.
	briefed bool
	// made is whether this check made it: a made intake's or
	// remediation's effort is set once the check records its role (intakeOf,
	// dispatch).
	made bool
}

// standingWorkspace answers queue Q's standing workspace in ROLE: the one
// recorded, reopened when it was closed, or one made and recorded when the
// queue has none (or the one recorded is gone from the registry). A made
// workspace's role and effort are set before it answers.
func (o *orchestrator) standingWorkspace(ctx context.Context, q checkedQueue, role DeployCheckRole, fields dlog.Context) (standing, error) {
	fields = withField(fields, "role", role.String())
	recorded, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, standingRole(role))
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the queue's standing deploy check workspace", withField(fields, "error", err.Error()))
		return standing{}, err
	}
	if found {
		record, err := o.deps.DB.Workspace(ctx, recorded.Workspace)
		switch {
		case errors.Is(err, wsm.ErrNotFound):
			o.deps.Log.Global().Info(opMediation, "the standing deploy check workspace is gone from the registry; a new one is made", withField(fields, "gone", string(recorded.Workspace)))
		case err != nil:
			o.deps.Log.Global().Error(opMediation, "could not read the standing deploy check workspace", withField(withField(fields, "workspace", string(recorded.Workspace)), "error", err.Error()))
			return standing{}, err
		case record.Closed:
			if err := o.deps.DeployChecks.Reopen(ctx, recorded.Workspace); err != nil {
				o.deps.Log.Global().Error(opMediation, "the standing deploy check workspace could not be reopened", withField(withField(fields, "workspace", string(recorded.Workspace)), "error", err.Error()))
				return standing{}, fmt.Errorf("merge: reopen the standing %s workspace %s: %w", role, recorded.Workspace, err)
			}
			o.deps.Log.Global().Info(opMediation, "reopened the standing deploy check workspace", withField(fields, "workspace", string(recorded.Workspace)))
			return standing{ws: recorded.Workspace, briefed: recorded.Briefed}, nil
		default:
			return standing{ws: recorded.Workspace, briefed: recorded.Briefed}, nil
		}
	}
	made, err := o.makeStanding(ctx, q, role, fields)
	if err != nil {
		return standing{}, err
	}
	return standing{ws: made, made: true}, nil
}

// makeStanding makes queue Q's standing workspace in ROLE and records it,
// with its effort set: an escalation's finish lands directly, recorded
// before anything prompts it (RecordDeployEscalation). An intake's and a
// remediation's effort is left to their caller: setting it starts the
// workspace's shim, whose landing reads the role the check records after
// this (intakeOf, dispatch).
func (o *orchestrator) makeStanding(ctx context.Context, q checkedQueue, role DeployCheckRole, fields dlog.Context) (ids.WorkspaceID, error) {
	name := standingNames[role]
	fields = withField(fields, "name", name)
	var (
		made wsm.Workspace
		err  error
	)
	switch role {
	case DeployCheckIntake:
		made, err = o.deps.DeployChecks.CreateDeployIntake(ctx, q.repository.Dir, name)
	case DeployCheckRemediation:
		made, err = o.deps.DeployChecks.CreateDeployRemediation(ctx, q.repository.Dir, name)
	case DeployCheckEscalation:
		made, err = o.deps.DeployChecks.CreateDeployEscalation(ctx, q.repository.Dir, name)
	default:
		panic(fmt.Sprintf("merge: no standing workspace for %v", role))
	}
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the standing deploy check workspace could not be made", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: make the standing %s workspace %s: %w", role, name, err)
	}
	fields = withField(fields, "workspace", string(made.ID))
	if err := o.deps.DB.RecordStandingDeployCheck(ctx, q.key, standingRole(role), made.ID); err != nil {
		o.deps.Log.Global().Error(opMediation, "the standing deploy check workspace was made but could not be recorded", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: record the standing %s workspace %s: %w", role, made.ID, err)
	}
	if role == DeployCheckEscalation {
		if err := o.deps.DB.RecordDeployEscalation(ctx, made.ID, q.key, o.deps.Now()); err != nil {
			o.deps.Log.Global().Error(opMediation, "the standing escalation could not be recorded as landing directly", withField(fields, "error", err.Error()))
			return "", fmt.Errorf("merge: record the standing escalation %s: %w", made.ID, err)
		}
	}
	if role != DeployCheckEscalation {
		o.deps.Log.Global().Info(opMediation, "made the queue's standing deploy check workspace; its effort is set once the check records its role", fields)
		return made.ID, nil
	}
	if err := o.deps.DeployChecks.SetDeployCheckEffort(ctx, made.ID, role); err != nil {
		o.deps.Log.Global().Error(opMediation, "the standing deploy check workspace's effort could not be set", withField(fields, "error", err.Error()))
		return "", fmt.Errorf("merge: set the standing %s workspace %s's effort: %w", role, made.ID, err)
	}
	o.deps.Log.Global().Info(opMediation, "made the queue's standing deploy check workspace", fields)
	return made.ID, nil
}

// markBriefed records queue Q's standing workspace in ROLE briefed in its
// role, once its first brief is submitted. A failure is recorded and left:
// the next brief states the role in full again, which is no harm.
func (o *orchestrator) markBriefed(ctx context.Context, q checkedQueue, role DeployCheckRole, fields dlog.Context) {
	if err := o.deps.DB.MarkStandingBriefed(ctx, q.key, standingRole(role)); err != nil {
		o.deps.Log.Global().Error(opMediation, "the standing deploy check workspace was briefed but could not be recorded so; its next brief states its role in full again", withField(withField(fields, "role", role.String()), "error", err.Error()))
	}
}

// noteCheckWork records that check work came for queue Q (a dispatch or an
// escalation): its quiet run starts over, and its standing workspaces owe a
// compaction once the next quiet run completes.
func (o *orchestrator) noteCheckWork(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	if err := o.deps.DB.NoteDeployCheckWork(ctx, q); err != nil {
		o.deps.Log.Global().Error(opMediation, "could not record the check's work; the standing workspaces' compaction is not armed by it", withField(fields, "error", err.Error()))
	}
}

// noteQuietCheck records that a check of queue Q ended with no dispatch and
// no escalation, and compacts its standing workspaces when that completes
// an armed quiet run (owner ruling, 2026-11-17).
func (o *orchestrator) noteQuietCheck(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	compact, err := o.deps.DB.NoteQuietDeployCheck(ctx, q)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not count the quiet check; the standing workspaces are not compacted by it", withField(fields, "error", err.Error()))
		return
	}
	if compact {
		o.compactStanding(ctx, q, fields)
	}
}

// compactStanding owes each of queue Q's standing workspaces a compaction of
// its conversation by the cold gate's summarizer (Haiku) at its next session
// start: a conversation is never cleared, only compacted, once a quiet run
// follows work. The intake and the remediation stand down between checks,
// so their next brief starts them; a session still up compacts when it next
// starts.
func (o *orchestrator) compactStanding(ctx context.Context, q wsm.RepoKey, fields dlog.Context) {
	for _, role := range wsm.StandingDeployCheckRoles {
		roleFields := withField(fields, "role", string(role))
		recorded, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q, role)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read a standing workspace to compact; it is not compacted", withField(roleFields, "error", err.Error()))
			continue
		}
		if !found {
			continue
		}
		roleFields = withField(roleFields, "workspace", string(recorded.Workspace))
		owed, err := o.deps.DB.OweCompaction(ctx, recorded.Workspace)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not owe a standing workspace its compaction; it is not compacted", withField(roleFields, "error", err.Error()))
			continue
		}
		o.deps.Log.Global().Info(opMediation, "the quiet run after the checks' work owes the standing workspace a compaction at its next start", withField(roleFields, "owed", owed))
	}
}

// adoptStanding makes the workspace check M has at work in each role queue Q
// has no standing workspace in yet the standing one, briefed (owner ruling,
// 2026-11-17): a queue whose checks made a workspace per check before the
// standing ones keeps the workspaces at work, with their conversations, and
// a held escalation goes on holding the queue in the workspace the user
// already works in.
func (o *orchestrator) adoptStanding(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	if m == nil {
		return
	}
	for _, at := range []struct {
		role wsm.DeployCheckRoleName
		ws   ids.WorkspaceID
	}{{wsm.StandingIntake, m.Intake}, {wsm.StandingFixer, m.Remediation}, {wsm.StandingFixer, m.Escalation}} {
		if at.ws == "" {
			continue
		}
		fields := dlog.Context{"repo": string(q.key), "role": string(at.role), "workspace": string(at.ws)}
		_, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, at.role)
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read whether the queue has a standing workspace to adopt the check's into", withField(fields, "error", err.Error()))
			continue
		}
		if found {
			continue
		}
		if err := o.deps.DB.RecordStandingDeployCheck(ctx, q.key, at.role, at.ws); err != nil {
			o.deps.Log.Global().Error(opMediation, "could not adopt the check's workspace as the queue's standing one", withField(fields, "error", err.Error()))
			continue
		}
		if err := o.deps.DB.MarkStandingBriefed(ctx, q.key, at.role); err != nil {
			o.deps.Log.Global().Error(opMediation, "adopted the check's workspace as the standing one but could not record it briefed", withField(fields, "error", err.Error()))
			continue
		}
		o.deps.Log.Global().Info(opMediation, "adopted the check's workspace as the queue's standing one", fields)
	}
}

// retireOneOffEscalations closes every escalation workspace made for queue Q
// before its standing one, once the standing one exists (owner ruling,
// 2026-11-17), except the one check M is held by: the standing escalation
// owns what they owned from then on, and an escalation still holding the
// queue is the user's to release. Each is closed at its running turn's end.
func (o *orchestrator) retireOneOffEscalations(ctx context.Context, q checkedQueue, m *wsm.Mediation) {
	fields := dlog.Context{"repo": string(q.key)}
	standing, found, err := o.deps.DB.StandingDeployCheckOf(ctx, q.key, wsm.StandingFixer)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the standing escalation; no earlier escalation is retired", withField(fields, "error", err.Error()))
		return
	}
	if !found {
		return
	}
	escalations, err := o.deps.DB.DeployEscalationsOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not list the queue's escalations; no earlier escalation is retired", withField(fields, "error", err.Error()))
		return
	}
	for _, ws := range escalations {
		if ws == standing.Workspace || (m != nil && ws == m.Escalation) {
			continue
		}
		wsFields := withField(fields, "escalation", string(ws))
		record, err := o.deps.DB.Workspace(ctx, ws)
		if errors.Is(err, wsm.ErrNotFound) {
			continue
		}
		if err != nil {
			o.deps.Log.Global().Error(opMediation, "could not read an earlier escalation; it is not retired", withField(wsFields, "error", err.Error()))
			continue
		}
		if record.Closed {
			continue
		}
		if _, err := o.deps.DeployChecks.CloseSelf(ctx, ws); err != nil {
			o.deps.Log.Global().Error(opMediation, "an earlier escalation could not be retired", withField(wsFields, "error", err.Error()))
			continue
		}
		o.deps.Log.Global().Info(opMediation, "retired an escalation made before the queue's standing one", wsFields)
	}
}
