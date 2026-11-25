package workspace

import (
	"context"
	"fmt"
	"slices"

	"agentrepl/logging/logsite"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// THE DEPLOY MEDIATOR (repoconfig.v1.LifecycleConfig.deploy_checks; owner
// ruling, 2026-11-17). A repository whose queue checks its deploys keeps ONE
// standing workspace that checks every deploy, fixes what it dispatches,
// and settles an escalation with the user. It is made with no initial
// prompt, so no session comes up at the create, and recorded as its queue's
// standing mediator before it is briefed: its session first comes up for the
// brief's delivery, at the effort picked for it (SetMediatorAgent),
// so it always comes up under the mediator's landing (sessionProfile).

// opDeployChecks is the operation the deploy mediator's verbs are recorded
// under.
const opDeployChecks = "daemon.workspace.deploy_checks"

// MediatorAgent is how a standing mediator's session runs: its model and its
// effort, read from its repository's lifecycle config at each brief
// (MediatorAgentOf).
type MediatorAgent struct {
	Model  string
	Effort conversationv1.AgentEffortLevel
}

// mediatorEffort is a mediator's effort when its config names none (owner
// ruling, 2026-11-17: "an opus low-effort agent").
const mediatorEffort = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW

// MediatorAgentOf is how a mediator configured by CFG runs: the model and
// effort CFG names, else the daemon's default launch model at low effort.
func MediatorAgentOf(cfg *repoconfigv1.MediatorAgent) MediatorAgent {
	agent := MediatorAgent{Model: DefaultLaunchModel, Effort: mediatorEffort}
	if cfg != nil && cfg.Model != nil {
		agent.Model = cfg.GetModel()
	}
	if cfg != nil && cfg.Effort != nil {
		agent.Effort = cfg.GetEffort()
	}
	return agent
}

// mediatorSpec is the create of the standing mediator of ROLE of the
// repository at REPODIR: shown, since it stands in its repository's section
// for good (owner ruling, 2026-11-19), on MODEL, with no initial prompt. Its
// name and branch are the role's name (wsm.MediatorRole.Name), which no other
// workspace may take (IsStandingName).
func mediatorSpec(repoDir string, role wsm.MediatorRole, model string) CreateSpec {
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.Name, spec.Model = repoDir, role.Name(), model
	spec.standing = true
	return spec
}

// CreateMediator implements Verbs: makes a repository's standing mediator,
// unprompted, on the model CFG configures (MediatorAgentOf).
//
// IT IS MADE IN REMOTE CONTROL (owner ruling, 2026-11-16): the user settles
// an escalation from wherever they are, so its mode is recorded on before
// its first session starts, which puts the mode in force at bring-up.
func (v *verbs) CreateMediator(ctx context.Context, repoDir string, role wsm.MediatorRole, cfg *repoconfigv1.MediatorAgent) (wsm.Workspace, error) {
	spec := mediatorSpec(repoDir, role, MediatorAgentOf(cfg).Model)
	fields := dlog.Context{"repo": spec.RepoDir, "name": spec.Name, "model": spec.Model}
	made, err := v.Create(ctx, spec)
	if err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the standing mediator could not be made", dlog.WithCause(fields, err))
		return wsm.Workspace{}, fmt.Errorf("make the standing mediator %q: %w", spec.Name, err)
	}
	if _, err := v.deps.DB.SetRemoteControlMode(ctx, made.ID, wsm.RemoteControlOn); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the standing mediator was made but its remote control could not be turned on", dlog.WithCause(withWorkspace(fields, made.ID), err))
		return wsm.Workspace{}, fmt.Errorf("turn remote control on for the standing mediator %q: %w", spec.Name, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "made the standing mediator", withWorkspace(fields, made.ID))
	return made, nil
}

// SetMediatorAgent implements Verbs: puts WS on the model and effort CFG
// configures (MediatorAgentOf)
// before a brief, so an edit of the repository's lifecycle config takes
// effect at the mediator's next brief.
//
// THE MODEL IS RECORDED FIRST (wsm.SetSessionModel), which every start of the
// session names; only then is a live session switched, through the queue
// (SetModel), so the switch never overtakes the brief. A model the live
// session's catalog does not offer is refused at ERROR, never swapped for
// another.
//
// THE EFFORT IS A RECORDED PICK FIRST (Topbar.SetPickedEffort), which every
// session start puts back in force (Fleet.reapplyEffort, from the cold
// bring-up and a rollout's relaunch alike); only then is a live session
// switched. So the mediator's shim being absent, stood down, mid-replacement
// or unreachable never fails a check's brief: the session the brief's
// delivery starts comes up at the pick. 2026-11-21 12:56:28: the switch met
// a shim the rollout had just stood down, the round failed, and the deploy's
// check was never told.
//
// THE ROUND NEVER WAITS ON THE EFFORT'S SWITCH. A live session is switched
// off the round's goroutine (Sessions.SwitchEffort): the shim holds a switch
// asked mid-turn until the turn ends, which has no bound (2026-11-21
// 13:55:20: a round stood behind the mediator's running turn for minutes),
// and the brief the round submits is queued behind that same turn.
//
// A REAL FAILURE STAYS AN ERROR: a level the live session's selector never
// offered is refused here, before any pick, and a vendor's refusal of the
// switch is recorded at ERROR by the fleet.
func (v *verbs) SetMediatorAgent(ctx context.Context, ws wsm.WorkspaceID, cfg *repoconfigv1.MediatorAgent) error {
	agent := MediatorAgentOf(cfg)
	if err := v.setMediatorModel(ctx, ws, agent.Model); err != nil {
		return err
	}
	return v.setMediatorEffort(ctx, ws, agent.Effort)
}

// setMediatorModel records MODEL as WS's session model when it is not
// already, and switches a live session to it.
func (v *verbs) setMediatorModel(ctx context.Context, ws wsm.WorkspaceID, model string) error {
	fields := dlog.Context{"workspace": string(ws), "model": model}
	session, _, err := v.deps.DB.Session(ctx, ws)
	if err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the standing mediator's recorded model could not be read", dlog.WithCause(fields, err))
		return fmt.Errorf("read the standing mediator %s's model: %w", ws, err)
	}
	if session.Model == model {
		return nil
	}
	fields["was"] = session.Model
	if err := v.deps.DB.SetSessionModel(ctx, ws, model); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the standing mediator's model could not be recorded", dlog.WithCause(fields, err))
		return fmt.Errorf("record the standing mediator %s's model: %w", ws, err)
	}
	if _, live := v.deps.Cards.Models(ws); !live {
		v.deps.Log.Global().Info(opDeployChecks, "recorded the standing mediator's model; its session starts on it", fields)
		return nil
	}
	if err := v.SetModel(ctx, ws, model); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the standing mediator's live session could not be switched to its model", dlog.WithCause(fields, err))
		return fmt.Errorf("switch the standing mediator %s's model: %w", ws, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "switched the standing mediator's live session to its model before its brief", fields)
	return nil
}

// setMediatorEffort puts WS at EFFORT. A sessionless workspace's shim starts
// for it, so its standing is recorded before.
func (v *verbs) setMediatorEffort(ctx context.Context, ws wsm.WorkspaceID, effort conversationv1.AgentEffortLevel) error {
	fields := dlog.Context{"workspace": string(ws), "effort": effort.String()}
	// A WORKSPACE WITH NO SESSION YET SERVES NO EFFORT SELECTOR, so its level
	// is only picked: setting it through the selector refused every fresh
	// intake (2026-11-15 16:17, 0.444.0: "serves no effort level for its
	// model") and left the queue held for it.
	served, live := v.deps.Cards.EffortLevels(ws)
	if !live {
		v.deps.Topbar.SetPickedEffort(ws, effort)
		v.deps.Log.Global().Info(opDeployChecks, "picked the standing mediator's effort; its session starts at it", fields)
		return nil
	}
	// unset records the effort's failure, ERR, once for every way it fails.
	unset := func(err error) error {
		v.deps.Log.Global().Error(opDeployChecks, "the standing mediator's effort could not be set", dlog.WithCause(fields, err))
		return fmt.Errorf("set the standing mediator %s's effort: %w", ws, err)
	}
	if !slices.Contains(served, effort) {
		return unset(fmt.Errorf("the selector served %v and never offered %v", served, effort))
	}
	_, log, err := v.owned(ctx, "SetMediatorAgent", ws)
	if err != nil {
		return unset(err)
	}
	v.deps.Topbar.SetPickedEffort(ws, effort)
	v.deps.Sessions.SwitchEffort(log, ws, effort)
	v.deps.Log.Global().Info(opDeployChecks, "picked the standing mediator's effort; its live session is switched to it before its next turn", fields)
	return nil
}

// CompactDeployCheck implements Verbs: compacts standing deploy check WS's
// conversation by the cold gate's summarizer (Haiku) NOW, answering whether
// it did (owner ruling, 2026-11-17): the compaction is owed to the session's
// next start and the live session ended (oweCompaction), and a session is
// started again at once to carry it out, so no later check waits on it. A
// workspace with no conversation has nothing to compact, and one running a
// turn (the user's, in the fixer) is never cut: its next start compacts it.
// The start runs the summarizer, about a minute on a long conversation, so
// it is never asked beside a rollout that may hand WS to a successor
// (OweDeployCheckCompaction).
func (v *verbs) CompactDeployCheck(ctx context.Context, ws wsm.WorkspaceID) (bool, error) {
	log, ended, err := v.oweCompaction(ctx, "CompactDeployCheck", ws)
	if err != nil || !ended {
		return false, err
	}
	if err := v.deps.Sessions.Start(shimlife.WithStart(ctx, shimlife.StartCompaction), ws); err != nil {
		log.Error(opDeployChecks, "the deploy check workspace's session could not start to compact", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("compact the deploy check %s: start the compacting session: %w", ws, err)
	}
	log.Info(opDeployChecks, "compacted the deploy check workspace's conversation; its session runs on the summary", nil)
	return true, nil
}

// OweDeployCheckCompaction implements Verbs: owes standing deploy check WS's
// compaction to its session's next start and ends the live session, starting
// none, answering whether the compaction stands owed with no session running
// (2026-11-22). It is CompactDeployCheck without the start: a remediation's
// deploy hands WS to a successor about 30s after it starts, which cut every
// compacting start beside it (the summarizer takes about 70s on the
// mediator's conversation) and logged three false errors, so the start is
// left to the session's next bring-up, the successor's after a handover or
// the next brief's when the deploy replaces no daemon. Nothing is in flight
// for a handover to cut. A workspace with no conversation has nothing to
// compact, and one running a turn is never cut: its next start compacts it.
func (v *verbs) OweDeployCheckCompaction(ctx context.Context, ws wsm.WorkspaceID) (bool, error) {
	log, ended, err := v.oweCompaction(ctx, "OweDeployCheckCompaction", ws)
	if err != nil || !ended {
		return false, err
	}
	log.Info(opDeployChecks, "owed the deploy check workspace's compaction to its next start; its session is ended", nil)
	return true, nil
}

// oweCompaction is the part CompactDeployCheck and OweDeployCheckCompaction
// share, so the two cannot drift: it owes WS's compaction to its session's
// next start (wsm.OweCompaction) and ends the live session, answering WS's
// logger and whether the compaction is owed with no session running. It
// answers false, ending nothing, when WS has no conversation to compact or
// runs a turn, which is never cut: the compaction stays owed to its next
// start. VERB names the caller in the ownership check.
func (v *verbs) oweCompaction(ctx context.Context, verb string, ws wsm.WorkspaceID) (dlog.Logger, bool, error) {
	_, log, err := v.owned(ctx, verb, ws)
	if err != nil {
		return nil, false, err
	}
	if _, err := v.deps.DB.OweCompaction(ctx, ws); err != nil {
		log.Error(opDeployChecks, "the deploy check workspace's compaction could not be owed", dlog.Context{"cause": err.Error()})
		return nil, false, fmt.Errorf("compact the deploy check %s: owe the compaction: %w", ws, err)
	}
	owed, found, err := v.deps.DB.OwedStartWork(ctx, ws)
	if err != nil {
		log.Error(opDeployChecks, "could not read the deploy check workspace's owed start work", dlog.Context{"cause": err.Error()})
		return nil, false, fmt.Errorf("compact the deploy check %s: read the owed start work: %w", ws, err)
	}
	if !found || !owed.Compact {
		log.Info(opDeployChecks, "the deploy check workspace has no conversation to compact", dlog.Context{"owed": found})
		return log, false, nil
	}
	// A TURN RUNNING IS NEVER CUT FOR A COMPACTION: a turn the user started
	// in the fixer keeps it, and the compaction stays owed to its next start.
	open, err := v.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		log.Error(opDeployChecks, "could not read whether a turn runs in the deploy check workspace; it is not compacted now", dlog.Context{"cause": err.Error()})
		return nil, false, fmt.Errorf("compact the deploy check %s: read its open turns: %w", ws, err)
	}
	if len(open) > 0 {
		log.Info(opDeployChecks, "a turn runs in the deploy check workspace; its compaction waits for its next start", dlog.Context{"open_turns": len(open)})
		return log, false, nil
	}
	if v.deps.Sessions.Live(ws) {
		if _, err := v.endSession(ctx, log, ws); err != nil {
			return nil, false, fmt.Errorf("compact the deploy check %s: end the session: %w", ws, err)
		}
	}
	return log, true, nil
}

// DeployCheckContextTokens implements Verbs: the context chip's figure for
// deploy check WS, which the merge queue weighs against the bound past which
// a quiet check compacts the mediator.
func (v *verbs) DeployCheckContextTokens(ws wsm.WorkspaceID) int64 {
	return v.deps.Topbar.ContextTokens(ws)
}

// AnswerDeployCheckColdGate implements Verbs: answers the cold gate that
// stands on standing deploy check WS with compact-and-resume at once, by the
// cold gate's default summarizer (Haiku), so a held brief goes on without
// the user (owner ruling, 2026-11-17: no deploy check waits on a cold gate).
// A workspace that is no standing deploy check, or has no gate standing, is
// left as it is.
//
// THE ANSWER AWAITS THE ADOPTION rather than reading the standing once: at a
// handover the gate is raised inside the adoption itself (Fleet.AdoptParked
// stands the carried gate) before the rollout marks the workspace owned, so
// a single read refused the answer not_yet_adopted at every handover and the
// gate stood with no answerer (2026-11-26 02:04:57Z handover, the production
// mediator). A deploy check handed to a successor instead is left to the
// daemon that serves it.
func (v *verbs) AnswerDeployCheckColdGate(ctx context.Context, ws wsm.WorkspaceID) error {
	// EVERY GATE RAISED COMES HERE, so a workspace that is no standing deploy
	// check is passed over before any ownership is asked: a handover raises
	// gates again on workspaces this daemon has not adopted yet (2026-11-17
	// 13:24: five not_yet_adopted ERRORs, none a deploy check).
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil || !record.IsMediator() {
		return err
	}
	log := v.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(ws)})
	first, err := v.deps.Ownership.Standing(ctx, ws)
	if err != nil {
		return fmt.Errorf("answer the deploy check %s's cold gate: read its serving standing: %w", ws, err)
	}
	reached, err := v.deps.Ownership.AwaitAdopted(ctx, ws)
	if err != nil {
		return fmt.Errorf("answer the deploy check %s's cold gate: await its adoption: %w", ws, err)
	}
	switch reached {
	case StandingOwned:
		if first == StandingNotYetAdopted {
			log.Info(opDeployChecks, "the gate was raised before this daemon adopted the deploy check; its answer waited for the adoption", nil)
		}
	case StandingTransferringAway:
		log.Info(opDeployChecks, "the deploy check was handed to a successor daemon; its gate's answer is left to the daemon that serves the workspace", dlog.Context{"standing": "transferring_away"})
		return nil
	default:
		return fmt.Errorf("answer the deploy check %s's cold gate: the adoption wait ended at the standing %d", ws, reached)
	}
	_, err = v.answerColdGateByCompaction(ctx, "AnswerDeployCheckColdGate", ws, true,
		logsite.Msg("a cold gate stands on the standing deploy check; it is answered compact-and-resume at once"), nil)
	return err
}

// CompactRequeuedMergeColdGate implements Verbs: answers the cold gate that
// stands on WS, whose merge waits in line at it, by a compaction that keeps
// the prompts held at the gate (owner request, 2026-11-20: a requeue at a
// cold gate compacts and goes in line, where it was refused). The held
// prompts are the user's to send: the merge runs first.
//
// IT ANSWERS ONLY ONCE THE ANSWER'S RE-OPEN HAS SETTLED (owner ruling,
// 2026-11-21), because the queue's one answer is spent by an attempt that
// ended in failure, never by one still compacting. The re-open is a detached
// bring-up whose every step runs under its own measured bound (the adoption
// bound, DefaultSummarizeBound for the compaction, DefaultStartSessionBound
// for the opening), and the daemon's stand-down ends it, so the wait is
// bounded by them and adds no deadline of its own: a slow compaction inside
// its bound is never judged failed here. It answers true when the session
// came up; false with no error when no gate stood or the re-open was
// abandoned (a stand-down, or its shim taken); and the failure otherwise,
// the gate standing again.
func (v *verbs) CompactRequeuedMergeColdGate(ctx context.Context, ws wsm.WorkspaceID) (bool, error) {
	return v.compactColdGateUntilSettled(ctx, "CompactRequeuedMergeColdGate", ws,
		logsite.Msg("a cold gate stands on the workspace of a requeued merge; it is answered by a compaction at once"))
}

// compactColdGateUntilSettled answers the cold gate standing on WS, for VERB,
// by the compaction answerColdGateByCompaction makes, keeping the prompts held
// at the gate and recording WHY, and answers once the answer's re-open has
// settled: true when the session came up, false with no error when no gate
// stood or the re-open was abandoned, and the failure otherwise, the gate
// standing again. The wait adds no deadline of its own (the re-open's steps
// are each bounded); CTX ending stops only the wait.
func (v *verbs) compactColdGateUntilSettled(ctx context.Context, verb string, ws wsm.WorkspaceID, why logsite.Message) (bool, error) {
	settled := make(chan ColdGateAnswerSettled, 1)
	answered, err := v.answerColdGateByCompaction(ctx, verb, ws, false, why,
		func(outcome ColdGateAnswerSettled) { settled <- outcome })
	if err != nil || !answered {
		return false, err
	}
	select {
	case outcome := <-settled:
		switch outcome.Outcome {
		case GateAnswerResumed:
			return true, nil
		case GateAnswerAbandoned:
			return false, nil
		case GateAnswerFailed:
			return false, outcome.Err
		}
		panic(fmt.Sprintf("workspace: a cold gate's re-open settled with the undeclared outcome %q", outcome.Outcome))
	case <-ctx.Done():
		return false, fmt.Errorf("compact the cold gate on %s (%s): the wait for its re-open ended: %w", ws, verb, context.Cause(ctx))
	}
}

// answerColdGateByCompaction answers the cold gate standing on WS, for VERB,
// by a compaction of the whole conversation with the gate's default
// summarizer (Haiku, coldCompactMenu), recording WHY. RESUME sends the
// prompts held at the gate once the session is up; otherwise they stay held.
// SETTLED, when given, is told how the re-open ended (answerColdGate). It
// answers whether a gate stood to answer: a workspace with no gate standing
// is left as it is.
func (v *verbs) answerColdGateByCompaction(ctx context.Context, verb string, ws wsm.WorkspaceID, resume bool, why logsite.Message, settled func(ColdGateAnswerSettled)) (bool, error) {
	_, log, err := v.owned(ctx, verb, ws)
	if err != nil {
		return false, err
	}
	if _, standing := v.deps.Cards.ColdGate(ws); !standing {
		return false, nil
	}
	compact := defaultSummarizerAnswer()
	answer := &agentreplv1.AnswerColdGateRequest{Choice: &agentreplv1.AnswerColdGateRequest_Compact{Compact: compact}}
	if resume {
		answer.Choice = &agentreplv1.AnswerColdGateRequest_CompactAndResume{CompactAndResume: compact}
	}
	log.Log(dlog.LevelInfo, opColdGate, why, dlog.Context{"resume": resume})
	if err := v.answerColdGate(ctx, ws, answer, settled); err != nil {
		return false, err
	}
	return true, nil
}

// RefreshDeployCheck implements Verbs: brings standing fixer WS's worktree
// to its parent branch's tip before a new brief (advanceToParent), so its
// work starts from every fix landed since. A worktree holding uncommitted
// work or commits the main branch lacks stays where it is, and says so.
func (v *verbs) RefreshDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	record, log, err := v.owned(ctx, "RefreshDeployCheck", ws)
	if err != nil {
		return err
	}
	return v.advanceToParent(ctx, log, record, opDeployChecks, "refresh", "standing deploy fixer")
}

// ResumeDeployCheck implements Verbs: shows backlogged deploy check workspace WS
// again, for the brief that sends it back to work.
func (v *verbs) ResumeDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	_, log, err := v.owned(ctx, "ResumeDeployCheck", ws)
	if err != nil {
		return err
	}
	if err := v.showFamily(ctx, log, ws); err != nil {
		return fmt.Errorf("resume the deploy check %s: show: %w", ws, err)
	}
	log.Info(opDeployChecks, "the deploy check workspace is shown again; its brief brings its session up", nil)
	return nil
}

// showFamily shows standing deploy check WS and the workspaces backlogged with
// it: a mediator an earlier build hid, with the workspaces it made (owner
// ruling, 2026-11-17: "hide its kids too").
func (v *verbs) showFamily(ctx context.Context, log dlog.Logger, ws wsm.WorkspaceID) error {
	members, err := v.deps.DB.ShowFamily(ctx, ws)
	if err != nil {
		if refusedByFamily(log, opSetBacklogged, err) {
			return err
		}
		log.Error(opSetBacklogged, "could not show the deploy check workspace with its family", dlog.Context{"cause": err.Error()})
		return err
	}
	log.Info(opSetBacklogged, "showed the deploy check workspace with its family", dlog.Context{"backlogged": false, "family": familyIDs(members)})
	v.familyMoved(ctx, log, ws, members)
	return nil
}

// familyMoved is backloggedRecorded for ROOT and every member moved with it.
func (v *verbs) familyMoved(ctx context.Context, log dlog.Logger, root wsm.WorkspaceID, members []wsm.WorkspaceID) {
	v.backloggedRecorded(ctx, log, root, opSetBacklogged)
	for _, member := range members {
		v.deps.CacheLapses.VisibilityChanged(member)
	}
}

// familyIDs names a family's members for a record.
func familyIDs(members []wsm.WorkspaceID) []string {
	out := make([]string, len(members))
	for i, id := range members {
		out[i] = string(id)
	}
	return out
}

// withWorkspace is FIELDS naming workspace WS.
func withWorkspace(fields dlog.Context, ws wsm.WorkspaceID) dlog.Context {
	fields["workspace"] = string(ws)
	return fields
}
