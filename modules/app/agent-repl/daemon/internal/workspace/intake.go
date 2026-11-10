package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// opIntake is the operation an intake workspace's creation is recorded under,
// and opIntakeRetire the one its retirement is.
const (
	opIntake       = "daemon.workspace.intake"
	opIntakeRetire = "daemon.workspace.intake_retire"
)

// BriefQuestionIntake is the brief a feed card's `?` question hands its
// intake workspace (prompts/question-intake.md).
const BriefQuestionIntake = "question-intake"

// IntakeSpec is one EPHEMERAL INTAKE WORKSPACE to create: a hidden workspace
// with a fresh context whose single job is to work out what a request is and
// hand it on, by answering the workspace that asked or by creating the
// workspace that does the work. It does none of that work itself.
//
// ONE HELPER SERVES EVERY INTAKE (a card's `?` question, a Slack emoji ack),
// so what an intake workspace guarantees is stated once:
//
//   - it is created with its brief as its initial prompt, or not at all: a
//     brief that will not compose creates nothing, and a create that fails
//     after its worktree was made undoes itself (Create);
//   - it is recorded as an intake workspace and hidden before its session
//     exists (wsm.MarkIntake), so no turn of it ever ends unmarked;
//   - the daemon retires it when a turn of it ends (retireIntake), whatever
//     its agent did or did not do.
type IntakeSpec struct {
	// RepoDir is the repository the intake workspace is cut from, which is
	// the one it explores.
	RepoDir string
	// Name is the workspace's name, the caller's: unique per request, so two
	// intakes never contend for one branch.
	Name string
	// Model is the model its session starts under.
	Model string
	// Brief names the prompt file it is handed (prompts/<Brief>.md), and
	// Values fills that brief's placeholders.
	Brief  string
	Values map[string]string
}

// CreateIntake implements Verbs. The brief's failures are recorded by
// composeBrief under opIntake, and the create's by Create.
func (v *verbs) CreateIntake(ctx context.Context, spec IntakeSpec) (wsm.Workspace, error) {
	log := v.deps.Log.Global().With(dlog.Context{
		"repo_dir": spec.RepoDir, "name": spec.Name, "model": spec.Model, "brief": spec.Brief,
	})
	prompt, err := v.composeBrief(ctx, log, opIntake, spec.Brief, spec.Values)
	if err != nil {
		return wsm.Workspace{}, fmt.Errorf("create the intake workspace %q: %w", spec.Name, err)
	}
	created, err := v.Create(ctx, CreateSpec{
		// AN INTAKE WORKSPACE IS NO EMOJI-ACK WORKSPACE, even one made for an
		// ack: the kind belongs to the workspace that does the work.
		Originator:    lifecycleconfig.OriginatorUserOrWorkspace,
		RepoDir:       spec.RepoDir,
		InitialPrompt: prompt,
		Name:          spec.Name,
		Model:         spec.Model,
		Intake:        true,
	})
	if err != nil {
		return wsm.Workspace{}, fmt.Errorf("create the intake workspace %q: %w", spec.Name, err)
	}
	log.Info(opIntake, "created an intake workspace and handed it its brief", dlog.Context{
		"workspace": string(created.ID), "dir": created.Dir,
	})
	return created, nil
}

// AskQuestionIntake implements Verbs: QUESTION, a feed card's `?` question
// quoted with the person's words, goes from ASKER to an intake workspace
// created for it in the repository agent-repl's own checkout is in. The
// intake acknowledges ASKER, then answers it or creates the workspace that
// makes the change asked for (prompts/question-intake.md).
func (v *verbs) AskQuestionIntake(ctx context.Context, asker ids.WorkspaceID, question string) (wsm.Workspace, error) {
	record, log, err := v.owned(ctx, "SubmitPrompt", asker)
	if err != nil {
		return wsm.Workspace{}, err
	}
	home, err := v.deps.Git.MainWorktree(ctx, v.deps.CheckoutRoot)
	if err != nil {
		log.Error(opIntake, "git could not name the main worktree of agent-repl's own checkout, where a question's intake workspace is cut", dlog.WithCause(dlog.Context{"checkout": v.deps.CheckoutRoot}, err))
		return wsm.Workspace{}, fmt.Errorf("ask about a card of %s: %w", asker, err)
	}
	return v.CreateIntake(ctx, IntakeSpec{
		RepoDir: home,
		// THE INSTANT NAMES IT: one workspace's questions are taken one at a
		// time, so no two of its intakes share an instant.
		Name:  "question-intake-" + strconv.FormatInt(v.now().UnixNano(), 36),
		Model: DefaultLaunchModel,
		Brief: BriefQuestionIntake,
		Values: map[string]string{
			"asker":    record.Name,
			"question": question,
		},
	})
}

// retireIntake retires the intake workspace WS at the end of a turn of it:
// its session stopped, its worktree and branch destroyed, the workspace
// closed by the daemon. Its record stays, so its feed can still be read.
//
// IT RUNS AT EVERY TURN END OF AN INTAKE WORKSPACE (CarryOutSelfClose), so a
// retirement something blocks (work still running in the background, a held
// prompt) is tried again when the turn that work wakes has ended, and one a
// git failure cut short is finished by the next.
func (v *verbs) retireIntake(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, opIntakeRetire, ws)
	if err != nil {
		return err
	}
	fields := dlog.Context{"dir": record.Dir, "branch": record.Branch}
	if _, err := os.Stat(record.Dir); errors.Is(err, fs.ErrNotExist) {
		log.Debug(opIntakeRetire, "the intake workspace's worktree is already gone; it was retired before", fields)
		return nil
	} else if err != nil {
		log.Error(opIntakeRetire, "could not read the intake workspace's worktree; it is kept", dlog.WithCause(fields, err))
		return fmt.Errorf("retire the intake workspace %q: stat %s: %w", ws, record.Dir, err)
	}
	blocked, err := v.closeBlocker(ctx, ws)
	if err != nil {
		log.Error(opIntakeRetire, "could not judge whether the intake workspace is quiet; it is kept", dlog.WithCause(fields, err))
		return fmt.Errorf("retire the intake workspace %q: %w", ws, err)
	}
	if blocked != nil {
		fields["blocked"] = blocked.Reason
		log.Info(opIntakeRetire, "the intake workspace's turn ended with something still standing; it is retired at a later turn's end", fields)
		return nil
	}
	// THE CLOSE IS RECORDED BEFORE THE SESSION DIES, as a kill's is: a closed
	// workspace's session is never brought back.
	if err := v.deps.DB.MarkClosed(ctx, ws, wsm.Closure{At: v.now(), By: wsm.CloseByDaemon}); err != nil {
		log.Error(opIntakeRetire, "could not record the intake workspace's close; it is kept", dlog.WithCause(fields, err))
		return fmt.Errorf("retire the intake workspace %q: record closed: %w", ws, err)
	}
	v.republishRegistry(ctx, log, opIntakeRetire)
	if err := v.kill(ctx, log, ws, wsm.CloseByDaemon); err != nil {
		log.Error(opIntakeRetire, "could not stop the intake workspace's session; it is closed and its worktree is kept", dlog.WithCause(fields, err))
		return fmt.Errorf("retire the intake workspace %q: %w", ws, err)
	}
	if err := v.destroyStorage(ctx, log, record); err != nil {
		log.Error(opIntakeRetire, "could not destroy the intake workspace's worktree and branch; it is closed and they are kept", dlog.WithCause(fields, err))
		return fmt.Errorf("retire the intake workspace %q: %w", ws, err)
	}
	log.Info(opIntakeRetire, "retired the intake workspace: its session stopped, its worktree and branch destroyed", fields)
	return nil
}
