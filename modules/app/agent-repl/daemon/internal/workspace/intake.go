package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

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
	// Card is the feed card the intake is made for, nil for none (a Slack
	// emoji ack's). It is recorded in the insert's own transaction, so the
	// card's next question always finds this intake (AskAboutCard).
	Card *wsm.IntakeCard
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
	// AN INTAKE WORKSPACE IS NO EMOJI-ACK WORKSPACE, even one made for an
	// ack: the kind belongs to the workspace that does the work.
	create := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	create.RepoDir, create.InitialPrompt, create.Name, create.Model = spec.RepoDir, prompt, spec.Name, spec.Model
	create.Intake, create.IntakeCard = true, spec.Card
	created, err := v.Create(ctx, create)
	if err != nil {
		return wsm.Workspace{}, fmt.Errorf("create the intake workspace %q: %w", spec.Name, err)
	}
	log.Info(opIntake, "created an intake workspace and handed it its brief", dlog.Context{
		"workspace": string(created.ID), "dir": created.Dir,
	})
	return created, nil
}

// CardQuestionSend sends a card's question, as an agent-sent prompt from its
// asker, to TO, the workspace already addressing the card. A refusal is
// answered as REFUSED, and anything that kept the prompt from being judged as
// an error.
type CardQuestionSend func(ctx context.Context, to wsm.Workspace) (refused *agentreplv1.SubmitPromptError, err error)

// CardQuestionAnswer is where AskAboutCard handed a card's question.
type CardQuestionAnswer struct {
	// Recipient is the workspace handed the question: the one already
	// addressing the card, or the intake workspace created for it.
	Recipient wsm.Workspace
	// Refused is the recipient's refusal of the question, nil when it took it.
	// Only a workspace already addressing the card can refuse it.
	Refused *agentreplv1.SubmitPromptError
}

// AskAboutCard implements Verbs: QUESTION, a feed card's `?` question quoted
// with the person's words, goes from the card's asker to the workspace
// already addressing that card, so every question about one card reaches one
// workspace:
//
//   - the newest open workspace an intake made for the card created, which
//     is doing the work the card's first question asked for;
//   - else the card's intake workspace still open, still taking a question in;
//   - else an intake workspace created for it in the repository agent-repl's
//     own checkout is in, recorded with the card. The intake acknowledges the
//     asker, then answers it or creates the workspace that makes the change
//     asked for (prompts/question-intake.md).
//
// A QUESTION SENT TO AN OPEN INTAKE NEVER MEETS ITS RETIREMENT: the lookup
// and the send run under intakeHandoffs, which retireIntake holds from its
// quiet check through its close, so the intake either takes the question
// before it is judged quiet, and is kept for it, or is already closed and is
// no addressee.
func (v *verbs) AskAboutCard(ctx context.Context, card wsm.IntakeCard, question string, send CardQuestionSend) (CardQuestionAnswer, error) {
	record, log, err := v.owned(ctx, "SubmitPrompt", card.Asker)
	if err != nil {
		return CardQuestionAnswer{}, err
	}
	log = log.With(dlog.Context{"row": card.Row})
	answer, handed, err := v.handToAddressee(ctx, log, card, send)
	if err != nil || handed {
		return answer, err
	}
	home, err := v.deps.Git.MainWorktree(ctx, v.deps.CheckoutRoot)
	if err != nil {
		log.Error(opIntake, "git could not name the main worktree of agent-repl's own checkout, where a question's intake workspace is cut", dlog.WithCause(dlog.Context{"checkout": v.deps.CheckoutRoot}, err))
		return CardQuestionAnswer{}, fmt.Errorf("ask about a card of %s: %w", card.Asker, err)
	}
	created, err := v.CreateIntake(ctx, IntakeSpec{
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
		Card: &card,
	})
	if err != nil {
		return CardQuestionAnswer{}, err
	}
	return CardQuestionAnswer{Recipient: created}, nil
}

// handToAddressee sends CARD's question through SEND to the workspace already
// addressing the card, answering false when none does. It holds
// intakeHandoffs throughout (AskAboutCard).
func (v *verbs) handToAddressee(ctx context.Context, log dlog.Logger, card wsm.IntakeCard, send CardQuestionSend) (CardQuestionAnswer, bool, error) {
	v.intakeHandoffs.Lock()
	defer v.intakeHandoffs.Unlock()
	addressees, err := v.deps.DB.CardAddressees(ctx, card)
	if err != nil {
		log.Error(opIntake, "could not read which workspace already addresses the card's questions", dlog.WithCause(nil, err))
		return CardQuestionAnswer{}, false, fmt.Errorf("ask about a card of %s: read its addressees: %w", card.Asker, err)
	}
	to, by := addressees.Work, "work"
	if to == nil {
		to, by = addressees.OpenIntake, "intake"
	}
	if to == nil {
		log.Debug(opIntake, "no workspace addresses the card's questions yet; an intake workspace is created for this one", nil)
		return CardQuestionAnswer{}, false, nil
	}
	recipient, err := v.deps.DB.Workspace(ctx, *to)
	if err != nil {
		log.Error(opIntake, "could not read the workspace already addressing the card's questions", dlog.WithCause(dlog.Context{"recipient": string(*to), "addressee": by}, err))
		return CardQuestionAnswer{}, false, fmt.Errorf("ask about a card of %s: read %s: %w", card.Asker, *to, err)
	}
	fields := dlog.Context{"recipient": string(recipient.ID), "recipient_name": recipient.Name, "addressee": by}
	refused, err := send(ctx, recipient)
	if err != nil {
		log.Error(opIntake, "could not send the card's question to the workspace already addressing it", dlog.WithCause(fields, err))
		return CardQuestionAnswer{}, false, fmt.Errorf("ask about a card of %s: send to %q: %w", card.Asker, recipient.Name, err)
	}
	if refused != nil {
		fields["refusal"] = fmt.Sprintf("%T", refused.GetReason())
		log.Info(opIntake, "the workspace already addressing the card refused its question", fields)
	} else {
		log.Info(opIntake, "sent the card's question to the workspace already addressing it, creating no intake", fields)
	}
	return CardQuestionAnswer{Recipient: recipient, Refused: refused}, true, nil
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
	closed, err := v.closeQuietIntake(ctx, log, ws, fields)
	if err != nil || !closed {
		return err
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

// closeQuietIntake records the intake workspace WS closed when it is quiet,
// answering whether it did; one still busy is kept, to be retired at a later
// turn's end.
//
// THE QUIET CHECK AND THE CLOSE ARE ONE STEP to a card's next question
// (AskAboutCard), under intakeHandoffs: a question handed in between would be
// lost with the session.
func (v *verbs) closeQuietIntake(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, fields dlog.Context) (bool, error) {
	v.intakeHandoffs.Lock()
	defer v.intakeHandoffs.Unlock()
	blocked, err := v.closeBlocker(ctx, ws)
	if err != nil {
		log.Error(opIntakeRetire, "could not judge whether the intake workspace is quiet; it is kept", dlog.WithCause(fields, err))
		return false, fmt.Errorf("retire the intake workspace %q: %w", ws, err)
	}
	if blocked != nil {
		fields["blocked"] = blocked.Reason
		log.Info(opIntakeRetire, "the intake workspace's turn ended with something still standing; it is retired at a later turn's end", fields)
		return false, nil
	}
	// THE CLOSE IS RECORDED BEFORE THE SESSION DIES, as a kill's is: a closed
	// workspace's session is never brought back.
	if err := v.deps.DB.MarkClosed(ctx, ws, wsm.Closure{At: v.now(), By: wsm.CloseByDaemon}); err != nil {
		log.Error(opIntakeRetire, "could not record the intake workspace's close; it is kept", dlog.WithCause(fields, err))
		return false, fmt.Errorf("retire the intake workspace %q: record closed: %w", ws, err)
	}
	return true, nil
}
