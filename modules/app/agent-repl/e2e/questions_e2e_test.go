// questions_e2e_test.go — AskUserQuestion, §C #88-92 (SPEC.md).
//
// # The contract point this area exists to protect
//
// `shim.md` §"The permission gate": "ONE vendor gate exists (canUseTool):
// every tool passes through it, and AskUserQuestion is a tool riding the same
// gate. Mechanism shared, meaning not: a QUESTION's "allow" is answer
// transport, a PERMISSION's allow IS consent — two conversation.v1 units
// (`AgentQuestion`, `AgentPermission`), each with its own identity space (a
// permission's id IS the gated unit's `AgentActivityId` on purpose — consent
// joins to the work it gates; a question joins to no unit and has its own
// id)." `conversation/v1/question.proto`'s own doc comment on
// `AgentQuestion.id` restates it: "WHICH ASK... Its OWN identity space: a
// question joins to no unit of work, unlike a permission, whose identity is
// the tool unit it gates." Every test below therefore answers a question by
// its OWN `frontend.v1.FeedId` (the row `AgentQuestion` renders as, via
// `AnswerQuestionRequest.question`), never by any `AgentActivityId` /
// gated-unit identity — there is none to join to.
//
// # Grounding
//
// All five goldens this file drives (`question-free-text`,
// `question-multi-select`, `question-multiple-in-one-batch`,
// `question-single-select`, `question-unanswered`) are REAL, captured vendor
// recordings (`agent-shim/claude/shim/testdata/captures/MANIFEST.md`, rows
// dated 2026-09-01, `ok: true`) — none is UNGROUNDED/INVENTED/DECLARED-ONLY.
// They are driven through the fake SDK's `src/fake/scenarios/questions.ts`,
// which documents itself as asking through the SAME `canUseTool` callback a
// permission uses ("the mock ASKS; the shim's gate DECIDES" — see
// `permissions.ts`'s file doc, the pattern `questions.ts` shares). That file
// declares only FOUR named scenarios for the FIVE golden captures
// (`!ask-single`, `!ask-multi`, `!ask-free`, `!ask-unanswered` —
// `agent-shim/claude/shim/AGENTS.md`'s generated prompt table, lines
// 272-275); `!ask-multi`'s own doc comment ("a TWO-question batch: one
// multi-select and one single-select, so the answer map has to be keyed by
// the question's own text rather than by position") is deliberately built to
// carry BOTH the `question-multi-select` fact (a multi-select question) and
// the `question-multiple-in-one-batch` fact (more than one question posed
// together) at once, so TestQuestionMultiSelect and
// TestQuestionMultipleInOneBatch both drive `!ask-multi`, asserting
// different facts from independent runs of the same scenario — this is a
// deliberate reuse the scenario file's own comment documents, not a writer's
// guess.
//
// # Wire shapes asserted
//
// `conversation/v1/question.proto` (the shim's own production of the ask)
// and `frontend/v1/feed.proto`'s `FeedQuestion` family (the daemon's resolved
// render of it, `FeedRow.question`, tag 11) agree on the same skeleton: a
// batch of one-to-four questions, each with EITHER a single_select or a
// multi_select set of options, each option identified by its label (the
// producer's own echo key — `AgentQuestionOptionLabel`, restated as
// `FeedQuestionOptionLabel`), and the whole batch's `AgentQuestionText`
// (`FeedQuestionText`) as the echo key for WHICH question an answer answers
// ("THE PRODUCER'S OWN KEY IS THIS TEXT... a consumer COPIES it back verbatim
// and never edits it"). Every assertion below reads the OPEN row's batch
// shape from `frontend.v1`, answers through `agentrepl.v1`'s
// `AnswerQuestionRequest` (`endpoint_answer_question.proto`), and re-reads
// the SAME row's settled state (`FeedQuestionAnswered`/`FeedQuestionExpired`)
// — never the shim's or daemon's internal representation.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Helpers local to this file. World/SubmitPrompt/AwaitTurnEnded/DefaultTimeout
// come from world_test.go (this package's shared harness surface, never
// edited here). The open-row / settled-row lookups below are a
// question-specific variant of world_test.go's own AwaitTurnEnded pattern
// (fresh OpenFeed, check the already-served page first, fall back to the
// watch stream) — duplicated locally rather than added to world_test.go,
// which this area writer does not touch.
// ---------------------------------------------------------------------------

// newQuestionWorkspace registers a fresh scripted-fake-git repository (the
// e2e suite mocks every external dependency — git included, via
// harness.NewRepo's scripted fixture; the AskUserQuestion gate this file
// exercises lives entirely in the shim/daemon/store, never in git) as a
// workspace on a fresh World, and answers both.
func newQuestionWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return w, ws
}

// qAwaitFeedRow opens the workspace's feed fresh and answers the first row
// (from the already-served page, or from the watch stream if it has not
// arrived yet) satisfying pred.
func qAwaitFeedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if pred(row) {
			return row
		}
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	return harness.AwaitView(t, w.Ctx(), stream, what, pred)
}

// awaitOpenQuestion waits for a FeedQuestion row whose batch's first question
// reads firstQuestionText, still in its FeedQuestionOpen state.
func awaitOpenQuestion(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, firstQuestionText string) *frontendv1.FeedRow {
	t.Helper()
	return qAwaitFeedRow(t, w, ws, "the \""+firstQuestionText+"\" question to open", func(row *frontendv1.FeedRow) bool {
		q := row.GetQuestion()
		if q == nil || q.GetOpen() == nil {
			return false
		}
		items := q.GetQuestions()
		return len(items) > 0 && items[0].GetText().GetText() == firstQuestionText
	})
}

// awaitSettledQuestion waits for the SAME upsert-keyed row (by FeedId) to
// carry a non-open state — either FeedQuestionAnswered or FeedQuestionExpired.
func awaitSettledQuestion(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, id *frontendv1.FeedId) *frontendv1.FeedRow {
	t.Helper()
	return qAwaitFeedRow(t, w, ws, "question "+id.GetValue()+" to settle", func(row *frontendv1.FeedRow) bool {
		if row.GetId().GetValue() != id.GetValue() {
			return false
		}
		q := row.GetQuestion()
		return q != nil && q.GetOpen() == nil
	})
}

// answerQuestion submits an AnswerQuestionRequest and fails the test loudly
// on anything but AnswerQuestionSuccess — every test in this file expects its
// answer to be accepted; a refusal here is a test bug, not an assertable arm.
func answerQuestion(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, question *frontendv1.FeedId, answers ...*agentreplv1.AnswerQuestionAnswer) {
	t.Helper()
	resp, err := w.Client().AnswerQuestion(w.Ctx(), connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
		Workspace: ws,
		Question:  question,
		Answers:   answers,
	}))
	if err != nil {
		t.Fatalf("AnswerQuestion: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerQuestion = %v, want success", resp.Msg)
	}
}

// givenAnswer finds the FeedQuestionGivenAnswer matching a question's header
// (the settled row's own re-stated chip, FeedQuestionGivenAnswer.header) —
// never by position, mirroring the wire's own "echo by text" contract.
func givenAnswer(t *testing.T, answered *frontendv1.FeedQuestionAnswered, header string) *frontendv1.FeedQuestionGivenAnswer {
	t.Helper()
	for _, a := range answered.GetAnswers() {
		if a.GetHeader().GetText() == header {
			return a
		}
	}
	t.Fatalf("no given answer for header %q in %v", header, answered)
	return nil
}

// ---------------------------------------------------------------------------
// #89 QuestionSingleSelect — question-single-select.
//
// `!ask-single` (questions.ts ASK_SINGLE): one single-select question, four
// options, asked through the shim's own gate. arms (per the scenario's own
// doc field): "AgentQuestion.start choices=single_select +
// AgentQuestionSuccess.outcome=answered".
// ---------------------------------------------------------------------------

func TestQuestionSingleSelect(t *testing.T) {
	t.Parallel()
	w, ws := newQuestionWorkspace(t)
	const question = "How do you want the new branch set up?"
	const chosenLabel = "New worktree off master"

	turn := SubmitPrompt(t, w, ws, "!ask-single")

	open := awaitOpenQuestion(t, w, ws, question)
	items := open.GetQuestion().GetQuestions()
	if len(items) != 1 {
		t.Fatalf("ask-single batch = %d questions, want 1", len(items))
	}
	single := items[0].GetSingleSelect()
	if single == nil {
		t.Fatalf("ask-single question = %v, want single_select", items[0])
	}
	if len(single.GetOptions()) != 4 {
		t.Fatalf("ask-single options = %d, want 4", len(single.GetOptions()))
	}
	var sawChosen bool
	for _, opt := range single.GetOptions() {
		if opt.GetLabel().GetText() == chosenLabel {
			sawChosen = true
		}
	}
	if !sawChosen {
		t.Fatalf("ask-single options = %v, want one labeled %q", single.GetOptions(), chosenLabel)
	}

	answerQuestion(t, w, ws, open.GetId(), &agentreplv1.AnswerQuestionAnswer{
		QuestionText: question,
		Chosen:       []string{chosenLabel},
	})

	settled := awaitSettledQuestion(t, w, ws, open.GetId())
	answered := settled.GetQuestion().GetAnswered()
	if answered == nil {
		t.Fatalf("ask-single settled as %v, want FeedQuestionAnswered", settled.GetQuestion())
	}
	given := givenAnswer(t, answered, items[0].GetHeader().GetText())
	if want := []string{chosenLabel}; len(given.GetChosen()) != 1 || given.GetChosen()[0] != want[0] {
		t.Errorf("ask-single given.Chosen = %v, want %v", given.GetChosen(), want)
	}
	if given.GetOtherText() != nil {
		t.Errorf("ask-single given.OtherText = %v, want unset (a listed choice, not free text)", given.GetOtherText())
	}

	AwaitTurnEnded(t, w, ws, turn)
}

// TestAnOpenQuestionIsNamedAndGreenOnTheStripAndTheRail pins the owner's
// ruling of 2026-10-08: while a question gate stands in a running turn, the
// footer's status is `question` and the roster row's arm is `question` (both
// green in render-colors.json), never `working` / `thinking`.
func TestAnOpenQuestionIsNamedAndGreenOnTheStripAndTheRail(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newQuestionWorkspace(t)
	const question = "How do you want the new branch set up?"
	footer := w.WatchFooter(ws)
	defer footer.Close()
	roster := w.WatchRoster()
	defer roster.Close()

	// Act
	turn := SubmitPrompt(t, w, ws, "!ask-single")
	open := awaitOpenQuestion(t, w, ws, question)

	// Assert
	harness.AwaitView(t, w.Ctx(), footer.Stream, "the strip to name the question gate", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetQuestion() != nil
	})
	harness.AwaitView(t, w.Ctx(), roster, "the row to name the question gate", func(r *frontendv1.WorkspaceRoster) bool {
		return raArm(r, ws.GetId()) == "question"
	})
	answerQuestion(t, w, ws, open.GetId(), &agentreplv1.AnswerQuestionAnswer{
		QuestionText: question,
		Chosen:       []string{"New worktree off master"},
	})
	AwaitTurnEnded(t, w, ws, turn)
}

// ---------------------------------------------------------------------------
// #92 QuestionUnanswered — question-unanswered.
//
// GROUNDING: the golden itself is a real, captured vendor recording (see this
// file's header) whose own terminal is `success.completed`
// (MANIFEST.md: "question-unanswered | ... | success.completed"). The FAKE
// scenario reproduces the shape by having the shim's `canUseTool` gate
// resolve DENY, per its own doc comment: "the gate's DENY becomes an error
// tool_result and the batch ends unanswered... an expiry is modeled as this
// same denial".
//
// OPEN QUESTION (flagging rather than guessing, per this area's binding
// instruction): the only DAEMON-API-LEVEL lever this suite found for
// resolving an open question's gate as DENY without answering it is the one
// `shim.md` documents generically for "every teardown path — interrupt,
// shutdown, SDK abort" (§"What the shim IS", the PERMISSION-CALLBACK
// LIVENESS bullet): "resolves ALL pending permission callbacks (as denied)
// before proceeding." `AnswerQuestionRequest` itself
// (endpoint_answer_question.proto) has NO deny arm — only real answers — so
// this test uses the daemon's `Interrupt` rpc (target: turn) as that
// documented teardown path. Whether the turn that follows reaches
// `success.completed` (as the real capture did — captured, presumably,
// against a DIFFERENT denial path this suite has no rpc-level equivalent
// for) or `success.interrupted` (Interrupt's own ordinary outcome) is NOT
// verified here — verifying it would mean reading the shim's/daemon's
// production translation of an interrupt-during-an-open-question, which this
// area's instructions forbid. This test therefore asserts only what is
// structurally certain from the proto alone: the question settles OUT of its
// open state, and — since `FeedQuestion.state` has exactly three arms
// (open/answered/expired) and nobody answered — it must be
// `FeedQuestionExpired`, never `FeedQuestionAnswered`. If the project lead's
// run instead shows the turn's terminal disagreeing with the capture's
// `success.completed`, that is this open question resolving, not a
// production defect this test should paper over.
// ---------------------------------------------------------------------------
