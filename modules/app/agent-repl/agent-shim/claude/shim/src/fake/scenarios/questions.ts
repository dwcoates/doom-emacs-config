/**
 * fake/scenarios/questions.ts — AskUserQuestion, in every outcome.
 *
 * # A question is a gated tool call, and that is not an accident
 *
 * `AgentQuestionId` IS the AskUserQuestion call's `tool_use_id`, so the mock
 * asks through the SAME `canUseTool` callback a permission uses. The gate
 * distinguishes the two by tool name; the question's answers ride back as the
 * allow's `updatedInput`, which is the vendor's own mechanism for a rewritten
 * tool input.
 *
 * # The corpus's answer shape is a MAP, not a list
 *
 * `tool-results/ask_user_question.jsonl` answers with
 * `{questions: [...], answers: {"<question text>": "<label>"}}` — keyed by the
 * question's own prose. A consumer that expected positional answers would
 * silently mis-associate a multi-question batch, which is why the multi-select
 * scenario asks TWO questions.
 *
 * # Free text and "unanswered" are flagged, not observed
 *
 * The corpus has no sample of a free-text answer (the vendor's automatic
 * "Other" option) and NO sample of an unanswered or expired question, and
 * `sdk.d.ts` declares no question timeout at all. Those two scenarios use the
 * closest declared shapes and are named in the report as unsettled.
 */
import { conclude, scenario } from "./support.js";
import type { ScenarioContext, ToolCall } from "../scenario.js";

interface FakeQuestion {
  question: string;
  header: string;
  options: { label: string; description: string }[];
  multiSelect: boolean;
}

async function ask(ctx: ScenarioContext, questions: FakeQuestion[]): Promise<{
  call: ToolCall;
  answers: Record<string, unknown>;
}> {
  const call = ctx.toolUse("AskUserQuestion", { questions });
  const decision = await ctx.canUseTool(
    "AskUserQuestion",
    { questions },
    {
      signal: new AbortController().signal,
      toolUseID: call.toolUseId,
      requestId: `req_question_${call.toolUseId}`,
      title: questions[0]?.question ?? "",
      displayName: "Ask",
    },
  );
  // The answers come back as the allow's `updatedInput` — the vendor's way of
  // handing a rewritten tool input to the model. A deny means the batch went
  // unanswered, which is a different outcome, not a missing one.
  const answers =
    decision?.behavior === "allow" && decision.updatedInput !== undefined
      ? ((decision.updatedInput as { answers?: Record<string, unknown> }).answers ?? {})
      : {};
  ctx.log({ answered: Object.keys(answers).length, behavior: decision?.behavior ?? "none" }, "fake question resolved");
  return { call, answers };
}

const ASK_SINGLE = scenario({
  name: "ask-single",
  prompt: "!ask-single",
  emits: "one single-select `AskUserQuestion` with four options, asked through the shim's own gate",
  writes: "the tool_use line, the tool_result line carrying `questions` and the `answers` map, the closing text line",
  arms: "AgentQuestion.start choices=single_select + AgentQuestionSuccess.outcome=answered",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "ask-single" }, "fake single-select question turn");
    const questions: FakeQuestion[] = [
      {
        question: "How do you want the new branch set up?",
        header: "Setup",
        options: [
          { label: "New worktree off master", description: "Leaves the master checkout untouched." },
          { label: "Switch this checkout", description: "Moves the live checkout onto the new branch." },
          { label: "Reuse the existing branch", description: "Continues where the last run stopped." },
          { label: "Do not branch", description: "Work directly on the current branch." },
        ],
        multiSelect: false,
      },
    ];
    const { call, answers } = await ask(ctx, questions);
    ctx.toolResult(call, "Answered.", { questions, answers });
    conclude(ctx, "The setup question was answered.");
  },
});

const ASK_MULTI = scenario({
  name: "ask-multi",
  prompt: "!ask-multi",
  emits:
    "a TWO-question batch: one multi-select and one single-select, so the answer map has to be keyed by the " +
    "question's own text rather than by position",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentQuestion.choices=multi_select alongside single_select in one batch",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "ask-multi" }, "fake multi-select question turn");
    const questions: FakeQuestion[] = [
      {
        question: "Which suites should run?",
        header: "Suites",
        options: [
          { label: "Unit", description: "The vitest unit suites." },
          { label: "Integration", description: "The shim.v1 integration suite." },
          { label: "Elisp", description: "The batch ert suites." },
        ],
        multiSelect: true,
      },
      {
        question: "Run them now?",
        header: "When",
        options: [
          { label: "Now", description: "Start immediately." },
          { label: "After the merge", description: "Wait for the branch to land." },
        ],
        multiSelect: false,
      },
    ];
    const { call, answers } = await ask(ctx, questions);
    ctx.toolResult(call, "Answered.", { questions, answers });
    conclude(ctx, "Both questions were answered.");
  },
});

const ASK_FREE_TEXT = scenario({
  name: "ask-free",
  prompt: "!ask-free",
  emits:
    "a single-select question answered with FREE TEXT rather than a listed label — the vendor's automatic " +
    "\"Other\" option. NO corpus sample exists for this shape; the answer map simply carries prose no option matches",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentQuestionAnswers carrying free text — the residue rule's subject",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "ask-free" }, "fake free-text question turn");
    const questions: FakeQuestion[] = [
      {
        question: "Which model should the sweep use?",
        header: "Model",
        options: [
          { label: "Fake Opus", description: "The default." },
          { label: "Fake Haiku", description: "The fast one." },
        ],
        multiSelect: false,
      },
    ];
    const { call, answers } = await ask(ctx, questions);
    const withFreeText = {
      ...answers,
      "Which model should the sweep use?":
        (answers["Which model should the sweep use?"] as string | undefined) ??
        "whichever one is cheapest today",
    };
    ctx.toolResult(call, "Answered in free text.", { questions, answers: withFreeText });
    conclude(ctx, "The question was answered in free text.");
  },
});

const ASK_UNANSWERED = scenario({
  name: "ask-unanswered",
  prompt: "!ask-unanswered",
  emits:
    "a question the user never answers: the gate's DENY becomes an error tool_result and the batch ends " +
    "unanswered. `sdk.d.ts` declares NO question timeout, so an expiry is modeled as this same denial and the " +
    "gap is recorded rather than invented",
  writes: "the tool_use line, the error tool_result line, the closing text line",
  arms: "AgentQuestionSuccess.outcome=unanswered",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "ask-unanswered" }, "fake unanswered-question turn");
    const questions: FakeQuestion[] = [
      {
        question: "Should I keep going?",
        header: "Continue",
        options: [
          { label: "Yes", description: "Carry on." },
          { label: "No", description: "Stop here." },
        ],
        multiSelect: false,
      },
    ];
    const call = ctx.toolUse("AskUserQuestion", { questions });
    const decision = await ctx.canUseTool(
      "AskUserQuestion",
      { questions },
      {
        signal: new AbortController().signal,
        toolUseID: call.toolUseId,
        requestId: `req_question_${call.toolUseId}`,
        title: "Should I keep going?",
        displayName: "Ask",
      },
    );
    const message = decision?.behavior === "deny" ? decision.message : "The question went unanswered.";
    ctx.log({ behavior: decision?.behavior ?? "none" }, "fake question ended unanswered");
    ctx.toolResult(call, `Error: ${message}`, { questions, answers: {} }, { isError: true });
    conclude(ctx, "Nobody answered the question.");
  },
});

export const QUESTION_SCENARIOS = [ASK_SINGLE, ASK_MULTI, ASK_FREE_TEXT, ASK_UNANSWERED];
