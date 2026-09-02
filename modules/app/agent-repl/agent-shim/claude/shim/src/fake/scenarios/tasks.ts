/**
 * fake/scenarios/tasks.ts — the task board (TaskCreate/Update) and SendMessage.
 *
 * # The DAG links are the reason `!task-create` creates two tasks
 *
 * `AgentTaskCreated` carries `blocks`/`blockedBy` edges. A scenario that
 * created one task could not produce an edge at all, so this one creates two
 * and links them — the second blocked by the first.
 *
 * # SendMessage has TWO deliveries and they are not interchangeable
 *
 * `queued_to_live` means the recipient was already running and takes the
 * message at its next tool round. `resumed_recipient` means it was NOT running
 * and the vendor resumed it from its transcript — the corpus result carries
 * `resumedAgentId` in exactly that case and omits it in the other, which is the
 * only discriminator either plane gets.
 */
import { conclude, scenario } from "./support.js";

const TASK_CREATE = scenario({
  name: "task-create",
  prompt: "!task-create",
  emits: "two `TaskCreate` calls and a `TaskUpdate` that links the second as blocked by the first",
  writes: "the tool_use and tool_result lines for all three calls, the closing text line",
  arms: "AgentTaskAct.act=created (twice) with the DAG edge",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "task-create" }, "fake task-create turn");
    const first = ctx.toolUse("TaskCreate", { subject: "Land the converter", description: "the fold" });
    ctx.toolResult(first, "Created task 1", { task: { id: "1", subject: "Land the converter" } });
    const second = ctx.toolUse("TaskCreate", { subject: "Land the store writer", description: "rows" });
    ctx.toolResult(second, "Created task 2", { task: { id: "2", subject: "Land the store writer" } });
    const link = ctx.toolUse("TaskUpdate", { taskId: "2", blockedBy: ["1"] });
    ctx.toolResult(link, "Updated task 2", { success: true, taskId: "2", updatedFields: ["blockedBy"] });
    conclude(ctx, "Created two tasks and linked them.");
  },
});

const TASK_CHANGE = scenario({
  name: "task-change",
  prompt: "!task-change",
  emits: "a `TaskUpdate` answered with the corpus's `statusChange` shape (`from`/`to`)",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentTaskAct.act=changed with status pending→running",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "task-change" }, "fake task-change turn");
    const call = ctx.toolUse("TaskUpdate", { taskId: "1", status: "in_progress" });
    ctx.toolResult(call, "Updated task 1", {
      success: true,
      taskId: "1",
      updatedFields: ["status"],
      statusChange: { from: "pending", to: "in_progress" },
    });
    conclude(ctx, "Moved the task to in progress.");
  },
});

const TASK_REJECT = scenario({
  name: "task-reject",
  prompt: "!task-reject",
  emits: "a `TaskUpdate` the board REFUSES, answered with `success: false` and an `error`",
  writes: "the tool_use line, the error tool_result line, the closing text line",
  arms: "AgentTaskAct.act=rejected",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "task-reject" }, "fake task-reject turn");
    const call = ctx.toolUse("TaskUpdate", { taskId: "9", status: "completed" });
    ctx.toolResult(
      call,
      "Error: no task with id 9",
      { success: false, taskId: "9", updatedFields: [], error: "no task with id 9" },
      { isError: true },
    );
    conclude(ctx, "The board rejected the update.");
  },
});

const SEND_MESSAGE_QUEUED = scenario({
  name: "send-message",
  prompt: "!send-message",
  emits: "a `SendMessage` to a LIVE agent, answered WITHOUT `resumedAgentId` — the message queues for its next tool round",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentSendMessage.delivery=queued_to_live",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "send-message-queued" }, "fake send-message (queued) turn");
    const call = ctx.toolUse("SendMessage", { to: "a1234567890abcde", summary: "check the branch" });
    ctx.toolResult(call, "Message queued for the running agent.", {
      success: true,
      message: 'Message queued for agent "a1234567890abcde"; it will be delivered at its next tool round.',
      pin: { id: "a1234567890abcde", name: "a1234567890abcde", ref: "2175c2" },
    });
    conclude(ctx, "Queued the message for the live agent.");
  },
});

const SEND_MESSAGE_RESUMED = scenario({
  name: "send-message-resumed",
  prompt: "!send-message-resumed",
  emits:
    "a `SendMessage` to an IDLE agent, answered WITH `resumedAgentId` and an output-file path — the vendor " +
    "resumed it from its transcript in the background",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentSendMessage.delivery=resumed_recipient",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "send-message-resumed" }, "fake send-message (resumed) turn");
    const agentId = ctx.mintAgentTaskId();
    const call = ctx.toolUse("SendMessage", { to: agentId, summary: "resume the sweep" });
    ctx.toolResult(call, "Agent resumed from transcript.", {
      success: true,
      message:
        `Agent "${agentId}" had no active task; resumed from transcript in the background with your message. ` +
        `You'll be notified when it finishes. Output: ${ctx.files.spoolPathFor(agentId)}`,
      // THE discriminator. Present only on the resumed delivery.
      resumedAgentId: agentId,
      pin: { id: agentId, name: agentId, ref: "2175c2" },
    });
    conclude(ctx, "Resumed the idle agent with the message.");
  },
});

const SEND_MESSAGE_REFUSED = scenario({
  name: "send-message-refused",
  prompt: "!send-message-refused",
  emits: "a `SendMessage` to an agent the user stopped, answered with `success: false` and the vendor's refusal prose",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentSendMessageFailure",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "send-message-refused" }, "fake send-message (refused) turn");
    const call = ctx.toolUse("SendMessage", { to: "a85a6434719755df1", summary: "continue" });
    ctx.toolResult(
      call,
      "The agent was stopped by the user.",
      {
        success: false,
        message:
          "Agent a85a6434719755df1 was stopped by the user and won't be resumed. Treat its work as cancelled; " +
          "only launch a new agent if the user explicitly asks.",
      },
      { isError: true },
    );
    conclude(ctx, "The agent was stopped and cannot be resumed.");
  },
});

export const TASK_SCENARIOS = [
  TASK_CREATE,
  TASK_CHANGE,
  TASK_REJECT,
  SEND_MESSAGE_QUEUED,
  SEND_MESSAGE_RESUMED,
  SEND_MESSAGE_REFUSED,
];
