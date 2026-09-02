/**
 * fake/scenarios/subagents.ts — the Agent tool, synchronous and detached.
 *
 * # `forwardSubagentText` is the whole reason the sync scenario emits anything
 *
 * Without that option a subagent's prose and reasoning never reach the shim at
 * all (shim.md, "Gotchas"). With it, the subagent's assistant and user messages
 * arrive on the SAME stream as the main agent's, distinguished only by
 * `agent_id` / `parent_tool_use_id` / `subagent_type` / `task_description`. The
 * sync scenario emits exactly those, so a converter that ignored the
 * attribution would fold a subagent's work into the main conversation.
 *
 * # The `.meta.json` is not optional decoration
 *
 * A subagent's TYPE, description, spawning call and spawn depth exist ONLY in
 * `agent-<id>.meta.json`. Ingesting transcripts alone cannot recover them, so
 * every scenario here writes the sidecar before it writes a single line.
 *
 * # A detached agent's spool is agent JSONL, not text
 *
 * The corpus's `spools/agent.output` is the agent's own transcript, line for
 * line — not prose and not an `EXIT=` terminated shell spool. The detached
 * scenario writes it that way, which is what lets the sidecar ingest a
 * detached agent's work at all.
 */
import { askPermission, conclude, scenario } from "./support.js";
import { FAKE_SIGNATURE } from "./support.js";

/** The `AgentOutput` a completed synchronous subagent answers with. */
function completedAgentOutput(fields: {
  agentId: string;
  agentType: string;
  prompt: string;
  report: string;
}): Record<string, unknown> {
  return {
    status: "completed",
    prompt: fields.prompt,
    agentId: fields.agentId,
    agentType: fields.agentType,
    content: [{ type: "text", text: fields.report }],
    resolvedModel: "fake-sonnet-5",
    modelsUsed: ["fake-sonnet-5"],
    totalDurationMs: 228_159,
    totalTokens: 54_975,
    totalToolUseCount: 2,
    usage: {
      input_tokens: 2,
      output_tokens: 3_200,
      cache_creation_input_tokens: 1_707,
      cache_read_input_tokens: 50_066,
      server_tool_use: { web_search_requests: 0, web_fetch_requests: 0 },
      service_tier: "standard",
      cache_creation: { ephemeral_1h_input_tokens: 0, ephemeral_5m_input_tokens: 1_707 },
      inference_geo: "not_available",
      speed: "standard",
    },
    toolStats: {
      readCount: 1,
      searchCount: 0,
      bashCount: 1,
      editFileCount: 0,
      linesAdded: 0,
      linesRemoved: 0,
      otherToolCount: 0,
    },
  };
}

export const SUBAGENT_SYNC = scenario({
  name: "subagent",
  prompt: "!subagent",
  emits:
    "an `Agent` tool_use, then the SUBAGENT's own assistant and user messages carrying `agent_id`, " +
    "`parent_tool_use_id`, `subagent_type` and `task_description`, then the completed `AgentOutput`",
  writes:
    "`<session>/subagents/agent-<id>.meta.json` and `agent-<id>.jsonl` (the subagent's own chained " +
    "sidechain transcript), plus the main transcript's tool_use and tool_result lines",
  arms: "AgentSubagent.start + AgentSubagentUpdate (nested activity) + AgentSubagentSuccess with full usage",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "subagent-sync" }, "fake synchronous-subagent turn");
    const description = ctx.args === "" ? "Explore the module" : ctx.args;
    const agentPrompt = "Read the module's AGENTS.md and report the test command.";
    const call = ctx.toolUse("Agent", {
      description,
      prompt: agentPrompt,
      subagent_type: "general-purpose",
    });
    const agentId = ctx.mintAgentTaskId();
    const agent = {
      agentId,
      parentToolUseId: call.toolUseId,
      subagentType: "general-purpose",
      taskDescription: description,
    };
    const writer = ctx.files.subagent(agentId);
    writer.writeMeta({
      agentType: "general-purpose",
      description,
      toolUseId: call.toolUseId,
      spawnDepth: 1,
    });
    // The subagent's own first user record IS its commission — the only place
    // a workflow or subagent's prompt is ever recorded.
    writer.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: { role: "user", content: agentPrompt },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.assistant(
      [
        { type: "thinking", thinking: "", signature: FAKE_SIGNATURE },
        { type: "text", text: "Reading the module's conventions." },
      ],
      { agent, model: "fake-sonnet-5" },
    );
    const nested = ctx.toolUse("Read", { file_path: `${ctx.cwd}/AGENTS.md` }, { agent, model: "fake-sonnet-5" });
    ctx.toolResult(nested, "npm test", {
      type: "text",
      file: { filePath: `${ctx.cwd}/AGENTS.md`, content: "npm test", numLines: 1, startLine: 1, totalLines: 1 },
    }, { agent });
    const report = "The module's test command is `npm test`.";
    ctx.assistant([{ type: "text", text: report }], { agent, model: "fake-sonnet-5", stopReason: "end_turn" });
    ctx.toolResult(
      call,
      report,
      completedAgentOutput({ agentId, agentType: "general-purpose", prompt: agentPrompt, report }),
    );
    conclude(ctx, "The subagent reported back.");
  },
});

export const SUBAGENT_DETACHED = scenario({
  name: "subagent-detached",
  prompt: "!subagent-detached",
  emits:
    "an `Agent` with `run_in_background`: `task_started`, `background_tasks_changed`, an `async_launched` " +
    "`AgentOutput` naming the output file, then a completed `task_notification` carrying usage",
  writes:
    "the agent's `.meta.json` and `agent-<id>.jsonl`, the spool " +
    "`<spool-root>/<slug>/<session>/tasks/a<hex>.output` written as AGENT JSONL, and the main transcript's lines",
  arms: "AgentSubagent detached_work + AgentSubagentSuccess.usage=total_only from the notification",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "subagent-detached" }, "fake detached-subagent turn");
    const description = ctx.args === "" ? "Pointless background sweep" : ctx.args;
    const agentPrompt = "Do the sweep and report.";
    const call = ctx.toolUse("Agent", {
      description,
      prompt: agentPrompt,
      subagent_type: "general-purpose",
      run_in_background: true,
    });
    const agentId = ctx.mintAgentTaskId();
    // A detached agent's task id IS its agent id (corpus: `a0cbd94e5da2d662d`
    // appears as both), which is what makes the spool, the transcript file and
    // the task stream address the same thing.
    ctx.startTask({ taskId: agentId, toolUseId: call.toolUseId, kind: "local_agent", description });
    ctx.announceLiveTasks();
    const writer = ctx.files.subagent(agentId);
    writer.writeMeta({
      agentType: "general-purpose",
      description,
      toolUseId: call.toolUseId,
      spawnDepth: 1,
    });
    writer.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: { role: "user", content: agentPrompt },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.toolResult(
      call,
      `Async agent launched successfully.\nagentId: ${agentId}`,
      {
        isAsync: true,
        status: "async_launched",
        agentId,
        description,
        resolvedModel: "fake-sonnet-5",
        prompt: agentPrompt,
        outputFile: ctx.files.spoolPathFor(agentId),
        canReadOutputFile: true,
      },
    );
    conclude(ctx, "Dispatched the agent to the background.");
    // The agent's work lands AFTER the turn ended — that is what detached means.
    await ctx.tick();
    ctx.assistant([{ type: "text", text: "Sweep finished." }], {
      agent: {
        agentId,
        parentToolUseId: call.toolUseId,
        subagentType: "general-purpose",
        taskDescription: description,
      },
      model: "fake-sonnet-5",
      stopReason: "end_turn",
    });
    // The spool is the agent's OWN transcript, verbatim — the corpus's
    // `spools/agent.output` is agent JSONL, not prose and not an EXIT= file.
    ctx.files.spool(agentId).append(writer.read());
    ctx.endTask(agentId);
    ctx.systemMessage("task_notification", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      status: "completed",
      output_file: ctx.files.spoolPathFor(agentId),
      summary: "Sweep finished.",
      // The ONLY usage a detached agent ever reports: three totals, no
      // per-model breakdown. `AgentSubagentAsyncUsage` exists for exactly this.
      usage: { total_tokens: 11_114, tool_uses: 2, duration_ms: 1_403 },
    });
    ctx.announceLiveTasks();
  },
});

export const SUBAGENT_DETACHED_LIVE = scenario({
  name: "subagent-detached-live",
  prompt: "!subagent-detached-live",
  emits:
    "a detached `Agent` that is left LIVE after the turn ends and then raises its OWN gated call: the " +
    "`canUseTool` ask carries the subagent's `agentID`. Nothing here ever finishes the agent — only a " +
    "`stopTask` does, which is what makes a stop targeted at a subagent's AgentId observable",
  writes: "the agent's `.meta.json` and `agent-<id>.jsonl`, its spool, and the main transcript's lines",
  arms:
    "AgentSubagent detached_work left live, an AgentPermission raised UNDER the subagent, and " +
    "AgentSubagentFailure.cause=stopped_by_user when the stop lands",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "subagent-detached-live" }, "fake LIVE detached-subagent turn");
    const description = "A sweep that keeps running";
    const agentPrompt = "Sweep until told to stop.";
    const call = ctx.toolUse("Agent", {
      description,
      prompt: agentPrompt,
      subagent_type: "general-purpose",
      run_in_background: true,
    });
    const agentId = ctx.mintAgentTaskId();
    ctx.startTask({ taskId: agentId, toolUseId: call.toolUseId, kind: "local_agent", description });
    ctx.announceLiveTasks();
    const writer = ctx.files.subagent(agentId);
    writer.writeMeta({
      agentType: "general-purpose",
      description,
      toolUseId: call.toolUseId,
      spawnDepth: 1,
    });
    writer.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: { role: "user", content: agentPrompt },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.toolResult(call, `Async agent launched successfully.\nagentId: ${agentId}`, {
      isAsync: true,
      status: "async_launched",
      agentId,
      description,
      prompt: agentPrompt,
      outputFile: ctx.files.spoolPathFor(agentId),
      canReadOutputFile: true,
    });
    ctx.files.spool(agentId).append(writer.read());
    conclude(ctx, "Dispatched a long-running agent to the background.");
    // The gated call belongs to the AGENT and lands AFTER the turn ended, which
    // is what detached means. Never awaited here: only the consumer's answer or
    // the shim's stand-down resolves it, and the agent stays live either way.
    await ctx.tick();
    const gated = ctx.toolUse("Bash", { command: "rm -rf ./scratch" });
    void askPermission(ctx, gated, {
      agentID: agentId,
      title: "The background agent wants to run rm -rf ./scratch",
    });
  },
});

export const SUBAGENT_FAILED = scenario({
  name: "subagent-failed",
  prompt: "!subagent-failed",
  emits: "a detached `Agent` that ends in failure: `task_updated{status:\"failed\"}` and a failed `task_notification`",
  writes: "the agent's `.meta.json` and transcript, its spool, and the main transcript's lines",
  arms: "AgentSubagentFailure",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "subagent-failed" }, "fake failing detached-subagent turn");
    const call = ctx.toolUse("Agent", {
      description: "A sweep that will fail",
      prompt: "Fail.",
      subagent_type: "general-purpose",
      run_in_background: true,
    });
    const agentId = ctx.mintAgentTaskId();
    ctx.startTask({
      taskId: agentId,
      toolUseId: call.toolUseId,
      kind: "local_agent",
      description: "A sweep that will fail",
    });
    ctx.announceLiveTasks();
    const writer = ctx.files.subagent(agentId);
    writer.writeMeta({
      agentType: "general-purpose",
      description: "A sweep that will fail",
      toolUseId: call.toolUseId,
      spawnDepth: 1,
    });
    writer.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: { role: "user", content: "Fail." },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.toolResult(call, `Async agent launched successfully.\nagentId: ${agentId}`, {
      isAsync: true,
      status: "async_launched",
      agentId,
      description: "A sweep that will fail",
      prompt: "Fail.",
      outputFile: ctx.files.spoolPathFor(agentId),
      canReadOutputFile: true,
    });
    conclude(ctx, "Dispatched an agent that will fail.");
    await ctx.tick();
    ctx.files.spool(agentId).append(writer.read());
    ctx.endTask(agentId);
    ctx.systemMessage("task_updated", { task_id: agentId, patch: { status: "failed", error: "agent raised" } });
    ctx.systemMessage("task_notification", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      status: "failed",
      output_file: ctx.files.spoolPathFor(agentId),
      summary: "The agent raised.",
      usage: { total_tokens: 400, tool_uses: 0, duration_ms: 120 },
    });
    ctx.announceLiveTasks();
  },
});

export const CANCEL_ALL = scenario({
  name: "cancel-all",
  prompt: "!cancel-all",
  emits:
    "THREE detached items launched in one turn — two agents and a shell — left LIVE. The cancel is the caller's " +
    "`stopTask` per item; emptying the live set makes the engine write the vendor's `agents_killed` record",
  writes: "both agents' `.meta.json` and transcripts, the shell's spool, and the main transcript's lines",
  arms: "the fan-wide cancel: AgentSubagentFailure.cause=stopped_by_user per item, plus the agents_killed record",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "cancel-all" }, "fake fan-wide-cancel setup turn");
    for (const description of ["Fan item one", "Fan item two"]) {
      const call = ctx.toolUse("Agent", {
        description,
        prompt: description,
        subagent_type: "general-purpose",
        run_in_background: true,
      });
      const agentId = ctx.mintAgentTaskId();
      ctx.startTask({ taskId: agentId, toolUseId: call.toolUseId, kind: "local_agent", description });
      const writer = ctx.files.subagent(agentId);
      writer.writeMeta({ agentType: "general-purpose", description, toolUseId: call.toolUseId, spawnDepth: 1 });
      writer.append({
        promptId: ctx.newUuid(),
        type: "user",
        message: { role: "user", content: description },
        uuid: ctx.newUuid(),
        timestamp: ctx.nowIso(),
      });
      ctx.toolResult(call, `Async agent launched successfully.\nagentId: ${agentId}`, {
        isAsync: true,
        status: "async_launched",
        agentId,
        description,
        prompt: description,
        outputFile: ctx.files.spoolPathFor(agentId),
        canReadOutputFile: true,
      });
      ctx.files.spool(agentId).append(writer.read());
    }
    const shell = ctx.toolUse("Bash", { command: "sleep 100000", run_in_background: true });
    const shellTask = ctx.mintShellTaskId();
    ctx.startTask({
      taskId: shellTask,
      toolUseId: shell.toolUseId,
      kind: "local_bash",
      description: "sleep 100000",
    });
    ctx.toolResult(shell, "", {
      stdout: "",
      stderr: "",
      interrupted: false,
      isImage: false,
      noOutputExpected: false,
      backgroundTaskId: shellTask,
    });
    ctx.files.spool(shellTask).appendLine("running");
    ctx.announceLiveTasks();
    conclude(ctx, "Three detached items are live.");
    await ctx.tick();
    // THE SCENARIO STOPS HERE, deliberately. The cancel itself is the caller's
    // `stopTask` per live item; the vendor's `agents_killed` record is written
    // by the engine when the last one goes, because that record states a fact
    // about the SET emptying and no single stop can know it.
  },
});

export const SUBAGENT_SCENARIOS = [
  SUBAGENT_SYNC,
  SUBAGENT_DETACHED,
  SUBAGENT_DETACHED_LIVE,
  SUBAGENT_FAILED,
  CANCEL_ALL,
];
