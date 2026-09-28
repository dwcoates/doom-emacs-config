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
import { askPermission, conclude, scenario, visibleThinking } from "./support.js";
import { FAKE_SIGNATURE } from "./support.js";
import {
  NETWORK_RESUME_MARKER,
  NETWORK_RESUME_MESSAGE,
  resumePromptTargets,
} from "../../engine/network-resume-prompt.js";

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

const SUBAGENT_SYNC = scenario({
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
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-sync" }, "fake synchronous-subagent turn");
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

const SUBAGENT_DETACHED = scenario({
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
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-detached" }, "fake detached-subagent turn");
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
    // RUNNING BEATS WHILE THE DETACHED AGENT WORKS. The vendor emits
    // `task_progress` messages carrying a RUNNING token sum (with a tool-call
    // count and elapsed wall-clock) as a backgrounded agent runs. They are the
    // ONLY account of a detached agent's spend before it settles — its
    // transcript does not reach this stream — so the footer and its bubble
    // advance incrementally from them and the settled total below supersedes.
    // Each beat states a total that only grows, ending under the notification's.
    await ctx.tick();
    ctx.systemMessage("task_progress", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      description,
      subagent_type: "general-purpose",
      usage: { total_tokens: 4_200, tool_uses: 1, duration_ms: 500 },
      last_tool_name: "Bash",
    });
    await ctx.tick();
    ctx.systemMessage("task_progress", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      description,
      subagent_type: "general-purpose",
      usage: { total_tokens: 8_600, tool_uses: 2, duration_ms: 1_000 },
      last_tool_name: "Read",
    });
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

const SUBAGENT_DETACHED_LIVE = scenario({
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
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-detached-live" }, "fake LIVE detached-subagent turn");
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

const SUBAGENT_DETACHED_HOLD = scenario({
  name: "subagent-detached-hold",
  prompt: "!subagent-detached-hold",
  emits:
    "a detached `Agent` left LIVE, and then the turn that spawned it HOLDS: nothing further until an interrupt " +
    "lands, which ends the turn the way an interrupted turn ends. Whether the agent survives that interrupt is " +
    "the vendor's `perTaskStopAffordance` posture, never the scenario's",
  writes: "the agent's `.meta.json` and `agent-<id>.jsonl`, its spool, and the main transcript's lines",
  arms:
    "AgentSubagent detached_work live UNDER AN OPEN TURN, and AgentInterrupted.by_user for the turn while the " +
    "agent stays live",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-detached-hold" }, "fake detached-subagent HOLD turn");
    const description = "A sweep the turn keeps waiting beside";
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
    ctx.assistant([{ type: "text", text: "Waiting beside the sweep…" }], { stopReason: null });
    // The park is armed in the same synchronous run as the frame above, as
    // `!hold`'s is, so a consumer that saw the frame can interrupt it.
    await ctx.awaitInterrupt();
    ctx.log.debug({ turn: ctx.turn }, "fake detached-subagent HOLD turn released by an interrupt");
  },
});

const SUBAGENT_DETACHED_UTTERANCE = scenario({
  name: "subagent-detached-utterance",
  prompt: "!subagent-detached-utterance",
  emits:
    "a detached `Agent` left LIVE after the turn ends, whose only post-turn activity is ONE ordinary sidechain " +
    "assistant text line — a mid-flight utterance with `IsSidechain`/`AgentId`/`SourceToolUseId` set and NO " +
    "completion. Nothing here ever finishes the agent",
  writes: "the agent's `.meta.json` and `agent-<id>.jsonl` (the utterance lands there too), its spool, and the main transcript's lines",
  arms:
    "AgentSubagent detached_work left live; the utterance itself proves the router keeps a live subagent's prose " +
    "OUT of the top-level feed rather than adding a new arm",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-detached-utterance" }, "fake mid-flight detached-subagent utterance turn");
    const description = "A sweep that talks while it works";
    const agentPrompt = "Sweep and narrate as you go.";
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
    conclude(ctx, "Dispatched a narrating agent to the background.");
    // The utterance lands AFTER the turn ended, which is what detached means.
    // NO completion follows: the agent stays live, and this is its only
    // post-turn activity.
    await ctx.tick();
    ctx.assistant([{ type: "text", text: "Still sweeping; found something interesting." }], {
      agent: {
        agentId,
        parentToolUseId: call.toolUseId,
        subagentType: "general-purpose",
        taskDescription: description,
      },
      model: "fake-sonnet-5",
    });
    // ONE spool write, capturing everything written to the agent's own
    // transcript so far (commission + utterance) — appending more than once
    // would duplicate bytes, since a spool append is raw and never a diff.
    ctx.files.spool(agentId).append(writer.read());
  },
});

const SUBAGENT_FAILED = scenario({
  name: "subagent-failed",
  prompt: "!subagent-failed",
  emits: "a detached `Agent` that ends in failure: `task_updated{status:\"failed\"}` and a failed `task_notification`",
  writes: "the agent's `.meta.json` and transcript, its spool, and the main transcript's lines",
  arms: "AgentSubagentFailure",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-failed" }, "fake failing detached-subagent turn");
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

const CANCEL_ALL = scenario({
  name: "cancel-all",
  prompt: "!cancel-all",
  emits:
    "THREE detached items launched in one turn — two agents and a shell — left LIVE. The cancel is the caller's " +
    "`stopTask` per item; emptying the live set makes the engine write the vendor's `agents_killed` record",
  writes: "both agents' `.meta.json` and transcripts, the shell's spool, and the main transcript's lines",
  arms: "the fan-wide cancel: AgentSubagentFailure.cause=stopped_by_user per item, plus the agents_killed record",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "cancel-all" }, "fake fan-wide-cancel setup turn");
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

const USAGE_HISTORICAL = scenario({
  name: "usage-historical",
  prompt: "!usage-historical",
  emits:
    "prose only on the main stream. The historical usage record itself is written ONLY to a NESTED subagent's " +
    "own transcript file (spawnDepth 2), as a FILE-plane assistant record with NO paired STREAM-plane " +
    "`message_start` — the historical case that must retain usage without inventing a generation duration. " +
    "UNGROUNDED, INVENTED: no capture carries a file-plane-only historical usage record with nested-subagent " +
    "attribution and this sub-field set",
  writes: "the nested subagent's `agent-<id>.meta.json` and `agent-<id>.jsonl` carrying one untimed assistant record, plus the main turn's ordinary lines",
  arms: "ungrounded — see MANIFEST.md; the usage sub-fields (cache_creation split, server_tool_use, service_tier, speed, inference_geo) are the ones a session-usage aggregation would need to attribute to an untimed nested actor",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "usage-historical" }, "fake INVENTED nested-subagent historical-usage turn");
    const agentId = ctx.mintAgentTaskId();
    const writer = ctx.files.subagent(agentId);
    // `spawnDepth: 2` is what makes this agent NESTED — a subagent of a
    // subagent — rather than an ordinary top-level one.
    writer.writeMeta({
      agentType: "general-purpose",
      description: "a nested subagent's historical response",
      toolUseId: ctx.mintToolUseId(),
      spawnDepth: 2,
    });
    writer.append({
      type: "assistant",
      message: {
        id: ctx.mintMessageId(),
        model: "fake-opus-4-8",
        type: "message",
        role: "assistant",
        content: [{ type: "text", text: "historical response" }],
        stop_reason: "end_turn",
        stop_sequence: null,
        stop_details: null,
        usage: {
          input_tokens: 100,
          output_tokens: 200,
          cache_read_input_tokens: 800,
          cache_creation_input_tokens: 75,
          cache_creation: { ephemeral_5m_input_tokens: 25, ephemeral_1h_input_tokens: 50 },
          server_tool_use: { web_search_requests: 2, web_fetch_requests: 3 },
          service_tier: "priority",
          speed: "fast",
          inference_geo: "us-east-1",
        },
        diagnostics: null,
      },
      requestId: `req_fake_historical_${String(ctx.turn)}`,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    conclude(ctx, `Wrote a historical usage record for nested subagent ${agentId}.`);
  },
});

const SUBAGENT_INTERLEAVED = scenario({
  name: "subagent-interleaved",
  prompt: "!subagent-interleaved",
  emits:
    "a detached `Agent` whose own responses stream INTO the main agent's open blocks: one whole subagent " +
    "response (its `message_start` included) between the two deltas of the main thinking block, another " +
    "between the two deltas of the main text block, then the completed `task_notification`",
  writes:
    "the agent's `.meta.json` and `agent-<id>.jsonl`, its spool as agent JSONL, and the main transcript's lines",
  arms:
    "AgentThinking + AgentResponse.from_model on the main book, each ONE unit, beside the subagent's own " +
    "AgentResponse units on its book",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-interleaved" }, "fake interleaved-subagent turn");
    const description = ctx.args === "" ? "Background sweep" : ctx.args;
    const agentPrompt = "Do the sweep and report.";
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
      resolvedModel: "fake-sonnet-5",
      prompt: agentPrompt,
      outputFile: ctx.files.spoolPathFor(agentId),
      canReadOutputFile: true,
    });
    const agent = {
      agentId,
      parentToolUseId: call.toolUseId,
      subagentType: "general-purpose",
      taskDescription: description,
    };
    const subagentSays = (text: string) => (): void => {
      ctx.assistant([{ type: "text", text }], { agent, model: "fake-sonnet-5", stopReason: "end_turn" });
    };
    // THE MAIN AGENT ANSWERS WHILE ITS SUBAGENT STREAMS: each subagent response
    // lands mid-block, so a fold that kept one block cursor for every agent
    // would re-key the rest of the main block onto the subagent's message.
    const conclusion = "The sweep is running in the background.";
    ctx.assistant(
      [visibleThinking("Answering while the sweep runs."), { type: "text", text: conclusion }],
      {
        stopReason: "end_turn",
        interleave: new Map([
          [0, subagentSays("Sweep started.")],
          [1, subagentSays("Sweep finished.")],
        ]),
      },
    );
    ctx.result({ subtype: "success", result: conclusion });
    ctx.files.spool(agentId).append(writer.read());
    ctx.endTask(agentId);
    ctx.systemMessage("task_notification", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      status: "completed",
      output_file: ctx.files.spoolPathFor(agentId),
      summary: "Sweep finished.",
      usage: { total_tokens: 6_200, tool_uses: 0, duration_ms: 900 },
    });
    ctx.announceLiveTasks();
  },
});

/** The vendor's own notice when the API host could not be reached (2026-09-27 incident). */
const UNREACHABLE_NOTICE = "API Error: Can't reach the API server — check your internet or DNS (ENOTFOUND)";

const SUBAGENT_NETWORK_FAILED = scenario({
  name: "subagent-network-failed",
  prompt: "!subagent-network-failed",
  emits:
    "a detached `Agent` the vendor ENDS because the API was unreachable, in the 2026-09-27 incident's shape: " +
    "after the turn, the agent's own SYNTHETIC error message (`model: \"<synthetic>\"`, `error: \"server_error\"`, " +
    "the vendor's ENOTFOUND notice) and then a failed `task_notification` whose summary carries the same notice " +
    "and `(error type server_error)`",
  writes:
    "the agent's `.meta.json` and transcript (the synthetic error record lands there, as it did in the " +
    "incident's transcript), its spool, and the main transcript's lines",
  arms: "AgentSubagentFailure — and the shim's network resume: the agent waits for the API and is then continued",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-network-failed" }, "fake network-killed detached-subagent turn");
    const description = "A sweep the network will cut off";
    const call = ctx.toolUse("Agent", {
      description,
      prompt: "Sweep.",
      subagent_type: "general-purpose",
      run_in_background: true,
    });
    const agentId = ctx.mintAgentTaskId();
    ctx.startTask({ taskId: agentId, toolUseId: call.toolUseId, kind: "local_agent", description });
    ctx.announceLiveTasks();
    const writer = ctx.files.subagent(agentId);
    writer.writeMeta({ agentType: "general-purpose", description, toolUseId: call.toolUseId, spawnDepth: 1 });
    writer.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: { role: "user", content: "Sweep." },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.toolResult(call, `Async agent launched successfully.\nagentId: ${agentId}`, {
      isAsync: true,
      status: "async_launched",
      agentId,
      description,
      prompt: "Sweep.",
      outputFile: ctx.files.spoolPathFor(agentId),
      canReadOutputFile: true,
    });
    conclude(ctx, "Dispatched an agent the network will cut off.");
    await ctx.tick();
    ctx.assistant([{ type: "text", text: UNREACHABLE_NOTICE }], {
      agent: {
        agentId,
        parentToolUseId: call.toolUseId,
        subagentType: "general-purpose",
        taskDescription: description,
      },
      model: "<synthetic>",
      error: "server_error",
      noReasoning: true,
    });
    ctx.files.spool(agentId).append(writer.read());
    ctx.endTask(agentId);
    ctx.systemMessage("task_updated", { task_id: agentId, patch: { status: "failed", error: UNREACHABLE_NOTICE } });
    ctx.systemMessage("task_notification", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      status: "failed",
      output_file: ctx.files.spoolPathFor(agentId),
      summary:
        `Agent "${description}" failed: Agent terminated early due to an API error: ` +
        `${UNREACHABLE_NOTICE} (error type server_error)`,
      usage: { total_tokens: 400, tool_uses: 0, duration_ms: 120 },
    });
    ctx.announceLiveTasks();
  },
});

const SUBAGENT_RESUMED = scenario({
  name: "subagent-resumed",
  prompt: "!subagent-resumed",
  emits:
    "a `SendMessage` that RESUMES an idle background agent — its task id is the prompt's argument, else a minted " +
    "one: `task_started` naming the agent's task id and the SEND's tool_use_id, the send's `resumedAgentId` " +
    "result, then a completed `task_notification`",
  writes: "the main transcript's lines",
  arms:
    "AgentDetachedWork(kind=subagent) detached from the send, naming the agent its spawn created + " +
    "AgentSendMessage.delivery=resumed_recipient",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "subagent-resumed" }, "fake resumed-subagent turn");
    // THE RESUME'S TASK ID IS THE AGENT'S OWN, stated when the agent was
    // spawned, so a suite names it to resume an agent an earlier turn spawned.
    const agentId = ctx.args === "" ? ctx.mintAgentTaskId() : ctx.args;
    const description = "Resume the sweep";
    const call = ctx.toolUse("SendMessage", { to: agentId, summary: "resume the sweep" });
    // THE VENDOR STARTS THE RESUMED AGENT'S TASK FROM THE SEND, not from its
    // spawn: the task id comes back, the call is the send.
    ctx.startTask({ taskId: agentId, toolUseId: call.toolUseId, kind: "local_agent", description });
    ctx.announceLiveTasks();
    ctx.toolResult(call, "Agent resumed from transcript.", {
      success: true,
      message:
        `Agent "${agentId}" had no active task; resumed from transcript in the background with your message. ` +
        `You'll be notified when it finishes. Output: ${ctx.files.spoolPathFor(agentId)}`,
      resumedAgentId: agentId,
      pin: { id: agentId, name: agentId, ref: "2175c2" },
    });
    conclude(ctx, "Resumed the idle agent with the message.");
    await ctx.tick();
    ctx.endTask(agentId);
    ctx.systemMessage("task_notification", {
      task_id: agentId,
      tool_use_id: call.toolUseId,
      status: "completed",
      output_file: ctx.files.spoolPathFor(agentId),
      summary: "Sweep resumed and finished.",
      usage: { total_tokens: 3_100, tool_uses: 1, duration_ms: 700 },
    });
    ctx.announceLiveTasks();
  },
});

export const NETWORK_RESUME = scenario({
  name: "network-resume",
  prompt: NETWORK_RESUME_MARKER,
  emits:
    "the MAIN agent answering the shim's own network-resume prompt: one `SendMessage` per agent the prompt names, " +
    "each answered WITH `resumedAgentId` (the vendor resuming that SAME agent from its transcript), then that " +
    "agent's `task_started` under the `SendMessage` call, and — after the turn — one model-authored message of " +
    "the resumed agent and its completed `task_notification`",
  writes: "the tool_use and tool_result lines, the closing text line, and each resumed agent's reply in its own transcript",
  arms:
    "AgentSendMessage.delivery=resumed_recipient per agent, then AgentSubagentSuccess for the SAME agent the " +
    "outage failed",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "network-resume" }, "fake network-resume turn");
    const resumed = resumePromptTargets(ctx.prompt).map((agentId) => {
      const call = ctx.toolUse("SendMessage", {
        to: agentId,
        summary: "Resume after network outage",
        message: NETWORK_RESUME_MESSAGE,
      });
      ctx.toolResult(call, "Agent resumed from transcript.", {
        success: true,
        message:
          `Agent "${agentId}" had no active task; resumed from transcript in the background with your message. ` +
          `You'll be notified when it finishes. Output: ${ctx.files.spoolPathFor(agentId)}`,
        resumedAgentId: agentId,
        pin: { id: agentId, name: agentId, ref: "2175c2" },
      });
      ctx.startTask({ taskId: agentId, toolUseId: call.toolUseId, kind: "local_agent", description: "resumed" });
      return { agentId, call };
    });
    ctx.announceLiveTasks();
    conclude(ctx, "Resumed the agents the network outage cut off.");
    await ctx.tick();
    for (const { agentId, call } of resumed) {
      ctx.assistant([{ type: "text", text: "Picking up where I left off." }], {
        agent: {
          agentId,
          parentToolUseId: call.toolUseId,
          subagentType: "general-purpose",
          taskDescription: "resumed",
        },
        model: "fake-sonnet-5",
        noReasoning: true,
      });
      ctx.endTask(agentId);
      ctx.systemMessage("task_notification", {
        task_id: agentId,
        tool_use_id: call.toolUseId,
        status: "completed",
        output_file: ctx.files.spoolPathFor(agentId),
        summary: `Agent "${agentId}" finished`,
        usage: { total_tokens: 400, tool_uses: 0, duration_ms: 120 },
      });
    }
    ctx.announceLiveTasks();
  },
});

export const SUBAGENT_SCENARIOS = [
  SUBAGENT_SYNC,
  SUBAGENT_DETACHED,
  SUBAGENT_INTERLEAVED,
  SUBAGENT_DETACHED_LIVE,
  SUBAGENT_RESUMED,
  SUBAGENT_DETACHED_HOLD,
  SUBAGENT_DETACHED_UTTERANCE,
  SUBAGENT_FAILED,
  SUBAGENT_NETWORK_FAILED,
  NETWORK_RESUME,
  CANCEL_ALL,
  USAGE_HISTORICAL,
];
