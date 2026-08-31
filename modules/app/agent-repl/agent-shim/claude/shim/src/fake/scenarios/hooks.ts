/**
 * fake/scenarios/hooks.ts — hook activity, on both planes at once.
 *
 * A hook shows up TWICE and the two are not redundant. On the stream it is a
 * `hook_started` / `hook_response` pair carrying the run's id, name, event and
 * outcome. On disk it is an ATTACHMENT record — `hook_success`,
 * `hook_blocking_error`, `hook_non_blocking_error` or `hook_cancelled` — whose
 * shape differs per outcome and which carries the `toolUseID` that joins the
 * hook to the tool call it guarded. The stream says a hook ran; the attachment
 * says what it did to the tool.
 *
 * THE SHIM NEVER SYNTHESIZES A TURN TERMINAL FROM HOOK ACTIVITY (shim.md). Even
 * the blocking hook here ends its turn with an ordinary success `result` — the
 * hook blocked a TOOL, not the turn. The turn-stopping hook terminals live in
 * `failures.ts`, where they come from `result.terminal_reason` as they must.
 */
import { conclude, scenario } from "./support.js";

export const HOOK_SUCCESS = scenario({
  name: "hook-success",
  prompt: "!hook-success",
  emits: "`hook_started` and `hook_response{outcome:\"success\"}` around a `Read`",
  writes: "the tool_use line, a `hook_success` attachment line carrying `toolUseID`, the tool_result line",
  arms: "AgentHook.result=succeeded",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "hook-success" }, "fake succeeding-hook turn");
    const call = ctx.toolUse("Read", { file_path: `${ctx.cwd}/AGENTS.md` });
    const hookId = ctx.newUuid();
    ctx.systemMessage("hook_started", {
      hook_id: hookId,
      hook_name: "PreToolUse:Read",
      hook_event: "PreToolUse",
    });
    ctx.systemMessage("hook_response", {
      hook_id: hookId,
      hook_name: "PreToolUse:Read",
      hook_event: "PreToolUse",
      output: "",
      stdout: "{}\n",
      stderr: "",
      exit_code: 0,
      outcome: "success",
    });
    ctx.attachment({
      type: "hook_success",
      hookName: "PreToolUse:Read",
      toolUseID: call.toolUseId,
      hookEvent: "PreToolUse",
      content: "",
      stdout: "{}\n",
      stderr: "",
      exitCode: 0,
      command: "/w/s/.claude/hooks/pre-read.sh",
      durationMs: 45,
    });
    ctx.toolResult(call, "the file", {
      type: "text",
      file: { filePath: `${ctx.cwd}/AGENTS.md`, content: "the file", numLines: 1, startLine: 1, totalLines: 1 },
    });
    conclude(ctx, "The hook allowed the read.");
  },
});

export const HOOK_BLOCKED = scenario({
  name: "hook-blocked",
  prompt: "!hook-blocked",
  emits: "`hook_started` and `hook_response{outcome:\"error\"}` around an `Edit` the hook BLOCKS",
  writes: "the tool_use line, a `hook_blocking_error` attachment line, the error tool_result line",
  arms: "AgentHook.result=blocking_error; the TURN still succeeds, because a blocked tool is not a stopped turn",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "hook-blocked" }, "fake blocking-hook turn");
    const call = ctx.toolUse("Edit", {
      replace_all: false,
      file_path: "/w/s/example.ts",
      old_string: "a",
      new_string: "b",
    });
    const hookId = ctx.newUuid();
    ctx.systemMessage("hook_started", {
      hook_id: hookId,
      hook_name: "PostToolUse:Edit",
      hook_event: "PostToolUse",
    });
    ctx.systemMessage("hook_response", {
      hook_id: hookId,
      hook_name: "PostToolUse:Edit",
      hook_event: "PostToolUse",
      output: "the suite failed after the edit",
      stdout: "",
      stderr: "the suite failed after the edit",
      exit_code: 2,
      outcome: "error",
    });
    ctx.attachment({
      type: "hook_blocking_error",
      hookName: "PostToolUse:Edit",
      toolUseID: call.toolUseId,
      hookEvent: "PostToolUse",
      blockingError: {
        blockingError: "the suite failed after editing /w/s/example.ts",
        command: '"$CLAUDE_PROJECT_DIR"/.claude/run-tests.sh',
      },
    });
    ctx.toolResult(call, "Blocked by a PostToolUse hook.", { error: "blocked by hook" }, { isError: true });
    conclude(ctx, "A hook blocked the edit.");
  },
});

export const HOOK_FAILED = scenario({
  name: "hook-failed",
  prompt: "!hook-failed",
  emits: "a `SessionStart` hook that FAILS without blocking anything: exit 1 on stderr",
  writes: "a `hook_non_blocking_error` attachment line carrying stderr, exitCode, command and durationMs",
  arms: "AgentHook.result=non_blocking_error",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "hook-failed" }, "fake failing-hook turn");
    const hookId = ctx.newUuid();
    ctx.systemMessage("hook_started", {
      hook_id: hookId,
      hook_name: "SessionStart:startup",
      hook_event: "SessionStart",
    });
    ctx.systemMessage("hook_response", {
      hook_id: hookId,
      hook_name: "SessionStart:startup",
      hook_event: "SessionStart",
      output: "",
      stdout: "",
      stderr: "Failed to run: no interpreter on PATH.",
      exit_code: 1,
      outcome: "error",
    });
    ctx.attachment({
      type: "hook_non_blocking_error",
      hookName: "SessionStart:startup",
      toolUseID: hookId,
      hookEvent: "SessionStart",
      stderr: "Failed to run: no interpreter on PATH.",
      stdout: "",
      exitCode: 1,
      command: "pwsh -File ${CLAUDE_PLUGIN_ROOT}/scripts/init.ps1",
      durationMs: 8,
    });
    conclude(ctx, "A startup hook failed without blocking anything.");
  },
});

export const HOOK_CANCELLED = scenario({
  name: "hook-cancelled",
  prompt: "!hook-cancelled",
  emits: "`hook_started` and `hook_response{outcome:\"cancelled\"}` around an `Edit`",
  writes: "the tool_use line, a `hook_cancelled` attachment line — four fields and nothing else — the tool_result line",
  arms: "AgentHook.result=cancelled",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "hook-cancelled" }, "fake cancelled-hook turn");
    const call = ctx.toolUse("Edit", {
      replace_all: false,
      file_path: "/w/s/example.ts",
      old_string: "a",
      new_string: "b",
    });
    const hookId = ctx.newUuid();
    ctx.systemMessage("hook_started", {
      hook_id: hookId,
      hook_name: "PostToolUse:Edit",
      hook_event: "PostToolUse",
    });
    ctx.systemMessage("hook_response", {
      hook_id: hookId,
      hook_name: "PostToolUse:Edit",
      hook_event: "PostToolUse",
      output: "",
      stdout: "",
      stderr: "",
      outcome: "cancelled",
    });
    // The corpus's `hook_cancelled` attachment carries exactly four fields. No
    // stderr, no exit code, no duration — a cancelled hook produced none.
    ctx.attachment({
      type: "hook_cancelled",
      hookName: "PostToolUse:Edit",
      toolUseID: call.toolUseId,
      hookEvent: "PostToolUse",
    });
    ctx.toolResult(call, "The file has been updated.", {
      filePath: "/w/s/example.ts",
      oldString: "a",
      newString: "b",
      originalFile: null,
      structuredPatch: [],
      userModified: false,
      replaceAll: false,
    });
    conclude(ctx, "The hook was cancelled and the edit stood.");
  },
});

export const HOOK_SCENARIOS = [HOOK_SUCCESS, HOOK_BLOCKED, HOOK_FAILED, HOOK_CANCELLED];
