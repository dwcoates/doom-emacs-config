/**
 * fake/scenarios/shell.ts — Bash, foreground and detached.
 *
 * # Foreground shell output is observable NOWHERE while running
 *
 * Verified empirically (shim.md, "Gotchas"): the persisted-output file
 * materializes at exit, at final size. So a FOREGROUND shell here emits nothing
 * between its call and its result — a mock that dribbled progress out would
 * teach the stack a delta path production does not have.
 *
 * # Every byte of DETACHED shell output comes from the spool
 *
 * `AgentBashUpdate` is structurally detach-only, and the sidecar produces it by
 * TAILING the vendor's `tasks/<id>.output`. The detached scenarios therefore
 * write the spool incrementally, one append at a time, and terminate it with
 * `EXIT=<code>`. That file — not any SDK message — is the whole test.
 *
 * # Backgrounding causes come from the TOOL RESULT
 *
 * `timedOutAfterMs` and `backgroundedByUser` are fields of `BashOutput`, never
 * of the task stream. The timeout and Ctrl-B scenarios set exactly those, which
 * is what makes the two causes distinguishable downstream.
 */
import { bashResult, conclude, scenario } from "./support.js";

export const BASH = scenario({
  name: "bash",
  prompt: "!bash [command]",
  emits: "a foreground `Bash` tool_use, then its result — nothing in between, because foreground output is unobservable while running",
  writes: "the tool_use line, the tool_result line with a `BashOutput`-shaped `toolUseResult`, the closing text line",
  arms: "AgentBash.start + AgentBashSuccess.outcome=completed how=exited(0)",
  run(ctx) {
    const command = ctx.args === "" ? "pwd; ls | head" : ctx.args;
    ctx.log({ turn: ctx.turn, branch: "bash" }, "fake foreground bash turn");
    const call = ctx.toolUse("Bash", { command });
    ctx.toolResult(call, "one\ntwo\n", bashResult({ stdout: "one\ntwo\n" }));
    conclude(ctx, "Ran the command.");
  },
});

export const BASH_FAIL = scenario({
  name: "bash-fail",
  prompt: "!bash-fail",
  emits: "a foreground `Bash` whose result is an ERROR carrying stderr and a non-zero interpretation",
  writes: "the tool_use line, the error tool_result line, the closing text line",
  arms: "AgentBashSuccess.outcome=completed how=exited(non-zero) — a non-zero exit is a completed run, not a failure",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-fail" }, "fake failing-bash turn");
    const call = ctx.toolUse("Bash", { command: "exit 3" });
    ctx.toolResult(
      call,
      "boom\n",
      bashResult({
        stdout: "",
        stderr: "boom\n",
        extra: { returnCodeInterpretation: "exited with code 3" },
      }),
      { isError: true },
    );
    conclude(ctx, "The command exited non-zero.");
  },
});

export const BASH_TIMEOUT = scenario({
  name: "bash-timeout",
  prompt: "!bash-timeout",
  emits:
    "a foreground `Bash` that hits its timeout: `task_started`, then a result carrying `timedOutAfterMs` " +
    "and `backgroundTaskId` — the vendor auto-backgrounds rather than killing",
  writes: "the tool_use line, the tool_result line, an incremental spool with NO `EXIT=` line, the closing text line",
  arms: "AgentBashInterrupted.cause=timed_out; the run stays live as detached work",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-timeout" }, "fake timed-out bash turn");
    const call = ctx.toolUse("Bash", { command: "sleep 600", timeout: 120_000 });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "sleep 600" });
    ctx.announceLiveTasks();
    const spool = ctx.files.spool(taskId);
    spool.appendLine("still going");
    await ctx.tick();
    ctx.toolResult(
      call,
      "",
      bashResult({
        stdout: "",
        // The CAUSE of the backgrounding, harvested from the tool result and
        // from nowhere else. Without it the run is indistinguishable from a
        // user-requested detach.
        extra: { backgroundTaskId: taskId, timedOutAfterMs: 120_000 },
      }),
    );
    conclude(ctx, "The command timed out and moved to the background.");
  },
});

export const BASH_SPILL = scenario({
  name: "bash-spill",
  prompt: "!bash-spill",
  emits: "a foreground `Bash` whose output was too large for the message and spilled to a file on disk",
  writes: "the tool_use line, the tool_result line carrying `persistedOutputPath`/`persistedOutputSize`, the closing text line",
  arms: "AgentBashOutputPartial — the partial extent with the omitted byte count",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-spill" }, "fake spilled-output bash turn");
    const call = ctx.toolUse("Bash", { command: "yes | head -100000" });
    ctx.toolResult(
      call,
      "y\ny\ny\n… [output truncated]",
      bashResult({
        stdout: "y\ny\ny\n… [output truncated]",
        extra: {
          persistedOutputPath: `${ctx.spoolRoot}/tool-results/spill.txt`,
          persistedOutputSize: 200_000,
        },
      }),
    );
    conclude(ctx, "The output was too large and spilled to a file.");
  },
});

export const BASH_IMAGE = scenario({
  name: "bash-image",
  prompt: "!bash-image",
  emits: "a foreground `Bash` whose stdout IS image data (`isImage: true`), answered with an image content block",
  writes: "the tool_use line, the image tool_result line, the closing text line",
  arms: "AgentBashOutput.form=image",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-image" }, "fake image-output bash turn");
    const call = ctx.toolUse("Bash", { command: "screencapture -x -" });
    const base64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==";
    ctx.toolResult(call, "", bashResult({ stdout: base64, isImage: true }), {
      blocks: [{ type: "image", source: { type: "base64", data: base64, media_type: "image/png" } }],
    });
    conclude(ctx, "Captured the screen.");
  },
});

export const BASH_DETACH = scenario({
  name: "bash-detach",
  prompt: "!bash-detach",
  emits:
    "a `Bash` with `run_in_background`, `task_started`, `background_tasks_changed`, a result carrying only " +
    "`backgroundTaskId`, then — after the turn — `task_updated` and a completed `task_notification`",
  writes:
    "the tool_use and tool_result lines, and `<spool-root>/<slug>/<session>/tasks/b<hex>.output` written " +
    "INCREMENTALLY and terminated by `EXIT=0`",
  arms: "AgentBash detached_work + AgentBashUpdate deltas fed by the sidecar tailing the spool",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-detach" }, "fake detached-bash turn");
    const command = ctx.args === "" ? "for i in 1 2 3; do echo line-$i; sleep 1; done" : ctx.args;
    const call = ctx.toolUse("Bash", { command, run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: command });
    ctx.announceLiveTasks();
    const spool = ctx.files.spool(taskId);
    // The result lands while the run is STILL GOING, which is the whole point:
    // a detached run's output outlives the turn that started it.
    ctx.toolResult(call, "", bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }));
    conclude(ctx, "Backgrounded the command.");
    // Separate appends, each a growth event a tailer can observe. A single
    // whole-file write would leave the delta path unexercised.
    for (const line of ["line-1", "line-2", "line-3"]) {
      await ctx.tick();
      spool.appendLine(line);
    }
    await ctx.tick();
    spool.finish(0);
    ctx.endTask(taskId);
    ctx.systemMessage("task_updated", { task_id: taskId, patch: { status: "completed", end_time: ctx.nowMs() } });
    ctx.systemMessage("task_notification", {
      task_id: taskId,
      tool_use_id: call.toolUseId,
      status: "completed",
      output_file: ctx.files.spoolPathFor(taskId),
      summary: "Background command completed",
    });
    ctx.announceLiveTasks();
  },
});

export const BASH_DETACH_FAIL = scenario({
  name: "bash-detach-fail",
  prompt: "!bash-detach-fail",
  emits: "a detached `Bash` that ends non-zero: `task_updated{status:\"failed\"}` and a failed `task_notification`",
  writes: "the tool_use and tool_result lines, and a spool terminated by `EXIT=3`",
  arms: "AgentBash detached_work terminating in a non-zero exit",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-detach-fail" }, "fake failing detached-bash turn");
    const call = ctx.toolUse("Bash", { command: "echo error && exit 3", run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "echo error and exit 3" });
    ctx.announceLiveTasks();
    const spool = ctx.files.spool(taskId);
    ctx.toolResult(call, "", bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }));
    conclude(ctx, "Backgrounded a command that will fail.");
    await ctx.tick();
    spool.appendLine("error");
    spool.finish(3);
    ctx.endTask(taskId);
    ctx.systemMessage("task_updated", { task_id: taskId, patch: { status: "failed", end_time: ctx.nowMs() } });
    ctx.systemMessage("task_notification", {
      task_id: taskId,
      tool_use_id: call.toolUseId,
      status: "failed",
      output_file: ctx.files.spoolPathFor(taskId),
      summary: "Background command exited 3",
    });
    ctx.announceLiveTasks();
  },
});

export const BASH_DETACH_LIVE = scenario({
  name: "bash-detach-live",
  prompt: "!bash-detach-live",
  emits: "a detached `Bash` that NEVER finishes: no terminal notification, and the task stays in the live set",
  writes: "an unterminated spool with no `EXIT=` line — the corpus's `bash-midoutput.output` shape",
  arms: "AgentBash detached_work still live; what a fan-wide cancel and a StopBash act on",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "bash-detach-live" }, "fake never-ending detached-bash turn");
    const call = ctx.toolUse("Bash", { command: "sleep 100000", run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "sleep 100000" });
    ctx.announceLiveTasks();
    ctx.toolResult(call, "", bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }));
    conclude(ctx, "The command will run until something stops it.");
    await ctx.tick();
    ctx.files.spool(taskId).appendLine("partial output with no terminator");
  },
});

export const CTRL_B = scenario({
  name: "ctrl-b",
  prompt: "!ctrl-b",
  emits:
    "a FOREGROUND `Bash` the user detaches mid-flight: the scenario parks, `backgroundTasks(toolUseId)` marks it " +
    "`is_backgrounded`, and the foreground result then reports `backgroundedByUser: true`",
  writes: "the tool_use line, the tool_result line carrying `backgroundedByUser`, an unterminated spool",
  arms: "AgentBackgrounded — the user-requested detach of foreground work",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "ctrl-b" }, "fake Ctrl-B detach turn");
    const call = ctx.toolUse("Bash", { command: "tail -f /var/log/system.log" });
    const taskId = ctx.mintShellTaskId();
    // Announced as a task BEFORE the detach so `backgroundTasks(toolUseId)` has
    // something to find: the vendor tracks a foreground shell as a task the
    // moment it starts, which is what makes Ctrl-B addressable at all.
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "tail -f" });
    ctx.announceLiveTasks();
    ctx.files.spool(taskId).appendLine("first line before the detach");
    await ctx.tick();
    ctx.toolResult(
      call,
      "",
      bashResult({
        stdout: "first line before the detach\n",
        // THE cause of this backgrounding. `timedOutAfterMs` would say the
        // opposite thing, and the task stream says neither.
        extra: { backgroundTaskId: taskId, backgroundedByUser: true },
      }),
    );
    conclude(ctx, "Moved the command to the background.");
  },
});

export const SHELL_SCENARIOS = [
  BASH,
  BASH_FAIL,
  BASH_TIMEOUT,
  BASH_SPILL,
  BASH_IMAGE,
  BASH_DETACH,
  BASH_DETACH_FAIL,
  BASH_DETACH_LIVE,
  CTRL_B,
];
