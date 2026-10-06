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
 * `AgentBashTail` is structurally detach-only, and the sidecar produces it by
 * TAILING the vendor's `tasks/<id>.output`. The detached scenarios therefore
 * write the spool incrementally, one append at a time, and terminate it with
 * `EXIT=<code>`. That file — not any SDK message — is the whole test.
 *
 * # Backgrounding causes come from the TOOL RESULT
 *
 * `timedOutAfterMs` and `backgroundedByUser` are fields of `BashOutput`, never
 * of the task stream. The timeout and vendor-backgrounded scenarios set exactly
 * those, which is what makes the two causes distinguishable downstream.
 */
import { bashResult, conclude, scenario } from "./support.js";

/**
 * The sentence the vendor puts in a backgrounded Bash's tool_result content.
 *
 * VERBATIM FROM THE CAPTURE (testdata/corpus/tool-results/bash-background.jsonl):
 * `toolUseResult` on a backgrounded shell carries `backgroundTaskId` and
 * nothing else, and this prose is the vendor's ONLY statement of where the
 * output accumulates — the shim reads the path back out of it
 * (src/convert/detached.ts, outputPathFromProse) to announce the detachment
 * with an output a surface can open. An empty content string, which is what the
 * mock used to send, left the announcement with no output and its readability
 * unset.
 */
function backgroundingProse(taskId: string, outputPath: string): string {
  return (
    `Command running in background with ID: ${taskId}. ` +
    `Output is being written to: ${outputPath}. ` +
    "You will be notified when it completes. To check interim output, use Read on that file path."
  );
}

const BASH = scenario({
  name: "bash",
  prompt: "!bash [command]",
  emits:
    "a foreground `Bash` tool_use, its FOREGROUND task (`task_started` with `is_backgrounded: false`, the 0.3.280 " +
    "shape), then its result and the task's completed `task_notification` — no output in between, because " +
    "foreground output is unobservable while running",
  writes: "the tool_use line, the tool_result line with a `BashOutput`-shaped `toolUseResult`, the closing text line",
  arms:
    "AgentBash.start + AgentBashSuccess.outcome=completed how=exited(0); the foreground task is NEVER detached " +
    "work — no detachment, no live-set entry",
  run(ctx) {
    const command = ctx.args === "" ? "pwd; ls | head" : ctx.args;
    ctx.log.debug({ turn: ctx.turn, branch: "bash" }, "fake foreground bash turn");
    const call = ctx.toolUse("Bash", { command });
    // EVERY `Bash` IS A TASK on the 0.3.280 vendor, a blocking one included:
    // it starts in the foreground and concludes with its own result.
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: command, foreground: true });
    ctx.toolResult(call, "one\ntwo\n", bashResult({ stdout: "one\ntwo\n" }));
    ctx.endTask(taskId);
    ctx.systemMessage("task_notification", {
      task_id: taskId,
      tool_use_id: call.toolUseId,
      status: "completed",
      output_file: ctx.files.spoolPathFor(taskId),
      summary: "Command completed",
    });
    conclude(ctx, "Ran the command.");
  },
});

const BASH_HOLD = scenario({
  name: "bash-hold",
  prompt: "!bash-hold",
  emits:
    "a FOREGROUND `Bash` that never returns: the tool_use lands and the turn parks until an interrupt, so the " +
    "unit stays live and foreground for as long as a caller needs it to",
  writes: "the tool_use line, the prompt line and (at the interrupt) the turn record",
  arms:
    "no terminal at all while it holds — the lever for DetachForeground's `not_in_foreground` refusal, which needs a " +
    "GENUINELY LIVE foreground unit to refuse (`!bash` settles before the call can be made, so it answered " +
    "`already_concluded` instead and the refusal under test was never reached). AT THE STOP the unit settles " +
    "AgentBashInterrupted.cause=by_user, minted by the converter's own `cut` rather than by any vendor result: " +
    "the vendor returns none for a call a stop landed inside, and a unit left on its running arm draws a live " +
    "shell inside a turn that ended",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-hold" }, "fake held foreground bash turn");
    // NO startTask AND NO run_in_background: the vendor tracks no task for
    // this call at all, which is the whole point — the unit is detachable IN
    // KIND, but `backgroundTasks` matches it to no foreground task and moves
    // nothing.
    ctx.toolUse("Bash", { command: "tail -f /var/log/system.log" });
    // Parked in the same synchronous run as the call above, so there is no
    // window in which the unit is live and a stop has nothing to resolve.
    await ctx.awaitInterrupt();
    ctx.log.debug({ turn: ctx.turn }, "fake held foreground bash turn released by an interrupt");
    // No result and no explicit terminal: the engine emits the interrupt
    // terminal, which is the ONE place that shape is spelled.
  },
});

const BASH_FAIL = scenario({
  name: "bash-fail",
  prompt: "!bash-fail",
  emits: "a foreground `Bash` whose result is an ERROR carrying stderr and a non-zero interpretation",
  writes: "the tool_use line, the error tool_result line, the closing text line",
  arms: "AgentBashSuccess.outcome=completed how=exited(non-zero) — a non-zero exit is a completed run, not a failure",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-fail" }, "fake failing-bash turn");
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

const BASH_TIMEOUT = scenario({
  name: "bash-timeout",
  prompt: "!bash-timeout",
  emits:
    "a foreground `Bash` that hits its timeout: `task_started`, then a result carrying `timedOutAfterMs` " +
    "and `backgroundTaskId` — the vendor auto-backgrounds rather than killing",
  writes: "the tool_use line, the tool_result line, an incremental spool with NO `EXIT=` line, the closing text line",
  arms: "AgentBashInterrupted.cause=timed_out; the run stays live as detached work",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-timeout" }, "fake timed-out bash turn");
    const call = ctx.toolUse("Bash", { command: "sleep 600", timeout: 120_000 });
    const taskId = ctx.mintShellTaskId();
    // A FOREGROUND start: the call blocks on it until the timeout moves it.
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "sleep 600", foreground: true });
    const spool = ctx.files.spool(taskId);
    spool.appendLine("still going");
    await ctx.tick();
    // THE TIMEOUT MOVES IT: the level now lists it, and the result below is
    // what states why.
    ctx.markBackgrounded(taskId);
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

const BASH_SPILL = scenario({
  name: "bash-spill",
  prompt: "!bash-spill",
  emits: "a foreground `Bash` whose output was too large for the message and spilled to a file on disk",
  writes: "the tool_use line, the tool_result line carrying `persistedOutputPath`/`persistedOutputSize`, the closing text line",
  arms: "AgentBashOutputPartial — the partial extent with the omitted byte count",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-spill" }, "fake spilled-output bash turn");
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

const BASH_IMAGE = scenario({
  name: "bash-image",
  prompt: "!bash-image",
  emits: "a foreground `Bash` whose stdout IS image data (`isImage: true`), answered with an image content block",
  writes: "the tool_use line, the image tool_result line, the closing text line",
  arms: "AgentBashOutput.form=image",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-image" }, "fake image-output bash turn");
    const call = ctx.toolUse("Bash", { command: "screencapture -x -" });
    const base64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==";
    ctx.toolResult(call, "", bashResult({ stdout: base64, isImage: true }), {
      blocks: [{ type: "image", source: { type: "base64", data: base64, media_type: "image/png" } }],
    });
    conclude(ctx, "Captured the screen.");
  },
});

const BASH_DETACH = scenario({
  name: "bash-detach",
  prompt: "!bash-detach",
  emits:
    "a `Bash` with `run_in_background`, `task_started`, `background_tasks_changed`, a result carrying only " +
    "`backgroundTaskId`, then — after the turn — `task_updated` and a completed `task_notification`. When " +
    "`AGENT_REPL_FAKE_DETACH_GATE` names a path, the run PARKS after its first spool line until that path " +
    "exists, so a test can observe the turn concluded and the detached work still going",
  writes:
    "the tool_use and tool_result lines, and `<spool-root>/<slug>/<session>/tasks/b<hex>.output` written " +
    "INCREMENTALLY (the first line before any detach gate, the rest after it) and terminated by `EXIT=0`",
  arms: "AgentBash detached_work + the AgentBashTail snapshot fed by the sidecar tailing the spool",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-detach" }, "fake detached-bash turn");
    const command = ctx.args === "" ? "for i in 1 2 3; do echo line-$i; sleep 1; done" : ctx.args;
    const call = ctx.toolUse("Bash", { command, run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: command });
    ctx.announceLiveTasks();
    const spool = ctx.files.spool(taskId);
    // The result lands while the run is STILL GOING, which is the whole point:
    // a detached run's output outlives the turn that started it.
    ctx.toolResult(
      call,
      backgroundingProse(taskId, ctx.files.spoolPathFor(taskId)),
      bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }),
    );
    conclude(ctx, "Backgrounded the command.");
    // Separate appends, each a growth event a tailer can observe. A single
    // whole-file write would leave the delta path unexercised.
    await ctx.tick();
    spool.appendLine("line-1");
    // THE DETACHED-WORK GATE. A no-op when unset, so an ungated run's timing
    // is unchanged; when set, the remaining lines and the spool's EXIT
    // terminator wait here, proving the detached work outlives the turn by
    // more than a scheduler tick.
    await ctx.awaitDetachGate();
    for (const line of ["line-2", "line-3"]) {
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

/**
 * The `toolUseResult` shape an explicit `TaskOutput` poll answers with.
 *
 * UNGROUNDED, INVENTED (see `testdata/captures/MANIFEST.md`): `TaskOutput` is
 * a declared vendor tool name (every capture's `init.tools` lists it) but NO
 * capture ever calls it — the recorded runs always retrieved a backgrounded
 * task's output by re-reading its spool path instead. This shape mirrors the
 * daemon/e2e Go harness's own invented `bashTaskOutcome` (`RetrievalStatus`,
 * `Task{TaskId, TaskType, Status, Description, Output, ExitCode,
 * ExitCodeSet}`) in the mock's camelCase spelling, since no vendor recording
 * exists to spell it from.
 */
function taskOutputResult(fields: {
  taskId: string;
  command: string;
  status: "RUNNING" | "COMPLETED";
  output: string;
  exitCode?: number;
  exitCodeSet?: boolean;
}): Record<string, unknown> {
  return {
    retrievalStatus: "SUCCESS",
    task: {
      taskId: fields.taskId,
      taskType: "local_bash",
      status: fields.status,
      description: fields.command,
      output: fields.output,
      exitCode: fields.exitCode ?? null,
      exitCodeSet: fields.exitCodeSet ?? false,
    },
  };
}

const BASH_DETACH_POLL = scenario({
  name: "bash-detach-poll",
  prompt: "!bash-detach-poll [command]",
  emits:
    "a `Bash` with `run_in_background`, then — in the SAME turn — explicit `TaskOutput` poll tool_use/tool_result " +
    "pairs: two reporting RUNNING with growing output, then one reporting a terminal exit code and status. " +
    "UNGROUNDED, INVENTED: no capture ever calls `TaskOutput`, only lists it in `init.tools`",
  writes: "the tool_use/tool_result lines for the background and for each poll, and the spool terminated by `EXIT=0`",
  arms: "AgentBash detached_work; the polls themselves reach no converter arm — `TaskOutput` is in `EXEMPT_TOOLS`, so each poll is dropped SILENTLY: no unit, no `AgentUnmodeled`, no unmodeled warning",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-detach-poll" }, "fake explicit-poll detached-bash turn");
    const command = ctx.args === "" ? "tail -f build.log" : ctx.args;
    const call = ctx.toolUse("Bash", { command, run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: command });
    ctx.announceLiveTasks();
    const spool = ctx.files.spool(taskId);
    ctx.toolResult(
      call,
      backgroundingProse(taskId, ctx.files.spoolPathFor(taskId)),
      bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }),
    );

    // Poll #1: RUNNING, partial output.
    spool.appendLine("compiling");
    await ctx.tick();
    const poll1 = ctx.toolUse("TaskOutput", { task_id: taskId, block: false, timeout: 0 });
    ctx.toolResult(
      poll1,
      "compiling\n",
      taskOutputResult({ taskId, command, status: "RUNNING", output: "compiling\n" }),
    );

    // Poll #2: RUNNING, GROWING output — the delta a tailer would observe.
    spool.appendLine("linking");
    await ctx.tick();
    const poll2 = ctx.toolUse("TaskOutput", { task_id: taskId, block: false, timeout: 0 });
    ctx.toolResult(
      poll2,
      "compiling\nlinking\n",
      taskOutputResult({ taskId, command, status: "RUNNING", output: "compiling\nlinking\n" }),
    );

    // Poll #3: the TERMINAL retrieval — a completed status and an exit code.
    spool.appendLine("done");
    spool.finish(0);
    await ctx.tick();
    const poll3 = ctx.toolUse("TaskOutput", { task_id: taskId, block: false, timeout: 0 });
    ctx.toolResult(
      poll3,
      "compiling\nlinking\ndone\n",
      taskOutputResult({
        taskId,
        command,
        status: "COMPLETED",
        output: "compiling\nlinking\ndone\n",
        exitCode: 0,
        exitCodeSet: true,
      }),
    );
    ctx.endTask(taskId);
    ctx.announceLiveTasks();
    conclude(ctx, "Polled the background command to completion.");
  },
});

const BASH_DETACH_FAIL = scenario({
  name: "bash-detach-fail",
  prompt: "!bash-detach-fail",
  emits: "a detached `Bash` that ends non-zero: `task_updated{status:\"failed\"}` and a failed `task_notification`",
  writes: "the tool_use and tool_result lines, and a spool terminated by `EXIT=3`",
  arms: "AgentBash detached_work terminating in a non-zero exit",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-detach-fail" }, "fake failing detached-bash turn");
    const call = ctx.toolUse("Bash", { command: "echo error && exit 3", run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "echo error and exit 3" });
    ctx.announceLiveTasks();
    const spool = ctx.files.spool(taskId);
    ctx.toolResult(
      call,
      backgroundingProse(taskId, ctx.files.spoolPathFor(taskId)),
      bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }),
    );
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

const BASH_DETACH_LIVE = scenario({
  name: "bash-detach-live",
  prompt: "!bash-detach-live",
  emits: "a detached `Bash` that NEVER finishes: no terminal notification, and the task stays in the live set",
  writes: "an unterminated spool with no `EXIT=` line — the corpus's `bash-midoutput.output` shape",
  arms: "AgentBash detached_work still live; what a fan-wide cancel and a StopBash act on",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-detach-live" }, "fake never-ending detached-bash turn");
    const call = ctx.toolUse("Bash", { command: "sleep 100000", run_in_background: true });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "sleep 100000" });
    ctx.announceLiveTasks();
    ctx.toolResult(
      call,
      backgroundingProse(taskId, ctx.files.spoolPathFor(taskId)),
      bashResult({ stdout: "", extra: { backgroundTaskId: taskId } }),
    );
    conclude(ctx, "The command will run until something stops it.");
    await ctx.tick();
    ctx.files.spool(taskId).appendLine("partial output with no terminator");
  },
});

const VENDOR_BACKGROUNDED = scenario({
  name: "vendor-backgrounded",
  prompt: "!vendor-backgrounded",
  emits:
    "a FOREGROUND `Bash` the vendor detaches mid-flight: the scenario parks, `backgroundTasks(toolUseId)` marks it " +
    "`is_backgrounded`, and the foreground result then reports `backgroundedByUser: true`",
  writes: "the tool_use line, the tool_result line carrying `backgroundedByUser`, an unterminated spool",
  arms: "AgentBackgrounded — a vendor-backgrounded foreground unit",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "vendor-backgrounded" }, "fake vendor-backgrounded turn");
    const call = ctx.toolUse("Bash", { command: "tail -f /var/log/system.log" });
    const taskId = ctx.mintShellTaskId();
    // Announced as a task BEFORE the detach so `backgroundTasks(toolUseId)` has
    // something to find: the vendor tracks a foreground shell as a task the
    // moment it starts, which is what makes it addressable at all.
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "local_bash", description: "tail -f", foreground: true });
    ctx.files.spool(taskId).appendLine("first line before the detach");
    // PARK UNTIL THE VENDOR ACTUALLY DETACHES. The scenario cannot decide when
    // that happens — a caller's DetachForeground does — and emitting the
    // backgrounded result on a timer instead would make the test's ordering a
    // race it has to sleep around.
    const detached = await ctx.awaitBackgrounded(call.toolUseId);
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
    // Acknowledged only now: `backgroundTasks` answers the caller AFTER the
    // vendor's own detachment record is on the stream, which is the order a
    // real vendor-side backgrounding has.
    detached();
  },
});

const BASH_REREPORTED = scenario({
  name: "bash-rereported",
  prompt: "!bash-rereported <task_id> <tool_use_id>",
  emits:
    "a `task_notification` with `status: \"stopped\"` for a backgrounded shell task this query never started — " +
    "no `task_started`, no `task_type` — naming the prompt's task id and spawning call, then a conclusion. A " +
    "keep-alive rewind's replacement query re-reports an earlier query's ended shell exactly so",
  writes: "the closing text line",
  arms: "nothing for the re-reported task: its conclusion is already on record",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "bash-rereported" }, "fake re-reported-shell turn");
    const [taskId = ctx.mintShellTaskId(), toolUseId = "toolu_rereported"] = ctx.args.split(/\s+/).filter((word) => word !== "");
    ctx.systemMessage("task_notification", {
      task_id: taskId,
      tool_use_id: toolUseId,
      status: "stopped",
      output_file: ctx.files.spoolPathFor(taskId),
      summary: "Background command stopped",
    });
    conclude(ctx, "Noted the stopped command.");
  },
});

export const SHELL_SCENARIOS = [
  BASH,
  BASH_HOLD,
  BASH_FAIL,
  BASH_TIMEOUT,
  BASH_SPILL,
  BASH_IMAGE,
  BASH_DETACH,
  BASH_DETACH_POLL,
  BASH_DETACH_FAIL,
  BASH_DETACH_LIVE,
  VENDOR_BACKGROUNDED,
  BASH_REREPORTED,
];
