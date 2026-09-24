/**
 * fake/scenarios/automation.ts — plan mode, findings, worktrees, cron,
 * notifications, monitors, wakeups and artifacts.
 *
 * These nine tool kinds share nothing but their shape of evidence: each has a
 * declared Output in `sdk-tools.d.ts` and (except `ExitPlanMode`, `Monitor` and
 * `ScheduleWakeup`) NO corpus fixture. So the results here follow the
 * declarations, and the report says so. Where the corpus does have a fixture —
 * `tool-results/monitor.jsonl`, `schedule_wakeup.jsonl`, and the
 * `plan_mode_exit` attachment — the observed shape wins.
 *
 * Every family is represented in every arm the proto declares, because each
 * arm is a separate rendering: a push notification that was NOT sent has three
 * distinct reasons, and a mock that only ever sent one would leave two of them
 * unreachable.
 */
import { conclude, scenario } from "./support.js";

const PLAN_MODE = scenario({
  name: "plan",
  prompt: "!plan",
  emits:
    "an `EnterPlanMode` call, prose written under plan mode, then an `ExitPlanMode` answered with the plan and " +
    "the path it was saved to, plus the vendor's `plan_mode_exit` attachment",
  writes: "the tool_use and tool_result lines for both calls, a `plan_mode_exit` attachment line, the closing text line",
  arms: "AgentPlanMode.act=enter/exit with AgentPlanModeEntered and AgentPlanModeExited",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "plan" }, "fake plan-mode turn");
    const enter = ctx.toolUse("EnterPlanMode", {});
    ctx.toolResult(enter, "Entered plan mode.", { message: "Entered plan mode." });
    ctx.assistant([{ type: "text", text: "Drafting the plan without touching anything." }]);
    const plan = "1. Read the module.\n2. Change the converter.\n3. Run the suite.";
    const planFile = `${ctx.configDir}/plans/offline-plan.md`;
    const exit = ctx.toolUse("ExitPlanMode", { plan });
    ctx.toolResult(exit, plan, { plan, isAgent: false, filePath: planFile, hasTaskTool: true, planWasEdited: false });
    ctx.attachment({ type: "plan_mode_exit", planFilePath: planFile, planExists: true });
    conclude(ctx, "The plan is ready.");
  },
});

const REPORT_FINDINGS = scenario({
  name: "findings",
  prompt: "!findings",
  emits:
    "a `ReportFindings` carrying THREE findings — one confirmed, one plausible, and one re-reported with an " +
    "`outcome` — so every verdict and every outcome arm is reachable from one call",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentReportFindings.start + Success with verdict=confirmed/plausible and outcome=fixed/skipped/no_change_needed",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "findings" }, "fake report-findings turn");
    const findings = [
      {
        file: "src/convert/fold.ts",
        line: 42,
        summary: "The fold drops a tool_result whose call it never saw.",
        short_summary: "orphan tool_result dropped",
        failure_scenario: "A resumed session's first tool_result arrives with no preceding tool_use and is lost.",
        category: "correctness",
        verdict: "CONFIRMED" as const,
        outcome: "fixed" as const,
      },
      {
        file: "src/store/keys.ts",
        line: 12,
        summary: "The write id may collide across producers.",
        failure_scenario: "Two shims for one workspace mint the same sha256 for different frames.",
        category: "correctness",
        verdict: "PLAUSIBLE" as const,
        outcome: "skipped" as const,
      },
      {
        file: "src/log.ts",
        summary: "The stderr mirror retires silently.",
        failure_scenario: "A daemon bounce retires the mirror and nothing records it.",
        category: "observability",
        outcome: "no_change_needed" as const,
      },
    ];
    const call = ctx.toolUse("ReportFindings", { findings, level: "high" });
    ctx.toolResult(call, "Reported 3 findings.", { count: 3, level: "high", findings });
    conclude(ctx, "Reported three findings.");
  },
});

const WORKTREE_KEEP = scenario({
  name: "worktree-keep",
  prompt: "!worktree-keep",
  emits: "an `EnterWorktree` then an `ExitWorktree` with `action: \"keep\"` — the worktree and branch stay on disk",
  writes: "the tool_use and tool_result lines for both calls, the closing text line",
  arms: "AgentWorktree.act=enter/exit with outcome=kept",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "worktree-keep" }, "fake worktree (keep) turn");
    const enter = ctx.toolUse("EnterWorktree", { branch: "offline/experiment" });
    ctx.toolResult(enter, "Entered the worktree.", {
      worktreePath: "/w/worktrees/experiment",
      worktreeBranch: "offline/experiment",
      message: "Entered the worktree.",
    });
    const exit = ctx.toolUse("ExitWorktree", { action: "keep" });
    ctx.toolResult(exit, "Left the worktree in place.", {
      action: "keep",
      originalCwd: ctx.cwd,
      worktreePath: "/w/worktrees/experiment",
      worktreeBranch: "offline/experiment",
      message: "Left the worktree in place.",
    });
    conclude(ctx, "Kept the worktree.");
  },
});

const WORKTREE_REMOVE = scenario({
  name: "worktree-remove",
  prompt: "!worktree-remove",
  emits: "an `ExitWorktree` with `action: \"remove\"` reporting the discarded file and commit counts",
  writes: "the tool_use and tool_result lines, the closing text line",
  arms: "AgentWorktree.act=exit with outcome=removed",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "worktree-remove" }, "fake worktree (remove) turn");
    const enter = ctx.toolUse("EnterWorktree", { branch: "offline/throwaway" });
    ctx.toolResult(enter, "Entered the worktree.", {
      worktreePath: "/w/worktrees/throwaway",
      worktreeBranch: "offline/throwaway",
      message: "Entered the worktree.",
    });
    const exit = ctx.toolUse("ExitWorktree", { action: "remove", discard_changes: true });
    ctx.toolResult(exit, "Removed the worktree.", {
      action: "remove",
      originalCwd: ctx.cwd,
      worktreePath: "/w/worktrees/throwaway",
      worktreeBranch: "offline/throwaway",
      discardedFiles: 3,
      discardedCommits: 1,
      message: "Removed the worktree.",
    });
    conclude(ctx, "Removed the worktree.");
  },
});

const CRON = scenario({
  name: "cron",
  prompt: "!cron",
  emits: "a `CronCreate`, a `CronList` and a `CronDelete` — all three acts in one turn",
  writes: "the tool_use and tool_result lines for all three calls, the closing text line",
  arms: "AgentCron.act=create/list/delete with created/listed/deleted",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "cron" }, "fake cron turn");
    const create = ctx.toolUse("CronCreate", { cron: "0 9 * * 1", prompt: "weekly standup" });
    ctx.toolResult(create, "Created job cron-1.", {
      id: "cron-1",
      humanSchedule: "every Monday at 9:00am",
      recurring: true,
      durable: true,
    });
    const list = ctx.toolUse("CronList", {});
    ctx.toolResult(list, "One job.", {
      jobs: [
        {
          id: "cron-1",
          cron: "0 9 * * 1",
          humanSchedule: "every Monday at 9:00am",
          prompt: "weekly standup",
          recurring: true,
          durable: true,
        },
      ],
    });
    const remove = ctx.toolUse("CronDelete", { id: "cron-1" });
    ctx.toolResult(remove, "Deleted job cron-1.", { id: "cron-1" });
    conclude(ctx, "Created, listed and deleted a cron job.");
  },
});

/** One push-notification scenario per declared outcome. */
function pushScenario(
  name: string,
  outcome: { pushSent: boolean; disabledReason?: "config_off" | "user_present" | "no_transport" },
  arm: string,
) {
  return scenario({
    name,
    prompt: `!${name}`,
    emits: `a \`PushNotification\` answered with ${outcome.pushSent ? "`pushSent: true`" : `\`disabledReason: "${outcome.disabledReason}"\``}`,
    writes: "the tool_use line, the tool_result line, the closing text line",
    arms: arm,
    run(ctx) {
      ctx.log.debug({ turn: ctx.turn, branch: name }, "fake push-notification turn");
      const call = ctx.toolUse("PushNotification", { message: "The offline run finished." });
      ctx.toolResult(call, outcome.pushSent ? "Sent." : "Not sent.", {
        message: "The offline run finished.",
        pushSent: outcome.pushSent,
        localSent: outcome.pushSent,
        ...(outcome.disabledReason === undefined ? {} : { disabledReason: outcome.disabledReason }),
        sentAt: ctx.nowIso(),
      });
      conclude(ctx, outcome.pushSent ? "Sent the notification." : "The notification was not sent.");
    },
  });
}

const PUSH_SENT = pushScenario("push-sent", { pushSent: true }, "AgentPushNotification.outcome=sent");
const PUSH_CONFIG_OFF = pushScenario(
  "push-config-off",
  { pushSent: false, disabledReason: "config_off" },
  "AgentPushNotification.outcome=not_sent reason=config_off",
);
const PUSH_USER_PRESENT = pushScenario(
  "push-user-present",
  { pushSent: false, disabledReason: "user_present" },
  "AgentPushNotification.outcome=not_sent reason=user_present",
);
const PUSH_NO_TRANSPORT = pushScenario(
  "push-no-transport",
  { pushSent: false, disabledReason: "no_transport" },
  "AgentPushNotification.outcome=not_sent reason=no_transport",
);

const MONITOR_DEADLINE = scenario({
  name: "monitor-deadline",
  prompt: "!monitor-deadline",
  emits: "a `Monitor` with a finite `timeoutMs` and `persistent: false` (corpus: tool-results/monitor.jsonl)",
  writes: "the tool_use line, the tool_result line, the closing text line; the monitor stays in the live set",
  arms: "AgentMonitor.lifetime=deadline",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "monitor-deadline" }, "fake deadline-monitor turn");
    // MonitorInput (sdk-tools.d.ts) declares `description`, `timeout_ms` and
    // `persistent` REQUIRED on the tool's own input — the shim reads the watch's
    // lifetime and its footer text off the call, never off the task record.
    const call = ctx.toolUse("Monitor", {
      description: "build log",
      timeout_ms: 600_000,
      persistent: false,
      command: "tail -f /var/log/build.log",
    });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "monitor", description: "build log" });
    ctx.announceLiveTasks();
    ctx.toolResult(call, "Monitoring.", { taskId, timeoutMs: 600_000, persistent: false });
    conclude(ctx, "Monitoring until the deadline.");
  },
});

const MONITOR_PERSISTENT = scenario({
  name: "monitor-persistent",
  prompt: "!monitor-persistent",
  emits: "a `Monitor` with `timeoutMs: 0` and `persistent: true` — it runs until TaskStop or session end",
  writes: "the tool_use line, the tool_result line, the closing text line; the monitor stays in the live set",
  arms: "AgentMonitor.lifetime=persistent",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "monitor-persistent" }, "fake persistent-monitor turn");
    // `timeout_ms` is required even when `persistent` ignores it, and the
    // source is a `ws` object — `server`/`tool` are on no declared input.
    const call = ctx.toolUse("Monitor", {
      description: "echo watch",
      timeout_ms: 0,
      persistent: true,
      ws: { url: "ws://127.0.0.1:8787/echo" },
    });
    const taskId = ctx.mintShellTaskId();
    ctx.startTask({ taskId, toolUseId: call.toolUseId, kind: "monitor", description: "echo watch" });
    ctx.announceLiveTasks();
    ctx.toolResult(call, "Monitoring.", { taskId, timeoutMs: 0, persistent: true });
    conclude(ctx, "Monitoring until something stops it.");
  },
});

const WAKEUP_SCHEDULE = scenario({
  name: "wakeup-schedule",
  prompt: "!wakeup-schedule",
  emits: "a `ScheduleWakeup` answered with the corpus shape — scheduledFor, clampedDelaySeconds, wasClamped",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentScheduleWakeup.act=schedule outcome=scheduled",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "wakeup-schedule" }, "fake schedule-wakeup turn");
    const call = ctx.toolUse("ScheduleWakeup", { delaySeconds: 1_200 });
    ctx.toolResult(call, "Scheduled.", {
      scheduledFor: ctx.nowMs() + 1_200_000,
      clampedDelaySeconds: 1_200,
      wasClamped: false,
    });
    conclude(ctx, "Scheduled the wakeup.");
  },
});

const WAKEUP_STOP = scenario({
  name: "wakeup-stop",
  prompt: "!wakeup-stop",
  emits: "a `ScheduleWakeup` with `stop: true`, answered with `stopped: true` and the cancelled count",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentScheduleWakeup.act=stop outcome=stopped",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "wakeup-stop" }, "fake stop-wakeup turn");
    const call = ctx.toolUse("ScheduleWakeup", { stop: true });
    ctx.toolResult(call, "Stopped.", {
      scheduledFor: 0,
      clampedDelaySeconds: 0,
      wasClamped: false,
      stopped: true,
      cancelledWakeups: 1,
    });
    conclude(ctx, "Stopped the wakeup loop.");
  },
});

const ARTIFACT_PUBLISH = scenario({
  name: "artifact-publish",
  prompt: "!artifact-publish",
  emits: "an `Artifact` publish answered with the url, the source path, a title and a contract version",
  writes: "the tool_use line, the tool_result line, and a `frame-link` metadata line, the closing text line",
  arms: "AgentArtifact.act=publish outcome=published",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "artifact-publish" }, "fake artifact-publish turn");
    const call = ctx.toolUse("Artifact", { file_path: "/tmp/offline-report.html", favicon: "📊" });
    const url = "https://claude.ai/code/artifact/00000000-0000-4000-8000-000000000000";
    ctx.toolResult(call, url, {
      url,
      path: "/tmp/offline-report.html",
      title: "Offline Report",
      version: "1",
      contract: "1.0.0",
      updated: false,
    });
    // The vendor's own sidebar record of a published artifact; unchained, like
    // every other metadata line.
    ctx.files.transcript.appendUnchained({
      type: "frame-link",
      path: "/tmp/offline-report.html",
      frameUrl: url,
      timestamp: ctx.nowIso(),
    });
    conclude(ctx, "Published the artifact.");
  },
});

const ARTIFACT_LIST = scenario({
  name: "artifact-list",
  prompt: "!artifact-list",
  emits: "an `Artifact` list answered with two rows, one owned and one shared, and `truncated: false`",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentArtifact.act=list outcome=listed",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "artifact-list" }, "fake artifact-list turn");
    const call = ctx.toolUse("Artifact", { action: "list", scope: "all" });
    ctx.toolResult(call, "Two artifacts.", {
      artifacts: [
        {
          title: "Offline Report",
          url: "https://claude.ai/code/artifact/00000000-0000-4000-8000-000000000000",
          updatedAt: ctx.nowIso(),
          rel: "mine",
        },
        {
          title: "Someone Else's Page",
          url: "https://claude.ai/code/artifact/00000000-0000-4000-8000-000000000001",
          rel: "shared",
        },
      ],
      truncated: false,
      scope: "all",
    });
    conclude(ctx, "Listed the artifacts.");
  },
});

const MCP_TOOL = scenario({
  name: "mcp-tool",
  prompt: "!mcp-tool",
  emits: "an `mcp__echo__echo` call — an MCP server's tool — answered with its text",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentMcpToolCall, an ordinary tool call, its address resolved by lookup against the session's `echo` server",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "mcp-tool" }, "fake MCP tool turn");
    const call = ctx.toolUse("mcp__echo__echo", { text: "hello from the offline session" });
    ctx.toolResult(call, "hello from the offline session", {
      content: [{ type: "text", text: "hello from the offline session" }],
      isError: false,
    });
    conclude(ctx, "The MCP tool echoed.");
  },
});

const UNMODELED = scenario({
  name: "unmodeled",
  prompt: "!unmodeled",
  emits: "a `StructuredOutput` call — an SDK tool NO converter owns and no MCP server serves — answered with an opaque payload",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentUnmodeled",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "unmodeled" }, "fake unmodeled-tool turn");
    const call = ctx.toolUse("StructuredOutput", { answer: "offline" });
    ctx.toolResult(call, "Structured output provided successfully", {
      content: [{ type: "text", text: "Structured output provided successfully" }],
      isError: false,
    });
    conclude(ctx, "The unmodeled tool answered.");
  },
});

export const AUTOMATION_SCENARIOS = [
  PLAN_MODE,
  REPORT_FINDINGS,
  WORKTREE_KEEP,
  WORKTREE_REMOVE,
  CRON,
  PUSH_SENT,
  PUSH_CONFIG_OFF,
  PUSH_USER_PRESENT,
  PUSH_NO_TRANSPORT,
  MONITOR_DEADLINE,
  MONITOR_PERSISTENT,
  WAKEUP_SCHEDULE,
  WAKEUP_STOP,
  ARTIFACT_PUBLISH,
  ARTIFACT_LIST,
  MCP_TOOL,
  UNMODELED,
];
