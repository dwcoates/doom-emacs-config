/**
 * convert/tools/registry.ts — WHICH CONVERTER OWNS WHICH VENDOR TOOL NAME.
 *
 * ONE TABLE, and it is the only place a vendor tool name appears beside a unit
 * kind. An `mcp__` name is an MCP server's tool and becomes `AgentMcpToolCall`.
 * Any other name absent from here and from the exempt and engine-owned sets
 * becomes `AgentUnmodeled`, which is the contract's meaning of "a tool whose
 * schema genuinely cannot be known" — so a built-in missing from this table is a
 * producer defect, and the suite asserts it never happens.
 *
 * SEVERAL NAMES CAN SHARE A CONVERTER: the vendor spells the subagent spawn both
 * `Agent` and `Task`, and the plan-mode and worktree pairs are two calls each of
 * one unit kind, distinguished by the `act` arm the converter reads off the name.
 */
import { MCP_KEY, UNMODELED_KEY, type ToolConverter } from "../tool-calls.js";
import { artifactConverter } from "./artifact.js";
import { bashConverter } from "./bash.js";
import { cronConverter } from "./cron.js";
import { editConverter } from "./edit.js";
import { globConverter } from "./glob.js";
import { grepConverter } from "./grep.js";
import { mcpToolConverter } from "./mcp.js";
import { monitorConverter } from "./monitor.js";
import { planModeConverter } from "./plan-mode.js";
import { pushNotificationConverter } from "./push-notification.js";
import { readConverter } from "./read.js";
import { reportFindingsConverter } from "./report-findings.js";
import { scheduleWakeupConverter } from "./schedule-wakeup.js";
import { sendMessageConverter } from "./send-message.js";
import { skillUseConverter } from "./skill-use.js";
import { subagentConverter } from "./subagent.js";
import { taskActConverter } from "./task-act.js";
import { unmodeledConverter } from "./unmodeled.js";
import { webFetchConverter } from "./web-fetch.js";
import { webSearchConverter } from "./web-search.js";
import { worktreeConverter } from "./worktree.js";
import { writeConverter } from "./write.js";

/** Every modelled vendor tool name, and the converter that owns it. */
export const TOOL_CONVERTERS: ReadonlyMap<string, ToolConverter> = new Map<string, ToolConverter>([
  // Files.
  ["Read", readConverter],
  ["Write", writeConverter],
  ["Edit", editConverter],
  // Search.
  ["Grep", grepConverter],
  ["Glob", globConverter],
  // The shell.
  ["Bash", bashConverter],
  // Agents. The vendor spells one spawn two ways.
  ["Agent", subagentConverter],
  ["Task", subagentConverter],
  ["SendMessage", sendMessageConverter],
  // Skills.
  ["Skill", skillUseConverter],
  // The task tracker.
  ["TaskCreate", taskActConverter],
  ["TaskUpdate", taskActConverter],
  // The web.
  ["WebFetch", webFetchConverter],
  ["WebSearch", webSearchConverter],
  // Background watchers and self-pacing.
  ["Monitor", monitorConverter],
  ["ScheduleWakeup", scheduleWakeupConverter],
  // Publishing.
  ["Artifact", artifactConverter],
  // The plan-mode pair.
  ["EnterPlanMode", planModeConverter],
  ["ExitPlanMode", planModeConverter],
  // The review report.
  ["ReportFindings", reportFindingsConverter],
  // The worktree pair.
  ["EnterWorktree", worktreeConverter],
  ["ExitWorktree", worktreeConverter],
  // Scheduled jobs.
  ["CronCreate", cronConverter],
  ["CronDelete", cronConverter],
  ["CronList", cronConverter],
  // Outbound attention.
  ["PushNotification", pushNotificationConverter],
  // Every MCP server's tool (`mcp__<server>__<tool>`), matched by prefix and
  // filed under a key no vendor tool can be named.
  [MCP_KEY, mcpToolConverter],
  // The fallback, filed under a key no vendor tool can be named.
  [UNMODELED_KEY, unmodeledConverter],
]);
