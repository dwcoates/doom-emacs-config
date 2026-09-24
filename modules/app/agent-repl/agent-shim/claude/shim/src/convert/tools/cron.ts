/**
 * convert/tools/cron.ts — the scheduled-jobs family, three calls of one kind.
 *
 * # ONE CONVERTER, THREE TOOL NAMES
 *
 * `CronCreate`, `CronDelete` and `CronList` are three vendor calls of one unit
 * kind; the act is read off the NAME. Job ids are the VENDOR'S cron ids,
 * carried OPAQUE — compared for equality, never parsed. They name JOBS rather
 * than transcript records, which is what permits them here at all.
 *
 * # A listing is REPLACE semantics
 *
 * `AgentCronListed` states the job set WHOLE, exactly as the footer panel
 * consumes it. That is why a listing whose typed output states no `jobs` array
 * produces NO frame: an empty listed arm would tell the panel to forget every
 * job it knows about, on the strength of a field the vendor never sent.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, bool, failureOf, settle, str, strOr } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-cron", operation: "shim.convert.tools.cron" });

/** The vendor's three spellings of this unit's three acts. */
const CREATE = "CronCreate";
const DELETE = "CronDelete";
const LIST = "CronList";

/** Which act a call is, by the name the agent used. */
function actNameOf(call: PendingCall): "create" | "delete" | "list" | undefined {
  if (call.toolName === CREATE) return "create";
  if (call.toolName === DELETE) return "delete";
  if (call.toolName === LIST) return "list";
  LOGGER.debug(
    { tool: call.toolName, tool_use_id: call.toolUseId },
    "a cron unit was built from a tool name that is none of the three cron calls",
  );
  return undefined;
}

/** What was asked, as the call spelled it. */
function actOf(call: PendingCall): conversationv1.AgentCronStart["act"] {
  switch (actNameOf(call)) {
    case "create":
      return {
        case: "create",
        value: create(conversationv1.AgentCronCreateSchema, {
          cron: strOr(call.input, "cron"),
          prompt: strOr(call.input, "prompt"),
          recurring: bool(call.input, "recurring") ?? false,
          durable: bool(call.input, "durable") ?? false,
        }),
      };
    case "delete": {
      const jobId = str(call.input, "id");
      if (jobId === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a cron delete names no job id; the unit cannot restate which job is being removed",
        );
      }
      return {
        case: "delete",
        value: create(conversationv1.AgentCronDeleteSchema, { jobId: jobId ?? "" }),
      };
    }
    case "list":
      return { case: "list", value: create(conversationv1.AgentCronListSchema, {}) };
    default:
      return { case: undefined };
  }
}

/** One job as the listing states it. */
function jobOf(entry: unknown, toolUseId: string): conversationv1.AgentCronJob | undefined {
  const record = asRecord(entry);
  const jobId = str(record, "id");
  if (jobId === undefined) {
    // An id-less row cannot be deleted, matched, or updated; it is not a job.
    LOGGER.debug(
      { tool_use_id: toolUseId },
      "a cron listing row named no job id; the row is dropped",
    );
    return undefined;
  }
  return create(conversationv1.AgentCronJobSchema, {
    jobId,
    cron: strOr(record, "cron"),
    humanSchedule: strOr(record, "humanSchedule"),
    prompt: strOr(record, "prompt"),
    recurring: bool(record, "recurring") ?? false,
    durable: bool(record, "durable") ?? false,
  });
}

/** The one wrapper every frame of this unit shares. */
function item(state: conversationv1.AgentCron["state"]): conversationv1.AgentActivity["item"] {
  return { case: "cron", value: create(conversationv1.AgentCronSchema, { state }) };
}

/** A settled frame around one answered act. */
function success(
  call: PendingCall,
  act: conversationv1.AgentCronSuccess["act"],
  outcome: ToolOutcome,
): conversationv1.AgentActivity["item"] {
  return item({
    case: "success",
    value: create(conversationv1.AgentCronSuccessSchema, { act, settledAt: settle(call, outcome) }),
  });
}

export const cronConverter: ToolConverter = {
  kind: "cron",
  // AgentCron declares no progress arm.
  carriesProgress: false,

  start(call) {
    const act = actOf(call);
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act: act.case }, "a cron call was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentCronStartSchema, {
        act,
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the cron call never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentCronFailureSchema, { error: failureOf(call, outcome) }),
      });
    }
    const act = actNameOf(call);
    const output = asRecord(outcome.structured);
    if (act === undefined || output === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId, act },
        "a settled cron call carried no act or no typed output; no success frame is produced",
      );
      return undefined;
    }
    if (act === "list") {
      const jobs = arr(output, "jobs");
      if (jobs === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a cron listing stated no jobs array; an empty listed arm would tell the reader to forget every job it knows",
        );
        return undefined;
      }
      const rows = jobs
        .map((entry) => jobOf(entry, call.toolUseId))
        .filter((job): job is conversationv1.AgentCronJob => job !== undefined);
      LOGGER.logVerbose({ tool_use_id: call.toolUseId, jobs: rows.length }, "the job set was read");
      return success(
        call,
        { case: "listed", value: create(conversationv1.AgentCronListedSchema, { jobs: rows }) },
        outcome,
      );
    }
    const jobId = str(output, "id");
    if (jobId === undefined) {
      // Both remaining arms are ABOUT one job; without its id neither names
      // anything the reader can act on.
      LOGGER.debug(
        { tool_use_id: call.toolUseId, act },
        "a settled cron call named no job id; no success frame is produced",
      );
      return undefined;
    }
    if (act === "delete") {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId, job_id: jobId }, "a job was removed");
      return success(
        call,
        { case: "deleted", value: create(conversationv1.AgentCronDeletedSchema, { jobId }) },
        outcome,
      );
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, job_id: jobId }, "a job was created");
    return success(
      call,
      {
        case: "created",
        value: create(conversationv1.AgentCronCreatedSchema, {
          jobId,
          humanSchedule: strOr(output, "humanSchedule"),
          recurring: bool(output, "recurring") ?? false,
          durable: bool(output, "durable") ?? false,
        }),
      },
      outcome,
    );
  },
};
