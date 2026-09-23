/**
 * convert/tools/subagent.ts — the spawn of a subagent, and the report it settles with.
 *
 * # The spawn's identity IS the spawning call's id
 *
 * `AgentSubagentStart.created_agent_id` names THE AGENT THIS SPAWN CREATED —
 * the join key every later frame of that agent routes by. The vendor states no
 * identity at announcement: `AgentInput` carries a description, a prompt and a
 * configuration, and the vendor's own 17-hex `agentId` first appears on the
 * RESULT and on the subagent's files.
 *
 * THE MINTING RULE SETTLES IT (project lead, landing 3, binding and in the
 * proto comment): a subagent's `AgentId` IS the spawning call's `tool_use_id`.
 * That is not a guess standing in for the vendor's id — it is the wire
 * identity, chosen precisely because the stream plane carries no agent id
 * anywhere (a subagent's own messages name `parent_tool_use_id` and nothing
 * else), and it is the SAME value `convert/fold-context.ts:subagentBook` routes
 * the subagent's frames into. So the announcement DOES produce a start frame,
 * and the container a consumer draws on it is the one every later frame lands
 * in. The vendor's 17-hex id stays on the FILE plane, where `meta.toolUseId` is
 * the link back to this call — a reader-side join, so the mock's layout can
 * stay vendor-faithful.
 *
 * {@link subagentStartFrom} is the way in for a caller that has some OTHER
 * identity to state (a re-announcement on the work's own stream).
 *
 * # One lifecycle, three vendor statuses
 *
 * The vendor answers a spawn three ways. `completed` is the conclusion and
 * settles the unit. `async_launched` and `remote_launched` are NOT conclusions:
 * the run moved to the background and will end somewhere else, so this result
 * settles nothing and the unit stays open.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { prose, settledAt, startedAt } from "../entries.js";
import { subagentId } from "../ids.js";
import type { PendingCall, ToolConverter } from "../tool-calls.js";
import { arr, asRecord, bool, failureOf, num, obj, str, uint } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-subagent", operation: "shim.convert.subagent" });

/** The vendor statuses that are NOT this unit's conclusion. */
const BACKGROUNDED_STATUSES: ReadonlySet<string> = new Set(["async_launched", "remote_launched"]);

/**
 * Whether a spawn's structured result is a LAUNCH RECEIPT rather than the run's
 * conclusion: the run moved to the background and is still going.
 */
export function isLaunchReceipt(structured: unknown): boolean {
  const status = str(asRecord(structured), "status");
  return status !== undefined && BACKGROUNDED_STATUSES.has(status);
}

// ---------------------------------------------------------------------------
// The prompt, restated on every frame
// ---------------------------------------------------------------------------

/**
 * What the subagent was asked to do, from the CALL'S OWN INPUT.
 *
 * The input is the only authority for the configuration — a result restates the
 * prompt text and nothing else — so every frame of the unit builds this from the
 * same place and the start and the terminal can never disagree.
 */
export function subagentPrompt(call: PendingCall): conversationv1.AgentSubagentPrompt {
  const input = call.input;
  const text = str(input, "prompt");
  if (text === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId, tool: call.toolName },
      "a subagent spawn states no prompt text; the instruction is carried empty",
    );
  }
  const subagentType = str(input, "subagent_type");
  const model = str(input, "model");
  return create(conversationv1.AgentSubagentPromptSchema, {
    description: str(input, "description"),
    text: text ?? "",
    subagentType,
    requestedName: str(input, "name"),
    requestedModel:
      model === undefined ? undefined : create(conversationv1.AgentModelSchema, { name: model }),
    // The vendor spells a fork as a subagent TYPE; the contract spells it as a
    // fact about the prompt, because a fork's answer rests on context its own
    // instruction never shows.
    forkedFromCaller: subagentType === "fork",
    isolation: isolationOf(call),
  });
}

/** Where the subagent was asked to run. Absent means the caller's own tree. */
function isolationOf(call: PendingCall): conversationv1.AgentSubagentPrompt["isolation"] {
  const isolation = str(call.input, "isolation");
  switch (isolation) {
    case "worktree":
      return {
        case: "worktree",
        value: create(conversationv1.AgentSubagentIsolationWorktreeSchema, {}),
      };
    case "remote":
      // The remote's handles arrive with the LAUNCH, never with the request, so
      // both stay unset here — see the file header for why the launch result
      // never reaches a frame of this unit.
      return {
        case: "remote",
        value: create(conversationv1.AgentSubagentIsolationRemoteSchema, {}),
      };
    case undefined:
      return { case: "none", value: create(conversationv1.AgentSubagentIsolationNoneSchema, {}) };
    default:
      LOGGER.debug(
        { tool_use_id: call.toolUseId, isolation },
        "a subagent spawn names an isolation this contract has no arm for; none is carried",
      );
      return { case: "none", value: create(conversationv1.AgentSubagentIsolationNoneSchema, {}) };
  }
}

// ---------------------------------------------------------------------------
// The start, for whoever knows the created agent
// ---------------------------------------------------------------------------

/**
 * The spawn's `start` arm, for a caller that KNOWS which agent it created.
 *
 * `spawn_depth` and `working_dir` stay UNSET: the first is derived by walking the
 * vendor's `parent_agent_id` chain, which this fold does not hold, and the
 * second is stated by no vendor field on either the input or the output.
 * `transcript_suppressed` is likewise unstated and therefore false.
 */
export function subagentStartFrom(
  call: PendingCall,
  createdAgentId: conversationv1.AgentId,
): conversationv1.AgentActivity["item"] {
  return {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: {
        case: "start",
        value: create(conversationv1.AgentSubagentStartSchema, {
          createdAgentId,
          prompt: subagentPrompt(call),
          startedAt: startedAt(call.startedAtMs),
        }),
      },
    }),
  };
}

// ---------------------------------------------------------------------------
// The report
// ---------------------------------------------------------------------------

/** The subagent's own words, joined from the output's text blocks. */
function reportOf(structured: Record<string, unknown>): conversationv1.AgentSubagentReport | undefined {
  const content = arr(structured, "content");
  if (content === undefined) {
    LOGGER.debug(
      {},
      "a completed subagent stated no report content; no success frame is produced",
    );
    return undefined;
  }
  const markdown = content
    .map((block) => str(asRecord(block), "text"))
    .filter((text): text is string => text !== undefined)
    .join("");
  // `structured_result` stays UNSET: neither the SDK's AgentOutput nor the
  // corpus declares a structured report beside the prose, and inventing a key
  // to read it from would be inventing a vendor field.
  return create(conversationv1.AgentSubagentReportSchema, { prose: prose(markdown) });
}

/** The vendor's usage counters, in this system's one canonical token shape. */
function usageOf(structured: Record<string, unknown>): conversationv1.TokenUsage | undefined {
  const usage = obj(structured, "usage");
  if (usage === undefined) return undefined;
  const details = obj(usage, "output_tokens_details");
  return create(conversationv1.TokenUsageSchema, {
    inputHits: create(conversationv1.TokenCacheHitsSchema, {
      read: BigInt(uint(usage, "cache_read_input_tokens") ?? 0),
    }),
    inputMisses: create(conversationv1.TokenCacheMissesSchema, {
      written: BigInt(uint(usage, "cache_creation_input_tokens") ?? 0),
      unwritten: BigInt(uint(usage, "input_tokens") ?? 0),
    }),
    outputTokens: BigInt(uint(usage, "output_tokens") ?? 0),
    outputThinkingTokens: BigInt(uint(details, "reasoning_tokens") ?? 0),
  });
}

/** What the run did with its tool calls, when the vendor broke it down. */
function toolStatsOf(
  structured: Record<string, unknown>,
): conversationv1.AgentSubagentToolStats | undefined {
  const stats = obj(structured, "toolStats");
  if (stats === undefined) return undefined;
  return create(conversationv1.AgentSubagentToolStatsSchema, {
    readCount: uint(stats, "readCount") ?? 0,
    searchCount: uint(stats, "searchCount") ?? 0,
    bashCount: uint(stats, "bashCount") ?? 0,
    editFileCount: uint(stats, "editFileCount") ?? 0,
    linesAdded: uint(stats, "linesAdded") ?? 0,
    linesRemoved: uint(stats, "linesRemoved") ?? 0,
    otherToolCount: uint(stats, "otherToolCount") ?? 0,
    frameCount: uint(stats, "frameCount") ?? 0,
  });
}

/**
 * What the run cost and what it did.
 *
 * A SYNC completion is the only path this converter settles, so the usage arm is
 * always `full` when the vendor reported usage at all — the async arm belongs to
 * whatever concludes a backgrounded run, which is not this result.
 */
function totalsOf(
  structured: Record<string, unknown>,
): conversationv1.AgentSubagentTotals | undefined {
  const durationMs = num(structured, "totalDurationMs");
  const toolUseCount = uint(structured, "totalToolUseCount");
  if (durationMs === undefined || toolUseCount === undefined) {
    LOGGER.debug(
      { duration_stated: durationMs !== undefined },
      "a completed subagent stated no duration or no tool-use count; no success frame is produced",
    );
    return undefined;
  }
  const usage = usageOf(structured);
  return create(conversationv1.AgentSubagentTotalsSchema, {
    durationMs: BigInt(Math.max(0, Math.trunc(durationMs))),
    usage: usage === undefined ? { case: undefined } : { case: "full", value: usage },
    toolUseCount,
    toolStats: toolStatsOf(structured),
  });
}

/** Which models actually ran it, in the order the vendor reports them. */
function modelsUsedOf(structured: Record<string, unknown>): conversationv1.AgentModel[] {
  const declared = arr(structured, "modelsUsed");
  const names =
    declared === undefined
      ? [str(structured, "resolvedModel")].filter((name): name is string => name !== undefined)
      : declared.filter((name): name is string => typeof name === "string");
  return names.map((name) => create(conversationv1.AgentModelSchema, { name }));
}

/** Where it actually worked, when the vendor gave it a tree of its own. */
function worktreeOf(
  structured: Record<string, unknown>,
): conversationv1.AgentSubagentWorktree | undefined {
  const path = str(structured, "worktreePath");
  const branch = str(structured, "worktreeBranch");
  if (path === undefined || branch === undefined) {
    if (path !== undefined || branch !== undefined) {
      LOGGER.debug(
        { path_stated: path !== undefined },
        "a subagent stated half a worktree; a worktree without both a path and a branch is not one",
      );
    }
    return undefined;
  }
  // `provenance` and `cleanup` stay UNSET: the vendor states neither where the
  // tree came from nor what became of it.
  return create(conversationv1.AgentSubagentWorktreeSchema, { path, branch });
}

// ---------------------------------------------------------------------------
// The converter
// ---------------------------------------------------------------------------

/** The vendor spells this spawn both `Agent` and `Task`; one unit kind either way. */
export const subagentConverter: ToolConverter = {
  kind: "subagent",
  // `AgentSubagent` declares no progress arm — a running spawn reports through
  // its own update arm, which no tool result carries.
  carriesProgress: false,

  start(call) {
    // THE CREATED AGENT IS THE CALL'S OWN ID (landing 3's minting rule), which
    // is exactly the book its frames route into. Producing no start frame left
    // the spawn unit with no start at all: a consumer had no container to draw,
    // and the unit's terminal was its first and only frame.
    LOGGER.logVerbose(
      { tool_use_id: call.toolUseId, tool: call.toolName },
      "announcing a subagent spawn; the created agent's id is the spawning call's own",
    );
    return subagentStartFrom(call, subagentId(call.toolUseId));
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a subagent spawn failed");
      return {
        case: "subagent",
        value: create(conversationv1.AgentSubagentSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentSubagentFailureSchema, {
              // `cause` stays UNSET: no vendor field states a user stop, and an
              // arm claiming one would say a person did what nothing observed.
              error: failureOf(outcome),
            }),
          },
        }),
      };
    }
    const structured = asRecord(outcome.structured);
    if (structured === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a subagent result carried no typed output; no terminal frame is produced",
      );
      return undefined;
    }
    if (isLaunchReceipt(structured)) {
      LOGGER.info(
        { tool_use_id: call.toolUseId, status: str(structured, "status"), is_async: bool(structured, "isAsync") },
        "a subagent spawn moved to the background; this result is a launch receipt, not the run's conclusion",
      );
      return undefined;
    }
    return settleCompleted(call, structured, outcome.settledAtMs);
  },
};

/** The awaited spawn's conclusion: its report, and what the run cost. */
function settleCompleted(
  call: PendingCall,
  structured: Record<string, unknown>,
  settledAtMs: number,
): conversationv1.AgentActivity["item"] | undefined {
  const report = reportOf(structured);
  const totals = totalsOf(structured);
  if (report === undefined || totals === undefined) return undefined;
  if (str(structured, "agentId") === undefined) {
    // NOT FATAL, and it does not cost the conclusion its identity: the created
    // agent is the SPAWNING CALL'S OWN ID (the minting rule at the head of this
    // file), which is stated below whether or not the vendor echoed its own
    // 17-hex locator here. What is lost is only the link to this run's files.
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a completed subagent named no vendor agent id; its transcript files cannot be linked to this spawn",
    );
  }
  return {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: {
        case: "success",
        value: create(conversationv1.AgentSubagentSuccessSchema, {
          // THE SAME IDENTITY THE START STATES, restated so a delivery that
          // carries only this frame — a replayed history, a transcript read
          // with nothing watching live — can still name the agent this spawn
          // created. It is the minting rule's value, not a second identity.
          createdAgentId: subagentId(call.toolUseId),
          prompt: subagentPrompt(call),
          report,
          totals,
          modelsUsed: modelsUsedOf(structured),
          worktree: worktreeOf(structured),
          // The agent TYPE that actually ran — `resolvedModel` names a MODEL and
          // belongs to `models_used`, not here.
          resolvedSubagentType: str(structured, "agentType"),
          settledAt: settledAt(settledAtMs),
        }),
      },
    }),
  };
}
