/**
 * convert/tool-results.ts — the vendor's USER-role records, which are mostly not
 * a user.
 *
 * The vendor delivers tool results inside user-role records. That is an accident
 * of its API shape and this contract erases it: a tool result is not something a
 * person said, so a `tool_result` block settles the unit its `tool_use_id` names
 * and never becomes a prompt.
 *
 * WHAT ABOUT THE ACTUAL PROMPT? R15: the shim's own `AgentPrompt` row, written
 * by StartTurn, is the ONE served prompt. The vendor's echo of the same prompt
 * back down the stream is the same fact a second time, so it is dropped here —
 * and the sidecar classifies the transcript's copy as unserved for the same
 * reason. A record carrying neither a tool result nor a prompt echo is residue.
 */
import { bindLog } from "../log.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { toolResultContent } from "./blocks.js";
import { spawningCallOf, type FoldContext } from "./fold-context.js";
import { residueEntry, residueForMessage } from "./residue.js";
import {
  convertProgressBeat,
  convertToolResult,
  type CallRegistry,
  type ToolConverter,
  type ToolOutcome,
} from "./tool-calls.js";
import { skillDocumentSettle } from "./tools/skill-use.js";
import { bashDetachmentEntry, type TaskKindRegistry } from "./detached.js";
import { activityEntry, agentActivity } from "./entries.js";
import { convertPeerMessage } from "./peer.js";
import { toolCallActivityId } from "./ids.js";

const LOGGER = bindLog({ component: "shim-convert-results", operation: "shim.convert.results" });

/** A user-record content block, read loosely before it is typed. */
interface RawUserBlock {
  readonly type?: string;
  readonly tool_use_id?: string;
  readonly content?: unknown;
  readonly is_error?: boolean;
}

/**
 * One `type: "user"` record.
 *
 * `tool_use_result` is the tool's own typed Output object and is the source
 * every converter reads; the block's `content` is what the MODEL was shown.
 */
export function convertUserRecord(
  message: Extract<SdkMessage, { type: "user" }>,
  context: FoldContext,
  registry: CallRegistry,
  converters: ReadonlyMap<string, ToolConverter>,
  taskKinds: TaskKindRegistry,
): readonly PersistEntry[] {
  const record = message as unknown as Record<string, unknown>;
  const skillDocument = skillDocumentEntry(message, context, registry);
  if (skillDocument !== undefined) return skillDocument;

  // A MESSAGE FROM ANOTHER CLAUDE, not a person and not a prompt (origin.kind
  // "peer" — an inter-session peer or a subagent hand-back). It is recognized
  // BEFORE the R15 drop below: it carries no tool result, so without this it
  // would fall through as "the prompt the shim already wrote" and vanish from a
  // resumed session's feed. Emitted as the ONE peer-message row instead.
  const peer = convertPeerMessage(message, context);
  if (peer !== undefined) return [peer];

  const content = (message.message as { content?: unknown } | undefined)?.content;
  const blocks = Array.isArray(content) ? (content as RawUserBlock[]) : [];
  const results = blocks.filter((block) => block.type === "tool_result");
  if (results.length === 0) {
    if (record.isSynthetic === true) {
      LOGGER.logVerbose({ uuid: message.uuid }, "a synthetic user record carries no tool result; dropped");
      return [];
    }
    LOGGER.logVerbose(
      { uuid: message.uuid },
      "a user record with no tool result is the prompt the shim already wrote (R15); dropped",
    );
    return [];
  }

  const settledAtMs = context.nowMs();
  const structured = record.tool_use_result;
  // THE STREAM THE RESULTS RODE — the identity their calls were held under.
  const stream = spawningCallOf(message.parent_tool_use_id);
  // THE SDK DECLARES A USER RECORD'S uuid OPTIONAL, and a write id needs a
  // coordinate. The settling call's own id is the deterministic stand-in: it
  // names exactly this settle and nothing else, so a replay still absorbs.
  const uuidOf = (toolUseId: string): string => message.uuid ?? `tool_result:${toolUseId}`;
  const entries: PersistEntry[] = [];
  for (const block of results) {
    const toolUseId = block.tool_use_id;
    if (typeof toolUseId !== "string" || toolUseId === "") {
      // warn: a defect because a tool result without a call id cannot settle a unit.
      LOGGER.warn(
        { uuid: message.uuid },
        "a tool_result block names no call; nothing can be settled by it",
      );
      entries.push(
        residueEntry(
          context,
          block,
          residueForMessage(block, "tool_result block has no tool_use_id"),
          "residue.unparsed",
        ),
      );
      continue;
    }
    // A DENIED CALL RETIRES ITS UNIT (project lead ruling, 2026-09-01, final).
    // The start is ALREADY on the stream — the `tool_use` block precedes
    // `canUseTool`, and starts are never deferred — so the unit exists and must
    // reach a terminal like every other. It settles `failure` with content
    // UNSET (the producer observed no error content: the documented meaning of
    // an unset `AgentToolFailure.content`) and `settled_at` stamped. It is
    // DRAWN denied through its permission unit, whose id IS this unit's
    // AgentActivityId, so no `denied` cause on AgentToolFailure is needed.
    //
    // The vendor's own `tool_result` for a denial is the deny sentence the
    // MODEL was shown (`toolUseResult` is a bare "Error: …" string there, not
    // the tool's Output object), which is why neither is carried.
    if (context.deniedCall(toolUseId)) {
      LOGGER.debug(
        { tool_use_id: toolUseId, uuid: message.uuid },
        "a denied call retires its unit: settling failure with no content, drawn denied via its permission unit",
      );
      entries.push(
        ...convertToolResult(
          converters,
          context,
          registry,
          toolUseId,
          { content: undefined, isError: true, structured: undefined, settledAtMs },
          { vendorUuid: uuidOf(toolUseId) },
          stream,
        ),
      );
      continue;
    }
    const outcome: ToolOutcome = {
      content: toolResultContent(block.content),
      isError: block.is_error === true,
      structured,
      settledAtMs,
    };
    // A BACKGROUNDED SHELL COMMAND DID NOT END, IT MOVED — and its result is the
    // only record that says WHY (the task stream carries no cause on any frame).
    // The announcement is read off the result BEFORE the unit is settled,
    // because settling it consumes the call this frame is about.
    const pending = registry.peek(toolUseId);
    if (pending !== undefined && pending.toolName === "Bash") {
      const detachment = bashDetachmentEntry(
        context,
        pending.agentId,
        uuidOf(toolUseId),
        toolUseId,
        structured,
        // The vendor names the spool path only in the result's PROSE on a
        // backgrounded Bash; see outputPathFromProse.
        outcome.content,
        // So the cause this result STATES is the one the later
        // `task_notification` upserts, rather than a hard-coded `requested`.
        taskKinds,
      );
      if (detachment !== undefined) entries.push(detachment);
    }
    entries.push(
      ...convertToolResult(
        converters,
        context,
        registry,
        toolUseId,
        outcome,
        { vendorUuid: uuidOf(toolUseId) },
        stream,
      ),
    );
  }
  return entries;
}

/**
 * The skill DOCUMENT record, joined to its call by `sourceToolUseID`.
 *
 * THE SKILL UNIT SETTLES ON THE DOCUMENT, never on the acknowledgement: the
 * tool's own result is a bare restatement of the skill's name, and what is worth
 * carrying is the markdown the skill put in front of the agent, which arrives
 * afterwards as a separate injected record linked back to the call. The join is
 * by that link and NEVER by name-matching.
 *
 * The SDK declares neither `isMeta` nor `sourceToolUseID`; both are observed on
 * the vendor's own records, and observed shapes beat declared types.
 */
function skillDocumentEntry(
  message: Extract<SdkMessage, { type: "user" }>,
  context: FoldContext,
  registry: CallRegistry,
): readonly PersistEntry[] | undefined {
  const record = message as unknown as Record<string, unknown>;
  const sourceToolUseId = record.sourceToolUseID;
  if (typeof sourceToolUseId !== "string" || sourceToolUseId === "") return undefined;
  if (record.isMeta !== true) return undefined;
  const call = registry.peek(sourceToolUseId);
  if (call === undefined || call.toolName !== "Skill") return undefined;
  const content = (message.message as { content?: unknown } | undefined)?.content;
  const markdown =
    typeof content === "string"
      ? content
      : Array.isArray(content)
        ? content
            .map((block) => (block as { text?: unknown }).text)
            .filter((text): text is string => typeof text === "string")
            .join("")
        : "";
  if (markdown === "") {
    // warn: a defect because an empty skill document leaves its invocation open.
    LOGGER.warn(
      { tool_use_id: sourceToolUseId },
      "a skill document record carried no markdown; the skill unit stays open",
    );
    return [];
  }
  const item = skillDocumentSettle(
    call,
    markdown,
    // The document record states no tool allowances of its own: the declared
    // set rode the acknowledgement, which was retained onto the call.
    call.retainedAllowedTools,
    context.nowMs(),
  );
  if (item === undefined) {
    // warn: a defect because a nameless skill document cannot populate its success arm.
    LOGGER.warn(
      { tool_use_id: sourceToolUseId },
      "a skill document names no skill; the settle frame is skipped rather than written with no arm",
    );
    return [];
  }
  registry.take(sourceToolUseId);
  LOGGER.logVerbose({ tool_use_id: sourceToolUseId }, "settling a skill unit on its document");
  return [
    activityEntry(
      context,
      {
        agentId: call.agentId,
        vendorUuid: message.uuid ?? `skill_document:${call.toolUseId}`,
        discriminator: "activity.skill_use.success",
      },
      agentActivity(toolCallActivityId(call.toolUseId), item),
    ),
  ];
}

/**
 * One `type: "tool_progress"` beat.
 *
 * `elapsed_time_seconds` and `heartbeat` are CONSUMED, never forwarded: the
 * start instant plus this beat replace them.
 */
export function convertToolProgressMessage(
  message: Extract<SdkMessage, { type: "tool_progress" }>,
  context: FoldContext,
  registry: CallRegistry,
  converters: ReadonlyMap<string, ToolConverter>,
): readonly PersistEntry[] {
  return convertProgressBeat(converters, context, registry, message.tool_use_id, context.nowMs(), {
    vendorUuid: message.uuid,
  });
}
