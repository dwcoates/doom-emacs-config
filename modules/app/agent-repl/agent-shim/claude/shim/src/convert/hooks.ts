/**
 * convert/hooks.ts — the user's own configured automation, firing around the work.
 *
 * # Quiet by default, loud when it refuses
 *
 * A succeeded hook draws nothing; a failing one draws a card; a BLOCKING one is
 * a refusal the user must be able to understand, which is why the refusal text
 * is the whole point of that arm.
 *
 * # No hook record is stored (owner ruling 2026-10-06)
 *
 * "We should stop storing hook records, they are just bloat." A hook firing that
 * draws nothing — its start, a success, a cancellation, the vendor's
 * `hook_progress` — produces NO entry at all: it is logged at DEBUG with the
 * session's running count and counted into the INFO summary
 * {@link HookRegistry.reportDropped} writes when the query ends. Before the
 * ruling every `SessionStart:resume` firing filled a history page with a row
 * that drew nothing.
 *
 * A FAILED or BLOCKED firing draws its card LIVE, so it is still handed to the
 * record plane — its start (named from the registry, so the card keeps its
 * `name (event)` headline) and its outcome, together at the response. The
 * store delivers a hook line to the standing watches and keeps none of it
 * (shim-store `hook_dropped`): the card is drawn while the session runs, a
 * daemon that restarts never redraws it, and one written before the feed's
 * watch subscribed is not drawn at all (accepted by the owner, 2026-10-06).
 *
 * # A hook NEVER ends a turn
 *
 * Stop hooks fire AFTER a turn's stop and never determine how it ended. The
 * turn's own terminal is authoritative, and the sixteen failure arms include the
 * vendor's own hook-stop terminals precisely so those are RELAYED from the
 * result rather than inferred from hook activity.
 *
 * # Duration is spanned by the shim
 *
 * The vendor's hook response carries no duration, so the shim spans its own
 * started→response instants. That is a derived figure and it is stated as such;
 * a hook whose start this shim never saw gets no duration to derive from, and
 * produces no succeeded frame rather than a made-up one.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { activityEntry, agentActivity, startedAt } from "./entries.js";
import type { FoldContext } from "./fold-context.js";
import { hookActivityId } from "./ids.js";

const LOGGER = bindLog({ component: "shim-convert-hooks", operation: "shim.convert.hooks" });

/** How many hook firings are remembered before the oldest is forgotten. */
const HOOK_REGISTRY_CAPACITY = 128;

/** One hook firing, remembered until its response arrives. */
export interface PendingHook {
  readonly hookId: string;
  /** The firing's activity identity, minted once from its id (ids.ts). */
  readonly activityId: conversationv1.AgentActivityId;
  /** The `hook_started` record's own uuid: the start frame's write identity. */
  readonly startUuid: string;
  readonly hookName: string;
  readonly event: conversationv1.AgentHookEvent;
  readonly startedAtMs: number;
}

/** The hook firings in flight, and the hook records this query dropped. */
export interface HookRegistry {
  remember(hook: PendingHook): void;
  take(hookId: string): PendingHook | undefined;
  /**
   * Count one hook record that was not stored, by its kind; answers the
   * query's running total across every kind.
   */
  noteDropped(kind: DroppedHookKind): number;
  /**
   * Write the INFO summary of what this query dropped, and start counting
   * afresh. Writes nothing when nothing was dropped.
   */
  reportDropped(why: string): void;
}

/** The hook records that draw nothing, and so are never stored. */
export type DroppedHookKind = "start" | "succeeded" | "cancelled" | "progress";

export function createHookRegistry(): HookRegistry {
  const hooks = new Map<string, PendingHook>();
  let dropped = new Map<DroppedHookKind, number>();
  let droppedTotal = 0;
  return {
    remember(hook) {
      if (hooks.size >= HOOK_REGISTRY_CAPACITY) {
        const oldest = hooks.keys().next();
        if (oldest.done !== true) {
          // warn: a defect because bounded hook bookkeeping discarded an unanswered hook.
          LOGGER.warn(
            { hook_id: oldest.value },
            "forgetting the oldest unanswered hook: the in-flight registry is full",
          );
          hooks.delete(oldest.value);
        }
      }
      hooks.set(hook.hookId, hook);
    },
    take(hookId) {
      const hook = hooks.get(hookId);
      if (hook !== undefined) hooks.delete(hookId);
      return hook;
    },
    noteDropped(kind) {
      dropped.set(kind, (dropped.get(kind) ?? 0) + 1);
      droppedTotal += 1;
      return droppedTotal;
    },
    reportDropped(why) {
      if (droppedTotal === 0) return;
      LOGGER.info(
        { dropped_total: droppedTotal, dropped_by_kind: Object.fromEntries(dropped), why },
        "hook records this query produced that draw nothing were not stored",
      );
      dropped = new Map();
      droppedTotal = 0;
    },
  };
}

/** Drop one hook record that draws nothing: DEBUG, with the running count. */
function dropHookRecord(
  registry: HookRegistry,
  kind: DroppedHookKind,
  fields: { readonly hook_id: string; readonly hook: string; readonly event: string },
): readonly PersistEntry[] {
  const total = registry.noteDropped(kind);
  LOGGER.debug(
    { ...fields, hook_record: kind, dropped_total: total },
    "a hook record that draws nothing is not stored",
  );
  return [];
}

/**
 * The vendor's hook-event literal, in this contract's enum.
 *
 * The vendor spells its HOOK_EVENTS in PascalCase; the enum spells the same
 * closed set in the repo's own convention. An event the vendor adds later maps
 * to UNSPECIFIED, which the proto declares as a malformed frame — so it is
 * logged loudly rather than silently becoming PreToolUse.
 */
function hookEvent(literal: string): conversationv1.AgentHookEvent {
  const screaming = literal
    .replace(/([a-z0-9])([A-Z])/g, "$1_$2")
    .replace(/([A-Z]+)([A-Z][a-z])/g, "$1_$2")
    .toUpperCase();
  // protobuf-es STRIPS the enum's own prefix from its generated member names:
  // `AGENT_HOOK_EVENT_PRE_TOOL_USE` is generated as `PRE_TOOL_USE`. Spelling the
  // prefix here made EVERY lookup miss, so every hook event resolved UNSPECIFIED.
  const key = screaming as keyof typeof conversationv1.AgentHookEvent;
  const value = conversationv1.AgentHookEvent[key];
  if (typeof value !== "number" || value === conversationv1.AgentHookEvent.UNSPECIFIED) {
    LOGGER.debug(
      { hook_event: literal },
      "the vendor named a hook event this contract does not spell",
    );
    return conversationv1.AgentHookEvent.UNSPECIFIED;
  }
  return value;
}

/**
 * The refusal text of a hook that BLOCKED, or `undefined` for one that did not.
 *
 * A HOOK THAT BLOCKED is the refusal the user must understand, and it is told
 * apart from a hook that merely failed by whether it produced BLOCKING TEXT —
 * the vendor's `output`, which is what the gated action is answered with and
 * what the model reads.
 *
 * STDERR IS NOT THAT SIGNAL. A hook that gates nothing still writes stderr when
 * it fails — a `SessionStart` hook with no interpreter on PATH is the grounded
 * case — and reading stderr as blocking text drew every such failure as a
 * refusal of an action that was never gated.
 *
 * The engine asks this too: a hook that blocks BEFORE the vendor's `init` has
 * blocked the session's own opening, and StartSession refuses with this text as
 * its reason. Both readings come from here so the two cannot drift apart.
 */
export function hookBlockingText(
  message: Pick<
    Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>,
    "outcome" | "output"
  >,
): string | undefined {
  if (message.outcome !== "error") return undefined;
  return message.output === "" ? undefined : message.output;
}

/** A hook's printed output, when it printed anything. */
function hookOutput(stdout: string, stderr: string): conversationv1.AgentHookOutput | undefined {
  if (stdout === "" && stderr === "") return undefined;
  return create(conversationv1.AgentHookOutputSchema, { stdout, stderr });
}

/**
 * `hook_started` — which hook, on which event. Remembered, and NOT stored: a
 * start draws nothing on its own, and a firing that goes on to fail or block
 * has its start written beside its outcome ({@link convertHookResponse}).
 */
export function convertHookStarted(
  message: Extract<SdkMessage, { type: "system"; subtype: "hook_started" }>,
  context: FoldContext,
  registry: HookRegistry,
): readonly PersistEntry[] {
  // THE FIRING ID IS ITS IDENTITY, read through ids.ts before anything is
  // remembered: an empty one names no firing its outcome could join, and is the
  // converter defect it always was, though nothing of the start is stored.
  const activityId = hookActivityId(message.hook_id);
  registry.remember({
    hookId: message.hook_id,
    activityId,
    startUuid: message.uuid,
    hookName: message.hook_name,
    event: hookEvent(message.hook_event),
    startedAtMs: context.nowMs(),
  });
  LOGGER.logVerbose(
    { hook_id: message.hook_id, hook: message.hook_name, event: message.hook_event },
    "a hook fired",
  );
  return dropHookRecord(registry, "start", {
    hook_id: message.hook_id,
    hook: message.hook_name,
    event: message.hook_event,
  });
}

/**
 * `hook_progress` — a running hook's partial output. Draws nothing, so it is
 * not stored; before the ruling it landed as vendor-specific residue.
 */
export function convertHookProgress(
  message: Extract<SdkMessage, { type: "system"; subtype: "hook_progress" }>,
  registry: HookRegistry,
): readonly PersistEntry[] {
  return dropHookRecord(registry, "progress", {
    hook_id: message.hook_id,
    hook: message.hook_name,
    event: message.hook_event,
  });
}

/** The start frame of a firing that went on to fail or block. */
function startEntry(context: FoldContext, pending: PendingHook): PersistEntry {
  return activityEntry(
    context,
    {
      agentId: context.mainAgentId,
      vendorUuid: pending.startUuid,
      discriminator: "activity.hook.start",
    },
    agentActivity(pending.activityId, {
      case: "hook",
      value: create(conversationv1.AgentHookSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentHookStartSchema, {
            hookName: pending.hookName,
            event: pending.event,
            // The vendor's hook_started names no gated call, so the join to
            // the call this firing gates has no producer and stays UNSET.
            startedAt: startedAt(pending.startedAtMs),
          }),
        },
      }),
    }),
  );
}

/** `hook_response` — how the firing went. */
export function convertHookResponse(
  message: Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>,
  context: FoldContext,
  registry: HookRegistry,
): readonly PersistEntry[] {
  // Read first, for the same reason as the start's: an empty id is a defect.
  const activityId = hookActivityId(message.hook_id);
  const pending = registry.take(message.hook_id);
  const settledAtMs = context.nowMs();
  const durationMs = pending === undefined ? undefined : settledAtMs - pending.startedAtMs;
  const command = message.hook_name;
  const output = hookOutput(message.stdout, message.stderr);
  const exitCode = message.exit_code ?? 0;

  const dropped = { hook_id: message.hook_id, hook: message.hook_name, event: message.hook_event };
  let result: conversationv1.AgentHook["result"];
  if (message.outcome === "cancelled") {
    LOGGER.info({ hook_id: message.hook_id }, "a hook was cancelled before it finished");
    return dropHookRecord(registry, "cancelled", dropped);
  } else if (message.outcome === "error") {
    // Blocking or merely failing is {@link hookBlockingText}'s single reading,
    // shared with the engine's start gate.
    const blockingText = hookBlockingText(message);
    if (blockingText !== undefined) {
      LOGGER.info(
        {
          hook_id: message.hook_id,
          hook: message.hook_name,
          event: message.hook_event,
          blocking_text: blockingText,
        },
        "a hook blocked the gated action",
      );
      result = {
        case: "blockingError",
        value: create(conversationv1.AgentHookBlockingErrorSchema, { command, blockingText }),
      };
    } else {
      LOGGER.info(
        { hook_id: message.hook_id, hook: message.hook_name, exit_code: exitCode },
        "a hook failed without blocking anything",
      );
      result = {
        case: "nonBlockingError",
        value: create(conversationv1.AgentHookNonBlockingErrorSchema, {
          command,
          exitCode,
          durationMs: BigInt(Math.max(0, durationMs ?? 0)),
          output,
        }),
      };
    }
  } else {
    LOGGER.logVerbose({ hook_id: message.hook_id, hook: message.hook_name }, "a hook succeeded");
    return dropHookRecord(registry, "succeeded", dropped);
  }

  // THE START RIDES WITH THE OUTCOME, so the live card is headlined by the
  // hook's name and event exactly as when every start was written as it fired.
  // A firing whose start this shim never saw has only its outcome.
  return [
    ...(pending === undefined ? [] : [startEntry(context, pending)]),
    activityEntry(
      context,
      {
        agentId: context.mainAgentId,
        vendorUuid: message.uuid,
        discriminator: `activity.hook.${String(result.case)}`,
      },
      agentActivity(activityId, {
        case: "hook",
        value: create(conversationv1.AgentHookSchema, { result }),
      }),
    ),
  ];
}
