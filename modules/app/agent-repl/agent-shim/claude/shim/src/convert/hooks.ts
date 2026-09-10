/**
 * convert/hooks.ts — the user's own configured automation, firing around the work.
 *
 * # Quiet by default, loud when it refuses
 *
 * A succeeded hook draws nothing; a failing one draws a card; a BLOCKING one is
 * a refusal the user must be able to understand, which is why the refusal text
 * is the whole point of that arm.
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
  readonly hookName: string;
  readonly event: conversationv1.AgentHookEvent;
  readonly startedAtMs: number;
}

/** The hook firings in flight. */
export interface HookRegistry {
  remember(hook: PendingHook): void;
  take(hookId: string): PendingHook | undefined;
}

export function createHookRegistry(): HookRegistry {
  const hooks = new Map<string, PendingHook>();
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
  };
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

/** A hook's printed output, when it printed anything. */
function hookOutput(stdout: string, stderr: string): conversationv1.AgentHookOutput | undefined {
  if (stdout === "" && stderr === "") return undefined;
  return create(conversationv1.AgentHookOutputSchema, { stdout, stderr });
}

/** `hook_started` — which hook, on which event. */
export function convertHookStarted(
  message: Extract<SdkMessage, { type: "system"; subtype: "hook_started" }>,
  context: FoldContext,
  registry: HookRegistry,
): readonly PersistEntry[] {
  const startedAtMs = context.nowMs();
  const event = hookEvent(message.hook_event);
  registry.remember({
    hookId: message.hook_id,
    hookName: message.hook_name,
    event,
    startedAtMs,
  });
  LOGGER.logVerbose(
    { hook_id: message.hook_id, hook: message.hook_name, event: message.hook_event },
    "a hook fired",
  );
  return [
    activityEntry(
      context,
      {
        agentId: context.mainAgentId,
        vendorUuid: message.uuid,
        discriminator: "activity.hook.start",
      },
      agentActivity(hookActivityId(message.hook_id), {
        case: "hook",
        value: create(conversationv1.AgentHookSchema, {
          result: {
            case: "start",
            value: create(conversationv1.AgentHookStartSchema, {
              hookName: message.hook_name,
              event,
              // The vendor's hook_started names no gated call, so the join to
              // the call this firing gates has no producer and stays UNSET.
              startedAt: startedAt(startedAtMs),
            }),
          },
        }),
      }),
    ),
  ];
}

/** `hook_response` — how the firing went. */
export function convertHookResponse(
  message: Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>,
  context: FoldContext,
  registry: HookRegistry,
): readonly PersistEntry[] {
  const pending = registry.take(message.hook_id);
  const settledAtMs = context.nowMs();
  const durationMs = pending === undefined ? undefined : settledAtMs - pending.startedAtMs;
  const command = message.hook_name;
  const output = hookOutput(message.stdout, message.stderr);
  const exitCode = message.exit_code ?? 0;

  let result: conversationv1.AgentHook["result"];
  if (message.outcome === "cancelled") {
    LOGGER.info({ hook_id: message.hook_id }, "a hook was cancelled before it finished");
    result = {
      case: "cancelled",
      value: create(conversationv1.AgentHookCancelledSchema, {}),
    };
  } else if (message.outcome === "error") {
    // A HOOK THAT BLOCKED is the refusal the user must understand, and it is
    // told apart from a hook that merely failed by whether it produced BLOCKING
    // TEXT — the vendor's `output`, which is what the gated call is answered
    // with and what the model reads.
    //
    // STDERR IS NOT THAT SIGNAL. A hook that gates nothing still writes stderr
    // when it fails — a `SessionStart` hook with no interpreter on PATH is the
    // grounded case — and reading stderr as blocking text drew every such
    // failure as a refusal of a call that was never gated.
    const blockingText = message.output;
    if (blockingText !== "") {
      LOGGER.info(
        { hook_id: message.hook_id, hook: message.hook_name },
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
    result = {
      case: "succeeded",
      value: create(conversationv1.AgentHookSucceededSchema, {
        command,
        exitCode,
        durationMs: BigInt(Math.max(0, durationMs ?? 0)),
        output,
      }),
    };
  }

  return [
    activityEntry(
      context,
      {
        agentId: context.mainAgentId,
        vendorUuid: message.uuid,
        discriminator: `activity.hook.${String(result.case)}`,
      },
      agentActivity(hookActivityId(message.hook_id), {
        case: "hook",
        value: create(conversationv1.AgentHookSchema, { result }),
      }),
    ),
  ];
}
