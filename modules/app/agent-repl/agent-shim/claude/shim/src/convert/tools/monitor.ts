/**
 * convert/tools/monitor.ts — a background watcher, armed and later ended.
 *
 * # THE TOOL RESULT IS AN ARMING RECEIPT, NOT AN ENDING
 *
 * A monitor is ALWAYS DETACHED: the watch outlives the call, its events reach
 * the agent as ordinary turn input, and the turn never blocks on it. So the
 * `tool_result` — "Monitor started (task b1xo9bsxw, timeout 600000ms)" in the
 * one observed corpus line — says the watch is LIVE, not that it finished.
 * Settling the unit on it would draw a watch as over while it was still
 * running, which is the one thing a footer chip must never say.
 *
 * `settle` therefore answers `undefined` on a successful arming — the fold's
 * own vocabulary for "this result is not the unit's conclusion" — and the
 * `ended` arm is minted by {@link monitorEnded} at the moment the watch leaves
 * the live set, which is observed elsewhere. An arming that FAILED is a real
 * conclusion: nothing was ever watching, so that settles as `failure`.
 *
 * # The lifetime comes from the CALL, not the receipt
 *
 * `MonitorOutput` restates `timeoutMs` and `persistent`, but the start frame is
 * built before any output exists, so the input's own `persistent`/`timeout_ms`
 * are what it reads. The vendor's own rule makes the arms exclusive: the
 * timeout is ignored when persistent is set.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { bool, failureOf, obj, str, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-monitor",
  operation: "shim.convert.tools.monitor",
});

/** How long the watch may live, as the call configured it. */
function lifetimeOf(call: PendingCall): conversationv1.AgentMonitorStart["lifetime"] {
  if (bool(call.input, "persistent") === true) {
    return {
      case: "persistent",
      value: create(conversationv1.AgentMonitorPersistentSchema, {}),
    };
  }
  const timeoutMs = uint(call.input, "timeout_ms");
  if (timeoutMs === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a monitor call stated neither a persistent flag nor a timeout; the watch's lifetime is left unstated",
    );
    return { case: undefined };
  }
  return {
    case: "deadline",
    value: create(conversationv1.AgentMonitorDeadlineSchema, { timeoutMs: BigInt(timeoutMs) }),
  };
}

/** What is being watched, as the call named it. */
function sourceOf(call: PendingCall): conversationv1.AgentMonitorStart["source"] {
  const command = str(call.input, "command");
  if (command !== undefined) {
    return {
      case: "command",
      value: create(conversationv1.AgentMonitorCommandSchema, { command }),
    };
  }
  const url = str(obj(call.input, "ws"), "url");
  if (url !== undefined) {
    return {
      case: "websocket",
      value: create(conversationv1.AgentMonitorWebsocketSchema, { url }),
    };
  }
  LOGGER.debug(
    { tool_use_id: call.toolUseId },
    "a monitor call named neither a command nor a websocket; the watch's source is left unstated",
  );
  return { case: undefined };
}

/** The one wrapper every frame of this unit shares. */
function item(result: conversationv1.AgentMonitor["result"]): conversationv1.AgentActivity["item"] {
  return { case: "monitor", value: create(conversationv1.AgentMonitorSchema, { result }) };
}

/**
 * The watch left the live set.
 *
 * Exported for whatever observes the detached watch's end: no cause taxonomy is
 * claimed, because the vendor reports only that it is gone. CALL is the start
 * the watch was armed with, RESTATED so the settled frame stands alone (the
 * store keeps one row per unit, and the daemon draws the monitor's tool-call
 * card from it on a replay); undefined when nothing recorded the start.
 */
export function monitorEnded(
  call: conversationv1.AgentMonitorStart | undefined,
): conversationv1.AgentActivity["item"] {
  LOGGER.logVerbose(
    { restated: call !== undefined },
    "a monitor left the live set; the watch's unit is settled as ended",
  );
  return item({ case: "ended", value: create(conversationv1.AgentMonitorEndedSchema, { call }) });
}

/** The watch as the call armed it: the start frame's body, and every settle's restatement. */
function armedAs(call: PendingCall): conversationv1.AgentMonitorStart {
  const description = str(call.input, "description");
  if (description === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a monitor call carries no description; the footer row has nothing to draw",
    );
  }
  return create(conversationv1.AgentMonitorStartSchema, {
    description: description ?? "",
    lifetime: lifetimeOf(call),
    source: sourceOf(call),
    startedAtMs: BigInt(Math.trunc(call.startedAtMs)),
  });
}

export const monitorConverter: ToolConverter = {
  kind: "monitor",
  // AgentMonitor declares no progress arm: a watch reports its events as turn
  // input, and there is no heartbeat to relay.
  carriesProgress: false,

  start(call) {
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a monitor was armed");
    return item({ case: "start", value: armedAs(call) });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the monitor never armed");
      return item({
        case: "failure",
        // THE CALL IS RESTATED: the failure replaces the start in the store,
        // and a replay draws the monitor's card from this frame alone.
        value: create(conversationv1.AgentMonitorFailureSchema, {
          failure: failureOf(outcome),
          call: armedAs(call),
        }),
      });
    }
    LOGGER.logVerbose(
      { tool_use_id: call.toolUseId },
      "a monitor's result is its ARMING RECEIPT, not its ending; the watch's unit stays open",
    );
    return undefined;
  },
};
