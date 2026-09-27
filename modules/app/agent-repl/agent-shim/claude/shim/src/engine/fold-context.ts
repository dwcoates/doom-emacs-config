/**
 * engine/fold-context.ts — what the engine tells the fold, and what it expects
 * back.
 *
 * # Why the context exists
 *
 * The fold converts ONE SDK message at a time and accumulates nothing. Several
 * conversions nevertheless need a fact the message itself does not carry: which
 * turn is open, whether this turn is a keep-alive, which agent's book the main
 * thread is, whether a `permission_denied` record refers to an ask the shim is
 * already holding, which tool call a task id belongs to. Every one of those is
 * SESSION state, which is the engine's, so the engine passes it IN rather than
 * letting the fold keep a second copy that can drift.
 *
 * Each accessor is ONE LOOKUP of a value the engine already holds. Nothing here
 * is a scan, and nothing here is a history.
 *
 * # Why the engine declares its own view of the fold
 *
 * `convert/fold.ts` is the record-plane agent's file and its output shape is
 * theirs to settle. {@link EngineFold} is the narrow shape the ENGINE depends
 * on — entries to persist, and the fact that a message ended the turn — so the
 * two can be built in parallel and the dependency is stated rather than
 * implied. The record plane's `Fold` satisfies this structurally.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../proto.js";
import type { PersistEntry } from "../store/persistence.js";
import type { SdkMessage } from "../sdk/types.js";

/**
 * A change to a file the agent made, as the diagnostics join remembers it.
 *
 * BOTH HALVES ARE LOAD-BEARING: the unit is what the report is attached to, and
 * the kind is which arm it rides on that unit.
 */
export interface LastChange {
  readonly unit: conversationv1.AgentActivityId;
  readonly kind: "write" | "edit";
}

/** What the shim is already holding when a message arrives. */
export interface FoldContext {
  /** The conversation's book: the ORIGINAL vendor session id (R9). */
  readonly mainAgentId: conversationv1.AgentId;
  /** The open turn, when one is open. */
  readonly turnId?: conversationv1.TurnId;
  /** True while the open turn is the shim's own keep-alive. */
  readonly keepalive: boolean;
  /** The clock, injected so conversions are testable without one. */
  readonly nowMs: () => number;
  /**
   * The ACCOUNT this session runs as: `CLAUDE_CONFIG_DIR`, a directory PATH.
   *
   * DIAGNOSTIC ONLY, and never on the wire. A vendor credential rejection or a
   * "model or resource does not exist" cannot be told apart — a clobbered token,
   * an expired one, an account/scope mismatch — without knowing WHICH config-dir
   * the failing token belonged to, so the terminal's api-failure record names it.
   * It is a directory name, not a secret, and no credential VALUE is ever logged.
   * Optional: a fold driven without an engine (unit tests) may state none.
   */
  readonly claudeConfigDir?: string;
  /**
   * The model the turn is running under, when one is in effect.
   *
   * DIAGNOSTIC ONLY. A "model or resource does not exist" failure is only
   * legible beside the model that provoked it, so the terminal's api-failure
   * record names it. Optional: unset until the session has an effective model.
   */
  readonly model?: string;
  /**
   * The pending ask for a gated call, if the shim is blocking on one.
   *
   * This is what tells a `permission_denied` record whether it is the vendor
   * answering an ask we raised (already settled here) or a policy denial with
   * no open ask (the `denied.by_policy` producer).
   */
  pendingAsk(toolUseId: string): { kind: "permission" | "question" } | undefined;
  /**
   * Was this call DENIED, so that its result settles nothing?
   *
   * A DENIED TOOL NEVER STARTS. The vendor still emits a `tool_result` for it
   * -- the deny message IS the result the model sees -- but that result is the
   * denial being relayed, not the call having run, and folding it into an
   * activity would put a unit in the feed for work that never happened. The
   * transcript marks such a result `toolDenialKind`; the STREAM carries no such
   * field, so the shim's own knowledge of what it denied is the only signal
   * available on this plane.
   *
   * Answered from two places: the engine's gate, for the asks it settled as
   * denied, and the fold's own registry, for the policy and undecidable
   * denials that never had an open ask.
   */
  deniedCall(toolUseId: string): boolean;
  /** A live task, by vendor task id: the call it belongs to, and its agent when known. */
  liveTask(taskId: string): { toolUseId: string; agentId?: conversationv1.AgentId } | undefined;
  /**
   * WHICH AGENT a spawning call created, by that call's `tool_use_id`.
   *
   * ADDED BY THE RECORD PLANE (2026-08-29). The pinned SDK stream carries NO
   * agent id anywhere — a subagent's own messages name only
   * `parent_tool_use_id` — so this is the one place a subagent's book can be
   * resolved. UNSET falls back to the record plane's own minting rule
   * (`convert/ids.ts subagentId`), which is deliberately ONE function so a
   * later ruling changes one line.
   */
  subagentFor?(toolUseId: string): conversationv1.AgentId | undefined;
  /**
   * The MCP server names this session knows (`system:init.mcp_servers`,
   * `mcpServerStatus()`).
   *
   * ADDED BY THE RECORD PLANE (2026-08-29), per the shim lead's ruling: no
   * vendor field states which server served an `mcp__<server>__<tool>` call, so
   * `AgentUnmodeled.mcp_server` is resolved by an EXACT match against these
   * names and never by splitting the qualified name.
   */
  mcpServerNames?(): readonly string[];

  /**
   * REPORT A CONVERTER DEFECT the fold could not convert around.
   *
   * The fold's answer to a record it refuses is residue plus a log; that is the
   * RECORD plane's half. This is the CONTROL plane's half: the shim's own
   * diagnostics must say the converter is unhealthy while it is, and say so on
   * WatchSession rather than only in a log nobody is reading. It is deliberately
   * NOT the store writer's `store_unreachable` path — nothing about the store
   * failed — and deliberately optional, so a fold driven without an engine
   * (every unit test) needs no channel at all.
   */
  reportFault?(kind: "converter_defect", detail: string): void;

  /**
   * The last write or edit seen, and WHICH of the two it was.
   *
   * The vendor's IDE-diagnostics record carries no tool id, so the join is by
   * ADJACENCY — one remembered value, which is exactly this. The kind rides
   * along because the report lands on the remembered unit's OWN arm: a report
   * following a `Write` is `AgentWrite.diagnostics`, and one following an
   * `Edit` is `AgentEdit.diagnostics`. Nothing else can tell them apart — the
   * id alone states no kind, and a call site that guesses attributes a write's
   * findings to an edit that never happened.
   */
  readonly lastChange?: LastChange;

  /**
   * HOW the person commanded the stop the shim last issued on the main thread:
   * `KillTurnRequest.commanded_by`, exactly as the caller stated it.
   *
   * The vendor's user-stop result states only THAT the turn was stopped; who
   * decided it, and how, is known to the caller of `KillTurn` alone (an
   * interjection is decided by the daemon's prompt queue, invisibly to the
   * shim). So the engine hands the caller's statement to the fold, which
   * records it verbatim as the terminal's `by_user` cause. Unset when the
   * caller stated none, when no kill preceded this message, and on every
   * message the keep-alive produced.
   */
  readonly stopCommand?: conversationv1.AgentInterruptedByUser;
}

/** Everything one SDK message produced, as the engine consumes it. */
export interface EngineFoldOutput {
  /** Rows to record, in the order the vendor stated them. */
  readonly entries: readonly PersistEntry[];
  /**
   * Present exactly when the message was the turn's `result`.
   *
   * The terminal frame is ALREADY among `entries`; this arm exists so the
   * engine can close the turn without re-deriving which entry ended it.
   */
  readonly turnEnded?: { readonly frame: conversationv1.AgentFrame };
}

/** The fold, as the engine drives it: one call per message, in arrival order. */
export interface EngineFold {
  onSdkMessage(message: SdkMessage, context: FoldContext): EngineFoldOutput;
  /**
   * The query the fold was reading is OVER — it died, or the engine replaced
   * it. Nothing that query announced can settle any more, so whatever the fold
   * still holds for it is let go (convert/fold.ts `endQuery`).
   */
  endQuery(why: string): void;
}

/**
 * The fold before there is a fold.
 *
 * SCAFFOLD, replaced by the record-plane agent's `convert/fold.ts`. It converts
 * NOTHING — producing frames is that agent's whole brief — but it does report
 * the turn's end, because the turn's end is a CONTROL-PLANE fact the engine
 * needs to close the turn, resolve a pending model change and push context
 * usage. Wiring an engine to a fold that never ended a turn would leave every
 * session permanently mid-turn, which is a worse lie than producing no records.
 *
 * `turnEnded.frame` carries the agent id and nothing else: the terminal's own
 * taxonomy is the record plane's to map, and stating an arm here would be this
 * file inventing a conversation fact.
 */
export function turnBoundaryOnlyFold(): EngineFold {
  return {
    onSdkMessage: (message, context): EngineFoldOutput =>
      message.type === "result"
        ? {
            entries: [],
            turnEnded: {
              frame: create(conversationv1.AgentFrameSchema, { agentId: context.mainAgentId }),
            },
          }
        : { entries: [] },
    // HOLDS NOTHING, so a query's end has nothing to let go.
    endQuery: (): void => undefined,
  };
}
