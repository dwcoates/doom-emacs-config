/**
 * fake/scenarios/support.ts — the shared spine every scenario family stands on.
 *
 * Two things live here and nothing else: the `scenario()` constructor (so a
 * scenario is declared as data plus one `run`, and the table's four metadata
 * columns are impossible to forget) and the few emission shapes that repeat
 * across families — the closing prose that names the turn's conclusion, the
 * signed-thinking prelude, and the permission round-trip.
 *
 * Anything used by exactly one family stays in that family's file. A "helpers"
 * module that accumulated one-off emitters would become the place shapes drift
 * away from the corpus.
 */
import type { PermissionResultLike, PermissionUpdateLike } from "../../sdk/types.js";
import type { Scenario, ScenarioContext, ToolCall } from "../scenario.js";
import { FAKE_REASONING_SIGNATURE } from "../vendor-files.js";

/**
 * A thinking signature shaped like the corpus's (opaque, truncated there too).
 *
 * Re-exported from the mock's own module: the emitters there need it for the
 * reasoning prelude every tool call and turn conclusion carries, so one
 * constant serves both and the two cannot drift.
 */
export const FAKE_SIGNATURE = FAKE_REASONING_SIGNATURE;

/** Declare one scenario. The metadata is required, so the table cannot rot. */
export function scenario(spec: Scenario): Scenario {
  return spec;
}

/**
 * Close a turn with one prose block and the matching success result.
 *
 * The result's `result` field and the final text block carry the SAME string on
 * purpose: the settled answer of a turn is named by the turn's conclusion, and
 * a mock whose two spellings disagreed would let a consumer pick either and
 * still pass.
 */
export function conclude(ctx: ScenarioContext, conclusion: string): void {
  // THE OBSERVED SHAPE OF A TURN'S CLOSING API RESPONSE: `[thinking, text]` on
  // one message id, one assistant line per block (every capture; the smallest
  // is `bash-foreground-completed`). The vendor reasons before it answers, so a
  // conclusion emitted as a bare text block was a shape no capture shows.
  ctx.assistant([withheldThinking(), { type: "text", text: conclusion }], {
    stopReason: "end_turn",
  });
  ctx.result({ subtype: "success", result: conclusion });
}

/**
 * A signed thinking block whose reasoning is WITHHELD.
 *
 * On the wire that is a `thinking` block with an empty `thinking` string and a
 * present `signature` — exactly what `content-blocks/thinking.jsonl` carries.
 * The signature is what tells a consumer the reasoning existed and was not
 * surfaced, so a block without one would be a different fact entirely.
 */
export function withheldThinking(): { type: "thinking"; thinking: string; signature: string } {
  return { type: "thinking", thinking: "", signature: FAKE_SIGNATURE };
}

/** A signed thinking block whose reasoning IS surfaced. */
export function visibleThinking(text: string): {
  type: "thinking";
  thinking: string;
  signature: string;
} {
  return { type: "thinking", thinking: text, signature: FAKE_SIGNATURE };
}

/**
 * Ask the shim's gate about one tool call.
 *
 * `suggestions` is populated for every ask because the vendor offers them
 * whenever a standing rule could be written, and the gate's standing-allow path
 * echoes them back as `updatedPermissions` — a round trip that cannot be
 * exercised at all if the ask carries none.
 */
export async function askPermission(
  ctx: ScenarioContext,
  call: ToolCall,
  extra: {
    title?: string;
    displayName?: string;
    description?: string;
    decisionReason?: string;
    /**
     * The standing the ask OFFERS, when the default single add-rule suggestion
     * is not the shape under test (a `setMode` offer, for instance).
     */
    suggestions?: readonly PermissionUpdateLike[];
  } = {},
): Promise<PermissionResultLike | null> {
  return ctx.canUseTool(call.name, call.input, {
    signal: new AbortController().signal,
    toolUseID: call.toolUseId,
    requestId: `req_perm_${call.toolUseId}`,
    suggestions: [
      ...(extra.suggestions ?? [
        {
          type: "addRules",
          rules: [{ toolName: call.name, ruleContent: String(call.input.command ?? call.input.file_path ?? "") }],
          behavior: "allow",
          destination: "localSettings",
        },
      ]),
    ],
    title: extra.title ?? `Claude wants to run ${call.name}`,
    displayName: extra.displayName ?? call.name,
    description: extra.description ?? `Claude will use ${call.name}`,
    ...(extra.decisionReason === undefined ? {} : { decisionReason: extra.decisionReason }),
  });
}

/** The `toolUseResult` shape a Bash call answers with (corpus: tool-results/bash). */
export function bashResult(fields: {
  stdout: string;
  stderr?: string;
  interrupted?: boolean;
  isImage?: boolean;
  extra?: Record<string, unknown>;
}): Record<string, unknown> {
  return {
    stdout: fields.stdout,
    stderr: fields.stderr ?? "",
    interrupted: fields.interrupted ?? false,
    isImage: fields.isImage ?? false,
    noOutputExpected: false,
    ...(fields.extra ?? {}),
  };
}
