/**
 * response-highlight — WORDS HIGHLIGHTED IN A FINAL RESPONSE, told to the
 * daemon (agentrepl.v1.SelectResponseText).
 *
 * While the reader's selection holds words of one final response's prose, the
 * daemon holds them, and the next prompt the reader sends revises that
 * response instead of starting a turn. The DAEMON is the holder: this module
 * only reports the selection as it settles, and never decides what a prompt
 * does with it.
 *
 * THE SELECTION IS REPORTED WHEN IT SETTLES, not on every step of a drag: a
 * pointer press holds the report until the button comes up, and a keyboard
 * extension (or a collapse) reports at once. A report naming what was already
 * reported is not sent again, and a selection that leaves every final
 * response clears the held one exactly once.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { ConnectError } from "@connectrpc/connect";
import {
  SelectResponseTextRequestSchema,
  SelectResponseTextResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_response_text_pb";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { callFailure, refusalOf } from "../rpc/refuse.js";
import { requireCase } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { selectedSpan, type SelectionSource } from "../selection.js";
import { RESPONSE_PROSE_CLASS } from "./cards/response.js";
import { feedRowId, feedRowOf } from "./row-id.js";
import { FINAL_RESPONSE_CLASS } from "./rows/turn-ended.js";

/** SelectResponseText's own refusal arm, in the reader's words. */
const SELECT_RESPONSE_TEXT_REFUSALS = {
  notAFinalResponse: () => "the highlighted words are not in a final response",
};

/** One highlight: the final response's row and the words highlighted in it. */
export interface ResponseHighlight {
  readonly row: string;
  readonly words: string;
}

/**
 * The final response whose prose holds NODE, inside HOST: its prose slot's
 * row. Null for a node outside every final response's prose.
 */
function finalProseRowOf(host: Element, node: Node): string | null {
  const element = node instanceof Element ? node : node.parentElement;
  const prose = element?.closest(`.${RESPONSE_PROSE_CLASS}`);
  if (prose === null || prose === undefined || !host.contains(prose)) return null;
  if (prose.closest(`.${FINAL_RESPONSE_CLASS}`) === null) return null;
  const row = feedRowOf(prose);
  return row === null ? null : feedRowId(row);
}

/**
 * The highlight SOURCE's selection makes inside HOST: words that start and
 * end in the prose of ONE final response. Null for no selection, a blank one,
 * or one that reaches outside a single final response's prose.
 */
export function responseHighlightIn(host: Element, source: SelectionSource): ResponseHighlight | null {
  const span = selectedSpan(source);
  if (span === null || span.text.trim() === "") return null;
  const row = finalProseRowOf(host, span.anchor);
  if (row === null || finalProseRowOf(host, span.focus) !== row) return null;
  return { row, words: span.text };
}

/** The selection state one call sends: an arm of `SelectResponseTextSelection.state`. */
type HighlightState = NonNullable<
  MessageInitShape<typeof SelectResponseTextRequestSchema>["selection"]
>["state"];

/** Tell the daemon STATE; a refusal or a transport failure reaches the chip. */
async function selectResponseText(ctx: AppContext, state: HighlightState, what: string): Promise<void> {
  let response;
  try {
    response = await callUnary(
      ctx,
      "SelectResponseText",
      (client) =>
        client.selectResponseText(create(SelectResponseTextRequestSchema, { workspace: ctx.workspace, selection: { state } })),
      SelectResponseTextResponseSchema,
    );
  } catch (err) {
    // `callUnary` already logged the transport failure; the chip is told here.
    if (!(err instanceof ConnectError)) throw err;
    ctx.failures.report(controlPlaneFailed(what, callFailure(err).text));
    return;
  }
  const result = requireCase(response.result, "SelectResponseTextResponse.result");
  switch (result.case) {
    case "success":
      log.debug(`the daemon took the response highlight (${what})`, {
        operation: "feed.response-highlight.answered",
        context: { state: state?.case ?? "unset" },
      });
      return;
    case "error": {
      const said = refusalOf(result.value.cause, SELECT_RESPONSE_TEXT_REFUSALS, "SelectResponseTextError.cause");
      log.error(`the daemon refused to ${what}: ${said.text}`, {
        operation: "feed.response-highlight.refused",
        context: { state: state?.case ?? "unset", arm: said.arm },
      });
      ctx.failures.report(controlPlaneFailed(what, said.text));
      return;
    }
  }
}

/**
 * Report the highlights the reader makes in HOST's final responses, reading
 * DOC's selection. Answers the uninstall.
 */
export function installResponseHighlight(host: HTMLElement, ctx: AppContext, doc: Document = document): () => void {
  let held: ResponseHighlight | null = null;
  let pressed = false;
  const settle = (): void => {
    const now = responseHighlightIn(host, doc);
    if (now === null) {
      if (held === null) return;
      held = null;
      log.debug("the reader's selection left the final responses; the highlight is cleared", {
        operation: "feed.response-highlight.cleared",
        context: {},
      });
      void selectResponseText(ctx, { case: "cleared", value: {} }, "clear the response highlight");
      return;
    }
    if (held !== null && held.row === now.row && held.words === now.words) return;
    held = now;
    log.debug("the reader highlighted words of a final response; the next prompt revises it", {
      operation: "feed.response-highlight.held",
      context: { row: now.row, characters: now.words.length },
    });
    void selectResponseText(
      ctx,
      { case: "highlighted", value: { row: { value: now.row }, words: now.words } },
      "hold the response highlight",
    );
  };
  const onPointerDown = (): void => {
    pressed = true;
  };
  const onPointerUp = (): void => {
    pressed = false;
    settle();
  };
  const onSelectionChange = (): void => {
    if (!pressed) settle();
  };
  doc.addEventListener("pointerdown", onPointerDown, { capture: true });
  doc.addEventListener("pointerup", onPointerUp, { capture: true });
  doc.addEventListener("selectionchange", onSelectionChange);
  return () => {
    doc.removeEventListener("pointerdown", onPointerDown, { capture: true });
    doc.removeEventListener("pointerup", onPointerUp, { capture: true });
    doc.removeEventListener("selectionchange", onSelectionChange);
  };
}
