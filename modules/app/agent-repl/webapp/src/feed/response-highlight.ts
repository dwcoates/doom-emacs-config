/**
 * response-highlight — WORDS HIGHLIGHTED IN A FINAL RESPONSE, told to the
 * daemon (agentrepl.v1.SelectResponseText) and painted until the reader lets
 * go of them.
 *
 * While the reader's selection holds words of one final response's prose, the
 * daemon holds them, and the next prompt the reader sends revises that
 * response instead of starting a turn. The DAEMON is the holder: this module
 * only reports the selection as it settles, and never decides what a prompt
 * does with it.
 *
 * THE SELECTION IS REPORTED WHEN IT SETTLES, not on every step of a drag: a
 * pointer press holds the report until the button comes up, and a keyboard
 * extension reports at once. A report naming what was already reported is not
 * sent again.
 *
 * THE HIGHLIGHT OUTLIVES THE READER'S SELECTION (owner request, 2026-10-30:
 * clicking into Emacs's composer to write the prompt must not end it). WebKit
 * drops a page's selection when its web view loses the focus to an Emacs
 * window, so the held words are painted on their own (`HighlightPaint`, the
 * CSS Custom Highlight API), and a selection that EMPTIES with no press behind
 * it ends nothing. The highlight ends only when
 *
 *   - the reader presses the page and the press settles on no highlight (a
 *     click on the feed's background, another bubble, a new selection);
 *   - the reader presses the webhost around the page (its sidebar;
 *     webhost-pressed.ts);
 *   - a selection holding other words settles with no press (a select-all);
 *   - the prompt the reader sent takes it, which the response's revision
 *     going pending says (`responseHighlightTaken`).
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  SelectResponseTextRequestSchema,
  SelectResponseTextResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_response_text_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { controlCall } from "../rpc/control-call.js";
import { createListeners, type Listeners } from "../listeners.js";
import { perDocument } from "../per-document.js";
import { clearSelectionIn, selectedSpan, selectedText, type SelectionSource } from "../selection.js";
import { pageWebhostPresses, type WebhostPresses } from "../webhost-pressed.js";
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
  const success = await controlCall(ctx, {
    rpc: "SelectResponseText",
    what,
    send: (client) =>
      client.selectResponseText(create(SelectResponseTextRequestSchema, { workspace: ctx.workspace, selection: { state } })),
    schema: SelectResponseTextResponseSchema,
    result: (response) => response.result,
    refusals: SELECT_RESPONSE_TEXT_REFUSALS,
    refusedOperation: "feed.response-highlight.refused",
    context: { state: state?.case ?? "unset" },
  });
  if (success === undefined) return;
  log.debug(`the daemon took the response highlight (${what})`, {
    operation: "feed.response-highlight.answered",
    context: { state: state?.case ?? "unset" },
  });
}

/** The name the held words are painted under: `::highlight(response-highlight)` (styles.css). */
export const RESPONSE_HIGHLIGHT_NAME = "response-highlight";

/** Paints the held words, whatever the reader's live selection does. */
export interface HighlightPaint {
  /** Paint RANGE, replacing what was painted; null paints nothing. */
  paint(range: Range | null): void;
}

/**
 * The page's paint: the CSS Custom Highlight API, which every WebKit agent-repl
 * runs in has. A page without it cannot keep a highlight past the focus
 * leaving, so a paint there fails loudly rather than letting the purple vanish
 * with nothing said.
 */
export const cssHighlightPaint: HighlightPaint = {
  paint(range) {
    if (typeof CSS === "undefined" || CSS.highlights === undefined || typeof Highlight === "undefined") {
      log.error("this page has no CSS Custom Highlight API; a response highlight cannot be painted", {
        operation: "feed.response-highlight.unpaintable",
        context: { painting: range !== null },
      });
      throw new Error("agent-repl: the CSS Custom Highlight API is missing, so a response highlight cannot be painted");
    }
    if (range === null) {
      CSS.highlights.delete(RESPONSE_HIGHLIGHT_NAME);
      return;
    }
    CSS.highlights.set(RESPONSE_HIGHLIGHT_NAME, new Highlight(range));
  },
};

/** The rows whose highlight a sent prompt took, told per page. */
const takenOf = perDocument((): Listeners<[string]> => createListeners<[string]>());

/**
 * The prompt the reader sent took the highlight of ROW in DOC's page: the
 * daemon ended it and the response's revision is on its way, so the paint
 * goes and nothing is reported back.
 */
export function responseHighlightTaken(doc: Document, row: string): void {
  takenOf(doc).notify(row);
}

/** Tell LISTENER each row whose highlight a sent prompt took in DOC's page; answers the unsubscribe. */
export function onResponseHighlightTaken(doc: Document, listener: (row: string) => void): () => void {
  return takenOf(doc).add(listener);
}

/** What the highlight's install reads, injectable for tests. */
export interface ResponseHighlightEnv {
  /** The page whose selection is read; the document when omitted. */
  readonly doc?: Document;
  /** Where the held words are painted; `cssHighlightPaint` when omitted. */
  readonly paint?: HighlightPaint;
  /** The reader pressing the webhost around the page; the page's own signal when omitted. */
  readonly webhostPresses?: WebhostPresses;
}

/** What settled the selection: a press coming up, or a change with no press behind it. */
type Settled = "press" | "unpressed";

/**
 * Report the highlights the reader makes in HOST's final responses and paint
 * the held one until the reader lets go of it. Answers the uninstall.
 */
export function installResponseHighlight(
  host: HTMLElement,
  ctx: AppContext,
  env: ResponseHighlightEnv = {},
): () => void {
  const doc = env.doc ?? document;
  const paint = env.paint ?? cssHighlightPaint;
  let held: ResponseHighlight | null = null;
  let pressed = false;
  const end = (why: string): void => {
    if (held === null) return;
    const row = held.row;
    held = null;
    paint.paint(null);
    log.debug("the response highlight ended; the daemon is told", {
      operation: "feed.response-highlight.cleared",
      context: { row, why },
    });
    void selectResponseText(ctx, { case: "cleared", value: {} }, "clear the response highlight");
  };
  const hold = (now: ResponseHighlight): void => {
    if (held !== null && held.row === now.row && held.words === now.words) return;
    held = now;
    const selection = doc.getSelection();
    paint.paint(selection === null || selection.rangeCount === 0 ? null : selection.getRangeAt(0).cloneRange());
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
  const settle = (how: Settled): void => {
    const now = responseHighlightIn(host, doc);
    if (now !== null) {
      hold(now);
      return;
    }
    // AN EMPTIED SELECTION WITH NO PRESS BEHIND IT is the focus leaving the
    // web view (WebKit drops the page's selection when an Emacs window takes
    // the focus), never the reader letting go: the highlight stands.
    if (how === "unpressed" && selectedText(doc) === "") {
      if (held !== null) {
        log.debug("the selection emptied with no press behind it; the response highlight stands", {
          operation: "feed.response-highlight.stands",
          context: { row: held.row },
        });
      }
      return;
    }
    end(how === "press" ? "a press settled on no highlight" : "a selection elsewhere settled");
  };
  const onPointerDown = (): void => {
    pressed = true;
  };
  const onPointerUp = (): void => {
    pressed = false;
    settle("press");
  };
  const onSelectionChange = (): void => {
    if (!pressed) settle("unpressed");
  };
  const stopPresses = (env.webhostPresses ?? pageWebhostPresses(doc)).onPressed(() => {
    if (held === null) return;
    clearSelectionIn(host, doc);
    end("the reader pressed the webhost");
  });
  const stopTaken = onResponseHighlightTaken(doc, (row) => {
    if (held === null || held.row !== row) return;
    held = null;
    paint.paint(null);
    log.debug("the sent prompt took the response highlight; its paint goes", {
      operation: "feed.response-highlight.taken",
      context: { row },
    });
  });
  doc.addEventListener("pointerdown", onPointerDown, { capture: true });
  doc.addEventListener("pointerup", onPointerUp, { capture: true });
  doc.addEventListener("selectionchange", onSelectionChange);
  return () => {
    doc.removeEventListener("pointerdown", onPointerDown, { capture: true });
    doc.removeEventListener("pointerup", onPointerUp, { capture: true });
    doc.removeEventListener("selectionchange", onSelectionChange);
    stopPresses();
    stopTaken();
    if (held !== null) paint.paint(null);
    held = null;
  };
}
