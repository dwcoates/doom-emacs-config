/**
 * chess-board — a CEE CLI session's game, drawn as the CEE CLI webapp's own
 * widget (`@chesscom/cee-web-widget`) inside a purple agentic bubble.
 *
 * THIS END ONLY HOSTS THE WIDGET. The daemon resolved everything: the heading,
 * which state the board is in, the widget's data, where its bundle is served,
 * and the token a square click hands back. The card draws the progress or
 * unavailable line verbatim, or mounts the widget from the bytes it was given
 * and never decodes them.
 *
 * THE HOST'S TWO DUTIES, from the widget's host contract:
 *   - It knows which position the widget displays: the start position the
 *     daemon served until the widget reports navigation, then the last one
 *     it reported.
 *   - It answers a clicked square: the daemon asks the CEE backend, and the
 *     answer goes back whole through `showSquareEvents`. A later click or a
 *     navigation SUPERSEDES an answer still in flight, and a superseded answer
 *     is dropped, because the widget admits an answer only for the click it
 *     is still waiting on.
 *
 * A MOUNTED WIDGET SURVIVES A REDRAW OF THE SAME BOARD: when the row is drawn
 * again with the same bundle, token and data, the card hands back its previous
 * element, so the reader keeps the position they navigated to. Any other
 * change draws a fresh element, and discarding the old one unmounts its widget.
 */
import type {
  FeedChessBoard,
  FeedChessBoardReady,
} from "../../../../proto/gen/ts/frontend/v1/chess_board_pb";
import { InspectChessBoardSquareResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_inspect_chess_board_square_pb";
import { log } from "../../log.js";
import type { AppContext } from "../../rpc/context.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { callUnary } from "../../rpc/unary.js";
import type { RowContext } from "../renderers.js";
import { onDiscard } from "../ticking.js";
import {
  chessWidgetLoader,
  ensureChessWidgetStylesheet,
  type ChessWidgetHandle,
} from "./chess-widget-loader.js";
import { agenticBubble } from "./controls.js";

const PATH = "FeedChessBoard";

/** The attribute a ready board's element carries its identity in. */
export const CHESS_BOARD_KEY_ATTRIBUTE = "data-chess-board-key";

/** The line a board whose widget could not be loaded shows. */
export const WIDGET_LOAD_FAILED_TEXT = "The chess widget could not be loaded.";

/** The chess board bubble. */
export function drawFeedChessBoard(u: FeedChessBoard, rc: RowContext): HTMLElement {
  const heading = requireMessage(u.heading, `${PATH}.heading`).text;
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing a chess board", {
    operation: "feed.cards.chess-board",
    context: { state: state.case },
  });
  switch (state.case) {
    case "preparing":
      return agenticBubble({
        state: "preparing",
        heading,
        content: [line("chess-board-step", requireMessage(state.value.step, `${PATH}.state.preparing.step`).text)],
      });
    case "unavailable":
      return agenticBubble({
        state: "unavailable",
        heading,
        content: [
          line("chess-board-unavailable", requireMessage(state.value.reason, `${PATH}.state.unavailable.reason`).text),
        ],
      });
    case "ready":
      return drawReadyBoard(state.value, heading, rc);
    default:
      return unreachableArm(`${PATH}.state`, (state as { case: string }).case);
  }
}

/** A ready board: its previous element when it is the same board, else a fresh mount. */
function drawReadyBoard(u: FeedChessBoardReady, heading: string, rc: RowContext): HTMLElement {
  const widget = requireMessage(u.widget, `${PATH}.state.ready.widget`).ceeWebWidget;
  const bundle = requireMessage(u.bundle, `${PATH}.state.ready.bundle`);
  const token = requireMessage(u.squareToken, `${PATH}.state.ready.square_token`);
  const start = requireMessage(u.startPosition, `${PATH}.state.ready.start_position`).gamePoint;
  const key = boardKey(bundle.scriptUrl, token.value, widget);
  if (rc.previous?.getAttribute(CHESS_BOARD_KEY_ATTRIBUTE) === key) {
    log.debug("kept a mounted chess board across a redraw of the same board", {
      operation: "feed.cards.chess-board.kept",
      context: { script: bundle.scriptUrl },
    });
    return rc.previous;
  }

  const host = document.createElement("div");
  host.className = "chess-board-widget";
  const status = line("chess-board-status", "");
  status.hidden = true;
  const bubble = agenticBubble({ state: "ready", heading, content: [host, status], uncapped: true });
  bubble.setAttribute(CHESS_BOARD_KEY_ATTRIBUTE, key);
  mountWidget({
    ctx: rc.ctx,
    host,
    status,
    scriptUrl: bundle.scriptUrl,
    stylesheetUrl: bundle.stylesheetUrl,
    widget,
    token: token.value,
    start,
  });
  return bubble;
}

/** What one mount needs. */
interface MountSpec {
  ctx: AppContext;
  host: HTMLElement;
  status: HTMLElement;
  scriptUrl: string;
  stylesheetUrl: string;
  widget: Uint8Array;
  token: string;
  start: bigint;
}

/**
 * Mount the widget into HOST once its bundle is loaded, and tie its life to
 * the host: discarding the host unmounts it, and a host discarded before the
 * bundle arrived never mounts at all.
 */
function mountWidget(spec: MountSpec): void {
  let discarded = false;
  let handle: ChessWidgetHandle | undefined;
  onDiscard(spec.host, () => {
    discarded = true;
    handle?.unmount();
  });

  // THE DISPLAYED POSITION and THE NEWEST REQUEST. Every click takes the next
  // request number and so does every navigation, so an answer arriving after
  // either is recognized as superseded by its number alone.
  let displayed = spec.start;
  let newest = 0;

  ensureChessWidgetStylesheet(spec.host.ownerDocument, spec.stylesheetUrl);
  chessWidgetLoader.load(spec.scriptUrl).then(
    (module) => {
      if (discarded) return;
      handle = module.mountCeeWebWidget(spec.host, {
        widgetBytes: spec.widget,
        onPositionChange: (gamePoint) => {
          displayed = BigInt(gamePoint);
          newest += 1;
        },
        onSquareSelect: (square) => {
          newest += 1;
          const mine = newest;
          void inspectSquare(spec, displayed, square).then((answer) => {
            if (mine !== newest || discarded || handle === undefined) {
              log.debug("dropped a superseded square answer", {
                operation: "feed.cards.chess-board.square-superseded",
                context: { square },
              });
              return;
            }
            if (answer.kind === "answer") {
              spec.status.hidden = true;
              handle.showSquareEvents(answer.bytes);
            } else {
              showStatus(spec.status, answer.text);
            }
          });
        },
      });
      log.info("mounted a chess board", {
        operation: "feed.cards.chess-board.mounted",
        context: { script: spec.scriptUrl, widget_bytes: spec.widget.length },
      });
    },
    (err: unknown) => {
      showStatus(spec.status, WIDGET_LOAD_FAILED_TEXT);
      log.error(`the chess widget bundle could not be loaded: ${String(err)}`, {
        operation: "feed.cards.chess-board.load-failed",
        context: { script: spec.scriptUrl, cause: err },
      });
    },
  );
}

/** A square click's outcome: the widget's answer, or a line saying why none came. */
type SquareOutcome = { kind: "answer"; bytes: Uint8Array } | { kind: "refused"; text: string };

/** The lines a square click with no answer shows, by error arm. */
export const SQUARE_REFUSALS = {
  backendUnreachable: "The chess widget's backend did not answer that click.",
  sessionGone: "This board's CEE session no longer holds its game.",
  transport: "The click could not reach the daemon.",
} as const;

/** Ask the daemon what the engine says about SQUARE at DISPLAYED. */
async function inspectSquare(spec: MountSpec, displayed: bigint, square: number): Promise<SquareOutcome> {
  try {
    const response = await callUnary(
      spec.ctx,
      "InspectChessBoardSquare",
      (client) =>
        client.inspectChessBoardSquare({
          board: { value: spec.token },
          gamePoint: displayed,
          square,
        }),
      InspectChessBoardSquareResponseSchema,
    );
    const result = requireCase(response.result, "InspectChessBoardSquareResponse.result");
    if (result.case === "success") {
      return { kind: "answer", bytes: result.value.getSquareEventsResponse };
    }
    const cause = requireCase(result.value.cause, "InspectChessBoardSquareError.cause");
    log.info(`a square click got no answer: ${cause.case}`, {
      operation: "feed.cards.chess-board.square-refused",
      context: { square, arm: cause.case, detail: cause.value.detail },
    });
    switch (cause.case) {
      case "backendUnreachable":
      case "sessionGone":
        return { kind: "refused", text: SQUARE_REFUSALS[cause.case] };
      default:
        return unreachableArm("InspectChessBoardSquareError.cause", (cause as { case: string }).case);
    }
  } catch (err) {
    log.error(`InspectChessBoardSquare failed: ${String(err)}`, {
      operation: "feed.cards.chess-board.square-failed",
      context: { square, cause: err },
    });
    return { kind: "refused", text: SQUARE_REFUSALS.transport };
  }
}

/** Show TEXT on the board's status line. */
function showStatus(status: HTMLElement, text: string): void {
  status.textContent = text;
  status.hidden = false;
}

/** A plain line of daemon-composed text. */
function line(className: string, text: string): HTMLElement {
  const el = document.createElement("div");
  el.className = className;
  el.textContent = text;
  return el;
}

/**
 * A ready board's identity: the bundle it loads, the session it answers
 * clicks for, and its data. Two draws with one key mount the same widget.
 */
export function boardKey(scriptUrl: string, token: string, widget: Uint8Array): string {
  // FNV-1a over the bytes: an identity check, not a security boundary.
  let hash = 0x811c9dc5;
  for (const byte of widget) {
    hash ^= byte;
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return `${scriptUrl}\n${token}\n${widget.length}:${hash.toString(16)}`;
}
