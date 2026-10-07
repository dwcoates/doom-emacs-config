/**
 * chess-widget-loader — fetching the CEE CLI webapp's widget bundle into the
 * page: its ES module by URL, and its stylesheet once per URL.
 *
 * The card loads through {@link chessWidgetLoader}, an object rather than a
 * bare import, so a suite can stand a fake widget in for the bundle with
 * `vi.spyOn` (the real one is built in another repository and served by the
 * daemon) and restore it after: the unit suite shares one module graph per
 * worker, so replacing the module itself would leak into other files.
 *
 * THE BUNDLE IS SAME-ORIGIN: the daemon serves both files under its own
 * `/chess-widget/<stamp>/` route, so the import needs no CORS and a rebuilt
 * widget is a new URL, never a stale cached module.
 */
import { log } from "../../log.js";

/** The widget's handle, as the card uses it (the widget's `CeeWebWidgetHandle`). */
export interface ChessWidgetHandle {
  /** Show the answer to the square the widget last reported. */
  showSquareEvents(responseBytes: Uint8Array): void;
  /** Tear the widget down and release its container. */
  unmount(): void;
}

/** The widget's mount options, as the card supplies them. */
export interface ChessWidgetMountOptions {
  /** A `chesscom.cee_webapp.v1.CeeWebWidget` in binary form. */
  widgetBytes: Uint8Array;
  /** Navigation inside the widget, with the gamepoint now displayed. */
  onPositionChange: (gamePoint: number) => void;
  /** A clicked piece's square, as CEE's `chesscom.chess.v1.Square` number. */
  onSquareSelect: (square: number) => void;
}

/** The bundle's module, as the card uses it. */
export interface ChessWidgetModule {
  mountCeeWebWidget(element: HTMLElement, options: ChessWidgetMountOptions): ChessWidgetHandle;
}

/** Import the widget's module from URL, refusing one that exports no mount. */
export async function loadChessWidget(url: string): Promise<ChessWidgetModule> {
  const loaded = (await import(/* @vite-ignore */ url)) as Partial<ChessWidgetModule>;
  if (typeof loaded.mountCeeWebWidget !== "function") {
    throw new Error(`the chess widget bundle at ${url} exports no mountCeeWebWidget`);
  }
  return loaded as ChessWidgetModule;
}

/** Load the widget's stylesheet into the page, once per URL. */
export function ensureChessWidgetStylesheet(doc: Document, url: string): void {
  for (const link of doc.head.querySelectorAll<HTMLLinkElement>("link[data-chess-widget]")) {
    if (link.getAttribute("href") === url) return;
  }
  const link = doc.createElement("link");
  link.rel = "stylesheet";
  link.href = url;
  link.setAttribute("data-chess-widget", "");
  doc.head.append(link);
  log.debug("loaded the chess widget's stylesheet", {
    operation: "feed.cards.chess-board.stylesheet",
    context: { url },
  });
}

/** The card's ONE way to load the widget; a suite spies on `load`. */
export const chessWidgetLoader = {
  load: loadChessWidget,
};
