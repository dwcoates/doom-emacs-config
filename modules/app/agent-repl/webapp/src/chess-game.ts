/**
 * Chess-game marker channel (§design-chessboard-widget.md): the agent's
 * /show-chess-game skill writes a game payload (PGN/FEN/session pointer)
 * to a file under the worktree's .claude/emacs/cee-web-widget/ and emits
 * a single marker line in its response:
 *
 *   ---> agent-repl-chess-game-file: /abs/path/chess-game-<hash>.pgn <---
 *
 * The markdown pipeline suppresses that line and renders a chess-game
 * container div in its place (chessGameContainerHtml); text before and
 * after the marker keeps flowing around it in the same bubble. Marker
 * detection is a plain line-start string comparison — never a regex.
 *
 * This module carries the pure marker/container helpers; the widget
 * mount lifecycle lives alongside so the container's hydration and the
 * marker syntax cannot drift apart.
 */

import { escapeHtml } from "./highlight.js";

/** Line prefix the skill emits; everything after it is the file path. */
export const CHESS_GAME_MARKER = "---> agent-repl-chess-game-file: ";

/** Decorative marker suffix, stripped when present. */
const MARKER_SUFFIX = "<---";

/** The payload path carried by a marker line (prefix-matched by caller). */
export function chessGameMarkerPath(line: string): string {
  let rest = line.slice(CHESS_GAME_MARKER.length).trim();
  if (rest.endsWith(MARKER_SUFFIX)) {
    rest = rest.slice(0, -MARKER_SUFFIX.length).trimEnd();
  }
  return rest;
}

/**
 * True when a streaming buffer's LAST line could still become a marker
 * line (a strict prefix of the marker constant, e.g. "---> agent-").
 * The renderer then withholds the line for one delta instead of flashing
 * marker fragments as paragraph text.
 */
export function isPartialChessGameMarker(line: string): boolean {
  return line.length > 0 && line.length < CHESS_GAME_MARKER.length && CHESS_GAME_MARKER.startsWith(line);
}

/**
 * The container a marker line renders as. It shows the processing
 * indicator (the thinking indicator's spinner, relabeled) until the
 * hydration pass mounts the widget into it; the payload path rides in a
 * data attribute for that pass.
 */
export function chessGameContainerHtml(path: string): string {
  return (
    `<div class="chess-game" data-game-file="${escapeHtml(path)}">` +
    `<div class="thinking-pending"><span class="thinking-spinner" aria-hidden="true"></span> processing</div>` +
    `</div>`
  );
}

/** One piece of a bubble body: literal text, or a game payload path. */
export type ChessGameSegment = { text: string } | { path: string };

/**
 * Splits response text on marker lines, so the bubble renderer can lay
 * out each text segment through its own pipeline (markdown OR the
 * metaprompt-tree renderer) with widget containers between them. The
 * marker must be handled ABOVE that pipeline choice: a marker inside a
 * TLDR-tree response would otherwise be rendered literally by the tree
 * renderer, which never sees markdown handling.
 */
export function splitChessGameSegments(src: string, streamTail: boolean): ChessGameSegment[] {
  const lines = src.split("\n");
  if (streamTail && isPartialChessGameMarker(lines[lines.length - 1])) {
    lines.pop();
  }
  const segments: ChessGameSegment[] = [];
  let buf: string[] = [];
  // A marker inside a fenced code block is literal text (someone quoting
  // the marker syntax), so fence interiors are never intercepted.
  let inFence = false;
  const flush = (): void => {
    if (buf.length > 0) {
      segments.push({ text: buf.join("\n") });
      buf = [];
    }
  };
  for (const line of lines) {
    if (line.startsWith("```")) {
      inFence = !inFence;
      buf.push(line);
    } else if (!inFence && line.startsWith(CHESS_GAME_MARKER)) {
      flush();
      segments.push({ path: chessGameMarkerPath(line) });
    } else {
      buf.push(line);
    }
  }
  flush();
  return segments;
}

/* ------------------------------------------------------------------ *
 * Widget host: fetches the payload through the daemon's validated
 * /sessions/{id}/chess-game route and mounts the embeddable widget
 * (served in place at /widget-assets/) into the container. The widget's
 * protocol is chess-generic (see its chess-widget.d.ts): mount(el, opts)
 * -> { unmount, serializeState }.
 * ------------------------------------------------------------------ */

/** The widget's host protocol, mirrored structurally (engine-free). */
export interface ChessWidgetHandle {
  unmount(): void;
  serializeState(): unknown;
  /** Programmatic move stepping (optional protocol extension): absent on
   * widget builds that predate it, present once the widget ships it. */
  step?(direction: ChessStepDirection): void;
}

export type ChessStepDirection = "back" | "forward";
export interface ChessWidgetMountOpts {
  source: string;
  kind: "pgn" | "fen" | "session";
  backend?: { url: string };
  state?: unknown;
  onStateChange?: (state: unknown) => void;
}
export type ChessWidgetMount = (container: unknown, opts: ChessWidgetMountOpts) => ChessWidgetHandle;

/** What the hydrator touches on a container element (structural, so
 * tests drive it with plain objects; a real HTMLElement satisfies it). */
export interface GameContainer {
  isConnected: boolean;
  innerHTML: string;
  dataset: { gameFile?: string; chessHydrated?: string };
  closest(selectors: string): { getAttribute(name: string): string | null } | null;
  addEventListener(type: string, listener: () => void): void;
}

/** What the hydrator needs from a bubble root (HTMLElement satisfies). */
export interface ChessGameRoot {
  querySelectorAll(selectors: string): ArrayLike<unknown>;
}

const CHESS_GAME_SELECTOR = ".chess-game[data-game-file]";
const WIDGET_BUNDLE_PATH = "/widget-assets/chess-widget.js";

export interface ChessGameConfig {
  /** Daemon HTTP base, e.g. http://127.0.0.1:8787 */
  base: string;
  /** Live session id (a getter: rebind swaps the id under the view). */
  session: () => string;
  /** Test seam: replaces the dynamic bundle import. */
  loadMount?: () => Promise<ChessWidgetMount>;
  /** Test seam: replaces fetch-as-text (throws on any non-OK). */
  fetchText?: (url: string) => Promise<string>;
}

let config: ChessGameConfig | null = null;
let mountPromise: Promise<ChessWidgetMount> | null = null;
/** Serialized per-widget view state, surviving innerHTML rewrites. */
const gameStates = new Map<string, unknown>();
/** Live mount handles, keyed by container identity. */
const gameHandles = new WeakMap<object, ChessWidgetHandle>();
/** Live mounts by widget state key — a plain Map (not the WeakMap above)
 * because the out-of-band nav hook must resolve the ACTIVE board even
 * after its container was swapped by an innerHTML rewrite. */
const liveGames = new Map<string, { el: GameContainer; handle: ChessWidgetHandle }>();
/** State key of the board the user last clicked into (nav hook target). */
let activeGameKey: string | null = null;

/** Installs the host config (main.ts boot). Resets caches, so tests can
 * reconfigure freely. */
export function configureChessGames(cfg: ChessGameConfig | null): void {
  config = cfg;
  mountPromise = null;
  gameStates.clear();
  liveGames.clear();
  activeGameKey = null;
}

/** Game kind by payload file extension; null for an unsupported one. */
export function chessGameKind(path: string): ChessWidgetMountOpts["kind"] | null {
  if (path.endsWith(".pgn")) return "pgn";
  if (path.endsWith(".fen")) return "fen";
  if (path.endsWith(".session")) return "session";
  return null;
}

/**
 * A .session payload names a live engine-daemon session: line 1 the
 * daemon's HTTP base URL, line 2 the session id.
 */
export function parseSessionPointer(text: string): { backend: string; session: string } | null {
  const lines = text.split("\n").map((l) => l.trim());
  if (!lines[0]?.startsWith("http") || !lines[1]) return null;
  return { backend: lines[0], session: lines[1] };
}

/** The daemon URL serving a marker-carried payload path. */
export function chessGameFileUrl(base: string, sessionId: string, path: string): string {
  return `${base}/sessions/${sessionId}/chess-game?path=${encodeURIComponent(path)}`;
}

function errorHtml(msg: string): string {
  return `<div class="chess-game-error">chess game: ${escapeHtml(msg)}</div>`;
}

async function fetchTextDefault(url: string): Promise<string> {
  const resp = await fetch(url);
  if (!resp.ok) {
    let msg = `daemon answered ${resp.status}`;
    try {
      const body = (await resp.json()) as { error?: string };
      if (body.error) msg = body.error;
    } catch {
      // Non-JSON error body: the status line above already carries it.
    }
    throw new Error(msg);
  }
  return resp.text();
}

function loadMount(cfg: ChessGameConfig): Promise<ChessWidgetMount> {
  if (mountPromise === null) {
    mountPromise = (async () => {
      if (cfg.loadMount) return cfg.loadMount();
      const mod = (await import(/* @vite-ignore */ `${cfg.base}${WIDGET_BUNDLE_PATH}`)) as {
        default?: ChessWidgetMount;
      };
      if (typeof mod.default !== "function") {
        throw new Error("widget bundle carries no mount export");
      }
      return mod.default;
    })();
    // A failed load must not poison the capability forever: the daemon
    // may gain the -widget-assets mount on its next restart.
    mountPromise.catch(() => {
      mountPromise = null;
    });
  }
  return mountPromise;
}

/** Builds the widget mount options for one payload (fetch + classify). */
export async function chessGameMountOpts(
  path: string,
  cfg: ChessGameConfig,
  savedState: unknown,
): Promise<ChessWidgetMountOpts> {
  const kind = chessGameKind(path);
  if (kind === null) {
    throw new Error(`unsupported game file type: ${path}`);
  }
  const fetchText = cfg.fetchText ?? fetchTextDefault;
  const text = await fetchText(chessGameFileUrl(cfg.base, cfg.session(), path));
  if (kind === "session") {
    const ptr = parseSessionPointer(text);
    if (ptr === null) {
      throw new Error("malformed session pointer file (want: backend URL line, session id line)");
    }
    return { source: ptr.session, kind, backend: { url: ptr.backend }, state: savedState };
  }
  return { source: text, kind, state: savedState };
}

/**
 * Mounts widgets into every un-hydrated chess-game container under
 * `root`. Fire-and-forget per container: failures render as readable
 * errors inside the container frame, never thrown past the feed.
 */
export function hydrateChessGames(root: ChessGameRoot): void {
  const containers = Array.from(root.querySelectorAll(CHESS_GAME_SELECTOR)) as GameContainer[];
  containers.forEach((el, idx) => {
    if (el.dataset.chessHydrated) return;
    el.dataset.chessHydrated = "1";
    const path = el.dataset.gameFile;
    if (!path) return;
    if (config === null) {
      throw new Error("chess-game hydrator is not configured");
    }
    const cfg = config;
    const feedKey = el.closest(".feed-item")?.getAttribute("data-key") ?? "";
    const stateKey = `${feedKey}::${path}::${idx}`;
    // Clicking into a board makes it the out-of-band nav hook's target;
    // registered before the async mount so a click during loading counts.
    el.addEventListener("click", () => {
      activeGameKey = stateKey;
    });
    void (async () => {
      try {
        const [mount, opts] = await Promise.all([
          loadMount(cfg),
          chessGameMountOpts(path, cfg, gameStates.get(stateKey)),
        ]);
        // The container may have been replaced by a newer re-render while
        // the payload loaded; mounting into a detached node would leak.
        if (!el.isConnected) return;
        opts.onStateChange = (s) => gameStates.set(stateKey, s);
        const handle = mount(el, opts);
        gameHandles.set(el, handle);
        liveGames.set(stateKey, { el, handle });
      } catch (err) {
        if (el.isConnected) {
          el.innerHTML = errorHtml(err instanceof Error ? err.message : String(err));
        }
      }
    })();
  });
}

/**
 * Unmounts every live widget under `root`. Called before a bubble's
 * innerHTML rewrite (and before node removal) so widget resources are
 * released ahead of their containers being dropped.
 */
export function releaseChessGames(root: ChessGameRoot): void {
  const containers = Array.from(root.querySelectorAll(CHESS_GAME_SELECTOR)) as GameContainer[];
  for (const el of containers) {
    const handle = gameHandles.get(el);
    if (handle) {
      handle.unmount();
      gameHandles.delete(el);
      for (const [key, entry] of liveGames) {
        if (entry.el === el) liveGames.delete(key);
      }
    }
  }
}

/* ------------------------------------------------------------------ *
 * Out-of-band navigation: the Emacs host cannot deliver keyboard
 * events into the page (NS xwidget limitation), so its webview-buffer
 * keys drive the active board over the execute-script channel through
 * the window hook installed below.
 * ------------------------------------------------------------------ */

/** Name of the window global the Emacs host invokes (one contract with
 * `agent-repl-frontend-chess-step-hook` in frontend.el — MUST match). */
export const CHESS_NAV_HOOK = "agentReplChessStep";

/**
 * Steps the most recently clicked board. False when no board was
 * clicked yet, when the active board is gone, or when the mounted
 * widget build predates the protocol's optional step extension.
 */
export function stepActiveChessGame(direction: ChessStepDirection): boolean {
  if (activeGameKey === null) return false;
  const entry = liveGames.get(activeGameKey);
  if (!entry || entry.handle.step === undefined) return false;
  entry.handle.step(direction);
  return true;
}

/** Plants the nav hook on the host global (main.ts boot). */
export function installChessNavHook(target: Record<string, unknown>): void {
  target[CHESS_NAV_HOOK] = (direction: ChessStepDirection) => stepActiveChessGame(direction);
}
