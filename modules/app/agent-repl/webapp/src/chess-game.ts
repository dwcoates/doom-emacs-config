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
