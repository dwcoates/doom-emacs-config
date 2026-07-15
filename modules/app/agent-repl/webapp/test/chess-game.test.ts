import { describe, expect, it } from "vitest";
import {
  CHESS_GAME_MARKER,
  chessGameContainerHtml,
  chessGameMarkerPath,
  isPartialChessGameMarker,
} from "../src/chess-game.js";

describe("chessGameMarkerPath", () => {
  it("returns the path between prefix and closing arrow", () => {
    // Arrange
    const line = `${CHESS_GAME_MARKER}/ws/.claude/emacs/cee-web-widget/chess-game-ab12.pgn <---`;
    // Act + Assert
    expect(chessGameMarkerPath(line)).toBe("/ws/.claude/emacs/cee-web-widget/chess-game-ab12.pgn");
  });

  it("accepts a marker without the closing arrow", () => {
    // Arrange + Act + Assert
    expect(chessGameMarkerPath(`${CHESS_GAME_MARKER}/tmp/chess-game-x.fen`)).toBe("/tmp/chess-game-x.fen");
  });

  it("trims whitespace around the path", () => {
    // Arrange + Act + Assert
    expect(chessGameMarkerPath(`${CHESS_GAME_MARKER}  /tmp/chess-game-x.pgn  <---`)).toBe("/tmp/chess-game-x.pgn");
  });
});

describe("isPartialChessGameMarker", () => {
  it("holds for a strict prefix of the marker", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker("---> agent-repl-che")).toBe(true);
  });

  it("rejects a complete marker line", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker(`${CHESS_GAME_MARKER}/tmp/f.pgn`)).toBe(false);
  });

  it("rejects unrelated text", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker("hello world")).toBe(false);
  });

  it("rejects the empty line", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker("")).toBe(false);
  });
});

describe("chessGameContainerHtml", () => {
  it("carries the path in a data attribute with the processing indicator", () => {
    // Arrange + Act
    const html = chessGameContainerHtml("/tmp/chess-game-x.pgn");
    // Assert
    expect(html).toContain(`data-game-file="/tmp/chess-game-x.pgn"`);
    expect(html).toContain(`class="thinking-spinner"`);
    expect(html).toContain("processing");
  });

  it("escapes markup in the path", () => {
    // Arrange + Act + Assert
    expect(chessGameContainerHtml(`/tmp/<img>"x.pgn`)).not.toContain("<img>");
  });
});
