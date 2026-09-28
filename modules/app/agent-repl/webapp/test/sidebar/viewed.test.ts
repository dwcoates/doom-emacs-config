// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { viewedMode } from "../../src/sidebar/viewed.js";

describe("the display mode a row draws in", () => {
  it.each([
    { name: "is FULL for a row the daemon carries no marker for", arm: "ready", viewed: false, want: "full" },
    { name: "is PARTIAL for a row the daemon carries the marker for", arm: "done", viewed: true, want: "partial" },
    { name: "is PARTIAL for an interrupted row the daemon carries the marker for", arm: "interrupted", viewed: true, want: "partial" },
  ])("$name", ({ arm, viewed, want }) => {
    // Arrange, Act.
    const mode = viewedMode("ws-1", arm, viewed);

    // Assert: presence is the mode.
    expect(mode).toBe(want);
  });

  it("is PARTIAL on the push that returns a read row from idle_async", () => {
    // Arrange: the row was drawn FULL at idle_async.
    viewedMode("ws-1", "idle_async", false);

    // Act: the detached work ended; the daemon says the result is read.
    const mode = viewedMode("ws-1", "done", true);

    // Assert: a status change does not override the daemon's marker, or the
    // row would claim an unread result the user has already read.
    expect(mode).toBe("partial");
  });
});
