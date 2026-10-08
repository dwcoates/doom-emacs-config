/**
 * The owner suite for the shared TypeScript log level window at
 * `agent-shim/logging/ts/level-window.ts`, which the shim and the webapp both
 * compile. Its selection is asserted against `proto/vocab/log-level-window.json`,
 * the cross-language contract Go and elisp answer too.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { LevelWindow, selectLevel, selectionNote, WINDOW_SECONDS } from "../../../logging/ts/level-window.js";

interface WindowFixture {
  window_seconds: number;
  cases: {
    name: string;
    level: string | null;
    until: string | null;
    now: number;
    outcome: string;
    effective: string | null;
    until_effective: number | null;
  }[];
}

const fixture = JSON.parse(
  readFileSync(fileURLToPath(new URL("../../../../proto/vocab/log-level-window.json", import.meta.url)), "utf8"),
) as WindowFixture;

describe("the shared log level window", () => {
  it("lasts the fixture's window", () => {
    // Arrange / Act / Assert
    expect(WINDOW_SECONDS).toBe(fixture.window_seconds);
  });

  it.each(fixture.cases.map((c) => [c.name, c] as const))("answers the fixture case %s", (_name, c) => {
    // Arrange
    const level = c.level ?? undefined;
    const until = c.until ?? undefined;

    // Act
    const select = () => selectLevel(level, until, c.now * 1000);

    // Assert
    if (c.outcome === "refused") {
      expect(select).toThrow();
      return;
    }
    const selection = select();
    expect(selection.outcome).toBe(c.outcome);
    expect(selection.level).toBe(c.effective);
    expect(selection.untilMs).toBe(c.until_effective === null ? null : c.until_effective * 1000);
  });

  it("writes no note for an honored window", () => {
    // Arrange
    const selection = selectLevel("debug", "1000300", 1_000_000_000);

    // Act / Assert
    expect(selectionNote(selection)).toBeNull();
  });

  it("writes no note for the default", () => {
    // Arrange
    const selection = selectLevel(undefined, undefined, 1_000_000_000);

    // Act / Assert
    expect(selectionNote(selection)).toBeNull();
  });

  it("reports a window's end once", () => {
    // Arrange
    const clock = { at: 1_000_000_000 };
    const window = new LevelWindow(selectLevel("debug", "1000300", clock.at), () => clock.at);
    clock.at = 1_000_400_000;
    window.allows("info");

    // Act
    const { ended } = window.allows("info");

    // Assert
    expect(ended).toBeNull();
  });

  it("never ends a fixed window", () => {
    // Arrange
    const window = LevelWindow.fixed("debug");

    // Act
    const { allowed, ended } = window.allows("debug");

    // Assert
    expect([allowed, ended]).toEqual([true, null]);
  });
});
