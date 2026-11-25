/**
 * THE WEBHOST'S SHAPE (`src/host/host.ts`): a page is hidden through one step
 * both kinds of show share, taken out of the document through one removal,
 * and the webhost's log level is applied through `LevelWindow.apply`.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { levelApplyShapeOf } from "./level-apply-shape.js";

describe("the page a switch leaves", () => {
  it("is hidden through one step shared by both kinds of show", () => {
    // Arrange
    const source = readFileSync(path.join(import.meta.dirname, "../../src/host/host.ts"), "utf8");

    // Act
    const hand = source.match(/applyShown\(previous, false\)/g) ?? [];

    // Assert
    expect(hand).toHaveLength(1);
  });
});

describe("notes", () => {
  it("takes every page out of the document through one removal", () => {
    // A removal that skips removeFrame forgets the window, and the page's
    // last message would read as a stranger's warning. dispose alone removes
    // directly: it has already stopped listening.
    const source = readFileSync(path.join(import.meta.dirname, "../../src/host/host.ts"), "utf8");
    const direct = source.match(/\.iframe\.remove\(\)/g) ?? [];
    expect(direct.length).toBe(2);
    expect(source).toMatch(/const removeFrame = \(frame: Frame\): void => \{[^}]*frame\.iframe\.remove\(\);/s);
    expect(source).toMatch(/dispose\(\) \{[^}]*for \(const frame of frames\.values\(\)\) frame\.iframe\.remove\(\);/s);
  });
});

describe("the webhost's log level", () => {
  it("is applied through LevelWindow.apply, never opened or closed by hand", () => {
    // Act
    const shape = levelApplyShapeOf("host/host.ts");

    // Assert
    expect(shape).toEqual({ applies: true, handRolls: false });
  });
});
