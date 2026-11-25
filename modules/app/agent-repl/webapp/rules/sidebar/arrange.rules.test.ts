/**
 * THE SIDEBAR'S ARRANGEMENT SOURCES, held to their one way of doing things:
 * one awaiter of the arrangement hold, one way a held verb fires, one reader
 * of a held verb's revision.
 */
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../test/source-text.js";

describe("sending a held verb", () => {
  it("is the only place the sidebar awaits its arrangement hold", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar");
    const awaiting = readdirSync(dir)
      .filter((file) => file.endsWith(".ts"))
      .flatMap((file) => (readFileSync(path.join(dir, file), "utf8").match(/hold\.await\(\)/g) ?? []).map(() => file));
    expect(awaiting).toEqual(["arrange.ts"]);
  });
});

describe("sending held steps", () => {
  it("is what sendHeld asks through, so a held verb has one way to fire", () => {
    // Arrange
    const source = codeOf(readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar/arrange.ts"), "utf8"));

    // Act
    const fired = source.match(/fireVerb\(/g) ?? [];

    // Assert
    expect([fired.length, /return sendHeldSteps\(sc, control, \[heldStep\(verb\)\]\)/.test(source)]).toEqual([1, true]);
  });
});

describe("successRevision", () => {
  it.each(["arrange.ts", "rename.ts", "nest.ts"])("is how %s reads its held verb's revision", (file) => {
    // Arrange
    const source = codeOf(readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "src", "sidebar", file), "utf8"));

    // Act
    const reads = [source.includes("revision: successRevision"), source.includes(".rosterRevision : null")];

    // Assert
    expect(reads).toEqual([true, file === "arrange.ts"]);
  });
});
