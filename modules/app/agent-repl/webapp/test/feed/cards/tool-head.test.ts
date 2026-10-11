// @vitest-environment jsdom
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import {
  TOOL_HEAD_CLASS,
  TOOL_NAME_CLASS,
  drawTitledToolHead,
  drawToolHead,
  drawToolName,
} from "../../../src/feed/cards/tool-head.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src");

/** Every .ts file under DIR. */
function sources(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = path.join(dir, name);
    if (statSync(full).isDirectory()) return sources(full);
    return full.endsWith(".ts") ? [full] : [];
  });
}

describe("drawToolHead", () => {
  it("draws a div wearing the shared class first", () => {
    const head = drawToolHead();
    expect([head.localName, head.className]).toEqual(["div", TOOL_HEAD_CLASS]);
  });

  it("adds the card's own classes beside the shared one", () => {
    expect(drawToolHead("perm-head").className).toBe(`${TOOL_HEAD_CLASS} perm-head`);
  });
});

describe("drawToolName", () => {
  it("draws the name verbatim in a span wearing the shared class", () => {
    const name = drawToolName("  Bash  ");
    expect([name.localName, name.className, name.textContent]).toEqual(["span", TOOL_NAME_CLASS, "  Bash  "]);
  });

  it("adds the card's own classes beside the shared one", () => {
    expect(drawToolName("Claude wants to run ls", "perm-headline").className).toBe(
      `${TOOL_NAME_CLASS} perm-headline`,
    );
  });
});

describe("drawTitledToolHead", () => {
  it("draws the shared head holding only the shared name, verbatim", () => {
    // Arrange / Act
    const head = drawTitledToolHead("Skill");

    // Assert
    expect([head.className, head.children.length]).toEqual([TOOL_HEAD_CLASS, 1]);
    const name = head.firstElementChild as HTMLElement;
    expect([name.className, name.textContent]).toEqual([TOOL_NAME_CLASS, "Skill"]);
  });
});

describe("every client-titled tool head is built by the one builder", () => {
  it("leaves no module naming a head with a client constant by hand", () => {
    // A hand-rolled head-plus-title drifts from the shared one; this fails it by name.
    const own = path.join(SRC, "feed/cards/tool-head.ts");
    const offenders = sources(SRC)
      .filter((file) => file !== own)
      .filter((file) => /drawToolName\([A-Z][A-Z_]*\)/.test(readFileSync(file, "utf8")));
    expect(offenders.map((file) => path.relative(SRC, file))).toEqual([]);
  });

  it.each([
    { file: "feed/asks/question.ts", call: "drawTitledToolHead(QUESTION_TITLE)" },
    { file: "feed/cards/skill.ts", call: "drawTitledToolHead(SKILL_TITLE)" },
  ])("heads $file through the shared builder", ({ file, call }) => {
    expect(readFileSync(path.join(SRC, file), "utf8")).toContain(call);
  });
});

describe("every tool card head is built by the one builder", () => {
  it("leaves no module building a tool head or tool name by hand", () => {
    // A hand-rolled head drifts from the shared one; this fails it by name.
    const own = path.join(SRC, "feed/cards/tool-head.ts");
    const offenders = sources(SRC)
      .filter((file) => file !== own)
      .filter((file) => /className\s*=\s*["'`]tool-(head|name)\b/.test(readFileSync(file, "utf8")));
    expect(offenders.map((file) => path.relative(SRC, file))).toEqual([]);
  });
});
