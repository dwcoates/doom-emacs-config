import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { plainProse, servedProse, treeProse } from "./prose.js";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("plainProse", () => {
  it.each([
    { name: "one line", text: "hello" },
    { name: "a trailing newline", text: "**done**\n" },
    { name: "several lines", text: "first\nsecond" },
    { name: "only whitespace", text: "  \n" },
    { name: "a numbered list", text: "1. one\n2. two" },
  ])("is the daemon's own parse of $name", ({ text }) => {
    // Act + Assert — the fixture holds the daemon's answer for each.
    expect(plainProse(text)).toEqual(treeProse(text));
  });

  it.each([
    { name: "a connector line", text: "intro\n├── 1.1 a" },
    { name: "a last-child connector", text: "└── 1.1 a" },
    { name: "a labelled root", text: "1 🔧 Fixed it" },
  ])("refuses $name", ({ text }) => {
    // Act + Assert
    expect(() => plainProse(text)).toThrow(/plainProse given a tree/);
  });
});

describe("treeProse", () => {
  it("refuses a text the fixture holds no parse of, naming how to add one", () => {
    // Act + Assert
    expect(() => treeProse("1 🔧 never parsed\n└── 1.1 x")).toThrow(/prose-inputs\.json/);
  });

  it("answers the parse of a text still arriving", () => {
    // Arrange — the model is still typing the branch's label.
    const text = "1 🔧 Arriving\n└── 1.";
    // Act
    const block = treeProse(text, { arriving: true }).blocks[0]?.block;
    // Assert — the partial label is not drawn as the branch's text.
    expect(block?.case === "tree" ? block.value.nodes[1]?.text?.markdown : undefined).toBe("");
  });

  it("tells a settled parse from an arriving one", () => {
    // Act + Assert — the fixture holds this text only as it arrives.
    expect(() => treeProse("1 🔧 Arriving\n└── 1.")).toThrow(/arriving: false/);
  });
});

describe("servedProse", () => {
  it("serves a tree-shaped text as the daemon's parse", () => {
    // Arrange
    const text = "1 🔧 Fixed it\n└── 1.1 x";
    // Act + Assert
    expect(servedProse(text)).toEqual(treeProse(text));
  });

  it("serves text with no tree as one plain block", () => {
    // Act + Assert
    expect(servedProse("hello")).toEqual(plainProse("hello"));
  });

  it("is the one helper every fixture builder that takes either kind uses", () => {
    // Arrange — a builder handing a text variable to plainProse would refuse a
    // tree only when one is passed, so no builder does.
    const builders = ["feed/harness.ts", "integration/fixtures.ts"];
    // Act
    const handRolled = builders.filter((file) =>
      /plainProse\([a-z][A-Za-z]*\)/.test(readFileSync(path.join(here, file), "utf8")),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
