// @vitest-environment jsdom
/**
 * THE SHARED STYLESHEET-RULE READERS (`test/stylesheet.ts`).
 *
 * Every test that reads a rule off `src/styles.css` by text goes through
 * `rulesOf` or `selectorOf`, so a comment that names a selector can never be
 * credited to the rule after it. `source-text.test.ts` pins that sharing.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { cascadedValue, cascadedValueIn, inDocument, inRail, rulesOf, selectorOf } from "./stylesheet.js";
import STYLES_TEST from "./styles.test.ts?raw";
import STYLESHEET from "./stylesheet.ts?raw";
import { filesUnder } from "./files-under.js";

describe("rulesOf", () => {
  it.each([
    {
      name: "reads each rule's selectors and declarations in source order",
      css: ".a { color: red; } .b { color: blue; }",
      want: [
        { selectors: [".a"], declarations: " color: red; " },
        { selectors: [".b"], declarations: " color: blue; " },
      ],
    },
    {
      name: "splits a selector list into its trimmed selectors",
      css: ".a ,\n .b > .c { display: none; }",
      want: [{ selectors: [".a", ".b > .c"], declarations: " display: none; " }],
    },
    {
      name: "never credits a rule with a selector its preceding comment names",
      css: "/* see .bubble-subfeed { } */ .strip { display: flex; }",
      want: [{ selectors: [".strip"], declarations: " display: flex; " }],
    },
    {
      name: "reads no rules from a sheet of comments alone",
      css: "/* .a { color: red; } */",
      want: [],
    },
  ])("$name", ({ css, want }) => {
    // Arrange / Act
    const rules = rulesOf(css);
    // Assert
    expect(rules).toEqual(want);
  });
});

describe("selectorOf", () => {
  it("answers the selector the pattern's first group captures from the real stylesheet", () => {
    // Arrange / Act
    const selector = selectorOf(/([^{}]*\.merge-strip)\s*\{/, "merge strip");
    // Assert
    expect(selector).toBe(".merge-strip");
  });

  it("fails loudly naming what it looked for when nothing matches", () => {
    // Arrange / Act / Assert
    expect(() => selectorOf(/(\.no-such-class-anywhere)\s*\{/, "phantom rule")).toThrow(
      "the stylesheet has no phantom rule",
    );
  });
});

describe("inDocument", () => {
  it("mounts the drawn element in the document with the real stylesheet", () => {
    // Arrange: a dot carrying a tone, which only the real stylesheet paints.
    const dot = document.createElement("span");
    dot.className = "st tone-green";

    // Act
    const seen = inDocument(dot, (drawn) => ({ same: drawn === dot, attached: document.body.contains(dot), paint: cascadedValue(dot, "color") }));

    // Assert
    expect(seen).toEqual({ same: true, attached: true, paint: "var(--ok)" });
  });

  it("takes the element and the stylesheet down once the check returns", () => {
    // Arrange
    const el = document.createElement("span");
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    inDocument(el, () => undefined);

    // Assert
    expect([el.isConnected, document.head.querySelectorAll("style").length]).toEqual([false, sheets]);
  });

  it("takes the element and the stylesheet down when the check throws", () => {
    // Arrange
    const el = document.createElement("span");
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    const act = (): void =>
      inDocument(el, () => {
        throw new Error("boom");
      });

    // Assert
    expect(act).toThrow("boom");
    expect([el.isConnected, document.head.querySelectorAll("style").length]).toEqual([false, sheets]);
  });

  it("is the one mount-and-teardown the stylesheet suites share", () => {
    // Arrange: the hand-rolled shape inDocument replaced, an element removed and then the sheet torn down.
    const shape = /\.remove\(\);\s*teardown\(\);/g;

    // Act
    const sites = [STYLES_TEST.match(shape)?.length ?? 0, STYLESHEET.match(shape)?.length ?? 0];

    // Assert
    expect(sites).toEqual([0, 1]);
  });
});

describe("cascadedValueIn", () => {
  it("answers the declaration the cascade hands the child inside the parent", () => {
    // Arrange / Act: a verdict's chip, which only the parent-scoped rule rounds.
    const radius = cascadedValueIn("q-verdict", "badge q-chip", "border-radius");

    // Assert
    expect(radius).toBe("var(--tab-radius)");
  });

  it("takes the pair and the stylesheet down once it answers", () => {
    // Arrange
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    cascadedValueIn("q-verdict", "badge q-chip", "color");

    // Assert
    expect([document.body.querySelector(".q-verdict"), document.head.querySelectorAll("style").length]).toEqual([null, sheets]);
  });

  it("is the one child-in-parent cascade probe the stylesheet suite shares", () => {
    // Arrange: the hand-rolled shape cascadedValueIn replaced.
    const shape = /document\.body\.replaceChildren\(parent\)/g;

    // Act
    const sites = STYLES_TEST.match(shape)?.length ?? 0;

    // Assert
    expect(sites).toBe(0);
  });
});

describe("inRail", () => {
  it("mounts the drawn element in the sidebar rail with the real stylesheet", () => {
    // Arrange: a dot carrying a tone, which only the real stylesheet paints.
    const dot = document.createElement("span");
    dot.className = "st tone-green";

    // Act
    const seen = inRail(dot, (rail) => ({
      id: rail.id,
      attached: document.body.contains(dot),
      paint: cascadedValue(dot, "color"),
    }));

    // Assert
    expect(seen).toEqual({ id: "ws-sidebar", attached: true, paint: "var(--ok)" });
  });

  it("takes the rail and the stylesheet down once the check returns", () => {
    // Arrange
    const dot = document.createElement("span");
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    inRail(dot, () => undefined);

    // Assert
    expect([document.getElementById("ws-sidebar"), document.head.querySelectorAll("style").length]).toEqual([null, sheets]);
  });

  it("takes the rail and the stylesheet down when the check throws", () => {
    // Arrange
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    const act = (): void =>
      inRail(document.createElement("span"), () => {
        throw new Error("boom");
      });

    // Assert
    expect(act).toThrow("boom");
    expect([document.getElementById("ws-sidebar"), document.head.querySelectorAll("style").length]).toEqual([null, sheets]);
  });

  it("is the only place a test builds the jsdom sidebar rail", () => {
    // Arrange: the hand-rolled shape inRail replaced.
    const here = path.dirname(fileURLToPath(import.meta.url));
    const shape = 'rail.id = "ws-sidebar"';
    const owners = new Set([path.join(here, "stylesheet.ts"), fileURLToPath(import.meta.url)]);

    // Act
    const handRolled = filesUnder(here).filter((file) => !owners.has(file) && readFileSync(file, "utf8").includes(shape));

    // Assert
    expect(handRolled.map((file) => path.relative(here, file))).toEqual([]);
  });
});
