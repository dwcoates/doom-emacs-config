// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  WorkspaceNameEmojiAckSchema,
  WorkspaceNamePrefixSchema,
  WorkspaceNamePrioritySchema,
} from "../../proto/gen/ts/frontend/v1/workspace_name_prefix_pb";
import {
  NAME_EMOJI_ACK_CLASS,
  NAME_PREFIX_CLASS,
  NAME_PRIORITY_CLASS,
  drawNamePrefix,
  drawNameEmojiAck,
  drawNamePriority,
  type PriorityArm,
} from "../src/name-prefix.js";
import { MalformedView } from "../src/rpc/malformed.js";
import { cascadedValue, installStylesheet } from "./stylesheet.js";
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import priorityLabels from "../../proto/vocab/priority-labels.json";

const here = path.dirname(fileURLToPath(import.meta.url));
const ARMS: readonly PriorityArm[] = ["p05", "p1", "p2", "p3"];
const LABELS: Readonly<Record<PriorityArm, string>> = { p05: "P0.5", p1: "P1", p2: "P2", p3: "P3" };

const prefixOf = (arm: PriorityArm) =>
  create(WorkspaceNamePrefixSchema, { priority: { level: { case: arm, value: {} } } });

describe("drawNamePrefix", () => {
  it("draws nothing for a name that wears no prefix", () => {
    expect(drawNamePrefix(undefined, "P")).toBeUndefined();
  });

  it.each(ARMS)("draws the %s icon inside the prefix element", (arm) => {
    const drawn = drawNamePrefix(prefixOf(arm), "P");
    expect(drawn?.className).toBe(NAME_PREFIX_CLASS);
    expect(drawn?.querySelector(`.${NAME_PRIORITY_CLASS}`)?.getAttribute("data-priority")).toBe(arm);
  });

  it("refuses a set prefix carrying no mark", () => {
    expect(() => drawNamePrefix(create(WorkspaceNamePrefixSchema, {}), "P")).toThrow(MalformedView);
  });
});

describe("drawNamePriority", () => {
  it.each(ARMS)("labels the %s icon with its level", (arm) => {
    const icon = drawNamePriority(prefixOf(arm).priority!, "P") as HTMLImageElement;
    expect(icon.alt).toBe(LABELS[arm]);
    expect(icon.title).toBe(LABELS[arm]);
  });

  it("draws each level as its own bundled picture", () => {
    const sources = ARMS.map((arm) => (drawNamePriority(prefixOf(arm).priority!, "P") as HTMLImageElement).src);
    expect(sources.every((src) => src !== "")).toBe(true);
    expect(new Set(sources).size).toBe(ARMS.length);
  });

  it("refuses a priority naming no level", () => {
    expect(() => drawNamePriority(create(WorkspaceNamePrioritySchema, {}), "P")).toThrow(MalformedView);
  });
});

describe("the priority labels", () => {
  it.each(ARMS)("labels the %s icon from the shared vocabulary", (arm) => {
    const icon = drawNamePriority(prefixOf(arm).priority!, "P") as HTMLImageElement;
    expect(icon.alt).toBe(priorityLabels.labels[arm]);
  });

  it("never spells a label in the drawer itself", () => {
    const source = readFileSync(path.join(here, "../src/name-prefix.ts"), "utf8");
    expect(source).not.toContain('"P0.5"');
  });
});

describe("drawNameEmojiAck", () => {
  it("draws the daemon's glyph verbatim in its own element", () => {
    const glyph = drawNameEmojiAck(create(WorkspaceNameEmojiAckSchema, { text: "👍" }), "P.emoji_ack");
    expect([glyph.tagName, glyph.className, glyph.textContent]).toEqual(["SPAN", NAME_EMOJI_ACK_CLASS, "👍"]);
  });

  it("refuses a glyph that carries none", () => {
    expect(() => drawNameEmojiAck(create(WorkspaceNameEmojiAckSchema, { text: "" }), "P.emoji_ack")).toThrow(MalformedView);
  });

  it.each([
    ["font-size", "0.85em"],
    ["line-height", "1"],
  ])("is drawn a little smaller than the name, on the name's line (%s %s)", (property, value) => {
    // Arrange
    const uninstall = installStylesheet();
    const prefix = drawNamePrefix(create(WorkspaceNamePrefixSchema, { emojiAck: { text: "👍" } }), "P")!;
    document.body.append(prefix);

    // Act
    const cascaded = cascadedValue(prefix.querySelector(`.${NAME_EMOJI_ACK_CLASS}`)!, property);
    uninstall();
    prefix.remove();

    // Assert
    expect(cascaded).toBe(value);
  });

  // IN A SIDEBAR ROW THE GLYPH IS SIZED FROM THE STATUS MARK (owner request,
  // 2026-11-20): only slightly bigger than the indicator to its left.
  it("is a quarter larger than the status mark in a sidebar row", () => {
    // Arrange
    const uninstall = installStylesheet();
    const { sidebar, prefix } = inSidebarRow(create(WorkspaceNamePrefixSchema, { emojiAck: { text: "👍" } }));

    // Act
    const cascaded = [
      cascadedValue(sidebar, "--ws-mark-size"),
      cascadedValue(prefix.querySelector(`.${NAME_EMOJI_ACK_CLASS}`)!, "font-size"),
    ];
    uninstall();
    sidebar.remove();

    // Assert
    expect(cascaded).toEqual(["9px", "calc(var(--ws-mark-size) * 1.25)"]);
  });
});

/** Draw PREFIX into a sidebar row, as the roster draws a row's prefix. */
function inSidebarRow(prefix: ReturnType<typeof create<typeof WorkspaceNamePrefixSchema>>): {
  sidebar: HTMLElement;
  row: HTMLElement;
  prefix: HTMLElement;
} {
  const sidebar = document.createElement("div");
  sidebar.id = "ws-sidebar";
  const row = document.createElement("div");
  row.className = "row";
  const drawn = drawNamePrefix(prefix, "R.name.prefix")!;
  row.append(drawn);
  sidebar.append(row);
  document.body.append(sidebar);
  return { sidebar, row, prefix: drawn };
}

// ONE WORD SPACE STANDS BETWEEN THE PREFIX AND THE NAME (owner request,
// 2026-11-20). A sidebar row is a flex line whose gap stood there as well, so
// the prefix takes the row's gap back.
describe("the space before the name", () => {
  it("is one word space outside a sidebar row", () => {
    // Arrange
    const uninstall = installStylesheet();
    const prefix = drawNamePrefix(create(WorkspaceNamePrefixSchema, { emojiAck: { text: "👍" } }), "P")!;
    document.body.append(prefix);

    // Act
    const cascaded = [cascadedValue(prefix, "--name-prefix-space"), cascadedValue(prefix, "margin-right")];
    uninstall();
    prefix.remove();

    // Assert
    expect(cascaded).toEqual(["0.25em", "var(--name-prefix-space)"]);
  });

  it("takes a sidebar row's gap back, so one word space is all that stands before the name", () => {
    // Arrange
    const uninstall = installStylesheet();
    const { sidebar, row, prefix } = inSidebarRow(
      create(WorkspaceNamePrefixSchema, { priority: { level: { case: "p1", value: {} } } }),
    );

    // Act
    const cascaded = [cascadedValue(row, "gap"), cascadedValue(row, "--row-gap"), cascadedValue(prefix, "margin-right")];
    uninstall();
    sidebar.remove();

    // Assert
    expect(cascaded).toEqual(["var(--row-gap)", "0.45rem", "calc(var(--name-prefix-space) - var(--row-gap))"]);
  });
});

describe("the prefix's drawn order", () => {
  it("draws the priority icon first and the emoji-ack glyph to its right", () => {
    const prefix = drawNamePrefix(
      create(WorkspaceNamePrefixSchema, {
        priority: { level: { case: "p2", value: {} } },
        emojiAck: { text: "👍" },
      }),
      "P",
    );
    expect([...(prefix?.children ?? [])].map((mark) => mark.className)).toEqual([NAME_PRIORITY_CLASS, NAME_EMOJI_ACK_CLASS]);
  });

  // EVERY SURFACE DRAWS THE PREFIX THROUGH THIS ONE HELPER: a surface that
  // built its own mark element would draw it apart from the others.
  it.each(["src/sidebar/row.ts", "src/topbar/strip.ts"])("is the one drawing %s uses", (file) => {
    const source = readFileSync(path.join(here, "..", file), "utf8");
    expect([source.includes("drawNamePrefix("), source.includes('"name-emoji-ack"'), source.includes('"name-priority"')]).toEqual([
      true,
      false,
      false,
    ]);
  });
});
