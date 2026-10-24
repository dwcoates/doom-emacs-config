// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedResponseAnswerSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { ProseEntryTreeSchema, type ProseEntryTree } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import {
  ANSWER_PROMPT_ATTRIBUTE,
  ENTRY_ATTRIBUTE,
  ENTRY_CLASS,
  ENTRY_FLASH_CLASS,
  ENTRY_LABEL_CLASS,
  ENTRY_PATH_CLASS,
  LABEL_LINK_CLASS,
  entryTreeHtml,
  flashEntry,
  flashEntryOncePainted,
  linkAnswers,
} from "../../src/feed/entries.js";
import { createBubbleBody, markdownSlot, paintBody } from "../../src/bubble/body.js";
import { drawBubble } from "../../src/bubble/draw.js";
import { useTreeLayout } from "../tree-layout.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { plainProse, promptProse } from "../prose.js";
import { inline } from "../../src/markdown.js";
import { TREE_NODE_ATTRIBUTE } from "../../src/metaprompt-tree.js";

/** The entry tree the daemon parses PROMPT's one run of entries into. */
function entryTree(prompt: string): ProseEntryTree {
  const block = promptProse(prompt).blocks[0]?.block;
  if (block?.case !== "entries") throw new Error(`no entry tree leads ${JSON.stringify(prompt)}`);
  return block.value;
}

/** PROMPT's entry tree drawn, in a detached element. */
const drawnEntries = (prompt: string): HTMLElement => hosted(entryTreeHtml(entryTree(prompt), inline, 80));

/** The lines of EL whose node is LABEL. */
const linesOf = (el: HTMLElement, label: string): Element[] =>
  [...el.querySelectorAll(".mp-line")].filter((l) => l.getAttribute(TREE_NODE_ATTRIBUTE) === label);

/** HTML in a detached element. */
function hosted(html: string): HTMLElement {
  const el = document.createElement("div");
  el.innerHTML = html;
  return el;
}

/** Tree labels drawn as the tree draws them (metaprompt-tree.ts). */
const labels = (...names: string[]): HTMLElement =>
  hosted(names.map((n) => `<span class="mp-label" data-label="${n}">${n}</span>`).join(""));

/** An answer of LABEL by the prompt row PROMPT. */
const answer = (label: string, prompt: string) => create(FeedResponseAnswerSchema, { label, prompt: { value: prompt } });

describe("entryTreeHtml", () => {
  it("draws each entry's line with its label as the click target", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    const entries = [...el.querySelectorAll(`.${ENTRY_CLASS}`)].map((l) => [
      l.getAttribute(ENTRY_ATTRIBUTE),
      l.querySelector(`.${ENTRY_LABEL_CLASS}`)?.textContent,
    ]);
    expect(entries).toEqual([
      ["3.1", "3.1"],
      ["4.2", "4.2"],
      ["5.5.1", "5.5.1"],
    ]);
  });

  it("draws an entry's words as inline markdown", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    expect(linesOf(el, "4.2")[0]?.querySelector("strong")?.textContent).toBe("b");
  });

  it("draws every ancestor a deeper label implies by its label alone, dimmed", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    const paths = [...el.querySelectorAll(`.${ENTRY_PATH_CLASS}`)].map((l) => [
      l.getAttribute(TREE_NODE_ATTRIBUTE),
      l.querySelector(".mp-content")?.textContent,
    ]);
    expect(paths).toEqual([
      ["3", ""],
      ["4", ""],
      ["5", ""],
      ["5.5", ""],
    ]);
  });

  it("draws the rails the labels make, with no blank line between roots", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    const prefixes = [...el.querySelectorAll(".mp-line")].map((l) => l.querySelector(".mp-prefix")?.textContent);
    expect(prefixes).toEqual(["3 ", "└── 3.1 ", "4 ", "└── 4.2 ", "5 ", "└── 5.5 ", "    └── 5.5.1 "]);
  });

  it("draws an entry that says nothing as its label alone", () => {
    // Act
    const el = drawnEntries("3.1 -");
    // Assert
    expect(linesOf(el, "3.1")[0]?.querySelector(".mp-content")?.textContent).toBe("");
  });

  it("escapes what an entry says rather than reading it as markup", () => {
    // Act
    const el = drawnEntries("3.1 - <b>x</b>");
    // Assert
    expect([linesOf(el, "3.1")[0]?.querySelector("b"), linesOf(el, "3.1")[0]?.textContent]).toEqual([null, "└── 3.1 <b>x</b>"]);
  });

  it.each([
    ["no label", { nodes: [{ id: { index: 0 }, place: { case: "root", value: {} }, kind: { case: "path", value: {} } }] }],
    ["no kind", { nodes: [{ id: { index: 0 }, place: { case: "root", value: {} }, label: { text: "3" } }] }],
    ["no place", { nodes: [{ id: { index: 0 }, label: { text: "3" }, kind: { case: "path", value: {} } }] }],
  ])("refuses a node served with %s", (_name, init) => {
    // Act + Assert
    expect(() => entryTreeHtml(create(ProseEntryTreeSchema, init as never), inline, 80)).toThrow(MalformedView);
  });
});

describe("linkAnswers", () => {
  it("links each answered label to its prompt and leaves the others plain", () => {
    // Arrange
    const el = labels("1.1", "1.2");
    // Act
    linkAnswers(el, [answer("1.1", "p1")]);
    // Assert
    const [first, second] = [...el.children];
    expect([first.classList.contains(LABEL_LINK_CLASS), first.getAttribute(ANSWER_PROMPT_ATTRIBUTE), second.classList.contains(LABEL_LINK_CLASS)]).toEqual([
      true,
      "p1",
      false,
    ]);
  });

  it("links a label several prompts answer to the latest of them", () => {
    // Arrange
    const el = labels("1.1");
    // Act
    linkAnswers(el, [answer("1.1", "p1"), answer("1.1", "p2")]);
    // Assert
    expect(el.firstElementChild?.getAttribute(ANSWER_PROMPT_ATTRIBUTE)).toBe("p2");
  });

  it("unlinks a label whose answer is gone on the next paint", () => {
    // Arrange
    const el = labels("1.1");
    linkAnswers(el, [answer("1.1", "p1")]);
    // Act
    linkAnswers(el, []);
    // Assert
    const label = el.firstElementChild;
    expect([label?.classList.contains(LABEL_LINK_CLASS), label?.hasAttribute(ANSWER_PROMPT_ATTRIBUTE)]).toEqual([false, false]);
  });

  it("refuses an answer served without its prompt", () => {
    // Act + Assert
    expect(() => linkAnswers(labels("1.1"), [create(FeedResponseAnswerSchema, { label: "1.1" })])).toThrow(MalformedView);
  });
});

describe("flashEntry", () => {
  it("flashes the row's entry answering the label", () => {
    // Arrange
    const row = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Act
    flashEntry(row, "4.2");
    // Assert
    const flashed = [...row.querySelectorAll(`.${ENTRY_FLASH_CLASS}`)].map((el) => el.getAttribute("data-entry-label"));
    expect(flashed).toEqual(["4.2"]);
  });

  it("records a row that draws no such entry at warn", async () => {
    // Arrange
    const capture = captureLogRecords();
    const row = drawnEntries("3.1 -");
    row.setAttribute("data-feed-row", "p1");
    // Act
    flashEntry(row, "9.9");
    // Assert
    const record = await forwardedRecord(capture, "feed.entries.flash-absent");
    expect([record.level.case, record.context]).toEqual(["warn", expect.objectContaining({ row: "p1", label: "9.9" })]);
  });
});

describe("flashEntryOncePainted", () => {
  useTreeLayout();

  it("flashes at once a row with no markdown slot to wait for", () => {
    // Arrange
    const row = drawnEntries("3.1 -");
    // Act
    flashEntryOncePainted(row, "3.1");
    // Assert
    expect(row.querySelector(`.${ENTRY_FLASH_CLASS}`)).not.toBeNull();
  });

  it("waits for the row's slot to paint before flashing its entry", () => {
    // Arrange — a prompt holding an entry and a pasted tree, whose slot paints
    // only once the bubble is laid out.
    const slot = markdownSlot("prose", promptProse("3.1 - yes\n1 🔧 Fixed\n└── 1.1 a"));
    const { bubble } = drawBubble({ role: "response", variant: "response", content: [slot], capLines: "feed" });
    const row = document.createElement("article");
    row.append(bubble);
    flashEntryOncePainted(row, "3.1");
    const before = row.querySelector(`.${ENTRY_FLASH_CLASS}`);
    // Act
    const column = document.createElement("div");
    column.append(row);
    document.body.replaceChildren(column);
    // Assert
    expect([before, row.querySelector(`.${ENTRY_FLASH_CLASS}`)?.getAttribute("data-entry-label")]).toEqual([null, "3.1"]);
  });

  it("flashes nothing while the row's prose is plain and holds no entry", async () => {
    // Arrange
    const capture = captureLogRecords();
    const row = document.createElement("article");
    const slot = markdownSlot("prose", plainProse("no entries"));
    row.append(slot);
    paintBody(createBubbleBody(), [slot]);
    // Act
    flashEntryOncePainted(row, "3.1");
    // Assert
    const record = await forwardedRecord(capture, "feed.entries.flash-absent");
    expect(record.level.case).toBe("warn");
  });
});
