// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedResponseAnswerSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { ProseEntrySchema } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import {
  ANSWER_PROMPT_ATTRIBUTE,
  ENTRY_FLASH_CLASS,
  LABEL_LINK_CLASS,
  entryHtml,
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

/** An entry answering LABEL with TEXT. */
const entry = (label: string, text: string) => create(ProseEntrySchema, { label: { text: label }, text: { markdown: text } });

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

describe("entryHtml", () => {
  it("draws the label as the click target and the words as markdown", () => {
    // Act
    const el = hosted(entryHtml(entry("3.1", "**yes**")));
    // Assert
    expect([
      el.querySelector(".mp-entry")?.getAttribute("data-entry-label"),
      el.querySelector(".mp-entry-label")?.textContent,
      el.querySelector(".mp-entry-text strong")?.textContent,
    ]).toEqual(["3.1", "3.1", "yes"]);
  });

  it("draws an entry that says nothing as its label alone", () => {
    // Act
    const el = hosted(entryHtml(entry("3.1", "")));
    // Assert
    expect(el.querySelector(".mp-entry-text")?.innerHTML).toBe("");
  });

  it("escapes a label rather than reading it as markup", () => {
    // Act
    const el = hosted(entryHtml(entry("<b>1</b>", "x")));
    // Assert
    expect(el.querySelector(".mp-entry-label")?.textContent).toBe("<b>1</b>");
  });

  it("refuses an entry served without its label", () => {
    // Act + Assert
    expect(() => entryHtml(create(ProseEntrySchema, { text: { markdown: "x" } }))).toThrow(MalformedView);
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
    const row = hosted(entryHtml(entry("3.1", "a")) + entryHtml(entry("3.2", "b")));
    // Act
    flashEntry(row, "3.2");
    // Assert
    const flashed = [...row.querySelectorAll(`.${ENTRY_FLASH_CLASS}`)].map((el) => el.getAttribute("data-entry-label"));
    expect(flashed).toEqual(["3.2"]);
  });

  it("records a row that draws no such entry at warn", async () => {
    // Arrange
    const capture = captureLogRecords();
    const row = hosted(entryHtml(entry("3.1", "a")));
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
    const row = hosted(entryHtml(entry("3.1", "a")));
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
