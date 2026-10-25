// @vitest-environment jsdom
/**
 * copy-room — a bubble's copy control, and the ghost that opens its first line
 * of text so the bubble widens by the control's room.
 */
import { readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import {
  BUBBLE_COPY_CLASS,
  COPY_ROOM_CLASS,
  bubbleCopyControl,
  markCopyRoom,
  reserveCopyRoom,
} from "../../src/bubble/copy-room.js";
import { createBubbleBody, markdownSlot, paintBody } from "../../src/bubble/body.js";
import { COPY_CONTROL_CLASS, COPY_KIND_ATTRIBUTE, boundCopyText } from "../../src/copy-control.js";
import { plainProse } from "../prose.js";

afterEach(() => {
  document.body.replaceChildren();
});

/** ROOT built from HTML. */
function root(html: string): HTMLElement {
  const el = document.createElement("div");
  el.innerHTML = html;
  return el;
}

/** Each ghost under EL, as the tag of the element it opens and whether it is that element's first child. */
function ghosts(el: Element): [string | undefined, boolean][] {
  return [...el.querySelectorAll(`.${COPY_ROOM_CLASS}`)].map((g) => [
    g.parentElement?.tagName.toLowerCase(),
    g.parentElement?.firstChild === g,
  ]);
}

describe("bubbleCopyControl", () => {
  it("is the one copy control, of KIND, placed as a bubble's", () => {
    // Act
    const control = bubbleCopyControl("response", "Copy response", "text");
    // Assert
    expect([
      control.classList.contains(COPY_CONTROL_CLASS),
      control.classList.contains(BUBBLE_COPY_CLASS),
      control.getAttribute(COPY_KIND_ATTRIBUTE),
      control.title,
    ]).toEqual([true, true, "response", "Copy response"]);
  });

  it("copies the text it was handed", () => {
    // Act
    const control = bubbleCopyControl("prompt", "Copy prompt", "**my words**");
    // Assert
    expect(boundCopyText(control).text).toBe("**my words**");
  });
});

describe("markCopyRoom", () => {
  it.each([
    ["a paragraph", "<p>hello</p><p>later</p>", [["p", true]]],
    ["a heading", "<h2>Title</h2><p>body</p>", [["h2", true]]],
    ["a tight list's first item", "<ul><li>one</li><li>two</li></ul>", [["li", true]]],
    ["a loose list's first paragraph", "<ol><li><p>one</p></li></ol>", [["p", true]]],
    ["a code block", "<pre><code>let x = 1;</code></pre>", [["pre", true]]],
    ["a line of inline emphasis", "<p><strong>bold</strong> then plain</p>", [["p", true]]],
    ["text held by the root itself", "bare text", [["div", true]]],
  ])("opens %s", (_name, html, want) => {
    // Arrange
    const el = root(html);
    // Act
    markCopyRoom(el);
    // Assert
    expect(ghosts(el)).toEqual(want);
  });

  it("passes over white space to the first character of text", () => {
    // Arrange
    const el = root("<p>  \n </p><p>words</p>");
    // Act
    markCopyRoom(el);
    // Assert
    expect(el.querySelectorAll("p")[1]?.firstElementChild?.className).toBe(COPY_ROOM_CLASS);
  });

  it("answers that it opened a line", () => {
    // Act / Assert
    expect(markCopyRoom(root("<p>hi</p>"))).toBe(true);
  });

  it.each([
    ["nothing", ""],
    ["white space alone", "<p> \n </p>"],
    ["an image alone", '<img src="x.png" alt="">'],
  ])("opens no line in %s, and answers so", (_name, html) => {
    // Arrange
    const el = root(html);
    // Act
    const opened = markCopyRoom(el);
    // Assert
    expect([opened, ghosts(el)]).toEqual([false, []]);
  });

  it("draws a ghost that adds no text and is hidden from assistive technology", () => {
    // Arrange
    const el = root("<p>hello</p>");
    // Act
    markCopyRoom(el);
    // Assert
    const ghost = el.querySelector(`.${COPY_ROOM_CLASS}`);
    expect([el.textContent, ghost?.getAttribute("aria-hidden")]).toEqual(["hello", "true"]);
  });
});

describe("reserveCopyRoom", () => {
  /** CONTENT painted into a fresh body in the document. */
  function painted(content: ChildNode[]): HTMLElement {
    const body = createBubbleBody();
    document.body.append(body);
    paintBody(body, content);
    return body;
  }

  it("opens a markdown slot's first line on its paint", () => {
    // Arrange
    const slot = markdownSlot("prose", plainProse("first\n\nsecond"));
    // Act
    reserveCopyRoom([slot]);
    // Assert
    expect(ghosts(painted([slot]))).toEqual([["p", true]]);
  });

  it("keeps the slot kind's own mark", () => {
    // Arrange
    const slot = markdownSlot("prose", plainProse("first"), (rendered) => rendered.querySelector("p")?.classList.add("own"));
    // Act
    reserveCopyRoom([slot]);
    // Assert
    expect(painted([slot]).querySelector("p")?.classList.contains("own")).toBe(true);
  });

  it("opens a drawn element's first line at once", () => {
    // Arrange
    const quote = root("<div>⟢ Replying<span>:</span></div><div><pre><code>quoted</code></pre></div>");
    // Act
    reserveCopyRoom([quote]);
    // Assert
    expect(ghosts(quote)).toEqual([["div", true]]);
  });

  it("opens only the first content node, whatever follows it", () => {
    // Arrange
    const first = markdownSlot("prose", plainProse("first"));
    const second = markdownSlot("prose", plainProse("second"));
    // Act
    reserveCopyRoom([first, second]);
    // Assert
    expect(painted([first, second]).querySelectorAll(`.${COPY_ROOM_CLASS}`)).toHaveLength(1);
  });

  it("opens no line when the content opens with an image", () => {
    // Arrange
    const image = document.createElement("img");
    const words = markdownSlot("prose", plainProse("after"));
    // Act
    reserveCopyRoom([image, words]);
    // Assert
    expect(ghosts(painted([image, words]))).toEqual([]);
  });

  it("leaves empty content alone", () => {
    // Act / Assert
    expect(() => reserveCopyRoom([])).not.toThrow();
  });
});

describe("every bubble copy control's draw", () => {
  /** Every source file under src/, by its path from src/. */
  function sources(): { file: string; code: string }[] {
    const src = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src");
    const walk = (dir: string): string[] =>
      readdirSync(dir, { withFileTypes: true }).flatMap((entry) =>
        entry.isDirectory() ? walk(path.join(dir, entry.name)) : entry.name.endsWith(".ts") ? [path.join(dir, entry.name)] : [],
      );
    return walk(src).map((file) => ({ file: path.relative(src, file), code: readFileSync(file, "utf8") }));
  }

  it("leaves its room through reserveCopyRoom, wherever a copy control goes in a bubble's corner", () => {
    // Arrange: every file that draws a bubble's copy control into a bubble.
    const drawing = sources().filter(
      ({ file, code }) =>
        file !== "bubble/copy-room.ts" &&
        file !== "feed/rows/blocks.ts" &&
        /\b(?:bubbleCopyControl|drawPromptCopyControl)\(/.test(code),
    );
    // Act
    const without = drawing.filter(({ code }) => !/\breserveCopyRoom\(/.test(code)).map(({ file }) => file);
    // Assert
    expect([drawing.length > 0, without]).toEqual([true, []]);
  });

  it("is never a hand-rolled copy control in a bubble's corner", () => {
    // Act: every other file that builds a bound copy control.
    const rolled = sources()
      .filter(({ file, code }) => file !== "copy-control.ts" && file !== "bubble/copy-room.ts" && /\bboundCopyControl\(/.test(code))
      .map(({ file }) => file);
    // Assert
    expect(rolled).toEqual([]);
  });
});
