// @vitest-environment jsdom
/**
 * A TOOL CARD'S TITLE TYPES OUT (owner request, 2026-10-12): on a row's
 * first draw from a live push, resuming across its redraws, and never on a
 * row drawn from a page, a full collapsed title, or an unseen page.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { installAnimationFrames, type AnimationFrames } from "../animation-frames.js";
import { create } from "@bufbuild/protobuf";
import { FeedIdSchema, FeedRowSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { RowArrival, RowContext } from "../../src/feed/cards/context.js";
import { foldTitle } from "../../src/feed/title-fold.js";
import { TITLE_REVEALED_ATTRIBUTE } from "../../src/feed/title-reveal.js";
import { fullTextOf } from "../../src/bubble/text-reveal.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { fakePageSeen } from "../page-seen-fake.js";
import { harness, rowContext } from "./harness.js";
import { measureTitle } from "./title-measure.js";

const COMMAND = "$ go test ./... && npm run typecheck";

/** The page's frames, run by hand. */
let animation: AnimationFrames;

/** Run COUNT animation frames. */
function frames(count: number): void {
  animation.run(count);
}

beforeEach(() => {
  animation = installAnimationFrames();
});
afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

/** A row context for a row that ARRIVED so, on a page SEEN says, after PREVIOUS. */
function context(arrival: RowArrival, opts: { previous?: HTMLElement; seen?: boolean } = {}): RowContext {
  const { ctx } = harness();
  const row = create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }) });
  return rowContext({ ...ctx, seen: fakePageSeen(opts.seen ?? true) }, row, {
    arrival,
    ...(opts.previous === undefined ? {} : { previous: opts.previous }),
  });
}

/** A collapsed `.tool-fold` card holding COMMAND as its title, folded under RC. */
function card(rc: RowContext, overflow = false): { card: HTMLElement; title: HTMLElement } {
  const el = document.createElement("div");
  el.className = "tool-card tool-fold";
  const title = document.createElement("pre");
  title.textContent = COMMAND;
  measureTitle(title, overflow);
  el.append(title);
  document.body.replaceChildren(el);
  foldTitle(title, "card", rc);
  return { card: el, title };
}

/** The characters of TITLE on screen. */
const onScreen = (title: HTMLElement): number => title.textContent?.length ?? 0;

describe("a tool card's title type-out", () => {
  it("types a pushed row's title out from nothing", () => {
    // Arrange
    const { title } = card(context("push"));
    // Act
    frames(10);
    // Assert
    expect([onScreen(title) > 0, onScreen(title) < COMMAND.length]).toEqual([true, true]);
  });

  it("shows a paged row's title whole", () => {
    // Arrange, Act
    const { title } = card(context("page"));
    frames(1);
    // Assert
    expect([onScreen(title), title.hasAttribute(TITLE_REVEALED_ATTRIBUTE)]).toEqual([COMMAND.length, false]);
  });

  it("shows the whole title, carrying nothing, once the type-out ends", () => {
    // Arrange
    const { title } = card(context("push"));
    // Act
    frames(200);
    // Assert
    expect([onScreen(title), title.hasAttribute(TITLE_REVEALED_ATTRIBUTE)]).toEqual([COMMAND.length, false]);
  });

  it("carries a part-shown title's length on the title", () => {
    // Arrange
    const { title } = card(context("push"));
    // Act
    frames(10);
    // Assert
    expect(Number(title.getAttribute(TITLE_REVEALED_ATTRIBUTE))).toBe(onScreen(title));
  });

  it("resumes a redraw from what the previous draw's title showed", () => {
    // Arrange
    const first = card(context("push"));
    frames(10);
    const reached = onScreen(first.title);
    // Act
    const again = card(context("push", { previous: first.card }));
    // Assert
    expect(onScreen(again.title)).toBe(reached);
  });

  it("keeps a redraw whole when the previous draw's title was whole", () => {
    // Arrange
    const first = card(context("push"));
    frames(200);
    // Act
    const again = card(context("push", { previous: first.card }));
    frames(1);
    // Assert
    expect(onScreen(again.title)).toBe(COMMAND.length);
  });

  it("keeps a bubble head's redraw resuming when the previous head is the title", () => {
    // Arrange: a head whose own element is the title.
    const first = card(context("push"));
    frames(10);
    const reached = onScreen(first.title);
    // Act
    const again = card(context("push", { previous: first.title }));
    // Assert
    expect(onScreen(again.title)).toBe(reached);
  });

  it("shows the rest whole once the collapsed title overflows its line", () => {
    // Arrange
    const { title } = card(context("push"), true);
    // Act
    frames(10);
    // Assert
    expect(onScreen(title)).toBe(COMMAND.length);
  });

  it("types an expanded card's overflowing title to the end", () => {
    // Arrange
    const built = card(context("push"), true);
    built.card.classList.add("expanded");
    // Act
    frames(10);
    // Assert
    expect(onScreen(built.title) < COMMAND.length).toBe(true);
  });

  it("shows a pushed row's title whole on an unseen page", () => {
    // Arrange, Act
    const { title } = card(context("push", { seen: false }));
    frames(1);
    // Assert
    expect(onScreen(title)).toBe(COMMAND.length);
  });

  it("keeps the title's whole text for every reader of it while part shown", () => {
    // Arrange
    const { title } = card(context("push"));
    // Act
    frames(10);
    // Assert
    expect(fullTextOf(title)).toBe(COMMAND);
  });

  it.each([
    { name: "an unseen page", seen: false, overflow: false, suffix: "unseen" },
    { name: "a full collapsed title", seen: true, overflow: true, suffix: "collapsed-full" },
  ])("records drawing whole on $name with the row", async ({ seen, overflow, suffix }) => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    card(context("push", { seen }), overflow);
    frames(1);
    // Assert
    const record = await forwardedRecord(capture, `feed.title-reveal.${suffix}`);
    expect([record.level.case, record.context?.row]).toEqual(["debug", "row-1"]);
  });
});
