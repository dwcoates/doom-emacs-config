// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { SelectFeedRowResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  CLEAR_REQUEST,
  installBackgroundClear,
  isFeedBackground,
} from "../../src/feed/background-click.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { harness, type FeedScript } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
  document.getSelection()?.removeAllRanges();
});

async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/**
 * The page's scroll zone as the shell and the feed lay it out: the scroll box,
 * the feed host and the hold tray inside it, one root row wrapping a bubble
 * (with text and a control in it), one root row wrapping a tool card, a
 * load-more control straight in the feed host, a bubble's sub-feed row, and a
 * held card in the tray.
 */
function zone() {
  const box = document.createElement("div");
  box.id = "feed-scroll";
  box.innerHTML = `
    <main id="feed" data-feed="root">
      <ar-button role="button" class="load-more">load more</ar-button>
      <article class="feed-item" data-feed-row="r1">
        <div class="bubble" data-role="response">
          <div class="bubble-scroll"><div class="bubble-body"><p><span id="prose">words</span></p></div></div>
          <div class="bubble-subfeed"><article class="feed-item" data-feed-row="n1"></article></div>
        </div>
      </article>
      <article class="feed-item" data-feed-row="t1">
        <div class="tool-card"><ar-button role="button" id="card-control">stop</ar-button></div>
      </article>
    </main>
    <section id="hold-tray"><div class="hold-tray"><article data-held-turn="h1"></article></div></section>`;
  const outside = document.createElement("div");
  document.body.replaceChildren(box, outside);
  const at = (selector: string): Element => {
    const el = box.querySelector(selector);
    if (el === null) throw new Error(`the zone draws no ${selector}`);
    return el;
  };
  return { box, outside, at };
}

describe("isFeedBackground", () => {
  const cases: ReadonlyArray<{ name: string; pick: (z: ReturnType<typeof zone>) => EventTarget | null; want: boolean }> = [
    { name: "the scroll box itself", pick: (z) => z.box, want: true },
    { name: "the feed host, between rows", pick: (z) => z.at("#feed"), want: true },
    { name: "the hold tray host", pick: (z) => z.at("#hold-tray"), want: true },
    { name: "a root row's wrapper, beside its bubble", pick: (z) => z.at('[data-feed-row="r1"]'), want: true },
    { name: "a bubble", pick: (z) => z.at(".bubble"), want: false },
    { name: "the prose inside a bubble", pick: (z) => z.at("#prose"), want: false },
    { name: "a tool card", pick: (z) => z.at(".tool-card"), want: false },
    { name: "a control inside a card", pick: (z) => z.at("#card-control"), want: false },
    { name: "a control standing in the feed host", pick: (z) => z.at(".load-more"), want: false },
    { name: "a row nested in a bubble's sub-feed", pick: (z) => z.at('[data-feed-row="n1"]'), want: false },
    { name: "a held card in the tray", pick: (z) => z.at('[data-held-turn="h1"]'), want: false },
    { name: "an element outside the scroll box", pick: (z) => z.outside, want: false },
    { name: "no target at all", pick: () => null, want: false },
  ];

  for (const c of cases) {
    it(`answers ${c.want} for ${c.name}`, () => {
      // Arrange
      const z = zone();
      // Act
      const hit = isFeedBackground(c.pick(z), z.box);
      // Assert
      expect(hit).toBe(c.want);
    });
  }
});

describe("installBackgroundClear", () => {
  /** An armed zone, with the selection ACTIVE unless told otherwise. */
  function armed(script: FeedScript = {}, active = true) {
    const z = zone();
    const h = harness(script);
    const reported: FailureKind[] = [];
    vi.spyOn(h.sink, "report").mockImplementation((kind) => {
      reported.push(kind);
    });
    const uninstall = installBackgroundClear(z.box, h.ctx, () => active);
    return { ...z, h, reported, uninstall };
  }

  const click = (el: Element): void => {
    el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  };

  it("asks the daemon to clear the selection on a background click", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow.map((r) => r.move.case)).toEqual(["clear"]);
  });

  it("addresses the clear to this page's workspace", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.box);
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow[0]?.workspace?.id).toBe("ws-1");
  });

  it("sends nothing for a click on a bubble", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("sends nothing for a click on a control", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at(".load-more"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("sends nothing while no row is selected", async () => {
    // Arrange
    const a = armed({}, false);
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("sends nothing for a click that ends a text drag", async () => {
    // Arrange
    const a = armed();
    const range = document.createRange();
    range.selectNodeContents(a.at("#prose"));
    document.getSelection()?.addRange(range);
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("sends nothing once uninstalled", async () => {
    // Arrange
    const a = armed();
    a.uninstall();
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("logs the click-to-clear at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const a = armed();
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "feed.background-click-clear");
    expect(record.level.case).toBe("info");
  });

  it("files nothing on the chip when the daemon clears", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    expect(a.reported).toEqual([]);
  });

  it("names a refused clear on the warning chip as the clear", async () => {
    // Arrange
    const a = armed({
      selectFeedRow: () =>
        create(SelectFeedRowResponseSchema, {
          result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
        }),
    });
    // Act
    click(a.at("#feed"));
    await settle();
    // Assert
    const kind = a.reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.what : kind?.case).toBe(CLEAR_REQUEST);
  });
});
