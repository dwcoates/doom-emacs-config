// @vitest-environment jsdom
/**
 * THE RECENTLY MERGED BAND'S PAGES, LOADED AND LET GO FOR REAL
 * (src/sidebar/merged-pages.ts; docs/protobuf-design/sidebar-roster-streams.md,
 * sections 2 and 7): what is fetched as the band folds and the reader
 * scrolls, and the draw rule that holds a push until its pages agree with its
 * summary.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  ListMergedWorkspacesResponseSchema,
  type ListMergedWorkspacesRequestSchema,
  type ListMergedWorkspacesResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_list_merged_workspaces_pb";
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { AgentReplClient } from "../../src/rpc/client.js";
import type { UnaryContext } from "../../src/rpc/unary.js";
import { RosterMergedRowSchema, type RosterMergedRepoGroup, type RosterMergedRow, type RosterMergedSection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { WALK_LOADING_ATTRIBUTE, WALK_SPINNER_CLASS } from "../../src/feed/walk-control.js";
import { resetLoggingForTests } from "../../src/log.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  DEPLOYS_GROUP_CLASS,
  DEPLOYS_REPOSITORY_ATTRIBUTE,
  DEPLOYS_SECTION_ATTRIBUTE,
} from "../../src/sidebar/deploys-fold.js";
import { FOLDED_CLASS } from "../../src/sidebar/folded-class.js";
import {
  MERGED_MORE_ATTRIBUTE,
  MERGED_MORE_CLASS,
  MERGED_PAGE_CLASS,
  MERGED_PAGE_SIZE,
  MERGED_PAGE_TIMEOUT_MS,
  createMergedPages,
  drawMergedMoreSpinner,
  inView,
  mergedRowAtMs,
  type MergedPages,
} from "../../src/sidebar/merged-pages.js";
import { captureLogRecords, forwardedRecord, forwardedRecords } from "../log-capture.js";
import { SINK, appContext, mergedBand, mergedGroup, mergedPageAnswer, mergedRowsOf, row } from "./harness.js";

afterEach(() => {
  resetLoggingForTests();
});

/** Stub EL's rectangle as spanning TOP to BOTTOM. */
function placeAt(el: HTMLElement, top: number, bottom: number): void {
  el.getBoundingClientRect = () => new DOMRect(0, top, 0, bottom - top);
}

/** One ListMergedWorkspaces the daemon has been asked and not yet answered. */
interface Ask {
  repository: string;
  page: number;
  order: string | undefined;
  timeoutMs: number | undefined;
  answer(response: ListMergedWorkspacesResponse): void;
  fail(err: Error): void;
}

/** A group of repository ID holding N merged rows, ws-<id>-0 first. */
function groupOf(id: string, n: number): RosterMergedRepoGroup {
  return mergedGroup({ id, rows: Array.from({ length: n }, (_, i) => row({ id: `ws-${id}-${i}`, closed: true })) });
}

/**
 * Merged pages over a daemon whose every ListMergedWorkspaces waits for the
 * test's answer; FOLDED says which groups the page has folded. `changed()`
 * resolves the next time the pages tell the rail they changed.
 */
function harness(folded: Set<string> = new Set()) {
  const asks: Ask[] = [];
  let woken: (() => void) | null = null;
  let changes = 0;
  const root = document.createElement("div");
  let clock = 0;
  // THE ASK IS RECORDED AS IT IS MADE: the client stands in for the daemon
  // at the call itself, so a test reads what was asked with no wait.
  const client: AgentReplClient = {
    ...appContext().client,
    listMergedWorkspaces: (request: MessageInitShape<typeof ListMergedWorkspacesRequestSchema>, options?: { timeoutMs?: number }) =>
      new Promise<ListMergedWorkspacesResponse>((resolve, reject) => {
        const order = request.order?.order;
        asks.push({
          repository: request.repository?.id ?? "",
          page: request.page ?? 0,
          order: order?.case,
          timeoutMs: options?.timeoutMs,
          answer: resolve,
          fail: reject,
        });
      }),
  };
  const ctx: UnaryContext = { client, failures: SINK, isQuiesced: () => false };
  const pages = createMergedPages({
    ctx,
    root,
    folded: (repository) => folded.has(repository),
    changed: () => {
      changes += 1;
      woken?.();
      woken = null;
    },
    now: () => clock,
  });
  return {
    pages,
    asks,
    root,
    changes: () => changes,
    advance: (ms: number) => {
      clock += ms;
    },
    /** Resolves once the pages next say they changed. */
    changed: () =>
      new Promise<void>((resolve) => {
        woken = resolve;
      }),
    /** Answer the ask for REPOSITORY's PAGE with ROWS at TOKEN, and wait until it is loaded. */
    async answer(repository: string, page: number, rows: readonly RosterMergedRow[], token: bigint): Promise<void> {
      const ask = asks.find((a) => a.repository === repository && a.page === page);
      if (ask === undefined) throw new Error(`nothing asked for ${repository} page ${page}`);
      asks.splice(asks.indexOf(ask), 1);
      const landed = this.changed();
      ask.answer(mergedPageAnswer(rows, page, token));
      await landed;
    },
  };
}

/**
 * Every answer already given has run through the in-memory transport: its
 * chain is promises alone, which all settle before the next macrotask.
 */
const everyAnswerSettled = (): Promise<void> => new Promise((resolve) => globalThis.setTimeout(resolve, 0));

/** The cause a fetch-failed RECORD states, which is always a string. */
function causeOf(record: { context?: Record<string, unknown> }): string {
  const cause = record.context?.cause;
  if (typeof cause !== "string") throw new Error("the record states no cause");
  return cause;
}

/** Which (repository, page) pairs are being asked for. */
const asked = (asks: readonly Ask[]): Array<[string, number]> => asks.map((a) => [a.repository, a.page]);

/** The rows loaded for GROUP, by workspace id. */
const loadedIds = (pages: MergedPages, group: RosterMergedRepoGroup): string[][] =>
  pages.loaded(group, "g").pages.map((page) => page.map((m) => m.row?.workspace?.workspace?.id ?? ""));

/** An open band of GROUPS at TOKEN. */
const openBand = (groups: RosterMergedRepoGroup[], token = 1n): RosterMergedSection => mergedBand(groups, false, null, token);

/**
 * GROUP's drawing in ROOT, as roster.ts draws it: in a Recently Merged
 * viewport spanning 0 to 100, its first page in its body, each later page in
 * its own box, and the spinner at its end.
 */
function drawGroup(root: HTMLElement, repository: string, laterPages: number, spinner: boolean) {
  const section = document.createElement("div");
  section.className = "repo merged-section";
  const region = document.createElement("div");
  region.className = "rows";
  const group = document.createElement("div");
  group.className = DEPLOYS_GROUP_CLASS;
  group.setAttribute(DEPLOYS_SECTION_ATTRIBUTE, "recently-merged");
  group.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, repository);
  const body = document.createElement("div");
  const pageBoxes = Array.from({ length: laterPages }, () => {
    const box = document.createElement("div");
    box.className = MERGED_PAGE_CLASS;
    return box;
  });
  body.append(...pageBoxes);
  const end = spinner ? drawMergedMoreSpinner() : null;
  if (end !== null) body.append(end);
  group.appendChild(body);
  region.appendChild(group);
  section.appendChild(region);
  root.replaceChildren(section);
  placeAt(region, 0, 100);
  return { group, pageBoxes, spinner: end, region };
}

describe("inView", () => {
  it.each([
    { name: "inside the viewport", top: 20, bottom: 40, want: true },
    { name: "straddling the viewport's foot", top: 90, bottom: 110, want: true },
    { name: "wholly below the viewport", top: 100, bottom: 120, want: false },
    { name: "wholly above the viewport", top: -20, bottom: 0, want: false },
    { name: "drawn as nothing", top: 50, bottom: 50, want: false },
  ])("answers $want for an element $name", ({ top, bottom, want }) => {
    // Arrange
    const region = document.createElement("div");
    placeAt(region, 0, 100);
    const el = document.createElement("div");
    placeAt(el, top, bottom);
    // Act + Assert
    expect(inView(el, region)).toBe(want);
  });
});

describe("drawMergedMoreSpinner", () => {
  it("draws the feed's loading spinner, turning", () => {
    // Act
    const spinner = drawMergedMoreSpinner();
    // Assert
    expect([
      spinner.classList.contains(MERGED_MORE_CLASS),
      spinner.hasAttribute(MERGED_MORE_ATTRIBUTE),
      spinner.hasAttribute(WALK_LOADING_ATTRIBUTE),
      spinner.querySelector(`.${WALK_SPINNER_CLASS}`) !== null,
    ]).toEqual([true, true, true, true]);
  });
});

describe("mergedRowAtMs", () => {
  it("reads a merged row's instant", () => {
    const merged = create(RosterMergedRowSchema, { row: row({ id: "ws-1", when: { case: "merged", value: { atMs: 42n } } }) });
    expect(mergedRowAtMs(merged, "p")).toBe(42);
  });

  it("refuses a row on another when arm than merged as a malformed view", () => {
    const merged = create(RosterMergedRowSchema, { row: row({ id: "ws-1" }) });
    expect(() => mergedRowAtMs(merged, "p")).toThrow(MalformedView);
  });
});

describe("loading as the band folds and opens", () => {
  it("loads nothing while the section is folded", () => {
    // Arrange
    const h = harness();
    // Act
    const verdict = h.pages.agree(mergedBand([groupOf("r1", 3)], true), "s");
    // Assert
    expect([verdict, h.asks.length]).toEqual(["draw", 0]);
  });

  it("loads nothing while the band is not drawn", () => {
    const h = harness();
    expect([h.pages.agree(null, "s"), h.asks.length]).toEqual(["draw", 0]);
  });

  it("fetches each group's page 0 once the section is unfolded", () => {
    // Arrange
    const h = harness();
    // Act
    h.pages.agree(openBand([groupOf("r1", 3), groupOf("r2", 3)]), "s");
    // Assert
    expect(asked(h.asks)).toEqual([
      ["r1", 0],
      ["r2", 0],
    ]);
  });

  it("asks for the group's own repository, most recently merged first", () => {
    const h = harness();
    h.pages.agree(openBand([groupOf("r1", 3)]), "s");
    expect([h.asks[0].repository, h.asks[0].order]).toEqual(["r1", "mostRecentlyMerged"]);
  });

  it("asks for every page under the measured bound", () => {
    // Arrange
    const h = harness();
    // Act
    h.pages.agree(openBand([groupOf("r1", 3)]), "s");
    // Assert
    expect(h.asks[0].timeoutMs).toBe(MERGED_PAGE_TIMEOUT_MS);
  });

  it("asks for a group's page 0 once, however often the band is drawn", () => {
    const h = harness();
    const band = openBand([groupOf("r1", 3)]);
    h.pages.agree(band, "s");
    h.pages.agree(band, "s");
    expect(asked(h.asks)).toEqual([["r1", 0]]);
  });

  it("answers that more is coming while a group's page 0 is still being fetched", () => {
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    expect(h.pages.loaded(group, "g")).toEqual({ pages: [], more: true, rows: 0 });
  });

  it("holds a group's page once it lands, and tells the rail", async () => {
    // Arrange
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    const before = h.pages.version();
    // Act
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    // Assert
    expect([loadedIds(h.pages, group), h.pages.version() > before, h.changes()]).toEqual([[["ws-r1-0", "ws-r1-1", "ws-r1-2"]], true, 1]);
  });

  it("lets every loaded page go when the section folds", async () => {
    // Arrange
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    // Act
    h.pages.agree(mergedBand([group], true), "s");
    // Assert
    expect(h.pages.loaded(group, "g").rows).toBe(0);
  });

  it("drops a page that lands after the section folded", async () => {
    // Arrange
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    const [ask] = h.asks;
    h.pages.agree(mergedBand([group], true), "s");
    const capture = captureLogRecords("debug");
    // Act
    ask.answer(mergedPageAnswer(mergedRowsOf(group), 0, 1n));
    await everyAnswerSettled();
    // Assert
    const records = await forwardedRecords(capture);
    expect([h.pages.loaded(group, "g").rows, records.some((r) => r.operation === "sidebar.merged-pages.answer-dropped")]).toEqual([0, true]);
  });

  it("lets go of a group's pages once it leaves the summary", async () => {
    const h = harness();
    const gone = groupOf("r1", 3);
    h.pages.agree(openBand([gone]), "s");
    await h.answer("r1", 0, mergedRowsOf(gone), 1n);
    h.pages.agree(openBand([groupOf("r2", 1)], 2n), "s");
    expect(h.pages.loaded(gone, "g").rows).toBe(0);
  });

  it("refuses a summary that names no set of merged workspaces as a malformed view", () => {
    const h = harness();
    const band = openBand([groupOf("r1", 3)]);
    band.mergedRevision = undefined;
    expect(() => h.pages.agree(band, "s")).toThrow(MalformedView);
  });
});

describe("loading as a group folds and opens on the page", () => {
  it("fetches nothing for a group folded on the page", () => {
    const h = harness(new Set(["r1"]));
    h.pages.agree(openBand([groupOf("r1", 3), groupOf("r2", 3)]), "s");
    expect(asked(h.asks)).toEqual([["r2", 0]]);
  });

  it("lets a group's pages go once it is folded on the page", async () => {
    // Arrange
    const folded = new Set<string>();
    const h = harness(folded);
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    // Act
    folded.add("r1");
    h.pages.foldsChanged();
    // Assert
    expect([h.pages.loaded(group, "g").rows, h.changes()]).toEqual([0, 2]);
  });

  it("fetches a group's page 0 again once it is opened", async () => {
    // Arrange
    const folded = new Set<string>();
    const h = harness(folded);
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    folded.add("r1");
    h.pages.foldsChanged();
    // Act
    folded.delete("r1");
    h.pages.foldsChanged();
    // Assert
    expect(asked(h.asks)).toEqual([["r1", 0]]);
  });
});

describe("loading as the reader scrolls", () => {
  /** A band whose group r1 holds N merges with page 0 loaded, drawn with LATER page boxes and a spinner. */
  async function loadedFirstPage(n: number) {
    const h = harness();
    const group = groupOf("r1", n);
    h.pages.agree(openBand([group]), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    return { h, group };
  }

  it("fetches a group's next page once its end is scrolled into view while its last answer was more", async () => {
    // Arrange
    const { h } = await loadedFirstPage(MERGED_PAGE_SIZE + 5);
    const { spinner } = drawGroup(h.root, "r1", 0, true);
    placeAt(spinner!, 80, 90);
    // Act
    h.pages.check();
    // Assert
    expect(asked(h.asks)).toEqual([["r1", 1]]);
  });

  it("holds the next page after the first once it lands", async () => {
    // Arrange
    const { h, group } = await loadedFirstPage(MERGED_PAGE_SIZE + 5);
    const { spinner } = drawGroup(h.root, "r1", 0, true);
    placeAt(spinner!, 80, 90);
    h.pages.check();
    // Act
    await h.answer("r1", 1, mergedRowsOf(group), 1n);
    // Assert
    expect(h.pages.loaded(group, "g").pages.map((page) => page.length)).toEqual([MERGED_PAGE_SIZE, 5]);
  });

  it("fetches no further page once the last answer was last", async () => {
    // Arrange
    const { h, group } = await loadedFirstPage(MERGED_PAGE_SIZE + 5);
    const first = drawGroup(h.root, "r1", 0, true);
    placeAt(first.spinner!, 80, 90);
    h.pages.check();
    await h.answer("r1", 1, mergedRowsOf(group), 1n);
    // Act: the spinner is drawn no more, but one left in view asks nothing either.
    const again = drawGroup(h.root, "r1", 1, true);
    placeAt(again.pageBoxes[0], 50, 80);
    placeAt(again.spinner!, 80, 90);
    h.pages.check();
    // Assert
    expect([h.asks.length, h.pages.loaded(group, "g").more]).toEqual([0, false]);
  });

  it("fetches nothing while the group's end is below the viewport", async () => {
    const { h } = await loadedFirstPage(MERGED_PAGE_SIZE + 5);
    const { spinner } = drawGroup(h.root, "r1", 0, true);
    placeAt(spinner!, 150, 160);
    h.pages.check();
    expect(h.asks.length).toBe(0);
  });

  it("asks for the next page once, however often its end is looked at", async () => {
    const { h } = await loadedFirstPage(MERGED_PAGE_SIZE + 5);
    const { spinner } = drawGroup(h.root, "r1", 0, true);
    placeAt(spinner!, 80, 90);
    h.pages.check();
    h.pages.check();
    expect(asked(h.asks)).toEqual([["r1", 1]]);
  });

  /** A band whose group r1 holds three pages, every one loaded and drawn. */
  async function threePages() {
    const { h, group } = await loadedFirstPage(MERGED_PAGE_SIZE * 3);
    for (const page of [1, 2]) {
      const { spinner } = drawGroup(h.root, "r1", page - 1, true);
      placeAt(spinner!, 80, 90);
      h.pages.check();
      await h.answer("r1", page, mergedRowsOf(group), 1n);
    }
    return { h, group, drawn: drawGroup(h.root, "r1", 2, false) };
  }

  it("lets the last page go first once it lies wholly below the viewport", async () => {
    // Arrange
    const { h, group, drawn } = await threePages();
    placeAt(drawn.pageBoxes[0], 60, 99);
    placeAt(drawn.pageBoxes[1], 100, 140);
    // Act
    h.pages.check();
    // Assert
    expect(h.pages.loaded(group, "g").pages.length).toBe(2);
  });

  it("lets every later page below the viewport go, the last first", async () => {
    const { h, group, drawn } = await threePages();
    placeAt(drawn.pageBoxes[0], 100, 140);
    placeAt(drawn.pageBoxes[1], 140, 180);
    h.pages.check();
    expect([h.pages.loaded(group, "g").pages.length, h.pages.loaded(group, "g").more]).toEqual([1, true]);
  });

  it("keeps a page that still shows a sliver", async () => {
    const { h, group, drawn } = await threePages();
    placeAt(drawn.pageBoxes[0], 60, 90);
    placeAt(drawn.pageBoxes[1], 90, 130);
    h.pages.check();
    expect(h.pages.loaded(group, "g").pages.length).toBe(3);
  });

  it("keeps a group's first page however far below the viewport the group lies", async () => {
    const { h, group } = await loadedFirstPage(MERGED_PAGE_SIZE);
    const { group: drawn } = drawGroup(h.root, "r1", 0, false);
    placeAt(drawn, 300, 400);
    h.pages.check();
    expect(h.pages.loaded(group, "g").pages.length).toBe(1);
  });

  it("looks at no page of a group folded on the page", async () => {
    const { h, group, drawn } = await threePages();
    drawn.group.classList.add(FOLDED_CLASS);
    placeAt(drawn.pageBoxes[0], 100, 140);
    placeAt(drawn.pageBoxes[1], 140, 180);
    h.pages.check();
    expect(h.pages.loaded(group, "g").pages.length).toBe(3);
  });

  it("refuses a group hanging in no Recently Merged viewport, logging why", async () => {
    // Arrange
    const { h } = await loadedFirstPage(MERGED_PAGE_SIZE);
    const { group } = drawGroup(h.root, "r1", 0, true);
    h.root.replaceChildren(group);
    const capture = captureLogRecords("error");
    // Act + Assert
    expect(() => h.pages.check()).toThrow(/hangs in no Recently Merged viewport/);
    const record = await forwardedRecord(capture, "sidebar.merged-pages.malformed");
    expect(record.context).toMatchObject({ repository: "r1" });
  });
});

describe("the draw rule: pages are drawn only beside the summary they agree with", () => {
  /** A band of group r1 (N merges) whose page 0 loaded at token 1. */
  async function loadedAt1(n = 3) {
    const h = harness();
    const group = groupOf("r1", n);
    h.pages.agree(openBand([group], 1n), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    return { h, group };
  }

  it("draws while every loaded page carries the summary's token", async () => {
    const { h, group } = await loadedAt1();
    expect(h.pages.agree(openBand([group], 1n), "s")).toBe("draw");
  });

  it("holds a push that moves the token while pages are loaded", async () => {
    const { h, group } = await loadedAt1();
    expect(h.pages.agree(openBand([group], 2n), "s")).toBe("hold");
  });

  it("fetches the loaded pages again when a push moves the token", async () => {
    const { h, group } = await loadedAt1();
    h.pages.agree(openBand([group], 2n), "s");
    expect(asked(h.asks)).toEqual([["r1", 0]]);
  });

  it("keeps the pages it holds unchanged while the push is held", async () => {
    const { h, group } = await loadedAt1();
    h.pages.agree(openBand([group], 2n), "s");
    expect(loadedIds(h.pages, group)).toEqual([["ws-r1-0", "ws-r1-1", "ws-r1-2"]]);
  });

  it("draws the held push and the pages together once they agree", async () => {
    // Arrange: a merge landed, so the set at token 2 holds one more.
    const { h, group } = await loadedAt1();
    const grown = groupOf("r1", 4);
    h.pages.agree(openBand([grown], 2n), "s");
    // Act
    await h.answer("r1", 0, mergedRowsOf(grown), 2n);
    // Assert
    expect([h.pages.agree(openBand([grown], 2n), "s"), loadedIds(h.pages, group)[0].length]).toEqual(["draw", 4]);
  });

  it("keeps a page newer than the summary waiting for the push that carries its set", async () => {
    // Arrange: page 0 asked for at token 1 answers the set of token 2.
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group], 1n), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 2n);
    // Act
    const verdict = h.pages.agree(openBand([group], 1n), "s");
    // Assert: held, and nothing fetched again.
    expect([verdict, h.asks.length]).toEqual(["hold", 0]);
  });

  it("draws the newer page once the push carrying its set arrives", async () => {
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group], 1n), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 2n);
    h.pages.agree(openBand([group], 1n), "s");
    expect(h.pages.agree(openBand([group], 2n), "s")).toBe("draw");
  });

  it("holds nothing while the section is folded, whatever the token", async () => {
    const { h, group } = await loadedAt1();
    expect([h.pages.agree(mergedBand([group], true, null, 2n), "s"), h.asks.length]).toEqual(["draw", 0]);
  });

  it("logs the hold with the summary's token and every page that disagrees", async () => {
    // Arrange
    const { h, group } = await loadedAt1();
    const capture = captureLogRecords("info");
    // Act
    h.pages.agree(openBand([group], 2n), "s");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.merged-pages.hold");
    expect([record.level.case, record.context]).toEqual([
      "info",
      { connection_id: "test-connection", summary_token: "2", pages: [{ repository: "r1", page: 0, token: "1" }] },
    ]);
  });

  it("logs how long it held the push once the pages agree", async () => {
    // Arrange
    const { h, group } = await loadedAt1();
    h.pages.agree(openBand([group], 2n), "s");
    h.advance(35);
    await h.answer("r1", 0, mergedRowsOf(group), 2n);
    const capture = captureLogRecords("info");
    // Act
    h.pages.agree(openBand([group], 2n), "s");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.merged-pages.hold-released");
    expect(record.context).toEqual({ connection_id: "test-connection", held_ms: 35, outcome: "agreed" });
  });
});

describe("a page that cannot be loaded", () => {
  /** A held push: group r1's page 0 loaded at token 1, the summary moved to 2, the refetch asked. */
  async function held() {
    const h = harness();
    const group = groupOf("r1", 3);
    const other = groupOf("r2", 3);
    h.pages.agree(openBand([group, other], 1n), "s");
    await h.answer("r1", 0, mergedRowsOf(group), 1n);
    await h.answer("r2", 0, mergedRowsOf(other), 1n);
    h.pages.agree(openBand([group, other], 2n), "s");
    return { h, group, other };
  }

  /** Fail the ask for R1's page 0 with ERR, and wait for the pages to let go. */
  async function failWith(h: ReturnType<typeof harness>, err: Error): Promise<void> {
    const ask = h.asks.find((a) => a.repository === "r1" && a.page === 0)!;
    const released = h.changed();
    ask.fail(err);
    await released;
  }

  it("logs a failed fetch with its repository, page, cause and tokens", async () => {
    // Arrange
    const { h } = await held();
    const capture = captureLogRecords("error");
    // Act
    await failWith(h, new ConnectError("the daemon went away", Code.Unavailable));
    // Assert
    const record = await forwardedRecord(capture, "sidebar.merged-pages.fetch-failed");
    expect([record.level.case, record.context]).toEqual([
      "error",
      {
        connection_id: "test-connection",
        repository: "r1",
        page: 0,
        kind: "transport",
        cause: "ConnectError: [unavailable] the daemon went away",
        asked_for: "2",
        summary_token: "2",
        held: true,
        bound_ms: MERGED_PAGE_TIMEOUT_MS,
      },
    ]);
  });

  it("logs a fetch that outlived its bound as a failure naming the bound", async () => {
    // Arrange
    const { h } = await held();
    const capture = captureLogRecords("error");
    // Act
    await failWith(h, new ConnectError("the operation timed out", Code.DeadlineExceeded));
    // Assert
    const record = await forwardedRecord(capture, "sidebar.merged-pages.fetch-failed");
    expect([record.context?.cause, record.context?.bound_ms]).toEqual([
      "ConnectError: [deadline_exceeded] the operation timed out",
      MERGED_PAGE_TIMEOUT_MS,
    ]);
  });

  it("lets every group's pages go, so the held push is drawn without them", async () => {
    // Arrange
    const { h, group, other } = await held();
    await failWith(h, new ConnectError("the daemon went away", Code.Unavailable));
    // Act
    const verdict = h.pages.agree(openBand([group, other], 2n), "s");
    // Assert
    expect([verdict, h.pages.loaded(group, "g"), h.pages.loaded(other, "g")]).toEqual([
      "draw",
      { pages: [], more: false, rows: 0 },
      { pages: [], more: false, rows: 0 },
    ]);
  });

  it("logs the held push drawn as released", async () => {
    const { h, group, other } = await held();
    await failWith(h, new ConnectError("the daemon went away", Code.Unavailable));
    const capture = captureLogRecords("info");
    h.pages.agree(openBand([group, other], 2n), "s");
    const record = await forwardedRecord(capture, "sidebar.merged-pages.hold-released");
    expect(record.context).toMatchObject({ outcome: "released" });
  });

  it("fetches nothing on its own after a failure while the token stands", async () => {
    const { h, group, other } = await held();
    await failWith(h, new ConnectError("the daemon went away", Code.Unavailable));
    h.asks.splice(0);
    h.pages.agree(openBand([group, other], 2n), "s");
    expect(h.asks.length).toBe(0);
  });

  it("fetches each group's page 0 again once the token moves after a failure", async () => {
    const { h, group, other } = await held();
    await failWith(h, new ConnectError("the daemon went away", Code.Unavailable));
    h.asks.splice(0);
    h.pages.agree(openBand([group, other], 3n), "s");
    expect(asked(h.asks)).toEqual([
      ["r1", 0],
      ["r2", 0],
    ]);
  });

  it("fetches again after a failure once a group is folded or opened by hand", async () => {
    const { h, group, other } = await held();
    await failWith(h, new ConnectError("the daemon went away", Code.Unavailable));
    h.asks.splice(0);
    h.pages.agree(openBand([group, other], 2n), "s");
    h.pages.foldsChanged();
    expect(asked(h.asks)).toEqual([
      ["r1", 0],
      ["r2", 0],
    ]);
  });

  /** Answer group r1's first ask with RESPONSE and wait for the pages to let go. */
  async function answerFirstWith(response: ListMergedWorkspacesResponse) {
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group], 1n), "s");
    const capture = captureLogRecords("error");
    const released = h.changed();
    h.asks[0].answer(response);
    await released;
    return { h, group, record: await forwardedRecord(capture, "sidebar.merged-pages.fetch-failed") };
  }

  it("logs a refused page under the daemon's cause", async () => {
    const { record } = await answerFirstWith(
      create(ListMergedWorkspacesResponseSchema, { result: { case: "error", value: { cause: { case: "notReady", value: {} } } } }),
    );
    expect(record.context).toMatchObject({ repository: "r1", page: 0, kind: "notReady" });
  });

  it("logs an answer naming no set of merged workspaces as malformed", async () => {
    const response = mergedPageAnswer([], 0, 1n);
    if (response.result.case === "success") response.result.value.revision = undefined;
    const { record } = await answerFirstWith(response);
    expect([record.context?.kind, causeOf(record).includes("success.revision")]).toEqual(["malformed", true]);
  });

  it("logs an answer holding a row on another when arm than merged as malformed", async () => {
    const { record } = await answerFirstWith(mergedPageAnswer([create(RosterMergedRowSchema, { row: row({ id: "ws-1" }) })], 0, 1n));
    expect([record.context?.kind, causeOf(record).includes("row.when.shown")]).toEqual(["malformed", true]);
  });

  it("loads nothing of a page it could not read", async () => {
    const { h, group } = await answerFirstWith(
      create(ListMergedWorkspacesResponseSchema, { result: { case: "error", value: { cause: { case: "notReady", value: {} } } } }),
    );
    expect(h.pages.loaded(group, "g")).toEqual({ pages: [], more: false, rows: 0 });
  });
});

describe("disposing the pages", () => {
  it("drops an answer that lands after the rail went away", async () => {
    // Arrange
    const h = harness();
    const group = groupOf("r1", 3);
    h.pages.agree(openBand([group]), "s");
    const [ask] = h.asks;
    h.pages.dispose();
    // Act
    ask.answer(mergedPageAnswer(mergedRowsOf(group), 0, 1n));
    await everyAnswerSettled();
    // Assert
    expect([h.changes(), h.pages.loaded(group, "g").rows]).toEqual([0, 0]);
  });
});
