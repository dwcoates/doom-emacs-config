// @vitest-environment jsdom
import { SELECT_WORKSPACE_EXPECTED_ARMS } from "../../../src/rpc/refusal.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  SelectWorkspaceErrorSchema,
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
  type SelectWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  UpdateMergeQueueResponseSchema,
  type UpdateMergeQueueRequest,
  type UpdateMergeQueueResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import {
  FeedMergeQueueSchema,
  FeedMergeQueueBubbleSchema,
  FeedMergeQueueEntrySchema,
  type FeedMergeQueue,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { type AppContext } from "../../../src/rpc/context.js";
import { scriptedAppContext, testAppContext } from "../../rpc/app-context.js";
import {
  createFeedMergeQueueTable,
  dressQueueGhost,
  drawFeedMergeQueueEntry,
  QUEUE_DRAG_KIND,
  QUEUE_PAUSED_TEXT,
  QUEUE_STATUS_CLASS,
  queueEntryDrag,
  selectQueueEntryWorkspace,
  workspaceHeaderText,
  type FeedMergeQueueTable,
} from "../../../src/feed/merge/queue.js";
import { PAUSED_MARK_CLASS } from "../../../src/feed/merge/paused.js";
import { DRAG_SLOT_CLASS } from "../../../src/drag.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { boxRect } from "../../dom-rect.js";
import { primaryPointer, settleOwedDragClick } from "../../drag-pointer.js";
import { RecordingSink, WORKSPACE, mergeRow, rowContext } from "../harness.js";
import { oneofArms } from "../../arms.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW_MS);
});
afterEach(() => {
  vi.useRealTimers();
  settleOwedDragClick();
  document.body.replaceChildren();
});

/** Every SelectWorkspace this scripted daemon received. */
interface Scripted {
  ctx: AppContext;
  calls: SelectWorkspaceRequest[];
}

/** A context whose SelectWorkspace answers ANSWER. */
function scripted(answer: SelectWorkspaceResponse): Scripted {
  const calls: SelectWorkspaceRequest[] = [];
  const ctx = scriptedAppContext(
    {
      selectWorkspace: (req) => {
        calls.push(req);
        return answer;
      },
    },
    { workspace: WORKSPACE, failures: new RecordingSink() },
  );
  return { ctx, calls };
}

/** A success answer. */
const SELECTED = create(SelectWorkspaceResponseSchema, {
  result: { case: "success", value: {} },
});

/** A refusal answering CAUSE. */
function refusedWith(cause: { case: string; value: unknown }): SelectWorkspaceResponse {
  return create(SelectWorkspaceResponseSchema, {
    result: { case: "error", value: { cause } as never },
  });
}

/** The instant the page's clock reads when a snapshot is drawn. */
const NOW_MS = 1_000_000;

/** One entry of a queue snapshot, in its stage since ENTEREDMS. */
function entry(
  name: string,
  status: { case: "merging"; activeTab: { text: string; round: number } } | { case: "waiting" },
  enteredMs: number = NOW_MS - 5_000,
) {
  const stageEnteredAtMs = BigInt(enteredMs);
  return create(FeedMergeQueueEntrySchema, {
    workspace: { ref: create(WorkspaceRefSchema, { id: name, dir: `/w/${name}` }) },
    label: { text: name },
    target: { text: "doom", dir: "/src/doom" },
    status:
      status.case === "merging"
        ? {
            case: "merging",
            value: {
              activeTab: { words: { case: "text", value: status.activeTab.text }, round: status.activeTab.round },
              stageEnteredAtMs,
            },
          }
        : { case: "waiting", value: { stageEnteredAtMs } },
  });
}

/** A snapshot with one ahead, this workspace, one behind, on a queue PAUSED or not. */
function snapshot(
  frontEnteredMs: number = NOW_MS - 5_000,
  frontTab = { text: "tests", round: 2 },
  paused = false,
): FeedMergeQueue {
  return create(FeedMergeQueueSchema, {
    ahead: [entry("front", { case: "merging", activeTab: frontTab }, frontEnteredMs)],
    current: entry("mine", { case: "waiting" }, NOW_MS - 65_000),
    behind: [entry("after", { case: "waiting" }, NOW_MS - 3_000)],
    ...(paused ? { paused: {} } : {}),
  });
}

/** Draw a snapshot against a scripted daemon. */
function draw(answer: SelectWorkspaceResponse = SELECTED, queue: FeedMergeQueue = snapshot()): {
  el: HTMLElement;
  s: Scripted;
} {
  const s = scripted(answer);
  return { el: drawnTable(queue, rowContext(s.ctx, mergeRow("m1"))).table, s };
}

/** QUEUE drawn as the queue tab draws it, over RC: its panel, and the table in it. */
function drawnTable(queue: FeedMergeQueue, rc: RowContext): { panel: FeedMergeQueueTable; table: HTMLElement } {
  const panel = createFeedMergeQueueTable(rc);
  panel.offer(queue);
  return { panel, table: tableOf(panel) };
}

/** The table PANEL draws now. */
function tableOf(panel: FeedMergeQueueTable): HTMLElement {
  const table = panel.element.querySelector<HTMLElement>(":scope > .merge-queue");
  if (table === null) throw new Error("the queue panel draws no table");
  return table;
}

/** One drawn entry's cells' text, in column order. */
function cellsOf(el: HTMLElement, place: string): string[] {
  const line = el.querySelector(`[data-queue-place="${place}"] .merge-queue-line`);
  return [...(line?.children ?? [])].map((c) => c.textContent ?? "");
}

/** Let the scripted answer land. */
async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("drawFeedMergeQueue: the snapshot's shape is READ, never derived", () => {
  it("draws ahead front-first, then this workspace, then behind nearest-first", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll(".merge-queue-entry")].map((e) =>
        e.getAttribute("data-queue-place"),
      ),
    ).toEqual(["ahead", "current", "behind"]);
  });

  it("marks this workspace's own entry from the field, not by comparing ids", () => {
    const { el } = draw();
    expect(el.querySelector('[data-queue-place="current"] .merge-queue-label')?.textContent).toBe("mine");
  });

  it("labels no row 'you are here'", () => {
    const { el } = draw();
    expect(el.textContent ?? "").not.toContain("you are here");
  });

  it("draws each entry's label verbatim", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll(".merge-queue-label")].map((e) => e.textContent),
    ).toEqual(["front", "mine", "after"]);
  });
});

describe("drawFeedMergeQueue: a table of workspace, target and duration", () => {
  it("leads with a header row naming the three columns", () => {
    const { el } = draw();
    const header = el.firstElementChild;
    expect([
      header?.classList.contains("merge-queue-header"),
      [...(header?.children ?? [])].map((c) => c.getAttribute("data-column")),
    ]).toEqual([true, ["workspace", "target", "duration"]]);
  });

  it("heads the workspace column with how many entries the table holds", () => {
    const { el } = draw();
    expect([...(el.firstElementChild?.children ?? [])].map((c) => c.textContent)).toEqual([
      "workspace (3)",
      "target",
      "duration",
    ]);
  });

  it("counts a lone entry in the workspace header", () => {
    const { el } = draw(SELECTED, create(FeedMergeQueueSchema, { current: entry("mine", { case: "waiting" }) }));
    expect(el.querySelector('[data-column="workspace"]')?.textContent).toBe(workspaceHeaderText(1));
  });

  it("draws no stage column", () => {
    const { el } = draw();
    expect(el.querySelector('.merge-queue-stage, [data-column="stage"]')).toBeNull();
  });

  it("draws every row as the same three cells, in the header's column order", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll(".merge-queue-line")].map((line) =>
        [...line.children].map((c) => c.className),
      ),
    ).toEqual(
      Array.from({ length: 3 }, () => [
        "merge-queue-workspace",
        "merge-queue-target",
        "footer-row-clock merge-queue-duration",
      ]),
    );
  });

  it("takes the shared column table's columns on the header and every row", () => {
    const { el } = draw();
    expect(
      [el.querySelector(".merge-queue-header"), ...el.querySelectorAll(".merge-queue-entry, .merge-queue-line")].every(
        (row) => row?.classList.contains("footer-columns"),
      ),
    ).toBe(true);
  });

  it("marks only this workspace's own row as the current one", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll('.merge-queue-entry[data-queue-place="current"]')].map(
        (e) => e.querySelector(".merge-queue-label")?.textContent,
      ),
    ).toEqual(["mine"]);
  });
});

describe("drawFeedMergeQueueEntry: each entry's duration in its stage", () => {
  it("ticks the front's duration from when it entered its stage", () => {
    const { el } = draw();
    vi.advanceTimersByTime(2_000);
    expect(cellsOf(el, "ahead")[2]).toBe("7s");
  });

  it("ticks a waiting entry's duration from when it was queued", () => {
    const { el } = draw();
    vi.advanceTimersByTime(1_000);
    expect(cellsOf(el, "current")[2]).toBe("1m 6s");
  });

  it("starts over at zero when the front's stage changes", () => {
    const { el: before } = draw();
    vi.advanceTimersByTime(4_000);
    const { el: after } = draw(SELECTED, snapshot(NOW_MS + 4_000, { text: "committing", round: 1 }));
    expect([cellsOf(before, "ahead")[2], cellsOf(after, "ahead")[2]]).toEqual(["9s", "0s"]);
  });
});

describe("drawFeedMergeQueueEntry: the target and each entry's standing", () => {
  it("draws the worktree each entry lands in by its directory name", () => {
    const { el } = draw();
    expect(["ahead", "current", "behind"].map((place) => cellsOf(el, place)[1])).toEqual(["doom", "doom", "doom"]);
  });

  it("names the target worktree's full path on hover", () => {
    const { el } = draw();
    expect(el.querySelector<HTMLElement>('[data-queue-place="ahead"] .merge-queue-target')?.title).toBe("/src/doom");
  });

  it("marks the merge running as merging and every merge behind it as waiting", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll(".merge-queue-entry")].map((e) => e.getAttribute("data-queue-status")),
    ).toEqual(["merging", "waiting", "waiting"]);
  });

  it("refuses an entry carrying no target", () => {
    const s = scripted(SELECTED);
    const bare = entry("x", { case: "waiting" });
    bare.target = undefined;
    expect(() =>
      drawFeedMergeQueueEntry(bare, rowContext(s.ctx, mergeRow("m1")), "behind"),
    ).toThrow(MalformedView);
  });

  it("refuses an entry whose status oneof is unset", () => {
    const s = scripted(SELECTED);
    const bare = create(FeedMergeQueueEntrySchema, {
      workspace: { ref: create(WorkspaceRefSchema, { id: "x", dir: "/x" }) },
      label: { text: "x" },
    });
    expect(() =>
      drawFeedMergeQueueEntry(bare, rowContext(s.ctx, mergeRow("m1")), "behind"),
    ).toThrow(MalformedView);
  });

  it("refuses an entry carrying no workspace ref", () => {
    const s = scripted(SELECTED);
    const bare = create(FeedMergeQueueEntrySchema, {
      label: { text: "x" },
      status: { case: "waiting", value: {} },
    });
    expect(() =>
      drawFeedMergeQueueEntry(bare, rowContext(s.ctx, mergeRow("m1")), "behind"),
    ).toThrow(MalformedView);
  });
});

describe("drawFeedMergeQueue: a paused queue (owner request, 2026-10-23)", () => {
  it("draws the paused banner above the table while the queue is paused", () => {
    const { el } = draw(SELECTED, snapshot(undefined, undefined, true));
    const banner = el.firstElementChild;
    expect([
      banner?.classList.contains("merge-queue-paused"),
      banner?.classList.contains(PAUSED_MARK_CLASS),
      banner?.textContent,
      el.hasAttribute("data-queue-paused"),
    ]).toEqual([true, true, QUEUE_PAUSED_TEXT, true]);
  });

  it("draws no banner while the queue runs", () => {
    const { el } = draw();
    expect([el.querySelector(".merge-queue-paused"), el.hasAttribute("data-queue-paused")]).toEqual([null, false]);
  });
});

describe("the entry click is SelectWorkspace and nothing else (R8)", () => {
  it("echoes the ref the queue served, verbatim", async () => {
    const { el, s } = draw();
    el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]')?.click();
    await settle();
    expect(s.calls.map((c) => [c.workspace?.id, c.workspace?.dir])).toEqual([
      ["front", "/w/front"],
    ]);
  });

  it("draws NOTHING on success — the roster stream carries the new current", async () => {
    const { el } = draw();
    el.querySelector<HTMLElement>('[data-queue-place="behind"] [data-select]')?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });
});

describe("an async merge's entry opens its bubble in the requester (owner ruling, 2026-10-17)", () => {
  /** A snapshot whose one entry ahead is an async merge, carrying its bubble. */
  function asyncSnapshot(row: string | undefined): FeedMergeQueue {
    const front = entry("land-sidebar-palette", { case: "merging", activeTab: { text: "tests", round: 1 } });
    front.bubble = create(FeedMergeQueueBubbleSchema, {
      requester: create(WorkspaceRefSchema, { id: "requester", dir: "/w/requester" }),
      ...(row === undefined ? {} : { row: { value: row } }),
    });
    return create(FeedMergeQueueSchema, { ahead: [front], current: entry("mine", { case: "waiting" }) });
  }

  /** Click the async entry and answer what SelectWorkspace was asked. */
  async function clickAsync(row: string | undefined): Promise<SelectWorkspaceRequest[]> {
    const { el, s } = draw(SELECTED, asyncSnapshot(row));
    el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]')?.click();
    await settle();
    return s.calls;
  }

  it("names the entry for the merge itself", () => {
    const { el } = draw(SELECTED, asyncSnapshot("row-head"));
    expect(el.querySelector('[data-queue-place="ahead"] .merge-queue-label')?.textContent).toBe("land-sidebar-palette");
  });

  it("selects the requester, never the hidden merge workspace", async () => {
    const calls = await clickAsync("row-head");
    expect(calls.map((c) => c.workspace?.id)).toEqual(["requester"]);
  });

  it("asks the requester's page to reveal the merge's bubble", async () => {
    const calls = await clickAsync("row-head");
    expect(calls.map((c) => c.reveal?.value)).toEqual(["row-head"]);
  });

  it("selects the requester alone when the merge has no bubble row", async () => {
    const calls = await clickAsync(undefined);
    expect(calls.map((c) => [c.workspace?.id, c.reveal])).toEqual([["requester", undefined]]);
  });

  it("asks a sync merge's entry to reveal nothing", async () => {
    const { el, s } = draw();
    el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]')?.click();
    await settle();
    expect(s.calls.map((c) => c.reveal)).toEqual([undefined]);
  });
});

describe("every SelectWorkspaceError arm draws at the clicked entry", () => {
  const arms: { arm: string; value: unknown; says: RegExp }[] = [
    { arm: "unknownWorkspace", value: {}, says: /does not know this workspace/ },
    {
      arm: "workspaceRefMismatch",
      value: { registryDir: "/elsewhere" },
      says: /\/elsewhere/,
    },
    {
      arm: "transferringAway",
      value: { address: "127.0.0.1:9" },
      says: /127\.0\.0\.1:9/,
    },
    { arm: "notYetAdopted", value: {}, says: /adopting this workspace/ },
  ];

  it.each(arms)("draws $arm at the row that was clicked", async ({ arm, value, says }) => {
    const { el } = draw(refusedWith({ case: arm, value }));
    el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]')?.click();
    await settle();
    const refusal = el.querySelector('[data-queue-place="ahead"] .refusal');
    expect(refusal?.getAttribute("data-arm")).toBe(arm);
    expect(refusal?.textContent ?? "").toMatch(says);
  });

  it("holds to the schema: every cause arm of SelectWorkspaceError is covered", () => {
    // A refusal arm is drawn above; an expected answer is pinned below.
    expect([...arms.map((a) => a.arm), ...Object.keys(SELECT_WORKSPACE_EXPECTED_ARMS)].sort()).toEqual(
      [...oneofArms(SelectWorkspaceErrorSchema, "cause")].sort(),
    );
  });

  it("draws nothing at the entry for a select a daemon standing down answered", async () => {
    const { el } = draw(refusedWith({ case: "standingDown", value: {} }));
    el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]')?.click();
    await settle();
    expect(el.querySelector('[data-queue-place="ahead"] .refusal')).toBeNull();
  });

  it("records a select a daemon standing down answered at INFO", async () => {
    const capture = captureLogRecords();
    const { el } = draw(refusedWith({ case: "standingDown", value: {} }));
    el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]')?.click();
    await settle();
    const record = await forwardedRecord(capture, "merge.queue-select-expected-answer");
    expect(record.level.case).toBe("info");
  });

  it("refuses an error whose cause oneof is unset — a refusal must say why", async () => {
    const { el, s } = draw(
      create(SelectWorkspaceResponseSchema, { result: { case: "error", value: {} } }),
    );
    const row = el.querySelector<HTMLElement>('[data-queue-place="ahead"]');
    if (row === null) throw new Error("no ahead row");
    const ref = create(WorkspaceRefSchema, { id: "front", dir: "/w/front" });
    const call = selectQueueEntryWorkspace(row, rowContext(s.ctx, mergeRow("m1")), ref);
    const landed = vi.advanceTimersByTimeAsync(0);
    await expect(call).rejects.toThrow(MalformedView);
    await landed;
    expect(row.querySelector(".refusal")).toBeNull();
  });

  it("replaces the previous refusal rather than stacking them", async () => {
    const { el } = draw(refusedWith({ case: "unknownWorkspace", value: {} }));
    const line = el.querySelector<HTMLElement>('[data-queue-place="ahead"] [data-select]');
    line?.click();
    await settle();
    line?.click();
    await settle();
    expect(el.querySelectorAll('[data-queue-place="ahead"] .refusal').length).toBe(1);
  });
});

describe("an arm this build cannot draw is a refusal, never a default", () => {
  it("refuses an entry whose status is an arm a newer daemon set", () => {
    const s = scripted(SELECTED);
    const newer = create(FeedMergeQueueEntrySchema, {
      workspace: { ref: create(WorkspaceRefSchema, { id: "x", dir: "/x" }) },
      label: { text: "x" },
      target: { text: "doom", dir: "/src/doom" },
    });
    (newer as { status: unknown }).status = { case: "rebasing", value: {} };

    let thrown: unknown;
    try {
      drawFeedMergeQueueEntry(newer, rowContext(s.ctx, mergeRow("m1")), "behind");
    } catch (err) {
      thrown = err;
    }

    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedMergeQueueEntry.status");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'rebasing' is not one this build can draw",
    );
  });

  it("refuses a SelectWorkspace answer whose result is an arm a newer daemon set", async () => {
    // The router transport re-encodes through the frozen schema, which would
    // drop an arm the descriptor has no field for; a fake client hands the
    // renderer the newer daemon's answer unaltered, which is the case under
    // test.
    const answer = create(SelectWorkspaceResponseSchema, {
      result: { case: "success", value: {} },
    });
    (answer as { result: unknown }).result = { case: "deferred", value: {} };
    const ctx = testAppContext({
      client: {
        selectWorkspace: () => Promise.resolve(answer),
      } as unknown as ReturnType<typeof createAgentReplClient>,
      workspace: WORKSPACE,
      ticker: createTicker(1000),
      failures: new RecordingSink(),
      composerEnabled: false,
    });
    const row = document.createElement("div");
    const ref = create(WorkspaceRefSchema, { id: "front", dir: "/w/front" });

    const call = selectQueueEntryWorkspace(row, rowContext(ctx, mergeRow("m1")), ref);
    const landed = vi.advanceTimersByTimeAsync(0);
    await expect(call).rejects.toThrow(
      "malformed view at SelectWorkspaceResponse.result: arm 'deferred' is not one this build can draw",
    );
    await landed;

    expect(row.querySelector(".refusal")).toBeNull();
  });
});

describe("a daemon that cannot be reached answers at the clicked row", () => {
  it("draws the transport refusal in the row, and does not raise a malformed view", async () => {
    const ctx = scriptedAppContext(
      {
          selectWorkspace: () => {
            throw new ConnectError("socket is gone", Code.Unavailable);
          },
      },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const row = document.createElement("div");
    const ref = create(WorkspaceRefSchema, { id: "front", dir: "/w/front" });

    const call = selectQueueEntryWorkspace(row, rowContext(ctx, mergeRow("m1")), ref);
    const landed = vi.advanceTimersByTimeAsync(0);
    await call;
    await landed;

    const refusal = row.querySelector(".merge-queue-refusal");
    expect(refusal?.getAttribute("data-arm")).toBe("transport");
    expect(refusal?.textContent).toBe("the daemon could not be reached");
  });
});

/** Every request a move-scripted daemon received, by endpoint. */
interface MoveScripted {
  ctx: AppContext;
  moves: UpdateMergeQueueRequest[];
  selects: SelectWorkspaceRequest[];
}

/** A context whose UpdateMergeQueue runs ANSWER, and whose SelectWorkspace succeeds. */
function moveScripted(answer: () => UpdateMergeQueueResponse): MoveScripted {
  const moves: UpdateMergeQueueRequest[] = [];
  const selects: SelectWorkspaceRequest[] = [];
  const ctx = scriptedAppContext(
    {
      updateMergeQueue: (req) => {
        moves.push(req);
        return answer();
      },
      selectWorkspace: (req) => {
        selects.push(req);
        return SELECTED;
      },
    },
    { workspace: WORKSPACE, failures: new RecordingSink() },
  );
  return { ctx, moves, selects };
}

/** An UpdateMergeQueue success. */
const MOVED = create(UpdateMergeQueueResponseSchema, { result: { case: "success", value: {} } });

/** An UpdateMergeQueue refusal answering no_such_queued_merge. */
const NOTHING_QUEUED = create(UpdateMergeQueueResponseSchema, {
  result: { case: "error", value: { cause: { case: "noSuchQueuedMerge", value: {} } } },
});

/** Draw a snapshot against a move-scripted daemon, its panel in the page. */
function drawMove(
  answer: () => UpdateMergeQueueResponse = () => MOVED,
  queue: FeedMergeQueue = longQueue(),
): { panel: FeedMergeQueueTable; table: HTMLElement; rc: RowContext; s: MoveScripted } {
  const s = moveScripted(answer);
  const rc = rowContext(s.ctx, mergeRow("m1"));
  const { panel, table } = drawnTable(queue, rc);
  document.body.append(panel.element);
  return { panel, table, rc, s };
}

/** Each request's action arm, the merge it moves and the merge it is placed beside. */
function movesOf(s: MoveScripted): unknown[] {
  return s.moves.map((r) =>
    r.action.case === "moveTo"
      ? [r.action.value.workspace?.id, r.action.value.workspace?.dir, r.action.value.place.case, r.action.value.place.value?.id]
      : r.action.case,
  );
}

/** A queue of a merge running, then three waiting: this workspace in the middle. */
function longQueue(): FeedMergeQueue {
  return create(FeedMergeQueueSchema, {
    ahead: [
      entry("front", { case: "merging", activeTab: { text: "tests", round: 1 } }),
      entry("next", { case: "waiting" }),
    ],
    current: entry("mine", { case: "waiting" }),
    behind: [entry("after", { case: "waiting" })],
  });
}

/** The row of TABLE named NAME. */
function rowNamed(table: HTMLElement, name: string): HTMLElement {
  const row = [...table.querySelectorAll<HTMLElement>(".merge-queue-entry")].find(
    (el) => el.querySelector(".merge-queue-label")?.textContent === name,
  );
  if (row === undefined) throw new Error(`no queue row is named ${name}`);
  return row;
}

/** The names of TABLE's rows, in order. */
function namesOf(table: HTMLElement): string[] {
  return [...table.querySelectorAll(".merge-queue-entry .merge-queue-label")].map((el) => el.textContent ?? "");
}

/** Lay TABLE's rows out 20px tall, top to bottom, from 20px down (jsdom lays nothing out). */
function layOut(table: HTMLElement): void {
  for (const row of table.querySelectorAll<HTMLElement>(".merge-queue-entry")) {
    Object.defineProperty(row, "getBoundingClientRect", {
      configurable: true,
      value: () => {
        const index = [...table.querySelectorAll(".merge-queue-entry")].indexOf(row);
        return boxRect({ top: 20 + index * 20, height: 20, width: 300 });
      },
    });
  }
}

/** Press ROW of TABLE and carry it to height TO, leaving it lifted. */
function carry(table: HTMLElement, row: HTMLElement, to: number): void {
  layOut(table);
  const from = row.getBoundingClientRect().top + 10;
  primaryPointer("pointerdown", row.querySelector(".merge-queue-label") ?? row, from);
  primaryPointer("pointermove", window, from + 6);
  primaryPointer("pointermove", window, to);
}

/** Drag ROW of TABLE to height TO and drop it there. */
function dragTo(table: HTMLElement, row: HTMLElement, to: number): void {
  carry(table, row, to);
  primaryPointer("pointerup", window, to);
}

describe("a waiting row carries no move controls", () => {
  it("draws the workspace cell as the name alone", () => {
    const { table } = drawMove();
    const cell = rowNamed(table, "mine").querySelector(".merge-queue-workspace");
    expect([...(cell?.children ?? [])].map((c) => c.className)).toEqual(["merge-queue-label"]);
  });

  it("draws no control inside any row's jump", () => {
    const { table } = drawMove();
    expect(table.querySelectorAll(".merge-queue-line ar-button, .merge-queue-line button")).toHaveLength(0);
  });
});

describe("a press on a waiting row drags it among the waiting", () => {
  it("has no grip on the merge running", () => {
    const { table, rc, panel } = drawMove();
    const refs = new Map<HTMLElement, never>();
    expect(queueEntryDrag(rc, arrangeOf(panel), table, rowNamed(table, "front"), refs)).toBeNull();
  });

  it("has no grip on the header", () => {
    const { table, rc, panel } = drawMove();
    expect(queueEntryDrag(rc, arrangeOf(panel), table, table.querySelector(".merge-queue-header"), new Map())).toBeNull();
  });

  it("has no grip on a refusal drawn in a row", () => {
    const { table, rc, panel } = drawMove();
    const row = rowNamed(table, "mine");
    const refusal = document.createElement("div");
    refusal.className = "refusal";
    row.append(refusal);
    const refs = new Map([[row, { id: "mine", dir: "/w/mine" }]]);
    expect(queueEntryDrag(rc, arrangeOf(panel), table, refusal, refs as never)).toBeNull();
  });

  it("lifts the pressed row among the waiting rows of its table", () => {
    const { table, rc, panel } = drawMove();
    const waiting = ["next", "mine", "after"].map((name) => rowNamed(table, name));
    const refs = new Map(waiting.map((row, i) => [row, { id: ["next", "mine", "after"][i], dir: "" }]));
    const spec = queueEntryDrag(rc, arrangeOf(panel), table, waiting[1].querySelector(".merge-queue-label"), refs as never);
    expect([spec?.kind, spec?.item, spec?.box, spec?.lists(), spec?.itemsOf(table), spec?.scroller]).toEqual([
      QUEUE_DRAG_KIND,
      waiting[1],
      table,
      [table],
      waiting,
      undefined,
    ]);
  });

  it("leaves the row's slot dimmed in the table while it is carried", () => {
    const { table } = drawMove();
    const row = rowNamed(table, "after");
    carry(table, row, 45);
    expect([row.classList.contains(DRAG_SLOT_CLASS), namesOf(table)]).toEqual([true, ["front", "after", "next", "mine"]]);
  });

  it("never carries the merge running", () => {
    const { table, s } = drawMove();
    dragTo(table, rowNamed(table, "front"), 90);
    expect([namesOf(table), s.moves]).toEqual([["front", "next", "mine", "after"], []]);
  });
});

describe("a dropped row asks UpdateMergeQueue to move its merge beside its new neighbor", () => {
  it("places it ahead of the waiting merge it was dropped before, by the refs the queue served", async () => {
    const { table, s } = drawMove();
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    expect(movesOf(s)).toEqual([["after", "/w/after", "aheadOf", "next"]]);
  });

  it("places it behind the last waiting merge when dropped at the end", async () => {
    const { table, s } = drawMove();
    dragTo(table, rowNamed(table, "next"), 95);
    await settle();
    expect(movesOf(s)).toEqual([["next", "/w/next", "behind", "after"]]);
  });

  it("names the user as who asked", async () => {
    const { table, s } = drawMove();
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    expect(s.moves.map((m) => m.requester?.asker.case)).toEqual(["user"]);
  });

  it("asks nothing when the row is dropped where it started", async () => {
    const { table, s } = drawMove();
    dragTo(table, rowNamed(table, "mine"), 71);
    await settle();
    expect(s.moves).toEqual([]);
  });

  it("does not jump to the row's workspace", async () => {
    const { table, s } = drawMove();
    dragTo(table, rowNamed(table, "after"), 45);
    rowNamed(table, "after").querySelector<HTMLElement>(".merge-queue-line")?.click();
    await settle();
    expect(s.selects).toEqual([]);
  });

  it("records the move at INFO with its workspace and where it goes", async () => {
    const capture = captureLogRecords();
    const { table } = drawMove();
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    const record = await forwardedRecord(capture, "merge.queue-move");
    expect([record.level.case, record.context?.workspace, record.context?.side, record.context?.anchor]).toEqual([
      "info",
      "after",
      "aheadOf",
      "next",
    ]);
  });
});

describe("the table is held while a row is carried and its move asked about", () => {
  /** LONG's order with AFTER moved ahead of NEXT, as the daemon republishes it. */
  function reordered(): FeedMergeQueue {
    return create(FeedMergeQueueSchema, {
      ahead: [
        entry("front", { case: "merging", activeTab: { text: "tests", round: 1 } }),
        entry("after", { case: "waiting" }),
        entry("next", { case: "waiting" }),
      ],
      current: entry("mine", { case: "waiting" }),
      behind: [],
    });
  }

  it("keeps a snapshot pushed while the row is carried out of the table", () => {
    const { panel, table } = drawMove();
    carry(table, rowNamed(table, "after"), 45);
    panel.offer(reordered());
    expect(tableOf(panel)).toBe(table);
  });

  it("draws the newest snapshot once the move is answered", async () => {
    const { panel, table } = drawMove();
    carry(table, rowNamed(table, "after"), 45);
    panel.offer(reordered());
    primaryPointer("pointerup", window, 45);
    await settle();
    expect([tableOf(panel) === table, namesOf(tableOf(panel))]).toEqual([false, ["front", "after", "next", "mine"]]);
  });

  it("draws the table it was drawn from again when the drag is cancelled", () => {
    const { panel, table } = drawMove();
    carry(table, rowNamed(table, "after"), 45);
    window.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect([tableOf(panel) === table, namesOf(tableOf(panel))]).toEqual([false, ["front", "next", "mine", "after"]]);
  });

  it("draws a snapshot pushed while nothing is carried at once", () => {
    const { panel } = drawMove();
    panel.offer(reordered());
    expect(namesOf(tableOf(panel))).toEqual(["front", "after", "next", "mine"]);
  });
});

describe("a refused or failed move answers above the table", () => {
  /** The refusal drawn in PANEL's status line, if any. */
  function statusRefusal(panel: FeedMergeQueueTable): Element | null {
    return panel.element.querySelector(`:scope > .${QUEUE_STATUS_CLASS} .refusal`);
  }

  it("draws a refusal above the table when the merge is no longer queued", async () => {
    const { panel, table } = drawMove(() => NOTHING_QUEUED);
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    const refusal = statusRefusal(panel);
    expect([refusal?.getAttribute("data-arm"), refusal?.textContent]).toEqual([
      "noSuchQueuedMerge",
      "this merge is no longer waiting on the queue",
    ]);
  });

  it("puts the refused row back where the snapshot has it", async () => {
    const { panel, table } = drawMove(() => NOTHING_QUEUED);
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    expect(namesOf(tableOf(panel))).toEqual(["front", "next", "mine", "after"]);
  });

  it("records the refusal at WARN with its arm, workspace and place", async () => {
    const capture = captureLogRecords();
    const { table } = drawMove(() => NOTHING_QUEUED);
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    const record = await forwardedRecord(capture, "merge.queue-move-refused");
    expect([record.level.case, record.context?.arm, record.context?.workspace, record.context?.side, record.context?.anchor]).toEqual([
      "warn",
      "noSuchQueuedMerge",
      "after",
      "aheadOf",
      "next",
    ]);
  });

  it("draws the transport refusal above the table when the daemon cannot be reached", async () => {
    const { panel, table } = drawMove(() => {
      throw new ConnectError("socket is gone", Code.Unavailable);
    });
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    expect(statusRefusal(panel)?.getAttribute("data-arm")).toBe("transport");
  });

  it("records the transport failure at ERROR with its workspace and place", async () => {
    const capture = captureLogRecords();
    const { table } = drawMove(() => {
      throw new ConnectError("socket is gone", Code.Unavailable);
    });
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    const record = await forwardedRecord(capture, "merge.queue-move-failed");
    expect([record.level.case, record.context?.workspace, record.context?.side]).toEqual(["error", "after", "aheadOf"]);
  });

  it("lets the table go after a transport failure", async () => {
    const { panel, table } = drawMove(() => {
      throw new ConnectError("socket is gone", Code.Unavailable);
    });
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    panel.offer(create(FeedMergeQueueSchema, { ahead: [], current: entry("mine", { case: "waiting" }), behind: [] }));
    expect(namesOf(tableOf(panel))).toEqual(["mine"]);
  });

  it("clears an earlier refusal when a later move takes", async () => {
    let answer = NOTHING_QUEUED;
    const { panel, table } = drawMove(() => answer);
    dragTo(table, rowNamed(table, "after"), 45);
    await settle();
    answer = MOVED;
    const redrawn = tableOf(panel);
    dragTo(redrawn, rowNamed(redrawn, "after"), 45);
    await settle();
    expect(statusRefusal(panel)).toBeNull();
  });
});

describe("dressQueueGhost", () => {
  /** A row drawn in a table, its three cells WIDTHS wide. */
  function rowWithCells(widths: number[]): { row: HTMLElement; table: HTMLElement } {
    const { table } = drawMove();
    const row = rowNamed(table, "mine");
    const cells = [...(row.querySelector(".merge-queue-line")?.children ?? [])];
    cells.forEach((cell, i) => {
      Object.defineProperty(cell, "getBoundingClientRect", { value: () => boxRect({ width: widths[i] }) });
    });
    return { row, table };
  }

  it("lays the copy out at the widths the row's cells were drawn at", () => {
    const { row, table } = rowWithCells([120, 40, 80]);
    const ghost = row.cloneNode(true) as HTMLElement;
    dressQueueGhost(ghost, row, table);
    expect([ghost.style.display, ghost.style.gridTemplateColumns]).toEqual(["grid", "120px 40px 80px"]);
  });

  it("lets the copy's line take the copy's columns", () => {
    const { row, table } = rowWithCells([120, 40, 80]);
    const ghost = row.cloneNode(true) as HTMLElement;
    dressQueueGhost(ghost, row, table);
    const line = ghost.querySelector<HTMLElement>(".merge-queue-line");
    expect([line?.style.display, line?.style.gridTemplateColumns]).toEqual(["grid", "subgrid"]);
  });

  it("refuses a row that draws no line", () => {
    const { table } = drawMove();
    const bare = document.createElement("div");
    expect(() => dressQueueGhost(bare.cloneNode() as HTMLElement, bare, table)).toThrow(/invariant/);
  });
});

/** PANEL's drag context, as its table's drags are handed it. */
function arrangeOf(panel: FeedMergeQueueTable) {
  return {
    hold: { offer: () => {}, drag: () => {}, cancel: () => {}, await: () => {}, settle: () => {}, reset: () => {}, holding: () => false },
    layer: document.body,
    status: panel.element,
  };
}
