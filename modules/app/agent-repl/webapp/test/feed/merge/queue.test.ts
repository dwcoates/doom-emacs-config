// @vitest-environment jsdom
import { SELECT_WORKSPACE_EXPECTED_ARMS } from "../../../src/rpc/refusal.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  SelectWorkspaceErrorSchema,
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
  type SelectWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  FeedMergeQueueSchema,
  FeedMergeQueueEntrySchema,
  type FeedMergeQueue,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { type AppContext } from "../../../src/rpc/context.js";
import { testAppContext } from "../../rpc/app-context.js";
import {
  drawFeedMergeQueue,
  drawFeedMergeQueueEntry,
  selectQueueEntryWorkspace,
} from "../../../src/feed/merge/queue.js";
import { RecordingSink, WORKSPACE, mergeRow, rowContext } from "../harness.js";
import { oneofArms } from "../../arms.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW_MS);
});
afterEach(() => {
  vi.useRealTimers();
});

/** Every SelectWorkspace this scripted daemon received. */
interface Scripted {
  ctx: AppContext;
  calls: SelectWorkspaceRequest[];
}

/** A context whose SelectWorkspace answers ANSWER. */
function scripted(answer: SelectWorkspaceResponse): Scripted {
  const calls: SelectWorkspaceRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      selectWorkspace: (req) => {
        calls.push(req);
        return answer;
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures: new RecordingSink(),
    composerEnabled: false,
  });
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
    status:
      status.case === "merging"
        ? { case: "merging", value: { activeTab: status.activeTab, stageEnteredAtMs } }
        : { case: "waiting", value: { stageEnteredAtMs } },
  });
}

/** A snapshot with one ahead, this workspace, one behind. */
function snapshot(frontEnteredMs: number = NOW_MS - 5_000, frontTab = { text: "tests", round: 2 }): FeedMergeQueue {
  return create(FeedMergeQueueSchema, {
    ahead: [entry("front", { case: "merging", activeTab: frontTab }, frontEnteredMs)],
    current: entry("mine", { case: "waiting" }, NOW_MS - 65_000),
    behind: [entry("after", { case: "waiting" }, NOW_MS - 3_000)],
  });
}

/** Draw a snapshot against a scripted daemon. */
function draw(answer: SelectWorkspaceResponse = SELECTED, queue: FeedMergeQueue = snapshot()): {
  el: HTMLElement;
  s: Scripted;
} {
  const s = scripted(answer);
  return { el: drawFeedMergeQueue(queue, rowContext(s.ctx, mergeRow("m1"))), s };
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

describe("drawFeedMergeQueue: a table of workspace, stage and duration", () => {
  it("leads with a header row naming the three columns", () => {
    const { el } = draw();
    const header = el.firstElementChild;
    expect([
      header?.classList.contains("merge-queue-header"),
      [...(header?.children ?? [])].map((c) => c.getAttribute("data-column")),
      [...(header?.children ?? [])].map((c) => c.textContent),
    ]).toEqual([true, ["workspace", "stage", "duration"], ["workspace", "stage", "duration"]]);
  });

  it("draws every row as the same three cells, in the header's column order", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll(".merge-queue-line")].map((line) =>
        [...line.children].map((c) => c.className),
      ),
    ).toEqual(
      Array.from({ length: 3 }, () => [
        "merge-queue-label",
        "merge-queue-stage",
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
    expect([cellsOf(before, "ahead")[2], cellsOf(after, "ahead").slice(1)]).toEqual([
      "9s",
      ["committing", "0s"],
    ]);
  });
});

describe("drawFeedMergeQueueEntry: the front's progress", () => {
  it("shows the front entry's active tab as its stage, rounds decorated as the tab draws them", () => {
    const { el } = draw();
    expect(cellsOf(el, "ahead")[1]).toBe("tests (2)");
  });

  it("draws a waiting entry's stage as 'waiting'", () => {
    const { el } = draw();
    expect(cellsOf(el, "behind")[1]).toBe("waiting");
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
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        selectWorkspace: () => {
          throw new ConnectError("socket is gone", Code.Unavailable);
        },
      });
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: createTicker(1000),
      failures: new RecordingSink(),
      composerEnabled: false,
    });
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
