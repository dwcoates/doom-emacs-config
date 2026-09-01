// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
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
import { createAppContext, type AppContext } from "../../../src/rpc/context.js";
import {
  drawFeedMergeQueue,
  drawFeedMergeQueueEntry,
  selectQueueEntryWorkspace,
} from "../../../src/feed/merge/queue.js";
import { RecordingSink, WORKSPACE, mergeRow, rowContext } from "../harness.js";
import { armsOf } from "../arms.js";

beforeEach(() => {
  vi.useFakeTimers();
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
  const ctx = createAppContext({
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

/** One entry of a queue snapshot. */
function entry(
  name: string,
  status: { case: "merging"; activeTab: { text: string; round: number } } | { case: "waiting" },
) {
  return create(FeedMergeQueueEntrySchema, {
    workspace: { ref: create(WorkspaceRefSchema, { id: name, dir: `/w/${name}` }) },
    label: { text: name },
    status:
      status.case === "merging"
        ? { case: "merging", value: { activeTab: status.activeTab } }
        : { case: "waiting", value: {} },
  });
}

/** A snapshot with one ahead, this workspace, one behind. */
function snapshot(): FeedMergeQueue {
  return create(FeedMergeQueueSchema, {
    ahead: [entry("front", { case: "merging", activeTab: { text: "tests", round: 2 } })],
    current: entry("mine", { case: "waiting" }),
    behind: [entry("after", { case: "waiting" })],
  });
}

/** Draw a snapshot against a scripted daemon. */
function draw(answer: SelectWorkspaceResponse = SELECTED): {
  el: HTMLElement;
  s: Scripted;
} {
  const s = scripted(answer);
  return { el: drawFeedMergeQueue(snapshot(), rowContext(s.ctx, mergeRow("m1"))), s };
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
    const current = el.querySelector('[data-queue-place="current"]');
    expect(current?.querySelector(".merge-queue-here")?.textContent).toBe("you are here");
  });

  it("draws each entry's label verbatim", () => {
    const { el } = draw();
    expect(
      [...el.querySelectorAll(".merge-queue-label")].map((e) => e.textContent),
    ).toEqual(["front", "mine", "after"]);
  });
});

describe("drawFeedMergeQueueEntry: the front's progress", () => {
  it("shows the front entry's active tab, rounds decorated as the tab draws them", () => {
    const { el } = draw();
    const front = el.querySelector('[data-queue-place="ahead"]');
    expect(front?.querySelector(".merge-queue-active")?.textContent).toBe("tests (2)");
  });

  it("draws a waiting entry plain, with no progress line of its own", () => {
    const { el } = draw();
    const behind = el.querySelector('[data-queue-place="behind"]');
    expect(behind?.querySelector(".merge-queue-active")).toBeNull();
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
    { arm: "unknownWorkspace", value: {}, says: /registry/ },
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
    { arm: "notYetAdopted", value: {}, says: /adopting/ },
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
    expect(arms.map((a) => a.arm).sort()).toEqual(
      armsOf(SelectWorkspaceErrorSchema.oneofs, "cause").sort(),
    );
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
