// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  SelectInEditorResponseSchema,
  type SelectInEditorRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import {
  RosterMergeQueueHeadSchema,
  RosterMergeQueueRepoGroupSchema,
  RosterMergeQueueStageSchema,
  RosterRepoMergeQueueSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { formatElapsed, formatTickedElapsed } from "../../src/duration.js";
import { roundSuffix, STATE_GLYPHS } from "../../src/feed/merge/tab-strip.js";
import { MERGE_GLYPH } from "../../src/merge-glyph.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawRosterMergedSectionMergeQueuePaused,
  drawRosterMergeQueue,
  drawRosterMergeQueueHead,
  drawRosterMergeQueueRepoGroup,
  drawRosterMergeQueueStage,
  drawRosterMergeQueueWaiting,
  MERGE_QUEUE_CLASS,
  MERGE_QUEUE_GROUP_CLASS,
  MERGE_QUEUE_GROUP_HEAD_CLASS,
  MERGE_QUEUE_HEAD_CLASS,
  MERGE_QUEUE_WAITING_CLASS,
} from "../../src/sidebar/merge-queue-view.js";
import { BUCKET_ATTRIBUTE } from "../../src/sidebar/subsections.js";
import {
  appContext,
  fakeTicker,
  mergedBand,
  mergingHead,
  NOW,
  queueGroup,
  queueSubject,
  sidebarContext,
  waitingMerge,
} from "./harness.js";

/** A sidebar context on a fake daemon. */
function daemon() {
  return { sc: sidebarContext(appContext({})) };
}

/** Let the click's ask settle. */
async function settled(): Promise<void> {
  for (let i = 0; i < 5; i++) await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
}

describe("drawRosterMergedSectionMergeQueuePaused", () => {
  it.each([
    { name: "paused", paused: true },
    { name: "running", paused: false },
  ])("reads a section whose queues are $name", ({ paused }) => {
    expect(drawRosterMergedSectionMergeQueuePaused(mergedBand([], 0, true, { paused }))).toBe(paused);
  });
});

describe("drawRosterMergeQueueRepoGroup", () => {
  const header = (path: string): HTMLElement => {
    const head = document.createElement("div");
    head.textContent = path;
    return head;
  };

  it("draws the header the caller draws, then the queue", () => {
    const { sc } = daemon();
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1", queue: { head: mergingHead("w1") } }), sc, "queues[0]", header);
    expect([...group.children].map((el) => el.className)).toEqual([MERGE_QUEUE_GROUP_HEAD_CLASS, `rows ${MERGE_QUEUE_CLASS}`]);
    expect(group.firstElementChild?.textContent).toBe("queues[0].header");
  });

  it("names its repository", () => {
    const { sc } = daemon();
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1" }), sc, "queues[0]", header);
    expect([group.className, group.getAttribute("data-repository")]).toEqual([MERGE_QUEUE_GROUP_CLASS, "repo-1"]);
  });

  it.each([
    { name: "key", init: { header: { label: { text: "r" } }, queue: {} } },
    { name: "queue", init: { key: { repository: { id: "r" } }, header: { label: { text: "r" } } } },
  ])("refuses a group without its $name", ({ init }) => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueueRepoGroup(create(RosterMergeQueueRepoGroupSchema, init), sc, "queues[0]", header)).toThrow(MalformedView);
  });
});

/** A stage of LABEL at ROUND in STATE, begun at START and ended at END. */
function stage(label: string, state: "live" | "succeeded" | "failed", round = 1, start = 1_000n, end = 4_000n) {
  return create(RosterMergeQueueStageSchema, {
    label,
    round,
    state:
      state === "live"
        ? { case: "live", value: { startedAtMs: start } }
        : { case: state, value: { startedAtMs: start, endedAtMs: end } },
  });
}

describe("drawRosterMergeQueue", () => {
  it("draws the head's card, then each waiting merge, front first", () => {
    const { sc } = daemon();
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w3"), waiting: [waitingMerge("w1"), waitingMerge("w2")] });
    const body = drawRosterMergeQueue(queue, sc, "queue");
    expect([...body.children].map((el) => [el.className, el.getAttribute("data-workspace")])).toEqual([
      [MERGE_QUEUE_HEAD_CLASS, "w3"],
      [MERGE_QUEUE_WAITING_CLASS, "w1"],
      [MERGE_QUEUE_WAITING_CLASS, "w2"],
    ]);
  });

  it("is a rows box no drag arranges", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1") }), sc, "queue");
    expect([body.classList.contains("rows"), body.classList.contains(MERGE_QUEUE_CLASS), body.hasAttribute(BUCKET_ATTRIBUTE)]).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("draws nothing for an empty queue", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, {}), sc, "queue").children.length).toBe(0);
  });

  it("numbers each waiting merge by its place in line", () => {
    const { sc } = daemon();
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), waiting: [waitingMerge("w1"), waitingMerge("w2")] });
    const body = drawRosterMergeQueue(queue, sc, "queue");
    expect([...body.querySelectorAll(".sb-mq-place")].map((el) => el.textContent)).toEqual(["1", "2"]);
  });

  it("says a paused queue's head waits paused", () => {
    const { sc } = daemon();
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW) } } });
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head, paused: {} }), sc, "queue");
    expect(body.querySelector(".sb-mq-detail")?.textContent).toBe("paused");
  });
});

describe("drawRosterMergeQueueHead", () => {
  it("leads with the merge glyph and the credited workspace's name", () => {
    const { sc } = daemon();
    const card = drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { name: "one" }) }), false, sc, "head");
    expect([card.querySelector(".sb-mq-glyph")?.textContent, card.querySelector(".sb-mq-name")?.textContent]).toEqual([MERGE_GLYPH, "one"]);
  });

  it("says an async merge is async on hover", () => {
    const { sc } = daemon();
    const card = drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { async: true }) }), false, sc, "head");
    expect(card.querySelector(".sb-mq-name")?.getAttribute("title")).toBe("async merge");
  });

  it("draws each stage in order", () => {
    const { sc } = daemon();
    const head = mergingHead("w1", { stages: [stage("queue", "succeeded"), stage("rebasing", "succeeded"), stage("tests", "live")] });
    const card = drawRosterMergeQueueHead(head, false, sc, "head");
    expect([...card.querySelectorAll(".sb-mq-stage-label")].map((el) => el.textContent)).toEqual(["queue", "rebasing", "tests"]);
  });

  it("draws no stage strip before the merge has a stage", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1"), false, sc, "head").querySelector(".sb-mq-stages")).toBeNull();
  });

  it.each([
    { name: "a merging head draws its step", head: () => mergingHead("w1", { step: "rebasing 3/7" }), paused: false, want: "rebasing 3/7" },
    { name: "a merging head on a paused queue says so after its step", head: () => mergingHead("w1", { step: "testing" }), paused: true, want: "testing · paused" },
    { name: "a merging head with no step on a paused queue says paused", head: () => mergingHead("w1"), paused: true, want: "paused" },
    {
      name: "a waiting head says it is next",
      head: () => create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW) } } }),
      paused: false,
      want: "next",
    },
    {
      name: "a failed head draws its account",
      head: () => create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "failed", value: { summary: "the tests failed" } } }),
      paused: false,
      want: "the tests failed",
    },
  ])("$name", ({ head, paused, want }) => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(head(), paused, sc, "head").querySelector(".sb-mq-detail")?.textContent).toBe(want);
  });

  it("draws no detail line for a merging head with no step", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1"), false, sc, "head").querySelector(".sb-mq-detail")).toBeNull();
  });

  it.each([
    { name: "a merging head", standing: "merging" },
    { name: "a waiting head", standing: "waiting" },
    { name: "a failed head", standing: "failed" },
  ])("states its standing for $name", ({ standing }) => {
    const { sc } = daemon();
    const value = standing === "waiting" ? { queuedAtMs: 0n } : standing === "failed" ? { summary: "x" } : {};
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: standing, value } as never });
    expect(drawRosterMergeQueueHead(head, false, sc, "head").getAttribute("data-standing")).toBe(standing);
  });

  it("ticks a merging head's clock from its first stage's start", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = mergingHead("w1", { stages: [stage("queue", "succeeded", 1, BigInt(NOW - 5_000)), stage("tests", "live", 1, BigInt(NOW - 1_000))] });
    const card = drawRosterMergeQueueHead(head, false, sc, "head");
    ticker.tick(NOW + 60_000);
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(65_000));
  });

  it("ticks a waiting head's clock from when it was put in line", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW - 2_000) } } });
    const card = drawRosterMergeQueueHead(head, false, sc, "head");
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(2_000));
  });

  it("draws no clock on a failed head", () => {
    const { sc } = daemon();
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), stages: [stage("tests", "failed")], standing: { case: "failed", value: { summary: "x" } } });
    expect(drawRosterMergeQueueHead(head, false, sc, "head").querySelector(".sb-mq-clock")).toBeNull();
  });

  it("refuses a head with no standing as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueueHead(create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1") }), false, sc, "head")).toThrow(MalformedView);
  });

  it("refuses a head with no subject as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueueHead(create(RosterMergeQueueHeadSchema, { standing: { case: "merging", value: {} } }), false, sc, "head")).toThrow(
      MalformedView,
    );
  });
});

describe("drawRosterMergeQueueStage", () => {
  it.each([
    { name: "a live stage", state: "live" as const, glyph: STATE_GLYPHS.live },
    { name: "a succeeded stage", state: "succeeded" as const, glyph: STATE_GLYPHS.succeeded },
    { name: "a failed stage", state: "failed" as const, glyph: STATE_GLYPHS.failed },
  ])("wears the bubble's glyph for $name", ({ state, glyph }) => {
    const el = drawRosterMergeQueueStage(stage("tests", state), "stage");
    expect([el.getAttribute("data-state"), el.querySelector(".sb-mq-stage-glyph")?.textContent]).toEqual([state, glyph]);
  });

  it.each([
    { name: "draws round 1 undecorated", round: 1 },
    { name: "decorates a later round as the bubble does", round: 2 },
  ])("$name", ({ round }) => {
    expect(drawRosterMergeQueueStage(stage("committing", "live", round), "stage").querySelector(".sb-mq-stage-label")?.textContent).toBe(
      `committing${roundSuffix(round)}`,
    );
  });

  it("says how long a settled stage ran on hover", () => {
    expect(drawRosterMergeQueueStage(stage("tests", "succeeded", 1, 1_000n, 4_000n), "stage").title).toBe(formatElapsed(3_000));
  });

  it("refuses a stage with no state as a malformed view", () => {
    expect(() => drawRosterMergeQueueStage(create(RosterMergeQueueStageSchema, { label: "tests", round: 1 }), "stage")).toThrow(MalformedView);
  });
});

describe("drawRosterMergeQueueWaiting", () => {
  it("draws its place, its name and how long it has waited", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1", { queuedAtMs: BigInt(NOW - 3_000), subject: queueSubject("w1", { name: "one" }) }), 2, sc, "w");
    expect([line.querySelector(".sb-mq-place")?.textContent, line.querySelector(".sb-mq-name")?.textContent, line.querySelector(".sb-mq-clock")?.textContent]).toEqual([
      "2",
      "one",
      formatTickedElapsed(3_000),
    ]);
  });

  it.each([
    { name: "marks a merge put in line at the front", front: true, want: true },
    { name: "marks no other merge", front: false, want: false },
  ])("$name", ({ front, want }) => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueWaiting(waitingMerge("w1", { front }), 1, sc, "w").querySelector(".sb-mq-front") !== null).toBe(want);
  });
});

describe("a click on a merge in the queue", () => {
  /** A context recording every SelectWorkspace and SelectInEditor ask. */
  function selecting() {
    const asked: Array<SelectWorkspaceRequest | SelectInEditorRequest> = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          asked.push(request);
          return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });
        },
        selectInEditor: (request) => {
          asked.push(request);
          return create(SelectInEditorResponseSchema, { result: { case: "success", value: { route: { case: "relayed", value: {} } } } });
        },
      }),
    );
    return { asked, sc };
  }

  it("selects the credited workspace and opens the merge's bubble", async () => {
    const { asked, sc } = selecting();
    drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { reveal: "bubble-1" }) }), false, sc, "head").click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id, "reveal" in r ? r.reveal?.value : undefined])).toEqual([
      ["agentrepl.v1.SelectWorkspaceRequest", "w1", "bubble-1"],
    ]);
  });

  it("selects as a row does when the merge has no bubble", async () => {
    const { asked, sc } = selecting();
    drawRosterMergeQueueWaiting(waitingMerge("w2"), 1, sc, "w").click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id])).toEqual([["agentrepl.v1.SelectInEditorRequest", "w2"]]);
  });
});
