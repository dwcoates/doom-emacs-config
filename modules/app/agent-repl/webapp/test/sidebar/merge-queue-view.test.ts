// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
  type UpdateSidebarViewResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
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
  RosterMergeQueueStageSchema,
  RosterOpenSectionSchema,
  RosterRepoMergeQueueSchema,
  RosterRepoSectionSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { formatElapsed, formatTickedElapsed } from "../../src/duration.js";
import { roundSuffix, STATE_GLYPHS } from "../../src/feed/merge/tab-strip.js";
import { MERGE_GLYPH } from "../../src/merge-glyph.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawMergeQueueToggle,
  drawRosterMergeQueue,
  drawRosterMergeQueueHead,
  drawRosterMergeQueueStage,
  drawRosterMergeQueueWaiting,
  drawRosterOpenSectionMergeQueuePaused,
  drawRosterOpenSectionShows,
  drawRosterRepoShows,
  MERGE_QUEUE_CLASS,
  MERGE_QUEUE_HEAD_CLASS,
  MERGE_QUEUE_PAUSED_ATTRIBUTE,
  MERGE_QUEUE_TOGGLE_CLASS,
  MERGE_QUEUE_WAITING_CLASS,
  mergeQueueEmpty,
  OPEN_SHOWS_VIEW_KEY,
  paintMergeQueueToggle,
  SHOWS_ATTRIBUTE,
} from "../../src/sidebar/merge-queue-view.js";
import { BUCKET_ATTRIBUTE } from "../../src/sidebar/subsections.js";
import {
  appContext,
  fakeTicker,
  mergingHead,
  NOW,
  openSection,
  queueSubject,
  repoSection,
  sidebarContext,
  waitingMerge,
} from "./harness.js";

const OK = create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });

/** A context whose daemon records every UpdateSidebarView and answers ANSWER. */
function daemon(answer: () => UpdateSidebarViewResponse = () => OK) {
  const asked: UpdateSidebarViewRequest[] = [];
  const sc = sidebarContext(
    appContext({
      updateSidebarView: (request) => {
        asked.push(request);
        return answer();
      },
    }),
  );
  return { asked, sc };
}

/** Let the click's ask settle. */
async function settled(): Promise<void> {
  for (let i = 0; i < 5; i++) await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
}

describe("drawRosterOpenSectionShows", () => {
  it.each([
    { name: "the workspaces", mergeQueues: false },
    { name: "the merge queues", mergeQueues: true },
  ])("reads $name", ({ mergeQueues }) => {
    expect(drawRosterOpenSectionShows(openSection(false, mergeQueues), "open")).toBe(mergeQueues);
  });

  it("refuses an unset arm as a malformed view", () => {
    expect(() => drawRosterOpenSectionShows(create(RosterOpenSectionSchema, {}), "open")).toThrow(MalformedView);
  });
});

describe("drawRosterOpenSectionMergeQueuePaused", () => {
  it.each([
    { name: "a paused merge queue when the marker stands", paused: true },
    { name: "no paused merge queue when the marker is absent", paused: false },
  ])("reads $name", ({ paused }) => {
    expect(drawRosterOpenSectionMergeQueuePaused(openSection(false, false, paused))).toBe(paused);
  });
});

describe("drawRosterRepoShows", () => {
  it("reads the workspaces", () => {
    expect(drawRosterRepoShows(repoSection({ id: "repo-1" }), "section")).toEqual({ case: "workspaces" });
  });

  it("reads the merge queue with its head", () => {
    const shows = drawRosterRepoShows(repoSection({ id: "repo-1", mergeQueue: { head: mergingHead("w1") } }), "section");
    expect(shows.case === "mergeQueue" ? shows.queue.head?.subject?.workspace?.id : shows.case).toBe("w1");
  });

  it("refuses an unset arm as a malformed view", () => {
    expect(() => drawRosterRepoShows(create(RosterRepoSectionSchema, {}), "section")).toThrow(MalformedView);
  });
});

describe("paintMergeQueueToggle", () => {
  it.each([
    { name: "the workspaces", mergeQueues: false, shows: "workspaces", pressed: "false", title: "show each repository's merge queue" },
    { name: "the merge queues", mergeQueues: true, shows: "merge-queues", pressed: "true", title: "show each repository's workspaces" },
  ])("says the sections show $name", ({ mergeQueues, shows, pressed, title }) => {
    const toggle = document.createElement("span");
    paintMergeQueueToggle(toggle, mergeQueues);
    expect([toggle.getAttribute(SHOWS_ATTRIBUTE), toggle.getAttribute("aria-pressed"), toggle.title]).toEqual([shows, pressed, title]);
  });
});

describe("drawMergeQueueToggle", () => {
  it("draws the merge glyph", () => {
    const { sc } = daemon();
    const toggle = drawMergeQueueToggle(sc, false, false);
    expect([toggle.className, toggle.textContent]).toEqual([MERGE_QUEUE_TOGGLE_CLASS, MERGE_GLYPH]);
  });

  it.each([
    { name: "the workspaces", wire: false, shows: "workspaces" },
    { name: "the merge queues", wire: true, shows: "merge-queues" },
  ])("is drawn as the wire says when it shows $name", ({ wire, shows }) => {
    const { sc } = daemon();
    expect(drawMergeQueueToggle(sc, wire, false).getAttribute(SHOWS_ATTRIBUTE)).toBe(shows);
  });

  it("is drawn as this page asked while its ask is in flight", () => {
    const { sc } = daemon();
    sc.view.ask(OPEN_SHOWS_VIEW_KEY, true);
    expect(drawMergeQueueToggle(sc, false, false).getAttribute(SHOWS_ATTRIBUTE)).toBe("merge-queues");
  });

  it.each([
    { name: "asks for the merge queues while the workspaces show", wire: false, want: "mergeQueues" },
    { name: "asks for the workspaces while the merge queues show", wire: true, want: "workspaces" },
  ])("$name", async ({ wire, want }) => {
    const { asked, sc } = daemon();
    drawMergeQueueToggle(sc, wire, false).click();
    await settled();
    expect(asked.map((r) => (r.change.case === "showOpen" ? r.change.value.shows.case : r.change.case))).toEqual([want]);
  });

  it.each([
    { name: "marked while a merge queue is paused", paused: true },
    { name: "unmarked while no merge queue is paused", paused: false },
  ])("is $name", ({ paused }) => {
    const { sc } = daemon();
    expect(drawMergeQueueToggle(sc, false, paused).hasAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE)).toBe(paused);
  });

  it("keeps the paused mark across a click", () => {
    const { sc } = daemon();
    const toggle = drawMergeQueueToggle(sc, false, true);
    toggle.click();
    expect(toggle.hasAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE)).toBe(true);
  });

  it("shows the merge queues at once, before the daemon answers", () => {
    const { sc } = daemon();
    const toggle = drawMergeQueueToggle(sc, false, false);
    toggle.click();
    expect(toggle.getAttribute(SHOWS_ATTRIBUTE)).toBe("merge-queues");
  });

  it("repaints from the wire when the daemon cannot be reached", async () => {
    const { sc } = daemon(() => {
      throw new Error("down");
    });
    const host = document.createElement("div");
    const toggle = drawMergeQueueToggle(sc, false, false);
    host.appendChild(toggle);
    toggle.click();
    await settled();
    expect(toggle.getAttribute(SHOWS_ATTRIBUTE)).toBe("workspaces");
  });

  it("never reaches the header its click would fold", () => {
    const { sc } = daemon();
    const header = document.createElement("div");
    let folds = 0;
    header.addEventListener("click", () => folds++);
    const toggle = drawMergeQueueToggle(sc, false, false);
    header.appendChild(toggle);
    toggle.click();
    expect(folds).toBe(0);
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

describe("mergeQueueEmpty", () => {
  it.each([
    { name: "a queue with nothing in it", queue: {}, want: true },
    { name: "a queue with a head", queue: { head: mergingHead("w1") }, want: false },
    { name: "a queue with a merge waiting", queue: { waiting: [waitingMerge("w1")] }, want: false },
  ])("answers $want for $name", ({ queue, want }) => {
    expect(mergeQueueEmpty(create(RosterRepoMergeQueueSchema, queue))).toBe(want);
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
