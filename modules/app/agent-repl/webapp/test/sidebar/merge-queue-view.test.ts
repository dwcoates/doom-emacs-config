// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { cascadedValue, inRail } from "../stylesheet.js";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  SelectInEditorResponseSchema,
  type SelectInEditorRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import {
  UpdateMergeQueueResponseSchema,
  type UpdateMergeQueueRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import {
  RosterMergeQueueHeadSchema,
  RosterMergeQueuePausedSchema,
  RosterMergeQueueRepoGroupSchema,
  RosterMergeQueueStageSchema,
  RosterRepoMergeQueueSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { formatAge, formatElapsed, formatTickedElapsed } from "../../src/duration.js";
import { roundSuffix, STATE_GLYPHS } from "../../src/feed/merge/tab-strip.js";
import { MERGE_GLYPH } from "../../src/merge-glyph.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawHeadClock,
  drawRosterMergeQueue,
  drawRosterMergeQueueHead,
  drawRosterMergeQueueRepoGroup,
  drawRosterMergeQueueStage,
  drawRosterMergeQueueWaiting,
  MERGE_QUEUE_CLASS,
  MERGE_QUEUE_GROUP_CLASS,
  MERGE_QUEUE_GROUP_HEAD_CLASS,
  MERGE_QUEUE_HEAD_CLASS,
  MERGE_QUEUE_PAUSED_ATTRIBUTE,
  MERGE_QUEUE_SUSPENDED_ATTRIBUTE,
  MERGE_QUEUE_WAITING_CLASS,
  pausedText,
  placeBeside,
  queueDrag,
} from "../../src/sidebar/merge-queue-view.js";
import { BUCKET_ATTRIBUTE } from "../../src/sidebar/subsections.js";
import {
  appContext,
  fakeTicker,
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

/** A queue's pause: the user's unless said, at AT_MS when given, hard when said. */
function paused(init: { atMs?: number; workspace?: string; hard?: boolean } = {}) {
  return create(RosterMergeQueuePausedSchema, {
    pausedAtMs: init.atMs === undefined ? undefined : BigInt(init.atMs),
    by:
      init.workspace === undefined
        ? { asker: { case: "user", value: {} } }
        : { asker: { case: "workspace", value: { name: init.workspace } } },
    hard: init.hard ?? false,
  });
}

/** A context recording every UpdateMergeQueue ask, answering each with ANSWER. */
function queueDaemon(answer: () => ReturnType<typeof create<typeof UpdateMergeQueueResponseSchema>> = () =>
  create(UpdateMergeQueueResponseSchema, { result: { case: "success", value: {} } })) {
  const asked: UpdateMergeQueueRequest[] = [];
  const sc = sidebarContext(
    appContext({
      updateMergeQueue: (request) => {
        asked.push(request);
        return answer();
      },
    }),
  );
  return { asked, sc };
}

describe("pausedText", () => {
  it.each([
    { name: "the user's pause", pause: () => paused({ atMs: NOW - 90_000 }), want: `Paused ${formatAge(90_000)} ago by you. Click to resume.` },
    { name: "a workspace's hard stop", pause: () => paused({ atMs: NOW - 90_000, workspace: "doom", hard: true }), want: `Stopped ${formatAge(90_000)} ago by doom. Click to resume.` },
    { name: "a pause recorded before its record was kept", pause: () => create(RosterMergeQueuePausedSchema, {}), want: "Paused. Click to resume." },
  ])("says $name", ({ pause, want }) => {
    expect(pausedText(pause(), NOW, "paused")).toBe(want);
  });

  it("refuses a pauser with no asker as a malformed view", () => {
    expect(() => pausedText(create(RosterMergeQueuePausedSchema, { by: {} }), NOW, "paused")).toThrow(MalformedView);
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
    expect(group.firstElementChild?.textContent).toBe("queues[0].header(1)");
  });

  it("names its repository", () => {
    const { sc } = daemon();
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1", queue: { head: mergingHead("w1") } }), sc, "queues[0]", header);
    expect([group.className, group.getAttribute("data-repository")]).toEqual([MERGE_QUEUE_GROUP_CLASS, "repo-1"]);
  });

  it("counts the merges in its line beside the header's label", () => {
    const { sc } = daemon();
    const group = drawRosterMergeQueueRepoGroup(
      queueGroup({ id: "repo-1", queue: { head: mergingHead("w1"), waiting: [waitingMerge("w2"), waitingMerge("w3")] } }),
      sc,
      "queues[0]",
      header,
    );
    expect(group.querySelector(`.${MERGE_QUEUE_GROUP_HEAD_CLASS} .sb-count`)?.textContent).toBe("(3)");
  });

  it.each([
    { name: "a running queue", pause: undefined, want: null },
    { name: "a paused queue", pause: paused(), want: "pause" },
    { name: "a hard-stopped queue", pause: paused({ hard: true }), want: "hard" },
  ])("marks $name", ({ pause, want }) => {
    const { sc } = daemon();
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1", queue: { head: mergingHead("w1"), paused: pause } }), sc, "queues[0]", header);
    expect(group.getAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE)).toBe(want);
  });

  it("says on a paused queue's header who paused it and when", () => {
    const { sc } = daemon();
    const group = drawRosterMergeQueueRepoGroup(
      queueGroup({ id: "repo-1", queue: { head: mergingHead("w1"), paused: paused({ atMs: NOW, workspace: "doom" }) } }),
      sc,
      "queues[0]",
      header,
    );
    expect(group.querySelector<HTMLElement>(`.${MERGE_QUEUE_GROUP_HEAD_CLASS}`)?.title).toBe(pausedText(paused({ atMs: NOW, workspace: "doom" }), NOW, "p"));
  });

  it("resumes a paused queue on a click of its header", async () => {
    const { asked, sc } = queueDaemon();
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1", queue: { head: mergingHead("w1"), paused: paused() } }), sc, "queues[0]", header);
    group.querySelector<HTMLElement>(`.${MERGE_QUEUE_GROUP_HEAD_CLASS}`)?.click();
    await settled();
    expect(asked.map((r) => [r.action.case, r.action.case === "resume" ? r.action.value.repository?.id : undefined, r.requester?.asker.case])).toEqual([
      ["resume", "repo-1", "user"],
    ]);
  });

  it("draws a refused resume at the header", async () => {
    const { sc } = queueDaemon(() =>
      create(UpdateMergeQueueResponseSchema, { result: { case: "error", value: { cause: { case: "notPaused", value: {} } } } }),
    );
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1", queue: { head: mergingHead("w1"), paused: paused() } }), sc, "queues[0]", header);
    const head = group.querySelector<HTMLElement>(`.${MERGE_QUEUE_GROUP_HEAD_CLASS}`);
    head?.click();
    await settled();
    expect(group.querySelector('.refusal[data-arm="notPaused"]') !== null || head?.parentElement?.querySelector('.refusal[data-arm="notPaused"]') !== null).toBe(true);
  });

  it("asks nothing on a click of a running queue's header", async () => {
    const { asked, sc } = queueDaemon();
    const group = drawRosterMergeQueueRepoGroup(queueGroup({ id: "repo-1", queue: { head: mergingHead("w1") } }), sc, "queues[0]", header);
    group.querySelector<HTMLElement>(`.${MERGE_QUEUE_GROUP_HEAD_CLASS}`)?.click();
    await settled();
    expect(asked).toEqual([]);
  });

  it.each([
    { name: "key", init: { header: { label: { text: "r" } }, queue: { head: mergingHead("w1") }, count: { workspaces: 1 } } },
    { name: "queue", init: { key: { repository: { id: "r" } }, header: { label: { text: "r" } }, count: { workspaces: 1 } } },
    { name: "count", init: { key: { repository: { id: "r" } }, header: { label: { text: "r" } }, queue: { head: mergingHead("w1") } } },
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

  it("is a rows box no grouping drag arranges", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1") }), sc, "queue");
    expect([body.classList.contains("rows"), body.classList.contains(MERGE_QUEUE_CLASS), body.hasAttribute(BUCKET_ATTRIBUTE)]).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("refuses a queue with no head as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, {}), sc, "queue")).toThrow(MalformedView);
  });

  it("numbers no waiting merge: each opens with the same bullet, then its name", () => {
    const { sc } = daemon();
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), waiting: [waitingMerge("w1"), waitingMerge("w2")] });
    const body = drawRosterMergeQueue(queue, sc, "queue");
    expect([...body.querySelectorAll(`.${MERGE_QUEUE_WAITING_CLASS}`)].map((el) => [...el.children].slice(0, 2).map((child) => child.textContent))).toEqual([
      ["•", "w1"],
      ["•", "w2"],
    ]);
  });

  it.each([
    { name: "paused", pause: paused(), want: "paused" },
    { name: "stopped", pause: paused({ hard: true }), want: "stopped" },
  ])("says a $name queue's waiting head is $name", ({ pause, want }) => {
    const { sc } = daemon();
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW) } } });
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head, paused: pause }), sc, "queue");
    expect(body.querySelector(".sb-mq-detail")?.textContent).toBe(want);
  });
});

describe("dragging a waiting merge", () => {
  /** A queue of head w0 and waiting W, drawn inside a Merge Queue section's rows. */
  function drawnQueue(sc: ReturnType<typeof sidebarContext>, waiting: string[]) {
    const section = document.createElement("div");
    section.className = "repo merged-section";
    const rows = document.createElement("div");
    rows.className = "rows";
    const group = document.createElement("div");
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), waiting: waiting.map((w) => waitingMerge(w)) }), sc, "queue");
    group.appendChild(body);
    rows.appendChild(group);
    section.appendChild(rows);
    const lines = [...body.querySelectorAll<HTMLElement>(`.${MERGE_QUEUE_WAITING_CLASS}`)];
    const refs = new Map(lines.map((line) => [line, { id: line.getAttribute("data-workspace") ?? "", dir: `/w/${line.getAttribute("data-workspace")}` }] as const));
    return { body, lines, refs, rows, section };
  }

  it("has no grip on the head", () => {
    const { sc } = daemon();
    const { body, refs } = drawnQueue(sc, ["w1"]);
    expect(queueDrag(sc, body, body.querySelector(`.${MERGE_QUEUE_HEAD_CLASS}`), refs as never)).toBeNull();
  });

  it("lifts a waiting merge among the waiting, scrolling the section's rows", () => {
    const { sc } = daemon();
    const { body, lines, refs, rows, section } = drawnQueue(sc, ["w1", "w2"]);
    const spec = queueDrag(sc, body, lines[1].querySelector(".sb-mq-name"), refs as never);
    expect([spec?.item, spec?.box, spec?.scroller, spec?.lists(), spec?.itemsOf(body)]).toEqual([lines[1], section, rows, [body], lines]);
  });

  it.each([
    { name: "ahead of the merge it was dropped before", before: 0, want: { case: "aheadOf", id: "w1" } },
    { name: "behind the last merge when dropped at the end", before: null, want: { case: "behind", id: "w3" } },
  ])("places a dropped merge $name", ({ before, want }) => {
    const { sc } = daemon();
    const { lines, refs } = drawnQueue(sc, ["w1", "w2", "w3"]);
    const place = placeBeside(lines, lines[1], before === null ? null : lines[before], refs as never);
    expect([place.case, place.value?.id]).toEqual([want.case, want.id]);
  });

  it("asks the daemon to move a dropped merge to its place", async () => {
    const { asked, sc } = queueDaemon();
    const { body, lines, refs } = drawnQueue(sc, ["w1", "w2"]);
    queueDrag(sc, body, lines[1], refs as never)?.dropped(body, lines[0]);
    await settled();
    expect(asked.map((r) => (r.action.case === "moveTo" ? [r.action.value.workspace?.id, r.action.value.place.case, r.action.value.place.value?.id] : r.action.case))).toEqual([
      ["w2", "aheadOf", "w1"],
    ]);
  });

  it("draws a refused move at the arrangement's status and lets the drawing go", async () => {
    const { sc } = queueDaemon(() =>
      create(UpdateMergeQueueResponseSchema, { result: { case: "error", value: { cause: { case: "noSuchQueuedMerge", value: {} } } } }),
    );
    const { body, lines, refs } = drawnQueue(sc, ["w1", "w2"]);
    queueDrag(sc, body, lines[1], refs as never)?.dropped(body, lines[0]);
    await settled();
    expect([sc.arrange.status.parentElement?.querySelector('.refusal[data-arm="noSuchQueuedMerge"]') !== null || sc.arrange.status.querySelector('.refusal[data-arm="noSuchQueuedMerge"]') !== null, sc.arrange.hold.holding()]).toEqual([true, false]);
  });
});

describe("drawRosterMergeQueueHead", () => {
  it("leads with the merge glyph and the credited workspace's name", () => {
    const { sc } = daemon();
    const card = drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { name: "one" }) }), undefined, sc, "head");
    expect([card.querySelector(".sb-mq-glyph")?.textContent, card.querySelector(".sb-mq-name")?.textContent]).toEqual([MERGE_GLYPH, "one"]);
  });

  it("says an async merge is async on hover", () => {
    const { sc } = daemon();
    const card = drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { async: true }) }), undefined, sc, "head");
    expect(card.querySelector(".sb-mq-name")?.getAttribute("title")).toBe("async merge");
  });

  it("draws each stage in order", () => {
    const { sc } = daemon();
    const head = mergingHead("w1", { stages: [stage("queue", "succeeded"), stage("rebasing", "succeeded"), stage("tests", "live")] });
    const card = drawRosterMergeQueueHead(head, undefined, sc, "head");
    expect([...card.querySelectorAll(".sb-mq-stage-label")].map((el) => el.textContent)).toEqual(["queue", "rebasing", "tests"]);
  });

  it("draws no stage strip before the merge has a stage", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1"), undefined, sc, "head").querySelector(".sb-mq-stages")).toBeNull();
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
  ])("$name", ({ head, paused: isPaused, want }) => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(head(), isPaused ? paused() : undefined, sc, "head").querySelector(".sb-mq-detail")?.textContent).toBe(want);
  });

  it("says a merging head on a stopped queue is stopped", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1", { step: "deploying" }), paused({ hard: true }), sc, "head").querySelector(".sb-mq-detail")?.textContent).toBe(
      "deploying · stopped",
    );
  });

  it.each([
    { name: "marks a suspended head", suspended: true, want: true },
    { name: "marks no head whose work runs", suspended: false, want: false },
  ])("$name", ({ suspended, want }) => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1", { suspended }), paused(), sc, "head").hasAttribute(MERGE_QUEUE_SUSPENDED_ATTRIBUTE)).toBe(want);
  });

  it("stops a paused head's clock at the pause", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = mergingHead("w1", { stages: [stage("tests", "live", 1, BigInt(NOW - 5_000))], headSinceMs: BigInt(NOW - 5_000) });
    const card = drawRosterMergeQueueHead(head, paused({ atMs: NOW - 2_000 }), sc, "head");
    ticker.tick(NOW + 60_000);
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatElapsed(3_000));
  });

  it("draws no clock for a pause whose instant was not recorded", () => {
    const { sc } = daemon();
    const head = mergingHead("w1", { stages: [stage("tests", "live", 1, BigInt(NOW - 5_000))] });
    expect(drawRosterMergeQueueHead(head, create(RosterMergeQueuePausedSchema, {}), sc, "head").querySelector(".sb-mq-clock")).toBeNull();
  });

  it("draws no detail line for a merging head with no step", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1"), undefined, sc, "head").querySelector(".sb-mq-detail")).toBeNull();
  });

  it.each([
    { name: "a merging head", standing: "merging" },
    { name: "a waiting head", standing: "waiting" },
  ])("states its standing for $name", ({ standing }) => {
    const { sc } = daemon();
    const value = standing === "waiting" ? { queuedAtMs: 0n } : {};
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: standing, value } as never });
    expect(drawRosterMergeQueueHead(head, undefined, sc, "head").getAttribute("data-standing")).toBe(standing);
  });

  it("ticks a merging head's clock from when it reached the head, not its first stage", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = mergingHead("w1", {
      stages: [stage("rebasing", "succeeded", 1, BigInt(NOW - 3 * 3_600_000)), stage("tests", "live", 1, BigInt(NOW - 1_000))],
      headSinceMs: BigInt(NOW - 5_000),
    });
    const card = drawRosterMergeQueueHead(head, undefined, sc, "head");
    ticker.tick(NOW + 60_000);
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(65_000));
  });

  it("ticks a waiting head's clock from when it reached the head, not when it was put in line", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = create(RosterMergeQueueHeadSchema, {
      subject: queueSubject("w1"),
      headSinceMs: BigInt(NOW - 2_000),
      standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW - 3 * 3_600_000) } },
    });
    const card = drawRosterMergeQueueHead(head, undefined, sc, "head");
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(2_000));
  });

});

describe("drawHeadClock", () => {
  it.each([
    { name: "ticks while the queue runs", pause: undefined, want: formatTickedElapsed(5_000) },
    { name: "stands at the pause while it is paused", pause: paused({ atMs: NOW - 1_000 }), want: formatElapsed(4_000) },
  ])("$name", ({ pause, want }) => {
    const sc = sidebarContext(appContext({}, fakeTicker(NOW)));
    expect(drawHeadClock(sc, NOW - 5_000, pause, "paused")?.textContent).toBe(want);
  });

  it("is not drawn for a pause whose instant was not recorded", () => {
    const { sc } = daemon();
    expect(drawHeadClock(sc, NOW - 5_000, create(RosterMergeQueuePausedSchema, {}), "paused")).toBeNull();
  });
});

describe("drawRosterMergeQueueHead's refusals", () => {
  it("refuses a head with no standing as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueueHead(create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1") }), undefined, sc, "head")).toThrow(MalformedView);
  });

  it("refuses a head with no subject as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueueHead(create(RosterMergeQueueHeadSchema, { standing: { case: "merging", value: {} } }), undefined, sc, "head")).toThrow(
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
  it("draws a bullet, its name and how long it has waited, and no place number", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1", { queuedAtMs: BigInt(NOW - 3_000), subject: queueSubject("w1", { name: "one" }) }), sc, "w");
    expect([...line.children].map((el) => [el.className, el.textContent])).toEqual([
      ["sb-mq-bullet", "•"],
      ["sb-mq-name", "one"],
      ["sb-mq-clock", formatTickedElapsed(3_000)],
    ]);
  });

  it("draws its bullet in its name's own color", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1"), sc, "w");
    const [bullet, name] = inRail(line, (rail) => [".sb-mq-bullet", ".sb-mq-name"].map((sel) => cascadedValue(rail.querySelector(sel)!, "color")));
    expect(bullet).toBe(name);
  });

  it.each([
    { name: "marks a merge put in line at the front", front: true, want: true },
    { name: "marks no other merge", front: false, want: false },
  ])("$name", ({ front, want }) => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueWaiting(waitingMerge("w1", { front }), sc, "w").querySelector(".sb-mq-front") !== null).toBe(want);
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
    drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { reveal: "bubble-1" }) }), undefined, sc, "head").click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id, "reveal" in r ? r.reveal?.value : undefined])).toEqual([
      ["agentrepl.v1.SelectWorkspaceRequest", "w1", "bubble-1"],
    ]);
  });

  it("selects as a row does when the merge has no bubble", async () => {
    const { asked, sc } = selecting();
    drawRosterMergeQueueWaiting(waitingMerge("w2"), sc, "w").click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id])).toEqual([["agentrepl.v1.SelectInEditorRequest", "w2"]]);
  });
});
