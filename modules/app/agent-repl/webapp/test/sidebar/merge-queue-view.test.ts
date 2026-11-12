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
  UpdateMergeQueueResponseSchema,
  type UpdateMergeQueueRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import {
  RosterMergeQueueGithubEntrySchema,
  RosterMergeQueueHeadSchema,
  RosterMergeQueuePausedSchema,
  RosterMergeQueueRemediatingReasonSchema,
  RosterMergeQueueRemediatingSchema,
  RosterMergeQueueStageSchema,
  RosterMergeQueueSubjectSchema,
  RosterMergeQueueWaitingPlacementSchema,
  RosterRepoMergeQueueSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { RepositoryRefSchema as WorkspaceRepositorySchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { formatAge, formatElapsed, formatTickedElapsed } from "../../src/duration.js";
import { roundSuffix, STATE_GLYPHS } from "../../src/feed/merge/tab-strip.js";
import { MERGE_GLYPH } from "../../src/merge-glyph.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawCardLine,
  drawHeadClock,
  drawRosterMergeQueue,
  drawRosterMergeQueueHead,
  drawQueuedGroupHead,
  drawRosterMergeQueueStage,
  drawRosterMergeQueueWaiting,
  MERGE_QUEUE_LABEL_CLASS,
  placementLabel,
  MERGE_QUEUE_CARD_LINE_CLASS,
  githubPlaceText,
  MERGE_QUEUE_CLASS,
  MERGE_QUEUE_GITHUB_CLASS,
  MERGE_QUEUE_HEAD_CLASS,
  MERGE_QUEUE_PAUSED_ATTRIBUTE,
  MERGE_QUEUE_REASON_CLASS,
  MERGE_QUEUE_REMEDIATING_CLASS,
  MERGE_QUEUE_SUSPENDED_ATTRIBUTE,
  MERGE_QUEUE_WAITING_CLASS,
  pausedText,
  queueDrag,
  queueHoverText,
  queueCount,
  queueLine,
} from "../../src/sidebar/merge-queue-view.js";
import { drawRosterMergeQueueSection } from "../../src/sidebar/roster.js";
import { BUCKET_ATTRIBUTE } from "../../src/sidebar/subsections.js";
import {
  appContext,
  conflictsReason,
  fakeTicker,
  mergeQueueBand,
  mergingHead,
  NOW,
  queueGroup,
  queueSubject,
  remediatingMerge,
  sidebarContext,
  testsReason,
  waitingMerge,
} from "./harness.js";

/** A sidebar context on a fake daemon. */
/** A workspace standing in a GitHub merge queue at PLACE of TOTAL since QUEUED_AT_MS. */
function githubStanding(id: string, place: number, total: number, queuedAtMs: number) {
  return create(RosterMergeQueueGithubEntrySchema, { subject: queueSubject(id), place: { place, total }, queuedAtMs: BigInt(queuedAtMs) });
}

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
    { name: "the user's pause", pause: () => paused({ atMs: NOW - 90_000 }), want: `Paused ${formatAge(90_000)} ago by you. Double-click to resume.` },
    { name: "a workspace's hard stop", pause: () => paused({ atMs: NOW - 90_000, workspace: "doom", hard: true }), want: `Stopped ${formatAge(90_000)} ago by doom. Double-click to resume.` },
    { name: "a pause recorded before its record was kept", pause: () => create(RosterMergeQueuePausedSchema, {}), want: "Paused. Double-click to resume." },
  ])("says $name", ({ pause, want }) => {
    expect(pausedText(pause(), NOW, "paused")).toBe(want);
  });

  it("refuses a pauser with no asker as a malformed view", () => {
    expect(() => pausedText(create(RosterMergeQueuePausedSchema, { by: {} }), NOW, "paused")).toThrow(MalformedView);
  });
});

describe("queueLine", () => {
  it("lists the head, then each merge remediating, then each waiting, by workspace", () => {
    const queue = create(RosterRepoMergeQueueSchema, {
      head: mergingHead("w1"),
      remediating: [remediatingMerge("w2", NOW)],
      waiting: [waitingMerge("w3"), waitingMerge("w4")],
    });
    expect(queueLine(queue)).toBe("w1 w2 w3 w4");
  });

  it("states an empty line for a queue drawing no merge", () => {
    expect(queueLine(create(RosterRepoMergeQueueSchema, {}))).toBe("");
  });
});

describe("githubPlaceText", () => {
  it("draws a place as its number of the queue's length", () => {
    expect(githubPlaceText({ place: 3, total: 5 } as never)).toBe("#3 of 5");
  });
});

describe("queueLine and queueCount with GitHub standings", () => {
  it("lists and counts a GitHub standing after the merges agent-repl runs", () => {
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), github: [githubStanding("w2", 3, 5, NOW)] });
    expect([queueLine(queue), queueCount(queue)]).toEqual(["w1 w2", 2]);
  });
});

describe("queueCount", () => {
  it("counts the head, those remediating and those waiting", () => {
    const queue = create(RosterRepoMergeQueueSchema, {
      head: mergingHead("w1"),
      remediating: [remediatingMerge("w2", NOW)],
      waiting: [waitingMerge("w3"), waitingMerge("w4")],
    });
    expect(queueCount(queue)).toBe(4);
  });

  it("counts an empty queue as none", () => {
    expect(queueCount(create(RosterRepoMergeQueueSchema, {}))).toBe(0);
  });
});

describe("the Merge Queue section's counts", () => {
  const countOf = (el: Element | null): string | undefined => el?.querySelector(".sb-open-count")?.textContent ?? undefined;

  function drawn() {
    const section = mergeQueueBand([
      queueGroup({ id: "a", queue: { head: mergingHead("w1"), waiting: [waitingMerge("w2")] } }),
      queueGroup({ id: "b", queue: { head: mergingHead("w3"), waiting: [waitingMerge("w4"), waitingMerge("w5")] } }),
    ]);
    return drawRosterMergeQueueSection(section, daemon().sc, "p");
  }

  it("shows the sum across repositories in the section header", () => {
    expect(countOf(drawn().querySelector(".repo-head"))).toBe("(5)");
  });

  it("shows each repository's own count in its group header", () => {
    const heads = [...drawn().querySelectorAll(".merged-group-head")];
    expect(heads.map(countOf)).toEqual(["(2)", "(3)"]);
  });
});

describe("queueHoverText", () => {
  it.each([
    { name: "a running queue's double click", pause: undefined, want: "Double-click to pause the queue." },
    { name: "a paused queue's record", pause: paused({ atMs: NOW, workspace: "doom" }), want: pausedText(paused({ atMs: NOW, workspace: "doom" }), NOW, "p") },
  ])("says $name", ({ pause, want }) => {
    expect(queueHoverText(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), paused: pause }), NOW, "queue")).toBe(want);
  });
});

describe("drawQueuedGroupHead", () => {
  const repository = { id: "repo-1", dir: "/r/repo-1" } as const;

  /** A queued group's box and header, the header made the queue's control. */
  function drawn(sc: ReturnType<typeof daemon>["sc"], pause?: ReturnType<typeof paused>): { group: HTMLElement; head: HTMLElement } {
    const group = document.createElement("div");
    const head = document.createElement("div");
    group.appendChild(head);
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), paused: pause });
    drawQueuedGroupHead(group, head, queue, create(WorkspaceRepositorySchema, repository), sc, "groups[0].activity.queued.queue");
    return { group, head };
  }

  it.each([
    { name: "a running queue", pause: undefined, want: null },
    { name: "a paused queue", pause: paused(), want: "pause" },
    { name: "a hard-stopped queue", pause: paused({ hard: true }), want: "hard" },
  ])("marks $name", ({ pause, want }) => {
    expect(drawn(daemon().sc, pause).group.getAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE)).toBe(want);
  });

  it.each([
    { name: "pauses a running queue", pause: undefined, want: "pause" },
    { name: "resumes a paused queue", pause: paused(), want: "resume" },
  ])("$name on a double click of its header", async ({ pause, want }) => {
    const { asked, sc } = queueDaemon();
    drawn(sc, pause).head.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    await settled();
    expect(
      asked.map((r) => [
        r.action.case,
        r.action.case === "resume" || r.action.case === "pause" ? r.action.value.repository?.id : undefined,
        r.requester?.asker.case,
      ]),
    ).toEqual([[want, "repo-1", "user"]]);
  });

  it("draws a refused resume at the header", async () => {
    const { sc } = queueDaemon(() =>
      create(UpdateMergeQueueResponseSchema, { result: { case: "error", value: { cause: { case: "notPaused", value: {} } } } }),
    );
    const { group, head } = drawn(sc, paused());
    head.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    await settled();
    expect(group.querySelector('.refusal[data-arm="notPaused"]') !== null || head.parentElement?.querySelector('.refusal[data-arm="notPaused"]') !== null).toBe(true);
  });

  it("asks nothing on a single click of its header", async () => {
    const { asked, sc } = queueDaemon();
    drawn(sc, paused()).head.click();
    await settled();
    expect(asked).toEqual([]);
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

  it("draws a merge remediating out of line between the head and the waiting merges", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, {
        head: mergingHead("w1"),
        remediating: [remediatingMerge("w2", NOW)],
        waiting: [waitingMerge("w3")],
      }),
      sc,
      "queue",
    );
    expect([...body.children].map((el) => el.className)).toEqual([
      MERGE_QUEUE_HEAD_CLASS,
      `${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_REMEDIATING_CLASS}`,
      MERGE_QUEUE_WAITING_CLASS,
    ]);
  });

  it("draws a queue whose line is empty while a merge remediates, with no head", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, {
        remediating: [remediatingMerge("w2", NOW)],
      }),
      sc,
      "queue",
    );
    expect([body.querySelector(`.${MERGE_QUEUE_HEAD_CLASS}`), body.querySelectorAll(`.${MERGE_QUEUE_REMEDIATING_CLASS}`).length]).toEqual([null, 1]);
  });

  it("draws a queue holding only GitHub merge queue standings, with no head", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { github: [githubStanding("w2", 3, 5, NOW)] }), sc, "queue");
    expect([body.querySelector(`.${MERGE_QUEUE_HEAD_CLASS}`), body.querySelectorAll(`.${MERGE_QUEUE_GITHUB_CLASS}`).length]).toEqual([null, 1]);
  });

  it("draws GitHub merge queue standings after the merges agent-repl runs", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), waiting: [waitingMerge("w3")], github: [githubStanding("w2", 3, 5, NOW)] }),
      sc,
      "queue",
    );
    expect([...body.children].map((el) => el.classList[0])).toEqual([MERGE_QUEUE_HEAD_CLASS, MERGE_QUEUE_WAITING_CLASS, MERGE_QUEUE_GITHUB_CLASS]);
  });

  it("draws a GitHub merge queue standing with its name, its place and how long it has stood there", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { github: [githubStanding("w2", 3, 5, NOW - 4_000)] }), sc, "queue");
    const line = body.querySelector(`.${MERGE_QUEUE_GITHUB_CLASS}`);
    expect([
      line?.querySelector(".sb-mq-name")?.textContent,
      line?.querySelector(".sb-mq-github-place")?.textContent,
      line?.querySelector(".sb-mq-clock")?.textContent,
    ]).toEqual(["w2", "#3 of 5", formatTickedElapsed(4_000)]);
  });

  it("refuses a GitHub merge queue standing with no place as a malformed view", () => {
    const { sc } = daemon();
    const placeless = create(RosterMergeQueueGithubEntrySchema, { subject: queueSubject("w2"), queuedAtMs: BigInt(NOW) });
    expect(() => drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { github: [placeless] }), sc, "queue")).toThrow(MalformedView);
  });

  it("gives a GitHub merge queue standing no grip", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), github: [githubStanding("w2", 2, 2, NOW)] }), sc, "queue");
    expect(queueDrag(sc, body, body.querySelector(`.${MERGE_QUEUE_GITHUB_CLASS}`), new Map())).toBeNull();
  });

  it("draws a remediating merge on one line: its name, what it repairs, then its clock from when it stepped aside", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, {
        head: mergingHead("w1"),
        remediating: [remediatingMerge("w2", NOW - 4_000)],
      }),
      sc,
      "queue",
    );
    const line = body.querySelector(`.${MERGE_QUEUE_REMEDIATING_CLASS}`)!;
    expect([...line.children].map((el) => [el.className, el.textContent])).toEqual([
      ["sb-mq-name", "w2"],
      [`sb-mq-label ${MERGE_QUEUE_REASON_CLASS}`, "fixing tests"],
      ["sb-mq-clock", formatTickedElapsed(4_000)],
    ]);
  });

  it.each([
    ["its failing tests", testsReason(), "fixing tests"],
    ["its rebase's conflicts", conflictsReason(), "resolving conflicts"],
    ["a reason this build does not name", create(RosterMergeQueueRemediatingReasonSchema, {}), undefined],
  ])("says what a remediating merge repairs after its name: %s", (_name, reason, want) => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), remediating: [remediatingMerge("w2", NOW, reason)] }),
      sc,
      "queue",
    );
    expect(body.querySelector(`.${MERGE_QUEUE_REMEDIATING_CLASS} .${MERGE_QUEUE_REASON_CLASS}`)?.textContent).toBe(want);
  });

  it("refuses a remediating merge that carries no reason as malformed", () => {
    const { sc } = daemon();
    const unreasoned = create(RosterMergeQueueRemediatingSchema, { subject: queueSubject("w2"), sinceMs: BigInt(NOW) });
    expect(() =>
      drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), remediating: [unreasoned] }), sc, "queue"),
    ).toThrow(MalformedView);
  });

  it("numbers no waiting merge: each opens with its name", () => {
    const { sc } = daemon();
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), waiting: [waitingMerge("w1"), waitingMerge("w2")] });
    const body = drawRosterMergeQueue(queue, sc, "queue");
    expect([...body.querySelectorAll(`.${MERGE_QUEUE_WAITING_CLASS}`)].map((el) => [...el.children].slice(0, 1).map((child) => child.textContent))).toEqual([
      ["w1"],
      ["w2"],
    ]);
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
    { name: "a merging head", head: () => mergingHead("w1", { stages: [stage("queue", "succeeded"), stage("tests", "live")] }), pause: undefined, want: ["sb-mq-head-line", "sb-mq-stages"] },
    { name: "a merging head on a paused queue", head: () => mergingHead("w1", { stages: [stage("tests", "live")] }), pause: paused(), want: ["sb-mq-head-line", "sb-mq-stages"] },
    { name: "a merging head on a stopped queue", head: () => mergingHead("w1", { stages: [stage("tests", "live")] }), pause: paused({ hard: true }), want: ["sb-mq-head-line", "sb-mq-stages"] },
    {
      name: "a waiting head",
      head: () => create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW) } } }),
      pause: undefined,
      want: ["sb-mq-head-line"],
    },
    {
      name: "a waiting head on a paused queue",
      head: () => create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), standing: { case: "waiting", value: { queuedAtMs: BigInt(NOW) } } }),
      pause: paused(),
      want: ["sb-mq-head-line"],
    },
  ])("draws no status line under $name: its card is its name line and its stages", ({ head, pause, want }) => {
    const { sc } = daemon();
    expect([...drawRosterMergeQueueHead(head(), pause, sc, "head").children].map((el) => el.className)).toEqual(want);
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
  it("draws its name and how long it has waited, and no bullet or place number", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1", { queuedAtMs: BigInt(NOW - 3_000), subject: queueSubject("w1", { name: "one" }) }), sc, "w");
    expect([...line.children].map((el) => [el.className, el.textContent])).toEqual([
      ["sb-mq-name", "one"],
      ["sb-mq-clock", formatTickedElapsed(3_000)],
    ]);
  });

  it("ticks a racing merge's clock from when it took its racing slot, not when it was put in line", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(
      waitingMerge("w1", { queuedAtMs: BigInt(NOW - 3_600_000), racingSinceMs: BigInt(NOW - 4_000) }),
      sc,
      "w",
    );
    expect(line.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(4_000));
  });

  it.each([
    { name: "marks a merge the user put at the front front", init: { front: true }, want: "front" },
    { name: "marks a merge back from a repair returned", init: { returned: true }, want: "returned" },
    { name: "marks a merge put in line at the back with no label", init: {}, want: null },
  ])("$name", ({ init, want }) => {
    const { sc } = daemon();
    const labels = [...drawRosterMergeQueueWaiting(waitingMerge("w1", init), sc, "w").querySelectorAll(`.${MERGE_QUEUE_LABEL_CLASS}`)];
    expect(labels.map((label) => label.textContent)).toEqual(want === null ? [] : [want]);
  });

  it("draws no label for a placement this build does not name", () => {
    expect(placementLabel(create(RosterMergeQueueWaitingPlacementSchema, {}))).toBeNull();
  });

  it("draws the returned label as a peer of the front label, in the queue label's class", () => {
    const { sc } = daemon();
    const front = drawRosterMergeQueueWaiting(waitingMerge("w1", { front: true }), sc, "w").querySelector(".sb-mq-front");
    const returned = drawRosterMergeQueueWaiting(waitingMerge("w1", { returned: true }), sc, "w").querySelector(".sb-mq-returned");
    expect(front?.classList.contains(MERGE_QUEUE_LABEL_CLASS)).toBe(true);
    expect(returned?.classList.contains(MERGE_QUEUE_LABEL_CLASS)).toBe(true);
    expect(returned?.classList.contains("sb-mq-front")).toBe(false);
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

  // OWNER RULING: a select is idempotent, so its line never goes busy; a line
  // the roster keeps (its wire unchanged by the select) stays clickable.
  it.each([
    ["the head card", (sc: ReturnType<typeof selecting>["sc"]) => drawRosterMergeQueueHead(mergingHead("w1"), undefined, sc, "head")],
    [
      "a remediating card",
      (sc: ReturnType<typeof selecting>["sc"]) =>
        drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { remediating: [remediatingMerge("w2", NOW)] }), sc, "queue").querySelector(
          `.${MERGE_QUEUE_REMEDIATING_CLASS}`,
        ) as HTMLElement,
    ],
    ["a waiting line", (sc: ReturnType<typeof selecting>["sc"]) => drawRosterMergeQueueWaiting(waitingMerge("w3"), sc, "w")],
    [
      "a head card opening its bubble",
      (sc: ReturnType<typeof selecting>["sc"]) =>
        drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { reveal: "bubble-1" }) }), undefined, sc, "head"),
    ],
  ])("leaves %s clickable after it selects", async (_name, draw) => {
    // Arrange
    const { asked, sc } = selecting();
    const line = draw(sc);

    // Act
    line.click();
    await settled();
    line.click();
    await settled();

    // Assert
    expect([line.classList.contains("is-busy"), asked.length]).toEqual([false, 2]);
  });

  it("never greys a line while its select is in flight", async () => {
    const { sc } = selecting();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w3"), sc, "w");
    line.click();
    const busy = line.classList.contains("is-busy");
    await settled();
    expect(busy).toBe(false);
  });

  it("selects as a row does when the merge has no bubble", async () => {
    const { asked, sc } = selecting();
    drawRosterMergeQueueWaiting(waitingMerge("w2"), sc, "w").click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id])).toEqual([["agentrepl.v1.SelectInEditorRequest", "w2"]]);
  });
});

describe("drawCardLine", () => {
  it("draws the merge glyph, hidden from assistive tech, then the credited name", () => {
    const line = drawCardLine(queueSubject("w1", { name: "one" }), "subject");
    expect([line.className, [...line.children].map((el) => [el.className, el.textContent, el.getAttribute("aria-hidden")])]).toEqual([
      MERGE_QUEUE_CARD_LINE_CLASS,
      [
        ["sb-mq-glyph", MERGE_GLYPH, "true"],
        ["sb-mq-name", "one", null],
      ],
    ]);
  });

  it("refuses a subject that carries no mode as malformed", () => {
    expect(() => drawCardLine(create(RosterMergeQueueSubjectSchema, { workspace: { id: "w1", dir: "/w/w1" }, name: "one" }), "subject")).toThrow(MalformedView);
  });

  it.each([
    { name: "the head's card", card: () => drawRosterMergeQueueHead(mergingHead("w1"), undefined, daemon().sc, "head") },
  ])("opens $name with the shared card line", ({ card }) => {
    const shared = drawCardLine(queueSubject("w1"), "subject");
    const drawn = card().firstElementChild as HTMLElement;
    expect([...drawn.children].slice(0, 2).map((el) => el.outerHTML)).toEqual([...shared.children].map((el) => el.outerHTML));
  });
});
