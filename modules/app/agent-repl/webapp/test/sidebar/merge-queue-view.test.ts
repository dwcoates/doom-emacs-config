// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
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
import { RequeueMergeResponseSchema, type RequeueMergeRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_requeue_merge_pb";
import { DiscardMergeResponseSchema, type DiscardMergeRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_discard_merge_pb";
import {
  RosterMergeQueueGithubEntrySchema,
  RosterMergeQueueOffQueueSchema,
  RosterMergeQueueHeadClockSchema,
  RosterMergeQueueHeadSchema,
  RosterMergeQueueMediationSchema,
  RosterMergeQueuePausedSchema,
  RosterMergeQueueFixingTestsSchema,
  RosterMergeQueueStageSchema,
  RosterMergeQueueSubjectSchema,
  RosterMergeQueueWaitingSchema,
  RosterRepoMergeQueueSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { RepositoryRefSchema as WorkspaceRepositorySchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { formatAge, formatElapsed, formatTickedElapsed } from "../../src/duration.js";
import { roundSuffix, STATE_GLYPHS } from "../../src/feed/merge/tab-strip.js";
import { MERGE_GLYPH } from "../../src/merge-glyph.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord, forwardedRecords, recordFor } from "../log-capture.js";
import { UPDATE_MERGE_QUEUE_SENTENCES } from "../../src/rpc/merge-queue-refusal.js";
import {
  activeClockStartMs,
  drawCardLine,
  drawHeadClock,
  headClockStartMs,
  waitingClockStartMs,
  drawQueueControlLine,
  MERGE_QUEUE_CONTROL_CLASS,
  MERGE_QUEUE_GLYPH_CLASS,
  MERGE_QUEUE_RELEASE_CLASS,
  drawEscalationRelease,
  queueGlyphTitle,
  queueSwitchOf,
  toggleQueue,
  drawRosterMergeQueue,
  drawRosterMergeQueueHead,
  drawQueueMediationLabel,
  drawQueuedGroupHead,
  drawRosterMergeQueueStage,
  drawRosterMergeQueueWaiting,
  MERGE_QUEUE_CARD_LINE_CLASS,
  githubPlaceText,
  MERGE_QUEUE_CLASS,
  MERGE_QUEUE_GITHUB_CLASS,
  MERGE_QUEUE_ASYNC_OF_CLASS,
  MERGE_QUEUE_ASYNC_OF_LINK_CLASS,
  MERGE_QUEUE_OFF_QUEUE_CLASS,
  MERGE_QUEUE_OFF_QUEUE_RULE_CLASS,
  COLD_GATED_REQUEUE_TITLE,
  MERGE_QUEUE_HEAD_CLASS,
  MERGE_QUEUE_MEDIATION_CLASS,
  MERGE_QUEUE_NEEDS_YOU_CLASS,
  MERGE_QUEUE_PAUSED_ATTRIBUTE,
  MERGE_QUEUE_REASON_CLASS,
  MERGE_QUEUE_REMEDIATING_CLASS,
  MERGE_QUEUE_WAITING_CLASS,
  MERGE_QUEUE_POSITION_CLASS,
  pausedText,
  queueDrag,
  waitingMergeRef,
  queueHoverText,
  queueCount,
  queueLine,
  MERGE_QUEUE_SUBJECT_CLASS,
} from "../../src/sidebar/merge-queue-view.js";
import { drawRosterMergeQueueSection } from "../../src/sidebar/roster.js";
import { COLD_GATED_NAME_CLASS, NONE_COLD_GATED } from "../../src/sidebar/cold-gate-name.js";
import { cascadedValue, inRail } from "../stylesheet.js";
import { BUCKET_ATTRIBUTE } from "../../src/sidebar/subsections.js";
import {
  activeClock,
  activeHeadClock,
  appContext,
  fakeTicker,
  mergeQueueBand,
  mergingHead,
  NOW,
  queueGroup,
  queueSubject,
  fixingTestsMerge,
  resolvingConflictsMerge,
  awaitingFixingTestsMerge,
  sidebarContext,
  waitingClock,
  waitingHead,
  waitingHeadClock,
  waitingMerge,
} from "./harness.js";

/** A sidebar context on a fake daemon. */
/** A workspace standing in a GitHub merge queue at PLACE of TOTAL since QUEUED_AT_MS. */
function githubStanding(id: string, place: number, total: number, queuedAtMs: number) {
  return create(RosterMergeQueueGithubEntrySchema, { subject: queueSubject(id), place: { place, total }, queuedAtMs: BigInt(queuedAtMs) });
}

/** The repository a test's queue group is drawn for. */
const REPOSITORY = { key: create(WorkspaceRepositorySchema, { id: "repo-1", dir: "/r/repo-1" }), name: "doom" };

/** A stand-in for the queue's glyph, for a card drawn on its own. */
function glyph(): HTMLElement {
  const el = document.createElement("span");
  el.className = "test-glyph";
  el.textContent = MERGE_GLYPH;
  return el;
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
    {
      name: "the host's pause for its fault",
      pause: () => create(RosterMergeQueuePausedSchema, { by: { asker: { case: "host", value: { cause: "too many open files in system" } } } }),
      want: "Paused by the host, which could not run a test gate: too many open files in system. Double-click to resume.",
    },
  ])("says $name", ({ pause, want }) => {
    expect(pausedText(pause(), NOW, "paused")).toBe(want);
  });

  it("refuses a pauser with no asker as a malformed view", () => {
    expect(() => pausedText(create(RosterMergeQueuePausedSchema, { by: {} }), NOW, "paused")).toThrow(MalformedView);
  });
});

describe("queueLine", () => {
  it("lists the head, then fixing tests, every merge resolving conflicts and those awaiting fixing tests, then each waiting, by workspace", () => {
    const queue = create(RosterRepoMergeQueueSchema, {
      head: mergingHead("w1"),
      fixingTests: fixingTestsMerge("w2", NOW),
      resolvingConflicts: [resolvingConflictsMerge("w3", NOW), resolvingConflictsMerge("w4", NOW)],
      awaitingFixingTests: [awaitingFixingTestsMerge("w5", NOW)],
      waiting: [waitingMerge("w6"), waitingMerge("w7")],
    });
    expect(queueLine(queue)).toBe("w1 w2 w3 w4 w5 w6 w7");
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
  it("counts the head, those at or awaiting a repair slot and those waiting", () => {
    const queue = create(RosterRepoMergeQueueSchema, {
      head: mergingHead("w1"),
      fixingTests: fixingTestsMerge("w2", NOW),
      awaitingFixingTests: [awaitingFixingTestsMerge("w5", NOW)],
      waiting: [waitingMerge("w3"), waitingMerge("w4")],
    });
    expect(queueCount(queue)).toBe(5);
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
    return drawRosterMergeQueueSection(section, daemon().sc, "p", NONE_COLD_GATED);
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

/** The ask a recorded UpdateMergeQueue request makes: its action, the repository it addresses, who asks. */
function askOf(r: UpdateMergeQueueRequest): [string | undefined, string | undefined, string | undefined] {
  return [
    r.action.case,
    r.action.case === "resume" || r.action.case === "pause" ? r.action.value.repository?.id : undefined,
    r.requester?.asker.case,
  ];
}

/** A context recording every UpdateMergeQueue ask and every select, each answered with success. */
function glyphDaemon() {
  const asked: UpdateMergeQueueRequest[] = [];
  const selected: string[] = [];
  const sc = sidebarContext(
    appContext({
      updateMergeQueue: (request) => {
        asked.push(request);
        return create(UpdateMergeQueueResponseSchema, { result: { case: "success", value: {} } });
      },
      selectWorkspace: (request) => {
        selected.push(request.$typeName);
        return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
      selectInEditor: (request) => {
        selected.push(request.$typeName);
        return create(SelectInEditorResponseSchema, { result: { case: "success", value: { route: { case: "relayed", value: {} } } } });
      },
    }),
  );
  return { asked, selected, sc };
}

/** The glyph of the queue drawn in BODY. */
function glyphIn(body: Element): HTMLElement {
  const glyph = body.querySelector<HTMLElement>(`.${MERGE_QUEUE_GLYPH_CLASS}`);
  if (glyph === null) throw new Error("no queue glyph drawn");
  return glyph;
}

describe("queueSwitchOf", () => {
  it.each([
    { name: "pauses a running queue", pause: undefined, want: "pause" },
    { name: "resumes a paused queue", pause: paused(), want: "resume" },
    { name: "resumes a hard-stopped queue", pause: paused({ hard: true }), want: "resume" },
  ])("$name", ({ pause, want }) => {
    expect(queueSwitchOf(create(RosterRepoMergeQueueSchema, { paused: pause }))).toBe(want);
  });
});

describe("toggleQueue", () => {
  it.each([
    { name: "pauses a running queue", pause: undefined, want: "pause" },
    { name: "resumes a paused queue", pause: paused(), want: "resume" },
  ])("$name for the user", async ({ pause, want }) => {
    const { asked, sc } = queueDaemon();
    await toggleQueue(document.createElement("div"), sc, REPOSITORY.key, create(RosterRepoMergeQueueSchema, { paused: pause }));
    expect(asked.map(askOf)).toEqual([[want, "repo-1", "user"]]);
  });

  it("answers whether the switch took", async () => {
    const { sc } = queueDaemon(() =>
      create(UpdateMergeQueueResponseSchema, { result: { case: "error", value: { cause: { case: "notPaused", value: {} } } } }),
    );
    await expect(toggleQueue(document.createElement("div"), sc, REPOSITORY.key, create(RosterRepoMergeQueueSchema, { paused: paused() }))).resolves.toBe(false);
  });
});

describe("the escalation's release", () => {
  it("is a named button", () => {
    // Act
    const release = drawEscalationRelease(REPOSITORY.key, queueDaemon().sc);

    // Assert
    expect([release.getAttribute("role"), release.textContent, release.classList.contains(MERGE_QUEUE_RELEASE_CLASS)]).toEqual(["button", "release", true]);
  });

  it("asks the daemon to release the repository's escalation for the user on a click", async () => {
    // Arrange
    const { asked, sc } = queueDaemon();
    const release = drawEscalationRelease(REPOSITORY.key, sc);
    document.body.appendChild(release);

    // Act
    release.click();
    await settled();

    // Assert
    expect(asked.map((r) => [r.action.case, r.action.case === "releaseEscalation" ? r.action.value.repository?.id : undefined, r.requester?.asker.case])).toEqual([
      ["releaseEscalation", "repo-1", "user"],
    ]);
    release.remove();
  });

  it("words the no-escalation-held refusal", () => {
    expect(UPDATE_MERGE_QUEUE_SENTENCES.noEscalationHeld()).toBe("no escalation holds this queue now");
  });
});

describe("queueGlyphTitle", () => {
  it.each([
    { to: "pause" as const, want: "Pause the doom queue" },
    { to: "resume" as const, want: "Resume the doom queue" },
  ])("says what a click does to $to", ({ to, want }) => {
    expect(queueGlyphTitle("doom", to)).toBe(want);
  });
});

describe("the queue's merge glyph", () => {
  /** A queue of HEAD drawn for REPOSITORY, paused by PAUSE. */
  function body(sc: ReturnType<typeof glyphDaemon>["sc"], pause?: ReturnType<typeof paused>): HTMLElement {
    return drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), paused: pause }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
  }

  it.each([
    { name: "a running queue", pause: undefined, want: "Pause the doom queue" },
    { name: "a paused queue", pause: paused(), want: "Resume the doom queue" },
  ])("is a named button on $name", ({ pause, want }) => {
    const glyph = glyphIn(body(glyphDaemon().sc, pause));
    expect([glyph.getAttribute("role"), glyph.getAttribute("aria-label"), glyph.title, glyph.hasAttribute("aria-hidden"), glyph.textContent]).toEqual([
      "button",
      want,
      want,
      false,
      MERGE_GLYPH,
    ]);
  });

  it.each([
    { name: "pauses a running queue", pause: undefined, want: "pause" },
    { name: "resumes a paused queue", pause: paused(), want: "resume" },
  ])("$name on a click", async ({ pause, want }) => {
    const { asked, sc } = glyphDaemon();
    glyphIn(body(sc, pause)).click();
    await settled();
    expect(asked.map(askOf)).toEqual([[want, "repo-1", "user"]]);
  });

  it("selects no workspace on a click", async () => {
    const { selected, sc } = glyphDaemon();
    glyphIn(body(sc)).click();
    await settled();
    expect(selected).toEqual([]);
  });

  it("addresses its own repository's queue with two groups drawn", async () => {
    const { asked, sc } = glyphDaemon();
    const section = drawRosterMergeQueueSection(
      mergeQueueBand([
        queueGroup({ id: "a", queue: { head: mergingHead("w1") } }),
        queueGroup({ id: "b", queue: { head: mergingHead("w2"), paused: paused() } }),
      ]),
      sc,
      "p",
      NONE_COLD_GATED,
    );
    const glyphs = [...section.querySelectorAll<HTMLElement>(`.${MERGE_QUEUE_GLYPH_CLASS}`)];
    glyphs[1]?.click();
    await settled();
    expect([glyphs.map((g) => g.title), asked.map(askOf)]).toEqual([
      ["Pause the a queue", "Resume the b queue"],
      [["resume", "b", "user"]],
    ]);
  });

  it("asks what the header's double click asks", async () => {
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), paused: paused() });
    const byGlyph = glyphDaemon();
    glyphIn(drawRosterMergeQueue(queue, REPOSITORY, byGlyph.sc, "queue", NONE_COLD_GATED)).click();
    const byHeader = glyphDaemon();
    const head = document.createElement("div");
    drawQueuedGroupHead(document.createElement("div"), head, queue, REPOSITORY.key, byHeader.sc, "queue");
    head.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    await settled();
    expect(byGlyph.asked).toEqual(byHeader.asked);
  });

  it("draws a refused switch under its line", async () => {
    const { sc } = queueDaemon(() =>
      create(UpdateMergeQueueResponseSchema, { result: { case: "error", value: { cause: { case: "notPaused", value: {} } } } }),
    );
    const drawn = body(sc, paused());
    glyphIn(drawn).click();
    await settled();
    const line = drawn.querySelector(`.${MERGE_QUEUE_CARD_LINE_CLASS}`);
    expect(line?.nextElementSibling?.matches('.refusal[data-arm="notPaused"]')).toBe(true);
  });

  it("records a refused switch with its repository and action", async () => {
    const capture = captureLogRecords();
    const { sc } = queueDaemon(() =>
      create(UpdateMergeQueueResponseSchema, { result: { case: "error", value: { cause: { case: "notPaused", value: {} } } } }),
    );
    glyphIn(body(sc, paused())).click();
    await settled();
    const record = await forwardedRecord(capture, "sidebar.verbs.refused");
    expect([record.level.case, record.context]).toMatchObject(["warn", { rpc: "UpdateMergeQueue", arm: "notPaused", repository: "repo-1", action: "resume" }]);
  });

  it.each([
    { name: "only a merge remediating", queue: () => ({ fixingTests: fixingTestsMerge("w2", NOW) }) },
    { name: "only a GitHub merge queue standing", queue: () => ({ github: [githubStanding("w2", 3, 5, NOW)] }) },
  ])("is drawn on a line of its own for a queue holding $name", ({ queue }) => {
    const drawn = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, queue()), REPOSITORY, glyphDaemon().sc, "queue", NONE_COLD_GATED);
    const first = drawn.firstElementChild;
    expect([first?.className, first?.firstElementChild?.classList.contains(MERGE_QUEUE_GLYPH_CLASS)]).toEqual([
      `${MERGE_QUEUE_CARD_LINE_CLASS} ${MERGE_QUEUE_CONTROL_CLASS}`,
      true,
    ]);
  });

  it("resumes a paused queue with no head on a click", async () => {
    const { asked, sc } = glyphDaemon();
    const drawn = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, { fixingTests: fixingTestsMerge("w2", NOW), paused: paused() }),
      REPOSITORY,
      sc,
      "queue",
      NONE_COLD_GATED,
    );
    glyphIn(drawn).click();
    await settled();
    expect(asked.map(askOf)).toEqual([["resume", "repo-1", "user"]]);
  });
});

describe("drawQueueControlLine", () => {
  it("draws the glyph it is given alone on a card line", () => {
    const line = drawQueueControlLine(glyph());
    expect([line.className, [...line.children].map((el) => el.className)]).toEqual([
      `${MERGE_QUEUE_CARD_LINE_CLASS} ${MERGE_QUEUE_CONTROL_CLASS}`,
      ["test-glyph"],
    ]);
  });
});

/** A stage of LABEL at ROUND in STATE, begun at START and ended at END. */
function stage(label: string, state: "live" | "held" | "succeeded" | "failed", round = 1, start = 1_000n, end = 4_000n) {
  return create(RosterMergeQueueStageSchema, {
    label,
    round,
    state:
      state === "live"
        ? { case: "live", value: { startedAtMs: start } }
        : state === "held"
          ? { case: "held", value: { startedAtMs: start, heldSinceMs: end } }
          : { case: state, value: { startedAtMs: start, endedAtMs: end } },
  });
}

describe("drawRosterMergeQueue", () => {
  it("draws the head's card, then each waiting merge, front first", () => {
    const { sc } = daemon();
    const queue = create(RosterRepoMergeQueueSchema, { head: mergingHead("w3"), waiting: [waitingMerge("w1"), waitingMerge("w2")] });
    const body = drawRosterMergeQueue(queue, REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([...body.children].map((el) => [el.className, el.getAttribute("data-workspace")])).toEqual([
      [`${MERGE_QUEUE_HEAD_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`, "w3"],
      [`${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`, "w1"],
      [`${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`, "w2"],
    ]);
  });

  it("numbers the waiting merges by their order in the line, skipping the head and a remediating merge", () => {
    const { sc } = daemon();
    const draw = (order: string[]) =>
      [
        ...drawRosterMergeQueue(
          create(RosterRepoMergeQueueSchema, {
            head: mergingHead("w0"),
            fixingTests: fixingTestsMerge("wr", NOW),
            waiting: order.map((id) => waitingMerge(id)),
          }),
          REPOSITORY,
          sc,
          "queue",
          NONE_COLD_GATED,
        ).children,
      ].map((el) => [el.getAttribute("data-workspace"), el.querySelector(`.${MERGE_QUEUE_POSITION_CLASS}`)?.textContent ?? null]);
    expect(draw(["w1", "w2", "w3"])).toEqual([["w0", null], ["wr", null], ["w1", "1"], ["w2", "2"], ["w3", "3"]]);
    expect(draw(["w3", "w1", "w2"])).toEqual([["w0", null], ["wr", null], ["w3", "1"], ["w1", "2"], ["w2", "3"]]);
  });

  it("is a rows box no grouping drag arranges", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1") }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([body.classList.contains("rows"), body.classList.contains(MERGE_QUEUE_CLASS), body.hasAttribute(BUCKET_ATTRIBUTE)]).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("refuses a queue with no head as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, {}), REPOSITORY, sc, "queue", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it("draws the merges out of line between the head and the waiting merges: fixing tests, every merge resolving conflicts, then those awaiting fixing tests", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, {
        head: mergingHead("w1"),
        fixingTests: fixingTestsMerge("w2", NOW),
        resolvingConflicts: [resolvingConflictsMerge("w3", NOW), resolvingConflictsMerge("w4", NOW)],
        awaitingFixingTests: [awaitingFixingTestsMerge("w5", NOW)],
        waiting: [waitingMerge("w6")],
      }),
      REPOSITORY,
      sc,
      "queue",
      NONE_COLD_GATED,
    );
    const out = `${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_REMEDIATING_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`;
    expect([...body.children].map((el) => [el.className, el.querySelector(".sb-mq-name")?.textContent])).toEqual([
      [`${MERGE_QUEUE_HEAD_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`, "w1"],
      [out, "w2"],
      [out, "w3"],
      [out, "w4"],
      [out, "w5"],
      [`${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`, "w6"],
    ]);
  });

  it("marks every line credited to a workspace, of every kind, as a subject line the selection highlight is laid over", () => {
    // Arrange
    const { sc } = offQueueDaemon();
    const queue = create(RosterRepoMergeQueueSchema, {
      head: mergingHead("w1"),
      fixingTests: fixingTestsMerge("w2", NOW),
      resolvingConflicts: [resolvingConflictsMerge("w3", NOW)],
      awaitingFixingTests: [awaitingFixingTestsMerge("w4", NOW)],
      waiting: [waitingMerge("w5")],
      github: [githubStanding("w6", 1, 2, NOW)],
      offQueue: [offQueue("w7", "evicted")],
    });
    // Act
    const body = drawRosterMergeQueue(queue, REPOSITORY, sc, "queue", NONE_COLD_GATED);
    // Assert: every workspace line wears the class, and nothing else does.
    expect([...body.querySelectorAll(`.${MERGE_QUEUE_SUBJECT_CLASS}`)].map((el) => el.getAttribute("data-workspace"))).toEqual([
      "w1", "w2", "w3", "w4", "w5", "w6", "w7",
    ]);
  });

  it("draws a queue whose line is empty while a merge stands out of line, with no head", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, {
        resolvingConflicts: [resolvingConflictsMerge("w2", NOW)],
      }),
      REPOSITORY,
      sc,
      "queue",
      NONE_COLD_GATED,
    );
    expect([body.querySelector(`.${MERGE_QUEUE_HEAD_CLASS}`), body.querySelectorAll(`.${MERGE_QUEUE_REMEDIATING_CLASS}`).length]).toEqual([null, 1]);
  });

  it("draws a queue holding only GitHub merge queue standings, with no head", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { github: [githubStanding("w2", 3, 5, NOW)] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([body.querySelector(`.${MERGE_QUEUE_HEAD_CLASS}`), body.querySelectorAll(`.${MERGE_QUEUE_GITHUB_CLASS}`).length]).toEqual([null, 1]);
  });

  it("draws GitHub merge queue standings after the merges agent-repl runs", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), waiting: [waitingMerge("w3")], github: [githubStanding("w2", 3, 5, NOW)] }),
      REPOSITORY,
      sc,
      "queue",
      NONE_COLD_GATED,
    );
    expect([...body.children].map((el) => el.classList[0])).toEqual([MERGE_QUEUE_HEAD_CLASS, MERGE_QUEUE_WAITING_CLASS, MERGE_QUEUE_GITHUB_CLASS]);
  });

  it("draws a GitHub merge queue standing with its name, its place and how long it has stood there", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { github: [githubStanding("w2", 3, 5, NOW - 4_000)] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
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
    expect(() => drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { github: [placeless] }), REPOSITORY, sc, "queue", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it("gives a GitHub merge queue standing no grip", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), github: [githubStanding("w2", 2, 2, NOW)] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect(queueDrag(sc, body, body.querySelector(`.${MERGE_QUEUE_GITHUB_CLASS}`), new Map())).toBeNull();
  });

  it("draws a merge out of line on one line: its name, its repair, then its clock from when it took or began awaiting its slot", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, {
        head: mergingHead("w1"),
        fixingTests: fixingTestsMerge("w2", NOW - 4_000),
      }),
      REPOSITORY,
      sc,
      "queue",
      NONE_COLD_GATED,
    );
    const line = body.querySelector(`.${MERGE_QUEUE_REMEDIATING_CLASS}`)!;
    expect([...line.children].map((el) => [el.className, el.textContent])).toEqual([
      ["sb-mq-name", "w2"],
      [`sb-mq-label ${MERGE_QUEUE_REASON_CLASS}`, "fixing tests"],
      ["sb-mq-clock", formatTickedElapsed(4_000)],
    ]);
  });

  it.each([
    ["the fixing-tests slot's holder", { fixingTests: fixingTestsMerge("w2", NOW) }, "fixing tests"],
    ["a merge resolving conflicts", { resolvingConflicts: [resolvingConflictsMerge("w2", NOW)] }, "resolving conflicts"],
    ["a merge awaiting the fixing-tests slot", { awaitingFixingTests: [awaitingFixingTestsMerge("w2", NOW)] }, "waiting to fix tests"],
  ])("says the repair after the name of %s", (_name, slots, want) => {
    const { sc } = daemon();

    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), ...slots }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect(body.querySelector(`.${MERGE_QUEUE_REMEDIATING_CLASS} .${MERGE_QUEUE_REASON_CLASS}`)?.textContent).toBe(want);
  });

  it.each([
    { name: "the merge fixing its tests, from when it became active", slots: () => ({ fixingTests: fixingTestsMerge("w2", NOW - 4_000) }), want: 4_000 },
    { name: "a merge resolving conflicts, from when it became active", slots: () => ({ resolvingConflicts: [resolvingConflictsMerge("w2", NOW - 6_000)] }), want: 6_000 },
    {
      name: "a merge awaiting the fixing-tests slot, with the waiting before its active stretch",
      slots: () => ({ awaitingFixingTests: [awaitingFixingTestsMerge("w2", NOW - 2_000, 40 * 60_000)] }),
      want: 40 * 60_000 + 2_000,
    },
  ])("counts the clock of $name", ({ slots, want }) => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), ...slots() }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect(body.querySelector(`.${MERGE_QUEUE_REMEDIATING_CLASS} .sb-mq-clock`)?.textContent).toBe(formatTickedElapsed(want));
  });

  it("refuses a repair slot's holder that carries no clock as malformed", () => {
    const { sc } = daemon();
    const unclocked = create(RosterMergeQueueFixingTestsSchema, { subject: queueSubject("w2") });
    expect(() =>
      drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), fixingTests: unclocked }), REPOSITORY, sc, "queue", NONE_COLD_GATED),
    ).toThrow(MalformedView);
  });

  it("refuses a repair slot's holder that carries no subject as malformed", () => {
    const { sc } = daemon();
    const subjectless = create(RosterMergeQueueFixingTestsSchema, { clock: activeClock(NOW) });
    expect(() =>
      drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), fixingTests: subjectless }), REPOSITORY, sc, "queue", NONE_COLD_GATED),
    ).toThrow(MalformedView);
  });

});

/** A click target selecting workspace ID. */
function target(id: string) {
  return { workspace: { id, dir: `/wt/${id}` } };
}

/** The deploy check the queue waits on: CHECKING once the intake was told of the deploy. */
function mediation(checking: boolean) {
  return create(RosterMergeQueueMediationSchema, {
    standing: checking ? { case: "checking", value: { target: target("intake") } } : { case: "waiting", value: {} },
  });
}

describe("drawQueueMediationLabel", () => {
  /** A context recording every SelectWorkspace ask. */
  /** A context recording every select a click asks of the editor or the daemon. */
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

  it.each([
    ["hot-loading zettel", { case: "waiting" as const, value: {} },
      "Zettel is hot-loading the last landing; its health is assessed once it has, and the queue waits for it"],
    ["assessing zettel health", { case: "checking" as const, value: { target: target("intake") } },
      "Zettel's health after the last landing is being assessed; the queue waits for it"],
    ["fixing zettel", { case: "fixing" as const, value: { target: target("remedy") } },
      "the fixer is fixing the issues the last landing caused in Zettel; the queue waits for its fix"],
    ["testing zettel fixes", { case: "testing" as const, value: { target: target("remedy") } },
      "a remediation's fix is running its full gate, then lands; the queue waits for it"],
    ["zettel needs you", { case: "escalated" as const, value: { target: target("escalation") } },
      "a deploy check was escalated to you; the queue waits until its fix lands or you release it"],
  ])("reads %s, titled to match, for its standing", (want, standing, title) => {
    const label = drawQueueMediationLabel(create(RosterMergeQueueMediationSchema, { standing }), daemon().sc, "m");
    expect([label.textContent, label.title, label.classList.contains(MERGE_QUEUE_MEDIATION_CLASS)]).toEqual([want, title, true]);
  });

  it.each([
    ["a rollout's label selects the chain's intake", { case: "waiting" as const, value: { target: target("intake") } }, "intake"],
    ["an assessment's label selects the intake", { case: "checking" as const, value: { target: target("intake") } }, "intake"],
    ["a fix's label selects the remediation", { case: "fixing" as const, value: { target: target("remedy") } }, "remedy"],
    ["a fix under test's label selects the workspace standing by", { case: "testing" as const, value: { target: target("remedy") } }, "remedy"],
    ["an escalated check's label selects its escalation", { case: "escalated" as const, value: { target: target("escalation") } }, "escalation"],
  ])("%s on a click", async (_name, standing, want) => {
    const { asked, sc } = selecting();
    drawQueueMediationLabel(create(RosterMergeQueueMediationSchema, { standing }), sc, "m").click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id])).toEqual([["agentrepl.v1.SelectInEditorRequest", want]]);
  });

  it.each([
    ["waiting", { case: "waiting" as const, value: {} }, false],
    ["checking", { case: "checking" as const, value: { target: target("intake") } }, false],
    ["fixing", { case: "fixing" as const, value: { target: target("remedy") } }, false],
    ["testing", { case: "testing" as const, value: { target: target("remedy") } }, false],
    ["escalated", { case: "escalated" as const, value: { target: target("escalation") } }, true],
  ])("draws the %s label red only when it waits on the user", (_name, standing, want) => {
    const label = drawQueueMediationLabel(create(RosterMergeQueueMediationSchema, { standing }), daemon().sc, "m");
    expect(label.classList.contains(MERGE_QUEUE_NEEDS_YOU_CLASS)).toBe(want);
  });

  it("selects nothing on a click while an escalation is still being made", async () => {
    const { asked, sc } = selecting();
    const label = drawQueueMediationLabel(create(RosterMergeQueueMediationSchema, { standing: { case: "escalated", value: {} } }), sc, "m");
    label.click();
    await settled();
    expect([asked.length, label.hasAttribute("data-select")]).toEqual([0, false]);
  });

  it("selects nothing on a click while a chain's first deploy rolls out", async () => {
    const { asked, sc } = selecting();
    const label = drawQueueMediationLabel(mediation(false), sc, "m");
    label.click();
    await settled();
    expect([asked.length, label.hasAttribute("data-select")]).toEqual([0, false]);
  });

  it.each([
    ["no standing", create(RosterMergeQueueMediationSchema, {})],
    ["an assessment with no target", create(RosterMergeQueueMediationSchema, { standing: { case: "checking", value: {} } })],
    ["a fix with no target", create(RosterMergeQueueMediationSchema, { standing: { case: "fixing", value: {} } })],
    ["a fix under test with no target", create(RosterMergeQueueMediationSchema, { standing: { case: "testing", value: {} } })],
  ])("refuses %s as a malformed view", (_name, malformed) => {
    expect(() => drawQueueMediationLabel(malformed, daemon().sc, "m")).toThrow(MalformedView);
  });

  it("draws no row in the queue's body for the mediation", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { mediation: mediation(true), head: mergingHead("w1") }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([...body.children].map((el) => el.getAttribute("data-workspace"))).toEqual(["w1"]);
  });

  it("draws the label in the repository's group header, after its count (owner request, 2026-11-16)", () => {
    const section = mergeQueueBand([queueGroup({ id: "a", queue: { head: mergingHead("w1"), mediation: mediation(true) } })]);
    const head = drawRosterMergeQueueSection(section, daemon().sc, "p", NONE_COLD_GATED).querySelector(".merged-group-head");
    const label = head?.querySelector(`.${MERGE_QUEUE_MEDIATION_CLASS}`);
    expect([label?.textContent, label?.previousElementSibling?.classList.contains("sb-open-count")]).toEqual(["assessing zettel health", true]);
  });

  it("draws a queue holding only the mediation with no head, only the glyph's line", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { mediation: mediation(false) }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([...body.children].map((el) => el.className)).toEqual([`${MERGE_QUEUE_CARD_LINE_CLASS} ${MERGE_QUEUE_CONTROL_CLASS}`]);
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
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), waiting: waiting.map((w) => waitingMerge(w)) }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
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

describe("waitingMergeRef", () => {
  it("names the merge itself, never the requester two async merges share", () => {
    const first = waitingMerge("asker", { subject: queueSubject("asker", { async: true, name: "fix-a", merge: "merge-a" }) });
    const second = waitingMerge("asker", { subject: queueSubject("asker", { async: true, name: "fix-b", merge: "merge-b" }) });
    expect([waitingMergeRef(first, "w[0]").id, waitingMergeRef(second, "w[1]").id]).toEqual(["merge-a", "merge-b"]);
  });

  it("refuses a subject that names no merge", () => {
    const subject = create(RosterMergeQueueSubjectSchema, { workspace: { id: "w1", dir: "/w/w1" }, name: "one", mode: { mode: { case: "sync", value: {} } } });
    expect(() => waitingMergeRef(waitingMerge("w1", { subject }), "w[0]")).toThrow(MalformedView);
  });
});

describe("drawRosterMergeQueueHead", () => {
  it("leads with the glyph it is given and the credited workspace's name", () => {
    const { sc } = daemon();
    const card = drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { name: "one" }) }), undefined, glyph(), sc, "head", NONE_COLD_GATED);
    expect([card.querySelector(".test-glyph")?.textContent, card.querySelector(".sb-mq-name")?.textContent]).toEqual([MERGE_GLYPH, "one"]);
  });

  it("says an async merge is async on hover", () => {
    const { sc } = daemon();
    const card = drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { async: true }) }), undefined, glyph(), sc, "head", NONE_COLD_GATED);
    expect(card.querySelector(".sb-mq-name")?.getAttribute("title")).toBe("async merge");
  });

  it("draws each stage in order", () => {
    const { sc } = daemon();
    const head = mergingHead("w1", { stages: [stage("queue", "succeeded"), stage("rebasing", "succeeded"), stage("tests", "live")] });
    const card = drawRosterMergeQueueHead(head, undefined, glyph(), sc, "head", NONE_COLD_GATED);
    expect([...card.querySelectorAll(".sb-mq-stage-label")].map((el) => el.textContent)).toEqual(["queue", "rebasing", "tests"]);
  });

  it("draws no stage strip before the merge has a stage", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueueHead(mergingHead("w1"), undefined, glyph(), sc, "head", NONE_COLD_GATED).querySelector(".sb-mq-stages")).toBeNull();
  });

  it.each([
    { name: "a merging head", head: () => mergingHead("w1", { stages: [stage("queue", "succeeded"), stage("tests", "live")] }), pause: undefined, want: ["sb-mq-head-line", "sb-mq-stages"] },
    { name: "a merging head on a paused queue", head: () => mergingHead("w1", { stages: [stage("tests", "live")] }), pause: paused(), want: ["sb-mq-head-line", "sb-mq-stages"] },
    { name: "a merging head on a stopped queue", head: () => mergingHead("w1", { stages: [stage("tests", "live")] }), pause: paused({ hard: true }), want: ["sb-mq-head-line", "sb-mq-stages"] },
    {
      name: "a waiting head",
      head: () => waitingHead("w1"),
      pause: undefined,
      want: ["sb-mq-head-line"],
    },
    {
      name: "a waiting head on a paused queue",
      head: () => waitingHead("w1"),
      pause: paused(),
      want: ["sb-mq-head-line"],
    },
  ])("draws no status line under $name: its card is its name line and its stages", ({ head, pause, want }) => {
    const { sc } = daemon();
    expect([...drawRosterMergeQueueHead(head(), pause, glyph(), sc, "head", NONE_COLD_GATED).children].map((el) => el.className)).toEqual(want);
  });

  it("stops a paused head's clock at the pause", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = mergingHead("w1", { stages: [stage("tests", "live", 1, BigInt(NOW - 5_000))], clock: activeHeadClock(NOW - 5_000) });
    const card = drawRosterMergeQueueHead(head, paused({ atMs: NOW - 2_000 }), glyph(), sc, "head", NONE_COLD_GATED);
    ticker.tick(NOW + 60_000);
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatElapsed(3_000));
  });

  it("draws no clock for a pause whose instant was not recorded", () => {
    const { sc } = daemon();
    const head = mergingHead("w1", { stages: [stage("tests", "live", 1, BigInt(NOW - 5_000))] });
    expect(drawRosterMergeQueueHead(head, create(RosterMergeQueuePausedSchema, {}), glyph(), sc, "head", NONE_COLD_GATED).querySelector(".sb-mq-clock")).toBeNull();
  });

  it.each([
    { name: "a merging head", standing: "merging" },
    { name: "a waiting head", standing: "waiting" },
  ])("states its standing for $name", ({ standing }) => {
    const { sc } = daemon();
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), clock: activeHeadClock(NOW), standing: { case: standing, value: {} } as never });
    expect(drawRosterMergeQueueHead(head, undefined, glyph(), sc, "head", NONE_COLD_GATED).getAttribute("data-standing")).toBe(standing);
  });

  it("ticks an active head's clock from when it became active, not its first stage", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const head = mergingHead("w1", {
      stages: [stage("rebasing", "succeeded", 1, BigInt(NOW - 3 * 3_600_000)), stage("tests", "live", 1, BigInt(NOW - 1_000))],
      clock: activeHeadClock(NOW - 5_000),
    });
    const card = drawRosterMergeQueueHead(head, undefined, glyph(), sc, "head", NONE_COLD_GATED);
    ticker.tick(NOW + 60_000);
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(65_000));
  });

  it("counts a waiting head's whole wait: the waiting before its last active stretch and the waiting since", () => {
    const ticker = fakeTicker(NOW);
    const sc = sidebarContext(appContext({}, ticker));
    const card = drawRosterMergeQueueHead(waitingHead("w1", waitingHeadClock(NOW - 2_000, 40 * 60_000)), undefined, glyph(), sc, "head", NONE_COLD_GATED);
    ticker.tick(NOW + 60_000);
    expect(card.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(40 * 60_000 + 62_000));
  });

});

describe("merge clock starts", () => {
  it("counts an active clock from when its active stretch began", () => {
    expect(activeClockStartMs(activeClock(NOW - 5_000), "clock")).toBe(NOW - 5_000);
  });

  it.each([
    { name: "a clock with nothing waited before", waited: 0, want: NOW - 5_000 },
    { name: "a clock carrying the waiting before its last active stretch", waited: 40 * 60_000, want: NOW - 5_000 - 40 * 60_000 },
  ])("counts $name back by that waiting", ({ waited, want }) => {
    expect(waitingClockStartMs(waitingClock(NOW - 5_000, waited), "clock")).toBe(want);
  });

  it.each([
    { name: "an active head", clock: activeHeadClock(NOW - 5_000), want: NOW - 5_000 },
    { name: "a waiting head", clock: waitingHeadClock(NOW - 5_000, 60_000), want: NOW - 65_000 },
  ])("counts $name's clock from its stretch", ({ clock, want }) => {
    expect(headClockStartMs(clock, "clock")).toBe(want);
  });

  it.each([
    { name: "an unset active clock", start: () => activeClockStartMs(undefined, "clock") },
    { name: "an unset waiting clock", start: () => waitingClockStartMs(undefined, "clock") },
    { name: "an unset head clock", start: () => headClockStartMs(undefined, "clock") },
    { name: "a head clock in no stretch", start: () => headClockStartMs(create(RosterMergeQueueHeadClockSchema, {}), "clock") },
  ])("refuses $name as a malformed view", ({ start }) => {
    expect(start).toThrow(MalformedView);
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
    expect(() => drawRosterMergeQueueHead(create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), clock: activeHeadClock(NOW) }), undefined, glyph(), sc, "head", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it.each([
    { name: "no clock", clock: undefined },
    { name: "a clock in no stretch", clock: create(RosterMergeQueueHeadClockSchema, {}) },
  ])("refuses a head with $name as a malformed view", ({ clock }) => {
    const { sc } = daemon();
    const head = create(RosterMergeQueueHeadSchema, { subject: queueSubject("w1"), clock, standing: { case: "merging", value: {} } });
    expect(() => drawRosterMergeQueueHead(head, undefined, glyph(), sc, "head", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it("refuses a head with no subject as a malformed view", () => {
    const { sc } = daemon();
    expect(() => drawRosterMergeQueueHead(create(RosterMergeQueueHeadSchema, { standing: { case: "merging", value: {} } }), undefined, glyph(), sc, "head", NONE_COLD_GATED)).toThrow(
      MalformedView,
    );
  });
});

describe("drawRosterMergeQueueStage", () => {
  it.each([
    { name: "a live stage", state: "live" as const, glyph: STATE_GLYPHS.live },
    { name: "a held stage", state: "held" as const, glyph: STATE_GLYPHS.live },
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
  it("draws its position, its name and how long it has waited", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1", { clock: waitingClock(NOW - 3_000), subject: queueSubject("w1", { name: "one" }) }), 1, sc, "w", NONE_COLD_GATED);
    expect([...line.children].map((el) => [el.className, el.textContent])).toEqual([
      ["sb-mq-position", "1"],
      ["sb-mq-name", "one"],
      ["sb-mq-clock", formatTickedElapsed(3_000)],
    ]);
  });

  it("notes an async merge's requester after its name, whole on the line's hover", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(
      waitingMerge("w1", { clock: waitingClock(NOW - 3_000), subject: queueSubject("w1", { name: "fix-it", async: true, requester: "triage" }) }),
      1,
      sc,
      "w",
      NONE_COLD_GATED,
    );
    expect([...line.children].map((el) => [el.className, el.textContent])).toEqual([
      ["sb-mq-position", "1"],
      ["sb-mq-name", "fix-it"],
      [MERGE_QUEUE_ASYNC_OF_CLASS, "(triage)"],
      ["sb-mq-clock", formatTickedElapsed(3_000)],
    ]);
    expect([line.title, line.querySelector(".sb-mq-name")?.hasAttribute("title")]).toEqual(["(triage)", false]);
  });

  it("notes nothing after a sync merge's name", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1"), 1, sc, "w", NONE_COLD_GATED);
    expect([line.querySelector(`.${MERGE_QUEUE_ASYNC_OF_CLASS}`), line.title]).toEqual([null, ""]);
  });

  it("refuses an async merge that names no requester", () => {
    const { sc } = daemon();
    const subject = create(RosterMergeQueueSubjectSchema, {
      workspace: { id: "w1", dir: "/w/w1" }, name: "one", mode: { mode: { case: "async", value: {} } }, merge: { id: "w1" },
    });
    expect(() => drawRosterMergeQueueWaiting(waitingMerge("w1", { subject }), 1, sc, "w", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it("counts a waiting merge's waiting before its last active stretch, and no active stretch", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1", { clock: waitingClock(NOW - 3_000, 40 * 60_000) }), 1, sc, "w", NONE_COLD_GATED);
    expect(line.querySelector(".sb-mq-clock")?.textContent).toBe(formatTickedElapsed(40 * 60_000 + 3_000));
  });

  it("refuses a waiting merge with no clock as a malformed view", () => {
    const { sc } = daemon();
    const unclocked = create(RosterMergeQueueWaitingSchema, { subject: queueSubject("w1") });
    expect(() => drawRosterMergeQueueWaiting(unclocked, 1, sc, "w", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it("draws no front mark on a waiting merge", () => {
    const { sc } = daemon();
    const line = drawRosterMergeQueueWaiting(waitingMerge("w1"), 1, sc, "w", NONE_COLD_GATED);
    expect([...line.children].map((el) => el.className)).toEqual(["sb-mq-position", "sb-mq-name", "sb-mq-clock"]);
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

  // A PARTIAL SELECTION (2026-11-11): the daemon's own select left Emacs to
  // follow the roster, which waits on an active minibuffer, so the rail drew
  // the workspace selected while the frame showed another. The click is the
  // editor's own switch, as a row's is.
  it.each([
    ["a merge with a bubble", "bubble-1"],
    ["a merge without one", undefined],
  ])("selects the credited workspace through the editor, for %s", async (_name, reveal) => {
    const { asked, sc } = selecting();
    drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", reveal === undefined ? {} : { reveal }) }), undefined, glyph(), sc, "head", NONE_COLD_GATED).click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id, "reveal" in r ? r.reveal?.value : undefined])).toEqual([
      ["agentrepl.v1.SelectInEditorRequest", "w1", reveal],
    ]);
  });

  // OWNER RULING: a select is idempotent, so its line never goes busy; a line
  // the roster keeps (its wire unchanged by the select) stays clickable.
  it.each([
    ["the head card", (sc: ReturnType<typeof selecting>["sc"]) => drawRosterMergeQueueHead(mergingHead("w1"), undefined, glyph(), sc, "head", NONE_COLD_GATED)],
    [
      "a repair slot's card",
      (sc: ReturnType<typeof selecting>["sc"]) =>
        drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { fixingTests: fixingTestsMerge("w2", NOW) }), REPOSITORY, sc, "queue", NONE_COLD_GATED).querySelector(
          `.${MERGE_QUEUE_REMEDIATING_CLASS}`,
        ) as HTMLElement,
    ],
    ["a waiting line", (sc: ReturnType<typeof selecting>["sc"]) => drawRosterMergeQueueWaiting(waitingMerge("w3"), 1, sc, "w", NONE_COLD_GATED)],
    [
      "a head card opening its bubble",
      (sc: ReturnType<typeof selecting>["sc"]) =>
        drawRosterMergeQueueHead(mergingHead("w1", { subject: queueSubject("w1", { reveal: "bubble-1" }) }), undefined, glyph(), sc, "head", NONE_COLD_GATED),
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
    const line = drawRosterMergeQueueWaiting(waitingMerge("w3"), 1, sc, "w", NONE_COLD_GATED);
    line.click();
    const busy = line.classList.contains("is-busy");
    await settled();
    expect(busy).toBe(false);
  });

  it("selects as a row does when the merge has no bubble", async () => {
    const { asked, sc } = selecting();
    drawRosterMergeQueueWaiting(waitingMerge("w2"), 1, sc, "w", NONE_COLD_GATED).click();
    await settled();
    expect(asked.map((r) => [r.$typeName, r.workspace?.id])).toEqual([["agentrepl.v1.SelectInEditorRequest", "w2"]]);
  });
});

describe("drawCardLine", () => {
  it("draws the glyph it is given, then the credited name", () => {
    const line = drawCardLine(queueSubject("w1", { name: "one" }), glyph(), "subject", NONE_COLD_GATED);
    expect([line.className, [...line.children].map((el) => [el.className, el.textContent])]).toEqual([
      MERGE_QUEUE_CARD_LINE_CLASS,
      [
        ["test-glyph", MERGE_GLYPH],
        ["sb-mq-name", "one"],
      ],
    ]);
  });

  it("refuses a subject that carries no mode as malformed", () => {
    expect(() => drawCardLine(create(RosterMergeQueueSubjectSchema, { workspace: { id: "w1", dir: "/w/w1" }, name: "one" }), glyph(), "subject", NONE_COLD_GATED)).toThrow(MalformedView);
  });

  it.each([
    { name: "the head's card", card: () => drawRosterMergeQueueHead(mergingHead("w1"), undefined, glyph(), daemon().sc, "head", NONE_COLD_GATED) },
  ])("opens $name with the shared card line", ({ card }) => {
    const shared = drawCardLine(queueSubject("w1"), glyph(), "subject", NONE_COLD_GATED);
    const drawn = card().firstElementChild as HTMLElement;
    expect([...drawn.children].slice(0, 2).map((el) => el.outerHTML)).toEqual([...shared.children].map((el) => el.outerHTML));
  });
});

/** A merge credited to workspace ID off its queue for CAUSE, standing as MERGE, which its controls name. */
function offQueue(id: string, cause: string, merge = id) {
  return create(RosterMergeQueueOffQueueSchema, {
    subject: queueSubject(id, { merge }),
    cause: { text: cause },
    requeue: { merge: { id: merge, dir: `/wt/${merge}` } },
    discard: { merge: { id: merge, dir: `/wt/${merge}` } },
  });
}

/** A context recording every RequeueMerge, DiscardMerge and select ask, answering each with success. */
function offQueueDaemon() {
  const requeued: RequeueMergeRequest[] = [];
  const discarded: DiscardMergeRequest[] = [];
  const selected: Array<SelectWorkspaceRequest | SelectInEditorRequest> = [];
  const sc = sidebarContext(
    appContext({
      requeueMerge: (request: RequeueMergeRequest) => {
        requeued.push(request);
        return create(RequeueMergeResponseSchema, { result: { case: "success", value: {} } });
      },
      discardMerge: (request: DiscardMergeRequest) => {
        discarded.push(request);
        return create(DiscardMergeResponseSchema, { result: { case: "success", value: { discarded: { case: "ownBranch", value: {} } } } });
      },
      selectWorkspace: (request: SelectWorkspaceRequest) => {
        selected.push(request);
        return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
      selectInEditor: (request: SelectInEditorRequest) => {
        selected.push(request);
        return create(SelectInEditorResponseSchema, { result: { case: "success", value: { route: { case: "relayed", value: {} } } } });
      },
    }),
  );
  return { sc, requeued, discarded, selected };
}

describe("drawRosterMergeQueueOffQueue", () => {
  it("draws a queue holding only merges off it, with no head, after everything in line", () => {
    const { sc } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w2", "evicted")] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([body.querySelector(`.${MERGE_QUEUE_HEAD_CLASS}`), body.querySelectorAll(`.${MERGE_QUEUE_OFF_QUEUE_CLASS}`).length]).toEqual([null, 1]);
    const full = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), offQueue: [offQueue("w2", "evicted")] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    expect([...full.children].map((el) => el.classList[0])).toEqual([MERGE_QUEUE_HEAD_CLASS, MERGE_QUEUE_OFF_QUEUE_RULE_CLASS, MERGE_QUEUE_OFF_QUEUE_CLASS]);
  });

  it("draws the merge's name and why it left", () => {
    const { sc } = offQueueDaemon();
    const line = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w2", "evicted")] }), REPOSITORY, sc, "queue", NONE_COLD_GATED).querySelector(`.${MERGE_QUEUE_OFF_QUEUE_CLASS}`);
    expect([line?.querySelector(".sb-mq-name")?.textContent, line?.querySelector(".sb-mq-off-queue-cause")?.textContent]).toEqual(["w2", "evicted"]);
  });

  it("notes an async merge's requester after its name, before why it left", () => {
    const { sc } = offQueueDaemon();
    const off = offQueue("w2", "evicted");
    off.subject = queueSubject("w2", { name: "fix-it", async: true, requester: "triage" });
    const line = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [off] }), REPOSITORY, sc, "queue", NONE_COLD_GATED).querySelector<HTMLElement>(`.${MERGE_QUEUE_OFF_QUEUE_CLASS}`)!;
    expect([...line.children].slice(0, 3).map((el) => el.textContent)).toEqual(["fix-it", "(triage)", "evicted"]);
    expect(line.title).toBe("(triage)");
  });

  it("selects the requester alone when its name in the note is clicked, never the line", async () => {
    const { sc, selected } = offQueueDaemon();
    const off = offQueue("w2", "evicted");
    off.subject = queueSubject("w2", { name: "fix-it", async: true, requester: "triage", reveal: "bubble-1" });
    const line = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [off] }), REPOSITORY, sc, "queue", NONE_COLD_GATED).querySelector<HTMLElement>(`.${MERGE_QUEUE_OFF_QUEUE_CLASS}`)!;
    const link = line.querySelector<HTMLElement>(`.${MERGE_QUEUE_ASYNC_OF_LINK_CLASS}`)!;
    expect(link.textContent).toBe("triage");
    link.click();
    await vi.waitFor(() => expect(selected).toHaveLength(1));
    expect(selected.map((r) => [r.workspace?.id, "reveal" in r ? r.reveal : undefined])).toEqual([["w2", undefined]]);
  });

  it("requeues the merge's own workspace at the back, as the user, without selecting the line", async () => {
    const { sc, requeued, selected } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted", "w-merge")] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    (body.querySelector('[data-verb="requeue"]') as HTMLElement).click();
    await settled();
    expect([requeued.map((r) => [r.workspace?.id, r.placement?.place.case, r.requester?.asker.case]), selected.length]).toEqual([[["w-merge", "back", "user"]], 0]);
  });

  it("discards the merge's own workspace's request, as the user", async () => {
    const { sc, discarded } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted", "w-merge")] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    (body.querySelector('[data-verb="discard"]') as HTMLElement).click();
    await settled();
    expect(discarded.map((r) => [r.workspace?.id, r.requester?.asker.case])).toEqual([["w-merge", "user"]]);
  });

  // A MERGE WHOSE WORKSPACE STANDS AT ITS COLD GATE IS NOT REQUEUED (owner
  // request, 2026-11-17): the gate evicted every such requeue at once.
  it("selects a cold-gated merge's workspace in place of requeueing it", async () => {
    const { sc, requeued, selected } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted")] }), REPOSITORY, sc, "queue", new Set(["w1"]));
    (body.querySelector('[data-verb="requeue"]') as HTMLElement).click();
    await settled();
    expect([requeued.length, selected.map((r) => [r.$typeName, r.workspace?.id])]).toEqual([0, [["agentrepl.v1.SelectInEditorRequest", "w1"]]]);
  });

  it("records the cold-gated merge's select in place of a requeue", async () => {
    const capture = captureLogRecords();
    const { sc } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted")] }), REPOSITORY, sc, "queue", new Set(["w1"]));
    (body.querySelector('[data-verb="requeue"]') as HTMLElement).click();
    await settled();
    const records = await forwardedRecords(capture);
    expect([recordFor(records, "sidebar.merge-queue.cold-gate-select").context, records.some((r) => r.operation === "sidebar.merge-queue.requeue")]).toMatchObject([
      { workspace: "w1" },
      false,
    ]);
  });

  it.each([
    { name: "a cold-gated merge's", gated: ["w1"], want: COLD_GATED_REQUEUE_TITLE },
    { name: "any other merge's", gated: ["w9"], want: "put this merge back in line, at the back" },
  ])("titles $name requeue control for what its click does", ({ gated, want }) => {
    const { sc } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted")] }), REPOSITORY, sc, "queue", new Set(gated));
    expect((body.querySelector('[data-verb="requeue"]') as HTMLElement).title).toBe(want);
  });

  it("still requeues a merge whose workspace no cold gate stands on, while another's does", async () => {
    const { sc, requeued, selected } = offQueueDaemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted")] }), REPOSITORY, sc, "queue", new Set(["w9"]));
    (body.querySelector('[data-verb="requeue"]') as HTMLElement).click();
    await settled();
    expect([requeued.map((r) => r.workspace?.id), selected.length]).toEqual([["w1"], 0]);
  });

  it("refuses a merge off its queue with no requeue control as a malformed view", () => {
    const { sc } = offQueueDaemon();
    const broken = create(RosterMergeQueueOffQueueSchema, { subject: queueSubject("w2"), cause: { text: "evicted" }, discard: { merge: { id: "w2" } } });
    expect(() => drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [broken] }), REPOSITORY, sc, "queue", NONE_COLD_GATED)).toThrow(MalformedView);
  });
});

describe("a waiting merge's position numeral", () => {
  it("is drawn at its name's size", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1"), waiting: [waitingMerge("w2")] }), REPOSITORY, sc, "queue", NONE_COLD_GATED);
    const [numeral, name] = inRail(body, (rail) => {
      const line = rail.querySelector(`.${MERGE_QUEUE_WAITING_CLASS}`);
      return [
        cascadedValue(requireElement(line?.querySelector(`.${MERGE_QUEUE_POSITION_CLASS}`)), "font-size"),
        cascadedValue(requireElement(line?.querySelector(".sb-mq-name")), "font-size"),
      ];
    });
    expect([numeral, numeral === name]).toEqual([name, true]);
    expect(name).not.toBe("");
  });
});

/** EL, failing the test when the line drew none. */
function requireElement(el: Element | null | undefined): Element {
  if (el === null || el === undefined) throw new Error("the waiting line drew no such element");
  return el;
}

describe("a cold-gated merge's line", () => {
  // EVERY KIND OF QUEUE LINE STRIKES A COLD-GATED MERGE'S NAME, keyed on the
  // merge's own workspace (`RosterMergeQueueSubject.merge`).
  it.each([
    { kind: "the head", queue: () => ({ head: mergingHead("w1") }), line: `.${MERGE_QUEUE_HEAD_CLASS}` },
    { kind: "a waiting merge", queue: () => ({ head: mergingHead("w0"), waiting: [waitingMerge("w1")] }), line: `.${MERGE_QUEUE_WAITING_CLASS}:not(.${MERGE_QUEUE_REMEDIATING_CLASS})` },
    { kind: "a merge fixing tests", queue: () => ({ head: mergingHead("w0"), fixingTests: fixingTestsMerge("w1", NOW) }), line: `.${MERGE_QUEUE_REMEDIATING_CLASS}` },
    { kind: "a GitHub standing", queue: () => ({ head: mergingHead("w0"), github: [githubStanding("w1", 1, 1, NOW)] }), line: `.${MERGE_QUEUE_GITHUB_CLASS}` },
    { kind: "a merge off its queue", queue: () => ({ offQueue: [offQueue("w1", "evicted")] }), line: `.${MERGE_QUEUE_OFF_QUEUE_CLASS}` },
  ])("draws $kind's name with the shared cold-gate class and no status icon", ({ queue, line }) => {
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, queue()), REPOSITORY, offQueueDaemon().sc, "queue", new Set(["w1"]));
    const drawn = body.querySelector(line);
    expect([
      drawn?.querySelector(".sb-mq-name")?.classList.contains(COLD_GATED_NAME_CLASS),
      drawn?.querySelector(".st, img, svg"),
    ]).toEqual([true, null]);
  });

  it("leaves a merge unstruck while the gate stands only on its requester", () => {
    const off = offQueue("requester", "evicted", "merge-ws");
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [off] }), REPOSITORY, offQueueDaemon().sc, "queue", new Set(["requester"]));
    expect(body.querySelector(".sb-mq-name")?.classList.contains(COLD_GATED_NAME_CLASS)).toBe(false);
  });

  it("draws a cold-gated name struck and greyed in the rail", () => {
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { offQueue: [offQueue("w1", "evicted")] }), REPOSITORY, offQueueDaemon().sc, "queue", new Set(["w1"]));
    const painted = inRail(body, () => {
      const name = requireElement(body.querySelector(".sb-mq-name"));
      return [cascadedValue(name, "text-decoration"), cascadedValue(name, "color")];
    });
    expect(painted).toEqual(["line-through", "color-mix(in srgb, var(--muted) 55%, transparent)"]);
  });
});

describe("the rule above a queue's merges off it", () => {
  it.each([
    { name: "merges waiting and off the queue", queue: () => ({ head: mergingHead("w0"), waiting: [waitingMerge("w1")], offQueue: [offQueue("w2", "evicted")] }), want: 1 },
    { name: "only merges off the queue", queue: () => ({ offQueue: [offQueue("w2", "evicted")] }), want: 0 },
    { name: "only merges in the queue", queue: () => ({ head: mergingHead("w0"), waiting: [waitingMerge("w1")] }), want: 0 },
  ])("is drawn $want time(s) for $name", ({ queue, want }) => {
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, queue()), REPOSITORY, offQueueDaemon().sc, "queue", NONE_COLD_GATED);
    expect(body.querySelectorAll(`.${MERGE_QUEUE_OFF_QUEUE_RULE_CLASS}`).length).toBe(want);
  });

  it("stands between the last merge in the queue and the first off it", () => {
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), waiting: [waitingMerge("w1")], offQueue: [offQueue("w2", "evicted")] }),
      REPOSITORY,
      offQueueDaemon().sc,
      "queue",
      NONE_COLD_GATED,
    );
    expect([...body.children].map((el) => el.classList[0])).toEqual([
      MERGE_QUEUE_HEAD_CLASS,
      MERGE_QUEUE_WAITING_CLASS,
      MERGE_QUEUE_OFF_QUEUE_RULE_CLASS,
      MERGE_QUEUE_OFF_QUEUE_CLASS,
    ]);
  });

  it("is inset from both of the rail's margins", () => {
    const body = drawRosterMergeQueue(
      create(RosterRepoMergeQueueSchema, { head: mergingHead("w0"), offQueue: [offQueue("w2", "evicted")] }),
      REPOSITORY,
      offQueueDaemon().sc,
      "queue",
      NONE_COLD_GATED,
    );
    const margins = inRail(body, () => {
      const rule = requireElement(body.querySelector(`.${MERGE_QUEUE_OFF_QUEUE_RULE_CLASS}`));
      return [cascadedValue(rule, "margin-left"), cascadedValue(rule, "margin-right")];
    });
    expect(margins.every((m) => m !== "" && m !== "0px" && m !== "0")).toBe(true);
  });
});
