// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
  type UpdateSidebarViewResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import {
  RosterOpenSectionSchema,
  RosterRepoMergeQueueSchema,
  RosterRepoSectionSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MERGE_GLYPH } from "../../src/merge-glyph.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawMergeQueueToggle,
  drawRosterMergeQueue,
  drawRosterOpenSectionMergeQueuePaused,
  drawRosterOpenSectionShows,
  drawRosterRepoShows,
  MERGE_QUEUE_CLASS,
  MERGE_QUEUE_PAUSED_ATTRIBUTE,
  MERGE_QUEUE_TOGGLE_CLASS,
  OPEN_SHOWS_VIEW_KEY,
  paintMergeQueueToggle,
  SHOWS_ATTRIBUTE,
} from "../../src/sidebar/merge-queue-view.js";
import { BUCKET_ATTRIBUTE } from "../../src/sidebar/subsections.js";
import { appContext, openSection, repoSection, row, sidebarContext } from "./harness.js";

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

  it("reads the merge queue with its rows", () => {
    const shows = drawRosterRepoShows(repoSection({ id: "repo-1", mergeQueue: [row({ id: "w1" })] }), "section");
    expect(shows.case === "mergeQueue" ? shows.queue.rows.map((r) => r.row?.workspace?.workspace?.id) : shows.case).toEqual(["w1"]);
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

describe("drawRosterMergeQueue", () => {
  it("draws one row per merge in line, front first", () => {
    const { sc } = daemon();
    const queue = create(RosterRepoMergeQueueSchema, { rows: [{ row: row({ id: "w3" }) }, { row: row({ id: "w1" }) }] });
    const body = drawRosterMergeQueue(queue, sc, "queue");
    expect([...body.querySelectorAll("[data-roster-row]")].map((el) => el.getAttribute("data-roster-row"))).toEqual(["w3", "w1"]);
  });

  it("draws each row as a workspace row, its tab number slot and status mark included", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { rows: [{ row: row({ id: "w1" }) }] }), sc, "queue");
    expect([body.querySelector(".row .sb-num") !== null, body.querySelector(".row .st") !== null]).toEqual([true, true]);
  });

  it("is a rows box no drag arranges", () => {
    const { sc } = daemon();
    const body = drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { rows: [{ row: row({ id: "w1" }) }] }), sc, "queue");
    expect([body.classList.contains("rows"), body.classList.contains(MERGE_QUEUE_CLASS), body.hasAttribute(BUCKET_ATTRIBUTE)]).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("draws no row for an empty queue", () => {
    const { sc } = daemon();
    expect(drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, {}), sc, "queue").children.length).toBe(0);
  });
});
