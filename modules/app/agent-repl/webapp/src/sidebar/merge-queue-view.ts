/**
 * merge-queue-view — THE OPEN SECTION'S MERGE-QUEUE TOGGLE, and the queue a
 * repository section draws while it is on (owner request, 2026-10-24).
 *
 * The OPEN header line carries the merge glyph the feed's merge bubble draws
 * (`merge-glyph.ts`). In the purple of a merge in progress it says each
 * repository section shows its open workspaces, and a click asks for the
 * merge queues; the glyph takes the expanded repository names' colour while
 * every section lists its queue instead, and a click on it asks for the
 * workspaces again (owner request, 2026-10-25: the colour marks the way to
 * the queues). While a repository's merge queue is paused
 * (`frontend.v1.RosterOpenSection.merge_queue_paused`) the purple turns the
 * amber a paused merge bubble is framed in.
 *
 * WHAT THE SECTIONS SHOW IS THE DAEMON'S (`frontend.v1.RosterOpenSection.shows`,
 * restated on every `RosterRepoSection.shows`), like every fold, so every
 * page draws the same view. The glyph is painted at once and held until the
 * wire agrees (`view.ts`); the sections follow on the push that carries
 * their queues.
 *
 * A QUEUE IS DRAWN AS A SMALL MERGE BUBBLE AND ITS LINE (owner request,
 * 2026-10-25): the merge at the head as a card -- who it is credited to, the
 * stages it has been through, and the step it stands on (or why it failed) --
 * then every merge waiting behind it, numbered in the order it will run. A
 * merge is named as the rest of the roster credits it, so a hidden merge
 * workspace is never named here, and a click selects the credited workspace
 * and opens the merge's bubble in its feed. A queue with nothing in it draws
 * no body, and its header wears the folded header's faded grey.
 */
import { create } from "@bufbuild/protobuf";
import type {
  RosterMergeQueueHead,
  RosterMergeQueueStage,
  RosterMergeQueueSubject,
  RosterMergeQueueWaiting,
  RosterOpenSection,
  RosterRepoMergeQueue,
  RosterRepoSection,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  SelectWorkspaceRequestSchema,
  SelectWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { createControl } from "../control.js";
import { formatElapsed } from "../duration.js";
import { roundSuffix, STATE_GLYPHS } from "../feed/merge/tab-strip.js";
import { log } from "../log.js";
import { MERGE_GLYPH } from "../merge-glyph.js";
import { guardMalformed } from "../rpc/guard.js";
import { SELECT_WORKSPACE_EXPECTED_ARMS } from "../rpc/refusal.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { selectWorkspace, tickElapsed } from "./row.js";
import { runVerb } from "./verbs.js";
import { changeView } from "./view-change.js";

/**
 * The key what the open section shows is held under (`view.ts`): TRUE while
 * the repository sections show their merge queues. FIXED, because the roster
 * carries exactly one open section.
 */
export const OPEN_SHOWS_VIEW_KEY = "shows:open";

/** The class of the merge-queue toggle on the OPEN header line. */
export const MERGE_QUEUE_TOGGLE_CLASS = "sb-merge-queue-toggle";

/**
 * The attribute the toggle says what the sections show in: `workspaces` or
 * `merge-queues`. The stylesheet colours the glyph from it.
 */
export const SHOWS_ATTRIBUTE = "data-shows";

/**
 * The attribute the toggle carries, valueless, while a repository's merge
 * queue is paused. The stylesheet colours the glyph from it.
 */
export const MERGE_QUEUE_PAUSED_ATTRIBUTE = "data-merge-queue-paused";

/** The class of a repository section's body while it lists its merge queue. */
export const MERGE_QUEUE_CLASS = "sb-merge-queue";

/** Whether U's repository sections show their merge queues. EVERY arm is named; unset is malformed. */
export function drawRosterOpenSectionShows(u: RosterOpenSection, path: string): boolean {
  const arm = requireCase(u.shows, `${path}.shows`);
  switch (arm.case) {
    case "workspaces":
      return false;
    case "mergeQueues":
      return true;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.shows`, other.case);
    }
  }
}

/** Whether U says a repository's merge queue is paused: the marker's presence is the fact. */
export function drawRosterOpenSectionMergeQueuePaused(u: RosterOpenSection): boolean {
  return u.mergeQueuePaused !== undefined;
}

/** What a repository section's body shows. */
export type RepoShows = { case: "workspaces" } | { case: "mergeQueue"; queue: RosterRepoMergeQueue };

/** What U's body shows. EVERY arm is named; unset is malformed. */
export function drawRosterRepoShows(u: RosterRepoSection, path: string): RepoShows {
  const arm = requireCase(u.shows, `${path}.shows`);
  switch (arm.case) {
    case "workspaces":
      return { case: "workspaces" };
    case "mergeQueue":
      return { case: "mergeQueue", queue: arm.value };
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.shows`, other.case);
    }
  }
}

/** Paint TOGGLE as the sections stand: showing their merge queues or not. */
export function paintMergeQueueToggle(toggle: HTMLElement, mergeQueues: boolean): void {
  toggle.setAttribute(SHOWS_ATTRIBUTE, mergeQueues ? "merge-queues" : "workspaces");
  toggle.setAttribute("aria-pressed", mergeQueues ? "true" : "false");
  toggle.title = mergeQueues ? "show each repository's workspaces" : "show each repository's merge queue";
}

/**
 * The merge-queue toggle, drawn as WIRE_MERGE_QUEUES says unless this page's
 * own ask is still in flight, and marked while a merge queue is PAUSED. A
 * click asks for the other view; it never reaches the header, whose click
 * folds the section.
 */
export function drawMergeQueueToggle(sc: SidebarContext, wireMergeQueues: boolean, paused: boolean): HTMLElement {
  const toggle = createControl();
  toggle.className = MERGE_QUEUE_TOGGLE_CLASS;
  toggle.textContent = MERGE_GLYPH;
  toggle.toggleAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE, paused);
  paintMergeQueueToggle(toggle, sc.view.track(OPEN_SHOWS_VIEW_KEY, wireMergeQueues, (value) => paintMergeQueueToggle(toggle, value)));
  toggle.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    const mergeQueues = toggle.getAttribute(SHOWS_ATTRIBUTE) !== "merge-queues";
    void changeView(sc, {
      key: OPEN_SHOWS_VIEW_KEY,
      value: mergeQueues,
      control: toggle,
      change: {
        case: "showOpen",
        value: { shows: mergeQueues ? { case: "mergeQueues", value: {} } : { case: "workspaces", value: {} } },
      },
    });
  });
  return toggle;
}

/** The class a repository section wears while its merge queue holds nothing. */
export const MERGE_QUEUE_EMPTY_CLASS = "sb-merge-queue-empty";

/** The class of the head's card. Each card is one line of the band fit. */
export const MERGE_QUEUE_HEAD_CLASS = "sb-mq-head";

/** The class of a waiting merge's line. Each is one line of the band fit. */
export const MERGE_QUEUE_WAITING_CLASS = "sb-mq-waiting";

/** Whether U, a section's merge queue, holds nothing to draw. */
export function mergeQueueEmpty(u: RosterRepoMergeQueue): boolean {
  return u.head === undefined && u.waiting.length === 0;
}

/**
 * A repository section's body while it shows its merge queue: the head's card,
 * then each waiting merge, front first. It is no arranged list (no bucket), so
 * nothing in it is a drag grip.
 */
export function drawRosterMergeQueue(u: RosterRepoMergeQueue, sc: SidebarContext, path: string): HTMLElement {
  const body = document.createElement("div");
  body.className = `rows ${MERGE_QUEUE_CLASS}`;
  if (u.head !== undefined) body.appendChild(drawRosterMergeQueueHead(u.head, u.paused !== undefined, sc, `${path}.head`));
  u.waiting.forEach((waiting, index) =>
    body.appendChild(drawRosterMergeQueueWaiting(waiting, index + 1, sc, `${path}.waiting[${index}]`)),
  );
  return body;
}

/**
 * The head's card: a small merge bubble. Its head line is the merge glyph, the
 * credited workspace's name and, while it runs or waits, how long; then its
 * stages so far; then where it stands -- its step, that it waits (paused or
 * not), or the account of its failure.
 */
export function drawRosterMergeQueueHead(
  u: RosterMergeQueueHead,
  paused: boolean,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const standing = requireCase(u.standing, `${path}.standing`);
  const card = drawSubjectControl(subject, sc, MERGE_QUEUE_HEAD_CLASS, `${path}.subject`);
  card.setAttribute("data-standing", standing.case);

  const line = document.createElement("div");
  line.className = "sb-mq-head-line";
  const glyph = document.createElement("span");
  glyph.className = "sb-mq-glyph";
  glyph.setAttribute("aria-hidden", "true");
  glyph.textContent = MERGE_GLYPH;
  line.append(glyph, drawSubjectName(subject, `${path}.subject`));
  card.appendChild(line);

  if (u.stages.length > 0) {
    const strip = document.createElement("div");
    strip.className = "sb-mq-stages";
    u.stages.forEach((stage, index) => strip.appendChild(drawRosterMergeQueueStage(stage, `${path}.stages[${index}]`)));
    card.appendChild(strip);
  }

  const detail = document.createElement("div");
  detail.className = "sb-mq-detail";
  switch (standing.case) {
    case "waiting": {
      detail.textContent = paused ? "paused" : "next";
      const queuedMs = msOf(standing.value.queuedAtMs, `${path}.waiting.queued_at_ms`);
      line.appendChild(drawClock(sc, queuedMs));
      break;
    }
    case "merging": {
      detail.textContent = standing.value.step ?? "";
      const first = u.stages[0];
      if (first !== undefined) {
        line.appendChild(drawClock(sc, stageStartedMs(first, `${path}.stages[0]`)));
      }
      if (paused) detail.append(detail.textContent === "" ? "paused" : " · paused");
      break;
    }
    case "failed":
      detail.textContent = standing.value.summary;
      break;
    default: {
      const other: { case: string } = standing;
      return unreachableArm(`${path}.standing`, other.case);
    }
  }
  if (detail.textContent !== "") card.appendChild(detail);
  return card;
}

/**
 * One stage of the head's merge: its state glyph -- the bubble's own, so the
 * two read alike -- and its words, decorated with its round past the first.
 * A settled stage says how long it ran on hover.
 */
export function drawRosterMergeQueueStage(u: RosterMergeQueueStage, path: string): HTMLElement {
  const state = requireCase(u.state, `${path}.state`);
  const el = document.createElement("span");
  el.className = "sb-mq-stage";
  el.setAttribute("data-state", state.case);
  const glyph = document.createElement("span");
  glyph.className = "sb-mq-stage-glyph";
  glyph.setAttribute("aria-hidden", "true");
  switch (state.case) {
    case "live":
      glyph.textContent = STATE_GLYPHS.live;
      break;
    case "succeeded":
      glyph.textContent = STATE_GLYPHS.succeeded;
      el.title = formatElapsed(msOf(state.value.endedAtMs, `${path}.state.ended_at_ms`) - msOf(state.value.startedAtMs, `${path}.state.started_at_ms`));
      break;
    case "failed":
      glyph.textContent = STATE_GLYPHS.failed;
      el.title = formatElapsed(msOf(state.value.endedAtMs, `${path}.state.ended_at_ms`) - msOf(state.value.startedAtMs, `${path}.state.started_at_ms`));
      break;
    default: {
      const other: { case: string } = state;
      return unreachableArm(`${path}.state`, other.case);
    }
  }
  const label = document.createElement("span");
  label.className = "sb-mq-stage-label";
  label.textContent = `${u.label}${roundSuffix(u.round)}`;
  el.append(glyph, label);
  return el;
}

/** When STAGE began, epoch ms, whatever its state. */
function stageStartedMs(stage: RosterMergeQueueStage, path: string): number {
  const state = requireCase(stage.state, `${path}.state`);
  return msOf(state.value.startedAtMs, `${path}.state.started_at_ms`);
}

/**
 * One merge waiting behind the head: its place in line, the credited
 * workspace's name, a front mark when it was put in line at the front, and
 * how long it has waited.
 */
export function drawRosterMergeQueueWaiting(
  u: RosterMergeQueueWaiting,
  place: number,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const line = drawSubjectControl(subject, sc, MERGE_QUEUE_WAITING_CLASS, `${path}.subject`);
  const position = document.createElement("span");
  position.className = "sb-mq-place";
  position.textContent = String(place);
  line.append(position, drawSubjectName(subject, `${path}.subject`));
  if (u.placedAtFront) {
    const front = document.createElement("span");
    front.className = "sb-mq-front";
    front.textContent = "front";
    front.title = "put in line at the front";
    line.appendChild(front);
  }
  line.appendChild(drawClock(sc, msOf(u.queuedAtMs, `${path}.queued_at_ms`)));
  return line;
}

/** A merge's name, with an async merge saying so on hover. */
function drawSubjectName(u: RosterMergeQueueSubject, path: string): HTMLElement {
  const mode = requireCase(requireMessage(u.mode, `${path}.mode`).mode, `${path}.mode.mode`);
  const name = document.createElement("span");
  name.className = "sb-mq-name";
  name.textContent = u.name;
  switch (mode.case) {
    case "sync":
      break;
    case "async":
      name.title = "async merge";
      break;
    default: {
      const other: { case: string } = mode;
      return unreachableArm(`${path}.mode.mode`, other.case);
    }
  }
  return name;
}

/** A clock counting up from STARTED_MS, as a row's when-column ticks (`tickElapsed`). */
function drawClock(sc: SidebarContext, startedMs: number): HTMLElement {
  const clock = document.createElement("span");
  clock.className = "sb-mq-clock";
  tickElapsed(clock, sc, startedMs);
  return clock;
}

/**
 * A line wearing CLASSNAME whose click selects the workspace U is credited to
 * and, when U names one, opens its merge's bubble in that workspace's feed
 * (`SelectWorkspaceRequest.reveal`). Without a bubble it selects as a row's
 * click does. It is selectable as a row's line is (`data-select`), never a
 * button.
 */
function drawSubjectControl(u: RosterMergeQueueSubject, sc: SidebarContext, className: string, path: string): HTMLElement {
  const workspace = requireMessage(u.workspace, `${path}.workspace`);
  const control = document.createElement("div");
  control.className = className;
  control.setAttribute("data-select", "");
  control.setAttribute("data-workspace", workspace.id);
  control.addEventListener("click", (event) => {
    event.preventDefault();
    const reveal = u.reveal;
    if (reveal === undefined) {
      void guardMalformed(sc.ctx, "sidebar.merge-queue.select", selectWorkspace(control, sc, workspace));
      return;
    }
    log.info("opening a merge's bubble from the sidebar's merge queue", {
      operation: "sidebar.merge-queue.reveal",
      context: { workspace: workspace.id, reveal: reveal.value },
    });
    void guardMalformed(
      sc.ctx,
      "sidebar.merge-queue.reveal",
      runVerb(control, {
        sc,
        rpc: "SelectWorkspace",
        call: (client) => client.selectWorkspace(create(SelectWorkspaceRequestSchema, { workspace, reveal })),
        schema: SelectWorkspaceResponseSchema,
        expectedArms: SELECT_WORKSPACE_EXPECTED_ARMS,
      }),
    );
  });
  return control;
}
