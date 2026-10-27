/**
 * merge-queue-view — THE MERGE QUEUES the Merge Queue section draws above its
 * recently merged groups (owner requests, 2026-10-24 to 2026-10-26).
 *
 * ONE GROUP PER REPOSITORY HOLDING A MERGE (`frontend.v1.RosterMergedSection.queues`):
 * the repository's label over its queue, in the repository sections' order.
 * The section's own "Merge Queue" label is drawn in the purple of a merge in
 * progress, and in the amber a paused merge bubble is framed in while a
 * repository's merge queue is paused
 * (`frontend.v1.RosterMergedSection.merge_queue_paused`), so a pause reads
 * from the header even with the section folded.
 *
 * A QUEUE IS DRAWN AS A SMALL MERGE BUBBLE AND ITS LINE (owner request,
 * 2026-10-25): the merge at the head as a card -- who it is credited to, the
 * stages it has been through, and the step it stands on (or why it failed) --
 * then every merge waiting behind it, numbered in the order it will run. A
 * merge is named as the rest of the roster credits it, so a hidden merge
 * workspace is never named here, and a click selects the credited workspace
 * and opens the merge's bubble in its feed.
 */
import { create } from "@bufbuild/protobuf";
import type {
  RosterMergedSection,
  RosterMergeQueueHead,
  RosterMergeQueueRepoGroup,
  RosterMergeQueueStage,
  RosterMergeQueueSubject,
  RosterMergeQueueWaiting,
  RosterRepoMergeQueue,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  SelectWorkspaceRequestSchema,
  SelectWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
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

/**
 * The attribute the Merge Queue section carries, valueless, while a
 * repository's merge queue is paused. The stylesheet colours its label from it.
 */
export const MERGE_QUEUE_PAUSED_ATTRIBUTE = "data-merge-queue-paused";

/** Whether U says a repository's merge queue is paused: the marker's presence is the fact. */
export function drawRosterMergedSectionMergeQueuePaused(u: RosterMergedSection): boolean {
  return u.mergeQueuePaused !== undefined;
}

/** The class of the box the section's queue groups are drawn in, above its merged groups. */
export const MERGE_QUEUES_CLASS = "sb-merge-queues";

/** The class of one repository's queue group, and of its header. */
export const MERGE_QUEUE_GROUP_CLASS = "mq-group";
export const MERGE_QUEUE_GROUP_HEAD_CLASS = "mq-group-head";

/** The class of a queue group's body: its head's card and its waiting merges. */
export const MERGE_QUEUE_CLASS = "sb-merge-queue";

/**
 * One repository's queue group: its label header (HEADER, drawn by the
 * caller's group-header drawer so it reads as the merged groups' do) over
 * its queue.
 */
export function drawRosterMergeQueueRepoGroup(
  u: RosterMergeQueueRepoGroup,
  sc: SidebarContext,
  path: string,
  header: (path: string) => HTMLElement,
): HTMLElement {
  const key = requireMessage(requireMessage(u.key, `${path}.key`).repository, `${path}.key.repository`);
  const group = document.createElement("div");
  group.className = MERGE_QUEUE_GROUP_CLASS;
  group.setAttribute("data-repository", key.id);
  const head = header(`${path}.header`);
  head.classList.add(MERGE_QUEUE_GROUP_HEAD_CLASS);
  group.appendChild(head);
  group.appendChild(drawRosterMergeQueue(requireMessage(u.queue, `${path}.queue`), sc, `${path}.queue`));
  return group;
}

/** The class of the head's card. Each card is one line of the band fit. */
export const MERGE_QUEUE_HEAD_CLASS = "sb-mq-head";

/** The class of a waiting merge's line. Each is one line of the band fit. */
export const MERGE_QUEUE_WAITING_CLASS = "sb-mq-waiting";

/**
 * A queue group's body: the head's card, then each waiting merge, front
 * first. It is no arranged list (no bucket), so nothing in it is a drag grip.
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
