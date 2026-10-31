/**
 * merge-queue-view — THE MERGE QUEUES the Deploys section draws under its
 * "Deploy Queue" heading, above its recently deployed groups (owner
 * requests, 2026-10-24 to 2026-10-27).
 *
 * ONE GROUP PER REPOSITORY WHOSE LINE HOLDS A MERGE
 * (`frontend.v1.RosterMergedSection.queues`): the repository's label and the
 * merges in its line, "(N)", over its queue, in the repository sections'
 * order. The section's own "Deploys" label is always the purple of a
 * merge in progress; a PAUSE IS DRAWN ON ITS OWN REPOSITORY (owner request,
 * 2026-10-27): the label in the error red, the queue's merge glyphs in the
 * amber a paused merge bubble is framed in, and the head's clock stopped at
 * the pause. The label's hover says who paused it and when, and a click on it
 * resumes the queue.
 *
 * A QUEUE IS DRAWN AS A SMALL MERGE BUBBLE AND ITS LINE (owner request,
 * 2026-10-25): the merge at the head as a card -- who it is credited to, the
 * stages it has been through, and the step it stands on -- then every merge
 * waiting behind it, in the order it will run. A merge is named as the rest
 * of the roster credits it, so a hidden merge workspace is never named here,
 * and a click selects the credited workspace and opens the merge's bubble in
 * its feed. A WAITING MERGE IS DRAGGED to another place in its line
 * (`UpdateMergeQueueMoveTo`, owner request 2026-10-27), through the rail's one
 * drag gesture (`drag.ts`).
 */
import { create } from "@bufbuild/protobuf";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  UpdateMergeQueueResponseSchema,
  type UpdateMergeQueueMoveTo,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import type {
  RosterMergeQueueHead,
  RosterMergeQueuePaused,
  RosterMergeQueueRepoGroup,
  RosterMergeQueueStage,
  RosterMergeQueueSubject,
  RosterMergeQueueRemediating,
  RosterMergeQueueWaiting,
  RosterRepoMergeQueue,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  SelectWorkspaceRequestSchema,
  SelectWorkspaceResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { formatAge, formatElapsed } from "../duration.js";
import { settledElapsedClock } from "../elapsed-clock.js";
import { roundSuffix, STATE_GLYPHS } from "../feed/merge/tab-strip.js";
import { log } from "../log.js";
import { MERGE_GLYPH } from "../merge-glyph.js";
import { guardMalformed } from "../rpc/guard.js";
import { updateMergeQueueRefusal } from "../rpc/merge-queue-refusal.js";
import { userRequester } from "../rpc/requester.js";
import { SELECT_WORKSPACE_EXPECTED_ARMS } from "../rpc/refusal.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { pressToDrag, type DragSpec } from "./drag.js";
import { selectWorkspace, tickElapsed, tickPaint } from "./row.js";
import { sendHeld } from "./arrange.js";
import { fireVerb, runVerb } from "./verbs.js";

/**
 * The attribute a queue group carries while its queue is paused: `pause`, or
 * `hard` for a hard stop. The stylesheet draws the pause from it.
 */
export const MERGE_QUEUE_PAUSED_ATTRIBUTE = "data-paused";

/** The attribute the head's card carries, valueless, while nothing of its merge's work runs. */
export const MERGE_QUEUE_SUSPENDED_ATTRIBUTE = "data-suspended";

/** The class of a queue group's "(N)", the merges in its line. */
export const MERGE_QUEUE_COUNT_CLASS = "sb-count";

/** The class of the box the section's queue groups are drawn in, above its merged groups. */
export const MERGE_QUEUES_CLASS = "sb-merge-queues";

/**
 * The class of the "Deploy Queue" heading at the top of `MERGE_QUEUES_CLASS`,
 * drawn only while a queue is listed under it (owner request, 2026-10-27).
 */
export const DEPLOY_QUEUE_LABEL_CLASS = "sb-deploy-queue-label";

/** The class of one repository's queue group, and of its header. */
export const MERGE_QUEUE_GROUP_CLASS = "mq-group";
export const MERGE_QUEUE_GROUP_HEAD_CLASS = "mq-group-head";

/** The class of a queue group's body: its head's card and its waiting merges. */
export const MERGE_QUEUE_CLASS = "sb-merge-queue";

/**
 * One repository's queue group: its label header (HEADER, drawn by the
 * caller's group-header drawer so it reads as the merged groups' do) with
 * the merges in its line, over its queue. While the queue is paused the
 * header says who paused it on hover and resumes it on a click.
 */
export function drawRosterMergeQueueRepoGroup(
  u: RosterMergeQueueRepoGroup,
  sc: SidebarContext,
  path: string,
  header: (path: string) => HTMLElement,
): HTMLElement {
  const key = requireMessage(requireMessage(u.key, `${path}.key`).repository, `${path}.key.repository`);
  const queue = requireMessage(u.queue, `${path}.queue`);
  const group = document.createElement("div");
  group.className = MERGE_QUEUE_GROUP_CLASS;
  group.setAttribute("data-repository", key.id);
  const head = header(`${path}.header`);
  head.classList.add(MERGE_QUEUE_GROUP_HEAD_CLASS);
  const count = document.createElement("span");
  count.className = MERGE_QUEUE_COUNT_CLASS;
  count.textContent = `(${requireMessage(u.count, `${path}.count`).workspaces})`;
  head.appendChild(count);
  group.appendChild(head);
  if (queue.paused !== undefined) {
    const paused = queue.paused;
    group.setAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE, paused.hard ? "hard" : "pause");
    tickPaint(sc, (nowMs) => {
      head.title = pausedText(paused, nowMs, `${path}.queue.paused`);
    });
    head.addEventListener("click", (event) => {
      event.preventDefault();
      void resumeQueue(head, sc, key);
    });
  }
  group.appendChild(drawRosterMergeQueue(queue, sc, `${path}.queue`));
  return group;
}

/**
 * The hover on a paused queue's label: whether it was paused or stopped, how
 * long ago and by whom, as far as the daemon recorded them, and that a click
 * resumes it.
 */
export function pausedText(u: RosterMergeQueuePaused, nowMs: number, path: string): string {
  let text = u.hard ? "Stopped" : "Paused";
  if (u.pausedAtMs !== undefined) text += ` ${formatAge(nowMs - msOf(u.pausedAtMs, `${path}.paused_at_ms`))} ago`;
  if (u.by !== undefined) {
    const asker = requireCase(u.by.asker, `${path}.by.asker`);
    switch (asker.case) {
      case "user":
        text += " by you";
        break;
      case "workspace":
        text += ` by ${asker.value.name}`;
        break;
      default: {
        const other: { case: string } = asker;
        return unreachableArm(`${path}.by.asker`, other.case);
      }
    }
  }
  return `${text}. Click to resume.`;
}

/** Ask the daemon to resume REPOSITORY's queue, drawing a refusal at CONTROL. */
function resumeQueue(control: HTMLElement, sc: SidebarContext, repository: { id: string; dir: string }): Promise<boolean> {
  log.info("resuming a merge queue from the sidebar", {
    operation: "sidebar.merge-queue.resume",
    context: { repository: repository.id },
  });
  return fireVerb(control, {
    sc,
    rpc: "UpdateMergeQueue",
    // The webapp acts on the queue for the user alone, and says so.
    call: (client) =>
      client.updateMergeQueue({
        action: { case: "resume", value: { repository: { id: repository.id, dir: repository.dir } } },
        requester: userRequester(),
      }),
    schema: UpdateMergeQueueResponseSchema,
    refusalText: updateMergeQueueRefusal,
  });
}

/** The class of the head's card. Each card is one line of the band fit. */
export const MERGE_QUEUE_HEAD_CLASS = "sb-mq-head";

/** The class of a waiting merge's line. Each is one line of the band fit. */
export const MERGE_QUEUE_WAITING_CLASS = "sb-mq-waiting";

/** The class of a remediating merge's card, the queue's second spot. Each is one line of the band fit. */
export const MERGE_QUEUE_REMEDIATING_CLASS = "sb-mq-remediating";

/**
 * A queue group's body: the head's card, then each merge remediating out of
 * line (the second spot), then each waiting merge, front first. A listed
 * queue has its head unless its line is empty and a merge remediates. A
 * press on a waiting merge drags it among the waiting (`queueDrag`).
 */
export function drawRosterMergeQueue(u: RosterRepoMergeQueue, sc: SidebarContext, path: string): HTMLElement {
  const body = document.createElement("div");
  body.className = `rows ${MERGE_QUEUE_CLASS}`;
  // A HEAD IS ABSENT ONLY WHILE THE LINE IS EMPTY AND A MERGE REMEDIATES
  // (frontend.v1.RosterRepoMergeQueue.head); anywhere else it is required.
  if (u.head !== undefined || u.remediating.length === 0) {
    body.appendChild(drawRosterMergeQueueHead(requireMessage(u.head, `${path}.head`), u.paused, sc, `${path}.head`));
  }
  u.remediating.forEach((remediating, index) => {
    body.appendChild(drawRosterMergeQueueRemediating(remediating, sc, `${path}.remediating[${index}]`));
  });
  const refs = new Map<HTMLElement, WorkspaceRef>();
  u.waiting.forEach((waiting, index) => {
    const waitingPath = `${path}.waiting[${index}]`;
    const line = drawRosterMergeQueueWaiting(waiting, sc, waitingPath);
    refs.set(line, requireMessage(requireMessage(waiting.subject, `${waitingPath}.subject`).workspace, `${waitingPath}.subject.workspace`));
    body.appendChild(line);
  });
  body.addEventListener("pointerdown", (event) => {
    if (event.button !== 0 || !event.isPrimary) return;
    const spec = queueDrag(sc, body, event.target as Element | null, refs);
    if (spec !== null) pressToDrag(sc.arrange, spec, event);
  });
  return body;
}

/**
 * A press on a waiting merge of BODY drags it among the merges waiting
 * there; a drop elsewhere than where it started moves it beside the waiting
 * merge it now stands next to (`UpdateMergeQueueMoveTo`). The head is no grip:
 * only a waiting merge moves.
 */
export function queueDrag(
  sc: SidebarContext,
  body: HTMLElement,
  target: Element | null,
  refs: ReadonlyMap<HTMLElement, WorkspaceRef>,
): DragSpec | null {
  const item = target?.closest<HTMLElement>(`.${MERGE_QUEUE_WAITING_CLASS}`) ?? null;
  if (item === null || item.parentElement !== body) return null;
  const workspace = refs.get(item);
  if (workspace === undefined) throw new Error("invariant: every waiting merge drawn names its workspace");
  const section = body.closest<HTMLElement>(".repo");
  const scroller = body.parentElement?.closest<HTMLElement>(".rows") ?? null;
  if (section === null || scroller === null) throw new Error("invariant: a queue is drawn inside the Deploys section's rows");
  const waiting = (): HTMLElement[] => [...body.children].filter((el): el is HTMLElement => refs.has(el as HTMLElement));
  return {
    kind: "merge",
    item,
    box: section,
    scroller,
    lists: () => [body],
    itemsOf: waiting,
    dropped: (_list, before) => {
      const place = placeBeside(waiting(), item, before, refs);
      void moveInLine(sc, workspace, place);
    },
  };
}

/**
 * Where a merge dropped just before BEFORE (null: after every waiting merge)
 * goes, named by the waiting merge it now stands beside: ahead of BEFORE, or
 * behind the last of WAITING when it was dropped at the end.
 */
export function placeBeside(
  waiting: readonly HTMLElement[],
  item: HTMLElement,
  before: HTMLElement | null,
  refs: ReadonlyMap<HTMLElement, WorkspaceRef>,
): UpdateMergeQueueMoveTo["place"] {
  const anchorOf = (el: HTMLElement): WorkspaceRef => {
    const ref = refs.get(el);
    if (ref === undefined) throw new Error("invariant: a waiting merge's anchor names its workspace");
    return ref;
  };
  if (before !== null) return { case: "aheadOf", value: anchorOf(before) };
  const others = waiting.filter((el) => el !== item);
  const last = others[others.length - 1];
  if (last === undefined) throw new Error("invariant: a merge dropped elsewhere than it started has a waiting merge beside it");
  return { case: "behind", value: anchorOf(last) };
}

/**
 * Ask the daemon to move WORKSPACE's waiting merge to PLACE. The drawing is
 * held until the answer (`sendHeld`), so the moved line stands where it was
 * dropped until the push draws the queue in its new order.
 */
function moveInLine(sc: SidebarContext, workspace: WorkspaceRef, place: UpdateMergeQueueMoveTo["place"]): Promise<boolean> {
  log.info("moving a merge in its queue from the sidebar", {
    operation: "sidebar.merge-queue.move",
    context: { workspace: workspace.id, side: place.case, anchor: place.value?.id },
  });
  return sendHeld(sc, sc.arrange.status, {
    rpc: "UpdateMergeQueue",
    call: (client) =>
      client.updateMergeQueue({
        action: { case: "moveTo", value: { workspace, place } },
        requester: userRequester(),
      }),
    schema: UpdateMergeQueueResponseSchema,
    refusalText: updateMergeQueueRefusal,
    // UpdateMergeQueue answers no roster revision: the hold lets go with the
    // answer, and the push already drawn or next drawn carries the order.
    revision: () => null,
  });
}

/**
 * The head's card: a small merge bubble. Its head line is the merge glyph, the
 * credited workspace's name and how long it has run or waited -- stopped at
 * the pause while its queue is PAUSED, and not drawn for a pause whose
 * instant was not recorded; then its stages so far; then where it stands:
 * its step, or that it waits, and that its queue is paused or stopped. A
 * head nothing of whose work runs is marked suspended, and the stylesheet
 * stills its live stage.
 */
export function drawRosterMergeQueueHead(
  u: RosterMergeQueueHead,
  paused: RosterMergeQueuePaused | undefined,
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
  const pauseWord = paused === undefined ? undefined : paused.hard ? "stopped" : "paused";
  const clock = (startedMs: number): void => {
    const drawn = drawHeadClock(sc, startedMs, paused, `${path}.paused`);
    if (drawn !== null) line.appendChild(drawn);
  };
  // THE HEAD'S CLOCK STARTS AS IT TAKES THE HEAD SLOT (owner ruling,
  // 2026-10-30), never at its first stage or its enqueueing: a merge that
  // rebased in the background and waited hours in line reads zero here.
  clock(msOf(u.headSinceMs, `${path}.head_since_ms`));
  switch (standing.case) {
    case "waiting": {
      detail.textContent = pauseWord ?? "next";
      break;
    }
    case "merging": {
      detail.textContent = standing.value.step ?? "";
      if (pauseWord !== undefined) detail.append(detail.textContent === "" ? pauseWord : ` · ${pauseWord}`);
      card.toggleAttribute(MERGE_QUEUE_SUSPENDED_ATTRIBUTE, standing.value.suspended !== undefined);
      break;
    }
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

/** The class of the bullet a waiting merge's line opens with. */
export const MERGE_QUEUE_BULLET_CLASS = "sb-mq-bullet";

/**
 * THE SECOND SPOT (owner ruling, 2026-10-31): one merge that headed the
 * queue, finished its test suite with failures and stepped aside to fix them
 * out of line. Drawn under the head as a smaller, quieter card than the
 * head's -- the merge glyph, its name, how long it has remediated -- and
 * above the merges waiting, with a "remediating" line beneath. A click
 * reveals its merge bubble, as the head's does.
 */
export function drawRosterMergeQueueRemediating(u: RosterMergeQueueRemediating, sc: SidebarContext, path: string): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const card = drawSubjectControl(subject, sc, MERGE_QUEUE_REMEDIATING_CLASS, `${path}.subject`);
  const line = document.createElement("div");
  line.className = "sb-mq-head-line";
  const glyph = document.createElement("span");
  glyph.className = "sb-mq-glyph";
  glyph.setAttribute("aria-hidden", "true");
  glyph.textContent = MERGE_GLYPH;
  line.append(glyph, drawSubjectName(subject, `${path}.subject`), drawClock(sc, msOf(u.sinceMs, `${path}.since_ms`)));
  const detail = document.createElement("div");
  detail.className = "sb-mq-detail";
  detail.textContent = "remediating";
  card.append(line, detail);
  return card;
}

/**
 * One merge waiting behind the head, indented under the head's name: a bullet
 * in the name's own color (owner request, 2026-10-27), the credited
 * workspace's name, a front mark when it was put in line at the front, and how
 * long it has waited. Its place is the line's order; it is not numbered (owner
 * request, 2026-10-27).
 */
export function drawRosterMergeQueueWaiting(
  u: RosterMergeQueueWaiting,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const line = drawSubjectControl(subject, sc, MERGE_QUEUE_WAITING_CLASS, `${path}.subject`);
  const bullet = document.createElement("span");
  bullet.className = MERGE_QUEUE_BULLET_CLASS;
  bullet.setAttribute("aria-hidden", "true");
  bullet.textContent = "•";
  line.append(bullet, drawSubjectName(subject, `${path}.subject`));
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
 * The head's clock from STARTED_MS: ticking while its queue runs, stopped at
 * the instant of PAUSED while it is paused, and null (not drawn) for a pause
 * whose instant the daemon did not record.
 */
export function drawHeadClock(
  sc: SidebarContext,
  startedMs: number,
  paused: RosterMergeQueuePaused | undefined,
  path: string,
): HTMLElement | null {
  if (paused === undefined) return drawClock(sc, startedMs);
  if (paused.pausedAtMs === undefined) return null;
  return settledElapsedClock("sb-mq-clock", msOf(paused.pausedAtMs, `${path}.paused_at_ms`) - startedMs);
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
