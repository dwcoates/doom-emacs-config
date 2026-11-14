/**
 * merge-queue-view — A REPOSITORY'S MERGE QUEUE, drawn in its group in the
 * Deploys section above the repository's recent deploys (owner requests,
 * 2026-10-24 to 2026-10-31; frontend.v1.RosterMergedRepoGroupQueued).
 *
 * The section's own "Deploys" label is always the purple of a merge in
 * progress; a PAUSE IS DRAWN ON ITS OWN REPOSITORY (owner request,
 * 2026-10-27): the group's label in the error red, the queue's merge glyphs
 * in the amber a paused merge bubble is framed in, and the head's clock
 * stopped at the pause. The label's hover says who paused it and when. A
 * DOUBLE CLICK ON THE LABEL PAUSES A RUNNING QUEUE OR RESUMES A PAUSED ONE
 * (owner request, 2026-10-31), a single click being the group's fold, and
 * A SINGLE CLICK ON THE QUEUE'S MERGE GLYPH DOES THE SAME (owner request,
 * 2026-11-11): the glyph is a control on every queue group drawn.
 *
 * A QUEUE IS DRAWN AS A SMALL MERGE BUBBLE AND ITS LINE (owner request,
 * 2026-10-25): the merge at the head as a card -- who it is credited to, the
 * stages it has been through, and the step it stands on -- then every merge
 * waiting behind it, in the order it will run. A merge is named as the rest
 * of the roster credits it, so a hidden merge workspace is never named here,
 * and a click selects the credited workspace and opens the merge's bubble in
 * its feed. A WAITING MERGE IS DRAGGED to another place in its line
 * (`UpdateMergeQueueMoveTo`, owner request 2026-10-27), through the webapp's
 * one drag gesture (`drag.ts`), placed as the feed's queue tab places it
 * (`merge-queue-move.ts`).
 */
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  UpdateMergeQueueResponseSchema,
  type UpdateMergeQueueMoveTo,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import { RequeueMergeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_requeue_merge_pb";
import { DiscardMergeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_discard_merge_pb";
import type {
  RosterMergeQueueGithubEntry,
  RosterMergeQueueOffQueue,
  RosterMergeQueueGithubPlace,
  RosterMergeQueueHead,
  RosterMergeQueueMediation,
  RosterMergeQueueMediationTarget,
  RosterMergeQueuePaused,
  RosterMergeQueueStage,
  RosterMergeQueueSubject,
  RosterMergeQueueWaiting,
  RosterRepoMergeQueue,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { formatAge, formatElapsed } from "../duration.js";
import { settledElapsedClock } from "../elapsed-clock.js";
import { roundSuffix, STATE_GLYPHS } from "../feed/merge/tab-strip.js";
import { log } from "../log.js";
import { MERGE_GLYPH } from "../merge-glyph.js";
import { guardMalformed } from "../rpc/guard.js";
import { discardMergeRefusal, requeueMergeRefusal, updateMergeQueueRefusal } from "../rpc/merge-queue-refusal.js";
import { userRequester } from "../rpc/requester.js";
import { moveToAction, waitingMergeDrag } from "../merge-queue-move.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { createControl } from "../control.js";
import type { SidebarContext } from "./context.js";
import { pressToDrag, type DragSpec } from "../drag.js";
import { labelledControl } from "../control.js";
import { selectWorkspace, tickElapsed } from "./row.js";
import { sendHeld } from "./arrange.js";
import { fireVerb } from "./verbs.js";

/**
 * The attribute a queue group carries while its queue is paused: `pause`, or
 * `hard` for a hard stop. The stylesheet draws the pause from it.
 */
export const MERGE_QUEUE_PAUSED_ATTRIBUTE = "data-paused";

/** The attribute the head's card carries, valueless, while nothing of its merge's work runs. */
export const MERGE_QUEUE_SUSPENDED_ATTRIBUTE = "data-suspended";

/** The class of a queue group's body: its head's card and its waiting merges. */
export const MERGE_QUEUE_CLASS = "sb-merge-queue";

/**
 * A queued group's line as its fold reads it (`deploys-fold.ts`): the
 * workspaces at its head, at or awaiting its repair slots, and waiting, in
 * order. A change to it
 * is a change to the queue, which drops a fold chosen by hand.
 */
export function queueLine(u: RosterRepoMergeQueue): string {
  return queueSubjects(u)
    .map((subject) => subject?.workspace?.id ?? "")
    .join(" ");
}

function queueSubjects(u: RosterRepoMergeQueue) {
  return [
    ...(u.head === undefined ? [] : [u.head.subject]),
    ...repairsOf(u).map((r) => r.subject),
    ...u.waiting.map((w) => w.subject),
    ...u.github.map((g) => g.subject),
  ];
}

/** How many merges a queue holds: the one at its head, those at or awaiting its repair slots, those waiting and those in a GitHub merge queue. */
export function queueCount(u: RosterRepoMergeQueue): number {
  return queueSubjects(u).length;
}

/**
 * Make HEAD, the header of a queued GROUP, the queue's control: a double
 * click pauses a running queue or resumes a paused one, and a paused queue
 * marks its group so the stylesheet draws the pause. The hover's words are
 * `queueHoverText`'s.
 */
export function drawQueuedGroupHead(
  group: HTMLElement,
  head: HTMLElement,
  queue: RosterRepoMergeQueue,
  key: RepositoryRef,
  sc: SidebarContext,
  path: string,
): void {
  if (queue.paused !== undefined) group.setAttribute(MERGE_QUEUE_PAUSED_ATTRIBUTE, queue.paused.hard ? "hard" : "pause");
  head.addEventListener("dblclick", (event) => {
    event.preventDefault();
    void toggleQueue(head, sc, key, queue);
  });
  log.debug("drew a queued Deploys group", {
    operation: "sidebar.merge-queue.group",
    context: { repository: key.id, path, paused: queue.paused !== undefined },
  });
}

/** The hover a queued group's header adds: what a double click does, and a pause's record. */
export function queueHoverText(u: RosterRepoMergeQueue, nowMs: number, path: string): string {
  if (u.paused === undefined) return "Double-click to pause the queue.";
  return pausedText(u.paused, nowMs, `${path}.paused`);
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
  return `${text}. Double-click to resume.`;
}

/** What a double click on a queued group's header, or a click on its glyph, asks of its queue. */
export type QueueSwitch = "pause" | "resume";

/** What a toggle of QUEUE asks: a running queue is paused, a paused (or stopped) one resumed. */
export function queueSwitchOf(queue: RosterRepoMergeQueue): QueueSwitch {
  return queue.paused === undefined ? "pause" : "resume";
}

/**
 * THE ONE QUEUE TOGGLE: the header's double click and the glyph's click both
 * ask the daemon to pause or resume REPOSITORY's QUEUE through here, drawing
 * a refusal at CONTROL.
 */
export function toggleQueue(control: HTMLElement, sc: SidebarContext, repository: RepositoryRef, queue: RosterRepoMergeQueue): Promise<boolean> {
  return switchQueue(control, sc, repository, queueSwitchOf(queue));
}

/** Ask the daemon to pause or resume REPOSITORY's queue, as TO says, drawing a refusal at CONTROL. */
function switchQueue(control: HTMLElement, sc: SidebarContext, repository: RepositoryRef, to: QueueSwitch): Promise<boolean> {
  log.info(to === "pause" ? "pausing a merge queue from the sidebar" : "resuming a merge queue from the sidebar", {
    operation: `sidebar.merge-queue.${to}`,
    context: { repository: repository.id },
  });
  const scope = { repository: { id: repository.id, dir: repository.dir } };
  return fireVerb(control, {
    sc,
    rpc: "UpdateMergeQueue",
    // A refusal's or a failure's record names the queue and what was asked of it.
    context: { repository: repository.id, action: to },
    // The webapp acts on the queue for the user alone, and says so.
    call: (client) =>
      client.updateMergeQueue({
        action: to === "pause" ? { case: "pause", value: scope } : { case: "resume", value: scope },
        requester: userRequester(),
      }),
    schema: UpdateMergeQueueResponseSchema,
    refusalText: updateMergeQueueRefusal,
  });
}

/** The repository a queue group is drawn for: its key, and the name its label draws. */
export interface QueueRepository {
  key: RepositoryRef;
  name: string;
}

/** The class of the queue's merge glyph, the control that toggles its queue. */
export const MERGE_QUEUE_GLYPH_CLASS = "sb-mq-glyph";

/** The glyph's hover and accessible name: what its click does to REPOSITORY's queue. */
export function queueGlyphTitle(name: string, to: QueueSwitch): string {
  return `${to === "pause" ? "Pause" : "Resume"} the ${name} queue`;
}

/**
 * The queue's merge glyph, a control whose click pauses or resumes
 * REPOSITORY's QUEUE (`toggleQueue`). The click never reaches the line it is
 * drawn in, so it selects no workspace; the line goes busy while it is asked
 * and a refusal is drawn under it.
 */
export function drawQueueGlyph(queue: RosterRepoMergeQueue, repository: QueueRepository, sc: SidebarContext): HTMLElement {
  const glyph = createControl();
  glyph.className = MERGE_QUEUE_GLYPH_CLASS;
  glyph.textContent = MERGE_GLYPH;
  const title = queueGlyphTitle(repository.name, queueSwitchOf(queue));
  glyph.title = title;
  glyph.setAttribute("aria-label", title);
  glyph.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    const line = glyph.closest<HTMLElement>(`.${MERGE_QUEUE_CARD_LINE_CLASS}`);
    if (line === null) throw new Error("invariant: a queue's glyph is drawn in a card line");
    void toggleQueue(line, sc, repository.key, queue);
  });
  return glyph;
}

/** The class of the line a queue with no head draws its glyph on. Each is one line of the band fit. */
export const MERGE_QUEUE_CONTROL_CLASS = "sb-mq-control";

/**
 * The line a queue with no head draws its GLYPH on, above the merges
 * remediating and those in a GitHub merge queue, so every queue group has
 * its toggle.
 */
export function drawQueueControlLine(glyph: HTMLElement): HTMLElement {
  const line = document.createElement("div");
  line.className = `${MERGE_QUEUE_CARD_LINE_CLASS} ${MERGE_QUEUE_CONTROL_CLASS}`;
  line.appendChild(glyph);
  return line;
}

/** The class of the head's card. Each card is one line of the band fit. */
export const MERGE_QUEUE_HEAD_CLASS = "sb-mq-head";

/** The class of a waiting merge's line. Each is one line of the band fit. */
export const MERGE_QUEUE_WAITING_CLASS = "sb-mq-waiting";

/** The class of the numeral a waiting merge's line opens with: its position among the waiting. */
export const MERGE_QUEUE_POSITION_CLASS = "sb-mq-position";

/** The class of a GitHub merge queue standing's line. Each is one line of the band fit. */
export const MERGE_QUEUE_GITHUB_CLASS = "sb-mq-github";

/** The class of a merge out of line's line: at or awaiting a repair slot. Each is one line of the band fit. */
export const MERGE_QUEUE_REMEDIATING_CLASS = "sb-mq-remediating";

/**
 * A queue group's body: the head's card, then the merge holding each repair
 * slot and the merges awaiting each (repairsOf), then each waiting merge in
 * the order it will run. A listed queue has its head unless its line is empty
 * and a merge stands out of line. A press on a waiting merge drags it among
 * the waiting (`queueDrag`). A queue without a head draws REPOSITORY's queue
 * glyph on a line of its own in the head's place.
 */
export function drawRosterMergeQueue(u: RosterRepoMergeQueue, repository: QueueRepository, sc: SidebarContext, path: string): HTMLElement {
  const body = document.createElement("div");
  body.className = `rows ${MERGE_QUEUE_CLASS}`;
  const glyph = drawQueueGlyph(u, repository, sc);
  // A HEAD IS ABSENT ONLY WHILE THE LINE IS EMPTY AND A MERGE STANDS OUT OF
  // LINE, A WORKSPACE STANDS IN A GITHUB MERGE QUEUE, OR A MERGE STANDS OFF
  // THE QUEUE
  // (frontend.v1.RosterRepoMergeQueue.head); anywhere else it is required.
  const repairs = repairsOf(u, path);
  if (u.head !== undefined || (repairs.length === 0 && u.github.length === 0 && u.offQueue.length === 0 && u.mediation === undefined)) {
    body.appendChild(drawRosterMergeQueueHead(requireMessage(u.head, `${path}.head`), u.paused, glyph, sc, `${path}.head`));
  } else {
    body.appendChild(drawQueueControlLine(glyph));
  }
  for (const repair of repairs) {
    body.appendChild(drawRosterMergeQueueRepair(repair, sc));
  }
  const refs = new Map<HTMLElement, WorkspaceRef>();
  u.waiting.forEach((waiting, index) => {
    const waitingPath = `${path}.waiting[${index}]`;
    const line = drawRosterMergeQueueWaiting(waiting, index + 1, sc, waitingPath);
    refs.set(line, requireMessage(requireMessage(waiting.subject, `${waitingPath}.subject`).workspace, `${waitingPath}.subject.workspace`));
    body.appendChild(line);
  });
  u.github.forEach((github, index) => {
    body.appendChild(drawRosterMergeQueueGithub(github, sc, `${path}.github[${index}]`));
  });
  u.offQueue.forEach((off, index) => {
    body.appendChild(drawRosterMergeQueueOffQueue(off, sc, `${path}.off_queue[${index}]`));
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
  const section = body.closest<HTMLElement>(".repo");
  const scroller = section?.querySelector<HTMLElement>(":scope > .rows") ?? null;
  if (section === null || scroller === null) throw new Error("invariant: a queue is drawn inside the Deploys section's rows");
  return waitingMergeDrag({
    kind: "merge",
    item,
    list: body,
    box: section,
    scroller,
    refs,
    move: (workspace, place) => void moveInLine(sc, workspace, place),
  });
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
        action: moveToAction(workspace, place),
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
 * The head's card: a small merge bubble. Its head line is the merge GLYPH (the queue's toggle, `drawQueueGlyph`), the
 * credited workspace's name and how long it has run or waited -- stopped at
 * the pause while its queue is PAUSED, and not drawn for a pause whose
 * instant was not recorded; then its stages so far. IT DRAWS NO STATUS LINE
 * (owner request, 2026-11-09): the stage strip already names the step it
 * stands on, and a pause is drawn on the group's label. A head nothing of
 * whose work runs is marked suspended, and the stylesheet stills its live
 * stage.
 */
export function drawRosterMergeQueueHead(
  u: RosterMergeQueueHead,
  paused: RosterMergeQueuePaused | undefined,
  glyph: HTMLElement,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const standing = requireCase(u.standing, `${path}.standing`);
  const card = drawSubjectControl(subject, sc, MERGE_QUEUE_HEAD_CLASS, `${path}.subject`);
  card.setAttribute("data-standing", standing.case);

  const line = drawCardLine(subject, glyph, `${path}.subject`);
  card.appendChild(line);

  if (u.stages.length > 0) {
    const strip = document.createElement("div");
    strip.className = "sb-mq-stages";
    u.stages.forEach((stage, index) => strip.appendChild(drawRosterMergeQueueStage(stage, `${path}.stages[${index}]`)));
    card.appendChild(strip);
  }

  const clock = (startedMs: number): void => {
    const drawn = drawHeadClock(sc, startedMs, paused, `${path}.paused`);
    if (drawn !== null) line.appendChild(drawn);
  };
  // THE HEAD'S CLOCK STARTS AS IT TAKES THE HEAD SLOT (owner ruling,
  // 2026-10-30), never at its first stage or its enqueueing: a merge that
  // rebased in the background and waited hours in line reads zero here.
  clock(msOf(u.headSinceMs, `${path}.head_since_ms`));
  switch (standing.case) {
    case "waiting":
      break;
    case "merging":
      card.toggleAttribute(MERGE_QUEUE_SUSPENDED_ATTRIBUTE, standing.value.suspended !== undefined);
      break;
    default: {
      const other: { case: string } = standing;
      return unreachableArm(`${path}.standing`, other.case);
    }
  }
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

/** The class of a merge out of line's label: the repair it makes or awaits, after its name. */
export const MERGE_QUEUE_REASON_CLASS = "sb-mq-reason";

/**
 * One merge out of line as its line draws it: the repair slot it holds or
 * awaits, in the label's words.
 */
export interface QueueRepair {
  subject: RosterMergeQueueSubject | undefined;
  sinceMs: bigint;
  words: string;
  title: string;
  path: string;
}

/**
 * The queue's merges out of line, in the order they are drawn (owner rulings,
 * 2026-11-11 and 2026-11-14): the merge holding the fixing-tests slot, every
 * merge resolving conflicts (all at once, in the order each stepped aside),
 * then the merges awaiting the fixing-tests slot, in the order each is
 * granted it. The queue holds at most one merge fixing tests, which its shape
 * already says (frontend.v1.RosterRepoMergeQueue.fixing_tests).
 */
export function repairsOf(u: RosterRepoMergeQueue, path = "queue"): QueueRepair[] {
  const out: QueueRepair[] = [];
  if (u.fixingTests !== undefined) {
    out.push({ subject: u.fixingTests.subject, sinceMs: u.fixingTests.sinceMs, words: "fixing tests",
      title: "fixing its tests out of line", path: `${path}.fixing_tests` });
  }
  u.resolvingConflicts.forEach((r, index) => {
    out.push({ subject: r.subject, sinceMs: r.sinceMs, words: "resolving conflicts",
      title: "resolving its conflicts out of line", path: `${path}.resolving_conflicts[${index}]` });
  });
  u.awaitingFixingTests.forEach((a, index) => {
    out.push({ subject: a.subject, sinceMs: a.sinceMs, words: "waiting to fix tests",
      title: "waiting out of line for the fixing-tests slot", path: `${path}.awaiting_fixing_tests[${index}]` });
  });
  return out;
}

/**
 * ONE MERGE OUT OF LINE (owner rulings, 2026-10-31, 2026-11-03 and
 * 2026-11-11), at a repair slot or awaiting one. It stands above the merges
 * waiting and is DRAWN AS ONE OF THEM (owner request, 2026-11-10): its name,
 * the repair as a queue label (drawQueueLabel), and how long it has held or
 * awaited its slot. A click reveals its merge bubble, as the head's does.
 */
export function drawRosterMergeQueueRepair(u: QueueRepair, sc: SidebarContext): HTMLElement {
  const subject = requireMessage(u.subject, `${u.path}.subject`);
  const line = drawSubjectControl(subject, sc, `${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_REMEDIATING_CLASS}`, `${u.path}.subject`);
  line.append(drawSubjectName(subject, `${u.path}.subject`));
  line.appendChild(drawQueueLabel(u.words, MERGE_QUEUE_REASON_CLASS, u.title));
  line.appendChild(drawClock(sc, msOf(u.sinceMs, `${u.path}.since_ms`)));
  return line;
}

/** The class the deploy check's label beside the repository's name wears. */
export const MERGE_QUEUE_MEDIATION_CLASS = "sb-mq-mediation";

/**
 * The deploy check the queue waits on, as a purple label beside the
 * repository's name in its group header (owner rulings, 2026-11-14): `hot
 * reloading` while the landing's build rolls out, `assessing reload` while
 * the chain's intake checks it, `fixing reload` while a remediation
 * workspace fixes what the intake found. A click on it selects the workspace
 * at work (`frontend.v1.RosterMergeQueueMediationTarget`), showing the hidden
 * intake for the visit as a click on any hidden workspace's link does; a
 * chain's first rollout names none, and its label selects nothing.
 */
export function drawQueueMediationLabel(u: RosterMergeQueueMediation, sc: SidebarContext, path: string): HTMLElement {
  const standing = requireCase(u.standing, `${path}.standing`);
  switch (standing.case) {
    case "waiting":
      return mediationLabel(sc, "hot reloading", "the last landing is rolling out; it is assessed once it has, and the queue waits for it",
        standing.value.target, `${path}.standing.waiting.target`);
    case "checking":
      return mediationLabel(sc, "assessing reload", "the last landing's reload is being assessed; the queue waits for it",
        requireMessage(standing.value.target, `${path}.standing.checking.target`), `${path}.standing.checking.target`);
    case "fixing":
      return mediationLabel(sc, "fixing reload", "a remediation is fixing what the last landing's reload broke; the queue waits for its fix",
        requireMessage(standing.value.target, `${path}.standing.fixing.target`), `${path}.standing.fixing.target`);
    default: {
      const other: { case: string } = standing;
      return unreachableArm(`${path}.standing`, other.case);
    }
  }
}

/**
 * One deploy check label reading WORDS, titled TITLE, whose click selects
 * TARGET's workspace when it names one.
 */
function mediationLabel(sc: SidebarContext, words: string, title: string, target: RosterMergeQueueMediationTarget | undefined, path: string): HTMLElement {
  const label = drawQueueLabel(words, MERGE_QUEUE_MEDIATION_CLASS, title);
  if (target === undefined) return label;
  const workspace = requireMessage(target.workspace, `${path}.workspace`);
  label.setAttribute("data-select", "");
  label.setAttribute("data-workspace", workspace.id);
  label.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    log.info("selecting the workspace at work on a deploy check from the sidebar", {
      operation: "sidebar.merge-queue.mediation-select",
      context: { workspace: workspace.id, label: words },
    });
    void guardMalformed(
      sc.ctx,
      "sidebar.merge-queue.mediation-select",
      runSelectVerb(label, {
        sc,
        rpc: "SelectWorkspace",
        call: (client) => client.selectWorkspace(create(SelectWorkspaceRequestSchema, { workspace })),
        schema: SelectWorkspaceResponseSchema,
      }),
    );
  });
  return label;
}

/** The class every queue label wears: the repair a merge out of line makes or awaits, a GitHub place. */
export const MERGE_QUEUE_LABEL_CLASS = "sb-mq-label";

/**
 * One queue label after a merge's name (owner request, 2026-11-10): `fixing
 * tests`, `resolving conflicts`, a wait for either, and a GitHub place are
 * peers, drawn alike in the queue's purple. KIND is the label's own class
 * beside MERGE_QUEUE_LABEL_CLASS; TITLE is its hover.
 */
function drawQueueLabel(text: string, kind: string, title: string): HTMLElement {
  const label = document.createElement("span");
  label.className = `${MERGE_QUEUE_LABEL_CLASS} ${kind}`;
  label.textContent = text;
  label.title = title;
  return label;
}

/**
 * One merge waiting behind the head, indented under the head's name, with no
 * bullet (owner request, 2026-11-06): the credited workspace's name and how
 * long it has waited. IT OPENS WITH ITS POSITION AMONG THE WAITING MERGES
 * (owner request, 2026-11-11): POSITION is 1-based, counts only the merges in
 * `RosterRepoMergeQueue.waiting` (never the head or a remediating merge), and
 * is the line's order, not the workspace's number, so a reorder renumbers. No
 * "front" is drawn (owner ruling, 2026-11-11).
 */
export function drawRosterMergeQueueWaiting(
  u: RosterMergeQueueWaiting,
  position: number,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const line = drawSubjectControl(subject, sc, MERGE_QUEUE_WAITING_CLASS, `${path}.subject`);
  const numeral = document.createElement("span");
  numeral.className = MERGE_QUEUE_POSITION_CLASS;
  numeral.textContent = `${position}`;
  line.append(numeral, drawSubjectName(subject, `${path}.subject`));
  line.appendChild(drawClock(sc, msOf(u.queuedAtMs, `${path}.queued_at_ms`)));
  return line;
}

/** The class of the head card's name line. */
export const MERGE_QUEUE_CARD_LINE_CLASS = "sb-mq-head-line";

/**
 * The head card's name line: the merge GLYPH, then the name U is credited
 * under. The card appends what follows the name on its own line.
 */
export function drawCardLine(u: RosterMergeQueueSubject, glyph: HTMLElement, path: string): HTMLElement {
  const line = document.createElement("div");
  line.className = MERGE_QUEUE_CARD_LINE_CLASS;
  line.append(glyph, drawSubjectName(u, path));
  return line;
}

/**
 * One workspace whose owned pull request stands in a GitHub merge queue,
 * drawn as a waiting merge is, after the merges agent-repl runs: its name,
 * its GitHub place ("#3 of 5"), and how long its pull request has stood in
 * the queue. GitHub, not this queue, orders it, so it is no grip.
 */
export function drawRosterMergeQueueGithub(u: RosterMergeQueueGithubEntry, sc: SidebarContext, path: string): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const place = requireMessage(u.place, `${path}.place`);
  const line = drawSubjectControl(subject, sc, MERGE_QUEUE_GITHUB_CLASS, `${path}.subject`);
  const mark = drawQueueLabel(githubPlaceText(place), "sb-mq-github-place", "its pull request's place in the GitHub merge queue");
  line.append(drawSubjectName(subject, `${path}.subject`), mark, drawClock(sc, msOf(u.queuedAtMs, `${path}.queued_at_ms`)));
  return line;
}

/** The class of a merge off its queue's line. Each is one line of the band fit. */
export const MERGE_QUEUE_OFF_QUEUE_CLASS = "sb-mq-off-queue";

/** The class of an off-queue merge's requeue and discard controls. */
export const MERGE_QUEUE_OFF_QUEUE_CONTROL_CLASS = "sb-mq-off-queue-control";

/**
 * One merge that left its queue without landing and whose request still
 * stands, drawn after everything in the queue: its name, why it left, and two
 * controls, one putting it back in line as itself at the back
 * (`RequeueMerge`), the other ending its request (`DiscardMerge`, which
 * closes an async merge's merge workspace). A click on the line itself
 * selects as a waiting merge's does; it is no grip.
 */
export function drawRosterMergeQueueOffQueue(u: RosterMergeQueueOffQueue, sc: SidebarContext, path: string): HTMLElement {
  const subject = requireMessage(u.subject, `${path}.subject`);
  const cause = requireMessage(u.cause, `${path}.cause`);
  const requeue = requireMessage(requireMessage(u.requeue, `${path}.requeue`).merge, `${path}.requeue.merge`);
  const discard = requireMessage(requireMessage(u.discard, `${path}.discard`).merge, `${path}.discard.merge`);
  const line = drawSubjectControl(subject, sc, MERGE_QUEUE_OFF_QUEUE_CLASS, `${path}.subject`);
  const label = drawQueueLabel(cause.text, "sb-mq-off-queue-cause", "why this merge left the queue");
  line.append(drawSubjectName(subject, `${path}.subject`), label);
  line.appendChild(
    drawOffQueueControl("↺", "requeue", "put this merge back in line, at the back", (control) =>
      fireVerb(control, {
        sc,
        rpc: "RequeueMerge",
        // The webapp acts on the queue for the user alone, and says so.
        call: (client) =>
          client.requeueMerge({
            workspace: requeue,
            placement: { place: { case: "back", value: {} } },
            requester: userRequester(),
          }),
        schema: RequeueMergeResponseSchema,
        refusalText: requeueMergeRefusal,
      }),
    ),
  );
  line.appendChild(
    drawOffQueueControl("✕", "discard", "discard this merge's request", (control) =>
      fireVerb(control, {
        sc,
        rpc: "DiscardMerge",
        call: (client) => client.discardMerge({ workspace: discard, requester: userRequester() }),
        schema: DiscardMergeResponseSchema,
        refusalText: discardMergeRefusal,
      }),
    ),
  );
  return line;
}

/** One requeue or discard control: GLYPH, hovering TITLE, whose click runs ACT and never selects the line. */
function drawOffQueueControl(glyph: string, verb: string, title: string, act: (control: HTMLElement) => Promise<boolean>): HTMLElement {
  const control = labelledControl({
    className: MERGE_QUEUE_OFF_QUEUE_CONTROL_CLASS,
    hook: ["data-verb", verb],
    label: glyph,
    onClick: (control) => {
      log.info(`${verb === "requeue" ? "requeueing" : "discarding"} a merge off its queue from the sidebar`, {
        operation: `sidebar.merge-queue.${verb}`,
      });
      void act(control);
    },
  });
  control.title = title;
  // THE LINE'S OWN CLICK SELECTS ITS WORKSPACE (drawSubjectControl); a
  // control's click is the control's alone.
  control.addEventListener("click", (event) => event.stopPropagation());
  return control;
}

/** A GitHub merge queue place as drawn: "#3 of 5". */
export function githubPlaceText(u: RosterMergeQueueGithubPlace): string {
  return `#${u.place} of ${u.total}`;
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
 * exactly as a row's click does, and, when U names one, opens its merge's
 * bubble in that workspace's feed (`SelectInEditorRequest.reveal`). It is selectable as a row's line is (`data-select`), never a
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
    // THE SAME SELECT AS A ROW'S CLICK (`selectWorkspace`), so the editor
    // makes the switch: the daemon's own select left the frame to follow the
    // roster, which waits on an active minibuffer, and the rail drew the
    // workspace selected while Emacs showed another (2026-11-11).
    void guardMalformed(sc.ctx, "sidebar.merge-queue.select", selectWorkspace(control, sc, workspace, u.reveal));
  });
  return control;
}
