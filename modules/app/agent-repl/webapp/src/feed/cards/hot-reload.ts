/**
 * hot-reload — A DEPLOY'S HOT RELOAD (frontend.v1.FeedHotReload; owner
 * request, 2026-11-20), drawn in the deploy mediator's own feed.
 *
 * THE MERGE BUBBLE'S FRAME (`drawMergeFrame`), like the fix gate's "testing"
 * card beside it: glyph, label, badge, clock. Its body is one of two views:
 *
 *   - THE LIST: the deploy's shared steps (the protobufs), then one row per
 *     service, each its state's glyph, its name, its words and summary, and
 *     its clock. A service row is a button.
 *   - A SERVICE'S DETAIL, once the reader selects its row: a back control to
 *     the list, the service's own row, what reloading it means, its builds,
 *     its steps, and its running copies (each workspace's shim, each web
 *     view), each a row of the same shape.
 *
 * THE SELECTION IS THE READER'S, and it survives every push: the card is
 * replaced whole, and the new draw reads the selected service off the
 * previous draw's element (`rc.previous`). A service the push no longer lists
 * returns the reader to the list.
 *
 * THE DAEMON RESOLVED EVERY WORD. The client draws each state's glyph and
 * clock from its arm, and composes nothing but "·" between words and
 * summary, and "→" between two builds.
 */
import { armButtonRole } from "../../control.js";
import { labelledControl } from "../../control.js";
import { decorativeElement } from "../../dom.js";
import { liveElapsedClock, settledElapsedClock } from "../../elapsed-clock.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { drawMergeFrame } from "./merge-frame.js";
import type {
  FeedHotReload,
  FeedHotReloadBuilds,
  FeedHotReloadService,
  FeedHotReloadState,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedHotReload";

/** The badge words, by the card's state. */
export const HOT_RELOAD_WORDS = {
  live: "reloading",
  succeeded: "reloaded",
  failed: "failed",
} as const;

/** The attribute the card keeps the selected service's name under. */
export const SELECTED_SERVICE_ATTRIBUTE = "data-selected-service";

/** The glyph and tone each state arm is drawn with. */
const STATE_MARKS = {
  scheduled: { glyph: "○", tone: "is-scheduled" },
  running: { glyph: "●", tone: "is-live is-passing" },
  waiting: { glyph: "●", tone: "is-waiting" },
  done: { glyph: "✓", tone: "is-succeeded" },
  unchanged: { glyph: "✓", tone: "is-unchanged" },
  failed: { glyph: "✗", tone: "is-failed" },
  skipped: { glyph: "–", tone: "is-skipped" },
} as const satisfies Record<string, { glyph: string; tone: string }>;

/** A deploy's hot reload: the merge-styled head over the list or a service's detail. */
export function drawFeedHotReload(msg: FeedHotReload, rc: RowContext): HTMLElement {
  const head = requireMessage(msg.head, `${PATH}.head`);
  const card = drawMergeFrame(
    { kind: "hot-reload", label: requireMessage(head.label, `${PATH}.head.label`).text, props: [], words: HOT_RELOAD_WORDS },
    msg.state,
    rc,
    `${PATH}.state`,
  );
  const body = document.createElement("div");
  body.className = "merge-run hot-reload-body";
  card.append(body);
  show(card, body, msg, rc, rc.previous?.getAttribute(SELECTED_SERVICE_ATTRIBUTE) ?? null);
  return card;
}

/** The name a service row is drawn and selected by. */
function serviceName(svc: FeedHotReloadService, path: string): string {
  return requireMessage(svc.name, `${path}.name`).text;
}

/**
 * Draw BODY as the detail of service NAME, or as the list when NAME is null
 * or names no service of MSG, and record the selection on CARD.
 */
function show(card: HTMLElement, body: HTMLElement, msg: FeedHotReload, rc: RowContext, name: string | null): void {
  const at = name === null ? -1 : msg.services.findIndex((svc, i) => serviceName(svc, `${PATH}.services[${i}]`) === name);
  if (name !== null && at < 0) {
    log.debug("the selected hot reload service is no longer listed; showing the list", {
      operation: "feed.hot_reload.select",
      context: { service: name },
    });
  }
  const select = (next: string | null): void => show(card, body, msg, rc, next);
  if (at < 0) {
    card.removeAttribute(SELECTED_SERVICE_ATTRIBUTE);
    body.replaceChildren(drawList(msg, rc, select));
    return;
  }
  card.setAttribute(SELECTED_SERVICE_ATTRIBUTE, serviceName(msg.services[at]!, `${PATH}.services[${at}]`));
  body.replaceChildren(drawDetail(msg.services[at]!, rc, `${PATH}.services[${at}]`, select));
}

/** The list view: the shared steps, then one selectable row per service. */
function drawList(msg: FeedHotReload, rc: RowContext, select: (name: string | null) => void): HTMLElement {
  const el = document.createElement("div");
  el.className = "hot-reload-list list-rows";
  msg.steps.forEach((step, i) => {
    const path = `${PATH}.steps[${i}]`;
    const row = drawRow(requireMessage(step.name, `${path}.name`).text, step.state, null, rc, `${path}.state`);
    row.classList.add("hot-reload-shared-step");
    el.append(row);
  });
  msg.services.forEach((svc, i) => {
    const path = `${PATH}.services[${i}]`;
    const name = serviceName(svc, path);
    const row = drawRow(name, svc.state, svc.summary?.text ?? null, rc, `${path}.state`);
    row.classList.add("hot-reload-service");
    row.setAttribute("data-service", name);
    armButtonRole(row);
    row.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      select(name);
    });
    el.append(row);
  });
  return el;
}

/**
 * A service's detail: the back control, its own row, what reloading it
 * means, its builds, its steps, then its running copies.
 */
function drawDetail(
  svc: FeedHotReloadService,
  rc: RowContext,
  path: string,
  select: (name: string | null) => void,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "hot-reload-detail";
  const back = labelledControl({
    className: "hot-reload-back",
    hook: ["data-hot-reload-back", ""],
    label: "‹ back",
    onClick: () => select(null),
  });
  back.addEventListener("click", (event) => event.stopPropagation());
  const top = drawRow(serviceName(svc, path), svc.state, svc.summary?.text ?? null, rc, `${path}.state`);
  top.classList.add("hot-reload-detail-head");
  top.prepend(back);
  const about = document.createElement("div");
  about.className = "hot-reload-about";
  about.textContent = requireMessage(svc.about, `${path}.about`).text;
  el.append(top, about);
  if (svc.builds !== undefined) el.append(drawBuilds(svc.builds, `${path}.builds`));
  const steps = document.createElement("div");
  steps.className = "hot-reload-steps list-rows";
  svc.steps.forEach((step, i) => {
    steps.append(drawRow(requireMessage(step.name, `${path}.steps[${i}].name`).text, step.state, null, rc, `${path}.steps[${i}].state`));
  });
  el.append(steps);
  if (svc.instances.length > 0) {
    const instances = document.createElement("div");
    instances.className = "hot-reload-instances list-rows";
    svc.instances.forEach((inst, i) => {
      const at = `${path}.instances[${i}]`;
      instances.append(drawRow(requireMessage(inst.name, `${at}.name`).text, inst.state, null, rc, `${at}.state`));
    });
    el.append(instances);
  }
  return el;
}

/** "build 4f2a1c9 → 9be5151", or "build 9be5151" with no previous build. */
export function buildsText(builds: FeedHotReloadBuilds, path: string): string {
  const fresh = requireMessage(builds.fresh, `${path}.fresh`).shortHash;
  return builds.previous === undefined ? `build ${fresh}` : `build ${builds.previous.shortHash} → ${fresh}`;
}

function drawBuilds(builds: FeedHotReloadBuilds, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "hot-reload-builds";
  el.textContent = buildsText(builds, path);
  return el;
}

/**
 * One row of the card: its state's glyph, NAME, its state's words (and
 * SUMMARY after them), and its clock: ticking while it works or waits, the
 * span that ran once it is done or failed, none otherwise.
 */
export function drawRow(
  name: string,
  state: FeedHotReloadState | undefined,
  summary: string | null,
  rc: RowContext,
  path: string,
): HTMLElement {
  const st = requireCase(requireMessage(state, path).state, path);
  const row = document.createElement("div");
  row.className = "hot-reload-row";
  row.setAttribute("data-state", st.case);
  const words = requireMessage(st.value.words, `${path}.${st.case}.words`).text;
  let clock: HTMLElement | null = null;
  switch (st.case) {
    case "running":
    case "waiting":
      clock = liveElapsedClock(rc.ctx.ticker, "hot-reload-clock", msOf(st.value.startedAtMs, `${path}.${st.case}.started_at_ms`));
      break;
    case "done":
    case "failed": {
      const started = msOf(st.value.startedAtMs, `${path}.${st.case}.started_at_ms`);
      const ended = msOf(st.value.endedAtMs, `${path}.${st.case}.ended_at_ms`);
      if (started > 0) clock = settledElapsedClock("hot-reload-clock", ended - started);
      break;
    }
    case "scheduled":
    case "unchanged":
    case "skipped":
      break;
    default:
      return unreachableArm(path, armName(st));
  }
  const mark = STATE_MARKS[st.case];
  const glyph = decorativeElement("span", `merge-suite-glyph hot-reload-glyph ${mark.tone}`);
  glyph.textContent = mark.glyph;
  const label = document.createElement("span");
  label.className = "hot-reload-name";
  label.textContent = name;
  const said = document.createElement("span");
  said.className = "hot-reload-words";
  said.textContent = summary === null ? words : `${words} · ${summary}`;
  row.append(glyph, label, said);
  if (clock !== null) row.append(clock);
  return row;
}
