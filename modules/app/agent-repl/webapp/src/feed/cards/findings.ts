/**
 * findings — a review's typed defect list: purple, response-styled.
 *
 * THE WIRE CARRIES ARMS AND TEXT; THE DECORATION IS THIS END'S. No glyph and no
 * color rides the schema, deliberately, so the verdict badges, the category
 * chips and the severity register below are the webapp's treatment of arms the
 * daemon named — which is a different thing from the client DERIVING a verdict.
 * Nothing here reads the summary text, counts rows to label the heading, or
 * infers severity from words: an arm selects a class, and that is the whole of
 * it.
 *
 * THE SERVED ORDER IS THE ORDER, most-severe first. Re-sorting would replace
 * the reviewer's own ranking with a rule this end made up, and the heading the
 * daemon composed ("Findings · 3 · high") already describes the list as served.
 *
 * AN UNSET VERDICT IS AN UNVERIFIED FINDING and draws NO badge — not a
 * "pending" chip, which would state a step in a process the schema does not
 * model. An unset outcome is the ordinary case (a first report, before any
 * fixing happened) and likewise draws nothing.
 *
 * EVERY LOCATION IS A JUMP through the ONE shared editor link, the same
 * component the plan bubble's edit button uses. The two affordances are
 * consistent at the CODE level rather than merely looking alike.
 *
 * THE SCENARIO IS FOLDED, and the fold is per row: a list of ten findings whose
 * scenarios were all open would bury the summaries the reader is scanning.
 */
import type {
  FeedFindings,
  FeedFindingsCategory,
  FeedFindingsHeading,
  FeedFindingsLocation,
  FeedFindingsRow,
  FeedFindingsScenario,
  FeedFindingsSummary,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { renderEditorLink } from "../../link.js";
import { log } from "../../log.js";
import type { AppContext } from "../../rpc/context.js";
import { requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { RowContext } from "../renderers.js";
import { agenticBubble, foldSection } from "./controls.js";

const PATH = "FeedFindings";

/** What a bubble with no findings says. */
export const NOTHING_FOUND_TEXT = "no findings";

/**
 * The verdict badges. CONFIRMED is the error register (the reviewer reproduced
 * it), PLAUSIBLE the warning one (it has not been reproduced, and treating it
 * as confirmed would overstate the review).
 */
const VERDICT_BADGES = {
  confirmed: { className: "badge err", text: "confirmed" },
  plausible: { className: "badge warn", text: "plausible" },
} as const satisfies Record<string, { className: string; text: string }>;

/**
 * The outcome badges. FIXED is the success register; SKIPPED and NO_CHANGE are
 * both grey because neither is a fault — one was a decision, the other a
 * finding that survived a look — and coloring either would editorialize.
 */
const OUTCOME_BADGES = {
  fixed: { className: "badge ok", text: "fixed" },
  skipped: { className: "badge muted", text: "skipped" },
  noChange: { className: "badge muted", text: "no change" },
} as const satisfies Record<string, { className: string; text: string }>;

/** Every verdict arm this build draws, for the suite to hold to the schema. */
export const FINDINGS_VERDICT_ARMS: readonly string[] = Object.keys(VERDICT_BADGES);

/** Every outcome arm this build draws, for the suite to hold to the schema. */
export const FINDINGS_OUTCOME_ARMS: readonly string[] = Object.keys(OUTCOME_BADGES);

/** The findings bubble. */
export function drawFeedFindings(u: FeedFindings, rc: RowContext): HTMLElement {
  log.debug("drawing a findings bubble", {
    operation: "feed.cards.findings",
    context: { rows: u.rows.length },
  });

  const heading = drawFeedFindingsHeading(
    requireMessage(u.heading, `${PATH}.heading`),
    `${PATH}.heading`,
  );
  // The list's own shape is the only "state" this message has, and a reader
  // scanning for the empty case should not have to count rows to find it.
  if (u.rows.length === 0) {
    return agenticBubble({ previous: rc.previous, state: "empty", heading, content: [nothingFound()] });
  }

  const list = document.createElement("div");
  list.className = "findings-rows list-rows";
  u.rows.forEach((row, index) => {
    list.append(drawFeedFindingsRow(row, rc, index, `${PATH}.rows[${index}]`));
  });
  return agenticBubble({ previous: rc.previous, state: "findings", heading, content: [list] });
}

/** The composed heading line, drawn verbatim. */
export function drawFeedFindingsHeading(u: FeedFindingsHeading, path: string): string {
  log.debug("reading a findings heading", {
    operation: "feed.cards.findings.heading",
    context: { path },
  });
  return u.text;
}

/**
 * One finding: verdict, category, location, summary, folded scenario, outcome.
 *
 * INDEX names the fold, not the finding: a fold's state has to survive a redraw
 * and the schema gives a row no identity of its own, so the position within the
 * served list is what the reader's toggle is remembered against. It is used for
 * nothing else — no ordering decision, no label.
 */
export function drawFeedFindingsRow(
  u: FeedFindingsRow,
  rc: RowContext,
  index: number,
  path: string,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "finding";
  el.setAttribute("data-finding", String(index));

  const head = document.createElement("div");
  head.className = "finding-head";
  el.append(head);

  if (u.verdict.case !== undefined) {
    head.append(drawFindingVerdict(u.verdict, el, `${path}.verdict`));
  }
  if (u.category !== undefined) {
    head.append(drawFeedFindingsCategory(u.category, `${path}.category`));
  }
  head.append(
    drawFeedFindingsLocation(
      requireMessage(u.location, `${path}.location`),
      rc.ctx,
      `${path}.location`,
    ),
  );
  if (u.outcome.case !== undefined) {
    head.append(drawFindingOutcome(u.outcome, el, `${path}.outcome`));
  }

  el.append(
    drawFeedFindingsSummary(
      requireMessage(u.summary, `${path}.summary`),
      `${path}.summary`,
    ),
  );
  el.append(
    foldSection({
      name: `finding-scenario-${index}`,
      label: "scenario",
      body: drawFeedFindingsScenario(
        requireMessage(u.scenario, `${path}.scenario`),
        `${path}.scenario`,
      ),
      folded: true,
      rc,
    }),
  );

  log.debug("drew a finding row", {
    operation: "feed.cards.findings.row",
    context: {
      path,
      verdict: u.verdict.case ?? "unset",
      outcome: u.outcome.case ?? "unset",
      category: u.category !== undefined,
    },
  });
  return el;
}

/** The verdict badge, and the severity register it puts on the row. */
export function drawFindingVerdict(
  verdict: FeedFindingsRow["verdict"],
  row: HTMLElement,
  path: string,
): HTMLElement {
  switch (verdict.case) {
    case "confirmed":
    case "plausible": {
      row.setAttribute("data-verdict", verdict.case);
      row.classList.add(`verdict-${verdict.case}`);
      const spec = VERDICT_BADGES[verdict.case];
      const el = document.createElement("span");
      el.className = spec.className;
      el.textContent = spec.text;
      return el;
    }
    default:
      // Reachable only from a NEWER daemon's arm: the callers guard on
      // `case !== undefined`, so the unset verdict never arrives here.
      return unreachableArm(path, verdict.case ?? "unset");
  }
}

/** The outcome badge, drawn only on a re-report after fixes. */
export function drawFindingOutcome(
  outcome: FeedFindingsRow["outcome"],
  row: HTMLElement,
  path: string,
): HTMLElement {
  switch (outcome.case) {
    case "fixed":
    case "skipped":
    case "noChange": {
      row.setAttribute("data-outcome", outcome.case);
      const spec = OUTCOME_BADGES[outcome.case];
      const el = document.createElement("span");
      el.className = spec.className;
      el.textContent = spec.text;
      return el;
    }
    default:
      return unreachableArm(path, outcome.case ?? "unset");
  }
}

/** The category chip, drawn verbatim. */
export function drawFeedFindingsCategory(
  u: FeedFindingsCategory,
  path: string,
): HTMLElement {
  log.debug("drawing a finding category", {
    operation: "feed.cards.findings.category",
    context: { path },
  });
  const el = document.createElement("span");
  el.className = "badge finding-category";
  el.textContent = u.text;
  return el;
}

/**
 * The location line: drawn verbatim AND a jump target.
 *
 * `line` is passed only when the view set it — an unset line means the file's
 * top, and a zero would be a sentinel claiming a line zero exists.
 */
export function drawFeedFindingsLocation(
  u: FeedFindingsLocation,
  ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing a finding location", {
    operation: "feed.cards.findings.location",
    context: { path, line: u.line },
  });
  const el = document.createElement("span");
  el.className = "finding-location";
  el.append(
    renderEditorLink(ctx, {
      text: u.text,
      path: u.path,
      ...(u.line !== undefined ? { line: u.line } : {}),
    }),
  );
  return el;
}

/** The one-sentence summary, drawn verbatim. */
export function drawFeedFindingsSummary(
  u: FeedFindingsSummary,
  path: string,
): HTMLElement {
  log.debug("drawing a finding summary", {
    operation: "feed.cards.findings.summary",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "finding-summary";
  el.textContent = u.text;
  return el;
}

/** The failure scenario, drawn verbatim inside its fold. */
export function drawFeedFindingsScenario(
  u: FeedFindingsScenario,
  path: string,
): HTMLElement {
  log.debug("drawing a finding scenario", {
    operation: "feed.cards.findings.scenario",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "finding-scenario";
  el.textContent = u.text;
  return el;
}

/** The nothing-found treatment: a stated absence, not an empty box. */
function nothingFound(): HTMLElement {
  const el = document.createElement("div");
  el.className = "findings-empty";
  el.setAttribute("data-empty", "");
  el.textContent = NOTHING_FOUND_TEXT;
  return el;
}
