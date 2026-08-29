/**
 * tool-call — the grey tool card: ONE shell for every tool there is.
 *
 * NO PER-TOOL KNOWLEDGE LIVES HERE, and that is the whole point of the
 * message. The daemon composes the input line's phrasing ("$ go test ./...",
 * "src/render.ts", "grep: FeedRow"), picks which output FORM the result is
 * drawn in, composes the runtime sentence and the omitted floors, and resolves
 * a code block's paint spans. This module draws the head, the one input line,
 * the badge, the output section below the dashed divider, and the diagnostics
 * under it — and nothing it draws depends on which tool ran. A tool-specific
 * affordance would be a new arm in the schema, not a branch in here.
 *
 * THE LOOK IS TODAY'S LOOK. The shell reuses the existing card classes
 * (`.tool-card`, `.tool-head`, `.tool-name`, `.badge`, `.cmd`, `.tool-output`
 * and the capped-section classes), so an ordinary bash call renders exactly as
 * it did before the port. The one unification the contract forces is the input
 * line: the wire carries ONE composed line for every tool, so there is no
 * longer a bash treatment and a file-path treatment to choose between — every
 * card's input is the monospace accent line, capped and click-expandable, that
 * a bash command has always had.
 *
 * THE CLOCKS ARE THE CLIENT'S ONLY ARITHMETIC. `running.last_progress` ships an
 * instant and the card ticks "quiet for N" off the shared ticker; the settled
 * `runtime` ships a composed sentence and is drawn verbatim. Neither is ever
 * computed the other way round: there is no client threshold on quietness and
 * no ticking clock on a settled call.
 */
import {
  type FeedCodeSpan,
  type FeedDiffLine,
  type FeedSimpleToolCall,
  type FeedToolCallCodeOutput,
  type FeedToolCallDenied,
  type FeedToolCallDiagnostics,
  type FeedToolCallDiffOutput,
  type FeedToolCallInput,
  type FeedToolCallInputLink,
  type FeedToolCallLastProgress,
  type FeedToolCallLink,
  type FeedToolCallLinkUrl,
  type FeedToolCallLinesOutput,
  type FeedToolCallLinksOutput,
  type FeedToolCallName,
  type FeedToolCallOmitted,
  type FeedToolCallReturned,
  type FeedToolCallRunning,
  type FeedToolCallRuntime,
  type FeedToolCallTextOutput,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { formatAge } from "../../duration.js";
import { renderExternalLink } from "../../link.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { paintSpanClass } from "./paint.js";
import type { RowContext } from "./context.js";

/**
 * How many diagnostics rows draw before the rest go behind a toggle.
 *
 * The wire says the lines are "client-capped" and hands over all of them, so
 * the cap is a rendering decision: three is enough to see that an edit raised
 * findings and what the first of them are, without a long list pushing the
 * output that caused them off the screen.
 */
export const DIAGNOSTICS_VISIBLE = 3;

/** What an outcome arm contributes: the head's badge, and body elements. */
export interface OutcomeParts {
  /** The head badge — every arm has one; it is what the arm mostly IS. */
  badge: HTMLElement;
  /** Elements appended below the input line, in order. May be empty. */
  body: readonly HTMLElement[];
}

/**
 * The tool card.
 *
 * `data-state` carries the outcome's arm name and `data-verdict`, on a returned
 * call, the verdict's — so a cold repaint of the row is enough to tell what the
 * card is saying, and the integration suite targets the fact rather than the
 * badge's wording.
 */
export function drawFeedSimpleToolCall(u: FeedSimpleToolCall, rc: RowContext): HTMLElement {
  const path = "FeedSimpleToolCall";
  const outcome = requireCase(u.outcome, `${path}.outcome`);
  log("debug", "drawing a tool call card", {
    operation: "feed.cards.tool-call",
    context: { outcome: outcome.case },
  });

  const card = document.createElement("div");
  card.className = "tool-card";
  card.setAttribute("data-state", outcome.case);

  const head = document.createElement("div");
  head.className = "tool-head";
  head.appendChild(drawFeedToolCallName(requireMessage(u.name, `${path}.name`), `${path}.name`));
  card.appendChild(head);

  const parts = drawOutcome(outcome, rc, path, card);
  head.appendChild(parts.badge);

  card.appendChild(drawFeedToolCallInput(requireMessage(u.input, `${path}.input`), rc, `${path}.input`));
  for (const element of parts.body) card.appendChild(element);
  return card;
}

/** The outcome oneof: one arm, exhaustively. */
function drawOutcome(
  outcome: NonNullable<FeedSimpleToolCall["outcome"]> & { case: string },
  rc: RowContext,
  path: string,
  card: HTMLElement,
): OutcomeParts {
  switch (outcome.case) {
    case "running":
      return drawFeedToolCallRunning(outcome.value, rc, `${path}.running`);
    case "returned":
      return drawFeedToolCallReturned(outcome.value, rc, `${path}.returned`, card);
    case "denied":
      return drawFeedToolCallDenied(outcome.value, `${path}.denied`);
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = outcome;
      return unreachableArm(`${path}.outcome`, other.case);
    }
  }
}

/** ① The head's name, verbatim. */
export function drawFeedToolCallName(u: FeedToolCallName, path: string): HTMLElement {
  log("debug", "drawing a tool call name", {
    operation: "feed.cards.tool-call.name",
    context: { path },
  });
  const name = document.createElement("span");
  name.className = "tool-name";
  name.textContent = u.text;
  return name;
}

/**
 * ② The ONE composed input line.
 *
 * A `link` makes the line a hyperlink to the page the call fetched; without one
 * it is plain text. The `<pre>` carries the cap and the click-to-expand either
 * way, so a long command reads the same whether or not it links anywhere.
 */
export function drawFeedToolCallInput(
  u: FeedToolCallInput,
  rc: RowContext,
  path: string,
): HTMLElement {
  const line = document.createElement("pre");
  line.className = "cmd bash-input";
  if (u.link === undefined) {
    log("debug", "drawing a plain tool call input line", {
      operation: "feed.cards.tool-call.input",
      context: { path, linked: false },
    });
    line.textContent = u.text;
    return line;
  }
  log("debug", "drawing a linked tool call input line", {
    operation: "feed.cards.tool-call.input",
    context: { path, linked: true },
  });
  line.appendChild(drawFeedToolCallInputLink(u.link, rc, u.text, `${path}.link`));
  return line;
}

/** The input line's hyperlink target. */
export function drawFeedToolCallInputLink(
  u: FeedToolCallInputLink,
  rc: RowContext,
  text: string,
  path: string,
): HTMLElement {
  log("debug", "drawing a tool call input link", {
    operation: "feed.cards.tool-call.input-link",
    context: { path },
  });
  return renderExternalLink(rc.ctx, { text, url: u.url });
}

/**
 * The running state: the run badge, and "quiet for N" once there has been a
 * beat to count from.
 *
 * NOTHING IS DRAWN BEFORE THE FIRST BEAT. An unset `last_progress` means the
 * daemon has seen no sign of life yet, which is not the same as a call that has
 * been quiet since it started — so the card says nothing rather than starting a
 * clock from an instant nobody reported.
 */
export function drawFeedToolCallRunning(
  u: FeedToolCallRunning,
  rc: RowContext,
  path: string,
): OutcomeParts {
  const badge = document.createElement("span");
  badge.className = "badge run";
  const spinner = document.createElement("span");
  spinner.className = "tool-spinner";
  spinner.setAttribute("aria-hidden", "true");
  badge.appendChild(spinner);
  badge.appendChild(document.createTextNode("running…"));

  if (u.lastProgress === undefined) {
    log("debug", "drawing a running tool call with no observed beat", {
      operation: "feed.cards.tool-call.running",
      context: { path, beat: false },
    });
    return { badge, body: [] };
  }
  log("debug", "drawing a running tool call with a quiet-for clock", {
    operation: "feed.cards.tool-call.running",
    context: { path, beat: true },
  });
  return {
    badge,
    body: [drawFeedToolCallLastProgress(u.lastProgress, rc, `${path}.last_progress`)],
  };
}

/**
 * The last-observed-progress clock, ticking client-side off the shared ticker.
 *
 * IT UNSUBSCRIBES ITSELF. A re-push replaces the card whole, so the element
 * this subscription writes into leaves the document; the first tick that finds
 * it detached drops the subscription rather than ticking a node nobody can see.
 * That is what keeps a long-running turn from accumulating one live clock per
 * push of the same row.
 */
export function drawFeedToolCallLastProgress(
  u: FeedToolCallLastProgress,
  rc: RowContext,
  path: string,
): HTMLElement {
  const since = msOf(u.atMs, `${path}.at_ms`);
  const quiet = document.createElement("div");
  quiet.className = "tool-quiet";
  quiet.setAttribute("data-since-ms", String(since));
  const paint = (nowMs: number): void => {
    quiet.textContent = `quiet for ${formatAge(nowMs - since)}`;
  };
  paint(rc.ctx.ticker.now());
  const stop = rc.ctx.ticker.subscribe((nowMs) => {
    if (!quiet.isConnected) {
      stop();
      return;
    }
    paint(nowMs);
  });
  log("debug", "drawing a tool call quiet-for clock", {
    operation: "feed.cards.tool-call.last-progress",
    context: { path, since_ms: since },
  });
  return quiet;
}

/**
 * The denied state: the badge, and NO output section.
 *
 * The card's whole statement is that the call never ran — the consent story is
 * the permission card's, so there is nothing else to draw here.
 */
export function drawFeedToolCallDenied(_u: FeedToolCallDenied, path: string): OutcomeParts {
  log("debug", "drawing a denied tool call", {
    operation: "feed.cards.tool-call.denied",
    context: { path },
  });
  const badge = document.createElement("span");
  badge.className = "badge perm";
  badge.textContent = "denied";
  return { badge, body: [] };
}

/**
 * ③ The returned state: the verdict badge, the runtime beside it, the output
 * section below the dashed divider, and the IDE's diagnostics under that.
 *
 * The runtime is appended to the HEAD (the card's element is handed in for
 * exactly that reason) because it belongs beside the badge it qualifies, while
 * the output belongs below the divider.
 */
export function drawFeedToolCallReturned(
  u: FeedToolCallReturned,
  rc: RowContext,
  path: string,
  card: HTMLElement,
): OutcomeParts {
  const verdict = requireCase(u.verdict, `${path}.verdict`);
  // The arm's NAME, read before the exhaustive match narrows the value to
  // `never` and takes `.case` with it. The refusal still has to say which arm
  // it could not draw.
  const verdictArm: string = verdict.case;
  const form = requireCase(u.form, `${path}.form`);
  log("debug", "drawing a returned tool call", {
    operation: "feed.cards.tool-call.returned",
    context: { verdict: verdict.case, form: form.case },
  });
  card.setAttribute("data-verdict", verdict.case);

  const badge =
    verdict.case === "succeeded"
      ? drawFeedToolCallSucceeded(`${path}.succeeded`)
      : verdict.case === "failed"
        ? drawFeedToolCallFailed(`${path}.failed`)
        : unreachableArm(`${path}.verdict`, verdictArm);

  const head = card.querySelector(".tool-head");
  if (u.runtime !== undefined && head !== null) {
    head.appendChild(drawFeedToolCallRuntime(u.runtime, `${path}.runtime`));
  }

  const body = [...drawForm(form, rc, path, verdict.case === "failed")];
  if (u.diagnostics !== undefined) {
    body.push(drawFeedToolCallDiagnostics(u.diagnostics, `${path}.diagnostics`));
  }
  return { badge, body };
}

/** The output-form oneof: one arm, exhaustively. */
function drawForm(
  form: NonNullable<FeedToolCallReturned["form"]> & { case: string },
  rc: RowContext,
  path: string,
  failed: boolean,
): readonly HTMLElement[] {
  switch (form.case) {
    case "text":
      return [drawFeedToolCallTextOutput(form.value, failed, `${path}.text`)];
    case "code":
      return drawFeedToolCallCodeOutput(form.value, `${path}.code`);
    case "diff":
      return [drawFeedToolCallDiffOutput(form.value, `${path}.diff`)];
    case "lines":
      return drawFeedToolCallLinesOutput(form.value, `${path}.lines`);
    case "links":
      return drawFeedToolCallLinksOutput(form.value, rc, `${path}.links`);
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = form;
      return unreachableArm(`${path}.form`, other.case);
    }
  }
}

/** The ok badge. */
export function drawFeedToolCallSucceeded(path: string): HTMLElement {
  log("debug", "drawing a succeeded verdict", {
    operation: "feed.cards.tool-call.succeeded",
    context: { path },
  });
  const badge = document.createElement("span");
  badge.className = "badge ok";
  badge.textContent = "done";
  return badge;
}

/** The err badge. The error TEXT is the output section's, in its own form. */
export function drawFeedToolCallFailed(path: string): HTMLElement {
  log("debug", "drawing a failed verdict", {
    operation: "feed.cards.tool-call.failed",
    context: { path },
  });
  const badge = document.createElement("span");
  badge.className = "badge err";
  badge.textContent = "error";
  return badge;
}

/** The settled clock's composed sentence ("ran 4.2 s"), verbatim. */
export function drawFeedToolCallRuntime(u: FeedToolCallRuntime, path: string): HTMLElement {
  log("debug", "drawing a tool call runtime", {
    operation: "feed.cards.tool-call.runtime",
    context: { path },
  });
  const runtime = document.createElement("span");
  runtime.className = "tool-runtime";
  runtime.textContent = u.text;
  return runtime;
}

/** The plain-text form: verbatim, height-capped, red when the call failed. */
export function drawFeedToolCallTextOutput(
  u: FeedToolCallTextOutput,
  failed: boolean,
  path: string,
): HTMLElement {
  log("debug", "drawing a text output", {
    operation: "feed.cards.tool-call.text-output",
    context: { path, failed },
  });
  const pre = document.createElement("pre");
  pre.className = failed ? "tool-output bash-output stderr" : "tool-output bash-output";
  pre.textContent = u.text;
  return pre;
}

/**
 * The highlighted-code form: the daemon's paint spans, painted.
 *
 * The omitted line is a SIBLING rather than a child of the capped box, so the
 * count stays visible when the box is scrolled and the dashed divider is drawn
 * once, by the output element itself.
 */
export function drawFeedToolCallCodeOutput(
  u: FeedToolCallCodeOutput,
  path: string,
): readonly HTMLElement[] {
  log("debug", "drawing a code output", {
    operation: "feed.cards.tool-call.code-output",
    context: { path, spans: u.spans.length, omitted: u.omitted !== undefined },
  });
  const pre = document.createElement("pre");
  pre.className = "tool-output tool-read-output";
  const code = document.createElement("code");
  code.className = "hljs";
  for (const [index, span] of u.spans.entries()) {
    code.appendChild(drawFeedCodeSpan(span, `${path}.spans[${index}]`));
  }
  pre.appendChild(code);
  if (u.omitted === undefined) return [pre];
  return [pre, drawFeedToolCallOmitted(u.omitted, `${path}.omitted`)];
}

/**
 * One paint span: the text verbatim, the class from the shared vocabulary.
 *
 * Concatenating the spans' text is the code, so a span is never dropped —
 * including one whose class this build does not know, which draws unstyled.
 */
export function drawFeedCodeSpan(u: FeedCodeSpan, path: string): HTMLElement {
  const span = document.createElement("span");
  const painted = paintSpanClass(u.paintClass);
  if (painted !== "") span.className = painted;
  span.textContent = u.text;
  log("debug", "drawing a code span", {
    operation: "feed.cards.tool-call.code-span",
    context: { path, paint_class: u.paintClass, painted },
  });
  return span;
}

/** The diff form: the arm-typed lines, in order. */
export function drawFeedToolCallDiffOutput(
  u: FeedToolCallDiffOutput,
  path: string,
): HTMLElement {
  log("debug", "drawing a diff output", {
    operation: "feed.cards.tool-call.diff-output",
    context: { path, lines: u.lines.length },
  });
  const pre = document.createElement("pre");
  pre.className = "tool-output diff-output diff";
  for (const [index, line] of u.lines.entries()) {
    if (index > 0) pre.appendChild(document.createTextNode("\n"));
    pre.appendChild(drawFeedDiffLine(line, `${path}.lines[${index}]`));
  }
  return pre;
}

/**
 * One diff line: the arm is the color, and the GUTTER GLYPH IS THE CLIENT'S.
 *
 * The wire carries the text without any +/- prefix precisely because the arm
 * already says which kind of line it is; re-deriving the kind from a prefix
 * would be the client parsing what it was told.
 */
export function drawFeedDiffLine(u: FeedDiffLine, path: string): HTMLElement {
  const kind = requireCase(u.kind, `${path}.kind`);
  // As above: the name, before the match narrows the value away.
  const kindArm: string = kind.case;
  const gutter =
    kind.case === "added"
      ? "+"
      : kind.case === "removed"
        ? "-"
        : kind.case === "header" || kind.case === "context"
          ? " "
          : unreachableArm(`${path}.kind`, kindArm);
  const cls =
    kind.case === "added"
      ? "add"
      : kind.case === "removed"
        ? "del"
        : kind.case === "header"
          ? "hunk"
          : "ctx";
  log("debug", "drawing a diff line", {
    operation: "feed.cards.tool-call.diff-line",
    context: { path, kind: kind.case },
  });
  const line = document.createElement("span");
  line.className = cls;
  line.setAttribute("data-diff-line", kind.case);
  line.textContent = `${gutter}${u.text}`;
  return line;
}

/** The line-list form: the lines verbatim, plus the composed floor. */
export function drawFeedToolCallLinesOutput(
  u: FeedToolCallLinesOutput,
  path: string,
): readonly HTMLElement[] {
  log("debug", "drawing a lines output", {
    operation: "feed.cards.tool-call.lines-output",
    context: { path, lines: u.lines.length, omitted: u.omitted !== undefined },
  });
  const pre = document.createElement("pre");
  pre.className = "tool-output bash-output";
  pre.textContent = u.lines.join("\n");
  if (u.omitted === undefined) return [pre];
  return [pre, drawFeedToolCallOmitted(u.omitted, `${path}.omitted`)];
}

/** The link-list form: one row per result, plus the composed floor. */
export function drawFeedToolCallLinksOutput(
  u: FeedToolCallLinksOutput,
  rc: RowContext,
  path: string,
): readonly HTMLElement[] {
  log("debug", "drawing a links output", {
    operation: "feed.cards.tool-call.links-output",
    context: { path, links: u.links.length, omitted: u.omitted !== undefined },
  });
  const list = document.createElement("div");
  list.className = "tool-output tool-links list-rows";
  for (const [index, link] of u.links.entries()) {
    list.appendChild(drawFeedToolCallLink(link, rc, `${path}.links[${index}]`));
  }
  if (u.omitted === undefined) return [list];
  return [list, drawFeedToolCallOmitted(u.omitted, `${path}.omitted`)];
}

/**
 * One link row: clickable when the row has a url, narration when it does not.
 *
 * A narration row is the engine talking about the search rather than a result
 * to open, so it is drawn as text — never as a link that would go nowhere.
 */
export function drawFeedToolCallLink(
  u: FeedToolCallLink,
  rc: RowContext,
  path: string,
): HTMLElement {
  const row = document.createElement("div");
  row.className = "tool-link-row";
  if (u.url === undefined) {
    log("debug", "drawing a narration link row", {
      operation: "feed.cards.tool-call.link",
      context: { path, clickable: false },
    });
    row.textContent = u.text;
    return row;
  }
  log("debug", "drawing a clickable link row", {
    operation: "feed.cards.tool-call.link",
    context: { path, clickable: true },
  });
  row.appendChild(drawFeedToolCallLinkUrl(u.url, rc, u.text, `${path}.url`));
  return row;
}

/** A link row's hyperlink target. */
export function drawFeedToolCallLinkUrl(
  u: FeedToolCallLinkUrl,
  rc: RowContext,
  text: string,
  path: string,
): HTMLElement {
  log("debug", "drawing a link row's target", {
    operation: "feed.cards.tool-call.link-url",
    context: { path },
  });
  return renderExternalLink(rc.ctx, { text, url: u.url });
}

/** The composed truncation line, verbatim. */
export function drawFeedToolCallOmitted(u: FeedToolCallOmitted, path: string): HTMLElement {
  log("debug", "drawing an omitted line", {
    operation: "feed.cards.tool-call.omitted",
    context: { path },
  });
  const omitted = document.createElement("div");
  omitted.className = "tool-omitted";
  omitted.textContent = u.text;
  return omitted;
}

/**
 * The IDE's diagnostics against this change, below the output.
 *
 * CAPPED WITH A TOGGLE, NOT TRUNCATED: the lines the daemon sent are all in the
 * DOM, and the toggle reveals the rest in place rather than asking for them
 * again (there is nothing to ask — the row already carries them).
 */
export function drawFeedToolCallDiagnostics(
  u: FeedToolCallDiagnostics,
  path: string,
): HTMLElement {
  const hidden = Math.max(0, u.lines.length - DIAGNOSTICS_VISIBLE);
  log("debug", "drawing tool call diagnostics", {
    operation: "feed.cards.tool-call.diagnostics",
    context: { path, lines: u.lines.length, hidden },
  });
  const box = document.createElement("div");
  box.className = "tool-diagnostics";
  const list = document.createElement("div");
  list.className = "tool-diagnostic-rows list-rows";
  for (const [index, text] of u.lines.entries()) {
    const row = document.createElement("div");
    row.className = "tool-diagnostic";
    row.textContent = text;
    if (index >= DIAGNOSTICS_VISIBLE) row.hidden = true;
    list.appendChild(row);
  }
  box.appendChild(list);
  if (hidden === 0) return box;

  const more = document.createElement("button");
  more.type = "button";
  more.className = "tool-diagnostics-more";
  more.setAttribute("data-hidden-count", String(hidden));
  let open = false;
  const label = (): void => {
    more.textContent = open ? "− fewer" : `+${hidden} more`;
  };
  label();
  more.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
    open = !open;
    const rows = [...list.querySelectorAll<HTMLElement>(".tool-diagnostic")];
    rows.forEach((row, index) => {
      row.hidden = !open && index >= DIAGNOSTICS_VISIBLE;
    });
    label();
    log("debug", "toggling the diagnostics overflow", {
      operation: "feed.cards.tool-call.diagnostics-toggle",
      context: { path, open },
    });
  });
  box.appendChild(more);
  return box;
}

/**
 * The arms this module knows, for the suite that holds them to the schema.
 *
 * Exported as data rather than restated in the test file so a new arm in the
 * proto fails HERE, in one place, instead of passing a test that enumerated the
 * old set by hand.
 */
export const TOOL_CALL_OUTCOME_ARMS: readonly string[] = ["running", "returned", "denied"];
export const TOOL_CALL_VERDICT_ARMS: readonly string[] = ["succeeded", "failed"];
export const TOOL_CALL_FORM_ARMS: readonly string[] = ["text", "code", "diff", "lines", "links"];
