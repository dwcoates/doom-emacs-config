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
 * (`.tool-card`, `.tool-head`, `.tool-name`, `.badge`, `.cmd`, `.file-path`,
 * `.tool-output` and the capped-section classes), so an ordinary bash call
 * renders exactly as it did before the port. The input line's TREATMENT is the
 * one thing that used to be picked per tool: the wire now NAMES the drawn form
 * (command, path, query) and the client applies the treatment that form has
 * always had — the shell line, the muted file path, the query line — without
 * ever learning which tool ran. An unset form draws as plain text.
 *
 * THE CLOCKS ARE THE CLIENT'S ONLY ARITHMETIC. `running.last_progress` ships an
 * instant and the card ticks "quiet for N" off the shared ticker; the settled
 * `runtime` ships a composed sentence and is drawn verbatim. Neither is ever
 * computed the other way round: there is no client threshold on quietness and
 * no ticking clock on a settled call.
 */
import { createControl } from "../../control.js";
import {
  type FeedCodeSpan,
  type FeedDiffLine,
  type FeedImageBlock,
  type FeedShellExit,
  type FeedSimpleToolCall,
  type FeedToolCallCodeOutput,
  type FeedToolCallDenied,
  type FeedToolCallDiagnostics,
  type FeedToolCallDiffOutput,
  type FeedToolCallInput,
  type FeedToolCallInputCommand,
  type FeedToolCallInputLink,
  type FeedToolCallInputPath,
  type FeedToolCallInputQuery,
  type FeedToolCallLastProgress,
  type FeedToolCallLink,
  type FeedToolCallLinkUrl,
  type FeedToolCallLinesOutput,
  type FeedToolCallLinksOutput,
  type FeedToolCallName,
  type FeedToolCallNoOutput,
  type FeedToolCallOmitted,
  type FeedToolCallReturned,
  type FeedToolCallRunning,
  type FeedToolCallRuntime,
  type FeedToolCallTextOutput,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { formatTickedAge } from "../../duration.js";
import { drawFeedImageBlock } from "../rows/blocks.js";
import { drawFeedShellExit } from "./shell.js";
import { renderExternalLink } from "../../link.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { paintSpanClass } from "./paint.js";
import { stopTicking, tick, TICKING_ATTRIBUTE } from "../ticking.js";
import { foldTitle } from "../title-fold.js";
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
  log.debug("drawing a tool call card", {
    operation: "feed.cards.tool-call",
    context: { outcome: outcome.case },
  });

  // CARD-LEVEL FOLD (owner ruling, 2026-09-15). The whole card is ONE
  // click-to-expand unit (`.tool-fold`, CAPPED_CLASSES in expand.ts): collapsed
  // it shows only the head (the tool name, in full) and its input line (the
  // title fold, capped at two rows — title-fold.ts); its output section is
  // HIDDEN — no preview — until the card is `.expanded`, at which point the
  // section is revealed (scrolling at 50vh) and the input line's cap is
  // lifted. The stylesheet keys all of that off `.tool-fold`/`.tool-fold.expanded`
  // and `.title-fold`; this end only marks the card and its title.
  const card = document.createElement("div");
  card.className = "tool-card tool-fold";
  card.setAttribute("data-state", outcome.case);

  const head = document.createElement("div");
  head.className = "tool-head";
  head.appendChild(drawFeedToolCallName(requireMessage(u.name, `${path}.name`), `${path}.name`));
  card.appendChild(head);

  const parts = drawOutcome(outcome, rc, path, card);
  head.appendChild(parts.badge);

  const input = drawFeedToolCallInput(requireMessage(u.input, `${path}.input`), rc, `${path}.input`);
  card.appendChild(input);
  for (const element of parts.body) card.appendChild(element);
  // A CARD'S TIMER STOPS THE MOMENT ITS UNIT SETTLES. Every arm but `running`
  // is terminal — the call returned, was denied, or was handed off to a
  // detached shell — and a terminal draw owns no clock, so whatever this
  // element still holds (a live clock a previous draw left on a REUSED
  // element) is dropped here rather than left to a replace site upstream.
  if (outcome.case !== "running") stopTicking(card);
  // THE INPUT LINE IS THE CARD'S TITLE (owner ruling, 2026-09-23): the one
  // two-line title fold, owned by this card's fold. Folded AFTER the terminal
  // stop above, which would otherwise tear down the fold's measurer.
  foldTitle(input, "card");
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
  log.debug("drawing a tool call name", {
    operation: "feed.cards.tool-call.name",
    context: { path },
  });
  const name = document.createElement("span");
  name.className = "tool-name";
  name.textContent = u.text;
  return name;
}

/**
 * What a drawn input form contributes: the element that carries the treatment,
 * and the chrome the CLIENT owns in front of the daemon's text.
 *
 * The prefix exists because the shell form's "$" is stated by the schema to be
 * the client's, not part of the composed line — so it is drawn here rather than
 * expected on the wire, and no other form has any.
 */
export interface InputForm {
  element: HTMLElement;
  prefix: string;
}

/**
 * ② The ONE composed input line, in the FORM the daemon states.
 *
 * THE FORM IS THE DAEMON'S, THE TREATMENT IS THE CLIENT'S. The wire names the
 * shape of the line — a shell command, a file path, a search query — and this
 * module applies the treatment the existing look has always given that shape,
 * while still knowing nothing about which tool ran. An UNSET form is plain
 * text: presence, never a sentinel, and never a shape guessed from the text.
 *
 * A `link` makes the line a hyperlink to the page the call fetched; without one
 * it is plain text. The form's own element carries the treatment either way, so
 * a linked path still reads as a path.
 */
export function drawFeedToolCallInput(
  u: FeedToolCallInput,
  rc: RowContext,
  path: string,
): HTMLElement {
  const form = drawInputForm(u.form, path);
  // THE FORM THE DAEMON STATED, on the element that wears its treatment. An
  // UNSET form carries no attribute at all: absence is the fourth state, and a
  // sentinel value would make plain text look like a form nobody served.
  if (u.form.case !== undefined) {
    form.element.setAttribute("data-input-form", u.form.case);
    form.element.classList.add(`tool-input-${u.form.case}`);
  }
  log.debug("drawing a tool call input line", {
    operation: "feed.cards.tool-call.input",
    context: {
      path,
      form: u.form.case ?? "unset",
      linked: u.link !== undefined,
    },
  });
  if (form.prefix !== "") form.element.appendChild(document.createTextNode(form.prefix));
  if (u.link === undefined) {
    form.element.appendChild(document.createTextNode(u.text));
    return form.element;
  }
  form.element.appendChild(drawFeedToolCallInputLink(u.link, rc, u.text, `${path}.link`));
  return form.element;
}

/**
 * The input-form oneof: one arm, exhaustively — and UNSET is an arm's worth of
 * meaning of its own, so it is handled before the switch rather than defaulted
 * into one of the treatments.
 */
function drawInputForm(form: FeedToolCallInput["form"], path: string): InputForm {
  if (form.case === undefined) return drawPlainInput(`${path}.form`);
  switch (form.case) {
    case "command":
      return drawFeedToolCallInputCommand(form.value, `${path}.command`);
    case "path":
      return drawFeedToolCallInputPath(form.value, `${path}.path`);
    case "query":
      return drawFeedToolCallInputQuery(form.value, `${path}.query`);
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = form;
      return unreachableArm(`${path}.form`, other.case);
    }
  }
}

/**
 * No form stated: the line as plain monospace text.
 *
 * NOT the shell treatment. A line the daemon gave no form for is not a command,
 * and drawing it with the "$" chrome and the accent colour would be this module
 * asserting a shape nobody stated.
 */
function drawPlainInput(path: string): InputForm {
  log.debug("drawing an unformed tool call input line as plain text", {
    operation: "feed.cards.tool-call.input-plain",
    context: { path },
  });
  const line = document.createElement("pre");
  line.className = "tool-input";
  return { element: line, prefix: "" };
}

/** The shell-command form: the accent monospace line, with the client's "$". */
export function drawFeedToolCallInputCommand(
  _u: FeedToolCallInputCommand,
  path: string,
): InputForm {
  log.debug("drawing a tool call input line as a shell command", {
    operation: "feed.cards.tool-call.input-command",
    context: { path },
  });
  const line = document.createElement("pre");
  line.className = "cmd bash-input";
  return { element: line, prefix: "$ " };
}

/** The path form: the existing muted file-path line. */
export function drawFeedToolCallInputPath(_u: FeedToolCallInputPath, path: string): InputForm {
  log.debug("drawing a tool call input line as a file path", {
    operation: "feed.cards.tool-call.input-path",
    context: { path },
  });
  const line = document.createElement("div");
  line.className = "file-path";
  return { element: line, prefix: "" };
}

/**
 * The query form: the accent monospace line WITHOUT the shell chrome.
 *
 * A search pattern is not a command line — it gets the same monospace accent a
 * grep line has always had, and no "$".
 */
export function drawFeedToolCallInputQuery(_u: FeedToolCallInputQuery, path: string): InputForm {
  log.debug("drawing a tool call input line as a query", {
    operation: "feed.cards.tool-call.input-query",
    context: { path },
  });
  const line = document.createElement("pre");
  line.className = "cmd tool-query";
  return { element: line, prefix: "" };
}

/** The input line's hyperlink target. */
export function drawFeedToolCallInputLink(
  u: FeedToolCallInputLink,
  rc: RowContext,
  text: string,
  path: string,
): HTMLElement {
  log.debug("drawing a tool call input link", {
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
    log.debug("drawing a running tool call with no observed beat", {
      operation: "feed.cards.tool-call.running",
      context: { path, beat: false },
    });
    return { badge, body: [] };
  }
  log.debug("drawing a running tool call with a quiet-for clock", {
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
 * IT IS SUBSCRIBED THROUGH `tick`, NOT DIRECTLY. A subscription taken straight
 * off the ticker is invisible to `stopTicking`: it carries no mark, holds no
 * entry, and so no replace site, no dispose and no turn-end backstop can reach
 * it — its only stop was noticing, on some later tick, that the element had
 * left the document, which never happens while the card stays on screen. That
 * is exactly the clock that went on counting under a finished turn. Going
 * through `tick` puts it where every stop can find it.
 *
 * IT STILL UNSUBSCRIBES ITSELF when the element is discarded without a stop:
 * the first tick that finds it detached drops the subscription rather than
 * ticking a node nobody can see. The MARK is what tells a real tick from the
 * first paint `tick` runs before the element is marked or mounted.
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
  tick(quiet, rc.ctx.ticker, (nowMs) => {
    if (quiet.hasAttribute(TICKING_ATTRIBUTE) && !quiet.isConnected) {
      stopTicking(quiet);
      return;
    }
    quiet.textContent = `quiet for ${formatTickedAge(nowMs - since)}`;
  });
  log.debug("drawing a tool call quiet-for clock", {
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
  log.debug("drawing a denied tool call", {
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
  log.debug("drawing a returned tool call", {
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
  // THE EXIT CHIP IS THE DETACHED SHELL'S CHIP, drawn by the detached shell's
  // own function on the same element. The two cards are the same command told
  // twice, so a second chip renderer here could only drift from that one.
  // Absence draws no chip at all — never a zero.
  if (u.exit !== undefined && head !== null) {
    head.appendChild(drawFeedToolCallExit(u.exit, `${path}.exit`));
  }

  // The output FORM is a fact about the card, and the `none` arm draws nothing
  // at all — so the arm is stated on the card rather than inferred from whether
  // an output element happens to be there.
  card.setAttribute("data-output-form", form.case);
  const body = [...drawForm(form, rc, path, verdict.case === "failed")];
  for (const element of body) element.setAttribute("data-output-body", "");
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
    case "image":
      return [drawFeedToolCallImage(form.value, `${path}.image`)];
    case "none":
      return drawFeedToolCallNoOutput(form.value, `${path}.none`);
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = form;
      return unreachableArm(`${path}.form`, other.case);
    }
  }
}

/**
 * The image form: the feed's SHARED image block, drawn by the shared drawing.
 *
 * A tool's image and a prompt body's image are the same message and the same
 * picture-shaped problem, so this arm delegates rather than building a second
 * `<img>`: the daemon resolved the src on the one end that can, and this end
 * loads it. The wrapper carries the output-section class so a tool image sits
 * in the card where every other output form sits.
 */
export function drawFeedToolCallImage(u: FeedImageBlock, path: string): HTMLElement {
  log.debug("drawing an image output", {
    operation: "feed.cards.tool-call.image-output",
    context: { path, has_alt: u.alt !== "" },
  });
  const box = document.createElement("div");
  box.className = "tool-output tool-image-output";
  box.appendChild(drawFeedImageBlock(u));
  return box;
}

/**
 * The exit chip on a returned card — the SAME chip the detached shell's settled
 * head wears, by the same function.
 */
export function drawFeedToolCallExit(u: FeedShellExit, path: string): HTMLElement {
  return drawFeedShellExit(u, path);
}

/**
 * The no-output form: NOTHING, deliberately.
 *
 * The arm says the call returned nothing to draw — a write, an empty search —
 * so the output section is OMITTED WHOLE: no dashed divider, no empty box, no
 * "(no output)" line. An empty text arm would have been the sentinel this arm
 * exists to replace, and drawing an empty block would put the sentinel back in
 * the DOM instead of on the wire. The badge and the runtime still stand; the
 * diagnostics, if the edit raised any, are their own section and unaffected.
 */
export function drawFeedToolCallNoOutput(
  _u: FeedToolCallNoOutput,
  path: string,
): readonly HTMLElement[] {
  log.debug("the returned call has no output to draw", {
    operation: "feed.cards.tool-call.no-output",
    context: { path },
  });
  return [];
}

/** The ok badge. */
export function drawFeedToolCallSucceeded(path: string): HTMLElement {
  log.debug("drawing a succeeded verdict", {
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
  log.debug("drawing a failed verdict", {
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
  log.debug("drawing a tool call runtime", {
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
  log.debug("drawing a text output", {
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
  log.debug("drawing a code output", {
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
  log.debug("drawing a code span", {
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
  log.debug("drawing a diff output", {
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
  log.debug("drawing a diff line", {
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
  log.debug("drawing a lines output", {
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
  log.debug("drawing a links output", {
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
    log.debug("drawing a narration link row", {
      operation: "feed.cards.tool-call.link",
      context: { path, clickable: false },
    });
    row.textContent = u.text;
    return row;
  }
  log.debug("drawing a clickable link row", {
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
  log.debug("drawing a link row's target", {
    operation: "feed.cards.tool-call.link-url",
    context: { path },
  });
  return renderExternalLink(rc.ctx, { text, url: u.url });
}

/** The composed truncation line, verbatim. */
export function drawFeedToolCallOmitted(u: FeedToolCallOmitted, path: string): HTMLElement {
  log.debug("drawing an omitted line", {
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
  log.debug("drawing tool call diagnostics", {
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

  const more = createControl();
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
    log.debug("toggling the diagnostics overflow", {
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
export const TOOL_CALL_FORM_ARMS: readonly string[] = [
  "text",
  "code",
  "diff",
  "lines",
  "links",
  "image",
  "none",
];
export const TOOL_CALL_INPUT_FORM_ARMS: readonly string[] = ["command", "path", "query"];
