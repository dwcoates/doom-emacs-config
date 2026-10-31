/**
 * parts — the small drawing pieces the strip and its activity cell share.
 *
 * Their own module because the strip (`strip.ts`) draws the activity cell
 * (`activity.ts`) and both need these: kept in either, the other would import
 * back into it.
 */
import type { FooterColdGateFigure } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { coldGateFigureColor } from "../pressure-color.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireMessage } from "../rpc/strict.js";
import { protoArmName } from "../vocab.js";

/**
 * A status or substatus word: the arm's name, lowercase ASCII, with spaces.
 *
 * `protoArmName` reverses protobuf-es's lowerCamel back to the proto's own
 * snake_case, and the underscores become spaces — footer.proto's rule, applied
 * in exactly one place for both cells.
 */
export function statusWords(armCase: string): string {
  return protoArmName(armCase).replace(/_/g, " ");
}

/**
 * The status cell's word: the SUBSTATUS'S when the arm sets one, the arm's
 * own otherwise.
 *
 * THE SUBSTATUS IS THE STATUS (owner request, 2026-10-31). The strip has no
 * substatus cell: a step within a status is drawn AS the status, so a working
 * turn that is executing reads "executing" in the status cell's tone, never
 * "working · executing". The step therefore has to read as a status on its
 * own, and the declared exceptions below are exactly the steps whose bare
 * name would lie, say nothing without its status, or misspell a stage:
 *
 * - `FooterStatusClosing`'s `blocked` is also a whole STATUS elsewhere, so it
 *   draws its message's name, "close blocked".
 * - A deploy's first and last stages draw "pre-deploy" and "post-deploy", as
 *   a deploy config names them, and a merge waiting on a workspace "waiting
 *   for" what it waits on.
 * - `FooterStatusMergeFailed`'s areas name where the merge failed, which says
 *   nothing without "failed": "merge conflicts", "tests failed", "hook
 *   failed", and the catch-all `other` "merge failed".
 * - `FooterStatusInterrupted`'s `by_user` reads "interrupted by user".
 * - `FooterStatusLoading`'s items read as what is loading: "loading memory",
 *   "loading skill", "loading skills", "loading skill list".
 * - `FooterStatusDegraded`'s `observation` reads "observation degraded".
 *
 * Every other step reads as its name (statusWords). With no step, A BOOTING
 * SESSION READS "loading" (owner ruling, 2026-10-28): every shim start is
 * drawn as an extension of the loading the footer already shows, so the
 * `starting` arm takes that word, and every other arm reads as its name.
 */
export function statusCellWord(armCase: string, subCase?: string): string {
  if (subCase === undefined) return armCase === "starting" ? "loading" : statusWords(armCase);
  return ELEVATED_STEP_WORDS[`${armCase}/${subCase}`] ?? statusWords(subCase);
}

/** The declared exceptions of statusCellWord, keyed `arm/step`. */
const ELEVATED_STEP_WORDS: Readonly<Record<string, string>> = Object.freeze({
  "closing/blocked": "close blocked",
  "merging/awaitingFree": "waiting for",
  "merging/preDeploy": "pre-deploy",
  "merging/postDeploy": "post-deploy",
  "mergeFailed/conflicts": "merge conflicts",
  "mergeFailed/tests": "tests failed",
  "mergeFailed/hook": "hook failed",
  "mergeFailed/other": "merge failed",
  "interrupted/byUser": "interrupted by user",
  "loading/memory": "loading memory",
  "loading/invoked": "loading skill",
  "loading/discovered": "loading skills",
  "loading/listing": "loading skill list",
  "degraded/observation": "observation degraded",
});

/** A composed line drawn verbatim, in its own classed span. */
export function textLine(className: string, text: string): HTMLElement {
  const span = document.createElement("span");
  span.className = className;
  span.textContent = text;
  return span;
}

/** A composed line's parts around its one cold-gate token figure. */
export interface ColdGateFiguredParts {
  lead: string;
  figure?: FooterColdGateFigure;
  tail: string;
}

/**
 * A composed line around ONE cold-gate token figure, drawn from its parts so
 * the figure takes its color: the cold-gate gradient over its window fill
 * (`coldGateFigureColor`, the same call the docked gate's figure makes). The
 * parts must make exactly TEXT; a line that does not is refused at PATH.
 * Every footer line carrying such a figure draws it here.
 */
export function coldGateFiguredLine(
  className: string,
  text: string,
  parts: ColdGateFiguredParts,
  path: string,
): HTMLElement {
  const figure = requireMessage(parts.figure, `${path}.figure`);
  if (parts.lead + figure.text + parts.tail !== text) {
    throw new MalformedView(path, `its parts ${JSON.stringify([parts.lead, figure.text, parts.tail])} do not make its text ${JSON.stringify(text)}`);
  }
  const line = document.createElement("span");
  line.className = className;
  const number = document.createElement("span");
  number.className = "footer-cold-gate-figure";
  number.textContent = figure.text;
  // INLINE: the figure's color IS its reading (as the context chip's).
  number.style.color = coldGateFigureColor(figure.windowFill, `${path}.figure.window_fill`);
  line.append(document.createTextNode(parts.lead), number, document.createTextNode(parts.tail));
  return line;
}
