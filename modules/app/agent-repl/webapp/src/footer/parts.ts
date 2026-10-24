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
