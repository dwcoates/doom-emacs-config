/**
 * parts — the small drawing pieces the strip and its activity cell share.
 *
 * Their own module because the strip (`strip.ts`) draws the activity cell
 * (`activity.ts`) and both need these: kept in either, the other would import
 * back into it.
 */
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

/** The baseline strip's grabber notch, which lives inside the grow cell. */
export function grabber(): HTMLElement {
  const notch = document.createElement("div");
  notch.className = "pfooter-grab";
  notch.setAttribute("aria-hidden", "true");
  return notch;
}
