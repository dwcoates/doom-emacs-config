/**
 * A merge head's branch line as a fixture spells it: "source → target" is a
 * source and a target, and a label with no arrow is a source alone.
 */
import { BRANCH_LINE_ARROW } from "../../src/feed/merge/merge.js";

export function branchLineInit(label: string): { source: { text: string }; target?: { text: string } } {
  const at = label.indexOf(BRANCH_LINE_ARROW);
  if (at < 0) return { source: { text: label } };
  return { source: { text: label.slice(0, at) }, target: { text: label.slice(at + BRANCH_LINE_ARROW.length) } };
}
