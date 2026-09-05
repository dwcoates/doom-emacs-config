/**
 * test/expect-shapes.ts — vitest's asymmetric matchers, typed.
 *
 * `expect.objectContaining` and `expect.stringMatching` are declared to return
 * `any`, because they stand in for whatever value the expectation compares
 * against. Dropped into an expected object literal that `any` spreads: every
 * sibling field of one of them is then read off an `any`, so a renamed field
 * or a wrong arm passes the linter — and the compiler — in silence.
 *
 * These wrappers hand back `unknown` instead. A matcher still compares exactly
 * as it did (nothing about the runtime value changes), but it no longer poisons
 * the literal it sits in.
 */
import { expect } from "vitest";

/** `expect.objectContaining`, without the `any`. */
export function containing(shape: object): unknown {
  return expect.objectContaining(shape);
}

/** `expect.stringContaining`, without the `any`. */
export function textContaining(part: string): unknown {
  return expect.stringContaining(part);
}

/** `expect.stringMatching`, without the `any`. */
export function matching(pattern: RegExp | string): unknown {
  return expect.stringMatching(pattern);
}
