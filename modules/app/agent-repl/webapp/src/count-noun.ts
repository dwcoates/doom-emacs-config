/**
 * count-noun — THE ONE way the webapp says a count of something: "1 commit",
 * "3 commits", "0 commits".
 *
 * WHY IT IS SHARED. Every count the client words takes the same English rule
 * (singular for exactly one, a plain "s" otherwise), and a site spelling the
 * ternary itself is a site free to get it wrong ("1 commits", "0 commit").
 * `test/count-noun.test.ts` fails on any `src` file that hand-rolls it.
 */

/** NOUN as a count of N takes it: singular for exactly one, else with an "s". */
export function pluralNoun(n: number, noun: string): string {
  return n === 1 ? noun : `${noun}s`;
}

/** N followed by its NOUN: `1 commit`, `3 commits`, `0 commits`. */
export function countNoun(n: number, noun: string): string {
  return `${n} ${pluralNoun(n, noun)}`;
}
