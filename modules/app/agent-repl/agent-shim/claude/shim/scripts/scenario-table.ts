/**
 * scripts/scenario-table.ts — render the prompt→scenario table from the registry.
 *
 * The table published in `AGENTS.md` is GENERATED, not maintained: it is the
 * mocked vendor's public contract (which prompt produces which vendor behavior,
 * which files, which conversation.v1 arms), and a hand-written copy of a
 * hundred-row table drifts the day after it is written.
 *
 * `test/fake/registry.test.ts` asserts the committed table matches this
 * renderer's output exactly, so regenerating is the only way to change it.
 *
 * Run:
 * ```
 * npx esbuild scripts/scenario-table.ts --bundle --platform=node --format=esm \
 *   --outfile=/tmp/scenario-table.mjs && node /tmp/scenario-table.mjs
 * ```
 * The suite also prints the expected block on failure, so a mismatch is fixable
 * without leaving the test output.
 */
import { SCENARIOS } from "../src/fake/registry.js";

/** The heading the table lives under in `AGENTS.md`. */
export const TABLE_HEADING = "## Mocked vendor: prompt → scenario table";

const cell = (text: string): string => text.replace(/\|/g, "\\|").replace(/\n/g, " ");

/** The table's markdown, header row included, with no trailing newline. */
export function renderScenarioTable(): string {
  const rows = SCENARIOS.map(
    (s) => `| \`${cell(s.prompt)}\` | ${cell(s.emits)} | ${cell(s.writes)} | ${cell(s.arms)} |`,
  );
  return [
    "| Prompt | What the vendor emits | What it writes on disk | conversation.v1 arms exercised |",
    "| --- | --- | --- | --- |",
    ...rows,
  ].join("\n");
}

if ((process.argv[1] ?? "").includes("scenario-table")) {
  process.stdout.write(`${renderScenarioTable()}\n`);
}
