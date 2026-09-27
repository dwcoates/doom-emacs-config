/**
 * The real corpus's own `toolUseResult` objects.
 *
 * READ FROM THE CORPUS RATHER THAN COPIED: `toolUseResult` is exactly what
 * arrives as `ToolOutcome.structured`, so a test that asserts against a
 * hand-transcribed copy asserts against the transcription. If the recorded
 * vendor shape changes, these tests must change with it.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const CORPUS = fileURLToPath(new URL("../../../../../../testdata/corpus/", import.meta.url));

/** The `toolUseResult` of the single record in one tool-results corpus file. */
export function toolUseResult(name: string): Record<string, unknown> {
  const line = readFileSync(`${CORPUS}tool-results/${name}.jsonl`, "utf8").trim().split("\n")[0];
  if (line === undefined) throw new Error(`corpus ${name} is empty`);
  const record = JSON.parse(line) as { toolUseResult?: unknown };
  const result = record.toolUseResult;
  if (typeof result !== "object" || result === null) {
    throw new Error(`corpus ${name} carries no toolUseResult object`);
  }
  return result as Record<string, unknown>;
}

/** The `input` of the single `tool_use` block in one tool-inputs corpus file. */
export function toolInput(name: string): Record<string, unknown> {
  const line = readFileSync(`${CORPUS}tool-inputs/${name}.jsonl`, "utf8").trim().split("\n")[0];
  if (line === undefined) throw new Error(`corpus ${name} is empty`);
  const record = JSON.parse(line) as { input?: unknown };
  const input = record.input;
  if (typeof input !== "object" || input === null) {
    throw new Error(`corpus ${name} carries no tool input object`);
  }
  return input as Record<string, unknown>;
}

/**
 * The text of the single `tool_result` block in one tool-results corpus file,
 * for a result the vendor records with NO `toolUseResult` object — its only
 * statement is the block's own text.
 */
export function toolResultText(name: string): string {
  const line = readFileSync(`${CORPUS}tool-results/${name}.jsonl`, "utf8").trim().split("\n")[0];
  if (line === undefined) throw new Error(`corpus ${name} is empty`);
  const record = JSON.parse(line) as {
    message?: { content?: { type?: string; content?: { type?: string; text?: string }[] }[] };
  };
  const block = record.message?.content?.find((entry) => entry.type === "tool_result");
  const text = block?.content?.find((entry) => entry.type === "text")?.text;
  if (text === undefined) throw new Error(`corpus ${name} carries no tool_result text block`);
  return text;
}
