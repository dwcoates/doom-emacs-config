/**
 * test/convert/fold-harness.ts — the corpus, and a context to fold against.
 *
 * The goldens drive the REAL fold with the REAL corpus fixtures: every stream
 * record in `testdata/corpus/stream` is an anonymized capture from the actual
 * agent binary, so a converter that only agrees with our own idea of the shapes
 * fails here rather than in production.
 */
import { create } from "@bufbuild/protobuf";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { conversationv1 } from "../../src/proto.js";
import type { FoldContext, LastChange, LiveTask, PendingAsk } from "../../src/convert/fold-context.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import type { PersistEntry } from "../../src/store/persistence.js";

const HERE = dirname(fileURLToPath(import.meta.url));

/** The golden corpus root: real, anonymized captures, never transcribed copies. */
export const CORPUS = join(HERE, "..", "..", "..", "..", "..", "testdata", "corpus");

/** One corpus file's records, in file order. */
export function corpusLines(relativePath: string): Record<string, unknown>[] {
  return readFileSync(join(CORPUS, relativePath), "utf8")
    .split("\n")
    .filter((line) => line.trim() !== "")
    .map((line) => JSON.parse(line) as Record<string, unknown>);
}

/** One corpus file's single record. */
export function corpusLine(relativePath: string): Record<string, unknown> {
  const [first] = corpusLines(relativePath);
  if (first === undefined) throw new Error(`corpus fixture ${relativePath} is empty`);
  return first;
}

/** One corpus stream record, as the SDK would hand it to the fold. */
export function streamMessage(name: string): SdkMessage {
  return corpusLine(join("stream", `${name}.jsonl`)) as unknown as SdkMessage;
}

/** The main agent every golden folds against. */
export const MAIN_AGENT = create(conversationv1.AgentIdSchema, { value: "main-agent" });

/** What a test wants to vary about the engine's knowledge. */
export interface ContextOverrides {
  readonly keepalive?: boolean;
  /** The open turn; `null` folds with no turn open. Defaults to `turn-1`. */
  readonly turnId?: string | null;
  readonly nowMs?: number;
  readonly pendingAsk?: (toolUseId: string) => PendingAsk | undefined;
  /** Calls the shim denied, so their tool_result settles nothing. */
  readonly deniedCall?: (toolUseId: string) => boolean;
  readonly liveTask?: (taskId: string) => LiveTask | undefined;
  readonly subagentFor?: (toolUseId: string) => conversationv1.AgentId | undefined;
  readonly mcpServerNames?: readonly string[];
  readonly lastChange?: LastChange;
  readonly reportFault?: (kind: "converter_defect", detail: string) => void;
  /** The account config-dir the diagnostic api-failure record names. */
  readonly claudeConfigDir?: string;
  /** The model the diagnostic api-failure record names. */
  readonly model?: string;
}

/** A fold context with the engine's knowledge stubbed to nothing by default. */
export function foldContext(overrides: ContextOverrides = {}): FoldContext {
  return {
    mainAgentId: MAIN_AGENT,
    ...(overrides.turnId === null
      ? {}
      : { turnId: create(conversationv1.TurnIdSchema, { value: overrides.turnId ?? "turn-1" }) }),
    keepalive: overrides.keepalive ?? false,
    nowMs: () => overrides.nowMs ?? 1_000,
    pendingAsk: overrides.pendingAsk ?? (() => undefined),
    deniedCall: overrides.deniedCall ?? (() => false),
    liveTask: overrides.liveTask ?? (() => undefined),
    subagentFor: overrides.subagentFor ?? (() => undefined),
    mcpServerNames: () => overrides.mcpServerNames ?? [],
    lastChange: overrides.lastChange,
    ...(overrides.reportFault === undefined ? {} : { reportFault: overrides.reportFault }),
    ...(overrides.claudeConfigDir === undefined ? {} : { claudeConfigDir: overrides.claudeConfigDir }),
    ...(overrides.model === undefined ? {} : { model: overrides.model }),
  };
}

/** The activity a row carries, when it carries one. */
export function activityOf(entry: PersistEntry | undefined): conversationv1.AgentActivity | undefined {
  if (entry?.item.kind !== "frame") return undefined;
  const result = entry.item.frame.result;
  if (result.case !== "update") return undefined;
  const update = result.value.update;
  return update.case === "activity" ? update.value : undefined;
}

/** The unserved item a row carries, when it carries one. */
export function residueOf(entry: PersistEntry | undefined) {
  return entry?.item.kind === "residue" ? entry.item.residue : undefined;
}
