/**
 * test/convert/goldens/harness.ts — the REAL captures, driven through the REAL fold.
 *
 * # What a golden is here
 *
 * `testdata/captures/<scenario>/` is a verbatim recording of one supervised run
 * against the actual agent binary: `stream.jsonl` carries every record the SDK
 * handed the shim (`dir: "sdk"`) interleaved with what the shim asked of it
 * (`dir: "control"`), and `files/projects/**` carries the transcript and
 * subagent files the vendor wrote for the same run. Nothing here is
 * transcribed, so a converter that only agrees with our own idea of the vendor's
 * shapes fails in this directory rather than in production.
 *
 * # Why the context is DERIVED from the capture
 *
 * The fold's joins are the engine's knowledge, and a golden that stubbed them to
 * nothing would assert a fold running blind. The two the captures can honestly
 * supply are recovered here and nowhere else:
 *
 *   - `pendingAsk` — the capture's `can_use_tool_request` control lines name the
 *     tool and its input but NOT the call, so the join back to a `tool_use_id`
 *     is by (tool name, input) against the calls the same stream announced. That
 *     is the only link the recording preserves.
 *   - `subagentFor` — the vendor keys a subagent file by its own 17-hex agent id
 *     and states the spawning call in the file's `.meta.json` `toolUseId`, so
 *     the map is read off the files the run actually wrote.
 */
import { create } from "@bufbuild/protobuf";
import { readFileSync, readdirSync, statSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { conversationv1 } from "../../../src/proto.js";
import { createFold, type FoldOutput } from "../../../src/convert/fold.js";
import type { FoldContext, PendingAsk } from "../../../src/convert/fold-context.js";
import type { SdkMessage } from "../../../src/sdk/types.js";
import type { PersistEntry } from "../../../src/store/persistence.js";

const HERE = dirname(fileURLToPath(import.meta.url));

/** The landed capture root. Sibling of `testdata/corpus`, and read-only here. */
export const CAPTURES = join(HERE, "..", "..", "..", "testdata", "captures");

/** One recorded line of a capture stream. */
export interface CaptureLine {
  readonly t_ms: number;
  readonly dir: "sdk" | "control";
  readonly msg: Record<string, unknown>;
}

/** What the capture harness recorded about the run beside the stream. */
export interface CaptureMeta {
  readonly scenario: string;
  readonly ok: boolean;
  readonly failure_reasons: readonly string[];
  readonly captured_at: string;
  readonly vendor_session_id: string;
  readonly expect: readonly string[];
  readonly cwd_slug: string | null;
}

/** Every committed scenario, in directory order. */
export function scenarioNames(): string[] {
  return readdirSync(CAPTURES)
    .filter((name) => !name.startsWith(".") && !name.endsWith(".md"))
    .filter((name) => statSync(join(CAPTURES, name)).isDirectory())
    .sort();
}

/** Every line of one capture's stream, in recorded order. */
export function captureLines(scenario: string): CaptureLine[] {
  return readFileSync(join(CAPTURES, scenario, "stream.jsonl"), "utf8")
    .split("\n")
    .filter((line) => line.trim() !== "")
    .map((line) => JSON.parse(line) as CaptureLine);
}

/** One capture's `meta.json`. */
export function captureMeta(scenario: string): CaptureMeta {
  return JSON.parse(readFileSync(join(CAPTURES, scenario, "meta.json"), "utf8")) as CaptureMeta;
}

/** The SDK messages the converter consumes, in arrival order. */
export function sdkMessages(scenario: string): SdkMessage[] {
  return captureLines(scenario)
    .filter((line) => line.dir === "sdk")
    .map((line) => line.msg as unknown as SdkMessage);
}

/** The control lines the shim itself drove, in order. */
export function controlLines(scenario: string): Record<string, unknown>[] {
  return captureLines(scenario)
    .filter((line) => line.dir === "control")
    .map((line) => line.msg);
}

/** The vendor's transcript directory for a capture, when the run wrote one. */
function projectsDir(scenario: string): string | undefined {
  const root = join(CAPTURES, scenario, "files", "projects");
  let slugs: string[];
  try {
    slugs = readdirSync(root);
  } catch {
    return undefined;
  }
  const [slug] = slugs.sort();
  return slug === undefined ? undefined : join(root, slug);
}

/** Every vendor transcript line the run wrote for the main session. */
export function transcriptLines(scenario: string): Record<string, unknown>[] {
  const dir = projectsDir(scenario);
  if (dir === undefined) return [];
  const files = readdirSync(dir).filter((name) => name.endsWith(".jsonl"));
  return files.flatMap((name) =>
    readFileSync(join(dir, name), "utf8")
      .split("\n")
      .filter((line) => line.trim() !== "")
      .map((line) => JSON.parse(line) as Record<string, unknown>),
  );
}

/** One subagent file the vendor wrote: its own id, and the call that spawned it. */
export interface SubagentFile {
  /** The vendor's 17-hex agent id — the file's own name. */
  readonly agentId: string;
  /** `.meta.json`'s `toolUseId`: the spawning call. */
  readonly toolUseId?: string;
  /** The sidechain lines, in file order. */
  readonly lines: readonly Record<string, unknown>[];
}

/** Every subagent the run recorded, keyed by the vendor's agent id. */
export function subagentFiles(scenario: string): SubagentFile[] {
  const dir = projectsDir(scenario);
  if (dir === undefined) return [];
  const sessions = readdirSync(dir).filter((name) => !name.includes("."));
  const out: SubagentFile[] = [];
  for (const session of sessions) {
    const subagents = join(dir, session, "subagents");
    let names: string[];
    try {
      names = readdirSync(subagents);
    } catch {
      continue;
    }
    for (const name of names.filter((n) => n.endsWith(".jsonl"))) {
      const agentId = name.replace(/^agent-/, "").replace(/\.jsonl$/, "");
      const lines = readFileSync(join(subagents, name), "utf8")
        .split("\n")
        .filter((line) => line.trim() !== "")
        .map((line) => JSON.parse(line) as Record<string, unknown>);
      let toolUseId: string | undefined;
      try {
        const meta = JSON.parse(
          readFileSync(join(subagents, `agent-${agentId}.meta.json`), "utf8"),
        ) as { toolUseId?: string };
        toolUseId = meta.toolUseId;
      } catch {
        toolUseId = undefined;
      }
      out.push({ agentId, lines, ...(toolUseId === undefined ? {} : { toolUseId }) });
    }
  }
  return out.sort((a, b) => a.agentId.localeCompare(b.agentId));
}

/** The main agent every golden folds against. */
export const MAIN_AGENT = create(conversationv1.AgentIdSchema, { value: "main-agent" });

/** Every `tool_use` block the stream announced, in order. */
export function toolUses(scenario: string): { id: string; name: string; input: unknown }[] {
  const out: { id: string; name: string; input: unknown }[] = [];
  for (const message of sdkMessages(scenario)) {
    const record = message as unknown as { type: string; message?: { content?: unknown[] } };
    if (record.type !== "assistant") continue;
    for (const block of record.message?.content ?? []) {
      const typed = block as { type?: string; id?: string; name?: string; input?: unknown };
      if (typed.type === "tool_use" && typed.id !== undefined && typed.name !== undefined) {
        out.push({ id: typed.id, name: typed.name, input: typed.input });
      }
    }
  }
  return out;
}

/**
 * The permission gate's open asks, recovered from the capture.
 *
 * The recording states a `can_use_tool_request`'s tool and input but not the
 * call it belongs to, so the join is by (tool name, input) against the calls the
 * same stream announced. A request whose input matches no announced call is
 * DROPPED rather than guessed at — a wrong join here would make the fold assert
 * a permission arm for the wrong unit.
 */
function pendingAsksOf(scenario: string): Map<string, PendingAsk> {
  const calls = toolUses(scenario);
  const asks = new Map<string, PendingAsk>();
  const used = new Set<string>();
  for (const control of controlLines(scenario)) {
    if (control["kind"] !== "can_use_tool_request") continue;
    const name = control["tool_name"];
    const input = JSON.stringify(control["input"]);
    const match = calls.find(
      (call) => call.name === name && JSON.stringify(call.input) === input && !used.has(call.id),
    );
    if (match === undefined) continue;
    used.add(match.id);
    asks.set(match.id, { kind: name === "AskUserQuestion" ? "question" : "permission" });
  }
  return asks;
}

/** The subagent books the run's own files name: spawning call → vendor agent id. */
function subagentBooks(scenario: string): Map<string, conversationv1.AgentId> {
  const map = new Map<string, conversationv1.AgentId>();
  for (const file of subagentFiles(scenario)) {
    if (file.toolUseId === undefined) continue;
    map.set(file.toolUseId, create(conversationv1.AgentIdSchema, { value: file.agentId }));
  }
  return map;
}

/** What a golden wants to vary about the engine's knowledge. */
export interface GoldenOverrides {
  readonly keepalive?: boolean;
  readonly mcpServerNames?: readonly string[];
  readonly reportFault?: (kind: "converter_defect", detail: string) => void;
}

/** The fold context a golden runs under, derived from the capture itself. */
export function goldenContext(scenario: string, overrides: GoldenOverrides = {}): FoldContext {
  const asks = pendingAsksOf(scenario);
  const books = subagentBooks(scenario);
  return {
    mainAgentId: MAIN_AGENT,
    turnId: create(conversationv1.TurnIdSchema, { value: `turn-${scenario}` }),
    keepalive: overrides.keepalive ?? false,
    nowMs: () => 1_000,
    pendingAsk: (toolUseId: string) => asks.get(toolUseId),
    liveTask: () => undefined,
    subagentFor: (toolUseId: string) => books.get(toolUseId),
    mcpServerNames: () => overrides.mcpServerNames ?? [],
    ...(overrides.reportFault === undefined ? {} : { reportFault: overrides.reportFault }),
  };
}

/** Everything one scenario's whole stream produced. */
export interface GoldenRun {
  readonly entries: readonly PersistEntry[];
  readonly outputs: readonly FoldOutput[];
  readonly turnEnds: readonly conversationv1.AgentFrame[];
  /** Every converter defect the fold reported while folding the capture. */
  readonly faults: readonly string[];
}

/** Fold one whole capture through the REAL fold, in recorded order. */
export function foldScenario(scenario: string, overrides: GoldenOverrides = {}): GoldenRun {
  const faults: string[] = [];
  const context = goldenContext(scenario, {
    ...overrides,
    reportFault: (kind, detail) => {
      faults.push(detail);
      overrides.reportFault?.(kind, detail);
    },
  });
  const fold = createFold();
  const entries: PersistEntry[] = [];
  const outputs: FoldOutput[] = [];
  const turnEnds: conversationv1.AgentFrame[] = [];
  for (const message of sdkMessages(scenario)) {
    const output = fold.onSdkMessage(message, context);
    outputs.push(output);
    entries.push(...output.entries);
    if (output.turnEnded !== undefined) turnEnds.push(output.turnEnded.frame);
  }
  return { entries, outputs, turnEnds, faults };
}

// ---------------------------------------------------------------------------
// Reading a run back
// ---------------------------------------------------------------------------

/** The activity a row carries, when it carries one. */
export function activityOf(entry: PersistEntry): conversationv1.AgentActivity | undefined {
  if (entry.item.kind !== "frame") return undefined;
  const result = entry.item.frame.result;
  if (result.case !== "update") return undefined;
  const update = result.value.update;
  return update.case === "activity" ? update.value : undefined;
}

/** The unit KINDS a run produced, deduplicated, in first-appearance order. */
export function unitKinds(run: GoldenRun): string[] {
  const seen: string[] = [];
  for (const entry of run.entries) {
    const activity = activityOf(entry);
    const kind = activity?.item.case;
    if (kind !== undefined && !seen.includes(kind)) seen.push(kind);
  }
  return seen;
}

/** Every distinct arm path a run wrote, in first-appearance order. */
export function arms(run: GoldenRun): string[] {
  const seen: string[] = [];
  for (const entry of run.entries) {
    if (!seen.includes(entry.source.discriminator)) seen.push(entry.source.discriminator);
  }
  return seen;
}

/** Every residue key a run wrote (`<source>` for unknown/unparsed, arm for vendor-specific). */
export function residueKeys(run: GoldenRun): string[] {
  const seen: string[] = [];
  for (const entry of run.entries) {
    if (entry.item.kind !== "residue") continue;
    const item = entry.item.residue.unservedItem;
    const key =
      item.case === "vendorSpecific"
        ? `vendor_specific/${item.value.kind}`
        : item.case === "unknown"
          ? `unknown/${item.value.discriminator}`
          : item.case === "unparsed"
            ? `unparsed/${item.value.source}`
            : `${item.case ?? "unset"}`;
    if (!seen.includes(key)) seen.push(key);
  }
  return seen;
}

/** Every session-update arm a run produced, in first-appearance order. */
export function sessionUpdateArms(run: GoldenRun): string[] {
  const seen: string[] = [];
  for (const entry of run.entries) {
    if (entry.item.kind !== "session_update") continue;
    const arm = entry.item.update.update.case ?? "unset";
    if (!seen.includes(arm)) seen.push(arm);
  }
  return seen;
}

/** The turn terminal arm a run ended on, when it ended. */
export function terminalArm(run: GoldenRun): string | undefined {
  const [frame] = run.turnEnds;
  if (frame === undefined) return undefined;
  if (frame.result.case === "success") return `success.${frame.result.value.outcome.case ?? "unset"}`;
  if (frame.result.case === "failure") return `failure.${frame.result.value.failure.case ?? "unset"}`;
  return frame.result.case ?? "unset";
}
