/**
 * test/fake/goldens.ts — THE SCENARIO GOLDENS: what each mocked-vendor scenario
 * produces, driven once, deterministically, and checked in.
 *
 * ONE DRIVE, BOTH PLANES. Each scenario is driven in-process (driveScenario: no
 * spawned shim, a counted uuid source, a clock that advances one millisecond per
 * read) and yields two outputs:
 *
 * - `frames.json`: the STREAM plane, every row the fold produced, in order.
 * - `tree/`: the FILE plane, every file the mock wrote under its config root and
 *   its spool root, which is what the sidecar reads.
 *
 * They live at `testdata/scenario-goldens/<scenario>/` beside `meta.json`
 * (`{scenario, seam, planes}`; `seam` is the e2e matrix's to fill). Other
 * systems' unit suites read them instead of booting this shim
 * (`agent-repl/AGENTS.md`, "A system's integration tests run that system
 * alone"); goldens.test.ts keeps them fresh, and
 * `AGENT_REPL_SCENARIO_GOLDENS_WRITE=1` rewrites them.
 *
 * THE ONLY SUBSTITUTION IS THE DRIVE'S TEMP ROOT. Every id and instant is
 * already fixed at the source, so a golden is the drive's bytes with the one
 * thing a fresh directory makes different, its path, replaced by
 * {@link ROOT_TOKEN} (and its vendor slug by {@link ROOT_SLUG_TOKEN}).
 */
import { realpathSync, readdirSync, readFileSync, statSync } from "node:fs";
import { join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { toJson } from "@bufbuild/protobuf";

import { createFold } from "../../src/convert/fold.js";
import { cwdSlug } from "../../src/engine/vendor-paths.js";
import { conversationv1, storev1 } from "../../src/proto.js";
import type { CanUseToolLike } from "../../src/sdk/types.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import { foldContext } from "../convert/fold-harness.js";
import { driveScenario, HARNESS_NOW_MS, type DriveOptions, type Driven } from "./harness.js";

/** Where the goldens live, under the module's shared testdata. */
export const GOLDENS_DIR = resolve(fileURLToPath(new URL(".", import.meta.url)), "../../../../../testdata/scenario-goldens");

/** The environment switch that rewrites every golden from a fresh drive. */
export const GOLDENS_WRITE_ENV = "AGENT_REPL_SCENARIO_GOLDENS_WRITE";

/** The drive's temp root, wherever it appears in a golden. */
export const ROOT_TOKEN = "${GOLDEN_ROOT}";

/** The drive's temp root as the vendor slugs it into a project or spool directory name. */
export const ROOT_SLUG_TOKEN = "${GOLDEN_ROOT_SLUG}";

/** How the drive answers a scenario that blocks on the user, or what it does mid-drive. */
type Policy = "allow" | "deny" | "answer" | "decline" | "background" | "interrupt";

/** One scenario the goldens cover, and how its drive is completed. */
export interface GoldenScenario {
  /** The `!name` prompt that selects it. */
  readonly prompt: string;
  /** How the drive answers or steers it. */
  readonly policy: Policy;
}

/**
 * Every scenario with a golden. The policy follows the scenario's own name: a
 * `!perm-deny-*` row is the one where the user denies, every other gate is
 * allowed once, every question is answered with its first option, and the
 * scenarios that park until someone acts are acted on.
 */
export const GOLDEN_SCENARIOS: readonly GoldenScenario[] = [
  "!md", "!read", "!read-head", "!read-range", "!read-truncated", "!read-image", "!write-create",
  "!write-update", "!edit", "!ide-diagnostics", "!grep-content", "!grep-files", "!grep-count", "!glob",
  "!bash", "!bash-fail", "!bash-timeout", "!bash-spill", "!bash-image", "!bash-detach", "!bash-detach-fail",
  "!bash-detach-live", "!vendor-backgrounded", "!web-fetch", "!web-fetch-redirect", "!web-search", "!skill",
  "!skill-fail", "!memory", "!skills-injected", "!task-create", "!task-change", "!task-reject",
  "!send-message", "!send-message-resumed", "!send-message-refused", "!subagent", "!subagent-detached",
  "!subagent-failed", "!cancel-all", "!plan", "!findings", "!worktree-keep", "!worktree-remove", "!cron",
  "!push-sent", "!push-config-off", "!push-user-present", "!push-no-transport", "!monitor-deadline",
  "!monitor-persistent", "!wakeup-schedule", "!wakeup-stop", "!artifact-publish", "!artifact-list",
  "!unmodeled", "!hook-success", "!hook-blocked", "!hook-failed", "!hook-cancelled", "!perm-allow-once",
  "!perm-allow-standing", "!perm-deny-user", "!perm-deny-policy", "!perm-undecidable", "!ask-single",
  "!ask-multi", "!ask-free", "!ask-unanswered", "!rotate", "!slash", "!context-usage-drift",
  "!model-fallback", "!fast-on", "!fast-off", "!fast-cooldown", "!mcp-all", "!mcp-healthy",
  "!usage-available", "!usage-full", "!usage-opus-absent", "!usage-service-unavailable",
  "!usage-window-unavailable", "!usage-utilization-unavailable", "!usage-sampling-failure", "!rate-limit",
  "!compact", "!compact-auto", "!compact-failed", "!away-summary", "!residue", "!cold-seed",
  "!fail-execution", "!fail-max-turns", "!fail-budget", "!fail-structured-output", "!fail-blocking-limit",
  "!fail-rapid-refill", "!fail-prompt-too-long", "!fail-image", "!fail-model", "!fail-malformed-tool-use",
  "!fail-tool-deferred", "!fail-tool-deferred-unavailable", "!fail-turn-setup", "!fail-aborted-tools",
  "!fail-stop-hook", "!fail-hook-stopped", "!fail-continuation-prevented", "!api-429", "!api-529",
  "!api-401", "!api-403", "!api-400", "!api-413", "!api-404", "!api-500", "!api-billing", "!api-oauth-org",
  "!api-max-output", "!api-unmodeled", "!max-tokens", "!refusal-fallback", "!refusal-no-fallback",
  "!context-window", "!fault-converter", "!fault-recover", "!hold", "!interrupt", "!query-eof",
  "!query-fail", "!keepalive",
].map((prompt) => ({ prompt, policy: policyOf(prompt) }));

function policyOf(prompt: string): Policy {
  if (prompt.startsWith("!perm-deny")) return "deny";
  if (prompt === "!ask-unanswered") return "decline";
  if (prompt.startsWith("!ask-")) return "answer";
  if (prompt === "!vendor-backgrounded") return "background";
  if (prompt === "!hold" || prompt === "!interrupt") return "interrupt";
  return "allow";
}

const ALLOW: CanUseToolLike = async (_name, input) => ({ behavior: "allow", updatedInput: input });

const DENY: CanUseToolLike = async () => ({ behavior: "deny", message: "the user declined this call" });

const DECLINE: CanUseToolLike = async () => ({ behavior: "deny", message: "the question was never answered" });

/** Answers every question with its first option, or a free-text answer when it offers none. */
const ANSWER_FIRST: CanUseToolLike = async (name, input) => {
  if (name !== "AskUserQuestion") return { behavior: "allow", updatedInput: input };
  const questions = (input as { questions: { question: string; options?: { label: string }[] }[] }).questions;
  const answers: Record<string, string> = {};
  for (const q of questions) answers[q.question] = q.options?.[0]?.label ?? "the user's own answer";
  return { behavior: "allow", updatedInput: { ...input, answers } };
};

/** Lets the drive's queued messages reach the collector before acting on them. */
async function settle(): Promise<void> {
  for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
}

function driveOptions(policy: Policy): DriveOptions {
  let now = HARNESS_NOW_MS;
  const base: DriveOptions = { opts: { nowMs: () => now++, spawnTag: "g0000000" } };
  switch (policy) {
    case "allow":
      return { ...base, canUseTool: ALLOW };
    case "deny":
      return { ...base, canUseTool: DENY };
    case "answer":
      return { ...base, canUseTool: ANSWER_FIRST };
    case "decline":
      return { ...base, canUseTool: DECLINE };
    case "background":
      return {
        ...base,
        during: async (query, _prompts, messages) => {
          await settle();
          const started = messages.find((m) => m.subtype === "task_started");
          if (started === undefined) throw new Error("goldens: no task to background");
          await query.backgroundTasks(String(started.tool_use_id));
        },
      };
    case "interrupt":
      return {
        ...base,
        during: async (query) => {
          await settle();
          await query.interrupt();
        },
      };
  }
}

/** What one scenario's drive produced, with its temp root already substituted. */
export interface GoldenOutput {
  /** frames.json's content. */
  readonly frames: string;
  /** meta.json's content. */
  readonly meta: string;
  /** Every file the mock wrote, by its substituted path under the root. */
  readonly tree: ReadonlyMap<string, string>;
}

/** The directory name a scenario's golden lives under: its prompt without the `!`. */
export function goldenName(prompt: string): string {
  return prompt.replace(/^!/, "");
}

/** Drive one scenario and render both planes. Must run inside a test (its temp root is the test's). */
export async function driveGolden(scenario: GoldenScenario): Promise<GoldenOutput> {
  const driven = await driveScenario([scenario.prompt], driveOptions(scenario.policy));
  if (driven.failure !== undefined && !scenario.prompt.startsWith("!query-")) {
    throw new Error(`goldens: the drive of ${scenario.prompt} died: ${String(driven.failure)}`);
  }
  const substitute = substitution(rootOf(driven));
  return {
    frames: substitute(renderFrames(driven)),
    meta: `${JSON.stringify({ scenario: scenario.prompt, seam: null, planes: ["stream", "file"] }, null, 2)}\n`,
    tree: readTree(rootOf(driven), substitute),
  };
}

/** The drive's own temp root: the parent of its config directory. */
function rootOf(driven: Driven): string {
  return resolve(driven.configDir, "..");
}

/** Replaces the root, as written and as resolved, and the vendor's slugs of both. */
function substitution(root: string): (text: string) => string {
  const roots = [...new Set([realpathSync(root), root])].sort((a, b) => b.length - a.length);
  return (text) => {
    let out = text;
    for (const r of roots) out = out.split(r).join(ROOT_TOKEN);
    for (const r of roots) out = out.split(cwdSlug(r)).join(ROOT_SLUG_TOKEN);
    return out;
  };
}

/** Every file under root, keyed by its substituted relative path. */
function readTree(root: string, substitute: (text: string) => string): Map<string, string> {
  const tree = new Map<string, string>();
  const walk = (dir: string): void => {
    for (const name of readdirSync(dir).sort()) {
      const path = join(dir, name);
      if (statSync(path).isDirectory()) walk(path);
      else tree.set(substitute(relative(root, path)), substitute(readFileSync(path, "utf8")));
    }
  };
  walk(root);
  return tree;
}

/** The fold's rows for the drive's messages, one JSON object per row. */
function renderFrames(driven: Driven): string {
  const fold = createFold();
  const context = foldContext();
  const rows: unknown[] = [];
  for (const message of driven.messages) {
    for (const entry of fold.onSdkMessage(message, context).entries) rows.push(renderEntry(entry));
  }
  return `${JSON.stringify(rows, null, 2)}\n`;
}

function renderEntry(entry: PersistEntry): unknown {
  return {
    upsertKey: entry.upsertKey,
    book: entry.agentId?.value ?? null,
    keepalive: entry.keepalive,
    turn: entry.turn?.value ?? null,
    recordPlace: entry.recordPlace ?? null,
    source: entry.source,
    item: renderItem(entry.item),
  };
}

function renderItem(item: PersistEntry["item"]): unknown {
  switch (item.kind) {
    case "prompt":
      return { kind: item.kind, prompt: toJson(conversationv1.AgentPromptSchema, item.prompt) };
    case "peer":
      return { kind: item.kind, peer: toJson(conversationv1.PeerMessageSchema, item.peer) };
    case "frame":
      return { kind: item.kind, frame: toJson(conversationv1.AgentFrameSchema, item.frame) };
    case "session_update":
      return { kind: item.kind, update: toJson(conversationv1.SessionUpdateSchema, item.update) };
    case "bash_run":
      return {
        kind: item.kind,
        run: toJson(conversationv1.AgentActivityIdSchema, item.run),
        frame: toJson(conversationv1.AgentBashSchema, item.frame),
      };
    case "residue":
      return { kind: item.kind, residue: toJson(storev1.StoreUnservedItemSchema, item.residue) };
    case "shell_run_claim":
      return { kind: item.kind, claim: toJson(storev1.ShellRunClaimSchema, item.claim) };
  }
}
