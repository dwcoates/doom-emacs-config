/**
 * The held prompt card's fixtures: a tray context on a scripted daemon, held
 * prompts with the daemon's badge served, and the card mounted collapsed or
 * folded. The card's drawing tests (test/held/held-prompt.test.ts) and its
 * stylesheet rules (rules/held/held-prompt.rules.test.ts) both draw through
 * them.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  UpdateHeldPromptResponseSchema,
  type UpdateHeldPromptRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import {
  HeldPromptBadgeSchema,
  HeldPromptStatusNoteSchema,
  HeldPromptEditingSchema,
  HeldPromptCoalescedSchema,
  HeldPromptSchema,
  type HeldPrompt,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import { FeedUserPromptBodySchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import type { Ticker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { drawHeldPrompt, type HeldStatus } from "../../src/held/held-prompt.js";
import type { HeldContext } from "../../src/held/context.js";
import { installClickExpand } from "../../src/expand.js";
import { testAppContext } from "../rpc/app-context.js";
import { installStylesheet } from "../stylesheet.js";
import { drawnBodyOf } from "./drawn-body.js";
import { testHeldContext } from "./held-context.js";

export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
export const NOW = 1_700_000_000_000;

/** A ticker whose subscribers this test drives by hand. */
export function fakeTicker(): Ticker & {
  tick(nowMs: number): void;
  subscribers(): number;
} {
  const listeners = new Set<(nowMs: number) => void>();
  return {
    now: () => NOW,
    subscribe(fn) {
      listeners.add(fn);
      return () => listeners.delete(fn);
    },
    tick(nowMs: number) {
      for (const fn of [...listeners]) fn(nowMs);
    },
    subscribers: () => listeners.size,
  };
}

export const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

/** A context whose UpdateHeldPrompt answers with ANSWER and records requests. */
export function trayContext(
  answer: () => ReturnType<typeof successResponse> = successResponse,
  seen: UpdateHeldPromptRequest[] = [],
  ticker: Ticker = fakeTicker(),
): { tc: HeldContext; seen: UpdateHeldPromptRequest[]; ctx: AppContext } {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      updateHeldPrompt: (request) => {
        seen.push(request);
        return answer();
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker,
    failures: SINK,
    composerEnabled: false,
  });
  return { tc: testHeldContext(ctx), seen, ctx };
}

export const successResponse = () =>
  create(UpdateHeldPromptResponseSchema, {
    result: { case: "success", value: {} },
  });
/** What each fact-carrying arm must carry for its sentence to be complete. */
export const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
  releaseRefused: { cause: { case: "coldGate", value: {} } },
};

/** A refusal carrying ARM, with the payload that arm's sentence needs. */
export const refusalResponse = (arm: string) => () =>
  create(UpdateHeldPromptResponseSchema, {
    result: {
      case: "error",
      value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } },
    },
  } as never);
export const errorResponse = refusalResponse("noSuchHold");

/** A held prompt with every required field, overridden by OVERRIDES. */
export type HeldPromptInit = Exclude<
  MessageInitShape<typeof HeldPromptSchema>,
  HeldPrompt
>;

export function heldPrompt(overrides: Partial<HeldPromptInit> = {}): HeldPrompt {
  const prompt = create(HeldPromptSchema, {
    turn: { value: "turn-1" },
    queuedAt: { atMs: BigInt(NOW - 12_000) },
    said: overrides.said ?? {
      content: {
        blocks: [{ block: { case: "text", value: { text: "fix the test" } } }],
      },
    },
    body: overrides.body,
    classification: overrides.classification ?? {
      case: "classifying",
      value: {},
    },
    ...(overrides.hold !== undefined ? { hold: overrides.hold } : {}),
    ...(overrides.badge !== undefined ? { badge: overrides.badge } : {}),
    ...(overrides.notes !== undefined ? { notes: overrides.notes } : {}),
  });
  // THE DAEMON'S DRAWN BODY, as feed.DrawUserBlocks resolves the said.
  if (overrides.body === undefined) prompt.body = drawnBodyOf(prompt.said);
  else prompt.body = create(FeedUserPromptBodySchema, overrides.body);
  // THE DAEMON'S ONE BADGE and its notes, ranked as feed_held.proto fixes.
  // Their words are deliberately ones no card would compose (`wire <status>`),
  // so an assertion on them proves the card drew the wire verbatim.
  if (overrides.badge === undefined) serveStatus(prompt);
  return prompt;
}

/** Mark U as being edited, and serve the status the daemon then sends. */
export function markEditing(u: HeldPrompt): void {
  u.editing = create(HeldPromptEditingSchema, {});
  serveStatus(u);
}

/** Mark U as coalesced, and serve the status the daemon then sends. */
export function markCoalesced(u: HeldPrompt): void {
  u.coalesced = create(HeldPromptCoalescedSchema, {});
  serveStatus(u);
}

/** The label the test daemon sends for STATUS. */
export const wireLabel = (status: HeldStatus): string => `wire ${status}`;
/** The detail the test daemon sends for STATUS. */
export const wireDetail = (status: HeldStatus): string =>
  `the daemon's whole sentence for ${status}`;
/** The note the test daemon sends for the verdict's confirmation. */
/** The note the test daemon sends for a coalescence. */
export const COALESCED_NOTE = "wire note coalesced";

/** The standing facts that can claim the badge, strongest first. */
function rankedStatuses(prompt: HeldPrompt): HeldStatus[] {
  const ranked: HeldStatus[] = [];
  if (prompt.editing !== undefined) ranked.push("editing");
  if (prompt.sentNow !== undefined) ranked.push("sentNow");
  if (prompt.hold.case !== undefined) ranked.push(prompt.hold.case);
  const arm = prompt.classification.case;
  // daemon_held claims nothing: the hold arm beside it is what holds it.
  if (arm !== undefined && arm !== "daemonHeld") ranked.push(arm);
  return ranked;
}

/** Serve PROMPT the badge and notes the daemon composes for it. */
export function serveStatus(prompt: HeldPrompt): void {
  const [top, ...rest] = rankedStatuses(prompt);
  if (top === undefined) throw new Error("a test prompt with no fact to badge");
  prompt.badge = create(HeldPromptBadgeSchema, {
    label: wireLabel(top),
    detail: wireDetail(top),
    standsFor: { case: top, value: {} } as never,
  });
  const notes = rest.map((status) => wireDetail(status));
  if (prompt.coalesced !== undefined) notes.push(COALESCED_NOTE);
  prompt.notes = notes.map((sentence) => create(HeldPromptStatusNoteSchema, { sentence }));
}

/** Let the click's promise chain settle. */
export const settle = (): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, 0));

/** A held prompt whose words are TEXT. */
export const saying = (text: string): HeldPrompt =>
  heldPrompt({
    said: {
      content: { blocks: [{ block: { case: "text", value: { text } } }] },
    },
  });

/** A held prompt carrying every part a card draws, mounted under the one toggle. */
export function mountedFull(): HTMLElement {
  const { tc } = trayContext();
  const card = drawHeldPrompt(
    heldPrompt({
      classification: {
        case: "holdForTurnEnd",
        value: { rationale: "it can wait" },
      },
      hold: { case: "shutdown", value: { scheduleId: "sched-9" } },
    }),
    tc,
  );
  const host = document.createElement("div");
  installClickExpand(host, () => "");
  host.append(card);
  document.body.append(host);
  return card;
}

/** Run CHECK on a held prompt of forty lines, mounted with the real stylesheet. */
export function withTallHeld(check: (card: HTMLElement, box: HTMLElement) => void): void {
  const restore = installStylesheet();
  const { tc } = trayContext();
  const card = drawHeldPrompt(saying(Array.from({ length: 40 }, (_, i) => `line ${i}`).join("\n\n")), tc);
  document.body.append(card);
  try {
    check(card, card.querySelector<HTMLElement>(":scope > .bubble-scroll") as HTMLElement);
  } finally {
    card.remove();
    restore();
  }
}
