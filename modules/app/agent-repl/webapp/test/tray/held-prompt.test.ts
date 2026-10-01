// @vitest-environment jsdom
import { readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { afterEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import {
  EditHeldPromptResponseSchema,
  type EditHeldPromptRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_edit_held_prompt_pb";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  FoldHeldPromptErrorSchema,
  FoldHeldPromptResponseSchema,
  type FoldHeldPromptRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_held_prompt_pb";
import {
  UpdateHeldPromptErrorSchema,
  UpdateHeldPromptResponseSchema,
  type UpdateHeldPromptRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import { oneofArms } from "../arms.js";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  HeldPromptBadgeSchema,
  HeldPromptEditingSchema,
  HeldPromptCoalescedSchema,
  HeldPromptFoldAboveSchema,
  HeldPromptSchema,
  HeldSessionActSchema,
  type HeldPrompt,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { SessionCommand } from "../../../proto/gen/ts/conversation/v1/slash_command_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import type { Ticker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import {
  DROPPED_EVENT,
  EDIT_REQUEST,
  FOLD_ABOVE_LABEL,
  NO_RELEASE_TITLES,
  SEND_NOW_LABEL,
  HELD_BADGE_DETAIL_CLASS,
  HELD_STATUS_BADGES,
  drawHeldPrompt,
  drawUnsupportedBlock,
  heldBadgeClasses,
  sessionCommandLiteral,
  type HeldPromptDroppedDetail,
  type HeldStatus,
} from "../../src/tray/held-prompt.js";
import * as heldPromptModule from "../../src/tray/held-prompt.js";
import type { TrayContext } from "../../src/tray/context.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_EXPAND_ONLY_CLASS,
  BUBBLE_MORE_ATTRIBUTE,
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANT_ATTRIBUTE,
} from "../../src/bubble/draw.js";
import { PROMPT_WAVE_ATTRIBUTE } from "../../src/breathing.js";
import {
  FITTING_TREE,
  WIDE_TREE,
  installTreeLayout,
  stagedCols,
  treeLineWidths,
  useTreeLayout,
} from "../tree-layout.js";
import { installClickExpand } from "../../src/expand.js";
import { HAS_MORE_CLASS, refreshHasMore } from "../../src/feed/bubble-more.js";
import { resetLoggingForTests } from "../../src/log.js";
import stylesheet from "../../src/styles.css?raw";
import heldPromptSource from "../../src/tray/held-prompt.ts?raw";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const NOW = 1_700_000_000_000;

/** A ticker whose subscribers this test drives by hand. */
function fakeTicker(): Ticker & { tick(nowMs: number): void; subscribers(): number } {
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

const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

/** A context whose UpdateHeldPrompt answers with ANSWER and records requests. */
function trayContext(
  answer: () => ReturnType<typeof successResponse> = successResponse,
  seen: UpdateHeldPromptRequest[] = [],
  ticker: Ticker = fakeTicker(),
): { tc: TrayContext; seen: UpdateHeldPromptRequest[]; ctx: AppContext } {
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
  return { tc: { ctx, onDispose: () => undefined }, seen, ctx };
}

const successResponse = () =>
  create(UpdateHeldPromptResponseSchema, { result: { case: "success", value: {} } });
/** What each fact-carrying arm must carry for its sentence to be complete. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
};

/** A refusal carrying ARM, with the payload that arm's sentence needs. */
const refusalResponse = (arm: string) => () =>
  create(UpdateHeldPromptResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } } },
  } as never);
const errorResponse = refusalResponse("noSuchHold");

/** A held prompt with every required field, overridden by OVERRIDES. */
type HeldPromptInit = Exclude<MessageInitShape<typeof HeldPromptSchema>, HeldPrompt>;

function heldPrompt(overrides: Partial<HeldPromptInit> = {}): HeldPrompt {
  const prompt = create(HeldPromptSchema, {
    turn: { value: "turn-1" },
    queuedAt: { atMs: BigInt(NOW - 12_000) },
    said: overrides.said ?? {
      content: { blocks: [{ block: { case: "text", value: { text: "fix the test" } } }] },
    },
    classification: overrides.classification ?? { case: "classifying", value: {} },
    ...(overrides.hold !== undefined ? { hold: overrides.hold } : {}),
    ...(overrides.badges !== undefined ? { badges: overrides.badges } : {}),
  });
  // THE DAEMON'S BADGES, one per standing fact, in the proto's order. Their
  // words are deliberately ones no card would compose (`wire <status>`), so an
  // assertion on them proves the card drew the wire verbatim.
  if (overrides.badges === undefined) {
    prompt.badges = standingStatuses(prompt).map((status) =>
      create(HeldPromptBadgeSchema, { label: wireLabel(status), detail: wireDetail(status) }),
    );
  }
  return prompt;
}

/** Mark U as being edited, and serve the badges the daemon then sends. */
function markEditing(u: HeldPrompt): void {
  u.editing = create(HeldPromptEditingSchema, {});
  u.badges = standingStatuses(u).map((status) =>
    create(HeldPromptBadgeSchema, { label: wireLabel(status), detail: wireDetail(status) }),
  );
}

/** Mark U as coalesced, and serve the badges the daemon then sends. */
function markCoalesced(u: HeldPrompt): void {
  u.coalesced = create(HeldPromptCoalescedSchema, {});
  u.badges = standingStatuses(u).map((status) =>
    create(HeldPromptBadgeSchema, { label: wireLabel(status), detail: wireDetail(status) }),
  );
}

/** The label the test daemon sends for STATUS. */
const wireLabel = (status: HeldStatus): string => `wire ${status}`;
/** The detail the test daemon sends for STATUS. */
const wireDetail = (status: HeldStatus): string => `the daemon's whole sentence for ${status}`;

/** The facts a prompt's badges stand for, in daemon_hold.proto's order. */
function standingStatuses(prompt: HeldPrompt): HeldStatus[] {
  const statuses: HeldStatus[] = [];
  const classification = prompt.classification;
  if (classification.case !== undefined) {
    statuses.push(classification.case);
    if (prompt.editing !== undefined) statuses.push("editing");
    if (prompt.coalesced !== undefined) statuses.push("coalesced");
    if (classification.case === "holdForTurnEnd" && classification.value.accepted?.accepted === true) {
      statuses.push("accepted");
    }
  }
  if (prompt.hold.case !== undefined) statuses.push(prompt.hold.case);
  return statuses;
}

/** Let the click's promise chain settle. */
const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

describe("drawHeldPrompt identity", () => {
  it("carries the echoed TurnId on the card", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.getAttribute("data-held-turn")).toBe("turn-1");
  });

  it("states an absent hold as `none` rather than omitting the attribute", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.getAttribute("data-hold")).toBe("none");
  });

  it("hangs the card on the prompt bubble's own right rail", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.classList.contains("held-right")).toBe(true);
  });

  it("keeps the right rail on a card wearing a hold frame", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(
      heldPrompt({ hold: { case: "shutdown", value: { scheduleId: "sched-9" } } }),
      tc,
    );
    // Assert
    expect(card.classList.contains("held-right")).toBe(true);
  });

  it("draws the words the user typed", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.querySelector(".queued-text")?.textContent).toContain("fix the test");
  });
});

describe("drawHeldPrompt classification arms", () => {
  const cases: Array<{ name: string; prompt: HeldPrompt; arm: string; badge: string }> = [
    {
      name: "classifying",
      prompt: heldPrompt({ classification: { case: "classifying", value: {} } }),
      arm: "classifying",
      badge: "wire classifying",
    },
    {
      name: "interject",
      prompt: heldPrompt({
        classification: { case: "interject", value: { rationale: "it stops the wrong work" } },
      }),
      arm: "interject",
      badge: "wire interject",
    },
    {
      name: "after_tool_call",
      prompt: heldPrompt({
        classification: { case: "afterToolCall", value: { rationale: "it adds to the running work" } },
      }),
      arm: "afterToolCall",
      badge: "wire afterToolCall",
    },
    {
      name: "hold_for_turn_end",
      prompt: heldPrompt({
        classification: { case: "holdForTurnEnd", value: { rationale: "it can wait" } },
      }),
      arm: "holdForTurnEnd",
      badge: "wire holdForTurnEnd",
    },
    {
      name: "uninterruptible_turn",
      prompt: heldPrompt({
        classification: {
          case: "uninterruptibleTurn",
          value: { command: SessionCommand.COMPACT },
        },
      }),
      arm: "uninterruptibleTurn",
      badge: "wire uninterruptibleTurn",
    },
    {
      name: "classification_error",
      prompt: heldPrompt({
        classification: { case: "classificationError", value: { detail: "answered neither" } },
      }),
      arm: "classificationError",
      badge: "wire classificationError",
    },
  ];

  for (const testCase of cases) {
    it(`marks the ${testCase.name} arm on the card`, () => {
      const { tc } = trayContext();
      const card = drawHeldPrompt(testCase.prompt, tc);
      expect(card.getAttribute("data-arm")).toBe(testCase.arm);
    });

    it(`badges the ${testCase.name} arm distinctly`, () => {
      const { tc } = trayContext();
      const card = drawHeldPrompt(testCase.prompt, tc);
      expect(card.querySelector(".queued-head > .held-badge")?.textContent).toBe(testCase.badge);
    });
  }

  it("draws the classifier's rationale on an interjection", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "interject", value: { rationale: "urgent" } } }),
      tc,
    );
    expect(card.querySelector(".queued-reason")?.textContent).toBe("urgent");
  });

  it("draws the classifier's rationale on a prompt joining the running turn", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "afterToolCall", value: { rationale: "it adds to it" } } }),
      tc,
    );
    expect(card.querySelector(".queued-reason")?.textContent).toBe("it adds to it");
  });

  it("offers no accept on a prompt joining the running turn", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "afterToolCall", value: { rationale: "" } } }),
      tc,
    );
    expect(card.querySelector("[data-held-action='accept']")).toBeNull();
  });

  it("draws no rationale element when the classifier gave none", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "interject", value: { rationale: "" } } }),
      tc,
    );
    expect(card.querySelector(".queued-reason")).toBeNull();
  });

  it("draws the failure detail on classification_error", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        classification: { case: "classificationError", value: { detail: "both tokens" } },
      }),
      tc,
    );
    expect(card.querySelector(".queued-unclassified")?.textContent).toBe("both tokens");
  });

  it("refuses an entry whose classification oneof is unset", () => {
    const { tc } = trayContext();
    const prompt = create(HeldPromptSchema, {
      turn: { value: "turn-1" },
      said: { content: { blocks: [] } },
      queuedAt: { atMs: BigInt(NOW) },
    });
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });
});

describe("drawHeldPrompt accept", () => {
  it("offers accept on hold_for_turn_end", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "holdForTurnEnd", value: { rationale: "later" } } }),
      tc,
    );
    expect(card.querySelector('[data-held-action="accept"]')).not.toBeNull();
  });

  it("offers accept on no other classification arm", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "interject", value: { rationale: "now" } } }),
      tc,
    );
    expect(card.querySelector('[data-held-action="accept"]')).toBeNull();
  });

  it("replaces the accept button with the confirmed marker once accepted", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        classification: {
          case: "holdForTurnEnd",
          value: { rationale: "later", accepted: { accepted: true } },
        },
      }),
      tc,
    );
    expect(card.querySelector('[data-held-action="accept"]')).toBeNull();
    expect(card.querySelector("[data-accepted]")?.textContent).toBe("wire accepted");
  });

  it("echoes the TurnId on the accept request", async () => {
    const seen: UpdateHeldPromptRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "holdForTurnEnd", value: { rationale: "later" } } }),
      tc,
    );
    card.querySelector<HTMLButtonElement>('[data-held-action="accept"]')?.click();
    await settle();
    expect(seen[0]?.action.case).toBe("accept");
    expect(seen[0]?.turn?.value).toBe("turn-1");
  });
});

describe("drawHeldPrompt hold arms", () => {
  const holds: Array<{ name: string; prompt: HeldPrompt; arm: string; line: string }> = [
    {
      name: "shutdown",
      prompt: heldPrompt({ hold: { case: "shutdown", value: { scheduleId: "sched-9" } } }),
      arm: "shutdown",
      line: "wire shutdown",
    },
    {
      name: "session_starting",
      prompt: heldPrompt({ hold: { case: "sessionStarting", value: {} } }),
      arm: "sessionStarting",
      line: "wire sessionStarting",
    },
    {
      name: "build_refresh",
      prompt: heldPrompt({ hold: { case: "buildRefresh", value: {} } }),
      arm: "buildRefresh",
      line: "wire buildRefresh",
    },
  ];

  for (const hold of holds) {
    it(`marks the ${hold.name} hold on the card`, () => {
      const { tc } = trayContext();
      const card = drawHeldPrompt(hold.prompt, tc);
      expect(card.getAttribute("data-hold")).toBe(hold.arm);
    });

    it(`badges the ${hold.name} hold in the daemon's words`, () => {
      const { tc } = trayContext();
      const card = drawHeldPrompt(hold.prompt, tc);
      expect(card.querySelector(`.queued-head > [data-held-status="${hold.arm}"]`)?.textContent).toBe(hold.line);
    });
  }

  it("names the schedule holding a shutdown-held entry", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ hold: { case: "shutdown", value: { scheduleId: "sched-9" } } }),
      tc,
    );
    expect(card.querySelector('[data-held-status="shutdown"]')?.getAttribute("data-schedule-id")).toBe("sched-9");
  });

});

describe("drawHeldPrompt release availability", () => {
  it("offers release in the ordinary case", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.querySelector('[data-held-action="release"]')).not.toBeNull();
  });

  it("labels the release control Send now", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.querySelector('[data-held-action="release"]')?.textContent).toBe("Send now");
  });

  it("gives the release control the accessible name Send now", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    const button = card.querySelector<HTMLButtonElement>('[data-held-action="release"]');
    expect([button?.getAttribute("aria-label"), button?.textContent]).toEqual([null, SEND_NOW_LABEL]);
  });

  for (const [arm, title] of Object.entries(NO_RELEASE_TITLES)) {
    it(`words the ${arm} tooltip without the retired Release name`, () => {
      expect(/releas/i.test(title)).toBe(false);
    });
  }

  const forbidden: Array<{ name: string; prompt: HeldPrompt }> = [
    {
      name: "uninterruptible_turn",
      prompt: heldPrompt({
        classification: { case: "uninterruptibleTurn", value: { command: SessionCommand.CLEAR } },
      }),
    },
    {
      name: "session_starting",
      prompt: heldPrompt({ hold: { case: "sessionStarting", value: {} } }),
    },
  ];

  // EVERY ENTRY OFFERS A RELEASE: where an arm forbids the interrupt a release
  // needs, the daemon refuses it and the refusal is said at the control — the
  // contract's own answer for a verb that cannot run — rather than the button
  // being withheld and the reason hidden in a hover.
  for (const entry of forbidden) {
    it(`still offers release on ${entry.name}, for the daemon to refuse`, () => {
      const { tc } = trayContext();
      const card = drawHeldPrompt(entry.prompt, tc);
      expect(card.querySelector('[data-held-action="release"]')).not.toBeNull();
    });
  }

  it("warns on the release button why the arm is likely to refuse it", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({ hold: { case: "sessionStarting", value: {} } }),
      tc,
    );
    expect(
      card.querySelector<HTMLElement>('[data-held-action="release"]')?.title,
    ).toContain("the session is not up yet");
  });

  it("still offers release under a build refresh, which forbids nothing", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt({ hold: { case: "buildRefresh", value: {} } }), tc);
    expect(card.querySelector('[data-held-action="release"]')).not.toBeNull();
  });
});

describe("drawHeldPrompt actions", () => {
  it("echoes the TurnId on a release", async () => {
    const seen: UpdateHeldPromptRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="release"]')?.click();
    await settle();
    expect(seen[0]?.action.case).toBe("release");
    expect(seen[0]?.turn?.value).toBe("turn-1");
  });

  it("echoes the workspace on a drop", async () => {
    const seen: UpdateHeldPromptRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    expect(seen[0]?.workspace?.id).toBe("ws-1");
  });

  it("draws the refusal at the row when the daemon refuses", async () => {
    const { tc } = trayContext(errorResponse);
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    expect(card.querySelector(".queued-refusal")?.getAttribute("data-arm")).toBe("noSuchHold");
  });

  it.each(oneofArms(UpdateHeldPromptErrorSchema, "cause"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const { tc } = trayContext(refusalResponse(arm));
      const card = drawHeldPrompt(heldPrompt(), tc);
      card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
      await settle();
      const refusal = card.querySelector(".queued-refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([arm, false]);
    },
  );

  it("names the registry's directory on a mismatch, from the one shared wording", async () => {
    const { tc } = trayContext(refusalResponse("workspaceRefMismatch"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    expect(card.querySelector(".queued-refusal")?.textContent).toContain("/w/registry");
  });

  it("says an already-delivered prompt has been delivered", async () => {
    const { tc } = trayContext(refusalResponse("alreadyDelivered"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    expect(card.querySelector(".queued-refusal")?.textContent).toContain("already been delivered");
  });

  it("says accept applies only to a turn-end hold", async () => {
    const { tc } = trayContext(refusalResponse("acceptNotApplicable"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    expect(card.querySelector(".queued-refusal")?.textContent).toContain("the turn's end");
  });

  it("re-enables the row after a refusal so it can be retried", async () => {
    const { tc } = trayContext(errorResponse);
    const card = drawHeldPrompt(heldPrompt(), tc);
    const drop = card.querySelector<HTMLButtonElement>('[data-held-action="drop"]');
    drop?.click();
    await settle();
    expect(drop?.disabled).toBe(false);
  });

  it("hands the dropped words back on the bubbling event", async () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    document.body.appendChild(card);
    const seen: string[] = [];
    const listener = (event: Event): void => {
      seen.push((event as CustomEvent<HeldPromptDroppedDetail>).detail.text);
    };
    document.addEventListener(DROPPED_EVENT, listener);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    document.removeEventListener(DROPPED_EVENT, listener);
    card.remove();
    expect(seen).toEqual(["fix the test"]);
  });

  it("hands nothing back when the drop was refused", async () => {
    const { tc } = trayContext(errorResponse);
    const card = drawHeldPrompt(heldPrompt(), tc);
    document.body.appendChild(card);
    const seen: string[] = [];
    const listener = (): void => {
      seen.push("dropped");
    };
    document.addEventListener(DROPPED_EVENT, listener);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    document.removeEventListener(DROPPED_EVENT, listener);
    card.remove();
    expect(seen).toEqual([]);
  });
});

describe("the queued-at age", () => {
  it("draws the age from the served instant", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.querySelector("[data-queued]")?.textContent).toBe("queued 12s ago");
  });

  it("re-paints on the shared ticker rather than a timer of its own", () => {
    const ticker = fakeTicker();
    const { tc } = trayContext(successResponse, [], ticker);
    const card = drawHeldPrompt(heldPrompt(), tc);
    ticker.tick(NOW + 48_000);
    expect(card.querySelector("[data-queued]")?.textContent).toBe("queued 1m ago");
  });

  it("reads the nearest second when a tick samples just short of one", () => {
    // Arrange: the queue instant does not share the shared ticker's phase.
    const ticker = fakeTicker();
    const { tc } = trayContext(successResponse, [], ticker);
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act: a repaint 80ms before the fifth second of the wait it draws.
    ticker.tick(NOW - 12_000 + 4920);
    // Assert: five real seconds queued reads 5s, not the lagging 4s.
    expect(card.querySelector("[data-queued]")?.textContent).toBe("queued 5s ago");
  });

  it("registers its unsubscriber with the tray's teardown", () => {
    const { ctx } = trayContext();
    const disposers: Array<() => void> = [];
    const tc: TrayContext = { ctx, onDispose: (fn) => disposers.push(fn) };
    drawHeldPrompt(heldPrompt(), tc);
    expect(disposers).toHaveLength(1);
  });
});

describe("the said body", () => {
  it("renders text blocks as markdown", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        said: { content: { blocks: [{ block: { case: "text", value: { text: "**bold**" } } }] } },
      }),
      tc,
    );
    expect(card.querySelector(".queued-text")?.innerHTML).toContain("<strong>bold</strong>");
  });

  it("draws a url image as a picture", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        said: {
          content: {
            blocks: [
              {
                block: {
                  case: "image",
                  value: {
                    location: { case: "url", value: { url: "https://example.test/a.png" } },
                    mediaType: "image/png",
                  },
                },
              },
            ],
          },
        },
      }),
      tc,
    );
    expect(card.querySelector<HTMLImageElement>("img.queued-image")?.src).toBe(
      "https://example.test/a.png",
    );
  });

  it("names a host-path image rather than drawing a broken picture", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        said: {
          content: {
            blocks: [
              {
                block: {
                  case: "image",
                  value: {
                    location: { case: "path", value: { path: "/tmp/a.png" } },
                    mediaType: "image/png",
                  },
                },
              },
            ],
          },
        },
      }),
      tc,
    );
    expect(card.querySelector(".queued-image-path")?.textContent).toBe("/tmp/a.png");
  });

  it("draws nothing for an unmodeled block", () => {
    expect(drawUnsupportedBlock({ kind: "video" } as never, "path")).toBeNull();
  });

  it("hands back only the text blocks when a prompt is dropped", async () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        said: {
          content: {
            blocks: [
              { block: { case: "text", value: { text: "one" } } },
              {
                block: {
                  case: "image",
                  value: {
                    location: { case: "url", value: { url: "https://example.test/a.png" } },
                    mediaType: "image/png",
                  },
                },
              },
              { block: { case: "text", value: { text: "two" } } },
            ],
          },
        },
      }),
      tc,
    );
    document.body.appendChild(card);
    const seen: string[] = [];
    const listener = (event: Event): void => {
      seen.push((event as CustomEvent<HeldPromptDroppedDetail>).detail.text);
    };
    document.addEventListener(DROPPED_EVENT, listener);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    await settle();
    document.removeEventListener(DROPPED_EVENT, listener);
    card.remove();
    expect(seen).toEqual(["one\ntwo"]);
  });

  it("refuses a said with no content", () => {
    const { tc } = trayContext();
    const prompt = create(HeldPromptSchema, {
      turn: { value: "t" },
      said: {},
      queuedAt: { atMs: BigInt(NOW) },
      classification: { case: "classifying", value: {} },
    });
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });

  it("refuses a content block whose oneof is unset", () => {
    const { tc } = trayContext();
    const prompt = heldPrompt({ said: { content: { blocks: [{}] } } });
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });
});

/** A held prompt whose words are TEXT. */
const saying = (text: string): HeldPrompt =>
  heldPrompt({ said: { content: { blocks: [{ block: { case: "text", value: { text } } }] } } });

describe("the held prompt's spec: a prompt bubble on the held fill", () => {
  it("is a prompt-role bubble", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(heldPrompt(), tc).getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("prompt");
  });

  it("is the held variant, whose fill is the held grey", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(heldPrompt(), tc).getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("held");
  });

  it("collapses at one line", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(saying("first line\nsecond line\nthird line"), tc).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(
      "1",
    );
  });

  it("signals more with the ellipsis, never the fade", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(saying("first line\nsecond line"), tc).getAttribute(BUBBLE_MORE_ATTRIBUTE)).toBe(
      BUBBLE_MORE_ELLIPSIS,
    );
  });

  it("never waves: a held prompt has no turn in flight", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(heldPrompt(), tc).hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("puts its badges in the header strip", () => {
    const { tc } = trayContext();
    const head = drawHeldPrompt(heldPrompt(), tc).querySelector(".queued-head");
    expect([head?.classList.contains(BUBBLE_STRIP_CLASS), head?.querySelector(".held-badge") !== null]).toEqual([
      true,
      true,
    ]);
  });

  it("puts the whole prompt in the one scroll box, every line of it", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(saying("first line\nsecond line"), tc);
    expect(card.querySelector(".bubble-scroll .queued-text")?.textContent).toContain("second line");
  });

  it("keeps its actions after the scroll box, outside the cap", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.lastElementChild?.querySelector(":scope > .queued-actions")).not.toBeNull();
  });

  it("carries no private fold of its own", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(saying("first line\nsecond line"), tc).querySelector(".held-fold, .held-line")).toBeNull();
  });

  it.each([
    ["no hold", null, []],
    ["a lease hold", { case: "shutdown", value: { scheduleId: "s" } }, ["lease-card"]],
    ["a session-starting hold", { case: "sessionStarting", value: {} }, []],
  ] as const)("names %s by its hook, which selects no border", (_name, hold, frames) => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(hold === null ? {} : { hold: hold as never }), tc);
    expect(["lease-card"].filter((frame) => card.classList.contains(frame))).toEqual(frames);
  });
});

describe("a tree the held prompt carries", () => {
  const staged = useTreeLayout();

  /** A held prompt of TEXT, attached under its own column. */
  function mounted(text: string): HTMLElement {
    const { tc } = trayContext();
    const card = drawHeldPrompt(saying(text), tc);
    const column = document.createElement("div");
    column.append(card);
    document.body.append(column);
    return card;
  }

  it("wraps at the held bubble's own cap", () => {
    // Arrange / Act
    const card = mounted(WIDE_TREE);
    // Assert
    const widths = treeLineWidths(card);
    expect(widths.length).toBeGreaterThan(3);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("never wraps below its max width", () => {
    // Arrange / Act
    const card = mounted(FITTING_TREE);
    // Assert
    expect(treeLineWidths(card)).toHaveLength(3);
  });
});

describe("a held prompt's redraw", () => {
  it("updates the previous card in place, keeping its scroll box", () => {
    // Arrange
    const { tc } = trayContext();
    const first = drawHeldPrompt(heldPrompt(), tc);
    const box = first.querySelector(".bubble-scroll");
    // Act
    const again = drawHeldPrompt(heldPrompt(), tc, first);
    // Assert
    expect([again, again.querySelector(".bubble-scroll")]).toEqual([first, box]);
  });

  it("drops the acceptance mark when the new arm has none", () => {
    // Arrange
    const { tc } = trayContext();
    const first = drawHeldPrompt(
      heldPrompt({ classification: { case: "holdForTurnEnd", value: { rationale: "r" } } }),
      tc,
    );
    // Act
    drawHeldPrompt(heldPrompt({ classification: { case: "classifying", value: {} } }), tc, first);
    // Assert
    expect(first.hasAttribute("data-accepted")).toBe(false);
  });
});

describe("the Cancel control", () => {
  it("labels the drop control Cancel", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.querySelector('[data-held-action="drop"]')?.textContent).toBe("Cancel");
  });

  it("still drops the held prompt when Cancel is clicked", async () => {
    // Arrange
    const seen: UpdateHeldPromptRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldPrompt(heldPrompt(), tc);
    const cancel = [...card.querySelectorAll<HTMLButtonElement>("button")].find(
      (button) => button.textContent === "Cancel",
    );
    // Act
    cancel?.click();
    await settle();
    // Assert
    expect(seen[0]?.action.case).toBe("drop");
  });
});

describe("sessionCommandLiteral", () => {
  it("reads /clear off the enum value's own spec", () => {
    expect(sessionCommandLiteral(SessionCommand.CLEAR, "p")).toBe("/clear");
  });

  it("reads /compact off the enum value's own spec", () => {
    expect(sessionCommandLiteral(SessionCommand.COMPACT, "p")).toBe("/compact");
  });

  it("refuses UNSPECIFIED, which names no command", () => {
    expect(() => sessionCommandLiteral(SessionCommand.UNSPECIFIED, "p")).toThrow(MalformedView);
  });

  it("refuses a number no session command carries", () => {
    expect(() => sessionCommandLiteral(9999 as SessionCommand, "p")).toThrow(MalformedView);
  });
});

describe("in-flight", () => {
  it("disables the whole row while an action is in flight", () => {
    const { tc } = trayContext(() => {
      // Never answers within the test: the row must already be disabled.
      return new Promise<never>(() => undefined) as never;
    });
    const card = drawHeldPrompt(heldPrompt(), tc);
    card.querySelector<HTMLButtonElement>('[data-held-action="drop"]')?.click();
    const disabled = [...card.querySelectorAll("button")].every((b) => b.disabled);
    expect(disabled).toBe(true);
  });
});

describe("the tray's own logging", () => {
  it("does not reach for a timer of its own", () => {
    const spy = vi.spyOn(globalThis, "setInterval");
    const { tc } = trayContext();
    drawHeldPrompt(heldPrompt(), tc);
    expect(spy).not.toHaveBeenCalled();
    spy.mockRestore();
  });
});

/**
 * THE STATUS BADGES (owner spec, 2026-09-23): every status a held card can show
 * is a `.badge` whose color class comes from ONE table. The owner's anchors are
 * WAITING red and INTERRUPTING green; the rest are listed here for the owner to
 * confirm, and the table in src/tray/held-prompt.ts must match them row for row.
 */
const EXPECTED_BADGES: Readonly<Record<HeldStatus, string>> = {
  classifying: "run",
  interject: "ok",
  afterToolCall: "ok",
  holdForTurnEnd: "err",
  uninterruptibleTurn: "err",
  classificationError: "err",
  accepted: "muted",
  shutdown: "amber",
  buildRefresh: "amber",
  sessionStarting: "teal",
  editing: "run",
  coalesced: "muted",
};

describe("the daemon's badge words", () => {
  const byStatus: Array<[HeldStatus, () => HeldPrompt]> = [
    ["classifying", () => heldPrompt({ classification: { case: "classifying", value: {} } })],
    ["interject", () => heldPrompt({ classification: { case: "interject", value: { rationale: "" } } })],
    ["afterToolCall", () => heldPrompt({ classification: { case: "afterToolCall", value: { rationale: "" } } })],
    ["holdForTurnEnd", () => heldPrompt({ classification: { case: "holdForTurnEnd", value: { rationale: "" } } })],
    [
      "uninterruptibleTurn",
      () => heldPrompt({ classification: { case: "uninterruptibleTurn", value: { command: SessionCommand.COMPACT } } }),
    ],
    ["classificationError", () => heldPrompt({ classification: { case: "classificationError", value: { detail: "" } } })],
    [
      "accepted",
      () => heldPrompt({ classification: { case: "holdForTurnEnd", value: { accepted: { accepted: true } } } }),
    ],
    ["shutdown", () => heldPrompt({ hold: { case: "shutdown", value: { scheduleId: "s" } } })],
    ["buildRefresh", () => heldPrompt({ hold: { case: "buildRefresh", value: {} } })],
    ["sessionStarting", () => heldPrompt({ hold: { case: "sessionStarting", value: {} } })],
    [
      "editing",
      () => {
        const u = heldPrompt();
        markEditing(u);
        return u;
      },
    ],
    [
      "coalesced",
      () => {
        const u = heldPrompt();
        markCoalesced(u);
        return u;
      },
    ],
  ];

  afterEach(() => {
    resetLoggingForTests();
  });

  it.each(byStatus)("draws the %s label verbatim on its badge", (status, prompt) => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(prompt(), tc);
    // Assert
    expect(card.querySelector(`.queued-head > [data-held-status="${status}"]`)?.textContent).toBe(wireLabel(status));
  });

  it.each(byStatus)("draws the %s detail verbatim in the expand-only details", (status, prompt) => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(prompt(), tc);
    // Assert
    expect(
      card.querySelector(`.queued-details > .${HELD_BADGE_DETAIL_CLASS}[data-held-status="${status}"]`)?.textContent,
    ).toBe(wireDetail(status));
  });

  it("draws no detail element for a badge the daemon gave none", () => {
    // Arrange
    const { tc } = trayContext();
    const prompt = heldPrompt({ badges: [{ label: "after this turn" }], classification: { case: "holdForTurnEnd", value: {} } });
    // Act
    const card = drawHeldPrompt(prompt, tc);
    // Assert
    expect(card.querySelector(`.${HELD_BADGE_DETAIL_CLASS}`)).toBeNull();
  });

  it("refuses a badge with an empty label", () => {
    // Arrange
    const { tc } = trayContext();
    const prompt = heldPrompt({ badges: [{ label: "" }] });
    // Act / Assert
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });

  it("logs an empty label as an error through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { tc } = trayContext();
    // Act
    expect(() => drawHeldPrompt(heldPrompt({ badges: [{ label: "" }] }), tc)).toThrow(MalformedView);
    // Assert
    const record = await forwardedRecord(capture, "tray.held-prompt.badge-empty-label");
    expect(record.level.case).toBe("error");
  });

  it("refuses fewer badges than the standing facts", () => {
    // Arrange
    const { tc } = trayContext();
    const prompt = heldPrompt({
      badges: [{ label: "after this turn" }],
      classification: { case: "holdForTurnEnd", value: {} },
      hold: { case: "buildRefresh", value: {} },
    });
    // Act / Assert
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });

  it("refuses more badges than the standing facts", () => {
    // Arrange
    const { tc } = trayContext();
    const prompt = heldPrompt({ badges: [{ label: "classifying" }, { label: "restart hold" }] });
    // Act / Assert
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });

  it("logs a badge count mismatch as an error through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { tc } = trayContext();
    // Act
    expect(() => drawHeldPrompt(heldPrompt({ badges: [] }), tc)).toThrow(MalformedView);
    // Assert
    const record = await forwardedRecord(capture, "tray.held-prompt.badges-mismatch");
    expect(record.level.case).toBe("error");
  });

  it("composes no status sentence of its own", () => {
    // Arrange: every sentence the card used to compose for a badge.
    const composed = [
      '"queued — classifying"',
      '"interjects"',
      '"after this turn"',
      '"waits for "',
      '" to finish"',
      '"unclassified"',
      '"confirmed"',
      "held for the scheduled restart",
      "held until the session is up",
      "held for the build refresh",
    ];
    // Act
    const source = heldPromptSource.replace(/\/\*[\s\S]*?\*\//g, "").replace(/\/\/.*$/gm, "");
    // Assert
    expect(composed.filter((sentence) => source.includes(sentence))).toEqual([]);
  });
});

describe("the held status badge table", () => {
  afterEach(() => {
    resetLoggingForTests();
  });

  it.each(Object.entries(EXPECTED_BADGES))("badges %s in the %s tone", (status, tone) => {
    // Arrange / Act
    const classes = heldBadgeClasses(status);
    // Assert
    expect(classes).toBe(`badge held-badge ${tone}`);
  });

  it("names every status the owner's table names, and no other", () => {
    // Arrange / Act
    const named = Object.keys(HELD_STATUS_BADGES).sort();
    // Assert
    expect(named).toEqual(Object.keys(EXPECTED_BADGES).sort());
  });

  it("names every classification arm the schema can send", () => {
    // Arrange / Act
    const arms = oneofArms(HeldPromptSchema, "classification");
    // Assert
    expect(arms.filter((arm) => !Object.hasOwn(HELD_STATUS_BADGES, arm))).toEqual([]);
  });

  it("names every hold arm the schema can send", () => {
    // Arrange / Act
    const arms = oneofArms(HeldPromptSchema, "hold");
    // Assert
    expect(arms.filter((arm) => !Object.hasOwn(HELD_STATUS_BADGES, arm))).toEqual([]);
  });

  it("refuses a status the table does not name", () => {
    // Arrange / Act / Assert
    expect(() => heldBadgeClasses("someNewArm")).toThrow(MalformedView);
  });

  it("refuses a name the table only inherits", () => {
    // Arrange / Act / Assert
    expect(() => heldBadgeClasses("toString")).toThrow(MalformedView);
  });

  it("logs the refused status as an error through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => heldBadgeClasses("someNewArm")).toThrow(MalformedView);
    // Assert
    const record = await forwardedRecord(capture, "tray.held-prompt.badge-unknown-status");
    expect([record.level.case, (record.context as Record<string, unknown>).status]).toEqual(["error", "someNewArm"]);
  });
});

describe("every status a held card shows is a badge in the table's tone", () => {
  const cards: Array<[HeldStatus, () => HeldPrompt]> = [
    ["classifying", () => heldPrompt({ classification: { case: "classifying", value: {} } })],
    ["interject", () => heldPrompt({ classification: { case: "interject", value: { rationale: "now" } } })],
    ["afterToolCall", () => heldPrompt({ classification: { case: "afterToolCall", value: { rationale: "now" } } })],
    ["holdForTurnEnd", () => heldPrompt({ classification: { case: "holdForTurnEnd", value: { rationale: "r" } } })],
    [
      "uninterruptibleTurn",
      () => heldPrompt({ classification: { case: "uninterruptibleTurn", value: { command: SessionCommand.CLEAR } } }),
    ],
    [
      "classificationError",
      () => heldPrompt({ classification: { case: "classificationError", value: { detail: "d" } } }),
    ],
    [
      "accepted",
      () =>
        heldPrompt({
          classification: { case: "holdForTurnEnd", value: { rationale: "r", accepted: { accepted: true } } },
        }),
    ],
    ["shutdown", () => heldPrompt({ hold: { case: "shutdown", value: { scheduleId: "s" } } })],
    ["buildRefresh", () => heldPrompt({ hold: { case: "buildRefresh", value: {} } })],
    ["sessionStarting", () => heldPrompt({ hold: { case: "sessionStarting", value: {} } })],
    [
      "editing",
      () => {
        const u = heldPrompt();
        markEditing(u);
        return u;
      },
    ],
  ];

  it.each(cards)("draws the %s status as a badge in the header strip", (status, prompt) => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(prompt(), tc);
    // Assert
    expect(card.querySelector(`.queued-head > [data-held-status="${status}"]`)?.className).toBe(
      `badge held-badge ${EXPECTED_BADGES[status]}`,
    );
  });

  it("draws no acceptance badge on a hold the user has not confirmed", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(
      heldPrompt({ classification: { case: "holdForTurnEnd", value: { rationale: "r" } } }),
      tc,
    );
    // Assert
    expect(card.querySelector('[data-held-status="accepted"]')).toBeNull();
  });
});

/** The stylesheet's selector for an expand-only element the toggle has not opened. */
const HIDDEN_WHILE_COLLAPSED = (() => {
  const css = stylesheet.replace(/\/\*[\s\S]*?\*\//g, "");
  const found = /([^{}]*\.bubble-expand-only)\s*\{\s*display:\s*none;\s*\}/.exec(css)?.[1]?.trim();
  if (found === undefined) throw new Error("the stylesheet hides no expand-only region");
  return found;
})();

describe("a held prompt collapsed and expanded", () => {
  /** A held prompt carrying every expand-only part, mounted under the one toggle. */
  function mountedFull(): HTMLElement {
    const { tc } = trayContext();
    const card = drawHeldPrompt(
      heldPrompt({
        classification: { case: "holdForTurnEnd", value: { rationale: "it can wait" } },
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

  const EXPAND_ONLY: Array<[string, string]> = [
    ["the queued age", "[data-queued]"],
    ["the rationale", ".queued-reason"],
    ["the Send now button", '[data-held-action="release"]'],
    ["the Cancel button", '[data-held-action="drop"]'],
    ["the Accept button", '[data-held-action="accept"]'],
  ];

  it("holds nothing but badges in its collapsed strip", () => {
    // Arrange / Act
    const head = mountedFull().querySelector(".queued-head");
    // Assert
    expect([...(head?.children ?? [])].map((child) => child.classList.contains("held-badge"))).toEqual([
      true,
      true,
    ]);
  });

  it("keeps its badges visible while collapsed", () => {
    // Arrange / Act
    const badges = [...mountedFull().querySelectorAll(".held-badge")];
    // Assert
    expect(badges.map((badge) => badge.closest(HIDDEN_WHILE_COLLAPSED))).toEqual([null, null]);
  });

  it("puts the details in the bubble's one expand-only region", () => {
    // Arrange / Act
    const card = mountedFull();
    // Assert
    expect(card.querySelector(`:scope > .${BUBBLE_EXPAND_ONLY_CLASS}`)?.classList.contains("queued-details")).toBe(
      true,
    );
  });

  it.each(EXPAND_ONLY)("hides %s while collapsed", (_name, selector) => {
    // Arrange / Act
    const part = mountedFull().querySelector(selector);
    // Assert
    expect(part?.closest(HIDDEN_WHILE_COLLAPSED)).not.toBeNull();
  });

  it.each(EXPAND_ONLY)("shows %s once the bubble is expanded", (_name, selector) => {
    // Arrange
    const card = mountedFull();
    // Act
    card.querySelector<HTMLElement>(".queued-head")?.click();
    // Assert
    expect(card.querySelector(selector)?.closest(HIDDEN_WHILE_COLLAPSED)).toBeNull();
  });

  it("hides the details again once the bubble is collapsed", () => {
    // Arrange
    const card = mountedFull();
    const head = card.querySelector<HTMLElement>(".queued-head");
    head?.click();
    // Act
    head?.click();
    // Assert
    expect(card.querySelector(".queued-details")?.closest(HIDDEN_WHILE_COLLAPSED)).not.toBeNull();
  });

  it("draws a refused action's sentence inside the expand-only region", async () => {
    // Arrange
    const { tc } = trayContext(errorResponse);
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="release"]')?.click();
    await settle();
    // Assert
    expect(card.querySelector(".queued-refusal")?.parentElement?.classList.contains("queued-details")).toBe(true);
  });
});

describe("a tree the held prompt carries, at the held bubble's halved width", () => {
  let uninstall: (() => void) | null = null;
  afterEach(() => {
    uninstall?.();
    uninstall = null;
    document.body.replaceChildren();
  });

  /** A held prompt of TEXT under a column, its max-width the browser's half of 77%. */
  function mountedHalved(text: string): { card: HTMLElement; cols: number } {
    const installed = installTreeLayout({ maxWidth: "calc(38.5%)" });
    uninstall = installed.uninstall;
    const { tc } = trayContext();
    const card = drawHeldPrompt(saying(text), tc);
    const column = document.createElement("div");
    column.append(card);
    document.body.append(column);
    return { card, cols: stagedCols({ ...installed.layout, maxWidth: "38.5%" }) };
  }

  it("wraps at half the bubble cap", () => {
    // Arrange / Act
    const { card, cols } = mountedHalved(WIDE_TREE);
    // Assert
    expect(Math.max(...treeLineWidths(card))).toBeLessThanOrEqual(cols);
  });

  it("wraps a tree that would fit a full-width bubble", () => {
    // Arrange / Act
    const { card } = mountedHalved(FITTING_TREE);
    // Assert
    expect(treeLineWidths(card).length).toBeGreaterThan(3);
  });
});

/** A context whose EditHeldPrompt runs ANSWER, recording requests and chip filings. */
function editContext(answer: () => unknown = () =>
  create(EditHeldPromptResponseSchema, { result: { case: "success", value: {} } }),
): { tc: TrayContext; seen: EditHeldPromptRequest[]; reported: FailureKind[] } {
  const seen: EditHeldPromptRequest[] = [];
  const reported: FailureKind[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      editHeldPrompt: (request) => {
        seen.push(request);
        return answer() as never;
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: fakeTicker(),
    failures: { report: (kind) => reported.push(kind), retract: () => undefined },
    composerEnabled: false,
  });
  return { tc: { ctx, onDispose: () => undefined }, seen, reported };
}

/** An EditHeldPrompt refusal carrying ARM and its payload. */
const editRefusal = (arm: string, value: Record<string, unknown> = {}) => () =>
  create(EditHeldPromptResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value } } },
  } as never);

/** The chip's filing, read as the request it names and the cause it states. */
function filed(kind: FailureKind | undefined): [string, string] | undefined {
  return kind?.kind.case === "controlPlaneFailed"
    ? [kind.kind.value.what, kind.kind.value.cause]
    : undefined;
}

describe("the Edit control", () => {
  it("sits between Send now and Cancel", () => {
    // Arrange
    const { tc } = editContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    const labels = [...card.querySelectorAll(".queued-actions button")].map((b) => b.textContent);
    expect(labels.slice(0, 3)).toEqual(["Send now", "Edit", "Cancel"]);
  });

  it("begins an edit of this card's turn", async () => {
    // Arrange
    const { tc, seen } = editContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    expect([seen[0]?.turn?.value, seen[0]?.action.case]).toEqual(["turn-1", "begin"]);
  });

  it("files a refused begin on the warning chip", async () => {
    // Arrange
    const { tc, reported } = editContext(editRefusal("alreadyDelivered"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    expect(filed(reported[0])).toEqual([EDIT_REQUEST, "this prompt has already been delivered"]);
  });

  it("names the prompt already being edited on a being-edited refusal", async () => {
    // Arrange
    const { tc, reported } = editContext(editRefusal("beingEdited", { editingTurn: { value: "turn-0" } }));
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    expect(filed(reported[0])?.[1]).toBe("another held prompt is already being edited (turn turn-0)");
  });

  it("draws no refusal at the row: the chip is the one error surface", async () => {
    // Arrange
    const { tc } = editContext(editRefusal("notHeld"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    expect(card.querySelector(".queued-refusal")).toBeNull();
  });

  it("logs a refused begin at info with its arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { tc } = editContext(editRefusal("noSuchHold"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "tray.held-prompt.edit-refused");
    expect([record.level.case, record.context?.arm]).toEqual(["info", "noSuchHold"]);
  });

  it("files a begin that failed at the transport on the warning chip", async () => {
    // Arrange
    const { tc, reported } = editContext(() => {
      throw new ConnectError("connection refused", Code.Unavailable);
    });
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    expect(filed(reported[0])?.[0]).toBe(EDIT_REQUEST);
  });

  it("logs a begin that failed at the transport at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { tc } = editContext(() => {
      throw new ConnectError("connection refused", Code.Unavailable);
    });
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "tray.held-prompt.edit-failed");
    expect(record.level.case).toBe("error");
  });

  it("re-enables the row once the begin is answered", async () => {
    // Arrange
    const { tc } = editContext(editRefusal("notHeld"));
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="edit"]')?.click();
    await settle();
    // Assert
    expect([...card.querySelectorAll(".queued-actions button")].some((b) => (b as HTMLButtonElement).disabled)).toBe(false);
  });
});

describe("the editing badge", () => {
  it("draws the editing badge while the daemon says the prompt is being edited", () => {
    // Arrange
    const { tc } = editContext();
    const u = heldPrompt();
    markEditing(u);
    // Act
    const card = drawHeldPrompt(u, tc);
    // Assert
    expect(card.querySelector('[data-held-status="editing"]')?.textContent).toBe("wire editing");
  });

  it("draws no editing badge when the daemon says nothing", () => {
    // Arrange
    const { tc } = editContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.querySelector('[data-held-status="editing"]')).toBeNull();
  });

  it("states the editing standing on the card", () => {
    // Arrange
    const { tc } = editContext();
    const u = heldPrompt();
    markEditing(u);
    // Act
    const card = drawHeldPrompt(u, tc);
    // Assert
    expect(card.getAttribute("data-editing")).toBe("true");
  });

  it("drops the editing standing when a redraw no longer carries it", () => {
    // Arrange
    const { tc } = editContext();
    const u = heldPrompt();
    markEditing(u);
    const first = drawHeldPrompt(u, tc);
    // Act
    const second = drawHeldPrompt(heldPrompt(), tc, first);
    // Assert
    expect([second.hasAttribute("data-editing"), second.querySelector('[data-held-status="editing"]')]).toEqual([false, null]);
  });
});

describe("the retired Release label", () => {
  /** Every file under DIR whose name ends with SUFFIX, recursively. */
  function filesUnder(dir: string, suffix: string): string[] {
    return readdirSync(dir).flatMap((name) => {
      const path = join(dir, name);
      if (statSync(path).isDirectory()) return filesUnder(path, suffix);
      return path.endsWith(suffix) ? [path] : [];
    });
  }

  /** The files among PATHS holding a string literal that is exactly "Release". */
  function holdingTheLabel(paths: string[]): string[] {
    return paths.filter((path) => /(["'`])Release\1/.test(readFileSync(path, "utf8")));
  }

  it("is drawn by no webapp source", () => {
    // Arrange
    const sources = filesUnder(join(process.cwd(), "src"), ".ts");
    // Act, Assert
    expect(holdingTheLabel(sources)).toEqual([]);
  });

  it("is labelled by no Emacs source", () => {
    // Arrange — the suites are excluded: they are not labels.
    const lisp = join(process.cwd(), "..", "lisp");
    const sources = readdirSync(lisp)
      .filter((name) => name.endsWith(".el") && !name.startsWith("test-"))
      .map((name) => join(lisp, name));
    // Act, Assert
    expect([sources.length > 0, holdingTheLabel(sources)]).toEqual([true, []]);
  });
});

/**
 * THE KEEP-ALIVE HOLD IS RETIRED. The shim makes a prompt that arrives during
 * its own keep-alive wait inside the shim, so the daemon never holds one behind
 * a keep-alive and the tray draws no such hold, badge, tone or reason.
 */
describe("the retired keep-alive hold", () => {
  it("draws no keep-alive-card hook on a prompt whose hold is unset", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.classList.contains("keep-alive-card")).toBe(false);
  });

  it("draws no keep-alive status badge on a prompt whose hold is unset", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.querySelector('[data-held-status="keepAlive"]')).toBeNull();
  });

  it("exports no keep-alive hold drawer", () => {
    // Arrange / Act
    const exported = Object.keys(heldPromptModule);
    // Assert
    expect(exported).not.toContain("drawHeldPromptKeepAliveHold");
  });

  it("gives no keep-alive status a badge tone", () => {
    // Arrange / Act
    const statuses = Object.keys(HELD_STATUS_BADGES);
    // Assert
    expect(statuses).not.toContain("keepAlive");
  });

  it("states no keep-alive reason for a withheld release", () => {
    // Arrange / Act
    const arms = Object.keys(heldPromptModule.NO_RELEASE_TITLES);
    // Assert
    expect(arms).not.toContain("keepAlive");
  });
});

/** The selector of the stylesheet rule PATTERN captures, or a loud failure naming WHAT. */
function selectorOf(pattern: RegExp, what: string): string {
  const css = stylesheet.replace(/\/\*[\s\S]*?\*\//g, "");
  const found = pattern.exec(css)?.[1]?.trim();
  if (found === undefined) throw new Error(`the stylesheet has no ${what}`);
  return found;
}

/** The ellipsis clamp on a collapsed body, as the stylesheet writes it. */
const CLAMPED_BODY = selectorOf(/([^{}]*\.bubble-body)\s*\{[^{}]*-webkit-line-clamp/, "ellipsis clamp");

/** The box whose fade the ellipsis hides, as the stylesheet writes it (the pseudo dropped). */
const FADE_HIDDEN_ON = selectorOf(/([^{}]*\.bubble-scroll)::after\s*\{\s*display:\s*none;\s*\}/, "hidden fade");

/**
 * THE HELD PROMPT'S ONE LINE (owner ruling, 2026-09-27). Collapsed, it shows its
 * first line, ending in the ellipsis the stylesheet's clamp writes when anything
 * follows it, and no fade; expanded, everything. jsdom lays nothing out, so
 * each case states the geometry the engine would give it: the clamp holds the
 * body's own box at one line, and its rendered lines run LINES deep.
 */
describe("a held prompt's one collapsed line", () => {
  const LINE_PX = 21;

  /** A held prompt saying TEXT, laid out as LINES rendered lines, mounted under the one toggle. */
  function mounted(text: string, lines: number): { card: HTMLElement; box: HTMLElement; body: HTMLElement } {
    const { tc } = trayContext();
    const card = drawHeldPrompt(saying(text), tc);
    const host = document.createElement("div");
    installClickExpand(host, () => "", (section) => refreshHasMore(section));
    host.append(card);
    document.body.append(host);
    const box = card.querySelector<HTMLElement>(":scope > .bubble-scroll");
    const body = box?.querySelector<HTMLElement>(":scope > .bubble-body");
    if (box === null || box === undefined || body === null || body === undefined) throw new Error("fixture: no box");
    Object.defineProperty(box, "clientHeight", { configurable: true, value: LINE_PX });
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: LINE_PX });
    Object.defineProperty(body, "scrollHeight", { configurable: true, value: lines * LINE_PX });
    refreshHasMore(box);
    return { card, box, body };
  }

  /** Whether the collapsed line ends in the ellipsis: the clamp holds it, and something follows it. */
  const ellipsized = (box: HTMLElement, body: HTMLElement): boolean =>
    body.matches(CLAMPED_BODY) && box.classList.contains(HAS_MORE_CLASS);

  it("clamps its collapsed body to one line", () => {
    // Arrange / Act
    const { body } = mounted("only line", 1);
    // Assert
    expect(body.matches(CLAMPED_BODY)).toBe(true);
  });

  it("ends the line in an ellipsis when further lines follow", () => {
    // Arrange / Act
    const { box, body } = mounted("first line\nsecond line\nthird line", 3);
    // Assert
    expect(ellipsized(box, body)).toBe(true);
  });

  it("ends the line in an ellipsis when the one line is too long to fit", () => {
    // Arrange / Act — one over-long line, which wraps once into the clamp's hidden second line.
    const { box, body } = mounted("word ".repeat(80), 2);
    // Assert
    expect(ellipsized(box, body)).toBe(true);
  });

  it("ends the line in nothing when it is the whole prompt", () => {
    // Arrange / Act
    const { box, body } = mounted("only line", 1);
    // Assert
    expect(ellipsized(box, body)).toBe(false);
  });

  it("draws no fade, even with more to show", () => {
    // Arrange / Act
    const { box } = mounted("first line\nsecond line", 2);
    // Assert
    expect([box.classList.contains(HAS_MORE_CLASS), box.matches(FADE_HIDDEN_ON)]).toEqual([true, true]);
  });

  it("keeps its badges shown beside the one line", () => {
    // Arrange / Act
    const { card } = mounted("first line\nsecond line", 2);
    // Assert
    expect(card.querySelector(".held-badge")?.closest(HIDDEN_WHILE_COLLAPSED)).toBeNull();
  });

  it("lifts the clamp once expanded, showing everything", () => {
    // Arrange
    const { card, body } = mounted("first line\nsecond line\nthird line", 3);
    // Act
    card.querySelector<HTMLElement>(".queued-head")?.click();
    // Assert
    expect(body.matches(CLAMPED_BODY)).toBe(false);
  });

  it("drops the more signal once expanded", () => {
    // Arrange
    const { card, box } = mounted("first line\nsecond line\nthird line", 3);
    // Act
    card.querySelector<HTMLElement>(".queued-head")?.click();
    // Assert
    expect(box.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("clamps to the one line again once collapsed", () => {
    // Arrange
    const { card, box, body } = mounted("first line\nsecond line\nthird line", 3);
    const head = card.querySelector<HTMLElement>(".queued-head");
    head?.click();
    // Act
    head?.click();
    // Assert
    expect(ellipsized(box, body)).toBe(true);
  });
});

describe("a held session act", () => {
  it.each([
    ["model", { case: "model" as const, value: { model: "claude-opus-5-5" } }, "model → claude-opus-5-5"],
    ["permissionMode", { case: "permissionMode" as const, value: { mode: "plan" } }, "permission mode → plan"],
  ])("draws a %s change as what it does", (arm, act, text) => {
    // Arrange
    const { tc } = trayContext();
    const u = heldPrompt();
    u.act = create(HeldSessionActSchema, { act });
    // Act
    const card = drawHeldPrompt(u, tc);
    // Assert
    expect(card.querySelector(".queued-act")?.textContent).toBe(text);
    expect(card.getAttribute("data-act")).toBe(arm);
  });

  it("draws a prompt with no act attribute", () => {
    // Arrange
    const { tc } = trayContext();
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.hasAttribute("data-act")).toBe(false);
  });

  it("refuses an act that sets no arm", () => {
    // Arrange
    const { tc } = trayContext();
    const u = heldPrompt();
    u.act = create(HeldSessionActSchema, {});
    // Act / Assert
    expect(() => drawHeldPrompt(u, tc)).toThrow(MalformedView);
  });
});

describe("a coalesced hold", () => {
  it("marks the card coalesced", () => {
    // Arrange
    const { tc } = trayContext();
    const u = heldPrompt();
    markCoalesced(u);
    // Act
    const card = drawHeldPrompt(u, tc);
    // Assert
    expect(card.getAttribute("data-coalesced")).toBe("true");
  });
});

/** A context whose FoldHeldPrompt answers with ANSWER and records requests. */
function foldContext(
  answer: () => ReturnType<typeof foldSuccess>,
  seen: FoldHeldPromptRequest[] = [],
): { tc: TrayContext; seen: FoldHeldPromptRequest[] } {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      foldHeldPrompt: (request) => {
        seen.push(request);
        return answer();
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: fakeTicker(),
    failures: SINK,
    composerEnabled: false,
  });
  return { tc: { ctx, onDispose: () => undefined }, seen };
}

const foldSuccess = () =>
  create(FoldHeldPromptResponseSchema, { result: { case: "success", value: {} } });

/** A fold refusal carrying ARM. */
const foldRefusal = (arm: string) => () =>
  create(FoldHeldPromptResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } } },
  } as never);

/** A held prompt the daemon offers to fold into the entry under ABOVE. */
function foldable(above = "turn-0"): HeldPrompt {
  const u = heldPrompt();
  u.foldAbove = create(HeldPromptFoldAboveSchema, { above: { value: above } });
  return u;
}

describe("the fold above control", () => {
  it("is drawn, labelled fold above, when the daemon offers a fold", () => {
    // Arrange
    const { tc } = foldContext(foldSuccess);
    // Act
    const card = drawHeldPrompt(foldable(), tc);
    // Assert
    expect(card.querySelector('[data-held-action="fold"]')?.textContent).toBe(FOLD_ABOVE_LABEL);
  });

  it("is not drawn when the daemon offers no fold", () => {
    // Arrange
    const { tc } = foldContext(foldSuccess);
    // Act
    const card = drawHeldPrompt(heldPrompt(), tc);
    // Assert
    expect(card.querySelector('[data-held-action="fold"]')).toBeNull();
  });

  it("is a button of the held row's own kind, last in the actions row", () => {
    // Arrange
    const { tc } = foldContext(foldSuccess);
    // Act
    const card = drawHeldPrompt(foldable(), tc);
    // Assert
    const button = card.querySelector<HTMLButtonElement>('[data-held-action="fold"]');
    expect([button?.tagName, button?.type, button?.className, button?.parentElement?.lastElementChild === button]).toEqual([
      "BUTTON",
      "button",
      "queued-action queued-action-fold",
      true,
    ]);
  });

  it("refuses a served fold that names no entry ahead", () => {
    // Arrange
    const { tc } = foldContext(foldSuccess);
    const u = heldPrompt();
    u.foldAbove = create(HeldPromptFoldAboveSchema, {});
    // Act / Assert
    expect(() => drawHeldPrompt(u, tc)).toThrow(MalformedView);
  });

  it("echoes the entry's TurnId, the served entry ahead and the workspace", async () => {
    // Arrange
    const seen: FoldHeldPromptRequest[] = [];
    const { tc } = foldContext(foldSuccess, seen);
    const card = drawHeldPrompt(foldable("turn-ahead"), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="fold"]')?.click();
    await settle();
    // Assert
    expect([seen[0]?.turn?.value, seen[0]?.above?.value, seen[0]?.workspace?.id]).toEqual([
      "turn-1",
      "turn-ahead",
      "ws-1",
    ]);
  });

  it("leaves the row alone on success: the tray's push redraws it", async () => {
    // Arrange
    const { tc } = foldContext(foldSuccess);
    const card = drawHeldPrompt(foldable(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="fold"]')?.click();
    await settle();
    // Assert
    expect(card.querySelector(".queued-refusal")).toBeNull();
  });

  it.each(oneofArms(FoldHeldPromptErrorSchema, "cause"))(
    "draws the %s refusal at the row, saying something about it",
    async (arm) => {
      // Arrange
      const { tc } = foldContext(foldRefusal(arm));
      const card = drawHeldPrompt(foldable(), tc);
      // Act
      card.querySelector<HTMLButtonElement>('[data-held-action="fold"]')?.click();
      await settle();
      // Assert
      const refusal = card.querySelector(".queued-refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([arm, false]);
    },
  );

  it("names the edited entry on a being-edited refusal", async () => {
    // Arrange
    const { tc } = foldContext(() =>
      create(FoldHeldPromptResponseSchema, {
        result: {
          case: "error",
          value: { cause: { case: "beingEdited", value: { editingTurn: { value: "turn-0" } } } },
        },
      }),
    );
    const card = drawHeldPrompt(foldable(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="fold"]')?.click();
    await settle();
    // Assert
    expect(card.querySelector(".queued-refusal")?.textContent).toContain("turn-0");
  });

  it("logs a refusal through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { tc } = foldContext(foldRefusal("aboveMoved"));
    const card = drawHeldPrompt(foldable(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="fold"]')?.click();
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "tray.held-prompt.action-refused");
    expect(record.level.case).toBe("warn");
  });

  it("re-enables the row after a refusal so it can be retried", async () => {
    // Arrange
    const { tc } = foldContext(foldRefusal("aboveMoved"));
    const card = drawHeldPrompt(foldable(), tc);
    const button = card.querySelector<HTMLButtonElement>('[data-held-action="fold"]');
    // Act
    button?.click();
    await settle();
    // Assert
    expect(button?.disabled).toBe(false);
  });

  it("says the daemon could not be reached when the call never lands", async () => {
    // Arrange
    const { tc } = foldContext(() => {
      throw new ConnectError("down", Code.Unavailable);
    });
    const card = drawHeldPrompt(foldable(), tc);
    // Act
    card.querySelector<HTMLButtonElement>('[data-held-action="fold"]')?.click();
    await settle();
    // Assert
    expect(card.querySelector(".queued-refusal")?.getAttribute("data-arm")).toBe("error");
  });
});
