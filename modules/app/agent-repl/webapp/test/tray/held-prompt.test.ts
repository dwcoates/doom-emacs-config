// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  UpdateHeldPromptErrorSchema,
  UpdateHeldPromptResponseSchema,
  type UpdateHeldPromptRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import { oneofArms } from "../arms.js";
import {
  HeldPromptBadgeSchema,
  HeldPromptSchema,
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
  HELD_BADGE_DETAIL_CLASS,
  HELD_STATUS_BADGES,
  drawHeldPrompt,
  drawUnsupportedBlock,
  heldBadgeClasses,
  sessionCommandLiteral,
  type HeldPromptDroppedDetail,
  type HeldStatus,
} from "../../src/tray/held-prompt.js";
import type { TrayContext } from "../../src/tray/context.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_EXPAND_ONLY_CLASS,
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
      name: "keep_alive",
      prompt: heldPrompt({ hold: { case: "keepAlive", value: { turn: { value: "ka-1" } } } }),
      arm: "keepAlive",
      line: "wire keepAlive",
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

  it("composes no title of its own on a keep-alive hold's badge", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt({ hold: { case: "keepAlive", value: { turn: { value: "ka-1" } } } }), tc);
    expect(card.querySelector<HTMLElement>('[data-held-status="keepAlive"]')?.title).toBe("");
  });

  it("refuses a keep-alive hold whose turn is unset", () => {
    const { tc } = trayContext();
    const prompt = heldPrompt({ hold: { case: "keepAlive", value: {} } });
    expect(() => drawHeldPrompt(prompt, tc)).toThrow(MalformedView);
  });
});

describe("drawHeldPrompt release availability", () => {
  it("offers release in the ordinary case", () => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(), tc);
    expect(card.querySelector('[data-held-action="release"]')).not.toBeNull();
  });

  const forbidden: Array<{ name: string; prompt: HeldPrompt }> = [
    {
      name: "uninterruptible_turn",
      prompt: heldPrompt({
        classification: { case: "uninterruptibleTurn", value: { command: SessionCommand.CLEAR } },
      }),
    },
    {
      name: "keep_alive",
      prompt: heldPrompt({ hold: { case: "keepAlive", value: { turn: { value: "ka" } } } }),
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

  it("collapses at two lines", () => {
    const { tc } = trayContext();
    expect(drawHeldPrompt(saying("first line\nsecond line\nthird line"), tc).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(
      "2",
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
    ["a keep-alive hold", { case: "keepAlive", value: { turn: { value: "ka-1" } } }, ["keep-alive-card"]],
  ] as const)("names %s by its hook, which selects no border", (_name, hold, frames) => {
    const { tc } = trayContext();
    const card = drawHeldPrompt(heldPrompt(hold === null ? {} : { hold: hold as never }), tc);
    expect(["lease-card", "keep-alive-card"].filter((frame) => card.classList.contains(frame))).toEqual(frames);
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
  holdForTurnEnd: "err",
  uninterruptibleTurn: "err",
  classificationError: "err",
  accepted: "muted",
  shutdown: "amber",
  buildRefresh: "amber",
  keepAlive: "teal",
  sessionStarting: "teal",
};

describe("the daemon's badge words", () => {
  const byStatus: Array<[HeldStatus, () => HeldPrompt]> = [
    ["classifying", () => heldPrompt({ classification: { case: "classifying", value: {} } })],
    ["interject", () => heldPrompt({ classification: { case: "interject", value: { rationale: "" } } })],
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
    ["keepAlive", () => heldPrompt({ hold: { case: "keepAlive", value: { turn: { value: "ka" } } } })],
    ["sessionStarting", () => heldPrompt({ hold: { case: "sessionStarting", value: {} } })],
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
      "held behind a keep-alive",
      "waiting on turn",
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
    ["keepAlive", () => heldPrompt({ hold: { case: "keepAlive", value: { turn: { value: "ka" } } } })],
    ["sessionStarting", () => heldPrompt({ hold: { case: "sessionStarting", value: {} } })],
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
    ["the Release button", '[data-held-action="release"]'],
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
