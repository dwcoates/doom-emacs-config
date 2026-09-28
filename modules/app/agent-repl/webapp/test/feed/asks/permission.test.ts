// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  AnswerPermissionErrorSchema,
  AnswerPermissionResponseSchema,
  type AnswerPermissionResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import {
  FeedIdSchema,
  FeedPermissionAnsweredSchema,
  FeedPermissionSchema,
  FeedRowSchema,
  type FeedPermission,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FailureKind } from "../../../../proto/gen/ts/frontend/v1/failure_pb";
import { createTicker } from "../../../src/clock.js";
import type { AgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedPermission,
  PERMISSION_ANSWERED_ARMS,
  WAITING_SINCE_ATTRIBUTE,
} from "../../../src/feed/asks/permission.js";
import { armsOf } from "../arms.js";
import { askHarness, ROW_ID, settle as drain, WORKSPACE } from "./harness.js";
import { orderFor } from "../../feed-order.js";

type InitState = MessageInitShape<typeof FeedPermissionSchema>["state"];

const HEADLINE = "Claude wants to read foo.txt";

/** A consent card with the served parts and one STATE. */
function permission(
  state: InitState,
  opts: { subtitle?: string; trigger?: string; standing?: boolean; lines?: string[] } = {},
): FeedPermission {
  return create(FeedPermissionSchema, {
    headline: { text: HEADLINE },
    subtitle: opts.subtitle === undefined ? undefined : { text: opts.subtitle },
    trigger: opts.trigger === undefined ? undefined : { text: opts.trigger },
    arguments: { lines: opts.lines ?? ["path: /w/foo.txt"] },
    standingOffered: opts.standing === true ? {} : undefined,
    state,
  });
}

/** A refused answer carrying CAUSE. */
function refused(cause: MessageInitShape<typeof AnswerPermissionErrorSchema>["cause"]) {
  return create(AnswerPermissionResponseSchema, {
    result: { case: "error", value: { cause } },
  });
}

async function settle(): Promise<void> {
  await drain(vi.advanceTimersByTimeAsync);
}

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(0);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("the standing card", () => {
  it("draws the vendor's headline verbatim", () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector(".perm-headline")?.textContent).toBe(HEADLINE);
  });

  it("draws the subtitle when the vendor gave one", () => {
    const el = drawFeedPermission(
      permission({ case: "open", value: {} }, { subtitle: "in the workspace root" }),
      askHarness().rc,
    );
    expect(el.querySelector(".perm-subtitle")?.textContent).toBe("in the workspace root");
  });

  it("draws no subtitle when the vendor gave none", () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector(".perm-subtitle")).toBeNull();
  });

  it("draws the trigger note verbatim", () => {
    const el = drawFeedPermission(
      permission({ case: "open", value: {} }, { trigger: "path outside the allowed directories" }),
      askHarness().rc,
    );
    expect(el.querySelector(".perm-trigger")?.textContent).toBe(
      "path outside the allowed directories",
    );
  });

  it("draws no trigger note when the gate was simply the default", () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector(".perm-trigger")).toBeNull();
  });

  it("draws one row per composed argument line, in order", () => {
    const el = drawFeedPermission(
      permission({ case: "open", value: {} }, { lines: ["path: /w/foo.txt", "mode: read"] }),
      askHarness().rc,
    );
    expect([...el.querySelectorAll(".perm-arg")].map((n) => n.textContent)).toEqual([
      "path: /w/foo.txt",
      "mode: read",
    ]);
  });

  it("draws the two always-available buttons", () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    expect([...el.querySelectorAll("[data-permission]")].map((n) => n.getAttribute("data-permission"))).toEqual(
      ["allowOnce", "deny"],
    );
  });

  it("draws the standing button only when the vendor offered a standing form", () => {
    const el = drawFeedPermission(
      permission({ case: "open", value: {} }, { standing: true }),
      askHarness().rc,
    );
    expect(el.querySelector('[data-permission="allowStanding"]')).not.toBeNull();
  });

  it("ticks a waiting clock from the row's first draw", () => {
    vi.setSystemTime(10_000);
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector(".perm-waiting")?.textContent).toBe("waiting 0s");
  });

  it("keeps the wait growing across a re-push rather than restarting it", () => {
    const first = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    vi.setSystemTime(45_000);
    const second = drawFeedPermission(
      permission({ case: "open", value: {} }),
      askHarness({}, first).rc,
    );
    expect(second.querySelector(".perm-waiting")?.textContent).toBe("waiting 45s");
  });

  it("stamps the first-draw instant so the next draw can find it", () => {
    vi.setSystemTime(7000);
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    expect(el.getAttribute(WAITING_SINCE_ATTRIBUTE)).toBe("7000");
  });
});

describe("answering", () => {
  const buttons = [
    { hook: "allowOnce", arm: "allowOnce" },
    { hook: "deny", arm: "deny" },
  ] as const;

  for (const c of buttons) {
    it(`sends the ${c.arm} arm from the ${c.hook} button`, async () => {
      const h = askHarness();
      const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
      el.querySelector<HTMLButtonElement>(`[data-permission="${c.hook}"]`)?.click();
      await settle();
      expect(h.calls.permission[0]?.answer.case).toBe(c.arm);
    });
  }

  it("sends the standing arm from the standing button", async () => {
    const h = askHarness();
    const el = drawFeedPermission(
      permission({ case: "open", value: {} }, { standing: true }),
      h.rc,
    );
    el.querySelector<HTMLButtonElement>('[data-permission="allowStanding"]')?.click();
    await settle();
    expect(h.calls.permission[0]?.answer.case).toBe("allowStanding");
  });

  it("echoes this card's own row", async () => {
    const h = askHarness();
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    expect(h.calls.permission[0]?.permission?.value).toBe(ROW_ID);
  });

  it("echoes the workspace", async () => {
    const h = askHarness();
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    expect(h.calls.permission[0]?.workspace).toEqual(WORKSPACE);
  });

  it("carries the deny reason the user typed", async () => {
    const h = askHarness();
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    const reason = el.querySelector<HTMLInputElement>("[data-permission-reason]");
    if (reason !== null) reason.value = "not that path";
    el.querySelector<HTMLButtonElement>('[data-permission="deny"]')?.click();
    await settle();
    const answer = h.calls.permission[0]?.answer;
    expect(answer?.case === "deny" ? answer.value.reason?.text : null).toBe("not that path");
  });

  it("sends no reason when the field was left blank", async () => {
    const h = askHarness();
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    el.querySelector<HTMLButtonElement>('[data-permission="deny"]')?.click();
    await settle();
    const answer = h.calls.permission[0]?.answer;
    expect(answer?.case === "deny" ? answer.value.reason : "set").toBeUndefined();
  });

  it("latches every button inert while the answer is in flight", () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    expect([...el.querySelectorAll("button")].every((b) => b.disabled)).toBe(true);
  });

  it("draws nothing on success — the card's new state is the row's re-push", async () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("keeps the buttons latched after an answer that landed", async () => {
    const el = drawFeedPermission(permission({ case: "open", value: {} }), askHarness().rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    expect(el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.disabled).toBe(
      true,
    );
  });

  it("draws a transport failure at the buttons", async () => {
    const h = askHarness({ fail: true });
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    expect(el.querySelector(".perm-actions .refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("a refused answer", () => {
  const causes = [
    { arm: "unknownWorkspace", cause: { case: "unknownWorkspace", value: {} }, text: "the daemon does not know this workspace" },
    {
      arm: "workspaceRefMismatch",
      cause: { case: "workspaceRefMismatch", value: { registryDir: "/elsewhere" } },
      text: "this workspace's directory disagrees with the registry's: /elsewhere",
    },
    {
      arm: "transferringAway",
      cause: { case: "transferringAway", value: { address: "127.0.0.1:9931" } },
      text: "this workspace moved to another daemon at 127.0.0.1:9931",
    },
    {
      arm: "notYetAdopted",
      cause: { case: "notYetAdopted", value: {} },
      text: "the daemon has not finished adopting this workspace yet",
    },
    {
      arm: "askNotStanding",
      cause: { case: "askNotStanding", value: {} },
      text: "this ask is no longer standing",
    },
    {
      arm: "noStandingOffer",
      cause: { case: "noStandingOffer", value: {} },
      text: "no standing allow was offered for this call",
    },
    {
      arm: "noSession",
      cause: { case: "noSession", value: {} },
      text: "the workspace has no session to answer",
    },
  ] as const;

  for (const c of causes) {
    it(`draws the ${c.arm} cause at the buttons`, async () => {
      const h = askHarness({ permission: refused(c.cause) });
      const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
      el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
      await settle();
      expect(el.querySelector(".perm-actions .refusal")?.getAttribute("data-arm")).toBe(c.arm);
    });

    it(`says what ${c.arm} means`, async () => {
      const h = askHarness({ permission: refused(c.cause) });
      const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
      el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
      await settle();
      expect(el.querySelector(".perm-actions .refusal")?.textContent).toBe(c.text);
    });
  }

  it("words every cause the schema declares", () => {
    expect(causes.map((c) => c.arm).sort()).toEqual(
      armsOf(AnswerPermissionErrorSchema.oneofs, "cause").sort(),
    );
  });

  it("gives the buttons back so the reader can act on the cause", async () => {
    const h = askHarness({ permission: refused({ case: "noSession", value: {} } as never) });
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    const button = el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]');
    button?.click();
    await settle();
    expect(button?.disabled).toBe(false);
  });

  it("states an error whose cause oneof is unset as unreadable, not as a cause", async () => {
    const h = askHarness({
      permission: create(AnswerPermissionResponseSchema, {
        result: { case: "error", value: {} },
      }),
    });
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(el.querySelector(".perm-actions .refusal")).toBeNull();
  });
});

describe("the settled card", () => {
  const answers = [
    { arm: "allowedOnce", value: {}, text: "allowed once" },
    { arm: "allowedStanding", value: {}, text: "allowed with standing" },
    { arm: "deniedByUser", value: {}, text: "denied by user" },
    { arm: "deniedByPolicy", value: { text: "denied by rule" }, text: "denied by rule" },
    {
      arm: "deniedUndecidable",
      value: { text: "denied for want of a decider" },
      text: "denied for want of a decider",
    },
  ] as const;

  for (const c of answers) {
    it(`draws the ${c.arm} verdict`, () => {
      const el = drawFeedPermission(
        permission({
          case: "answered",
          value: create(FeedPermissionAnsweredSchema, {
            atMs: 0n,
            answer: { case: c.arm, value: c.value } as never,
          }),
        }),
        askHarness().rc,
      );
      expect(el.querySelector(".perm-verdict .badge")?.textContent).toBe(c.text);
    });

    it(`carries ${c.arm} as the verdict's arm`, () => {
      const el = drawFeedPermission(
        permission({
          case: "answered",
          value: create(FeedPermissionAnsweredSchema, {
            atMs: 0n,
            answer: { case: c.arm, value: c.value } as never,
          }),
        }),
        askHarness().rc,
      );
      expect(el.querySelector(".perm-verdict")?.getAttribute("data-arm")).toBe(c.arm);
    });
  }

  it("gives the undecidable denial its own verdict value, apart from the policy denial", () => {
    // Arrange
    const answered = create(FeedPermissionAnsweredSchema, {
      atMs: 0n,
      answer: { case: "deniedUndecidable", value: { text: "denied for want of a decider" } },
    });

    // Act
    const el = drawFeedPermission(
      permission({ case: "answered", value: answered }),
      askHarness().rc,
    );

    // Assert
    expect(el.querySelector(".perm-verdict")?.getAttribute("data-permission-verdict")).toBe(
      "deniedUndecidable",
    );
  });

  it("does not style the undecidable denial as the user's own act", () => {
    // Arrange
    const answered = create(FeedPermissionAnsweredSchema, {
      atMs: 0n,
      answer: { case: "deniedUndecidable", value: { text: "nobody could decide" } },
    });

    // Act
    const el = drawFeedPermission(
      permission({ case: "answered", value: answered }),
      askHarness().rc,
    );

    // Assert — the arm-specific class the user denial never wears.
    expect(
      el.querySelector(".perm-verdict .badge")?.classList.contains("arm-deniedUndecidable"),
    ).toBe(true);
  });

  it("draws every answered arm the schema carries", () => {
    expect([...PERMISSION_ANSWERED_ARMS].sort()).toEqual(
      armsOf(FeedPermissionAnsweredSchema.oneofs, "answer").sort(),
    );
  });

  it("offers no buttons once the ask is settled", () => {
    const el = drawFeedPermission(
      permission({
        case: "answered",
        value: { atMs: 0n, answer: { case: "allowedOnce", value: {} } },
      }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-permission]")).toBeNull();
  });

  it("stamps when the answer landed", () => {
    vi.setSystemTime(60_000);
    const el = drawFeedPermission(
      permission({
        case: "answered",
        value: { atMs: 0n, answer: { case: "allowedOnce", value: {} } },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".perm-when")?.textContent).toBe("1m ago");
  });

  it("reads the stamp's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the answer instant does not share the shared ticker's phase.
    vi.setSystemTime(4920);
    const el = drawFeedPermission(
      permission({
        case: "answered",
        value: { atMs: 0n, answer: { case: "allowedOnce", value: {} } },
      }),
      askHarness().rc,
    );
    // Assert: five real seconds ago reads 5s, not the lagging 4s.
    expect(el.querySelector(".perm-when")?.textContent).toBe("5s ago");
  });

  it("draws an abandoned ask as gone, not as pending", () => {
    const el = drawFeedPermission(
      permission({ case: "abandoned", value: { atMs: 0n } }),
      askHarness().rc,
    );
    expect(el.querySelector(".perm-verdict")?.getAttribute("data-arm")).toBe("abandoned");
  });

  it("offers no buttons on an abandoned ask", () => {
    const el = drawFeedPermission(
      permission({ case: "abandoned", value: { atMs: 0n } }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-permission]")).toBeNull();
  });
});

describe("drawFeedPermission malformed input", () => {
  it("refuses a card whose state oneof is unset", () => {
    const u = create(FeedPermissionSchema, {
      headline: { text: HEADLINE },
      arguments: { lines: [] },
    });
    expect(() => drawFeedPermission(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a card with no headline", () => {
    const u = permission({ case: "open", value: {} });
    (u as unknown as { headline: undefined }).headline = undefined;
    expect(() => drawFeedPermission(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a card with no argument preview", () => {
    const u = permission({ case: "open", value: {} });
    (u as unknown as { arguments: undefined }).arguments = undefined;
    expect(() => drawFeedPermission(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses an answered card whose answer oneof is unset", () => {
    const u = permission({ case: "answered", value: { atMs: 0n } });
    expect(() => drawFeedPermission(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = permission({ case: "open", value: {} });
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "withdrawn",
      value: {},
    };
    expect(() => drawFeedPermission(u, askHarness().rc)).toThrow(MalformedView);
  });
});

/**
 * A row context whose AnswerPermission hands ANSWER back UNALTERED, and a sink
 * that keeps what was filed.
 *
 * `createRouterTransport` re-encodes the response through the frozen schema,
 * which DROPS an arm no descriptor knows and turns "an arm this build cannot
 * draw" into "a oneof sets no arm" — a different refusal from the one under
 * test. A fake client is how an arm a NEWER daemon set actually reaches the
 * renderer.
 */
function unalteredPermission(answer: AnswerPermissionResponse): {
  rc: RowContext;
  filed: FailureKind[];
} {
  const filed: FailureKind[] = [];
  return {
    filed,
    rc: {
      ctx: testAppContext({
        client: {
          answerPermission: () => Promise.resolve(answer),
        } as unknown as AgentReplClient,
        workspace: WORKSPACE,
        ticker: createTicker(1000),
        failures: { report: (kind) => filed.push(kind), retract: () => {} },
        composerEnabled: false,
      }),
      feed: "root",
      row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: ROW_ID }), order: orderFor(ROW_ID) }),
      revealRow: async () => false,
    },
  };
}

describe("an answer this build cannot read", () => {
  it("files the unknown result arm by name rather than drawing a refusal", async () => {
    // Arrange: a newer daemon answers with a result arm this build has no case for.
    const answer = create(AnswerPermissionResponseSchema, {
      result: { case: "success", value: {} },
    });
    (answer as unknown as { result: { case: string; value: unknown } }).result = {
      case: "deferred",
      value: {},
    };
    const h = unalteredPermission(answer);
    const el = drawFeedPermission(permission({ case: "open", value: {} }), h.rc);
    // Act
    el.querySelector<HTMLButtonElement>('[data-permission="allowOnce"]')?.click();
    await settle();
    // Assert
    const kind = h.filed[0]?.kind;
    expect(
      kind?.case === "frameUndecodable" ? [kind.value.frameHead, kind.value.cause] : null,
    ).toEqual([
      "AnswerPermissionResponse.result",
      "arm 'deferred' is not one this build can draw",
    ]);
  });
});

describe("an answered verdict this build cannot read", () => {
  it("refuses an answer arm this build does not know, by name", () => {
    // Arrange
    const u = permission({
      case: "answered",
      value: { atMs: 0n, answer: { case: "allowedOnce", value: {} } },
    });
    (
      u.state as unknown as { value: { answer: { case: string; value: unknown } } }
    ).value.answer = { case: "allowedForSession", value: {} };
    // Act / Assert
    expect(() => drawFeedPermission(u, askHarness().rc)).toThrow(
      "malformed view at FeedPermission.answered.answer: arm 'allowedForSession' is not one this build can draw",
    );
  });
});
