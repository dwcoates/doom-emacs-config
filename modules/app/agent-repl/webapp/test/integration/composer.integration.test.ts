/**
 * COMPOSER — the dev-mode surface, and the fields a submission MUST carry.
 *
 * Production runs composer-less: the root composer is host-native (Emacs), so
 * the first thing this file asserts is that no composer exists without
 * `&composer=1`. Everything else is about the submission itself, because three
 * of its fields are load-bearing and each fails silently if omitted:
 *
 *   - `workspace` and `origin` are REQUIRED (the fake refuses either missing
 *     with InvalidArgument, so a forgotten field fails loudly here),
 *   - `idempotency_key` is the contract's ONE client-minted value: fresh per
 *     submission, and STABLE across a retry of the same submission, which is
 *     what stops a double-send from becoming two turns.
 *
 * The minted `TurnId` is then matched against the feed row that arrives, since
 * that match is the only thing tying a submission to its turn.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ConnectError, Code } from "@connectrpc/connect";

import { SubmitPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { SubmitPromptSuccessSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { SubmitPromptCommandPanelSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import {
  COMMAND_PANEL_ARMS,
  MCP_STATUS_ARMS,
  TODO_STATUS_ARMS,
  WEBAPP_ORIGIN,
  WORKSPACE_ID,
  activityRow,
  assertCoversOneof,
  commandPanel,
  feedId,
  feedPageSuccess,
  footerView,
  responseUnit,
  subagentUnit,
  userPromptRow,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** The dev composer's input, typed and dispatched as a user would. */
async function type(text: string, host = '[data-component="composer"]'): Promise<void> {
  const input = harness.$(`${host} textarea`) as HTMLTextAreaElement;
  if (!input) throw new Error(`no composer input at ${host}`);
  input.value = text;
  input.dispatchEvent(new Event("input", { bubbles: true }));
  await harness.settle();
}

/** Submit whatever is typed into the given composer. */
async function send(host = '[data-component="composer"]'): Promise<void> {
  await harness.click(`${host} [data-composer-send]`);
}

describe("production runs composer-less", () => {
  it("draws no composer without the dev flag", async () => {
    // Arrange / Act
    harness = await startHarness();
    // Assert
    expect(harness.$('[data-component="composer"]')?.hidden).toBe(true);
  });

  it("mounts no composer input without the dev flag", async () => {
    // Arrange / Act
    harness = await startHarness();
    // Assert
    expect(harness.$('[data-component="composer"] textarea')).toBeNull();
  });

  it("mounts no bubble composer without the dev flag", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert
    expect(harness.$('[data-feed-row="bubble"] textarea')).toBeNull();
  });
});

describe("the dev composer", () => {
  it("draws when the dev flag is on", async () => {
    // Arrange / Act
    harness = await startHarness({ composer: true });
    // Assert
    expect(harness.$('[data-component="composer"]')?.hidden).toBe(false);
  });

  it("calls SubmitPrompt on send", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("do the thing");
    // Act
    await send();
    // Assert
    expect(harness.fake.calls("submitPrompt")).toHaveLength(1);
  });

  it("sends the text as a UserSaid text block", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("do the thing");
    // Act
    await send();
    // Assert
    const [request] = harness.fake.calls<{
      said?: { content?: { blocks: { block: { case?: string; value?: { text: string } } }[] } };
    }>("submitPrompt");
    const block = request.said?.content?.blocks[0]?.block;
    expect(block?.case === "text" ? block.value?.text : undefined).toBe("do the thing");
  });

  it("clears the input on a successful send", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("do the thing");
    // Act
    await send();
    // Assert
    expect((harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement).value).toBe("");
  });

  it("sends nothing for an empty input", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    // Act
    await send();
    // Assert
    expect(harness.fake.calls("submitPrompt")).toHaveLength(0);
  });
});

describe("the required workspace", () => {
  it("carries the context's workspace on every submission", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("a prompt");
    // Act
    await send();
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("submitPrompt");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("carries the workspace's dir, not just its id", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("a prompt");
    // Act
    await send();
    // Assert: the ref is echoed whole; a half-built ref is a defect.
    const [request] = harness.fake.calls<{ workspace?: { dir: string } }>("submitPrompt");
    expect(request.workspace?.dir).not.toBe("");
  });

  it("carries the workspace on a bubble submission too", async () => {
    // Arrange
    harness = await startHarness({
      composer: true,
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await type("into the bubble", '[data-feed-row="bubble"]');
    // Act
    await send('[data-feed-row="bubble"]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("submitPrompt");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("is refused by the daemon when absent", async () => {
    // Arrange: prove the fake actually enforces it, so the assertions above
    // are not passing against a lenient stub.
    harness = await startHarness({ composer: true });
    const failed = await harness.ctx.client
      .submitPrompt({ idempotencyKey: "k", origin: WEBAPP_ORIGIN })
      .catch((e: unknown) => e);
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.InvalidArgument);
  });
});

describe("the required origin", () => {
  it("sends the webapp user-sent origin from the dev composer", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("a prompt");
    // Act
    await send();
    // Assert
    const [request] = harness.fake.calls<{ origin: PromptOrigin }>("submitPrompt");
    expect(request.origin).toBe(PromptOrigin.WEBAPP_USER_SENT);
  });

  it("sends the webapp user-sent origin from a bubble composer", async () => {
    // Arrange
    harness = await startHarness({
      composer: true,
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await type("into the bubble", '[data-feed-row="bubble"]');
    // Act
    await send('[data-feed-row="bubble"]');
    // Assert
    const [request] = harness.fake.calls<{ origin: PromptOrigin }>("submitPrompt");
    expect(request.origin).toBe(PromptOrigin.WEBAPP_USER_SENT);
  });

  it("never sends the unspecified origin", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("a prompt");
    // Act
    await send();
    // Assert
    const requests = harness.fake.calls<{ origin: PromptOrigin }>("submitPrompt");
    expect(requests.every((r) => r.origin !== PromptOrigin.UNSPECIFIED)).toBe(true);
  });

  it("is refused by the daemon when unspecified", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    const failed = await harness.ctx.client
      .submitPrompt({ workspace: harness.ctx.workspace, idempotencyKey: "k" })
      .catch((e: unknown) => e);
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.InvalidArgument);
  });
});

describe("the idempotency key", () => {
  it("mints a key for every submission", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("a prompt");
    // Act
    await send();
    // Assert
    const [request] = harness.fake.calls<{ idempotencyKey: string }>("submitPrompt");
    expect(request.idempotencyKey).not.toBe("");
  });

  it("mints a FRESH key per distinct submission", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("first");
    await send();
    // Act
    await type("second");
    await send();
    // Assert
    const keys = harness.fake.calls<{ idempotencyKey: string }>("submitPrompt").map((r) => r.idempotencyKey);
    expect(new Set(keys).size).toBe(2);
  });

  it("reuses the SAME key when the same submission is retried", async () => {
    // Arrange: the first attempt fails at the transport, so the text stands
    // and the retry is the same submission, not a new one.
    harness = await startHarness({ composer: true });
    harness.fake.failNext("submitPrompt", "the daemon dropped the call");
    await type("a prompt");
    await send();
    // Act
    await send();
    // Assert
    const keys = harness.fake.calls<{ idempotencyKey: string }>("submitPrompt").map((r) => r.idempotencyKey);
    expect(new Set(keys).size).toBe(1);
  });

  it("mints a new key after a successful send", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("first");
    await send();
    const [first] = harness.fake.calls<{ idempotencyKey: string }>("submitPrompt");
    // Act
    await type("second");
    await send();
    // Assert
    const keys = harness.fake.calls<{ idempotencyKey: string }>("submitPrompt");
    expect(keys[1].idempotencyKey).not.toBe(first.idempotencyKey);
  });
});

describe("the minted turn", () => {
  it("matches the turn the feed row carries", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "turn", value: { turn: { value: "turn-mine" } } } },
        },
      }),
    );
    await harness.fake.awaitStream("watchFeed");
    await type("a prompt");
    await send();
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      userPromptRow("a prompt", { id: feedId("mine"), turn: { value: "turn-mine" } }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("mine")?.dataset.turn).toBe("turn-mine");
  });

  it("marks the row as this client's own submission", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "turn", value: { turn: { value: "turn-mine" } } } },
        },
      }),
    );
    await harness.fake.awaitStream("watchFeed");
    await type("a prompt");
    await send();
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      userPromptRow("a prompt", { id: feedId("mine"), turn: { value: "turn-mine" } }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("mine")?.dataset.mine).toBe("true");
  });

  it("does not claim a row from another submitter's turn", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      userPromptRow("from Emacs", { id: feedId("theirs"), turn: { value: "turn-theirs" } }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("theirs")?.dataset.mine).toBeUndefined();
  });
});

describe("the bubble composer's gate", () => {
  /** Expand a bubble under the dev flag with the given footer status. */
  const withBubble = async (status: string): Promise<void> => {
    harness = await startHarness({
      composer: true,
      arrange: (fake) => {
        fake.setFooter(WORKSPACE_ID, footerView({ status }));
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
  };

  it("is open while the footer is idle", async () => {
    // Arrange / Act
    await withBubble("idle");
    // Assert
    expect(
      (harness.$('[data-feed-row="bubble"] textarea') as HTMLTextAreaElement)?.disabled,
    ).toBe(false);
  });

  it.each(["closing", "agentReplFault", "networkFault"])("closes while the footer is %s", async (status) => {
    // Arrange / Act
    await withBubble(status);
    // Assert
    expect(
      (harness.$('[data-feed-row="bubble"] textarea') as HTMLTextAreaElement)?.disabled,
    ).toBe(true);
  });

  it.each(["merging", "vendorFault"])("stays open while the footer is %s: its prompts are held", async (status) => {
    // Arrange / Act
    await withBubble(status);
    // Assert
    expect(
      (harness.$('[data-feed-row="bubble"] textarea') as HTMLTextAreaElement)?.disabled,
    ).toBe(false);
  });

  it("re-opens when the footer leaves an agent-repl fault", async () => {
    // Arrange
    await withBubble("agentReplFault");
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", substatus: "ready" }));
    await harness.settle();
    // Assert
    expect(
      (harness.$('[data-feed-row="bubble"] textarea') as HTMLTextAreaElement)?.disabled,
    ).toBe(false);
  });

  it("addresses the bubble's own FeedId on submission", async () => {
    // Arrange
    await withBubble("idle");
    await type("into the bubble", '[data-feed-row="bubble"]');
    // Act
    await send('[data-feed-row="bubble"]');
    // Assert
    const [request] = harness.fake.calls<{ feed?: { value: string } }>("submitPrompt");
    expect(request.feed?.value).toBe("bubble");
  });

  it("sends no feed from the root composer", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await type("at the root");
    // Act
    await send();
    // Assert: an absent `feed` addresses the workspace's own feed.
    const [request] = harness.fake.calls<{ feed?: { value: string } }>("submitPrompt");
    expect(request.feed).toBeUndefined();
  });
});

describe("a command refusal answer", () => {
  it("clears the composer text", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "commandRefused", value: { command: "/agents" } } },
        },
      }),
    );
    await type("/agents");
    // Act
    await send();
    // Assert: it was ACCEPTED and answered, so the draft is spent.
    expect((harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement).value).toBe("");
  });

  it("draws no composer refusal, since this is a success arm", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "commandRefused", value: { command: "/agents" } } },
        },
      }),
    );
    await type("/agents");
    // Act
    await send();
    // Assert
    expect(harness.$(".composer-refusal")).toBeNull();
  });
});

describe("command panels", () => {
  it("covers every panel arm the SubmitPrompt success carries", () => {
    assertCoversOneof(SubmitPromptCommandPanelSchema, "panel", [...COMMAND_PANEL_ARMS]);
  });

  it("covers every SubmitPrompt success outcome", () => {
    assertCoversOneof(SubmitPromptSuccessSchema, "outcome", [
      "turn",
      "commandPanel",
      "commandRefused",
      "commandActed",
    ]);
  });

  /** Submit a slash command whose answer is the given panel. */
  const submitPanel = async (arm: (typeof COMMAND_PANEL_ARMS)[number]): Promise<void> => {
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: { case: "success", value: { outcome: { case: "commandPanel", value: commandPanel(arm) } } },
      }),
    );
    await type(`/${arm}`);
    await send();
  };

  it.each(COMMAND_PANEL_ARMS)("hands the %s panel to the composer's callback", async (arm) => {
    // Arrange / Act
    await submitPanel(arm);
    // Assert
    expect(harness.panels).toHaveLength(1);
  });

  it.each(COMMAND_PANEL_ARMS)("draws the %s panel", async (arm) => {
    // Arrange / Act
    await submitPanel(arm);
    // Assert
    expect(harness.$(`[data-panel="${arm}"]`)).not.toBeNull();
  });

  it("draws the status panel's thin label/value rows", async () => {
    // Arrange / Act
    await submitPanel("status");
    // Assert
    expect(harness.$('[data-panel="status"]')?.textContent).toContain("0.1.0");
  });

  it("draws every status panel row", async () => {
    // Arrange / Act
    await submitPanel("status");
    // Assert: version, account, model, mode — the settled thin panel.
    expect(harness.$$('[data-panel="status"] [data-row]')).toHaveLength(4);
  });

  it.each(TODO_STATUS_ARMS)("draws the todos panel's %s glyph arm", async (status) => {
    // Arrange / Act
    await submitPanel("todos");
    // Assert
    expect(harness.$(`[data-panel="todos"] [data-todo-status="${status}"]`)).not.toBeNull();
  });

  it("draws the todos panel's subjects verbatim", async () => {
    // Arrange / Act
    await submitPanel("todos");
    // Assert
    expect(harness.$('[data-panel="todos"]')?.textContent).toContain("write the harness");
  });

  it("draws the agents panel's names and descriptions", async () => {
    // Arrange / Act
    await submitPanel("agents");
    // Assert
    const drawn = harness.$('[data-panel="agents"]')?.textContent ?? "";
    expect(drawn).toContain("reviewer");
    expect(drawn).toContain("search the repo");
  });

  it.each(MCP_STATUS_ARMS)("draws the mcp panel's %s badge arm", async (status) => {
    // Arrange / Act
    await submitPanel("mcp");
    // Assert
    expect(harness.$(`[data-panel="mcp"] [data-mcp-status="${status}"]`)).not.toBeNull();
  });

  it("draws the mcp failure's own detail verbatim", async () => {
    // Arrange / Act
    await submitPanel("mcp");
    // Assert
    expect(harness.$('[data-panel="mcp"]')?.textContent).toContain("handshake refused");
  });

  // /context IS THE ONE PANEL THIS END DRAWS CUSTOM. The daemon states the
  // header's PARTS (used, total, percent, model) and a recursive tree of
  // sections; the client joins the parts into one line, colors only the
  // percent, and renders every section that HAS detail as a `<details>` closed
  // by default. There is no named `data-fold` here and no per-section fold
  // ruling — the tree's own shape is the fold.
  it("composes the header line from the daemon's own parts", async () => {
    // Arrange / Act
    await submitPanel("context");
    // Assert: the client joins, it never does the arithmetic.
    expect(harness.$('[data-panel="context"] .context-header')?.textContent).toBe(
      "142.3k of 200k (71%) · claude-opus-5",
    );
  });

  it("colors only the percent, and echoes the daemon's figure verbatim", async () => {
    // Arrange / Act
    await submitPanel("context");
    // Assert: the percent is the one span that turns into a warning.
    const percent = harness.$('[data-panel="context"] .context-header-percent');
    expect(percent?.getAttribute("data-percent")).toBe("71");
    expect(percent?.style.color).not.toBe("");
  });

  it("draws a section that has detail as a fold closed by default", async () => {
    // Arrange / Act
    await submitPanel("context");
    // Assert: the `open` attribute is deliberately never set on a fresh draw.
    const sections = harness.$$('[data-panel="context"] details.context-section');
    expect(sections.length).toBeGreaterThan(0);
    expect(sections.every((el) => !el.hasAttribute("open"))).toBe(true);
  });

  it("nests a sub-fold beneath the section that carries it", async () => {
    // Arrange / Act: `tool calls` is a nested section under `Messages`.
    await submitPanel("context");
    // Assert
    const nested = harness.$$(
      '[data-panel="context"] details.context-section details.context-section',
    );
    expect(nested.map((el) => el.querySelector(".panel-row-label")?.textContent)).toContain(
      "tool calls",
    );
  });

  it("draws a section with no detail as a bare row rather than a fold", async () => {
    // Arrange / Act: `Free space` has neither items nor sub-sections, so
    // offering a chevron would lie about there being something to unfold.
    await submitPanel("context");
    // Assert
    const leaves = harness.$$('[data-panel="context"] .context-section-leaf');
    expect(leaves.map((el) => el.textContent)).toContain("Free space57.7k · 29%");
    expect(leaves.every((el) => el.tagName !== "DETAILS")).toBe(true);
  });

  it("draws the auto-compaction line the resolver composed, verbatim", async () => {
    // Arrange / Act
    await submitPanel("context");
    // Assert
    expect(harness.$('[data-panel="context"] .context-auto-compact')?.textContent).toBe(
      "auto-compact at 90%",
    );
  });

  it("draws the context panel's auto-compact line verbatim", async () => {
    // Arrange / Act
    await submitPanel("context");
    // Assert
    expect(harness.$('[data-panel="context"]')?.textContent).toContain("auto-compact at 90%");
  });

  it("draws the help panel's commands verbatim", async () => {
    // Arrange / Act
    await submitPanel("help");
    // Assert
    expect(harness.$('[data-panel="help"]')?.textContent).toContain("/compact");
  });

  it("draws no feed row for a command panel answer", async () => {
    // Arrange / Act: Q3 — panels render only from the dev composer.
    await submitPanel("status");
    // Assert
    expect(harness.feedContainer()?.querySelector("[data-panel]")).toBeNull();
  });
});

describe("submission does not derive state", () => {
  it("draws no optimistic prompt row before the daemon pushes one", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await harness.fake.awaitStream("watchFeed");
    await type("a prompt");
    // Act
    await send();
    // Assert: the client accumulates nothing; the row arrives on the feed.
    expect(harness.rowIds()).toEqual([]);
  });

  it("draws the row once the daemon pushes it", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await harness.fake.awaitStream("watchFeed");
    await type("a prompt");
    await send();
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("a prompt", { id: feedId("mine") }));
    await harness.settle();
    // Assert
    expect(harness.rowIds()).toEqual(["mine"]);
  });

  it("draws a response row pushed after the submission", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    await harness.fake.awaitStream("watchFeed");
    await type("a prompt");
    await send();
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      activityRow(responseUnit("success", "the answer"), { id: feedId("answer") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("answer")?.textContent).toContain("the answer");
  });
});

// ---------------------------------------------------------------------------
// command_acted: A RECOGNIZED ACT THAT MINTS NO TURN (audit 1, item 6)
//
// The answer is EMPTY on purpose: the set arm is the whole assertion, and the
// visible effect (the model change, restated authoritatively) arrives on the
// component streams, never in this answer. So the box clears — the words are
// spent — and the composer draws nothing at all.
// ---------------------------------------------------------------------------

describe("a command-acted answer", () => {
  /** Submit a session-acting command whose answer mints no turn. */
  const submitActed = async (): Promise<void> => {
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: { case: "success", value: { outcome: { case: "commandActed", value: {} } } },
      }),
    );
    await type("/model opus");
    await send();
  };

  it("clears the draft", async () => {
    // Arrange / Act
    await submitActed();
    // Assert
    expect((harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement).value).toBe("");
  });

  it("draws no refusal", async () => {
    // Arrange / Act: an act the daemon performed is an ANSWER, not a failure.
    await submitActed();
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  it("draws no panel", async () => {
    // Arrange / Act
    await submitActed();
    // Assert
    expect(harness.$("[data-panel]")).toBeNull();
  });

  it("hands the composer no panel to draw", async () => {
    // Arrange / Act
    await submitActed();
    // Assert
    expect(harness.panels).toEqual([]);
  });

  it("draws no feed row of its own", async () => {
    // Arrange / Act: the effect arrives on the component streams.
    await submitActed();
    // Assert
    expect(harness.rowIds()).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// duplicate_submission: THE KEY THE CLIENT MINTED WAS ALREADY ACCEPTED
// (audit 1, item 7)
//
// The idempotency key is the ONE client-minted value, and it is STABLE across
// a retry of the same submission — which is exactly how a retry provokes this
// arm. The words stay in the box: the earlier submission stands, and the user
// is told so at the control they pressed.
// ---------------------------------------------------------------------------

describe("a duplicate submission", () => {
  /**
   * Send once against a transport that drops the call (so the text stands and
   * the key is kept), then send again against a daemon that refuses the key it
   * has already accepted.
   */
  const retryDuplicate = async (): Promise<void> => {
    harness = await startHarness({ composer: true });
    harness.fake.failNext("submitPrompt", "the daemon dropped the call");
    await type("a prompt");
    await send();
    harness.fake.refuse("submitPrompt", "duplicateSubmission");
    await send();
  };

  it("retried with the same idempotency key", async () => {
    // Arrange / Act
    await retryDuplicate();
    // Assert: the retry IS the same submission, which is what makes the
    // daemon's refusal the right answer.
    const keys = harness.fake.calls<{ idempotencyKey: string }>("submitPrompt").map((r) => r.idempotencyKey);
    expect(new Set(keys).size).toBe(1);
  });

  it("draws the refusal at the composer", async () => {
    // Arrange / Act
    await retryDuplicate();
    // Assert
    expect(harness.$('.composer-refusal[data-arm="duplicateSubmission"]')).not.toBeNull();
  });

  it("says the prompt was already submitted", async () => {
    // Arrange / Act
    await retryDuplicate();
    // Assert
    expect(harness.text(".composer-refusal")).toContain("already submitted");
  });

  it("keeps the text in the box", async () => {
    // Arrange / Act
    await retryDuplicate();
    // Assert
    expect((harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement).value).toBe(
      "a prompt",
    );
  });

  it("draws no feed row for the refused resubmission", async () => {
    // Arrange / Act
    await retryDuplicate();
    // Assert
    expect(harness.rowIds()).toEqual([]);
  });
});
