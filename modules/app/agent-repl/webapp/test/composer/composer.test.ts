// @vitest-environment jsdom
import { type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { oneofArms } from "../arms.js";
import {
  SubmitPromptErrorSchema,
  SubmitPromptResponseSchema,
  type SubmitPromptCommandPanel,
  type SubmitPromptRequest,
  type SubmitPromptResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";
import { FeedIdSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import {
  createComposerGate,
  mountComposer,
  submitPromptRefusal,
  type ComposerHandle,
} from "../../src/composer/composer.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { DROPPED_EVENT } from "../../src/tray/held-prompt.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const FEED = create(FeedIdSchema, { value: "feed-9" });
/** A sink that keeps what was filed, so a test can read the arm it carried. */
function recordingSink(): FailureSink & { reports: FailureKind[] } {
  const reports: FailureKind[] = [];
  return { reports, report: (kind) => reports.push(kind), retract: () => undefined };
}

const turnSuccess = (turn = "turn-1"): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: {
      case: "success",
      value: { outcome: { case: "turn", value: { turn: { value: turn } } } },
    },
  });
const panelSuccess = (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: {
      case: "success",
      value: {
        outcome: {
          case: "commandPanel",
          value: { panel: { case: "status", value: { rows: [] } } },
        },
      },
    },
  });
const refusedSuccess = (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: {
      case: "success",
      value: { outcome: { case: "commandRefused", value: { command: "/agents" } } },
    },
  });
const actedSuccess = (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: { case: "success", value: { outcome: { case: "commandActed", value: {} } } },
  });
const mergingError = (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: { case: "error", value: { reason: { case: "merging", value: {} } } },
  });
const unsetError = (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, { result: { case: "error", value: {} } });

/** What each fact-carrying arm must carry for its sentence to be complete. */
const REASON_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
  bubbleRefused: { detail: "", kind: { case: "agentBusy", value: {} } },
};

/** A bubble refusal carrying KIND and the shim's DETAIL. */
const bubbleRefusal =
  (kind: string, detail: string) =>
  (): SubmitPromptResponse =>
    create(SubmitPromptResponseSchema, {
      result: {
        case: "error",
        value: {
          reason: {
            case: "bubbleRefused",
            value: { detail, kind: { case: kind, value: {} } },
          },
        },
      },
    } as never);

/** A cold-gate refusal carrying the gate's own DETAIL. */
const coldGateRefusal =
  (detail: string) =>
  (): SubmitPromptResponse =>
    create(SubmitPromptResponseSchema, {
      result: { case: "error", value: { reason: { case: "coldGate", value: { detail } } } },
    } as never);

/** A bubble refusal whose `kind` oneof sets no arm. */
const bubbleRefusalUnsetKind = (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: {
      case: "error",
      value: { reason: { case: "bubbleRefused", value: { detail: "d" } } },
    },
  } as never);

/** A refusal carrying ARM, with the payload that arm's sentence needs. */
const refusalError = (arm: string) => (): SubmitPromptResponse =>
  create(SubmitPromptResponseSchema, {
    result: { case: "error", value: { reason: { case: arm, value: REASON_FILL[arm] ?? {} } } },
  } as never);

interface Harness {
  host: HTMLElement;
  ctx: AppContext;
  seen: SubmitPromptRequest[];
  panels: SubmitPromptCommandPanel[];
  gate: ReturnType<typeof createComposerGate>;
  handle: ComposerHandle;
  input: HTMLTextAreaElement;
  send: Control;
  reports: FailureKind[];
}

function mount(
  answer: () => SubmitPromptResponse = turnSuccess,
  opts: { composerEnabled?: boolean; feed?: typeof FEED; throws?: Error } = {},
): Harness {
  const seen: SubmitPromptRequest[] = [];
  const panels: SubmitPromptCommandPanel[] = [];
  const sink = recordingSink();
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      submitPrompt: (request) => {
        seen.push(request);
        if (opts.throws !== undefined) throw opts.throws;
        return answer();
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(60_000),
    failures: sink,
    composerEnabled: opts.composerEnabled ?? true,
  });
  const host = document.createElement("div");
  document.body.appendChild(host);
  const gate = createComposerGate();
  const handle = mountComposer(host, ctx, {
    gate,
    onPanel: (panel) => panels.push(panel),
    ...(opts.feed !== undefined ? { feed: opts.feed } : {}),
  });
  return {
    host,
    ctx,
    seen,
    panels,
    gate,
    handle,
    input: host.querySelector("textarea") as HTMLTextAreaElement,
    send: host.querySelector("ar-button") as Control,
    reports: sink.reports,
  };
}

const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

/** Type TEXT and press Enter, which is the composer's send. */
async function sendText(h: Harness, text: string): Promise<void> {
  h.input.value = text;
  h.input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
  await settle();
}

describe("createComposerGate", () => {
  it("starts open", () => {
    expect(createComposerGate().current()).toBe("open");
  });

  it("carries the reason to its subscribers", () => {
    const gate = createComposerGate();
    const seen: Array<[string, string | undefined]> = [];
    gate.subscribe((state, reason) => seen.push([state, reason]));
    gate.set("closed", "merge in flight");
    expect(seen).toEqual([["closed", "merge in flight"]]);
  });

  it("stops calling an unsubscribed listener", () => {
    const gate = createComposerGate();
    let calls = 0;
    const off = gate.subscribe(() => {
      calls += 1;
    });
    off();
    gate.set("closed");
    expect(calls).toBe(0);
  });
});

describe("mountComposer presence", () => {
  it("draws nothing in production without a feed", () => {
    const h = mount(turnSuccess, { composerEnabled: false });
    expect(h.host.childElementCount).toBe(0);
    h.handle.dispose();
  });

  it("draws a bubble composer in production when a feed is given", () => {
    const h = mount(turnSuccess, { composerEnabled: false, feed: FEED });
    expect(h.host.querySelector("textarea")).not.toBeNull();
    h.handle.dispose();
  });

  it("draws the dev composer when the page enabled it", () => {
    const h = mount();
    expect(h.host.querySelector("[data-composer-send]")).not.toBeNull();
    h.handle.dispose();
  });
});

describe("the gate", () => {
  it("disables the send control when the gate closes", () => {
    const h = mount();
    h.gate.set("closed", "merging");
    expect(h.send.disabled).toBe(true);
    h.handle.dispose();
  });

  it("shows the gate's own reason as the notice", () => {
    const h = mount();
    h.gate.set("closed", "the workspace is closing");
    expect(h.host.querySelector(".composer-notice")?.textContent).toBe(
      "the workspace is closing",
    );
    h.handle.dispose();
  });

  it("disables the draft box while the gate is shut", () => {
    const h = mount();
    h.gate.set("closed", "merging");
    // R7 disables the composer while the footer reads merging, closing or
    // disconnected: a box that still takes keystrokes while nothing can be
    // sent invites a draft the reader then watches be refused.
    expect(h.input.disabled).toBe(true);
    h.handle.dispose();
  });

  it("re-enables the draft box when the gate opens again", () => {
    const h = mount();
    h.gate.set("closed", "merging");
    h.gate.set("open");
    expect(h.input.disabled).toBe(false);
    h.handle.dispose();
  });

  it("re-opens the send control when the gate opens", () => {
    const h = mount();
    h.gate.set("closed", "merging");
    h.gate.set("open");
    expect(h.send.disabled).toBe(false);
    h.handle.dispose();
  });

  it("submits nothing while the gate is shut", async () => {
    const h = mount();
    h.gate.set("closed", "merging");
    await sendText(h, "hello");
    expect(h.seen).toHaveLength(0);
    h.handle.dispose();
  });
});

describe("the keys", () => {
  it("sends on Enter", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.seen).toHaveLength(1);
    h.handle.dispose();
  });

  it("does not send on Shift+Enter", async () => {
    const h = mount();
    h.input.value = "hello";
    h.input.dispatchEvent(
      new KeyboardEvent("keydown", { key: "Enter", shiftKey: true, bubbles: true }),
    );
    await settle();
    expect(h.seen).toHaveLength(0);
    h.handle.dispose();
  });

  it("sends on the send button", async () => {
    const h = mount();
    h.input.value = "hello";
    h.send.click();
    await settle();
    expect(h.seen).toHaveLength(1);
    h.handle.dispose();
  });

  it("submits nothing for whitespace alone", async () => {
    const h = mount();
    await sendText(h, "   ");
    expect(h.seen).toHaveLength(0);
    h.handle.dispose();
  });
});

describe("the request", () => {
  it("echoes the workspace on every submission", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.seen[0]?.workspace?.id).toBe("ws-1");
    h.handle.dispose();
  });

  it("always sends the webapp user-sent origin", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.seen[0]?.origin).toBe(PromptOrigin.WEBAPP_USER_SENT);
    h.handle.dispose();
  });

  it("carries the typed words as one text block", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.seen[0]?.said?.content?.blocks[0]?.block.value).toEqual({
      $typeName: "conversation.v1.TextBlock",
      text: "hello",
    });
    h.handle.dispose();
  });

  it("leaves the feed unset on the root composer", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.seen[0]?.feed).toBeUndefined();
    h.handle.dispose();
  });

  it("echoes the bubble's feed on a bubble composer", async () => {
    const h = mount(turnSuccess, { feed: FEED });
    await sendText(h, "hello");
    expect(h.seen[0]?.feed?.value).toBe("feed-9");
    h.handle.dispose();
  });

  it("mints an idempotency key", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.seen[0]?.idempotencyKey).not.toBe("");
    h.handle.dispose();
  });

  it("reuses the key when the same unsent text is retried", async () => {
    const h = mount(mergingError);
    await sendText(h, "hello");
    await sendText(h, "hello");
    expect(h.seen[0]?.idempotencyKey).toBe(h.seen[1]?.idempotencyKey);
    h.handle.dispose();
  });

  it("mints a fresh key once the text changed", async () => {
    const h = mount(mergingError);
    await sendText(h, "hello");
    await sendText(h, "goodbye");
    expect(h.seen[0]?.idempotencyKey).not.toBe(h.seen[1]?.idempotencyKey);
    h.handle.dispose();
  });

  it("mints a fresh key after a submission was accepted", async () => {
    const h = mount();
    await sendText(h, "hello");
    await sendText(h, "hello");
    expect(h.seen[0]?.idempotencyKey).not.toBe(h.seen[1]?.idempotencyKey);
    h.handle.dispose();
  });
});

describe("the outcomes", () => {
  it("clears the box on a minted turn", async () => {
    const h = mount();
    await sendText(h, "hello");
    expect(h.input.value).toBe("");
    h.handle.dispose();
  });

  it("exposes the minted TurnId", async () => {
    const h = mount(() => turnSuccess("turn-7"));
    await sendText(h, "hello");
    expect(h.handle.lastTurn()?.value).toBe("turn-7");
    h.handle.dispose();
  });

  it("hands a command panel to the caller", async () => {
    const h = mount(panelSuccess);
    await sendText(h, "/status");
    expect(h.panels[0]?.panel.case).toBe("status");
    h.handle.dispose();
  });

  it("clears the box on a recognized-but-unsupported command", async () => {
    const h = mount(refusedSuccess);
    await sendText(h, "/agents");
    expect(h.input.value).toBe("");
    h.handle.dispose();
  });

  it("mints no turn for a recognized-but-unsupported command", async () => {
    const h = mount(refusedSuccess);
    await sendText(h, "/agents");
    expect(h.handle.lastTurn()).toBeUndefined();
    h.handle.dispose();
  });

  it("refuses a turn outcome with no TurnId, keeping the text", async () => {
    const h = mount(() =>
      create(SubmitPromptResponseSchema, {
        result: { case: "success", value: { outcome: { case: "turn", value: {} } } },
      }),
    );
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });
});

describe("the refusals", () => {
  it("draws the merging refusal inline at the composer", async () => {
    const h = mount(mergingError);
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.getAttribute("data-arm")).toBe("merging");
    h.handle.dispose();
  });

  it("keeps the text through a merging refusal", async () => {
    const h = mount(mergingError);
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });

  it.each(oneofArms(SubmitPromptErrorSchema, "reason"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const h = mount(refusalError(arm));
      await sendText(h, "hello");
      const refusal = h.host.querySelector(".composer-refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([arm, false]);
      h.handle.dispose();
    },
  );

  it.each(oneofArms(SubmitPromptErrorSchema, "reason"))(
    "keeps the words in the box through a %s refusal",
    async (arm) => {
      const h = mount(refusalError(arm));
      await sendText(h, "hello");
      expect(h.input.value).toBe("hello");
      h.handle.dispose();
    },
  );

  it("names the successor daemon on a transfer, from the one shared wording", async () => {
    const h = mount(refusalError("transferringAway"));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain("127.0.0.1:7777");
    h.handle.dispose();
  });

  it("tells a duplicate submitter the earlier submission already stands", async () => {
    const h = mount(refusalError("duplicateSubmission"));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain("already submitted");
    h.handle.dispose();
  });

  it("labels the duplicate-submission refusal with its own arm", async () => {
    const h = mount(refusalError("duplicateSubmission"));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.getAttribute("data-arm")).toBe(
      "duplicateSubmission",
    );
    h.handle.dispose();
  });

  it("keeps the words in the box through a duplicate-submission refusal", async () => {
    const h = mount(refusalError("duplicateSubmission"));
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });

  // GROUNDED 2026-09-14 (owner's report): three prompts to a workspace parked
  // at its cold gate were refused `no_session` against a session that was up,
  // and the composer said the workspace had no session to prompt.
  it("tells a cold-gated submitter the session is parked at its gate", async () => {
    const h = mount(coldGateRefusal(""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain(
      "parked at its cold gate",
    );
    h.handle.dispose();
  });

  it("sends a cold-gated submitter to the panel the gate is answered in", async () => {
    const h = mount(coldGateRefusal(""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain(
      "answer it in the panel",
    );
    h.handle.dispose();
  });

  it("draws the gate's own account after the stem when the refusal carries one", async () => {
    const h = mount(coldGateRefusal("182k tokens would be re-read"));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain(
      "(182k tokens would be re-read)",
    );
    h.handle.dispose();
  });

  it("draws no empty parenthetical when the cold gate carries no detail", async () => {
    const h = mount(coldGateRefusal(""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).not.toContain("(");
    h.handle.dispose();
  });

  it("labels the cold-gate refusal with its own arm, not noSession", async () => {
    const h = mount(coldGateRefusal(""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.getAttribute("data-arm")).toBe("coldGate");
    h.handle.dispose();
  });

  it("keeps the words in the box through a cold-gate refusal", async () => {
    const h = mount(coldGateRefusal(""));
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });

  it("tells a not-deliverable bubble prompt that this agent has no route", async () => {
    const h = mount(bubbleRefusal("notDeliverable", ""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain(
      "cannot be prompted directly",
    );
    h.handle.dispose();
  });

  it("tells an agent-busy bubble prompt to resubmit once that turn settles", async () => {
    const h = mount(bubbleRefusal("agentBusy", ""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain(
      "resubmit once it settles",
    );
    h.handle.dispose();
  });

  it("draws the shim's own account after the stem when the refusal carries one", async () => {
    const h = mount(bubbleRefusal("agentBusy", "agent 'reviewer' is mid-turn"));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).toContain(
      "(agent 'reviewer' is mid-turn)",
    );
    h.handle.dispose();
  });

  it("draws no empty parenthetical when the bubble refusal carries no detail", async () => {
    const h = mount(bubbleRefusal("agentBusy", ""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.textContent).not.toContain("(");
    h.handle.dispose();
  });

  it("labels a bubble refusal with the SubmitPromptError arm it arrived on", async () => {
    const h = mount(bubbleRefusal("notDeliverable", ""));
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.getAttribute("data-arm")).toBe(
      "bubbleRefused",
    );
    h.handle.dispose();
  });

  it("refuses a bubble refusal whose kind sets no arm rather than inventing words", async () => {
    const h = mount(bubbleRefusalUnsetKind);
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")).toBeNull();
    h.handle.dispose();
  });

  it("names the kind oneof in the card it files for an unset bubble kind", async () => {
    const h = mount(bubbleRefusalUnsetKind);
    await sendText(h, "hello");
    const kind = h.reports[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? kind.value.frameHead : undefined).toBe(
      "SubmitPromptBubbleRefused.kind",
    );
    h.handle.dispose();
  });

  it("keeps the words in the box when a bubble refusal's kind is unset", async () => {
    const h = mount(bubbleRefusalUnsetKind);
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });

  it("draws nothing for an error whose reason is unset, because every refusal is typed", async () => {
    // The view refusal travels up out of the submission rather than being drawn
    // as a sentence this end invented; the box is left exactly as it was.
    const h = mount(unsetError);
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")).toBeNull();
    h.handle.dispose();
  });

  it("files a frame_undecodable card for a reason it cannot read", async () => {
    // The refusal is not drawn as a sentence this end invented, but it is not
    // lost either: the owning layer files the card that says a frame could not
    // be read, exactly as an unreadable push does.
    const h = mount(unsetError);
    await sendText(h, "hello");
    expect(h.reports[0]?.kind.case).toBe("frameUndecodable");
    h.handle.dispose();
  });

  it("names the refusing field in the card it files for an unset reason", async () => {
    const h = mount(unsetError);
    await sendText(h, "hello");
    const kind = h.reports[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? kind.value.frameHead : undefined).toBe(
      "SubmitPromptError.reason",
    );
    h.handle.dispose();
  });

  it("keeps the words in the box when the reason is unset", async () => {
    const h = mount(unsetError);
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });

  it("draws a transport failure at the composer", async () => {
    const h = mount(turnSuccess, { throws: new Error("no daemon") });
    await sendText(h, "hello");
    expect(h.host.querySelector(".composer-refusal")?.getAttribute("data-arm")).toBe("transport");
    h.handle.dispose();
  });

  it("keeps the text through a transport failure", async () => {
    const h = mount(turnSuccess, { throws: new Error("no daemon") });
    await sendText(h, "hello");
    expect(h.input.value).toBe("hello");
    h.handle.dispose();
  });

  it("clears a standing refusal when the next submission is attempted", async () => {
    const h = mount(mergingError);
    await sendText(h, "hello");
    expect(h.host.querySelectorAll(".composer-refusal")).toHaveLength(1);
    await sendText(h, "hello");
    expect(h.host.querySelectorAll(".composer-refusal")).toHaveLength(1);
    h.handle.dispose();
  });
});

describe("restoring a dropped prompt", () => {
  it("restores the words into an empty box", () => {
    const h = mount();
    document.dispatchEvent(
      new CustomEvent(DROPPED_EVENT, { detail: { text: "the dropped words" } }),
    );
    expect(h.input.value).toBe("the dropped words");
    h.handle.dispose();
  });

  it("never overwrites a draft the user is typing", () => {
    const h = mount();
    h.input.value = "my draft";
    document.dispatchEvent(new CustomEvent(DROPPED_EVENT, { detail: { text: "dropped" } }));
    expect(h.input.value).toBe("my draft");
    h.handle.dispose();
  });

  it("stops listening once disposed", () => {
    const h = mount();
    h.handle.dispose();
    document.dispatchEvent(new CustomEvent(DROPPED_EVENT, { detail: { text: "dropped" } }));
    expect(h.input.value).toBe("");
  });
});

describe("a command the daemon acted on without minting a turn", () => {
  it("clears the box, because the words are spent", async () => {
    const h = mount(actedSuccess);
    await sendText(h, "/model opus");
    expect(h.input.value).toBe("");
    h.handle.dispose();
  });

  it("draws no refusal, because the act is an answer and not a failure", async () => {
    const h = mount(actedSuccess);
    await sendText(h, "/model opus");
    expect(h.host.querySelector(".composer-refusal")).toBeNull();
    h.handle.dispose();
  });

  it("hands no panel over, because the act carries none", async () => {
    const h = mount(actedSuccess);
    await sendText(h, "/model opus");
    expect(h.panels).toEqual([]);
    h.handle.dispose();
  });

  it("records no turn, because the act minted none", async () => {
    const h = mount(actedSuccess);
    await sendText(h, "/model opus");
    expect(h.handle.lastTurn()).toBeUndefined();
    h.handle.dispose();
  });
});

describe("the inert handle", () => {
  it("answers no last turn, because it never sent one", () => {
    // ARRANGE: production's root composer is Emacs's, so this one draws nothing.
    const h = mount(turnSuccess, { composerEnabled: false });
    // ACT / ASSERT
    expect(h.handle.lastTurn()).toBeUndefined();
    h.handle.dispose();
  });
});

describe("the gate's own wording", () => {
  it("says 'composer closed' when the gate shut without naming a reason", () => {
    // ARRANGE
    const h = mount();
    // ACT
    h.gate.set("closed");
    // ASSERT
    expect(h.host.querySelector(".composer-notice")?.textContent).toBe("composer closed");
    h.handle.dispose();
  });
});

describe("a command panel that sets no arm", () => {
  const emptyPanel = (): SubmitPromptResponse =>
    create(SubmitPromptResponseSchema, {
      result: { case: "success", value: { outcome: { case: "commandPanel", value: {} } } },
    });

  it("still hands the panel to the host rather than drawing a refusal", async () => {
    // ARRANGE
    const h = mount(emptyPanel);
    // ACT
    await sendText(h, "/status");
    // ASSERT: the panel's own emptiness is the panel mount's business, not this
    // component's — the composer's job was to pass it on.
    expect(h.panels.length).toBe(1);
    h.handle.dispose();
  });

  it("spends the words, because the daemon accepted them", async () => {
    const h = mount(emptyPanel);
    await sendText(h, "/status");
    expect(h.input.value).toBe("");
    h.handle.dispose();
  });
});

// ---------------------------------------------------------------------------
// THE ARMS A NEWER DAEMON COULD SET. The generated oneof drops a case this
// build has no descriptor for, so the wording is reached directly with the
// shape a future schema would produce.
// ---------------------------------------------------------------------------

describe("submitPromptRefusal: the unknown arm", () => {
  it("refuses a refusal reason this build cannot word", () => {
    expect(() =>
      submitPromptRefusal({ case: "quotaExhausted", value: {} } as never),
    ).toThrow(MalformedView);
  });

  it("names the arm it could not word, so the log says which", () => {
    expect(() => submitPromptRefusal({ case: "quotaExhausted", value: {} } as never)).toThrow(
      /arm 'quotaExhausted' is not one this build can draw/,
    );
  });

  it("refuses a bubble refusal whose kind this build cannot word", () => {
    expect(() =>
      submitPromptRefusal({
        case: "bubbleRefused",
        value: { detail: "", kind: { case: "agentAsleep", value: {} } },
      } as never),
    ).toThrow(/arm 'agentAsleep' is not one this build can draw/);
  });
});
