// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  AnswerColdGateResponseSchema,
  type AnswerColdGateRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import { FeedColdGateSchema, FeedIdSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { RosterColdGateSchema, type RosterColdGate } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import { DOCKED_CARD_CLASS, DOCK_HEIGHT_PROPERTY } from "../../src/feed/gate-dock.js";
import { GATE_ANSWERING_EVENT } from "../../src/feed/asks/cold-gate.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { createWebhostGate, GATE_BAND_PROPERTY, WEBHOST_GATE_ID, type WebhostGate } from "../../src/host/gate.js";
import { captureLogRecords, forwardedRecords } from "../log-capture.js";
import { testAppContext } from "../rpc/app-context.js";

/** A standing gate's card, TOKENS in its context. */
function card(tokens: bigint) {
  return create(FeedColdGateSchema, {
    state: {
      case: "standing",
      value: {
        contextTokens: { tokens, windowFill: 0.2 },
        lastRequest: { atMs: 0n },
        model: { model: { name: "claude-opus-5" } },
      },
    },
  });
}

/** Workspace ID's gate as the roster carries it, TOKENS in its card. */
function rosterGate(id: string, tokens = 1000n, row = `cold-gate:${id}`): RosterColdGate {
  return create(RosterColdGateSchema, {
    workspace: create(WorkspaceRefSchema, { id, dir: `/w/${id}` }),
    row: create(FeedIdSchema, { value: row }),
    gate: card(tokens),
  });
}

let el: HTMLElement;
let gate: WebhostGate;
let heights: number;
let resized: (() => void) | null;
let dismissed: string[];
let answered: AnswerColdGateRequest[];

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(0);
  el = document.createElement("div");
  document.body.replaceChildren(el);
  heights = 160;
  resized = null;
  dismissed = [];
  answered = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      answerColdGate: (req) => {
        answered.push(req);
        return create(AnswerColdGateResponseSchema, { result: { case: "success", value: {} } });
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    ticker: createTicker(),
    failures: { report: () => {}, retract: () => {} },
    workspace: create(WorkspaceRefSchema, { id: "webhost", dir: "" }),
    composerEnabled: false,
  });
  gate = createWebhostGate({
    doc: document,
    el,
    ctx,
    measure: () => heights,
    watchSize: (_el, fn) => {
      resized = fn;
      return () => {
        resized = null;
      };
    },
    dismissDigest: (id) => dismissed.push(id),
  });
});

afterEach(() => {
  gate.dispose();
  document.documentElement.style.removeProperty(GATE_BAND_PROPERTY);
  vi.useRealTimers();
});

/** The card drawn now, or null. */
function drawn(): HTMLElement | null {
  return el.querySelector<HTMLElement>(".cold-gate");
}

/** The band the pages and the sidebar end above. */
function band(): string {
  return document.documentElement.style.getPropertyValue(GATE_BAND_PROPERTY);
}

describe("the webhost's gate", () => {
  it("is the gate element, hidden until a gate stands", () => {
    expect([el.id, el.hidden]).toEqual([WEBHOST_GATE_ID, true]);
  });

  it("draws the shown workspace's gate as a docked card", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);

    // Act
    gate.setShown("a");

    // Assert
    expect([el.hidden, drawn()?.classList.contains(DOCKED_CARD_CLASS)]).toEqual([false, true]);
  });

  it("draws nothing for a shown workspace with no gate", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);

    // Act
    gate.setShown("b");

    // Assert
    expect([el.hidden, drawn()]).toEqual([true, null]);
  });

  it("draws the gate the instant its workspace is shown, before any page has loaded", () => {
    // Arrange: the roster carries the gate; no page exists in this suite at all.
    gate.setGates([rosterGate("a")]);
    gate.setShown("b");

    // Act
    gate.setShown("a");

    // Assert
    expect(drawn()).not.toBeNull();
  });

  it("hides once the shown workspace's gate leaves the roster", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);
    gate.setShown("a");

    // Act
    gate.setGates([]);

    // Assert
    expect([el.hidden, drawn()]).toEqual([true, null]);
  });

  it("keeps the card drawn while its row and facts are unchanged", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);
    gate.setShown("a");
    const first = drawn();

    // Act
    gate.setGates([rosterGate("a"), rosterGate("b")]);

    // Assert
    expect(drawn()).toBe(first);
  });

  it("redraws the card when its facts change", () => {
    // Arrange
    gate.setGates([rosterGate("a", 1000n)]);
    gate.setShown("a");
    const first = drawn();

    // Act
    gate.setGates([rosterGate("a", 2000n)]);

    // Assert
    expect([drawn() === first, el.querySelectorAll(".cold-gate").length]).toEqual([false, 1]);
  });

  it("takes the slot Emacs told for the shown workspace", () => {
    // Arrange
    gate.setSlot("a", 132);
    gate.setGates([rosterGate("a")]);

    // Act
    gate.setShown("a");

    // Assert
    expect(el.style.getPropertyValue(DOCK_HEIGHT_PROPERTY)).toBe("132px");
  });

  it("takes another workspace's slot on a switch to it", () => {
    // Arrange
    gate.setSlot("a", 132);
    gate.setSlot("b", 90);
    gate.setGates([rosterGate("a"), rosterGate("b")]);
    gate.setShown("a");

    // Act
    gate.setShown("b");

    // Assert
    expect(el.style.getPropertyValue(DOCK_HEIGHT_PROPERTY)).toBe("90px");
  });

  it("falls back to the stylesheet's slot for a workspace whose slot was never told", () => {
    // Arrange
    gate.setSlot("a", 132);
    gate.setGates([rosterGate("a"), rosterGate("b")]);
    gate.setShown("a");

    // Act
    gate.setShown("b");

    // Assert
    expect(el.style.getPropertyValue(DOCK_HEIGHT_PROPERTY)).toBe("");
  });

  it("ends the pages and the sidebar above the gate as soon as it is drawn", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);

    // Act
    gate.setShown("a");

    // Assert
    expect(band()).toBe("160px");
  });

  it("follows the gate's height as it grows for a selection", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);
    gate.setShown("a");
    heights = 284;

    // Act
    resized?.();

    // Assert
    expect(band()).toBe("284px");
  });

  it("runs the pages and the sidebar to the bottom once no gate is shown", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);
    gate.setShown("a");

    // Act
    gate.setShown("b");

    // Assert
    expect(band()).toBe("0px");
  });

  it("answers the gate for the workspace the roster names", async () => {
    // Arrange
    gate.setGates([rosterGate("a", 1000n, "cold-gate:a:vendor-1")]);
    gate.setShown("a");

    // Act
    drawn()?.querySelector<HTMLElement>('[data-cold-gate="pay"]')?.click();
    await vi.advanceTimersByTimeAsync(0);

    // Assert
    expect(answered.map((r) => [r.workspace?.id, r.gate?.value])).toEqual([["a", "cold-gate:a:vendor-1"]]);
  });

  it("closes the shown page's news digest when its gate is answered", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);
    gate.setShown("a");

    // Act
    drawn()?.dispatchEvent(new CustomEvent(GATE_ANSWERING_EVENT, { bubbles: true }));

    // Assert
    expect(dismissed).toEqual(["a"]);
  });

  it("records a roster gate missing its card as an error and draws nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const torn = rosterGate("a");
    torn.gate = undefined;

    // Act
    gate.setGates([torn]);
    gate.setShown("a");

    // Assert
    const records = await forwardedRecords(capture);
    const record = records.find((r) => r.operation === "webhost.gate.malformed");
    expect([record?.level.case, drawn()]).toEqual(["error", null]);
  });

  it("draws nothing once disposed", () => {
    // Arrange
    gate.setGates([rosterGate("a")]);
    gate.setShown("a");

    // Act
    gate.dispose();

    // Assert
    expect([el.hidden, drawn(), resized]).toEqual([true, null, null]);
  });
});
