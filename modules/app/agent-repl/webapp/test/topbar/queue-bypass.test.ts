// @vitest-environment jsdom
//
// The queue-bypass chip: its mode arms, and the context menu an off chip
// opens, drawn exactly as the topbar's other menus are and sending
// UpdateQueueBypassMode{on} with the picked row's context.
import { describe, expect, it } from "vitest";
import { create, equals } from "@bufbuild/protobuf";
import {
  UpdateQueueBypassModeErrorSchema,
  UpdateQueueBypassModeResponseSchema,
  type UpdateQueueBypassModeRequest,
  type UpdateQueueBypassModeResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_queue_bypass_mode_pb";
import {
  TopbarQueueBypassContextSchema,
  TopbarQueueBypassSchema,
  type TopbarQueueBypass,
  type TopbarQueueBypassContextOption,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { createControl, type Control } from "../../src/control.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  QUEUE_BYPASS_CONTEXT_ATTRIBUTE,
  drawTopbarQueueBypass,
  queueBypassContextToken,
} from "../../src/topbar/queue-bypass.js";
import { oneofArms } from "../arms.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { appContext, openPanel, topbarContext } from "./fixtures.js";

type ContextArm = NonNullable<TopbarQueueBypassContextOption["context"]>["context"];

/** A menu row naming ARM, labeled NAME. */
function option(arm: ContextArm, displayName: string): TopbarQueueBypassContextOption {
  return { $typeName: "frontend.v1.TopbarQueueBypassContextOption", displayName, context: create(TopbarQueueBypassContextSchema, { context: arm }) };
}

const FULL = option({ case: "fullMain", value: {} } as ContextArm, "Full main context");
const COMPACTED = option({ case: "compactedMain", value: {} } as ContextArm, "Compacted main context");
const EXISTING = option({ case: "existing", value: {} } as ContextArm, "Existing bypass context");
const EXISTING_PLUS = option({ case: "existingPlusCompactedMain", value: {} } as ContextArm, "Existing bypass plus compacted main");

/** An off chip serving OPTIONS. */
function offChip(options: TopbarQueueBypassContextOption[]): TopbarQueueBypass {
  return create(TopbarQueueBypassSchema, {
    mode: { case: "off", value: { options } },
    tooltip: { text: "the daemon's words" },
  });
}

/** An on chip. */
const ON_CHIP = create(TopbarQueueBypassSchema, { mode: { case: "on", value: {} }, tooltip: { text: "on" } });

const success = (): UpdateQueueBypassModeResponse =>
  create(UpdateQueueBypassModeResponseSchema, { result: { case: "success", value: { on: true } } });

/** Mount U and click its button, answering every UpdateQueueBypassMode with ANSWER. */
function mountAndClick(
  u: TopbarQueueBypass,
  answer: (req: UpdateQueueBypassModeRequest) => UpdateQueueBypassModeResponse = success,
): { host: HTMLElement; sent: UpdateQueueBypassModeRequest[] } {
  const sent: UpdateQueueBypassModeRequest[] = [];
  const { host, tc } = topbarContext(
    appContext({
      updateQueueBypassMode: (req) => {
        sent.push(req);
        return answer(req);
      },
    }),
  );
  host.append(drawTopbarQueueBypass(u, tc));
  host.querySelector<Control>(".topbar-queue-bypass-button")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  return { host, sent };
}

/** The open menu's rows, by context token. */
function rowTokens(host: HTMLElement): (string | null)[] {
  return [...(openPanel(host)?.querySelectorAll(".topbar-queue-bypass-option") ?? [])].map((row) =>
    row.getAttribute(QUEUE_BYPASS_CONTEXT_ATTRIBUTE),
  );
}

/** Let the pick's unary call settle. */
const settled = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

describe("the queue-bypass chip's context menu", () => {
  it("opens on an off chip's click, drawing exactly the served rows in the served order", () => {
    // Arrange / Act
    const { host, sent } = mountAndClick(offChip([FULL, COMPACTED, EXISTING, EXISTING_PLUS]));
    // Assert
    expect([rowTokens(host), sent.length]).toEqual([["fullMain", "compactedMain", "existing", "existingPlusCompactedMain"], 0]);
  });

  it("labels each row with the daemon's display name", () => {
    // Arrange / Act
    const { host } = mountAndClick(offChip([FULL, COMPACTED]));
    // Assert
    expect([...openPanel(host)!.querySelectorAll(".topbar-queue-bypass-option")].map((row) => row.textContent)).toEqual([
      "Full main context",
      "Compacted main context",
    ]);
  });

  it("draws no existing-context rows when the daemon serves none", () => {
    // Arrange / Act
    const { host } = mountAndClick(offChip([FULL, COMPACTED]));
    // Assert
    expect(rowTokens(host)).toEqual(["fullMain", "compactedMain"]);
  });

  it("draws its rows in the topbar's shared menu list", () => {
    // Arrange / Act
    const { host } = mountAndClick(offChip([FULL]));
    // Assert
    expect(openPanel(host)?.querySelector(".topbar-queue-bypass-options")?.classList.contains("list-rows")).toBe(true);
  });

  it.each([
    ["fullMain", FULL],
    ["compactedMain", COMPACTED],
    ["existing", EXISTING],
    ["existingPlusCompactedMain", EXISTING_PLUS],
  ])("a pick of %s takes the mode echoing that row's context", async (token, picked) => {
    // Arrange
    const { host, sent } = mountAndClick(offChip([FULL, COMPACTED, EXISTING, EXISTING_PLUS]));
    // Act
    openPanel(host)!
      .querySelector(`[${QUEUE_BYPASS_CONTEXT_ATTRIBUTE}="${token}"]`)!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await settled();
    // Assert
    const action = sent[0]?.action;
    expect(sent.length).toBe(1);
    expect(action?.case).toBe("on");
    expect(action?.case === "on" && equals(TopbarQueueBypassContextSchema, action.value.context!, picked.context!)).toBe(true);
  });

  it("closes the menu once the pick succeeds", async () => {
    // Arrange
    const { host } = mountAndClick(offChip([FULL]));
    // Act
    openPanel(host)!.querySelector<Control>(".topbar-queue-bypass-option")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await settled();
    // Assert
    expect(openPanel(host)).toBeNull();
  });

  /** The value each refusal arm carries. */
  const causes: Readonly<Record<string, unknown>> = {
    transferringAway: { address: "127.0.0.1:9" },
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    switchFailed: { detail: "the session would not end" },
    compactionFailed: { detail: "the summarizer failed" },
  };

  /** Pick the one served row of an off chip, refused with ARM. */
  async function refusedWith(arm: string): Promise<HTMLElement> {
    const { host } = mountAndClick(offChip([EXISTING]), () =>
      create(UpdateQueueBypassModeResponseSchema, {
        result: { case: "error", value: create(UpdateQueueBypassModeErrorSchema, { cause: { case: arm, value: causes[arm] ?? {} } } as never) },
      }),
    );
    openPanel(host)!.querySelector<Control>(".topbar-queue-bypass-option")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await settled();
    return host;
  }

  for (const arm of oneofArms(UpdateQueueBypassModeErrorSchema, "cause")) {
    it(`states the ${arm} refusal of a pick at the chip`, async () => {
      // Arrange / Act
      const host = await refusedWith(arm);
      // Assert
      expect(host.querySelector(".topbar-queue-bypass .refusal")?.getAttribute("data-arm")).toBe(arm);
    });
  }

  it.each([
    ["noBypassConversation", "this workspace has no bypass conversation to resume"],
    ["compactionFailed", "the compaction did not complete: the summarizer failed"],
  ])("words the %s refusal", async (arm, sentence) => {
    // Arrange / Act
    const host = await refusedWith(arm);
    // Assert
    expect(host.querySelector(".topbar-queue-bypass .refusal")?.textContent).toBe(sentence);
  });

  it("an on chip's click releases the mode through toggle and opens no menu", async () => {
    // Arrange / Act
    const { host, sent } = mountAndClick(ON_CHIP);
    await settled();
    // Assert
    expect([sent.map((req) => req.action.case), openPanel(host)]).toEqual([["toggle"], null]);
  });

  it("refuses a row whose context names no arm", () => {
    // Arrange
    const bare = option({ case: undefined }, "nameless");
    // Act / Assert
    expect(() => queueBypassContextToken(bare)).toThrow(MalformedView);
  });

  it("draws a row as the permission-mode picker draws its rows", () => {
    // Arrange
    const remove = installStylesheet();
    const make = (className: string): HTMLElement => {
      const element = createControl();
      element.className = className;
      document.body.append(element);
      return element;
    };
    const modeRow = make("topbar-mode-option");
    const bypassRow = make("topbar-queue-bypass-option");
    const properties = ["padding", "border", "border-radius", "font-size", "font-family", "color", "white-space", "cursor", "background"];
    // Act
    const computed = (element: HTMLElement): string[] => properties.map((property) => cascadedValue(element, property));
    // Assert
    expect(computed(bypassRow)).toEqual(computed(modeRow));
    modeRow.remove();
    bypassRow.remove();
    remove();
  });
});
