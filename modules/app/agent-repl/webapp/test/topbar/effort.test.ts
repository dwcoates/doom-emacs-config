// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetEffortErrorSchema,
  SetEffortResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_effort_pb";
import { AgentEffortLevel } from "../../../proto/gen/ts/conversation/v1/api_pb";
import {
  TopbarEffortOptionSchema,
  TopbarEffortSelectorSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  EFFORT_TOOLTIP,
  EFFORT_UNSUPPORTED_TOOLTIP,
  drawTopbarEffortSelector,
  effortToken,
  pickEffort,
} from "../../src/topbar/effort.js";
import { NO_SESSION_DASH } from "../../src/topbar/no-session.js";
import { oneofArms } from "../arms.js";
import { RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";

const option = (level: AgentEffortLevel, displayName: string) =>
  create(TopbarEffortOptionSchema, { level, displayName });

const LOW = option(AgentEffortLevel.LOW, "low");
const MEDIUM = option(AgentEffortLevel.MEDIUM, "medium");
const HIGH = option(AgentEffortLevel.HIGH, "high");

const supported = (current = MEDIUM, options = [LOW, MEDIUM, HIGH]) =>
  create(TopbarEffortSelectorSchema, {
    support: { case: "supported", value: { current, options } },
  });

function mountSelector(tc: ReturnType<typeof topbarContext>["tc"], host: HTMLElement, view = supported()) {
  host.append(drawTopbarEffortSelector(view, tc));
  return host.querySelector<HTMLButtonElement>(".topbar-effort-button")!;
}

function openAndPick(host: HTMLElement, button: HTMLButtonElement, level: string): void {
  button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  openPanel(host)!
    .querySelector(`[data-effort-option="${level}"]`)!
    .dispatchEvent(new MouseEvent("click", { bubbles: true }));
}

describe("drawTopbarEffortSelector", () => {
  it("shows the level in force by its display name", () => {
    // Arrange
    const { host, tc } = topbarContext();
    // Act
    const button = mountSelector(tc, host);
    // Assert
    expect(button.textContent).toBe("medium");
  });

  it("names the level in force on the control", () => {
    // Arrange
    const { host, tc } = topbarContext();
    // Act
    mountSelector(tc, host);
    // Assert
    expect(host.querySelector(".topbar-effort")?.getAttribute("data-effort")).toBe("MEDIUM");
  });

  it("carries the owner's hover copy", () => {
    // Arrange
    const { host, tc } = topbarContext();
    // Act
    mountSelector(tc, host);
    // Assert
    expect(host.querySelector<HTMLElement>(".topbar-effort")?.title).toBe(EFFORT_TOOLTIP);
  });

  it("lists exactly the served levels, in the served order", () => {
    // Arrange
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host, supported(MEDIUM, [HIGH, MEDIUM]));
    // Act
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    const levels = Array.from(openPanel(host)!.querySelectorAll("[data-effort-option]")).map((el) =>
      el.getAttribute("data-effort-option"),
    );
    expect(levels).toEqual(["HIGH", "MEDIUM"]);
  });

  it("marks the row that is the level in force", () => {
    // Arrange
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host);
    // Act
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    const selected = Array.from(openPanel(host)!.querySelectorAll("[data-selected]")).map((el) =>
      el.getAttribute("data-effort-option"),
    );
    expect(selected).toEqual(["MEDIUM"]);
  });

  it("draws the no-session dash when absent", () => {
    // Arrange
    const { tc } = topbarContext();
    // Act
    const drawn = drawTopbarEffortSelector(undefined, tc);
    // Assert
    expect([drawn.getAttribute("data-no-session"), drawn.textContent]).toEqual(["effort", NO_SESSION_DASH]);
  });

  it("draws a dash and no dropdown for a model that takes no level", () => {
    // Arrange
    const { host, tc } = topbarContext();
    const view = create(TopbarEffortSelectorSchema, { support: { case: "unsupported", value: {} } });
    // Act
    host.append(drawTopbarEffortSelector(view, tc));
    host.querySelector<HTMLElement>(".topbar-effort")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    const drawn = host.querySelector<HTMLElement>("[data-effort-unsupported]");
    expect([drawn?.textContent, drawn?.title, openPanel(host)]).toEqual([
      NO_SESSION_DASH,
      EFFORT_UNSUPPORTED_TOOLTIP,
      null,
    ]);
  });

  it("refuses a selector with no support arm", () => {
    // Arrange
    const { tc } = topbarContext();
    // Act / Assert
    expect(() => drawTopbarEffortSelector(create(TopbarEffortSelectorSchema, {}), tc)).toThrow(MalformedView);
  });

  it("refuses a supported selector carrying no current level", () => {
    // Arrange
    const { tc } = topbarContext();
    const view = create(TopbarEffortSelectorSchema, { support: { case: "supported", value: { options: [LOW] } } });
    // Act / Assert
    expect(() => drawTopbarEffortSelector(view, tc)).toThrow(MalformedView);
  });
});

describe("effortToken", () => {
  it("is the enum's own name", () => {
    // Arrange / Act / Assert
    expect(effortToken(HIGH)).toBe("HIGH");
  });

  it("refuses UNSPECIFIED, which the contract never serves", () => {
    // Arrange / Act / Assert
    expect(() => effortToken(option(AgentEffortLevel.UNSPECIFIED, ""))).toThrow(MalformedView);
  });

  it("refuses a level this build's enum does not carry", () => {
    // Arrange / Act / Assert
    expect(() => effortToken(option(99 as AgentEffortLevel, "ultra"))).toThrow(MalformedView);
  });
});

describe("the pick", () => {
  async function pick(
    setEffort: () => ReturnType<typeof create<typeof SetEffortResponseSchema>>,
  ): Promise<HTMLElement> {
    const { host, tc } = topbarContext(appContext({ setEffort }));
    openAndPick(host, mountSelector(tc, host), "HIGH");
    await new Promise((resolve) => setTimeout(resolve, 0));
    return host;
  }

  it("echoes the served level verbatim", async () => {
    // Arrange
    let sent = AgentEffortLevel.UNSPECIFIED;
    const { host, tc } = topbarContext(
      appContext({
        setEffort: (req) => {
          sent = req.effort;
          return create(SetEffortResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    // Act
    openAndPick(host, mountSelector(tc, host), "HIGH");
    await new Promise((resolve) => setTimeout(resolve, 0));
    // Assert
    expect(sent).toBe(AgentEffortLevel.HIGH);
  });

  it("draws nothing on success — the new level arrives on the topbar stream", async () => {
    // Arrange / Act
    const host = await pick(() => create(SetEffortResponseSchema, { result: { case: "success", value: {} } }));
    // Assert
    expect([host.querySelector(".refusal"), host.querySelector(".topbar-effort-button")?.textContent]).toEqual([
      null,
      "medium",
    ]);
  });

  const causes: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
    noSession: {},
    notSupported: {},
    vendorRefused: { detail: "no" },
  };

  for (const arm of oneofArms(SetEffortErrorSchema, "cause")) {
    it(`states the ${arm} refusal at the selector`, async () => {
      // Arrange / Act
      const host = await pick(() =>
        create(SetEffortResponseSchema, {
          result: { case: "error", value: { cause: { case: arm, value: causes[arm] } as never } },
        }),
      );
      // Assert
      expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe(arm);
    });
  }

  it("states a transport failure at the selector when the pick never reached the daemon", async () => {
    // Arrange / Act
    const host = await pick(() => {
      throw new Error("no route to the daemon");
    });
    // Assert
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("files a response with no result arm as machinery, not a refusal", async () => {
    // Arrange
    const sink = new RecordingSink();
    const { host, tc } = topbarContext(
      appContext({ setEffort: () => create(SetEffortResponseSchema, {}) }, sink),
    );
    // Act
    openAndPick(host, mountSelector(tc, host), "HIGH");
    await new Promise((resolve) => setTimeout(resolve, 0));
    // Assert
    expect([sink.reported.map((k) => k.kind.case), host.querySelector(".refusal")]).toEqual([
      ["frameUndecodable"],
      null,
    ]);
  });

  it("clears the previous refusal before the next pick", async () => {
    // Arrange
    let answers = 0;
    const { host, tc } = topbarContext(
      appContext({
        setEffort: () => {
          answers += 1;
          return answers === 1
            ? create(SetEffortResponseSchema, {
                result: { case: "error", value: { cause: { case: "noSession", value: {} } } },
              })
            : create(SetEffortResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    openAndPick(host, mountSelector(tc, host), "HIGH");
    await new Promise((resolve) => setTimeout(resolve, 0));
    // Act
    openPanel(host)!
      .querySelector('[data-effort-option="HIGH"]')!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // Assert
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("re-throws a failure that is not an unreadable view", async () => {
    // Arrange
    const { host, tc } = topbarContext(
      appContext({
        setEffort: () => create(SetEffortResponseSchema, { result: { case: "success", value: {} } }),
      }),
    );
    tc.reveals.close = () => {
      throw new Error("the reveal layer is gone");
    };
    const wrap = document.createElement("div");
    host.append(wrap);
    // Act / Assert
    await expect(
      pickEffort(HIGH, tc, wrap, document.createElement("button"), document.createElement("button")),
    ).rejects.toThrow("the reveal layer is gone");
  });
});
