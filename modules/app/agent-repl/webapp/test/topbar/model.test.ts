// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetModelErrorSchema,
  SetModelResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_model_pb";
import {
  AgentEffortLevel,
  ModelOptionSchema,
  type ModelOption,
} from "../../../proto/gen/ts/conversation/v1/api_pb";
import { TopbarModelSelectorSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  MODEL_PLACEHOLDER,
  drawModelCapabilities,
  drawTopbarModelSelector,
  effortLevelName,
  offerableOptions,
  syntheticMarkerLiteral,
} from "../../src/topbar/model.js";
import { oneofArms } from "../arms.js";
import { appContext, openPanel, topbarContext } from "./fixtures.js";

const option = (name: string, init: Partial<{ displayName: string; description: string }> = {}) =>
  create(ModelOptionSchema, {
    model: { name },
    displayName: init.displayName ?? name,
    description: init.description ?? "",
  });

const selector = (init: { selected?: ModelOption; options?: ModelOption[] }) =>
  create(TopbarModelSelectorSchema, {
    selected: init.selected,
    options: init.options ?? [],
  });

/** Mount the selector on the host so its anchor is findable. */
function mountSelector(tc: ReturnType<typeof topbarContext>["tc"], host: HTMLElement, view = selector({ options: [option("opus")] })) {
  host.append(drawTopbarModelSelector(view, tc));
  return host.querySelector<HTMLButtonElement>(".topbar-model-button")!;
}

describe("syntheticMarkerLiteral", () => {
  it("reads the spelling off the schema rather than restating it", () => {
    expect(syntheticMarkerLiteral()).toBe("<synthetic>");
  });
});

describe("drawTopbarModelSelector", () => {
  it("shows the selected option's display name", () => {
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host, selector({ selected: option("opus", { displayName: "Opus 5" }) }));
    expect(button.textContent).toBe("Opus 5");
  });

  it("shows the placeholder when the daemon reports no selection", () => {
    const { host, tc } = topbarContext();
    expect(mountSelector(tc, host, selector({})).textContent).toBe(MODEL_PLACEHOLDER);
  });

  it("marks the unselected state, so it can read as an invitation", () => {
    const { host, tc } = topbarContext();
    expect(mountSelector(tc, host, selector({})).hasAttribute("data-unselected")).toBe(true);
  });

  it("lists exactly the served options, in the served order", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host, selector({ options: [option("a"), option("b")] }));
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    const names = Array.from(openPanel(host)!.querySelectorAll("[data-model-option]")).map((el) =>
      el.getAttribute("data-model-option"),
    );
    expect(names).toEqual(["a", "b"]);
  });

  it("draws a non-empty description", () => {
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host, selector({ options: [option("a", { description: "fast one" })] }));
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.textContent).toContain("fast one");
  });

  it("draws no description row for an empty description", () => {
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host, selector({ options: [option("a")] }));
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.querySelector(".topbar-model-description")).toBeNull();
  });
});

describe("offerableOptions", () => {
  it("keeps a real model", () => {
    expect(offerableOptions([option("opus")]).length).toBe(1);
  });

  it("skips the synthetic marker, which is not a model and cannot be selected", () => {
    expect(offerableOptions([option("<synthetic>"), option("opus")]).map((o) => o.model?.name)).toEqual(
      ["opus"],
    );
  });

  it("refuses an option carrying no model", () => {
    expect(() => offerableOptions([create(ModelOptionSchema, { displayName: "x" })])).toThrow(
      MalformedView,
    );
  });
});

describe("drawModelCapabilities", () => {
  const caps = (init: Record<string, unknown>) =>
    create(ModelOptionSchema, { model: { name: "a" }, capabilities: init as never }).capabilities!;

  it("tags fast mode when the vendor declares it", () => {
    expect(
      drawModelCapabilities(
        caps({ supportsFastMode: true, effortSupport: { case: "effortUnsupported", value: {} } }),
      ).textContent,
    ).toContain("fast");
  });

  it("tags auto mode when the vendor declares it", () => {
    expect(
      drawModelCapabilities(
        caps({ supportsAutoMode: true, effortSupport: { case: "effortUnsupported", value: {} } }),
      ).textContent,
    ).toContain("auto");
  });

  it("draws nothing for a model that takes no effort level", () => {
    expect(
      drawModelCapabilities(caps({ effortSupport: { case: "effortUnsupported", value: {} } }))
        .textContent,
    ).toBe("");
  });

  it("tags each accepted effort level", () => {
    expect(
      drawModelCapabilities(
        caps({
          effortSupport: {
            case: "effortSupported",
            value: { levels: [AgentEffortLevel.LOW, AgentEffortLevel.HIGH] },
          },
        }),
      ).textContent,
    ).toBe("lowhigh");
  });

  it("refuses capabilities naming no effort arm", () => {
    expect(() => drawModelCapabilities(caps({}))).toThrow(MalformedView);
  });
});

describe("effortLevelName", () => {
  it("refuses the unspecified value, which is never a real level", () => {
    expect(() => effortLevelName(AgentEffortLevel.UNSPECIFIED)).toThrow(MalformedView);
  });
});

describe("the pick", () => {
  /** Click the first option and let the rpc settle. */
  async function pick(
    setModel: () => ReturnType<typeof create<typeof SetModelResponseSchema>>,
  ): Promise<HTMLElement> {
    const ctx = appContext({ setModel });
    const { host, tc } = topbarContext(ctx);
    const button = mountSelector(tc, host, selector({ options: [option("opus")] }));
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-model-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    return host;
  }

  it("echoes the served AgentModel verbatim", async () => {
    // ARRANGE
    let sent = "";
    const ctx = appContext({
      setModel: (req) => {
        sent = req.model?.name ?? "";
        return create(SetModelResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const { host, tc } = topbarContext(ctx);
    const button = mountSelector(tc, host, selector({ options: [option("claude-opus-5")] }));
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-model-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(sent).toBe("claude-opus-5");
  });

  it("draws nothing on success — the new selection arrives on the stream", async () => {
    const host = await pick(() =>
      create(SetModelResponseSchema, { result: { case: "success", value: {} } }),
    );
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("closes the reveal on success, the reader's question being answered", async () => {
    const host = await pick(() =>
      create(SetModelResponseSchema, { result: { case: "success", value: {} } }),
    );
    expect(openPanel(host)).toBeNull();
  });

  const causes: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
    noSession: {},
    notInCatalog: {},
    vendorRefused: { detail: "no" },
  };

  // ENUMERATED FROM THE SCHEMA: an arm added to the proto fails this test
  // rather than quietly going undrawn.
  for (const arm of oneofArms(SetModelErrorSchema, "cause")) {
    it(`states the ${arm} refusal at the selector`, async () => {
      const host = await pick(() =>
        create(SetModelResponseSchema, {
          result: {
            case: "error",
            value: { cause: { case: arm, value: causes[arm] } as never },
          },
        }),
      );
      expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe(arm);
    });
  }

  it("refuses an error naming no cause", async () => {
    const host = await pick(() =>
      create(SetModelResponseSchema, {
        result: { case: "error", value: create(SetModelErrorSchema, {}) },
      }),
    );
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("malformed");
  });
});
