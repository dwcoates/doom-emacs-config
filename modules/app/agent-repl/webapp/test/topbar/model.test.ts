// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetModelErrorSchema,
  SetModelResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_model_pb";
import {
  AgentEffortLevel,
  ModelMarker,
  ModelMarkerSchema,
  ModelOptionSchema,
  type ModelOption,
} from "../../../proto/gen/ts/conversation/v1/api_pb";
import { TopbarModelSelectorSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  COLD_ATTENTION_ATTRIBUTE,
  COLD_ATTENTION_VALUE,
  COLD_NOTICE_ATTRIBUTE,
  COLD_REFUSAL_SENTENCE,
  MODEL_PLACEHOLDER,
  SELECTED_MODEL_ATTRIBUTE,
  SELECTED_OPTION_ATTRIBUTE,
  drawModelCapabilities,
  drawTopbarModelSelector,
  effortLevelName,
  pickModel,
  offerableOptions,
  routeToColdGate,
  syntheticMarkerLiteral,
} from "../../src/topbar/model.js";
import { oneofArms } from "../arms.js";
import { RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";

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

  it("refuses to guess the spelling when the schema stops carrying it", () => {
    // ARRANGE: the enum value loses its option block, which is the only place
    // the literal lives. Restored in the same test, since the schema object is
    // shared by every file in this worker.
    const value = ModelMarkerSchema.value[ModelMarker.SYNTHETIC] as { proto: { options?: unknown } };
    const options = value.proto.options;
    try {
      value.proto.options = undefined;
      // ACT / ASSERT
      expect(() => syntheticMarkerLiteral()).toThrow(
        "conversation.v1.ModelMarker.SYNTHETIC carries no model_marker_literal",
      );
    } finally {
      value.proto.options = options;
    }
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

  it("names the model in force by its echo token, which a shared display name cannot", () => {
    // ARRANGE: two catalog rows spelled the same on screen, one of them chosen.
    const { host, tc } = topbarContext();
    const view = selector({
      selected: option("opus-5-1m", { displayName: "Opus 5" }),
      options: [option("opus-5", { displayName: "Opus 5" }), option("opus-5-1m", { displayName: "Opus 5" })],
    });
    // ACT
    mountSelector(tc, host, view);
    // ASSERT
    expect(host.querySelector(".topbar-model")!.getAttribute(SELECTED_MODEL_ATTRIBUTE)).toBe("opus-5-1m");
  });

  it("carries no model name at all when the daemon reports no selection", () => {
    // ARRANGE / ACT
    const { host, tc } = topbarContext();
    mountSelector(tc, host, selector({ options: [option("a")] }));
    // ASSERT: absent, not empty — an empty name would read as a nameless model.
    expect(host.querySelector(".topbar-model")!.hasAttribute(SELECTED_MODEL_ATTRIBUTE)).toBe(false);
  });

  it("marks the offered row that is the current selection", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const button = mountSelector(
      tc,
      host,
      selector({ selected: option("b"), options: [option("a"), option("b")] }),
    );
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    const marked = Array.from(openPanel(host)!.querySelectorAll(`[${SELECTED_OPTION_ATTRIBUTE}]`)).map(
      (el) => el.getAttribute("data-model-option"),
    );
    expect(marked).toEqual(["b"]);
  });

  it("refuses a selection carrying no model rather than drawing an unnamed chip", () => {
    // ARRANGE: a served selection whose model submessage never arrived.
    const { host, tc } = topbarContext();
    const view = selector({ options: [option("a")] });
    view.selected = create(ModelOptionSchema, { displayName: "Opus 5" });
    // ACT / ASSERT
    expect(() => mountSelector(tc, host, view)).toThrow("TopbarModelSelector.selected.model");
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

  it("draws the option's capability tags in its row", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const withCaps = create(ModelOptionSchema, {
      model: { name: "opus" },
      displayName: "Opus 5",
      capabilities: {
        supportsFastMode: true,
        effortSupport: { case: "effortUnsupported", value: {} },
      },
    });
    const button = mountSelector(tc, host, selector({ options: [withCaps] }));
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.querySelector(".topbar-model-tags")?.textContent).toBe("fast");
  });

  it("draws no tag row for an option the daemon served no capabilities for", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const button = mountSelector(tc, host, selector({ options: [option("opus")] }));
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.querySelector(".topbar-model-tags")).toBeNull();
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

  it("skips an option whose model name is empty, which names no model either", () => {
    expect(offerableOptions([option(""), option("opus")]).map((o) => o.model?.name)).toEqual(["opus"]);
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

  it("tags adaptive thinking when the vendor declares it", () => {
    expect(
      drawModelCapabilities(
        caps({
          supportsAdaptiveThinking: true,
          effortSupport: { case: "effortUnsupported", value: {} },
        }),
      ).textContent,
    ).toContain("adaptive");
  });

  it("draws no adaptive tag for a model that does not declare it", () => {
    expect(
      drawModelCapabilities(
        caps({
          supportsAdaptiveThinking: false,
          effortSupport: { case: "effortUnsupported", value: {} },
        }),
      ).textContent,
    ).not.toContain("adaptive");
  });

  it("refuses capabilities naming no effort arm", () => {
    expect(() => drawModelCapabilities(caps({}))).toThrow(MalformedView);
  });

  it("refuses an effort-support arm this build cannot draw", () => {
    // ARRANGE: a newer daemon's effort arm, reaching a build with no case for it.
    const capabilities = caps({ effortSupport: { case: "effortUnsupported", value: {} } });
    (capabilities.effortSupport as { case: string }).case = "effortInherited";
    // ACT / ASSERT
    expect(() => drawModelCapabilities(capabilities)).toThrow(
      /ModelCapabilities.effort_support.*effortInherited/,
    );
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
    // The footer is the cold refusal's fallback surface, and `topbarContext`
    // owns the body — so it goes in after the context, not before it.
    const footer = document.createElement("div");
    footer.setAttribute("data-component", "footer");
    document.body.append(footer);
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
    cold: {},
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

  it("files a response with no result arm as machinery, not a refusal", async () => {
    // ARRANGE: an unset `result` is a MALFORMED VIEW arriving on a click. It is
    // the same condition an unreadable push is, so it is reported once and
    // NOTHING is drawn at the control: there is no refusal to state.
    const sink = new RecordingSink();
    const ctx = appContext({ setModel: () => create(SetModelResponseSchema, {}) }, sink);
    const { host, tc } = topbarContext(ctx);
    const button = mountSelector(tc, host, selector({ options: [option("opus")] }));
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-model-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toContain("frameUndecodable");
  });

  it("draws no refusal for a response with no result arm", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const ctx = appContext({ setModel: () => create(SetModelResponseSchema, {}) }, sink);
    const { host, tc } = topbarContext(ctx);
    const button = mountSelector(tc, host, selector({ options: [option("opus")] }));
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-model-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("clears the previous refusal before the next pick", async () => {
    // ARRANGE: a stale refusal beside a control the reader just clicked again
    // reads as the answer to the NEW click.
    let answers = 0;
    const ctx = appContext({
      setModel: () => {
        answers += 1;
        return answers === 1
          ? create(SetModelResponseSchema, {
              result: { case: "error", value: { cause: { case: "noSession", value: {} } } },
            })
          : create(SetModelResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const { host, tc } = topbarContext(ctx);
    const button = mountSelector(tc, host, selector({ options: [option("opus")] }));
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    const clickOption = (): void => {
      openPanel(host)!
        .querySelector("[data-model-option]")!
        .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    };
    clickOption();
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ACT: the reveal stays open on a refusal, so the second pick is one click.
    clickOption();
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("states a transport failure at the selector when the pick never reached the daemon", async () => {
    // ARRANGE / ACT
    const host = await pick(() => {
      throw new Error("no route to the daemon");
    });
    // ASSERT
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("re-throws a page with nowhere to route the cold refusal, rather than wording it twice", async () => {
    // ARRANGE: the cold arm routes to the gate row, and a page with neither a
    // gate row nor a footer has nowhere to put it. That is this end's own
    // machinery failing, not an unreadable view, so it escapes.
    const ctx = appContext({
      setModel: () =>
        create(SetModelResponseSchema, {
          result: { case: "error", value: { cause: { case: "cold", value: {} } } },
        }),
    });
    const { host, tc } = topbarContext(ctx);
    const wrap = document.createElement("div");
    host.append(wrap);
    const button = document.createElement("button");
    const row = document.createElement("button");
    // ACT / ASSERT
    await expect(pickModel(option("opus"), tc, wrap, button, row)).rejects.toThrow(
      "the page has neither a cold gate row nor a footer to notice on",
    );
  });

  it("still states the cold refusal at the selector before that page failure escapes", async () => {
    // ARRANGE
    const ctx = appContext({
      setModel: () =>
        create(SetModelResponseSchema, {
          result: { case: "error", value: { cause: { case: "cold", value: {} } } },
        }),
    });
    const { host, tc } = topbarContext(ctx);
    const wrap = document.createElement("div");
    host.append(wrap);
    const button = document.createElement("button");
    const row = document.createElement("button");
    // ACT
    await pickModel(option("opus"), tc, wrap, button, row).catch(() => undefined);
    // ASSERT
    expect(wrap.querySelector(".refusal")?.getAttribute("data-arm")).toBe("cold");
  });

  it("refuses an error naming no cause", async () => {
    const host = await pick(() =>
      create(SetModelResponseSchema, {
        result: { case: "error", value: create(SetModelErrorSchema, {}) },
      }),
    );
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(host.querySelector(".refusal")).toBeNull();
  });
});

describe("routeToColdGate", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  it("marks the drawn gate card rather than noticing it on the footer", () => {
    // Arrange
    const gate = document.createElement("div");
    gate.setAttribute("data-unit", "coldGate");
    document.body.append(gate);

    // Act
    const routed = routeToColdGate(document);

    // Assert
    expect(routed).toBe("gate");
    expect(gate.getAttribute(COLD_ATTENTION_ATTRIBUTE)).toBe(COLD_ATTENTION_VALUE);
  });

  it("does not scroll the feed to the drawn gate card (removed trigger)", () => {
    // Arrange
    const gate = document.createElement("div");
    gate.setAttribute("data-unit", "coldGate");
    const blocks: string[] = [];
    gate.scrollIntoView = (arg: unknown) => {
      blocks.push((arg as { block: string }).block);
    };
    document.body.append(gate);

    // Act
    routeToColdGate(document);

    // Assert -- the user owns the scroll; the mark is the whole routing.
    expect(blocks).toEqual([]);
  });

  it("notices the gate on the footer when no gate card is drawn", () => {
    // Arrange
    const footer = document.createElement("div");
    footer.setAttribute("data-component", "footer");
    document.body.append(footer);

    // Act
    const routed = routeToColdGate(document);

    // Assert
    expect(routed).toBe("notice");
    expect(footer.querySelector(`[${COLD_NOTICE_ATTRIBUTE}]`)?.textContent).toBe(
      COLD_REFUSAL_SENTENCE,
    );
  });

  it("leaves one notice behind when the picker is refused twice", () => {
    // Arrange
    const footer = document.createElement("div");
    footer.setAttribute("data-component", "footer");
    document.body.append(footer);

    // Act
    routeToColdGate(document);
    routeToColdGate(document);

    // Assert
    expect(footer.querySelectorAll(`[${COLD_NOTICE_ATTRIBUTE}]`)).toHaveLength(1);
  });

  it("refuses a page with neither a gate card nor a footer", () => {
    // Arrange / Act / Assert
    expect(() => routeToColdGate(document)).toThrow();
  });
});

describe("the model selector with no session behind it", () => {
  it("draws the dash in its own slot rather than vanishing", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const cell = drawTopbarModelSelector(undefined, tc);
    // ASSERT
    expect(cell.getAttribute("data-no-session")).toBe("model");
  });

  it("offers no reveal, because there is nothing to pick", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(drawTopbarModelSelector(undefined, tc));
    // ACT
    host.querySelector(".topbar-model")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });
});
