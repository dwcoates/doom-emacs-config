// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetPermissionModeErrorSchema,
  SetPermissionModeResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_permission_mode_pb";
import { TopbarPermissionModePickerSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawTopbarPermissionModePicker } from "../../src/topbar/permission-mode.js";
import { oneofArms } from "../arms.js";
import { RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";

const picker = (
  current = { mode: "default", displayName: "default" },
  options: Array<{ mode: string; displayName: string }> = [
    { mode: "acceptEdits", displayName: "accept edits" },
  ],
) => create(TopbarPermissionModePickerSchema, { current, options });

function mountPicker(tc: ReturnType<typeof topbarContext>["tc"], host: HTMLElement, view = picker()) {
  host.append(drawTopbarPermissionModePicker(view, tc));
  return host.querySelector<HTMLButtonElement>(".topbar-mode-button")!;
}

describe("drawTopbarPermissionModePicker", () => {
  it("shows the mode in force by its display name", () => {
    const { host, tc } = topbarContext();
    expect(mountPicker(tc, host).textContent).toBe("default");
  });

  it("carries the mode's wire spelling as a hook", () => {
    const { host, tc } = topbarContext();
    expect(mountPicker(tc, host).getAttribute("data-mode")).toBe("default");
  });

  it("lists exactly the served options, in the served order", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const button = mountPicker(
      tc,
      host,
      picker(undefined, [
        { mode: "plan", displayName: "plan" },
        { mode: "acceptEdits", displayName: "accept edits" },
      ]),
    );
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    const modes = Array.from(openPanel(host)!.querySelectorAll("[data-mode-option]")).map((el) =>
      el.getAttribute("data-mode-option"),
    );
    expect(modes).toEqual(["plan", "acceptEdits"]);
  });

  it("refuses a picker carrying no current mode", () => {
    const { tc } = topbarContext();
    expect(() =>
      drawTopbarPermissionModePicker(create(TopbarPermissionModePickerSchema, {}), tc),
    ).toThrow(MalformedView);
  });
});

describe("the pick", () => {
  async function pick(
    setPermissionMode: () => ReturnType<typeof create<typeof SetPermissionModeResponseSchema>>,
  ): Promise<HTMLElement> {
    const { host, tc } = topbarContext(appContext({ setPermissionMode }));
    const button = mountPicker(tc, host);
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-mode-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    return host;
  }

  it("files a response with no result arm as machinery, not a refusal", async () => {
    // ARRANGE: an unset `result` is a malformed view arriving on a click — the
    // same condition an unreadable push is — so it is reported once and
    // nothing is drawn at the control.
    const sink = new RecordingSink();
    const { host, tc } = topbarContext(
      appContext({ setPermissionMode: () => create(SetPermissionModeResponseSchema, {}) }, sink),
    );
    const button = mountPicker(tc, host);
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-mode-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toContain("frameUndecodable");
  });

  it("draws no refusal for a response with no result arm", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { host, tc } = topbarContext(
      appContext({ setPermissionMode: () => create(SetPermissionModeResponseSchema, {}) }, sink),
    );
    const button = mountPicker(tc, host);
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-mode-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("clears the previous refusal before the next pick", async () => {
    // ARRANGE
    let answers = 0;
    const { host, tc } = topbarContext(
      appContext({
        setPermissionMode: () => {
          answers += 1;
          return answers === 1
            ? create(SetPermissionModeResponseSchema, {
                result: { case: "error", value: { cause: { case: "noSession", value: {} } } },
              })
            : create(SetPermissionModeResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    const button = mountPicker(tc, host);
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    const clickOption = (): void => {
      openPanel(host)!
        .querySelector("[data-mode-option]")!
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

  it("echoes the served spelling verbatim", async () => {
    // ARRANGE
    let sent = "";
    const { host, tc } = topbarContext(
      appContext({
        setPermissionMode: (req) => {
          sent = req.mode;
          return create(SetPermissionModeResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    const button = mountPicker(tc, host);
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector("[data-mode-option]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(sent).toBe("acceptEdits");
  });

  it("draws nothing on success — the new mode arrives on the pushed surfaces", async () => {
    const host = await pick(() =>
      create(SetPermissionModeResponseSchema, { result: { case: "success", value: {} } }),
    );
    expect(host.querySelector(".refusal")).toBeNull();
  });

  const causes: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
    modeNotServed: {},
    ungatedWithoutConsent: {},
    noSession: {},
    vendorRefused: { detail: "no" },
  };

  for (const arm of oneofArms(SetPermissionModeErrorSchema, "cause")) {
    it(`states the ${arm} refusal at the picker`, async () => {
      const host = await pick(() =>
        create(SetPermissionModeResponseSchema, {
          result: { case: "error", value: { cause: { case: arm, value: causes[arm] } as never } },
        }),
      );
      expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe(arm);
    });
  }

  it("refuses an error naming no cause", async () => {
    const host = await pick(() =>
      create(SetPermissionModeResponseSchema, {
        result: { case: "error", value: create(SetPermissionModeErrorSchema, {}) },
      }),
    );
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("malformed");
  });
});
