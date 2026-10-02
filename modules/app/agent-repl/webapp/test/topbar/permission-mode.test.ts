// @vitest-environment jsdom
import { createControl, type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetPermissionModeErrorSchema,
  SetPermissionModeResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_permission_mode_pb";
import {
  TopbarPermissionModeOptionSchema,
  TopbarPermissionModePickerSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawTopbarPermissionModePicker,
  pickPermissionMode,
} from "../../src/topbar/permission-mode.js";
import { oneofArms } from "../arms.js";
import { RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";

const picker = (
  current = { mode: "auto", displayName: "auto" },
  options: Array<{ mode: string; displayName: string }> = [
    { mode: "acceptEdits", displayName: "accept edits" },
  ],
) => create(TopbarPermissionModePickerSchema, { current, options });

function mountPicker(tc: ReturnType<typeof topbarContext>["tc"], host: HTMLElement, view = picker()) {
  host.append(drawTopbarPermissionModePicker(view, tc));
  return host.querySelector<Control>(".topbar-mode-button")!;
}

describe("drawTopbarPermissionModePicker", () => {
  it("shows the mode in force by its display name", () => {
    const { host, tc } = topbarContext();
    expect(mountPicker(tc, host).textContent).toBe("auto");
  });

  it("carries the mode's wire spelling as a hook", () => {
    const { host, tc } = topbarContext();
    expect(mountPicker(tc, host).getAttribute("data-mode")).toBe("auto");
  });

  // A SESSION STARTED BEFORE THE 2026-09-14 AUTO RULING still runs under the
  // vendor's `default`, and the daemon serves that as the current value while
  // leaving it out of the options. The picker must state it rather than draw a
  // mode nobody is running.
  it("states a live default as the mode in force even though nothing offers it", () => {
    const { host, tc } = topbarContext();
    const button = mountPicker(
      tc,
      host,
      picker({ mode: "default", displayName: "default" }, [{ mode: "auto", displayName: "auto" }]),
    );
    expect(button.textContent).toBe("default");
  });

  it("offers no row for a live default", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const button = mountPicker(
      tc,
      host,
      picker({ mode: "default", displayName: "default" }, [{ mode: "auto", displayName: "auto" }]),
    );
    // ACT
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    const modes = Array.from(openPanel(host)!.querySelectorAll("[data-mode-option]")).map((el) =>
      el.getAttribute("data-mode-option"),
    );
    expect(modes).toEqual(["auto"]);
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

  it("gives the picked option back after a refusal, so it can be picked again", async () => {
    // ARRANGE: a disabled control refuses every click, so an option left
    // disabled after a refusal could never be picked again.
    const { host, tc } = topbarContext(
      appContext({
        setPermissionMode: () =>
          create(SetPermissionModeResponseSchema, {
            result: { case: "error", value: { cause: { case: "noSession", value: {} } } },
          }),
      }),
    );
    const button = mountPicker(tc, host);
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ACT
    openPanel(host)!.querySelector<HTMLElement>("[data-mode-option]")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    // ASSERT
    expect(openPanel(host)!.querySelector("[data-mode-option]")?.getAttribute("aria-disabled")).toBeNull();
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

  it("states a transport failure at the picker when the pick never reached the daemon", async () => {
    // ARRANGE / ACT
    const host = await pick(() => {
      throw new Error("no route to the daemon");
    });
    // ASSERT
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("re-throws a failure that is not an unreadable view, rather than drawing it as a refusal", async () => {
    // ARRANGE: the reveal layer itself throws while closing on success. That is
    // machinery of this end, not an answer from the daemon, so it escapes with
    // no sentence invented at the control.
    const { host, tc } = topbarContext(
      appContext({
        setPermissionMode: () =>
          create(SetPermissionModeResponseSchema, { result: { case: "success", value: {} } }),
      }),
    );
    tc.reveals.close = () => {
      throw new Error("the reveal layer is gone");
    };
    const wrap = document.createElement("div");
    host.append(wrap);
    const button = createControl();
    const row = createControl();
    const option = create(TopbarPermissionModeOptionSchema, {
      mode: "acceptEdits",
      displayName: "accept edits",
    });
    // ACT / ASSERT
    await expect(pickPermissionMode(option, tc, wrap, button, row)).rejects.toThrow(
      "the reveal layer is gone",
    );
  });

  it("leaves the picker button usable after a failure it re-throws", async () => {
    // ARRANGE
    const { host, tc } = topbarContext(
      appContext({
        setPermissionMode: () =>
          create(SetPermissionModeResponseSchema, { result: { case: "success", value: {} } }),
      }),
    );
    tc.reveals.close = () => {
      throw new Error("the reveal layer is gone");
    };
    const wrap = document.createElement("div");
    host.append(wrap);
    const button = createControl();
    const row = createControl();
    const option = create(TopbarPermissionModeOptionSchema, {
      mode: "acceptEdits",
      displayName: "accept edits",
    });
    // ACT
    await pickPermissionMode(option, tc, wrap, button, row).catch(() => undefined);
    // ASSERT
    expect(button.disabled).toBe(false);
  });

  it("refuses an error naming no cause", async () => {
    const host = await pick(() =>
      create(SetPermissionModeResponseSchema, {
        result: { case: "error", value: create(SetPermissionModeErrorSchema, {}) },
      }),
    );
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(host.querySelector(".refusal")).toBeNull();
  });
});

describe("the permission-mode picker with no session behind it", () => {
  it("draws the dash in its own slot rather than vanishing", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const cell = drawTopbarPermissionModePicker(undefined, tc);
    // ASSERT
    expect(cell.getAttribute("data-no-session")).toBe("mode");
  });

  it("offers no reveal, because there is nothing to pick", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(drawTopbarPermissionModePicker(undefined, tc));
    // ACT
    host.querySelector(".topbar-mode")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });
});
