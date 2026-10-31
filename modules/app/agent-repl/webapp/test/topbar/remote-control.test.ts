// @vitest-environment jsdom
//
// The remote-control chip. One test per arm of its mode oneof, the unset and
// unnameable arms, the tooltip, the phone glyph, and the click that turns the
// mode over through UpdateRemoteControlMode{toggle} for this page's workspace.
import { type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create, equals } from "@bufbuild/protobuf";
import {
  TopbarRemoteControlSchema,
  type TopbarRemoteControl,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  UpdateRemoteControlModeErrorSchema,
  UpdateRemoteControlModeResponseSchema,
  type UpdateRemoteControlModeRequest,
  type UpdateRemoteControlModeResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_remote_control_mode_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { DISC_VIEW_BOX } from "../../src/topbar/disc-glyph.js";
import { PHONE_GLYPH_PATHS, drawTopbarRemoteControl } from "../../src/topbar/remote-control.js";
import { oneofArms } from "../arms.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { RecordingSink, WORKSPACE, appContext, topbarContext } from "./fixtures.js";

/** Draw the chip over a context whose rpcs nothing scripts. */
const draw = (u: TopbarRemoteControl): HTMLElement => drawTopbarRemoteControl(u, topbarContext().tc);

/** A chip with the given mode arm and a tooltip. */
function chip(mode: TopbarRemoteControl["mode"]): TopbarRemoteControl {
  return { ...create(TopbarRemoteControlSchema, { tooltip: { text: "the daemon's words" } }), mode };
}

const on = { case: "on", value: {} } as TopbarRemoteControl["mode"];
const off = { case: "off", value: {} } as TopbarRemoteControl["mode"];

describe("drawTopbarRemoteControl", () => {
  it("marks the mode on for the green disc", () => {
    expect(draw(chip(on)).getAttribute("data-mode")).toBe("on");
  });

  it("marks the mode off for the white disc", () => {
    expect(draw(chip(off)).getAttribute("data-mode")).toBe("off");
  });

  it("refuses a chip whose mode is unset", () => {
    expect(() => draw(chip({ case: undefined }))).toThrow(MalformedView);
  });

  it("refuses a mode arm this bundle cannot name", () => {
    expect(() => draw(chip({ case: "half", value: {} } as never))).toThrow(MalformedView);
  });

  it("carries the daemon's tooltip verbatim", () => {
    expect(draw(chip(on)).title).toBe("the daemon's words");
  });

  it("refuses a chip with no tooltip", () => {
    const bare = { ...create(TopbarRemoteControlSchema, {}), mode: on };
    expect(() => draw(bare)).toThrow(MalformedView);
  });

  it("draws the phone glyph's paths", () => {
    const paths = draw(chip(on)).querySelectorAll("svg.topbar-remote-control-glyph > path");
    expect([...paths].map((p) => p.getAttribute("d"))).toEqual([...PHONE_GLYPH_PATHS]);
  });

  it("draws its disc inside the glyph's own svg", () => {
    expect(draw(chip(on)).querySelector("svg.topbar-remote-control-glyph > circle.topbar-remote-control-disc"))
      .not.toBeNull();
  });

  it("draws the glyph on the shared disc's viewBox, the wifi glyph's size", () => {
    expect(draw(chip(on)).querySelector("svg.topbar-remote-control-glyph")?.getAttribute("viewBox")).toBe(DISC_VIEW_BOX);
  });

  it("draws the glyph inside a button", () => {
    expect(draw(chip(on)).querySelector("ar-button.topbar-remote-control-button > svg.topbar-remote-control-glyph"))
      .not.toBeNull();
  });

  it("draws no settling mark on a chip nobody clicked", () => {
    expect(draw(chip(off)).hasAttribute("data-settling")).toBe(false);
  });
});

/** A success answer. */
const success = (): UpdateRemoteControlModeResponse =>
  create(UpdateRemoteControlModeResponseSchema, { result: { case: "success", value: { on: true } } });

/** An error answer naming CAUSE. */
const refused = (cause: { case: string; value: unknown }): UpdateRemoteControlModeResponse =>
  create(UpdateRemoteControlModeResponseSchema, { result: { case: "error", value: { cause: cause as never } } });

/** Settle every microtask and the router transport's own hop. */
const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

/**
 * Mount the chip over a context whose UpdateRemoteControlMode is ANSWER,
 * click it, and let the answer land.
 */
async function clickWith(
  answer: (req: UpdateRemoteControlModeRequest) => UpdateRemoteControlModeResponse,
  failures = new RecordingSink(),
): Promise<{ host: HTMLElement; button: Control; failures: RecordingSink }> {
  const { host, tc } = topbarContext(appContext({ updateRemoteControlMode: answer }, failures));
  host.append(drawTopbarRemoteControl(chip(off), tc));
  const button = host.querySelector<Control>(".topbar-remote-control-button")!;
  button.click();
  await settle();
  return { host, button, failures };
}

/** Mount the chip over a context whose UpdateRemoteControlMode never answers, and click it. */
function clickPending(): { host: HTMLElement; button: Control } {
  const { host, tc } = topbarContext(appContext({ updateRemoteControlMode: () => new Promise(() => undefined) }));
  host.append(drawTopbarRemoteControl(chip(off), tc));
  const button = host.querySelector<Control>(".topbar-remote-control-button")!;
  button.click();
  return { host, button };
}

describe("clicking the remote-control chip", () => {
  it("sends UpdateRemoteControlMode with the toggle arm", async () => {
    // Arrange
    let sent: string | undefined;
    // Act
    await clickWith((req) => {
      sent = req.action.case;
      return success();
    });
    // Assert
    expect(sent).toBe("toggle");
  });

  it("sends this page's workspace ref", async () => {
    // Arrange
    let sent: UpdateRemoteControlModeRequest["workspace"];
    // Act
    await clickWith((req) => {
      sent = req.workspace;
      return success();
    });
    // Assert
    expect(sent !== undefined && equals(WorkspaceRefSchema, sent, WORKSPACE)).toBe(true);
  });

  it("logs the click at info with the mode the chip showed", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    await clickWith(() => success());
    // Assert
    const record = await forwardedRecord(capture, "topbar.remote-control-toggle");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ mode: "off" })]);
  });

  it("logs the landed toggle's mode at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    await clickWith(() => success());
    // Assert
    const record = await forwardedRecord(capture, "topbar.remote-control-toggled");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ on: true })]);
  });

  it("draws nothing on success, because the new mode arrives on the push", async () => {
    const { host } = await clickWith(() => success());
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("marks the chip as settling while the toggle is in flight", () => {
    const { host } = clickPending();
    expect(host.querySelector(".topbar-remote-control")?.hasAttribute("data-settling")).toBe(true);
  });

  it("clears the settling mark once the toggle landed", async () => {
    const { host } = await clickWith(() => success());
    expect(host.querySelector(".topbar-remote-control")?.hasAttribute("data-settling")).toBe(false);
  });

  it("clears the settling mark when the call failed at the transport", async () => {
    const { host } = await clickWith(() => {
      throw new Error("link dropped");
    });
    expect(host.querySelector(".topbar-remote-control")?.hasAttribute("data-settling")).toBe(false);
  });

  it("disables the button while the toggle is in flight", () => {
    const { button } = clickPending();
    expect(button.disabled).toBe(true);
  });

  it("gives the button back once the toggle landed", async () => {
    const { button } = await clickWith(() => success());
    expect(button.disabled).toBe(false);
  });

  const causes: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
    vendorRefused: { detail: "remote control is not enabled for this account" },
  };

  for (const arm of oneofArms(UpdateRemoteControlModeErrorSchema, "cause")) {
    it(`states the ${arm} refusal at the chip`, async () => {
      const { host } = await clickWith(() => refused({ case: arm, value: causes[arm] }));
      expect(host.querySelector(".topbar-remote-control > .refusal")?.getAttribute("data-arm")).toBe(arm);
    });
  }

  const sentences: Readonly<Record<string, string>> = {
    unknownWorkspace: "the daemon does not know this workspace",
    workspaceRefMismatch: "this workspace's directory disagrees with the registry's: /elsewhere",
    transferringAway: "this workspace moved to another daemon at 127.0.0.1:9",
    notYetAdopted: "the daemon has not finished adopting this workspace yet",
    vendorRefused: "the vendor refused remote control: remote control is not enabled for this account",
  };

  for (const arm of oneofArms(UpdateRemoteControlModeErrorSchema, "cause")) {
    it(`words the ${arm} refusal`, async () => {
      const { host } = await clickWith(() => refused({ case: arm, value: causes[arm] }));
      expect(host.querySelector(".refusal")?.textContent).toBe(sentences[arm]);
    });
  }

  it("gives the button back after a refusal", async () => {
    const { button } = await clickWith(() => refused({ case: "vendorRefused", value: { detail: "x" } }));
    expect(button.disabled).toBe(false);
  });

  it("states a transport failure at the chip when the toggle never reached the daemon", async () => {
    const { host } = await clickWith(() => {
      throw new Error("no route to the daemon");
    });
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("failed");
  });

  it("reports an error naming no cause as an unreadable frame", async () => {
    const { failures } = await clickWith(() =>
      create(UpdateRemoteControlModeResponseSchema, {
        result: { case: "error", value: create(UpdateRemoteControlModeErrorSchema, {}) },
      }),
    );
    expect(failures.reported).toHaveLength(1);
  });

  it("invents no refusal for an error naming no cause", async () => {
    const { host } = await clickWith(() =>
      create(UpdateRemoteControlModeResponseSchema, {
        result: { case: "error", value: create(UpdateRemoteControlModeErrorSchema, {}) },
      }),
    );
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("reports an answer with no result as an unreadable frame", async () => {
    const { failures } = await clickWith(() => create(UpdateRemoteControlModeResponseSchema, {}));
    expect(failures.reported).toHaveLength(1);
  });

  it("clears the previous refusal before the next click", async () => {
    // Arrange
    let answers = 0;
    const { host, button } = await clickWith(() => {
      answers += 1;
      return answers === 1 ? refused({ case: "vendorRefused", value: { detail: "x" } }) : success();
    });
    // Act
    button.click();
    await settle();
    // Assert
    expect(host.querySelector(".refusal")).toBeNull();
  });
});
