// @vitest-environment jsdom
//
// sendChipToggle, the one click path of a topbar chip that turns a mode over.
// Driven directly over UpdateRemoteControlMode (any toggle rpc serves), so
// each case pins what the helper guarantees every chip, and a source scan
// pins that both chips go through it.
import { readFileSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  UpdateRemoteControlModeErrorSchema,
  UpdateRemoteControlModeResponseSchema,
  type UpdateRemoteControlModeResponse,
  type UpdateRemoteControlModeSuccess,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_remote_control_mode_pb";
import { createControl, type Control } from "../../src/control.js";
import { sendChipToggle } from "../../src/topbar/chip-toggle.js";
import { RecordingSink, appContext, topbarContext } from "./fixtures.js";

/** A success answer. */
const success = (): UpdateRemoteControlModeResponse =>
  create(UpdateRemoteControlModeResponseSchema, { result: { case: "success", value: { on: true } } });

/** A chip wrap and its button, mounted in the context's host. */
interface Mounted {
  readonly chip: HTMLElement;
  readonly button: Control;
  readonly succeeded: UpdateRemoteControlModeSuccess[];
  readonly failures: RecordingSink;
  readonly sent: Promise<void>;
}

/** Send one toggle whose rpc answers ANSWER, over a fresh chip. */
function toggleWith(
  answer: () => UpdateRemoteControlModeResponse | Promise<UpdateRemoteControlModeResponse>,
  standing?: HTMLElement,
): Mounted {
  const failures = new RecordingSink();
  const { host, tc } = topbarContext(appContext({ updateRemoteControlMode: answer }, failures));
  const chip = document.createElement("span");
  const button = createControl();
  chip.append(button);
  if (standing !== undefined) chip.append(standing);
  host.append(chip);
  const succeeded: UpdateRemoteControlModeSuccess[] = [];
  const sent = sendChipToggle<UpdateRemoteControlModeResponse>(tc, chip, button, {
    rpc: "UpdateRemoteControlMode",
    operation: "test.chip",
    send: (client) => client.updateRemoteControlMode({ action: { case: "toggle", value: {} } }),
    schema: UpdateRemoteControlModeResponseSchema,
    causes: {},
    onSuccess: (s) => succeeded.push(s),
  });
  return { chip, button, succeeded, failures, sent };
}

describe("sendChipToggle", () => {
  it("hands a success to the chip's own success record", async () => {
    // Act
    const { succeeded, sent } = toggleWith(success);
    await sent;
    // Assert
    expect(succeeded.map((s) => s.on)).toEqual([true]);
  });

  it("marks the chip as settling while the toggle is in flight", () => {
    // Act
    const { chip } = toggleWith(() => new Promise(() => undefined));
    // Assert
    expect(chip.hasAttribute("data-settling")).toBe(true);
  });

  it("disables the button while the toggle is in flight", () => {
    // Act
    const { button } = toggleWith(() => new Promise(() => undefined));
    // Assert
    expect(button.disabled).toBe(true);
  });

  it("clears the settling mark once the toggle landed", async () => {
    // Act
    const { chip, sent } = toggleWith(success);
    await sent;
    // Assert
    expect(chip.hasAttribute("data-settling")).toBe(false);
  });

  it("gives the button back once the toggle landed", async () => {
    // Act
    const { button, sent } = toggleWith(success);
    await sent;
    // Assert
    expect(button.disabled).toBe(false);
  });

  it("draws a refusal by its cause's arm inside the chip", async () => {
    // Act
    const { chip, sent } = toggleWith(() =>
      create(UpdateRemoteControlModeResponseSchema, {
        result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
      }),
    );
    await sent;
    // Assert
    expect(chip.querySelector(":scope > .refusal")?.getAttribute("data-arm")).toBe("notYetAdopted");
  });

  it("states nothing as a success when refused", async () => {
    // Act
    const { succeeded, sent } = toggleWith(() =>
      create(UpdateRemoteControlModeResponseSchema, {
        result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
      }),
    );
    await sent;
    // Assert
    expect(succeeded).toEqual([]);
  });

  it("states a transport failure at the chip", async () => {
    // Act
    const { chip, sent } = toggleWith(() => {
      throw new Error("no route to the daemon");
    });
    await sent;
    // Assert
    expect(chip.querySelector(".refusal")?.getAttribute("data-arm")).toBe("failed");
  });

  it("clears the settling mark when the call failed at the transport", async () => {
    // Act
    const { chip, sent } = toggleWith(() => {
      throw new Error("link dropped");
    });
    await sent;
    // Assert
    expect(chip.hasAttribute("data-settling")).toBe(false);
  });

  it("reports an answer with no result as an unreadable frame", async () => {
    // Act
    const { failures, sent } = toggleWith(() => create(UpdateRemoteControlModeResponseSchema, {}));
    await sent;
    // Assert
    expect(failures.reported).toHaveLength(1);
  });

  it("invents no refusal for an error naming no cause", async () => {
    // Act
    const { chip, sent } = toggleWith(() =>
      create(UpdateRemoteControlModeResponseSchema, {
        result: { case: "error", value: create(UpdateRemoteControlModeErrorSchema, {}) },
      }),
    );
    await sent;
    // Assert
    expect(chip.querySelector(".refusal")).toBeNull();
  });

  it("clears a standing refusal before the call", () => {
    // Arrange
    const standing = document.createElement("span");
    standing.className = "refusal";
    // Act
    const { chip } = toggleWith(() => new Promise(() => undefined), standing);
    // Assert
    expect(chip.querySelector(".refusal")).toBeNull();
  });
});

describe("the toggle chips' one click path", () => {
  const topbarDir = join(dirname(fileURLToPath(import.meta.url)), "../../src/topbar");
  const sources = readdirSync(topbarDir)
    .filter((f) => f.endsWith(".ts") && f !== "chip-toggle.ts")
    .map((f) => ({ file: f, text: readFileSync(join(topbarDir, f), "utf8") }));

  it("is the only topbar code that marks a chip as settling", () => {
    expect(sources.filter((s) => s.text.includes('"data-settling"')).map((s) => s.file)).toEqual([]);
  });

  it("is what both toggle chips send through", () => {
    const callers = sources.filter((s) => s.text.includes("sendChipToggle(")).map((s) => s.file).sort();
    expect(callers).toEqual(["persistent-wifi.ts", "remote-control.ts"]);
  });
});
