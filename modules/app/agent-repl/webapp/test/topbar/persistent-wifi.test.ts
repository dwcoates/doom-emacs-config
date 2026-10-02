// @vitest-environment jsdom
//
// The persistent-wifi chip. One test per arm of each oneof, the unassigned
// (unread) case of each, the tooltip, the arm this bundle cannot name, and the
// click that turns the mode over through UpdatePersistentWifiMode{toggle}.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarPersistentWifiSchema,
  type TopbarPersistentWifi,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  UpdatePersistentWifiModeErrorSchema,
  UpdatePersistentWifiModeResponseSchema,
  type UpdatePersistentWifiModeRequest,
  type UpdatePersistentWifiModeResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_persistent_wifi_mode_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { DISC_RADIUS, drawTopbarPersistentWifi } from "../../src/topbar/persistent-wifi.js";
import { oneofArms } from "../arms.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { RecordingSink, appContext, topbarContext } from "./fixtures.js";

/** Draw the chip over a context whose rpcs nothing scripts. */
const draw = (u: TopbarPersistentWifi): HTMLElement => drawTopbarPersistentWifi(u, topbarContext().tc);

/** A chip with the given arms and a tooltip. */
function chip(
  wifi: TopbarPersistentWifi["wifi"],
  mode: TopbarPersistentWifi["mode"],
): TopbarPersistentWifi {
  return { ...create(TopbarPersistentWifiSchema, { tooltip: { text: "the daemon's words" } }), wifi, mode };
}

const joined = { case: "joined", value: {} } as TopbarPersistentWifi["wifi"];
const notJoined = { case: "notJoined", value: {} } as TopbarPersistentWifi["wifi"];
const on = { case: "on", value: {} } as TopbarPersistentWifi["mode"];
const off = { case: "off", value: {} } as TopbarPersistentWifi["mode"];
const unset = { case: undefined };

describe("drawTopbarPersistentWifi", () => {
  it("marks a joined network for the green glyph", () => {
    expect(draw(chip(joined, off)).getAttribute("data-wifi")).toBe("joined");
  });

  it("marks no network for the red glyph", () => {
    expect(draw(chip(notJoined, off)).getAttribute("data-wifi")).toBe("not-joined");
  });

  it("marks an unread network as unknown", () => {
    expect(draw(chip(unset as never, off)).getAttribute("data-wifi")).toBe("unknown");
  });

  it("marks the mode on for the black disc", () => {
    expect(draw(chip(notJoined, on)).getAttribute("data-mode")).toBe("on");
  });

  it("marks the mode off for no disc", () => {
    expect(draw(chip(joined, off)).getAttribute("data-mode")).toBe("off");
  });

  it("marks an unread mode as unknown", () => {
    expect(draw(chip(joined, unset as never)).getAttribute("data-mode")).toBe("unknown");
  });

  it("carries the daemon's tooltip verbatim", () => {
    expect(draw(chip(joined, on)).title).toBe("the daemon's words");
  });

  it("draws the wifi glyph", () => {
    expect(draw(chip(joined, on)).querySelector("svg.topbar-wifi-glyph")).not.toBeNull();
  });

  it("draws the disc inside the glyph's own svg, centered on the glyph's center", () => {
    const disc = draw(chip(joined, on)).querySelector("svg.topbar-wifi-glyph > circle.topbar-wifi-disc");
    expect([disc?.getAttribute("cx"), disc?.getAttribute("cy")]).toEqual(["12", "12"]);
  });

  it("sizes the svg's viewBox to the disc's square", () => {
    const svg = draw(chip(joined, on)).querySelector("svg.topbar-wifi-glyph");
    expect(svg?.getAttribute("viewBox")).toBe(`${12 - DISC_RADIUS} ${12 - DISC_RADIUS} ${2 * DISC_RADIUS} ${2 * DISC_RADIUS}`);
  });

  it("refuses a wifi arm this bundle cannot name", () => {
    expect(() => draw(chip({ case: "radioOff", value: {} } as never, on))).toThrow(MalformedView);
  });

  it("refuses a mode arm this bundle cannot name", () => {
    expect(() => draw(chip(joined, { case: "half", value: {} } as never))).toThrow(MalformedView);
  });

  it("refuses a chip with no tooltip", () => {
    const bare = { ...create(TopbarPersistentWifiSchema, {}), wifi: joined, mode: on };
    expect(() => draw(bare)).toThrow(MalformedView);
  });
});

/** A success answer carrying every field the daemon sets. */
const success = (): UpdatePersistentWifiModeResponse =>
  create(UpdatePersistentWifiModeResponseSchema, {
    result: {
      case: "success",
      value: {
        state: { wifi: { case: "joined", value: { networkName: "phone" } }, mode: { case: "on", value: {} } },
        hotspot: { outcome: { case: "joined", value: { networkName: "phone" } } },
        display: { outcome: { case: "dimmed", value: {} } },
      },
    },
  });

/** Settle every microtask and the router transport's own hop. */
const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

/**
 * Mount the chip over a context whose UpdatePersistentWifiMode is ANSWER,
 * click it, and let the answer land.
 */
async function clickWith(
  answer: (req: UpdatePersistentWifiModeRequest) => UpdatePersistentWifiModeResponse,
  failures = new RecordingSink(),
): Promise<{ host: HTMLElement; button: HTMLButtonElement; failures: RecordingSink }> {
  const { host, tc } = topbarContext(appContext({ updatePersistentWifiMode: answer }, failures));
  host.append(drawTopbarPersistentWifi(chip(joined, off), tc));
  const button = host.querySelector<HTMLButtonElement>(".topbar-wifi-button")!;
  button.click();
  await settle();
  return { host, button, failures };
}

describe("clicking the persistent-wifi chip", () => {
  it("draws the glyph inside a button", () => {
    expect(draw(chip(joined, on)).querySelector("button.topbar-wifi-button > svg.topbar-wifi-glyph")).not.toBeNull();
  });

  it("sends UpdatePersistentWifiMode with the toggle arm", async () => {
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

  it("logs the click at info with the standing the chip showed", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    await clickWith(() => success());
    // Assert
    const record = await forwardedRecord(capture, "topbar.persistent-wifi-toggle");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ wifi: "joined", mode: "off" })]);
  });

  it("logs the landed toggle's outcome at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    await clickWith(() => success());
    // Assert
    const record = await forwardedRecord(capture, "topbar.persistent-wifi-toggled");
    expect([record.level.case, record.context]).toEqual([
      "info",
      expect.objectContaining({ mode: "on", wifi: "joined", hotspot: "joined", display: "dimmed" }),
    ]);
  });

  it("draws nothing on success, because the new standing arrives on the push", async () => {
    const { host } = await clickWith(() => success());
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("gives the button back once the toggle landed", async () => {
    const { button } = await clickWith(() => success());
    expect(button.disabled).toBe(false);
  });

  it("disables the button while the toggle is in flight", () => {
    // Arrange
    const { host, tc } = topbarContext(appContext({ updatePersistentWifiMode: () => new Promise(() => undefined) }));
    host.append(drawTopbarPersistentWifi(chip(joined, off), tc));
    const button = host.querySelector<HTMLButtonElement>(".topbar-wifi-button")!;
    // Act
    button.click();
    // Assert
    expect(button.disabled).toBe(true);
  });

  const causes: Readonly<Record<string, unknown>> = {
    powerSettingsRefused: { detail: "sudo: a password is required" },
    modeUnreadable: { detail: "pmset -g printed nothing" },
  };

  for (const arm of oneofArms(UpdatePersistentWifiModeErrorSchema, "cause")) {
    it(`states the ${arm} refusal at the chip`, async () => {
      const { host } = await clickWith(() =>
        create(UpdatePersistentWifiModeResponseSchema, {
          result: { case: "error", value: { cause: { case: arm, value: causes[arm] } as never } },
        }),
      );
      expect(host.querySelector(".topbar-wifi > .refusal")?.getAttribute("data-arm")).toBe(arm);
    });
  }

  it("words a refusal with the daemon's own detail", async () => {
    const { host } = await clickWith(() =>
      create(UpdatePersistentWifiModeResponseSchema, {
        result: { case: "error", value: { cause: { case: "powerSettingsRefused", value: { detail: "no grant" } } } },
      }),
    );
    expect(host.querySelector(".refusal")?.textContent).toBe("the power settings change was refused: no grant");
  });

  it("gives the button back after a refusal", async () => {
    const { button } = await clickWith(() =>
      create(UpdatePersistentWifiModeResponseSchema, {
        result: { case: "error", value: { cause: { case: "modeUnreadable", value: { detail: "x" } } } },
      }),
    );
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
      create(UpdatePersistentWifiModeResponseSchema, {
        result: { case: "error", value: create(UpdatePersistentWifiModeErrorSchema, {}) },
      }),
    );
    expect(failures.reported).toHaveLength(1);
  });

  it("invents no refusal for an error naming no cause", async () => {
    const { host } = await clickWith(() =>
      create(UpdatePersistentWifiModeResponseSchema, {
        result: { case: "error", value: create(UpdatePersistentWifiModeErrorSchema, {}) },
      }),
    );
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("reports a success with no standing as an unreadable frame", async () => {
    const { failures } = await clickWith(() =>
      create(UpdatePersistentWifiModeResponseSchema, { result: { case: "success", value: {} } }),
    );
    expect(failures.reported).toHaveLength(1);
  });

  it("reports an answer with no result as an unreadable frame", async () => {
    const { failures } = await clickWith(() => create(UpdatePersistentWifiModeResponseSchema, {}));
    expect(failures.reported).toHaveLength(1);
  });

  it("clears the previous refusal before the next click", async () => {
    // Arrange
    let answers = 0;
    const { host, button } = await clickWith(() => {
      answers += 1;
      return answers === 1
        ? create(UpdatePersistentWifiModeResponseSchema, {
            result: { case: "error", value: { cause: { case: "modeUnreadable", value: { detail: "x" } } } },
          })
        : success();
    });
    // Act
    button.click();
    await settle();
    // Assert
    expect(host.querySelector(".refusal")).toBeNull();
  });
});
