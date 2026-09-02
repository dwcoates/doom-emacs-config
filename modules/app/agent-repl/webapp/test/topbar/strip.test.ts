// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarAccountSchema,
  TopbarConnectivitySchema,
  TopbarSessionLineSchema,
  TopbarTitleSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  bindSessionReveal,
  drawTopbarAccount,
  drawTopbarConnectivity,
  drawTopbarSessionLine,
  drawTopbarTitle,
} from "../../src/topbar/strip.js";
import { openPanel, topbarContext } from "./fixtures.js";

const loggedIn = (email: string) =>
  create(TopbarAccountSchema, { state: { case: "loggedIn", value: { email } } });
const loggedOut = () =>
  create(TopbarAccountSchema, { state: { case: "loggedOut", value: {} } });

describe("drawTopbarAccount", () => {
  it("draws the logged-in email verbatim", () => {
    const { tc } = topbarContext();
    expect(drawTopbarAccount(loggedIn("a@b.test"), tc).textContent).toBe("a@b.test");
  });

  it("carries the arm as a hook", () => {
    const { tc } = topbarContext();
    expect(drawTopbarAccount(loggedIn("a@b.test"), tc).getAttribute("data-arm")).toBe("loggedIn");
  });

  it("says 'logged out' rather than nothing, since a blank reads as loading", () => {
    const { tc } = topbarContext();
    expect(drawTopbarAccount(loggedOut(), tc).textContent).toBe("logged out");
  });

  it("wears the logged-out arm class the warning color hangs on", () => {
    const { tc } = topbarContext();
    expect(drawTopbarAccount(loggedOut(), tc).classList.contains("arm-loggedOut")).toBe(true);
  });

  it("opens the login when the logged-out chip is clicked", () => {
    // ARRANGE
    const openLogin = vi.fn();
    const { tc } = topbarContext(undefined, openLogin);
    const chip = drawTopbarAccount(loggedOut(), tc);
    // ACT
    chip.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openLogin).toHaveBeenCalledTimes(1);
  });

  it("refuses an account naming no arm", () => {
    const { tc } = topbarContext();
    expect(() => drawTopbarAccount(create(TopbarAccountSchema, {}), tc)).toThrow(MalformedView);
  });
});

describe("bindSessionReveal", () => {
  it("opens the session line on a click", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const chip = drawTopbarAccount(loggedIn("a@b.test"), tc);
    host.append(chip);
    bindSessionReveal(chip, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    // ACT
    chip.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  it("binds nothing when the view carries no session line", () => {
    const { host, tc } = topbarContext();
    const chip = drawTopbarAccount(loggedIn("a@b.test"), tc);
    host.append(chip);
    bindSessionReveal(chip, undefined, tc);
    chip.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)).toBeNull();
  });
});

describe("drawTopbarSessionLine", () => {
  it("draws the daemon's sentence verbatim", () => {
    expect(drawTopbarSessionLine(create(TopbarSessionLineSchema, { text: "x" })).textContent).toBe(
      "x",
    );
  });
});

describe("drawTopbarConnectivity", () => {
  const view = (tone: string) =>
    create(TopbarConnectivitySchema, { tone, glyph: "●", title: "connected" });

  it("draws the producer's glyph, never one of its own", () => {
    expect(drawTopbarConnectivity(view("green")).textContent).toBe("●");
  });

  it("carries the tooltip verbatim", () => {
    expect(drawTopbarConnectivity(view("green")).title).toBe("connected");
  });

  it("paints the tone class the shared vocabulary gives that name", () => {
    expect(drawTopbarConnectivity(view("green")).classList.contains("tone-green")).toBe(true);
  });

  it("carries the tone as a hook", () => {
    expect(drawTopbarConnectivity(view("none")).getAttribute("data-tone")).toBe("none");
  });

  it("refuses a tone the vocabulary file does not carry", () => {
    // The proto comment still lists a teal no producer may emit; the file wins.
    expect(() => drawTopbarConnectivity(view("teal"))).toThrow(MalformedView);
  });
});

describe("drawTopbarTitle", () => {
  it("draws the pre-composed title verbatim", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "DWC/fix" })).textContent).toBe(
      "DWC/fix",
    );
  });

  it("keeps the whole title in the tooltip, since the strip ellipsis-clips it", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "DWC/fix" })).title).toBe("DWC/fix");
  });
});
