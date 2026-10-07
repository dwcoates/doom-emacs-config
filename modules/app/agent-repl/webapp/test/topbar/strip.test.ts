// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarAccountSchema,
  TopbarConnectivitySchema,
  TopbarSessionLineSchema,
  TopbarTitleSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  bindTitleSessionReveal,
  drawTopbarAccount,
  drawTopbarConnectivity,
  drawTopbarSessionLine,
  drawTopbarTitle,
} from "../../src/topbar/strip.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { openPanel, topbarContext } from "./fixtures.js";

const loggedIn = (email: string) =>
  create(TopbarAccountSchema, { state: { case: "loggedIn", value: { email } } });
const loggedOut = () =>
  create(TopbarAccountSchema, { state: { case: "loggedOut", value: {} } });

describe("drawTopbarAccount", () => {
  it("draws the logged-in email verbatim", () => {
    expect(drawTopbarAccount(loggedIn("a@b.test")).textContent).toBe("a@b.test");
  });

  it("carries the arm as a hook", () => {
    expect(drawTopbarAccount(loggedIn("a@b.test")).getAttribute("data-arm")).toBe("loggedIn");
  });

  it("says 'logged out' rather than nothing, since a blank reads as loading", () => {
    expect(drawTopbarAccount(loggedOut()).textContent).toBe("logged out");
  });

  it("wears the logged-out arm class the warning color hangs on", () => {
    expect(drawTopbarAccount(loggedOut()).classList.contains("arm-loggedOut")).toBe(true);
  });

  it("refuses an account naming no arm", () => {
    expect(() => drawTopbarAccount(create(TopbarAccountSchema, {}))).toThrow(MalformedView);
  });

  it("refuses an account arm this build cannot draw, rather than drawing a blank chip", () => {
    // ARRANGE: a newer daemon's arm, reaching a build that has no case for it.
    const future = loggedOut();
    (future.state as { case: string }).case = "loggedInAsRobot";
    // ACT / ASSERT
    expect(() => drawTopbarAccount(future)).toThrow(
      /TopbarAccount.state.*loggedInAsRobot/,
    );
  });
});

describe("bindTitleSessionReveal", () => {
  it("opens the same session line the chip does, the title being the session's own door", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }));
    host.append(title);
    bindTitleSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    // ACT
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  it("marks the title as the reveal's anchor, so an outside click spares it", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }));
    host.append(title);
    bindTitleSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    expect(title.getAttribute("data-reveal-anchor")).toBe("session");
  });

  it("closes the open reveal on a second click of the title", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }));
    host.append(title);
    bindTitleSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ACT
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });

  it("registers the line as drawn, so a refresh re-opens THIS push's line", () => {
    // ARRANGE: the reveal is opened off the first push, then a second push
    // registers a newer line and the layer is refreshed.
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }));
    host.append(title);
    bindTitleSessionReveal(title, create(TopbarSessionLineSchema, { text: "session one" }), tc);
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ACT
    bindTitleSessionReveal(title, create(TopbarSessionLineSchema, { text: "session two" }), tc);
    tc.reveals.refresh();
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session two");
  });

  it("binds nothing when the view carries no session line", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }));
    host.append(title);
    bindTitleSessionReveal(title, undefined, tc);
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)).toBeNull();
  });

  it("leaves an unbound title without an anchor mark", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }));
    host.append(title);
    bindTitleSessionReveal(title, undefined, tc);
    expect(title.getAttribute("data-reveal-anchor")).toBeNull();
  });
});

describe("drawTopbarSessionLine", () => {
  it("draws the daemon's sentence verbatim", () => {
    expect(drawTopbarSessionLine(create(TopbarSessionLineSchema, { text: "x" })).textContent).toBe(
      "x",
    );
  });

  // THE LINE IS LONGER THAN THE PANEL AND THE PANEL IS CAPPED. `.topbar-reveal`
  // is `max-width: min(90vw, 32rem)`, so a line drawn `nowrap` runs past the
  // cap and the last segment -- the MODEL -- is cut off the right edge. The
  // reveal has a second axis to spend, so the line must be allowed to take a
  // second row rather than a hidden one.
  it("lets the line take a second row rather than run past the panel's capped width", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const line = drawTopbarSessionLine(
        create(TopbarSessionLineSchema, {
          text: "vend-1 · ~/workspace/very/deep/checkout/root · fake-opus-4-8",
        }),
      );
      document.body.append(line);

      // Act / Assert
      expect(cascadedValue(line, "white-space")).not.toBe("nowrap");
    } finally {
      teardown();
    }
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
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" })).textContent).toBe(
      "ABC/fix",
    );
  });

  it("keeps the whole title in the tooltip, since the strip ellipsis-clips it", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" })).title).toBe("ABC/fix");
  });
});
