// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarAccountSchema,
  TopbarConnectivitySchema,
  TopbarSessionLineSchema,
  TopbarTitleSchema,
  TopbarWorkspaceNameSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  OpenExternalResponseSchema,
  type OpenExternalRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import type { AppContext } from "../../src/rpc/context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  bindHeadingSessionReveal,
  drawHeadingLine,
  drawTopbarAccount,
  drawTopbarConnectivity,
  drawTopbarHeading,
  drawTopbarSessionLine,
  drawTopbarTitle,
  drawTopbarWorkspaceName,
} from "../../src/topbar/strip.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { appContext, openPanel, topbarContext } from "./fixtures.js";

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

describe("bindHeadingSessionReveal", () => {
  it("opens the same session line the chip does, the heading being the session's own door", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc.ctx);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    // ACT
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  it("marks the title as the reveal's anchor, so an outside click spares it", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc.ctx);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    expect(title.getAttribute("data-reveal-anchor")).toBe("session");
  });

  it("closes the open reveal on a second click of the title", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc.ctx);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
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
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc.ctx);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session one" }), tc);
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ACT
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session two" }), tc);
    tc.reveals.refresh();
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session two");
  });

  it("binds nothing when the view carries no session line", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc.ctx);
    host.append(title);
    bindHeadingSessionReveal(title, undefined, tc);
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)).toBeNull();
  });

  it("leaves an unbound title without an anchor mark", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc.ctx);
    host.append(title);
    bindHeadingSessionReveal(title, undefined, tc);
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

const workspaceName = (text: string) => create(TopbarWorkspaceNameSchema, { text });

describe("drawHeadingLine", () => {
  it("draws the text verbatim under the given class", () => {
    const line = drawHeadingLine("topbar-x", "some text");
    expect([line.className, line.textContent]).toEqual(["topbar-x", "some text"]);
  });

  it("keeps the whole text in the tooltip", () => {
    expect(drawHeadingLine("topbar-x", "some text").title).toBe("some text");
  });

  // Both heading lines are the one shape: a hand-rolled line that dropped the
  // tooltip or the class would fail here.
  it.each([
    ["title", () => drawTopbarTitle(create(TopbarTitleSchema, { text: "t" }), appContext()), "topbar-title"],
    ["workspace name", () => drawTopbarWorkspaceName(workspaceName("t")), "topbar-workspace-name"],
  ])("draws the %s as a heading line", (_, draw, className) => {
    expect(draw().outerHTML).toBe(drawHeadingLine(className, "t").outerHTML);
  });
});

describe("drawTopbarHeading", () => {
  it("draws the summary above the workspace's name", () => {
    // ACT
    const heading = drawTopbarHeading(
      create(TopbarTitleSchema, { text: "Fix the reconnect backoff" }),
      workspaceName("fix-reconnect"),
      appContext(),
    );
    // ASSERT
    expect([...heading.children].map((el) => [el.className, el.textContent])).toEqual([
      ["topbar-title", "Fix the reconnect backoff"],
      ["topbar-workspace-name", "fix-reconnect"],
    ]);
  });

  it("draws the name alone, and nothing in the summary's place, when there is no summary", () => {
    // ACT
    const heading = drawTopbarHeading(undefined, workspaceName("fix-reconnect"), appContext());
    // ASSERT
    expect([...heading.children].map((el) => el.className)).toEqual(["topbar-workspace-name"]);
  });
});

describe("drawTopbarWorkspaceName", () => {
  it("draws the name verbatim", () => {
    expect(drawTopbarWorkspaceName(workspaceName("fix-reconnect")).textContent).toBe(
      "fix-reconnect",
    );
  });

  it("keeps the whole name in the tooltip, since the strip ellipsis-clips it", () => {
    expect(drawTopbarWorkspaceName(workspaceName("fix-reconnect")).title).toBe("fix-reconnect");
  });
});

describe("drawTopbarTitle", () => {
  it("draws the summary verbatim", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), appContext()).textContent).toBe(
      "ABC/fix",
    );
  });

  it("keeps the whole title in the tooltip, since the strip ellipsis-clips it", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), appContext()).title).toBe("ABC/fix");
  });
});

describe("drawTopbarTitle with a principal link", () => {
  const URL = "https://github.com/o/r/pull/7";
  const linked = () =>
    create(TopbarTitleSchema, { text: "Fix the reconnect backoff", link: { url: URL } });

  /** A context whose OpenExternal records every request it is sent. */
  function recordingContext(): { ctx: AppContext; opened: OpenExternalRequest[] } {
    const opened: OpenExternalRequest[] = [];
    const ctx = appContext({
      openExternal: (req) => {
        opened.push(req);
        return create(OpenExternalResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    return { ctx, opened };
  }

  const click = (el: Element): void => {
    el.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }));
  };

  it("draws the summary as the link's text, pointing at the url", () => {
    const anchor = drawTopbarTitle(linked(), appContext()).querySelector("a[data-external-link]");
    expect([anchor?.textContent, anchor?.getAttribute("href")]).toEqual([
      "Fix the reconnect backoff",
      URL,
    ]);
  });

  it("names the url in the tooltip, beneath the whole summary", () => {
    expect(drawTopbarTitle(linked(), appContext()).title).toBe(`Fix the reconnect backoff\n${URL}`);
  });

  it("draws a title with no link as plain text", () => {
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "t" }), appContext());
    expect(title.querySelector("a")).toBeNull();
  });

  it("opens the url through OpenExternal when clicked", async () => {
    // Arrange
    const { ctx, opened } = recordingContext();
    const title = drawTopbarTitle(linked(), ctx);
    document.body.replaceChildren(title);
    // Act
    click(title.querySelector("a")!);
    // Assert: the router transport answers on a timer, so the request is
    // awaited rather than read synchronously.
    await vi.waitFor(() => expect(opened.map((req) => req.url)).toEqual([URL]));
  });

  it("does not also open the heading's session line", () => {
    // Arrange
    const { host, tc } = topbarContext();
    const heading = drawTopbarHeading(linked(), workspaceName("fix-reconnect"), tc.ctx);
    host.append(heading);
    bindHeadingSessionReveal(heading, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    // Act
    click(heading.querySelector("a")!);
    // Assert
    expect(openPanel(host)).toBeNull();
  });

  it("is underlined in the summary's own color", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const heading = drawTopbarHeading(linked(), workspaceName("fix-reconnect"), appContext());
      document.body.replaceChildren(heading);
      const anchor = heading.querySelector("a")!;
      // Act / Assert
      expect([cascadedValue(anchor, "text-decoration"), cascadedValue(anchor, "color")]).toEqual([
        "underline",
        "var(--tool-title)",
      ]);
    } finally {
      teardown();
    }
  });
});
