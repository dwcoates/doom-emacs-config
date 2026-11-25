// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { recorded } from "../events.js";
import { create } from "@bufbuild/protobuf";
import { drawNamePrefix } from "../../src/name-prefix.js";
import {
  TopbarAccountSchema,
  TopbarConnectivitySchema,
  TopbarSessionLineSchema,
  TopbarTitleSchema,
  TopbarWorkspaceNameSchema,
  TopbarWorkspaceNameLinkIcon,
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
  BADGED_LINK_CLASS,
  LINK_HEAD_CLASS,
  OWNED_LINK_CLASS,
} from "../../src/topbar/strip.js";
import { drawLinkIcon } from "../../src/topbar/link-icon.js";
import { LINK_BADGE_CLASS, LINK_MARK_CLASS } from "../../src/topbar/link-badge.js";
import { cascadedValue, cascadedValueIn, installStylesheet } from "../stylesheet.js";
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
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    // ACT
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  it("marks the title as the reveal's anchor, so an outside click spares it", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    expect(title.getAttribute("data-reveal-anchor")).toBe("session");
  });

  it("closes the open reveal on a second click of the title", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc);
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
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc);
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
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc);
    host.append(title);
    bindHeadingSessionReveal(title, undefined, tc);
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)).toBeNull();
  });

  it("leaves an unbound title without an anchor mark", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), tc);
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
    ["title", () => drawTopbarTitle(create(TopbarTitleSchema, { text: "t" }), topbarContext().tc), "topbar-title"],
    ["workspace name", () => drawTopbarWorkspaceName(workspaceName("t"), topbarContext().tc), "topbar-workspace-name"],
  ])("draws the %s as a heading line", (_, draw, className) => {
    expect(draw().outerHTML).toBe(drawHeadingLine(className, "t").outerHTML);
  });
});

describe("drawTopbarWorkspaceName's emoji-ack glyph", () => {
  it("draws the glyph in the name's prefix, the tooltip the name alone", () => {
    // Act
    const u = create(TopbarWorkspaceNameSchema, { text: "acked", prefix: { emojiAck: { text: "👍" } } });
    const line = drawTopbarWorkspaceName(u, topbarContext().tc);

    // Assert
    expect([line.firstElementChild?.outerHTML, line.textContent, line.title]).toEqual([
      drawNamePrefix(u.prefix, "TopbarWorkspaceName.prefix")?.outerHTML,
      "👍acked",
      "acked",
    ]);
  });

  it("draws no glyph for a name that wears none", () => {
    expect(drawTopbarWorkspaceName(workspaceName("plain"), topbarContext().tc).querySelector(".name-emoji-ack")).toBeNull();
  });
});

describe("drawTopbarHeading", () => {
  it("draws the summary above the workspace's name", () => {
    // ACT
    const heading = drawTopbarHeading(
      create(TopbarTitleSchema, { text: "Fix the reconnect backoff" }),
      workspaceName("fix-reconnect"),
      topbarContext().tc,
    );
    // ASSERT
    expect([...heading.children].map((el) => [el.className, el.textContent])).toEqual([
      ["topbar-title", "Fix the reconnect backoff"],
      ["topbar-workspace-name", "fix-reconnect"],
    ]);
  });

  it("draws the name alone, and nothing in the summary's place, when there is no summary", () => {
    // ACT
    const heading = drawTopbarHeading(undefined, workspaceName("fix-reconnect"), topbarContext().tc);
    // ASSERT
    expect([...heading.children].map((el) => el.className)).toEqual(["topbar-workspace-name"]);
  });
});

describe("drawTopbarWorkspaceName", () => {
  it("draws the name verbatim", () => {
    expect(drawTopbarWorkspaceName(workspaceName("fix-reconnect"), topbarContext().tc).textContent).toBe(
      "fix-reconnect",
    );
  });

  it("keeps the whole name in the tooltip, since the strip ellipsis-clips it", () => {
    expect(drawTopbarWorkspaceName(workspaceName("fix-reconnect"), topbarContext().tc).title).toBe("fix-reconnect");
  });

  it("draws the name's prefix before the name, through the one shared drawer", () => {
    const u = create(TopbarWorkspaceNameSchema, {
      text: "fix-reconnect",
      prefix: { priority: { level: { case: "p2", value: {} } } },
    });
    const line = drawTopbarWorkspaceName(u, topbarContext().tc);
    expect(line.firstElementChild?.outerHTML).toBe(drawNamePrefix(u.prefix, "TopbarWorkspaceName.prefix")?.outerHTML);
    expect(line.querySelector("[data-priority='p2']")).not.toBeNull();
  });

  it("keeps the prefix out of the tooltip", () => {
    const u = create(TopbarWorkspaceNameSchema, {
      text: "fix-reconnect",
      prefix: { priority: { level: { case: "p1", value: {} } } },
    });
    expect(drawTopbarWorkspaceName(u, topbarContext().tc).title).toBe("fix-reconnect");
  });

  it("draws no prefix for a name that wears none", () => {
    expect(drawTopbarWorkspaceName(workspaceName("fix-reconnect"), topbarContext().tc).querySelector(".name-prefix")).toBeNull();
  });
});

describe("drawTopbarTitle", () => {
  it("draws the summary verbatim", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), topbarContext().tc).textContent).toBe(
      "ABC/fix",
    );
  });

  it("keeps the whole title in the tooltip, since the strip ellipsis-clips it", () => {
    expect(drawTopbarTitle(create(TopbarTitleSchema, { text: "ABC/fix" }), topbarContext().tc).title).toBe("ABC/fix");
  });
});

/** What the cascade paints on EL: the summary's look, property by property. */
function paint(el: Element): Record<string, string> {
  return {
    color: cascadedValue(el, "color"),
    fontStyle: cascadedValue(el, "font-style"),
    fontSize: cascadedValue(el, "font-size"),
    fontWeight: cascadedValue(el, "font-weight"),
    fontFamily: cascadedValue(el, "font-family"),
    textDecoration: cascadedValue(el, "text-decoration"),
  };
}

/** The paint of a summary with no links, drawn in a heading as the strip does. */
function plainPaint(): Record<string, string> {
  const heading = drawTopbarHeading(create(TopbarTitleSchema, { text: "t" }), workspaceName("w"), topbarContext().tc);
  document.body.replaceChildren(heading);
  return paint(heading.querySelector(".topbar-title")!);
}

/** A context whose OpenExternal records every request it is sent. */
/** A context recording every OpenExternal request (events.ts's recorded). */
function recordingContext(): { ctx: AppContext; opened: OpenExternalRequest[]; first: Promise<void> } {
  const opens = recorded<OpenExternalRequest>();
  const ctx = appContext({
    openExternal: (req) => {
      opens.record(req);
      return create(OpenExternalResponseSchema, { result: { case: "success", value: {} } });
    },
  });
  // The router transport answers on a timer, so a test awaits the request
  // as the event it is rather than polling the recording.
  return { ctx, opened: opens.calls, first: opens.reached(1) };
}

const click = (el: Element): void => {
  el.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }));
};

/** The paint of a workspace name with no links, drawn in a heading as the strip does. */
function plainNamePaint(): Record<string, string> {
  const heading = drawTopbarHeading(create(TopbarTitleSchema, { text: "t" }), workspaceName("w"), topbarContext().tc);
  document.body.replaceChildren(heading);
  return paint(heading.querySelector(".topbar-workspace-name")!);
}

describe("drawTopbarTitle's long summary", () => {
  const LONG = "The reconnect backoff retries too fast. Fix its schedule.";
  const described = () => create(TopbarTitleSchema, { text: "Fix the reconnect backoff", description: { text: LONG } });
  const hover = (el: Element, type: "pointerenter" | "pointerleave"): void => {
    el.dispatchEvent(new Event(type));
  };

  it("draws the long summary beneath the strip while the pointer rests on the summary", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(described(), tc);
    host.append(title);
    hover(title, "pointerenter");
    expect(openPanel(host)?.querySelector(".topbar-title-description")?.textContent).toBe(LONG);
  });

  it("closes the long summary when the pointer leaves the summary", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(described(), tc);
    host.append(title);
    hover(title, "pointerenter");
    hover(title, "pointerleave");
    expect(openPanel(host)).toBeNull();
  });

  it("leaves another open reveal standing when the pointer leaves the summary", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(described(), tc);
    host.append(title);
    bindHeadingSessionReveal(title, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    click(title);
    hover(title, "pointerleave");
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  it("marks the summary as the popup's anchor", () => {
    expect(drawTopbarTitle(described(), topbarContext().tc).getAttribute("data-reveal-anchor")).toBe("description");
  });

  it("drops the line's own tooltip, which the popup stands in for", () => {
    expect(drawTopbarTitle(described(), topbarContext().tc).hasAttribute("title")).toBe(false);
  });

  it("opens nothing on a hover of a summary with no long summary", () => {
    const { host, tc } = topbarContext();
    const title = drawTopbarTitle(create(TopbarTitleSchema, { text: "t" }), tc);
    host.append(title);
    hover(title, "pointerenter");
    expect(openPanel(host)).toBeNull();
  });

  it("re-opens the popup with a later push's long summary", () => {
    const { host, tc } = topbarContext();
    const first = drawTopbarTitle(described(), tc);
    host.append(first);
    hover(first, "pointerenter");
    const later = drawTopbarTitle(create(TopbarTitleSchema, { text: "t", description: { text: "Later. Text." } }), tc);
    first.replaceWith(later);
    tc.reveals.refresh();
    expect(openPanel(host)?.textContent).toBe("Later. Text.");
  });
});

describe("the summary's look", () => {
  it("is the summary's purple and never underlined, whatever the name carries", () => {
    const teardown = installStylesheet();
    try {
      const plain = plainPaint();
      const heading = drawTopbarHeading(
        create(TopbarTitleSchema, { text: "t", description: { text: "One. Two." } }),
        create(TopbarWorkspaceNameSchema, { text: "w", links: { opens: { case: "single", value: { label: "L", url: "https://a.example/1" } } } }),
        topbarContext().tc,
      );
      document.body.replaceChildren(heading);
      const drawn = paint(heading.querySelector(".topbar-title")!);
      expect([drawn, drawn.textDecoration.includes("underline")]).toEqual([plain, false]);
    } finally {
      teardown();
    }
  });
});

describe("drawTopbarWorkspaceName with one shown link", () => {
  const URL = "https://github.com/o/r/pull/7";
  const linked = () =>
    create(TopbarWorkspaceNameSchema, {
      text: "fix-reconnect",
      links: { opens: { case: "single", value: { label: "The pull request", url: URL } } },
    });

  it("draws the name as the link's text, pointing at the url", () => {
    const anchor = drawTopbarWorkspaceName(linked(), topbarContext().tc).querySelector("a[data-external-link]");
    expect([anchor?.textContent, anchor?.getAttribute("href")]).toEqual(["fix-reconnect", URL]);
  });

  it("names the label and the url in the tooltip, beneath the whole name", () => {
    expect(drawTopbarWorkspaceName(linked(), topbarContext().tc).title).toBe(`fix-reconnect\nThe pull request\n${URL}`);
  });

  it("draws a name with no links as plain text", () => {
    expect(drawTopbarWorkspaceName(workspaceName("t"), topbarContext().tc).querySelector("a")).toBeNull();
  });

  it("keeps the name's prefix before its link", () => {
    const u = linked();
    u.prefix = create(TopbarWorkspaceNameSchema, { prefix: { priority: { level: { case: "p2", value: {} } } } }).prefix;
    const line = drawTopbarWorkspaceName(u, topbarContext().tc);
    expect([line.firstElementChild?.querySelector("[data-priority='p2']") !== null, line.lastElementChild?.tagName]).toEqual([true, "A"]);
  });

  it("opens the url through OpenExternal when clicked", async () => {
    const { ctx, opened, first } = recordingContext();
    const name = drawTopbarWorkspaceName(linked(), topbarContext(ctx).tc);
    document.body.replaceChildren(name);
    click(name.querySelector("a")!);
    await first;
    expect(opened.map((req) => req.url)).toEqual([URL]);
  });

  it("does not also open the heading's session line", () => {
    const { host, tc } = topbarContext();
    const heading = drawTopbarHeading(create(TopbarTitleSchema, { text: "t" }), linked(), tc);
    host.append(heading);
    bindHeadingSessionReveal(heading, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    click(heading.querySelector("a")!);
    expect(openPanel(host)).toBeNull();
  });

  it("is underlined and keeps the name's own color and face", () => {
    const teardown = installStylesheet();
    try {
      const heading = drawTopbarHeading(create(TopbarTitleSchema, { text: "t" }), linked(), topbarContext().tc);
      document.body.replaceChildren(heading);
      expect(paint(heading.querySelector("a")!)).toEqual({ ...plainNamePaint(), textDecoration: "underline" });
    } finally {
      teardown();
    }
  });
});

describe("drawTopbarWorkspaceName with several shown links", () => {
  const PR = "https://github.com/o/r/pull/7";
  const DOC = "https://example.com/design";
  const menued = () =>
    create(TopbarWorkspaceNameSchema, {
      text: "fix-reconnect",
      links: {
        opens: {
          case: "menu",
          value: { items: [{ label: "The pull request", url: PR }, { label: "The design doc", url: DOC }] },
        },
      },
    });

  /** Draw the heading with its session-line reveal bound, as the strip does. */
  function drawn(ctx?: AppContext) {
    const { host, tc } = topbarContext(ctx);
    const heading = drawTopbarHeading(create(TopbarTitleSchema, { text: "Fix the reconnect backoff" }), menued(), tc);
    host.append(heading);
    bindHeadingSessionReveal(heading, create(TopbarSessionLineSchema, { text: "session abc" }), tc);
    const name = heading.querySelector<HTMLElement>(".topbar-workspace-name")!;
    return { host, tc, name };
  }

  it("draws the name as plain text, not as a link", () => {
    const { name } = drawn();
    expect([name.textContent, name.querySelector("a")]).toEqual(["fix-reconnect", null]);
  });

  it("drops the menu of the labels, in the served order, on a click", () => {
    const { host, name } = drawn();
    click(name);
    const anchors = [...openPanel(host)!.querySelectorAll("a[data-external-link]")];
    expect(anchors.map((a) => [a.textContent, a.getAttribute("href")])).toEqual([
      ["The pull request", PR],
      ["The design doc", DOC],
    ]);
  });

  it("draws each row's url beneath its label, as the row's second line", () => {
    const { host, name } = drawn();
    click(name);
    const rows = [...openPanel(host)!.querySelectorAll<HTMLElement>(".topbar-name-link-option")];
    expect(
      rows.map((row) => [...row.children].map((el) => [el.className, [...el.children].map((c) => c.className), el.textContent])),
    ).toEqual([
      [[LINK_HEAD_CLASS, ["external-link"], "The pull request"], ["topbar-name-link-url", [], PR]],
      [[LINK_HEAD_CLASS, ["external-link"], "The design doc"], ["topbar-name-link-url", [], DOC]],
    ]);
  });

  it("names each row's url in its tooltip", () => {
    const { host, name } = drawn();
    click(name);
    const rows = [...openPanel(host)!.querySelectorAll<HTMLElement>(".topbar-name-link-option")];
    expect(rows.map((row) => row.title)).toEqual([PR, DOC]);
  });

  it("does not also open the heading's session line", () => {
    const { host, name } = drawn();
    click(name);
    expect(openPanel(host)?.textContent).not.toContain("session abc");
  });

  it("closes the menu on a second click of the name", () => {
    const { host, name } = drawn();
    click(name);
    click(name);
    expect(openPanel(host)).toBeNull();
  });

  it("marks the name as the menu's anchor, so an outside click spares it", () => {
    const { name } = drawn();
    expect(name.getAttribute("data-reveal-anchor")).toBe("links");
  });

  it("leaves the summary a plain line with no menu", () => {
    const { host, name } = drawn();
    const title = name.parentElement!.querySelector<HTMLElement>(".topbar-title")!;
    expect([title.hasAttribute("data-reveal-anchor"), title.querySelector("a"), openPanel(host)]).toEqual([false, null, null]);
  });

  it("opens a picked link through OpenExternal and closes the menu", async () => {
    const { ctx, opened, first } = recordingContext();
    const { host, name } = drawn(ctx);
    click(name);
    click(openPanel(host)!.querySelectorAll("a")[1]);
    await first;
    expect(opened.map((req) => req.url)).toEqual([DOC]);
    expect(openPanel(host)).toBeNull();
  });

  it("is underlined and keeps the name's own color and face", () => {
    const teardown = installStylesheet();
    try {
      const { name } = drawn();
      expect(paint(name)).toEqual({ ...plainNamePaint(), textDecoration: "underline" });
    } finally {
      teardown();
    }
  });

  it("opens a row's link when its url line is clicked", async () => {
    const { ctx, opened, first } = recordingContext();
    const { host, name } = drawn(ctx);
    click(name);
    click(openPanel(host)!.querySelectorAll<HTMLElement>(".topbar-name-link-url")[1]);
    await first;
    expect(opened.map((req) => req.url)).toEqual([DOC]);
    expect(openPanel(host)).toBeNull();
  });

  it("opens a row's link when the row's own background is clicked", async () => {
    const { ctx, opened, first } = recordingContext();
    const { host, name } = drawn(ctx);
    click(name);
    click(openPanel(host)!.querySelectorAll<HTMLElement>(".topbar-name-link-option")[0]);
    await first;
    expect(opened.map((req) => req.url)).toEqual([PR]);
    expect(openPanel(host)).toBeNull();
  });

  it("opens a label click's link exactly once", async () => {
    const { ctx, opened, first } = recordingContext();
    const { host, name } = drawn(ctx);
    click(name);
    click(openPanel(host)!.querySelectorAll("a")[0]);
    await first;
    expect(opened.map((req) => req.url)).toEqual([PR]);
  });

  it("draws each row's label like a model selector's name: white, the same size, never underlined", () => {
    const teardown = installStylesheet();
    try {
      const { host, name } = drawn();
      click(name);
      const label = openPanel(host)!.querySelector("a")!;
      const labelPaint = paint(label);
      const panel = document.createElement("div");
      panel.className = "topbar-reveal";
      const row = document.createElement("button");
      row.className = "topbar-model-option";
      const modelName = document.createElement("span");
      modelName.className = "topbar-model-name";
      row.append(modelName);
      panel.append(row);
      document.body.append(panel);
      expect(labelPaint).toEqual({ ...paint(modelName), textDecoration: "none" });
    } finally {
      teardown();
    }
  });

  it("refuses a links arm this build cannot draw", () => {
    const future = create(TopbarWorkspaceNameSchema, { text: "t", links: {} });
    expect(() => drawTopbarWorkspaceName(future, topbarContext().tc)).toThrow(/TopbarWorkspaceNameLinks.opens/);
  });
});

/** One name link-menu row to draw, as the daemon serves it. */
type MenuItem = {
  label: string;
  url: string;
  owned?: boolean;
  subscribed?: boolean;
  icon?: TopbarWorkspaceNameLinkIcon;
  badge?: { state: { case: "open" | "draft" | "merged" | "closed"; value: object } };
};

/**
 * Draws a heading whose name drops a menu of ITEMS, opens the menu, and
 * answers its rows in the order drawn. CTX is the app context the strip opens
 * links through.
 */
function rowsOf(items: MenuItem[], ctx?: AppContext): HTMLElement[] {
  const { host, tc } = topbarContext(ctx);
  const heading = drawTopbarHeading(
    create(TopbarTitleSchema, { text: "t" }),
    create(TopbarWorkspaceNameSchema, { text: "w", links: { opens: { case: "menu", value: { items } } } }),
    tc,
  );
  host.append(heading);
  bindHeadingSessionReveal(heading, create(TopbarSessionLineSchema, { text: "s" }), tc);
  click(heading.querySelector<HTMLElement>(".topbar-workspace-name")!);
  return [...openPanel(host)!.querySelectorAll<HTMLElement>(".topbar-name-link-option")];
}

describe("a name link-menu row's owned tint and service icon", () => {

  it("marks owned rows and leaves reference rows unmarked", () => {
    const rows = rowsOf([
      { label: "Mine", url: "https://a.example/1", owned: true },
      { label: "Theirs", url: "https://a.example/2" },
    ]);
    expect(rows.map((row) => row.classList.contains(OWNED_LINK_CLASS))).toEqual([true, false]);
  });

  it.each([
    [TopbarWorkspaceNameLinkIcon.GITHUB, "GITHUB"],
    [TopbarWorkspaceNameLinkIcon.SLACK, "SLACK"],
    [TopbarWorkspaceNameLinkIcon.GNS, "GNS"],
    [TopbarWorkspaceNameLinkIcon.NOTION, "NOTION"],
  ])("draws the icon of service %s left of the label", (icon, name) => {
    const [row] = rowsOf([{ label: "L", url: "https://a.example/1", icon }]);
    const anchor = row.querySelector("a")!;
    expect([anchor.firstElementChild!.getAttribute("data-link-icon"), anchor.textContent]).toEqual([name, "L"]);
  });

  it("draws no icon for a host with no service", () => {
    const [row] = rowsOf([{ label: "L", url: "https://a.example/1" }]);
    expect(row.querySelector("svg")).toBeNull();
  });

  it("refuses an icon arm this build cannot draw", () => {
    expect(() => drawLinkIcon(99 as TopbarWorkspaceNameLinkIcon)).toThrow(/TopbarWorkspaceNameLinkIcon/);
  });
});

describe("a summary menu row's pull request badge and owned label", () => {
  const PR = "https://github.com/o/r/pull/7";

  it("draws a pull request's badge after its label and url, and reserves the row's margin", () => {
    // Act
    const [row] = rowsOf([{ label: "The fix", url: PR, badge: { state: { case: "merged", value: {} } } }]);
    // Assert
    const last = row.lastElementChild as HTMLElement;
    expect([row.classList.contains(BADGED_LINK_CLASS), last.className, last.dataset.badge, last.textContent]).toEqual([
      true,
      LINK_BADGE_CLASS,
      "merged",
      "merged",
    ]);
  });

  it("draws no badge, and reserves no margin, for a row served none", () => {
    const [row] = rowsOf([{ label: "The doc", url: "https://example.com/doc" }]);
    expect([row.querySelector(`.${LINK_BADGE_CLASS}`), row.classList.contains(BADGED_LINK_CLASS)]).toEqual([null, false]);
  });

  it("draws an owned row's label, and only its label, in the summary's purple", () => {
    const teardown = installStylesheet();
    try {
      // Act
      const [owned, reference] = rowsOf([
        { label: "Mine", url: "https://a.example/1", owned: true },
        { label: "Theirs", url: "https://a.example/2" },
      ]);
      // Assert
      const summary = cascadedValueIn("topbar-strip", "topbar-title", "color");
      expect([
        cascadedValue(owned.querySelector("a")!, "color"),
        cascadedValue(reference.querySelector("a")!, "color") === summary,
        cascadedValue(owned.querySelector(".topbar-name-link-url")!, "color") === summary,
      ]).toEqual([summary, false, false]);
    } finally {
      teardown();
    }
  });
});

describe("a menu row's second line", () => {
  const SECOND_LINES = ["topbar-account-option-dir", "topbar-model-description", "topbar-name-link-url"];

  it.each(["color", "font-size"])("draws %s the same in every menu", (property) => {
    const teardown = installStylesheet();
    try {
      const values = SECOND_LINES.map((cls) => cascadedValueIn("topbar-reveal", cls, property));
      expect(new Set(values).size).toBe(1);
    } finally {
      teardown();
    }
  });
});

describe("a summary menu row's owned and subscribed marks and its untinted icon", () => {
  const marksOf = (row: HTMLElement) =>
    [...row.querySelectorAll<HTMLElement>(`.${LINK_HEAD_CLASS} > .${LINK_MARK_CLASS}`)].map((el) => el.dataset.mark);

  it.each([
    [{ owned: true, subscribed: true }, ["owned", "subscribed"]],
    [{ owned: true, subscribed: false }, ["owned"]],
    [{ owned: false, subscribed: true }, ["subscribed"]],
    [{ owned: false, subscribed: false }, []],
  ])("draws the marks of %o beside the label", (flags, want) => {
    // Act
    const [row] = rowsOf([{ label: "L", url: "https://a.example/1", ...flags }]);
    // Assert
    expect(marksOf(row)).toEqual(want);
  });

  it("opens the row's link when a mark is clicked", async () => {
    // Arrange
    const { ctx, first } = recordingContext();
    const [row] = rowsOf([{ label: "A", url: "https://a.example/1", subscribed: true }], ctx);
    // Act
    click(row.querySelector<HTMLElement>(`.${LINK_MARK_CLASS}`)!);
    // Assert
    await first;
  });

  it("draws an owned row's icon in the foreground, not the label's purple", () => {
    const teardown = installStylesheet();
    try {
      // Act
      const [owned, reference] = rowsOf([
        { label: "Mine", url: "https://github.com/o/r/pull/1", owned: true, icon: TopbarWorkspaceNameLinkIcon.GITHUB },
        { label: "Theirs", url: "https://github.com/o/r/pull/2", icon: TopbarWorkspaceNameLinkIcon.GITHUB },
      ]);
      // Assert
      const iconColor = (row: HTMLElement) => cascadedValue(row.querySelector("svg")!, "color");
      expect([iconColor(owned) === iconColor(reference), iconColor(owned) === cascadedValue(owned.querySelector("a")!, "color")]).toEqual([
        true,
        false,
      ]);
    } finally {
      teardown();
    }
  });
});
