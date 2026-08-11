/**
 * topbar-view — the header strip drawn from the RESOLVED `TopbarView`.
 *
 * The whole subject is VERBATIM rendering and honest absence: nothing here may
 * compose an identity, map a connectivity enum, or fill an empty slot from
 * somewhere else. One edge per test.
 */
import { describe, expect, it } from "vitest";
import {
  tokensDisclosureHtml,
  topbarWarningsHtml,
  WARNING_GLYPH,
  topbarConnectivityHtml,
  topbarModelOptionsHtml,
  topbarViewHtml,
} from "../src/topbar-view.js";
import type { TokenBreakdownView, TopbarView } from "../src/frontend-proto.js";

function view(over: Partial<TopbarView> = {}): TopbarView {
  return {
    workspace: "/ws",
    title: "agent-repl · feature-webapp-views",
    sessionLine: "session since 09:00",
    modelDisplay: "opus-5",
    modelOptions: [
      { value: "opus-5", displayName: "Opus 5", description: "highest capability" },
      { value: "sonnet-5", displayName: "Sonnet 5", description: "fast" },
    ],
    warnings: [],
    fence: "f1",
    ...over,
  };
}

describe("the title", () => {
  it("renders the daemon's composed title verbatim", () => {
    // Arrange / Act — the client never concatenates identity fragments: the
    // composition rule is the daemon's, and a second copy of it drifts.
    const html = topbarViewHtml(view());
    // Assert
    expect(html).toContain("agent-repl · feature-webapp-views");
  });

  it("escapes it", () => {
    // Arrange / Act
    const html = topbarViewHtml(view({ title: "<img src=x>" }));
    // Assert
    expect(html).not.toContain("<img");
  });
});

describe("absence renders absence", () => {
  it("renders NOTHING for a workspace with no published topbar", () => {
    // Arrange / Act — never a placeholder, and never a name pieced together
    // from whatever else the store happens to hold.
    // Assert
    expect(topbarViewHtml(null)).toBe("");
  });

  it("omits the session line when the daemon published none", () => {
    // Arrange / Act
    const html = topbarViewHtml(view({ sessionLine: "" }));
    // Assert
    expect(html).not.toContain("topbar-session-line");
  });

  it("never renders a warning's prose inline in the strip", () => {
    // Arrange / Act — the retired `accountingLine` put the whole sentence
    // across the header; the strip now carries no warning text at all.
    const html = topbarViewHtml(
      view({ warnings: [{ text: "INVALID ACCOUNTING: totals disagree", warning: { kind: "accounting" } }] }),
    );
    // Assert
    expect(html).not.toContain("INVALID ACCOUNTING");
  });

  it("renders the selector's placeholder when no model is selected", () => {
    // Arrange / Act — never a guessed model.
    const html = topbarViewHtml(view({ modelDisplay: "" }));
    // Assert
    expect(html).toContain("select a model");
  });
});

describe("the connectivity glyph", () => {
  it("renders nothing when the daemon published no connectivity", () => {
    // Arrange / Act — absence is the daemon not having spoken, not "unknown".
    // Assert
    expect(topbarConnectivityHtml(undefined)).toBe("");
  });

  it("draws the glyph the daemon resolved", () => {
    // Arrange / Act
    const html = topbarConnectivityHtml({ tone: "ok", glyph: "●", title: "connected" });
    // Assert
    expect(html).toContain("●");
  });

  it("takes the resolved TONE as its class rather than mapping an enum", () => {
    // Arrange / Act — the connectivity-to-color table a frontend used to carry
    // is gone, and a new one here would be a second authority on one fact.
    const html = topbarConnectivityHtml({ tone: "degraded", glyph: "●", title: "quiet" });
    // Assert
    expect(html).toContain("tone-degraded");
  });

  it("renders the tooltip verbatim", () => {
    // Arrange / Act
    const html = topbarConnectivityHtml({ tone: "ok", glyph: "●", title: "connected" });
    // Assert
    expect(html).toContain('title="connected"');
  });

  it("escapes the tooltip", () => {
    // Arrange / Act
    const html = topbarConnectivityHtml({ tone: "ok", glyph: "●", title: '"><img src=x>' });
    // Assert
    expect(html).not.toContain("<img");
  });
});

describe("the model selector", () => {
  it("renders exactly the options the daemon listed", () => {
    // Arrange / Act
    const html = topbarModelOptionsHtml(view());
    // Assert
    expect(html.match(/<option/g)).toHaveLength(2);
  });

  it("keeps the daemon's display order", () => {
    // Arrange / Act — order is a resolved fact like every other.
    const html = topbarModelOptionsHtml(view());
    // Assert
    expect(html.indexOf("Opus 5")).toBeLessThan(html.indexOf("Sonnet 5"));
  });

  it("marks the current selection selected", () => {
    // Arrange / Act
    const html = topbarModelOptionsHtml(view({ modelDisplay: "sonnet-5" }));
    // Assert
    expect(html).toContain('value="sonnet-5" title="fast" selected');
  });

  it("renders no options at all when the daemon listed none", () => {
    // Arrange / Act
    const html = topbarModelOptionsHtml(view({ modelOptions: [] }));
    // Assert
    expect(html).toBe("");
  });

  it("escapes an option's display name", () => {
    // Arrange / Act
    const html = topbarModelOptionsHtml(
      view({ modelOptions: [{ value: "m", displayName: "<img src=x>", description: "" }] }),
    );
    // Assert
    expect(html).not.toContain("<img");
  });
});

// --- the tokens disclosure, which replaced the old strip's tokens chip ------

describe("tokensDisclosureHtml", () => {
  function breakdown(over: Partial<TokenBreakdownView> = {}): TokenBreakdownView {
    return {
      workspace: "/ws",
      fence: "f1",
      sections: [
        { label: "session", rows: [{ label: "input", tokens: 10, sharePermille: 1000, emphasized: true, depth: 0 }] },
      ],
      ...over,
    };
  }

  it("renders NOTHING when no breakdown has been published", () => {
    // Arrange / Act — a control over a breakdown that does not exist invites
    // the click that proves it does not exist.
    const html = tokensDisclosureHtml(null, false);
    // Assert
    expect(html).toBe("");
  });

  it("renders the chip once a breakdown has been published", () => {
    // Arrange / Act
    const html = tokensDisclosureHtml(breakdown(), false);
    // Assert
    expect(html).toContain("data-tokens-toggle");
  });

  it("withholds the menu while the disclosure is closed", () => {
    // Arrange / Act
    const html = tokensDisclosureHtml(breakdown(), false);
    // Assert
    expect(html).not.toContain("tb-menu");
  });

  it("renders the resolved menu when the disclosure is open", () => {
    // Arrange / Act
    const html = tokensDisclosureHtml(breakdown(), true);
    // Assert
    expect(html).toContain("tb-menu");
  });

  it("labels the chip with the word alone, composing no figure of its own", () => {
    // Arrange / Act — every candidate source for a figure here is a client-side
    // recomposition of a number the breakdown itself already resolves.
    const html = tokensDisclosureHtml(breakdown(), false);
    // Assert
    expect(html).toContain(">tokens <span");
  });
});

describe("the warning indicator", () => {
  const WARNED = view({
    warnings: [
      { text: "INVALID ACCOUNTING: totals disagree", warning: { kind: "accounting" } },
    ],
  });

  it("renders the red exclamation affordance when the daemon raised a warning", () => {
    // Arrange / Act
    const html = topbarWarningsHtml(WARNED, false);
    // Assert
    expect(html).toContain(WARNING_GLYPH);
  });

  it("renders NO affordance at all when there is nothing to warn about", () => {
    // Arrange / Act — a control over an empty list only invites the click that
    // proves it is empty.
    const html = topbarWarningsHtml(view({ warnings: [] }), false);
    // Assert
    expect(html).toBe("");
  });

  it("renders nothing for a workspace with no published topbar", () => {
    // Arrange / Act
    // Assert
    expect(topbarWarningsHtml(null, false)).toBe("");
  });

  it("keeps the resolved text out of the DOM while it is closed", () => {
    // Arrange / Act — the sentence is disclosure, not strip content.
    const html = topbarWarningsHtml(WARNED, false);
    // Assert
    expect(html).not.toContain("INVALID ACCOUNTING");
  });

  it("shows the daemon's resolved text once opened", () => {
    // Arrange / Act
    const html = topbarWarningsHtml(WARNED, true);
    // Assert
    expect(html).toContain("INVALID ACCOUNTING: totals disagree");
  });

  it("lists every warning in the dropdown, not just the first", () => {
    // Arrange / Act — the indicator is the topbar's warnings, plural.
    const html = topbarWarningsHtml(
      view({
        warnings: [
          { text: "first complaint", warning: { kind: "accounting" } },
          { text: "second complaint", warning: { kind: "accounting" } },
        ],
      }),
      true,
    );
    // Assert
    expect(html).toContain("second complaint");
  });

  it("counts the warnings on the chip", () => {
    // Arrange / Act
    const html = topbarWarningsHtml(
      view({
        warnings: [
          { text: "first complaint", warning: { kind: "accounting" } },
          { text: "second complaint", warning: { kind: "accounting" } },
        ],
      }),
      false,
    );
    // Assert
    expect(html).toContain('class="topbar-warning-count">2<');
  });

  it("escapes the daemon's text", () => {
    // Arrange / Act
    const html = topbarWarningsHtml(
      view({ warnings: [{ text: "<img src=x>", warning: { kind: "accounting" } }] }),
      true,
    );
    // Assert
    expect(html).not.toContain("<img");
  });

  it("is a real button, so it is keyboard-reachable", () => {
    // Arrange / Act — the shared dropdown-chip shell, not a bespoke popover.
    const html = topbarWarningsHtml(WARNED, false);
    // Assert
    expect(html).toContain('<button type="button" class="info-warnings"');
  });

  it("reports its open state to assistive tech", () => {
    // Arrange / Act
    const html = topbarWarningsHtml(WARNED, true);
    // Assert
    expect(html).toContain('aria-expanded="true"');
  });

  it("hangs its overlay off the `.warnings-menu` the dismissal handlers key on", () => {
    // Arrange / Act — outside-click dismissal spares exactly this stem.
    const html = topbarWarningsHtml(WARNED, true);
    // Assert
    expect(html).toContain('class="warnings-menu"');
  });
});
