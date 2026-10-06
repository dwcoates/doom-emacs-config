// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  DismissNewsDigestResponseSchema,
  type DismissNewsDigestRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_dismiss_news_digest_pb";
import {
  OpenExternalResponseSchema,
  type OpenExternalRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import {
  NewsDigestStandingSchema,
  type NewsDigestStanding,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import { NewsDigestOverlaySchema } from "../../../proto/gen/ts/frontend/v1/news_digest_pb";
import {
  CLOSE_LABEL,
  formatPeriod,
  mountNewsDigest,
  type NewsDigestHandle,
} from "../../src/news-digest/news-digest.js";
import type { AppContext } from "../../src/rpc/context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { RecordingSink, appContext } from "../topbar/fixtures.js";

type OverlayInit = MessageInitShape<typeof NewsDigestOverlaySchema>;

const FROM_MS = 1_727_769_600_000n;
const TO_MS = 1_727_856_000_000n;

/** A whole overlay: a backend section of one dated item, a feature section, two sources. */
function overlayInit(): OverlayInit {
  return {
    id: { value: "digest-1" },
    header: { title: { text: "Claude news · Oct 2" }, period: { fromMs: FROM_MS, toMs: TO_MS } },
    sections: [
      {
        heading: { text: "Affects the agent-repl backend" },
        kind: { kind: { case: "backend", value: {} } },
        items: [
          {
            title: { text: "SDK drops subscription billing" },
            summary: { text: "The SDK now needs an API key." },
            effective: { text: "2026-11-01" },
            links: [{ label: "Release notes", url: "https://fixture.test/releases/v9" }],
          },
        ],
      },
      {
        heading: { text: "New features" },
        kind: { kind: { case: "feature", value: {} } },
        items: [
          {
            title: { text: "A new model" },
            summary: { text: "It is faster." },
            links: [{ label: "Announcement", url: "https://fixture.test/news/model" }],
          },
        ],
      },
    ],
    sources: {
      sources: [
        { name: "Agent SDK releases", url: "https://fixture.test/releases", outcome: { case: "read", value: { newEntries: 3 } } },
        { name: "Anthropic news", url: "https://fixture.test/news", outcome: { case: "failed", value: { reason: "fetch: HTTP 503" } } },
      ],
    },
  };
}

const shown = (init: OverlayInit = overlayInit()): NewsDigestStanding =>
  create(NewsDigestStandingSchema, { standing: { case: "shown", value: init } });

const none = (): NewsDigestStanding => create(NewsDigestStandingSchema, { standing: { case: "none", value: {} } });

interface World {
  ctx: AppContext;
  host: HTMLElement;
  feedScroll: HTMLElement;
  digest: NewsDigestHandle;
  dismissals: DismissNewsDigestRequest[];
  opened: OpenExternalRequest[];
}

/** A mounted overlay whose dismiss answers ANSWER. */
function world(answer: "success" | "unknownDigest" | "throw" = "success"): World {
  const dismissals: DismissNewsDigestRequest[] = [];
  const opened: OpenExternalRequest[] = [];
  const ctx = appContext(
    {
      dismissNewsDigest: (req) => {
        dismissals.push(req);
        if (answer === "throw") throw new ConnectError("gone", Code.Unavailable);
        return create(DismissNewsDigestResponseSchema, {
          result:
            answer === "success"
              ? { case: "success", value: {} }
              : { case: "error", value: { cause: { case: "unknownDigest", value: {} } } },
        });
      },
      openExternal: (req) => {
        opened.push(req);
        return create(OpenExternalResponseSchema, { result: { case: "success", value: {} } });
      },
    },
    new RecordingSink(),
  );
  const host = document.createElement("div");
  host.setAttribute("data-component", "news-digest");
  const feedScroll = document.createElement("div");
  document.body.replaceChildren(feedScroll, host);
  const digest = mountNewsDigest(host, ctx, { feedScroll });
  return { ctx, host, feedScroll, digest, dismissals, opened };
}

/** Let the scripted transport answer. */
const flush = async (): Promise<void> => {
  for (let i = 0; i < 10; i += 1) await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
};

const closeControl = (host: HTMLElement): HTMLElement => {
  const close = host.querySelector<HTMLElement>("[data-news-digest-close]");
  if (close === null) throw new Error("no close control is drawn");
  return close;
};

let w: World;

beforeEach(() => {
  w = world();
});

afterEach(() => {
  w.digest.dispose();
});

describe("mountNewsDigest: drawing the standing", () => {
  it("ships hidden", () => {
    // ARRANGE (beforeEach mounted it)
    // ACT
    const hidden = w.host.hidden;
    // ASSERT
    expect(hidden).toBe(true);
  });

  it("draws a shown digest over the feed", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    expect(w.host.hidden).toBe(false);
    expect(w.host.querySelector(".news-digest-title")?.textContent).toBe("Claude news · Oct 2");
    expect([...w.host.querySelectorAll(".news-digest-section-heading")].map((h) => h.textContent)).toEqual([
      "Affects the agent-repl backend",
      "New features",
    ]);
  });

  it("draws an item's title and summary verbatim", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    const item = w.host.querySelector(".news-digest-item");
    expect(item?.querySelector(".news-digest-item-title")?.textContent).toBe("SDK drops subscription billing");
    // The summary is rendered markdown now (owner, 2026-10-06): a paragraph,
    // whose text carries the renderer's trailing newline.
    expect(item?.querySelector(".news-digest-summary")?.textContent?.trim()).toBe("The SDK now needs an API key.");
  });

  /** A shown digest whose first item carries TITLE and SUMMARY. */
  function shownWith(title: string, summary: string): ReturnType<typeof shown> {
    const init = overlayInit();
    const item = init.sections?.[0]?.items?.[0];
    if (item === undefined) throw new Error("the fixture has a first item");
    item.title = { text: title };
    item.summary = { text: summary };
    return shown(init);
  }

  it("renders a summary's inline code as code, not literal backticks", () => {
    // ACT
    w.digest.apply(shownWith("t", "Set `apiKey` before use."));
    // ASSERT
    const code = w.host.querySelector(".news-digest-summary code");
    expect(code?.textContent).toBe("apiKey");
    expect(w.host.querySelector(".news-digest-summary")?.textContent).not.toContain("`");
  });

  it("renders a summary with the response bubble's markdown class", () => {
    // ACT
    w.digest.apply(shownWith("t", "s"));
    // ASSERT
    expect(w.host.querySelector(".news-digest-summary")?.classList.contains("md")).toBe(true);
  });

  it("renders a summary's block markdown: a list is a list", () => {
    // ACT
    w.digest.apply(shownWith("t", "- one\n- two"));
    // ASSERT
    expect([...w.host.querySelectorAll(".news-digest-summary li")].map((li) => li.textContent)).toEqual(["one", "two"]);
  });

  it("renders a title's inline code as code", () => {
    // ACT
    w.digest.apply(shownWith("The `query()` call changed", "s"));
    // ASSERT
    expect(w.host.querySelector(".news-digest-item-title code")?.textContent).toBe("query()");
  });

  it("escapes markup in a title rather than drawing it", () => {
    // ACT
    w.digest.apply(shownWith("<b>bold</b>", "s"));
    // ASSERT
    expect(w.host.querySelector(".news-digest-item-title b")).toBeNull();
    expect(w.host.querySelector(".news-digest-item-title")?.textContent).toBe("<b>bold</b>");
  });

  it("draws the period in the reader's locale", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    expect(w.host.querySelector(".news-digest-period")?.textContent).toBe(
      formatPeriod(Number(FROM_MS), Number(TO_MS)),
    );
  });

  it("draws an item's effective date when the daemon states one", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    const items = w.host.querySelectorAll(".news-digest-item");
    expect(items[0]?.querySelector("[data-effective]")?.textContent).toBe("effective 2026-11-01");
  });

  it("draws no effective date when the daemon states none", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    const items = w.host.querySelectorAll(".news-digest-item");
    expect(items[1]?.querySelector("[data-effective]")).toBeNull();
  });

  it("marks each section with its kind arm", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    expect([...w.host.querySelectorAll(".news-digest-section")].map((s) => s.getAttribute("data-kind"))).toEqual([
      "backend",
      "feature",
    ]);
  });

  it("accents the backend section as a warning", () => {
    // ARRANGE
    const removeStylesheet = installStylesheet();
    w.digest.apply(shown());
    const heading = w.host.querySelector('.news-digest-section[data-kind="backend"] .news-digest-section-heading');
    if (heading === null) throw new Error("no backend heading is drawn");
    // ACT
    const color = cascadedValue(heading, "color");
    removeStylesheet();
    // ASSERT
    expect(color).toBe("var(--err)");
  });

  it("draws a read source with its count of new entries", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    const read = w.host.querySelector('.news-digest-source[data-outcome="read"]');
    expect(read?.querySelector(".news-digest-source-outcome")?.textContent).toBe("3 new");
  });

  it("draws a failed source with the daemon's reason", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    const failed = w.host.querySelector('.news-digest-source[data-outcome="failed"]');
    expect(failed?.querySelector(".news-digest-source-outcome")?.textContent).toBe("failed: fetch: HTTP 503");
  });

  it("replaces a drawn digest whole with the next one", () => {
    // ARRANGE
    w.digest.apply(shown());
    const next = overlayInit();
    next.header = { title: { text: "Claude news · Oct 3" }, period: { fromMs: FROM_MS, toMs: TO_MS } };
    // ACT
    w.digest.apply(shown(next));
    // ASSERT
    expect(w.host.querySelectorAll(".news-digest-panel")).toHaveLength(1);
    expect(w.host.querySelector(".news-digest-title")?.textContent).toBe("Claude news · Oct 3");
  });

  it("removes the digest on none", () => {
    // ARRANGE
    w.digest.apply(shown());
    // ACT
    w.digest.apply(none());
    // ASSERT
    expect(w.host.hidden).toBe(true);
    expect(w.host.childElementCount).toBe(0);
  });

  it("covers the feed scroll zone's own box", () => {
    // ARRANGE
    w.feedScroll.getBoundingClientRect = () => new DOMRect(240, 36, 800, 600);
    // ACT
    w.digest.apply(shown());
    // ASSERT
    expect([w.host.style.top, w.host.style.left, w.host.style.width, w.host.style.height]).toEqual([
      "36px",
      "240px",
      "800px",
      "600px",
    ]);
  });
});

/** An overlay whose "Since last week" holds one dated regression risk. */
function riskyInit(): OverlayInit {
  const init = overlayInit();
  init.week = {
    heading: { text: "Since last week" },
    outcome: {
      case: "risks",
      value: {
        items: [
          {
            item: {
              title: { text: "Sonnet 4 retires" },
              summary: { text: "It retires." },
              effective: { text: "2026-11-01" },
              links: [{ label: "Deprecations", url: "https://fixture.test/deprecations" }],
            },
            reason: { text: "The daemon's `claude -p` calls name it." },
          },
        ],
      },
    },
  };
  return init;
}

/** An overlay whose "Since last week" is quiet. */
function quietInit(): OverlayInit {
  const init = overlayInit();
  init.week = {
    heading: { text: "Since last week" },
    outcome: { case: "quiet", value: { text: "Nothing since last week could regress agent-repl." } },
  };
  return init;
}

describe("mountNewsDigest: since last week", () => {
  it("draws the week first, before the run's sections", () => {
    // ACT
    w.digest.apply(shown(riskyInit()));
    // ASSERT
    const sections = [...w.host.querySelectorAll<HTMLElement>(".news-digest-body > .news-digest-section")];
    expect(sections.map((s) => s.getAttribute("data-week") ?? s.getAttribute("data-kind"))).toEqual([
      "risks",
      "backend",
      "feature",
    ]);
  });

  it("titles the week with the daemon's heading", () => {
    // ACT
    w.digest.apply(shown(riskyInit()));
    // ASSERT
    expect(w.host.querySelector("[data-week] .news-digest-section-heading")?.textContent).toBe("Since last week");
  });

  it("draws a risk as any section's item, its date included", () => {
    // ACT
    w.digest.apply(shown(riskyInit()));
    // ASSERT
    const item = w.host.querySelector('[data-week="risks"] .news-digest-item');
    expect([
      item?.querySelector(".news-digest-item-title")?.textContent,
      item?.querySelector("[data-effective]")?.textContent,
      item?.querySelector(".news-digest-links .external-link")?.textContent,
    ]).toEqual(["Sonnet 4 retires", "effective 2026-11-01", "Deprecations"]);
  });

  it("draws a risk's reason as markdown under its summary", () => {
    // ACT
    w.digest.apply(shown(riskyInit()));
    // ASSERT
    const reason = w.host.querySelector<HTMLElement>('[data-week="risks"] [data-risk-reason]');
    expect([reason?.querySelector("code")?.textContent, reason?.previousElementSibling?.textContent?.trim()]).toEqual([
      "claude -p",
      "It retires.",
    ]);
  });

  it("draws no reason on the run's own items", () => {
    // ACT
    w.digest.apply(shown(riskyInit()));
    // ASSERT
    expect(w.host.querySelectorAll("[data-risk-reason]").length).toBe(1);
  });

  it("tells a quiet week in the daemon's words", () => {
    // ACT
    w.digest.apply(shown(quietInit()));
    // ASSERT
    expect(w.host.querySelector('[data-week="quiet"] [data-week-quiet]')?.textContent).toBe(
      "Nothing since last week could regress agent-repl.",
    );
  });

  it("draws no item in a quiet week", () => {
    // ACT
    w.digest.apply(shown(quietInit()));
    // ASSERT
    expect(w.host.querySelectorAll("[data-week] .news-digest-item").length).toBe(0);
  });

  it("draws no week for a digest made before the week existed", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    expect(w.host.querySelector("[data-week]")).toBeNull();
  });
});

describe("mountNewsDigest: refusing a malformed standing", () => {
  it("refuses a week naming no outcome", () => {
    // ARRANGE
    const init = overlayInit();
    init.week = { heading: { text: "Since last week" } };
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a week with no heading", () => {
    // ARRANGE
    const init = quietInit();
    if (init.week) init.week.heading = undefined;
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a week of no risks", () => {
    // ARRANGE
    const init = overlayInit();
    init.week = { heading: { text: "Since last week" }, outcome: { case: "risks", value: { items: [] } } };
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a risk with no reason", () => {
    // ARRANGE
    const init = riskyInit();
    const risk = init.week?.outcome?.case === "risks" ? init.week.outcome.value.items?.[0] : undefined;
    if (risk) risk.reason = undefined;
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a standing naming no arm", () => {
    // ACT + ASSERT
    expect(() => w.digest.apply(create(NewsDigestStandingSchema, {}))).toThrow(MalformedView);
  });

  it("refuses a digest with no id", () => {
    // ARRANGE
    const init = overlayInit();
    init.id = { value: "" };
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a section with no kind", () => {
    // ARRANGE
    const init = overlayInit();
    init.sections = [{ heading: { text: "h" }, kind: {}, items: overlayInit().sections?.[0]?.items }];
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a section with no items", () => {
    // ARRANGE
    const init = overlayInit();
    init.sections = [{ heading: { text: "h" }, kind: { kind: { case: "release", value: {} } }, items: [] }];
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses an item that links to no source", () => {
    // ARRANGE
    const init = overlayInit();
    init.sections = [
      {
        heading: { text: "h" },
        kind: { kind: { case: "release", value: {} } },
        items: [{ title: { text: "t" }, summary: { text: "s" }, links: [] }],
      },
    ];
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });

  it("refuses a source row naming no outcome", () => {
    // ARRANGE
    const init = overlayInit();
    init.sources = { sources: [{ name: "n", url: "https://fixture.test/n" }] };
    // ACT + ASSERT
    expect(() => w.digest.apply(shown(init))).toThrow(MalformedView);
  });
});

describe("mountNewsDigest: dismissing", () => {
  it("names its close control", () => {
    // ACT
    w.digest.apply(shown());
    // ASSERT
    expect(closeControl(w.host).textContent).toBe(CLOSE_LABEL);
  });

  it("sends the served id when closed", async () => {
    // ARRANGE
    w.digest.apply(shown());
    // ACT
    closeControl(w.host).click();
    await flush();
    // ASSERT
    expect(w.dismissals.map((req) => req.id?.value)).toEqual(["digest-1"]);
  });

  it("leaves the overlay up until the daemon pushes none", async () => {
    // ARRANGE
    w.digest.apply(shown());
    // ACT
    closeControl(w.host).click();
    await flush();
    // ASSERT
    expect(w.host.hidden).toBe(false);
  });

  it("dismisses on Escape", async () => {
    // ARRANGE
    w.digest.apply(shown());
    // ACT
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await flush();
    // ASSERT
    expect(w.dismissals.map((req) => req.id?.value)).toEqual(["digest-1"]);
  });

  it("sends nothing on Escape while no digest is drawn", async () => {
    // ACT
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await flush();
    // ASSERT
    expect(w.dismissals).toEqual([]);
  });

  it("sends nothing on Escape once disposed", async () => {
    // ARRANGE
    w.digest.apply(shown());
    w.digest.dispose();
    // ACT
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await flush();
    // ASSERT
    expect(w.dismissals).toEqual([]);
  });

  it("draws an unknown_digest refusal at the close control", async () => {
    // ARRANGE
    w.digest.dispose();
    w = world("unknownDigest");
    w.digest.apply(shown());
    // ACT
    closeControl(w.host).click();
    await flush();
    // ASSERT
    expect(w.host.querySelector('.news-digest-actions .refusal[data-arm="unknownDigest"]')?.textContent).toBe(
      "a newer digest has replaced this one",
    );
  });

  it("draws a dismiss that never landed at the close control", async () => {
    // ARRANGE
    w.digest.dispose();
    w = world("throw");
    w.digest.apply(shown());
    // ACT
    closeControl(w.host).click();
    await flush();
    // ASSERT
    expect(w.host.querySelector('.news-digest-actions .refusal[data-arm="transport"]')).not.toBeNull();
  });
});

describe("mountNewsDigest: links", () => {
  it("opens an item's link in the system browser", async () => {
    // ARRANGE
    w.digest.apply(shown());
    const link = w.host.querySelector<HTMLElement>(".news-digest-links [data-external-link]");
    if (link === null) throw new Error("no item link is drawn");
    // ACT
    link.click();
    await flush();
    // ASSERT
    expect(w.opened.map((req) => req.url)).toEqual(["https://fixture.test/releases/v9"]);
  });

  it("opens a source's own page in the system browser", async () => {
    // ARRANGE
    w.digest.apply(shown());
    const link = w.host.querySelector<HTMLElement>(".news-digest-source [data-external-link]");
    if (link === null) throw new Error("no source link is drawn");
    // ACT
    link.click();
    await flush();
    // ASSERT
    expect(w.opened.map((req) => req.url)).toEqual(["https://fixture.test/releases"]);
  });
});
