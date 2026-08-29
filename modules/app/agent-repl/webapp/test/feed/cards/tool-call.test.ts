// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import { OpenExternalResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import {
  FeedCodeSpanSchema,
  FeedDiffLineSchema,
  FeedIdSchema,
  FeedRowSchema,
  FeedSimpleToolCallSchema,
  FeedToolCallReturnedSchema,
  type FeedSimpleToolCall,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { createAppContext } from "../../../src/rpc/context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import type { RowContext } from "../../../src/feed/cards/context.js";
import {
  DIAGNOSTICS_VISIBLE,
  TOOL_CALL_FORM_ARMS,
  TOOL_CALL_OUTCOME_ARMS,
  TOOL_CALL_VERDICT_ARMS,
  drawFeedSimpleToolCall,
} from "../../../src/feed/cards/tool-call.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** A row context whose only verb is the external open a link click makes. */
function rowContext(): RowContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openExternal: () =>
        create(OpenExternalResponseSchema, { result: { case: "success", value: {} } }),
    });
  });
  return {
    ctx: createAppContext({
      client: createAgentReplClient(transport),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      composerEnabled: false,
    }),
    feed: "root",
    row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }) }),
    revealRow: async () => true,
  };
}

/** A card with NAME, the composed line TEXT, and the outcome ARM. */
function card(init: Partial<FeedSimpleToolCall>): FeedSimpleToolCall {
  return create(FeedSimpleToolCallSchema, {
    name: { text: "Bash" },
    input: { text: "$ go test ./..." },
    ...init,
  });
}

/** The proto field names of one oneof, as generated arm case names. */
function armsOf(oneofs: readonly { name: string; fields: readonly { name: string }[] }[], name: string): string[] {
  const oneof = oneofs.find((o) => o.name === name);
  if (oneof === undefined) throw new Error(`no oneof named ${name}`);
  return oneof.fields.map((f) => f.name.replace(/_([a-z])/g, (_m, c: string) => c.toUpperCase()));
}

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

describe("the arms this module claims to draw", () => {
  it("covers every outcome arm the schema declares", () => {
    expect([...TOOL_CALL_OUTCOME_ARMS].sort()).toEqual(
      armsOf(FeedSimpleToolCallSchema.oneofs, "outcome").sort(),
    );
  });

  it("covers every verdict arm the schema declares", () => {
    expect([...TOOL_CALL_VERDICT_ARMS].sort()).toEqual(
      armsOf(FeedToolCallReturnedSchema.oneofs, "verdict").sort(),
    );
  });

  it("covers every output-form arm the schema declares", () => {
    expect([...TOOL_CALL_FORM_ARMS].sort()).toEqual(
      armsOf(FeedToolCallReturnedSchema.oneofs, "form").sort(),
    );
  });
});

describe("the card shell", () => {
  it("draws the tool name verbatim", () => {
    const el = drawFeedSimpleToolCall(
      card({ name: { text: "Grep" }, outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".tool-name")?.textContent).toBe("Grep");
  });

  it("carries the outcome arm as the card's state", () => {
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.getAttribute("data-state")).toBe("running");
  });

  it("draws the composed input line verbatim as plain text", () => {
    const el = drawFeedSimpleToolCall(
      card({ input: { text: "grep: FeedRow" }, outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".bash-input")?.textContent).toBe("grep: FeedRow");
  });

  it("draws the input line as a hyperlink when the view carries a link", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "example.com/page", link: { url: "https://example.com/page" } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    const anchor = el.querySelector(".bash-input a.external-link");
    expect(anchor?.getAttribute("href")).toBe("https://example.com/page");
  });
});

describe("the running state", () => {
  it("draws the run badge", () => {
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".badge.run")?.textContent).toContain("running");
  });

  it("draws no quiet-for clock before the first beat", () => {
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".tool-quiet")).toBeNull();
  });

  it("draws the quiet-for clock from the beat's instant", () => {
    vi.setSystemTime(10_000);
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: { lastProgress: { atMs: 7000n } } } }),
      rowContext(),
    );
    expect(el.querySelector(".tool-quiet")?.textContent).toBe("quiet for 3s");
  });

  it("ticks the quiet-for clock on the shared ticker", () => {
    vi.setSystemTime(10_000);
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: { lastProgress: { atMs: 7000n } } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    vi.advanceTimersByTime(2000);
    expect(el.querySelector(".tool-quiet")?.textContent).toBe("quiet for 5s");
    el.remove();
  });

  it("stops ticking once the card has left the document", () => {
    vi.setSystemTime(10_000);
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: { lastProgress: { atMs: 7000n } } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    vi.advanceTimersByTime(1000);
    el.remove();
    vi.advanceTimersByTime(5000);
    expect(el.querySelector(".tool-quiet")?.textContent).toBe("quiet for 4s");
  });
});

describe("the denied state", () => {
  it("draws the denied badge", () => {
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "denied", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".badge")?.textContent).toBe("denied");
  });

  it("draws no output section at all", () => {
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "denied", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".tool-output")).toBeNull();
  });
});

describe("the returned state", () => {
  it("draws the ok badge for a succeeded verdict", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: { verdict: { case: "succeeded", value: {} }, form: { case: "text", value: { text: "ok" } } },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".badge.ok")?.textContent).toBe("done");
  });

  it("draws the err badge for a failed verdict", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: { verdict: { case: "failed", value: {} }, form: { case: "text", value: { text: "boom" } } },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".badge.err")?.textContent).toBe("error");
  });

  it("carries the verdict arm on the card", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: { verdict: { case: "failed", value: {} }, form: { case: "text", value: { text: "boom" } } },
        },
      }),
      rowContext(),
    );
    expect(el.getAttribute("data-verdict")).toBe("failed");
  });

  it("draws the composed runtime beside the badge when present", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "text", value: { text: "ok" } },
            runtime: { text: "ran 4.2 s" },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-head .tool-runtime")?.textContent).toBe("ran 4.2 s");
  });

  it("draws no runtime figure when the view carries none", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: { verdict: { case: "succeeded", value: {} }, form: { case: "text", value: { text: "ok" } } },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-runtime")).toBeNull();
  });
});

describe("the text output form", () => {
  it("draws the text verbatim in the capped box", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "text", value: { text: "PASS\nok  1.2s" } },
          },
        },
      }),
      rowContext(),
    );
    const out = el.querySelector(".tool-output.bash-output");
    expect(out?.textContent).toBe("PASS\nok  1.2s");
  });

  it("wears the error hue when the call failed", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "failed", value: {} },
            form: { case: "text", value: { text: "boom" } },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-output")?.classList.contains("stderr")).toBe(true);
  });
});

describe("the code output form", () => {
  const spans = [
    create(FeedCodeSpanSchema, { text: "const", paintClass: "keyword" }),
    create(FeedCodeSpanSchema, { text: " x = 1", paintClass: "" }),
  ];

  function coded(omitted?: string): HTMLElement {
    return drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "code",
              value: {
                spans,
                omitted: omitted === undefined ? undefined : { text: omitted },
              },
            },
          },
        },
      }),
      rowContext(),
    );
  }

  it("paints a span whose class is in the inventory", () => {
    expect(coded().querySelector("code .paint-keyword")?.textContent).toBe("const");
  });

  it("draws a plain span with no class at all", () => {
    const plain = [...coded().querySelectorAll("code span")][1];
    expect(plain.className).toBe("");
  });

  it("draws a span whose class this build does not know as unstyled text", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "code",
              value: { spans: [create(FeedCodeSpanSchema, { text: "x", paintClass: "kwyjibo" })] },
            },
          },
        },
      }),
      rowContext(),
    );
    const span = el.querySelector("code span");
    expect([span?.className, span?.textContent]).toEqual(["", "x"]);
  });

  it("concatenating the spans recovers the code", () => {
    expect(coded().querySelector("code")?.textContent).toBe("const x = 1");
  });

  it("draws the omitted line when the head was truncated", () => {
    expect(coded("showing 200 of 4,312").querySelector(".tool-omitted")?.textContent).toBe(
      "showing 200 of 4,312",
    );
  });

  it("draws no omitted line when nothing was omitted", () => {
    expect(coded().querySelector(".tool-omitted")).toBeNull();
  });
});

describe("the diff output form", () => {
  const CASES = [
    ["header", "hunk", "@@ -3,7 +3,9 @@", " @@ -3,7 +3,9 @@"],
    ["added", "add", "a line", "+a line"],
    ["removed", "del", "a line", "-a line"],
    ["context", "ctx", "a line", " a line"],
  ] as const;

  it.each(CASES)("draws a %s line with the %s treatment", (kind, cls, text, drawn) => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "diff",
              value: {
                lines: [
                  create(FeedDiffLineSchema, { kind: { case: kind, value: {} }, text }),
                ],
              },
            },
          },
        },
      }),
      rowContext(),
    );
    const line = el.querySelector(`.diff .${cls}`);
    expect([line?.getAttribute("data-diff-line"), line?.textContent]).toEqual([kind, drawn]);
  });

  it("keeps the lines in the order the view carried them", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "diff",
              value: {
                lines: [
                  create(FeedDiffLineSchema, { kind: { case: "removed", value: {} }, text: "old" }),
                  create(FeedDiffLineSchema, { kind: { case: "added", value: {} }, text: "new" }),
                ],
              },
            },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".diff-output")?.textContent).toBe("-old\n+new");
  });
});

describe("the lines output form", () => {
  it("draws the lines verbatim, in order", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "lines", value: { lines: ["a.ts", "b.ts"] } },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-output")?.textContent).toBe("a.ts\nb.ts");
  });

  it("draws the composed floor when the list is short", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "lines", value: { lines: ["a.ts"], omitted: { text: "42 more" } } },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-omitted")?.textContent).toBe("42 more");
  });
});

describe("the links output form", () => {
  function linked(url?: string): HTMLElement {
    return drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "links",
              value: {
                links: [
                  {
                    text: "A result",
                    url: url === undefined ? undefined : { url },
                  },
                ],
              },
            },
          },
        },
      }),
      rowContext(),
    );
  }

  it("draws a row with a url as a link", () => {
    const anchor = linked("https://example.com").querySelector(".tool-link-row a.external-link");
    expect([anchor?.getAttribute("href"), anchor?.textContent]).toEqual([
      "https://example.com",
      "A result",
    ]);
  });

  it("draws a row without a url as narration text", () => {
    const row = linked().querySelector(".tool-link-row");
    expect([row?.querySelector("a"), row?.textContent]).toEqual([null, "A result"]);
  });

  it("delimits the rows with the one shared list rule", () => {
    expect(linked().querySelector(".tool-links")?.classList.contains("list-rows")).toBe(true);
  });

  it("draws the composed floor when the list is capped", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "links",
              value: { links: [{ text: "one" }], omitted: { text: "9 more not shown" } },
            },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-omitted")?.textContent).toBe("9 more not shown");
  });
});

describe("diagnostics", () => {
  function withLines(count: number): HTMLElement {
    return drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "text", value: { text: "ok" } },
            diagnostics: {
              lines: Array.from({ length: count }, (_v, i) => `render.ts:${i} · error · nope`),
            },
          },
        },
      }),
      rowContext(),
    );
  }

  it("draws no diagnostics box when the view carries none", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "text", value: { text: "ok" } },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-diagnostics")).toBeNull();
  });

  it("draws every line when they fit under the cap", () => {
    const el = withLines(DIAGNOSTICS_VISIBLE);
    const hidden = [...el.querySelectorAll<HTMLElement>(".tool-diagnostic")].filter((r) => r.hidden);
    expect(hidden).toEqual([]);
  });

  it("offers no toggle when they fit under the cap", () => {
    expect(withLines(DIAGNOSTICS_VISIBLE).querySelector(".tool-diagnostics-more")).toBeNull();
  });

  it("hides the overflow behind a toggle naming its count", () => {
    const el = withLines(DIAGNOSTICS_VISIBLE + 2);
    expect(el.querySelector(".tool-diagnostics-more")?.textContent).toBe("+2 more");
  });

  it("reveals the overflow in place when the toggle is pressed", () => {
    const el = withLines(DIAGNOSTICS_VISIBLE + 2);
    el.querySelector<HTMLButtonElement>(".tool-diagnostics-more")?.click();
    const hidden = [...el.querySelectorAll<HTMLElement>(".tool-diagnostic")].filter((r) => r.hidden);
    expect(hidden).toEqual([]);
  });
});

describe("a malformed card", () => {
  it("refuses an unset outcome", () => {
    expect(() => drawFeedSimpleToolCall(card({}), rowContext())).toThrow(MalformedView);
  });

  it("refuses an unset name", () => {
    const u = create(FeedSimpleToolCallSchema, {
      input: { text: "x" },
      outcome: { case: "running", value: {} },
    });
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses an unset input", () => {
    const u = create(FeedSimpleToolCallSchema, {
      name: { text: "Bash" },
      outcome: { case: "running", value: {} },
    });
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses a returned call with no verdict", () => {
    const u = card({
      outcome: { case: "returned", value: { form: { case: "text", value: { text: "x" } } } },
    });
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses a returned call with no output form", () => {
    const u = card({
      outcome: { case: "returned", value: { verdict: { case: "succeeded", value: {} } } },
    });
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses a diff line with no kind", () => {
    const u = card({
      outcome: {
        case: "returned",
        value: {
          verdict: { case: "succeeded", value: {} },
          form: {
            case: "diff",
            value: { lines: [create(FeedDiffLineSchema, { text: "a line" })] },
          },
        },
      },
    });
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses an arm this build has no case for", () => {
    const u = card({}) as FeedSimpleToolCall;
    // Arrange: the shape a NEWER daemon's arm arrives in.
    (u as { outcome: unknown }).outcome = { case: "teleported", value: {} };
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });
});
