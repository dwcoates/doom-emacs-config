// @vitest-environment jsdom
import { type Control } from "../../../src/control.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import { OpenExternalResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import {
  FeedCodeSpanSchema,
  FeedToolCallInputSchema,
  FeedToolCallNameSchema,
  FeedDiffLineSchema,
  FeedIdSchema,
  FeedRowSchema,
  FeedSimpleToolCallSchema,
  FeedToolCallReturnedSchema,
  type FeedSimpleToolCall,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker, type Ticker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { TICKING_ATTRIBUTE, stopTicking } from "../../../src/feed/ticking.js";
import { countingTicker } from "../harness.js";
import type { RowContext } from "../../../src/feed/cards/context.js";
import {
  DIAGNOSTICS_VISIBLE,
  TOOL_CALL_FORM_ARMS,
  TOOL_CALL_INPUT_FORM_ARMS,
  TOOL_CALL_OUTCOME_ARMS,
  TOOL_CALL_VERDICT_ARMS,
  drawFeedSimpleToolCall,
} from "../../../src/feed/cards/tool-call.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import { HAS_MORE_CLASS, TITLE_FOLD_CLASS } from "../../../src/feed/bubble-more.js";
import { fireResize } from "../../resize-observer.js";
import { measureTitle } from "../title-measure.js";
import { orderFor } from "../../feed-order.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** A row context whose only verb is the external open a link click makes. */
function rowContext(ticker: Ticker = createTicker(1000)): RowContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openExternal: () =>
        create(OpenExternalResponseSchema, { result: { case: "success", value: {} } }),
    });
  });
  return {
    ctx: testAppContext({
      client: createAgentReplClient(transport),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker,
      failures: SINK,
      composerEnabled: false,
    }),
    feed: "root",
    row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }), order: orderFor("row-1") }),
    revealRow: async () => true,
  };
}

/**
 * A card built from INIT, with a name and an input line filled in when the
 * case under test does not care which they are.
 *
 * The defaults are applied to the CREATED message rather than spread into the
 * initializer, so a test that deliberately leaves a field unset (the malformed
 * cases) builds its own message and this helper never re-supplies it.
 */
function card(init: MessageInitShape<typeof FeedSimpleToolCallSchema>): FeedSimpleToolCall {
  const built = create(FeedSimpleToolCallSchema, init);
  built.name ??= create(FeedToolCallNameSchema, { text: "Bash" });
  built.input ??= create(FeedToolCallInputSchema, { text: "$ go test ./..." });
  return built;
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

  it("covers every input-form arm the schema declares", () => {
    expect([...TOOL_CALL_INPUT_FORM_ARMS].sort()).toEqual(
      armsOf(FeedToolCallInputSchema.oneofs, "form").sort(),
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

  it("draws an unformed input line verbatim as plain text", () => {
    const el = drawFeedSimpleToolCall(
      card({ input: { text: "grep: FeedRow" }, outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".tool-input")?.textContent).toBe("grep: FeedRow");
  });

  it("gives an unformed input line no shell treatment", () => {
    const el = drawFeedSimpleToolCall(
      card({ input: { text: "whatever" }, outcome: { case: "running", value: {} } }),
      rowContext(),
    );
    expect(el.querySelector(".bash-input")).toBeNull();
  });

  it("draws the input line as a hyperlink when the view carries a link", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "example.com/page", link: { url: "https://example.com/page" } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    const anchor = el.querySelector(".tool-input a.external-link");
    expect(anchor?.getAttribute("href")).toBe("https://example.com/page");
  });
});

describe("the input line's drawn form", () => {
  it("draws a command in the shell treatment", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "go test ./...", form: { case: "command", value: {} } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    expect(el.querySelector("pre.cmd.bash-input")).not.toBeNull();
  });

  it("puts the client's shell chrome in front of a command", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "go test ./...", form: { case: "command", value: {} } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    expect(el.querySelector("pre.bash-input")?.textContent).toBe("$ go test ./...");
  });

  it("draws a path in the muted file-path treatment", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "src/render.ts", form: { case: "path", value: {} } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    expect(el.querySelector("div.file-path")?.textContent).toBe("src/render.ts");
  });

  it("gives a path no shell chrome", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "src/render.ts", form: { case: "path", value: {} } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    expect(el.querySelector(".file-path")?.textContent?.startsWith("$")).toBe(false);
  });

  it("draws a query in the query treatment", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: { text: "grep: FeedRow", form: { case: "query", value: {} } },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    expect(el.querySelector("pre.cmd.tool-query")?.textContent).toBe("grep: FeedRow");
  });

  it("keeps a linked line in its own form's treatment", () => {
    const el = drawFeedSimpleToolCall(
      card({
        input: {
          text: "src/render.ts",
          form: { case: "path", value: {} },
          link: { url: "https://example.com/render.ts" },
        },
        outcome: { case: "running", value: {} },
      }),
      rowContext(),
    );
    expect(el.querySelector("div.file-path a.external-link")?.getAttribute("href")).toBe(
      "https://example.com/render.ts",
    );
  });

  it("refuses an input form arm this build does not know", () => {
    const built = card({ outcome: { case: "running", value: {} } });
    built.input = create(FeedToolCallInputSchema, { text: "x" });
    (built.input as { form: unknown }).form = { case: "sonar", value: {} };
    expect(() => drawFeedSimpleToolCall(built, rowContext())).toThrow(MalformedView);
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

  it("reads the nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the beat does not share the shared ticker's phase.
    vi.setSystemTime(11_920);
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: { lastProgress: { atMs: 7000n } } } }),
      rowContext(),
    );
    // Assert: five real seconds of silence reads 5s, not the lagging 4s.
    expect(el.querySelector(".tool-quiet")?.textContent).toBe("quiet for 5s");
  });

  it("marks the quiet clock, so a stop from outside the card can reach it", () => {
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: { lastProgress: { atMs: 7000n } } } }),
      rowContext(),
    );
    expect(el.querySelector(".tool-quiet")?.hasAttribute(TICKING_ATTRIBUTE)).toBe(true);
  });

  it("is stopped by stopTicking on the card, which a direct subscription defeated", () => {
    const ticker = countingTicker();
    const el = drawFeedSimpleToolCall(
      card({ outcome: { case: "running", value: { lastProgress: { atMs: 7000n } } } }),
      rowContext(ticker),
    );
    stopTicking(el);
    expect(ticker.live()).toBe(0);
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

describe("a terminal draw's clocks", () => {
  it("subscribes to nothing when the call has returned", () => {
    const ticker = countingTicker();
    drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: { verdict: { case: "succeeded", value: {} }, form: { case: "text", value: { text: "ok" } } },
        },
      }),
      rowContext(ticker),
    );
    expect(ticker.live()).toBe(0);
  });

  it("subscribes to nothing when the call was denied", () => {
    const ticker = countingTicker();
    drawFeedSimpleToolCall(card({ outcome: { case: "denied", value: {} } }), rowContext(ticker));
    expect(ticker.live()).toBe(0);
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

describe("the no-output form", () => {
  it("omits the output section whole", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "none", value: {} },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-output")).toBeNull();
  });

  it("draws no omitted line in place of the missing output", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "none", value: {} },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-omitted")).toBeNull();
  });

  it("still draws the verdict badge", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "none", value: {} },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".badge.ok")?.textContent).toBe("done");
  });

  it("still draws the settled runtime beside the badge", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "none", value: {} },
            runtime: { text: "ran 0.1 s" },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-head .tool-runtime")?.textContent).toBe("ran 0.1 s");
  });

  it("still draws the diagnostics an edit with no output raised", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "none", value: {} },
            diagnostics: { lines: ["render.ts:1 · error · nope"] },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".tool-diagnostic")?.textContent).toBe("render.ts:1 · error · nope");
  });
});

/**
 * WHICH DRAWING A TEXT OUTPUT TAKES (owner request, 2026-10-08): a unified
 * diff is drawn as the classic diff, markdown from a call that is not a shell
 * is drawn formatted, and everything else, a failed call's text included,
 * verbatim.
 */
describe("the text output form's drawing", () => {
  const DIFF = "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n";
  const MARKDOWN = "# Title\n\nbody";

  /** A returned text card: TEXT, from a shell when SHELL, failed when FAILED. */
  function textCard(text: string, shell: boolean, failed = false): HTMLElement {
    return drawFeedSimpleToolCall(
      card({
        input: shell
          ? { text: "git diff", form: { case: "command", value: {} } }
          : { text: "https://example.com" },
        outcome: {
          case: "returned",
          value: {
            verdict: failed ? { case: "failed", value: {} } : { case: "succeeded", value: {} },
            form: { case: "text", value: { text } },
          },
        },
      }),
      rowContext(),
    );
  }

  it.each([
    { name: "a shell's unified diff", text: DIFF, shell: true, failed: false, want: ".diff-classic" },
    { name: "a unified diff from a call that is not a shell", text: DIFF, shell: false, failed: false, want: ".diff-classic" },
    { name: "a failed call's unified diff", text: DIFF, shell: true, failed: true, want: ".bash-output.stderr" },
    { name: "markdown from a call that is not a shell", text: MARKDOWN, shell: false, failed: false, want: ".tool-output-md" },
    { name: "markdown a shell printed", text: MARKDOWN, shell: true, failed: false, want: "pre.bash-output" },
    { name: "plain text", text: "done", shell: false, failed: false, want: "pre.bash-output" },
  ])("draws $name as $want", ({ text, shell, failed, want }) => {
    // Arrange / Act
    const el = textCard(text, shell, failed);

    // Assert
    expect(el.querySelector("[data-output-body]")?.matches(want)).toBe(true);
  });

  it("draws a shell's unified diff with no +/- marker on its changed lines", () => {
    // Arrange / Act
    const el = textCard(DIFF, true);

    // Assert
    const changed = [...el.querySelectorAll('[data-diff-line="removed"], [data-diff-line="added"]')];
    expect(changed.map((line) => line.textContent)).toEqual(["old", "new"]);
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
  // THE CLASSIC DIFF (owner request, 2026-10-08): the kind is the line's
  // treatment, and the text is drawn with no +/- or space marker.
  const CASES = [
    ["header", "hunk", "@@ -3,7 +3,9 @@", "@@ -3,7 +3,9 @@"],
    ["added", "add", "a line", "a line"],
    ["removed", "del", "a line", "a line"],
    ["context", "ctx", "a line", "a line"],
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
    const lines = [...el.querySelectorAll(".diff-output [data-diff-line]")];
    expect(lines.map((l) => [l.getAttribute("data-diff-line"), l.textContent])).toEqual([
      ["removed", "old"],
      ["added", "new"],
    ]);
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
    el.querySelector<Control>(".tool-diagnostics-more")?.click();
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
    const u = card({});
    // Arrange: the shape a NEWER daemon's arm arrives in.
    (u as { outcome: unknown }).outcome = { case: "teleported", value: {} };
    expect(() => drawFeedSimpleToolCall(u, rowContext())).toThrow(MalformedView);
  });
});

/**
 * The three arms a NEWER daemon could set on a RETURNED card. Each is planted
 * on the built fixture rather than passed to `create`, which would drop a case
 * the frozen schema has no field for.
 */
describe("a returned card a newer daemon wrote", () => {
  it("names the verdict arm it cannot draw", () => {
    // Arrange
    const u = card({
      outcome: {
        case: "returned",
        value: create(FeedToolCallReturnedSchema, {
          verdict: { case: "succeeded", value: {} },
          form: { case: "text", value: { text: "x" } },
        }),
      },
    });
    const returned = u.outcome.value as { verdict: unknown };
    returned.verdict = { case: "partiallySucceeded", value: {} };
    // Act
    const thrown = (() => {
      try {
        drawFeedSimpleToolCall(u, rowContext());
        return undefined;
      } catch (err) {
        return err;
      }
    })();
    // Assert
    expect([
      thrown instanceof MalformedView,
      (thrown as MalformedView).detail,
    ]).toEqual([true, "arm 'partiallySucceeded' is not one this build can draw"]);
  });

  it("names the output form arm it cannot draw", () => {
    // Arrange
    const u = card({
      outcome: {
        case: "returned",
        value: create(FeedToolCallReturnedSchema, {
          verdict: { case: "succeeded", value: {} },
          form: { case: "text", value: { text: "x" } },
        }),
      },
    });
    const returned = u.outcome.value as { form: unknown };
    returned.form = { case: "spectrogram", value: {} };
    // Act
    const thrown = (() => {
      try {
        drawFeedSimpleToolCall(u, rowContext());
        return undefined;
      } catch (err) {
        return err;
      }
    })();
    // Assert
    expect([
      thrown instanceof MalformedView,
      (thrown as MalformedView).detail,
    ]).toEqual([true, "arm 'spectrogram' is not one this build can draw"]);
  });

  it("names the diff line kind it cannot draw", () => {
    // Arrange
    const line = create(FeedDiffLineSchema, {
      text: "a line",
      kind: { case: "added", value: {} },
    });
    (line as { kind: unknown }).kind = { case: "moved", value: {} };
    const u = card({
      outcome: {
        case: "returned",
        value: create(FeedToolCallReturnedSchema, {
          verdict: { case: "succeeded", value: {} },
          form: { case: "diff", value: { lines: [line] } },
        }),
      },
    });
    // Act
    const thrown = (() => {
      try {
        drawFeedSimpleToolCall(u, rowContext());
        return undefined;
      } catch (err) {
        return err;
      }
    })();
    // Assert
    expect([
      thrown instanceof MalformedView,
      (thrown as MalformedView).detail,
    ]).toEqual([true, "arm 'moved' is not one this build can draw"]);
  });
});

describe("the image output form (landing 16)", () => {
  it("draws the shared image block with the daemon's resolved src", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "image", value: { src: "data:image/png;base64,iVBORw==", alt: "" } },
          },
        },
      }),
      rowContext(),
    );
    const img = el.querySelector<HTMLImageElement>("img.prompt-block-image");
    expect(img?.getAttribute("src")).toBe("data:image/png;base64,iVBORw==");
  });

  it("draws the daemon's alt text on the image", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: {
              case: "image",
              value: { src: "data:image/png;base64,iVBORw==", alt: "screencapture -x -" },
            },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector<HTMLImageElement>("img.prompt-block-image")?.alt).toBe(
      "screencapture -x -",
    );
  });

  it("states the image arm as the card's output form", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "image", value: { src: "data:image/png;base64,iVBORw==", alt: "" } },
          },
        },
      }),
      rowContext(),
    );
    expect(el.getAttribute("data-output-form")).toBe("image");
  });
});

describe("the returned card's exit chip (landing 16)", () => {
  it("draws the code the command reported, on the head", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "failed", value: {} },
            form: { case: "text", value: { text: "boom" } },
            exit: { code: 3 },
          },
        },
      }),
      rowContext(),
    );
    const chip = el.querySelector(".tool-head .shell-exit");
    expect(chip?.textContent).toBe("exit 3");
    expect(chip?.getAttribute("data-exit-code")).toBe("3");
  });

  it("gives a zero exit the ok tone, as the detached shell's chip does", () => {
    const el = drawFeedSimpleToolCall(
      card({
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "text", value: { text: "ok" } },
            exit: { code: 0 },
          },
        },
      }),
      rowContext(),
    );
    expect(el.querySelector(".shell-exit")?.className).toBe("badge ok shell-exit");
  });

  it("draws NO chip when the producer stated no exit code", () => {
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
    expect(el.querySelector(".shell-exit")).toBeNull();
  });
});

/**
 * THE CARD-LEVEL FOLD (owner ruling, 2026-09-15). A tool card is ONE
 * click-to-expand unit: collapsed it shows the head (the title in full) and its
 * input line (capped at two rows), its output section HIDDEN — no preview —
 * until the whole card is `.expanded`, at which point the section is revealed
 * (scrolling at 50vh) and the input line's two-row cap is lifted.
 */
describe("the card-level fold", () => {
  /** A returned Bash card: a command input line and a text output section. */
  function bashCard() {
    return drawFeedSimpleToolCall(
      card({
        name: { text: "Bash" },
        input: { text: "go test ./...", form: { case: "command", value: {} } },
        outcome: {
          case: "returned",
          value: {
            verdict: { case: "succeeded", value: {} },
            form: { case: "text", value: { text: "ok\nok\nok" } },
          },
        },
      }),
      rowContext(),
    );
  }

  it("marks the whole card one fold, collapsed by default", () => {
    // Arrange / Act
    const el = bashCard();
    // Assert — the card is the capped section, and it starts closed.
    expect(el.classList.contains("tool-fold")).toBe(true);
    expect(el.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("shows NO output-section preview while collapsed", () => {
    // Arrange — the real stylesheet, so the collapse rule can win the cascade.
    const remove = installStylesheet();
    try {
      const el = bashCard();
      document.body.replaceChildren(el);
      // Act / Assert — the section is hidden entirely, not a height-capped peek.
      expect(cascadedValue(el.querySelector(".tool-output") as Element, "display")).toBe("none");
    } finally {
      remove();
    }
  });

  it("caps the collapsed header's input line at one row", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const el = bashCard();
      document.body.replaceChildren(el);
      // Act / Assert — the non-title header content is clamped to one text row,
      // which the engine ends in an ellipsis when more follows.
      expect(cascadedValue(el.querySelector(".bash-input") as Element, "-webkit-line-clamp")).toBe(
        "1",
      );
    } finally {
      remove();
    }
  });

  it("never caps the tool name in the head, which is not the card's title", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const el = bashCard();
      document.body.replaceChildren(el);
      // Act / Assert — the title (`.tool-name`, in `.tool-head`) is not line-clamped.
      expect(cascadedValue(el.querySelector(".tool-name") as Element, "-webkit-line-clamp")).not.toBe(
        "2",
      );
    } finally {
      remove();
    }
  });

  it("expands the whole card when the reader clicks the head", () => {
    // Arrange — the feed-wide click-to-expand, armed over the card.
    const el = bashCard();
    const feed = document.createElement("div");
    feed.append(el);
    document.body.replaceChildren(feed);
    installClickExpand(feed, () => "");
    // Act — a click on the head, the collapsed face.
    (el.querySelector(".tool-head") as HTMLElement).dispatchEvent(
      new MouseEvent("click", { bubbles: true }),
    );
    // Assert
    expect(el.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("reveals the section, bounded by the card's ceiling alone, once expanded", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const el = bashCard();
      el.classList.add(EXPANDED_CLASS);
      document.body.replaceChildren(el);
      const out = el.querySelector(".tool-output") as Element;
      // Act / Assert
      expect(cascadedValue(out, "display")).toBe("block");
      expect(cascadedValue(out, "max-height")).toBe("none");
    } finally {
      remove();
    }
  });

  it("lifts the header's two-row cap once expanded", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const el = bashCard();
      el.classList.add(EXPANDED_CLASS);
      document.body.replaceChildren(el);
      // Act / Assert — the full input line shows alongside the revealed section.
      expect(cascadedValue(el.querySelector(".bash-input") as Element, "-webkit-line-clamp")).toBe(
        "none",
      );
    } finally {
      remove();
    }
  });
});

/**
 * THE INPUT LINE IS THE CARD'S TITLE (owner ruling, 2026-09-23): the one
 * two-line title fold (title-fold.ts), owned by the card's own `.tool-fold`.
 */
describe("the title fold on the input line", () => {
  /** A connected Bash card of OUTCOME, and its input line. */
  function drawn(outcome: MessageInitShape<typeof FeedSimpleToolCallSchema>["outcome"]) {
    const el = drawFeedSimpleToolCall(
      card({ input: { text: "cd /some/path && cat some_file.txt", form: { case: "command", value: {} } }, outcome }),
      rowContext(),
    );
    document.body.replaceChildren(el);
    return { el, title: el.querySelector(".bash-input") as HTMLElement };
  }

  const RETURNED = {
    case: "returned",
    value: { verdict: { case: "succeeded", value: {} }, form: { case: "text", value: { text: "ok" } } },
  } as const;

  it("marks the input line with the one title-fold class", () => {
    // Arrange / Act
    const { title } = drawn({ case: "running", value: {} });

    // Assert
    expect(title.classList.contains(TITLE_FOLD_CLASS)).toBe(true);
  });

  it("wears has-more when the input line overflows its two rows", () => {
    // Arrange
    const { title } = drawn({ case: "running", value: {} });
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps has-more off an input line that fits its two rows", () => {
    // Arrange
    const { title } = drawn({ case: "running", value: {} });
    measureTitle(title, false);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("drops has-more once the card is expanded", () => {
    // Arrange
    const { el, title } = drawn({ case: "running", value: {} });
    measureTitle(title, true);
    fireResize(title);
    el.classList.add(EXPANDED_CLASS);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("keeps measuring a returned card's title after the card's terminal stop", () => {
    // Arrange
    const { title } = drawn(RETURNED);
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("stops measuring once the card is discarded", () => {
    // Arrange
    const { el, title } = drawn(RETURNED);

    // Act
    stopTicking(el);

    // Assert — nothing observes the title any more, so a fire has no target.
    expect(() => fireResize(title)).toThrow();
  });
});
