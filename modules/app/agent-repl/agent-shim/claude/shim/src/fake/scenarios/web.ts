/**
 * fake/scenarios/web.ts — WebFetch and WebSearch.
 *
 * Both `toolUseResult` shapes are the corpus's (`tool-results/web_fetch.jsonl`,
 * `web_search.jsonl`). The search result's `results` array is deliberately
 * MIXED — one entry of hits and one bare commentary string — because that union
 * is what `AgentWebSearchEntry` splits into `link` and `note`, and a fixture
 * with only links could not reach the note arm.
 */
import { conclude, scenario } from "./support.js";

const WEB_FETCH = scenario({
  name: "web-fetch",
  prompt: "!web-fetch",
  emits: "a `WebFetch` answered with the corpus shape: bytes, code, codeText, result, durationMs, url",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentWebFetch.start + AgentWebFetchSuccess",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "web-fetch" }, "fake web-fetch turn");
    const call = ctx.toolUse("WebFetch", {
      url: "https://example.com/docs",
      prompt: "What does this page document?",
    });
    ctx.toolResult(call, "The page documents the example API.", {
      bytes: 626,
      code: 200,
      codeText: "OK",
      result: "The page documents the example API.",
      durationMs: 158,
      url: "https://example.com/docs",
    });
    conclude(ctx, "Fetched and summarized the page.");
  },
});

const WEB_FETCH_REDIRECT = scenario({
  name: "web-fetch-redirect",
  prompt: "!web-fetch-redirect",
  emits: "a `WebFetch` answered with a 302 and the vendor's redirect instruction as the result body",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentWebFetchSuccess carrying a non-2xx status",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "web-fetch-redirect" }, "fake redirected web-fetch turn");
    const call = ctx.toolUse("WebFetch", { url: "https://api.example.com/methods", prompt: "List the methods." });
    const body =
      "REDIRECT DETECTED: The URL redirects to a different host.\n\n" +
      "Original URL: https://api.example.com/methods\n" +
      "Redirect URL: https://docs.example.com/methods\nStatus: 302 Found";
    ctx.toolResult(call, body, {
      bytes: 626,
      code: 302,
      codeText: "Found",
      result: body,
      durationMs: 158,
      url: "https://api.example.com/methods",
    });
    conclude(ctx, "The fetch was redirected.");
  },
});

const WEB_SEARCH = scenario({
  name: "web-search",
  prompt: "!web-search",
  emits: "a `WebSearch` answered with BOTH result kinds — a hit list keyed by a server tool_use id, and a bare commentary string",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentWebSearch.start + AgentWebSearchSuccess with entry=link AND entry=note",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "web-search" }, "fake web-search turn");
    const call = ctx.toolUse("WebSearch", { query: "example api reference" });
    ctx.toolResult(call, "Found two pages.", {
      query: "example api reference",
      results: [
        {
          tool_use_id: "srvtoolu_fake_01",
          content: [
            { title: "Example API reference", url: "https://docs.example.com/reference/" },
            { title: "Example changelog", url: "https://docs.example.com/changelog/" },
          ],
        },
        "The reference page covers every method; the changelog lists recent additions.",
      ],
      durationSeconds: 6.92,
      searchCount: 1,
    });
    conclude(ctx, "Searched the web and summarized the hits.");
  },
});

export const WEB_SCENARIOS = [WEB_FETCH, WEB_FETCH_REDIRECT, WEB_SEARCH];
