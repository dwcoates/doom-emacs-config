/**
 * The web family. WebSearch's `results` array is a UNION — hit lists and bare
 * commentary strings — and that union is exactly what splits into the `link`
 * and `note` arms, so the fixture must carry both.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, toolUseResults } from "../harness.js";

const result = async (prompt: string): Promise<Record<string, unknown>> => {
  const driven = await driveScenario([prompt]);
  return toolUseResults(driven.transcript())[0] as Record<string, unknown>;
};

describe("WebFetch", () => {
  it("answers with the corpus's six fields and nothing invented", async () => {
    // Arrange + Act
    const fetched = await result("!web-fetch");

    // Assert
    expect(Object.keys(fetched).sort()).toEqual(
      ["bytes", "code", "codeText", "durationMs", "result", "url"].sort(),
    );
  });

  it("reports a 200 on the ordinary fetch", async () => {
    // Arrange + Act
    const fetched = await result("!web-fetch");

    // Assert
    expect(fetched.code).toBe(200);
  });

  it("reports the redirect status rather than following it silently", async () => {
    // Arrange + Act
    const redirected = await result("!web-fetch-redirect");

    // Assert
    expect({ code: redirected.code, text: redirected.codeText }).toEqual({ code: 302, text: "Found" });
  });

  it("puts the vendor's redirect instruction in the result body", async () => {
    // Arrange + Act
    const redirected = await result("!web-fetch-redirect");

    // Assert
    expect(String(redirected.result)).toContain("REDIRECT DETECTED");
  });
});

describe("WebSearch", () => {
  it("answers with BOTH result kinds, so link and note are each reachable", async () => {
    // Arrange + Act
    const searched = await result("!web-search");
    const kinds = (searched.results as unknown[]).map((r) => typeof r);

    // Assert
    expect(kinds).toEqual(["object", "string"]);
  });

  it("keys the hit list by a server tool_use id", async () => {
    // Arrange + Act
    const searched = await result("!web-search");
    const hits = (searched.results as { tool_use_id?: string }[])[0];

    // Assert
    expect(hits?.tool_use_id).toMatch(/^srvtoolu_/);
  });

  it("reports the search count separately from the hit count", async () => {
    // Arrange + Act
    const searched = await result("!web-search");

    // Assert
    expect(searched.searchCount).toBe(1);
  });
});
