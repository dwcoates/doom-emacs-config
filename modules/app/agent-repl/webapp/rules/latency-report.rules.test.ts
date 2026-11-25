/** A page installs its latency reporter in one place (src/latency-report.ts's installDaemonLatencyReport). */
import { describe, expect, it } from "vitest";
import { codeOf } from "../test/source-text.js";

describe("installDaemonLatencyReport", () => {
  it("is the one place a page installs its reporter", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const sources = import.meta.glob("../src/**/*.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;

    // Act
    const installers = Object.entries(sources)
      .filter(([path]) => !path.endsWith("/latency-report.ts"))
      .filter(([, source]) => /\binstallLatencyReport\(/.test(codeOf(source)))
      .map(([path]) => path);

    // Assert
    expect(installers).toEqual([]);
    expect(codeOf(sources["../src/main.ts"] ?? "")).toMatch(/installDaemonLatencyReport\(ctx, ctx\.workspace\)/);
    expect(codeOf(sources["../src/host/sidebar-page.ts"] ?? "")).toMatch(/installDaemonLatencyReport\)\(started\.ctx\)/);
  });
});
