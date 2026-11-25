/**
 * `createDaemonTransport` IS THE ONLY WAY THE WEBAPP'S SOURCES BUILD A DAEMON
 * TRANSPORT (`src/rpc/transport.ts`).
 */
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder } from "../files-under.js";

describe("createPageTransport", () => {
  it("is the only way the webapp's sources build a daemon transport", () => {
    // Arrange: every source file but the transport module itself.
    const sources = [...codeUnder(join(__dirname, "../../src"))].filter(([file]) => file !== join("rpc", "transport.ts"));
    // Act
    const byHand = sources.filter(([, code]) => code.includes("createDaemonTransport(")).map(([file]) => file);
    // Assert
    expect(byHand).toEqual([]);
  });
});
