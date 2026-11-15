// @vitest-environment jsdom
// Which daemon serves a workspace while a handover moves it, as the sidebar
// reads it off a refusal. See src/sidebar/handover.ts.
import { describe, expect, it } from "vitest";

import { MalformedView } from "../../src/rpc/malformed.js";
import { contextAt, handoverAddress, HANDOVER_ARMS, isHandoverArm } from "../../src/sidebar/handover.js";
import { codeOf } from "../source-text.js";
import { appContext } from "./harness.js";

const PATH = "SelectInEditorError.cause";

describe("isHandoverArm", () => {
  it.each([
    ["notYetAdopted", true],
    ["transferringAway", true],
    ["unknownWorkspace", false],
    ["standingDown", false],
    ["", false],
  ])("answers %s as %s", (arm, expected) => {
    // Act, Assert
    expect(isHandoverArm(arm)).toBe(expected);
  });
});

describe("handoverAddress", () => {
  it.each([
    ["a successor transferring_away names", { case: "transferringAway", value: { address: "127.0.0.1:6666" } }, "127.0.0.1:6666"],
    ["a predecessor not_yet_adopted names", { case: "notYetAdopted", value: { address: "127.0.0.1:5555" } }, "127.0.0.1:5555"],
    ["no daemon for a not_yet_adopted naming none", { case: "notYetAdopted", value: {} }, null],
    ["no daemon for a not_yet_adopted with an empty address", { case: "notYetAdopted", value: { address: "" } }, null],
    ["no daemon for an arm that is no handover", { case: "unknownWorkspace", value: {} }, null],
    ["no daemon for an unset cause", {}, null],
  ])("answers %s", (_name, cause, expected) => {
    // Act, Assert
    expect(handoverAddress(PATH, cause)).toBe(expected);
  });

  it.each([
    ["no address", { case: "transferringAway", value: {} }],
    ["an empty address", { case: "transferringAway", value: { address: "" } }],
  ])("throws a malformed view for a transferring_away with %s", (_name, cause) => {
    // Act, Assert
    expect(() => handoverAddress(PATH, cause)).toThrow(MalformedView);
  });

  it("names the missing field's wire path in the malformed view", () => {
    // Act
    let thrown: unknown;
    try {
      handoverAddress(PATH, { case: "transferringAway", value: {} });
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect((thrown as MalformedView).path).toBe("SelectInEditorError.cause.transferringAway.address");
  });
});

describe("contextAt", () => {
  it("dials the daemon at the address through the host's clientAt", () => {
    // Arrange
    const served = appContext();
    const dialed: string[] = [];
    const ctx = {
      ...appContext(),
      clientAt: (at: string) => {
        dialed.push(at);
        return served.client;
      },
    };

    // Act
    const unary = contextAt(ctx, "127.0.0.1:6666");

    // Assert
    expect(dialed).toEqual(["127.0.0.1:6666"]);
    expect(unary?.client).toBe(served.client);
  });

  it("shares the page's failures", () => {
    // Arrange
    const ctx = { ...appContext(), clientAt: () => appContext().client };

    // Act, Assert
    expect(contextAt(ctx, "127.0.0.1:6666")?.failures).toBe(ctx.failures);
  });

  it("goes quiet with the page", () => {
    // Arrange
    const page = appContext();
    const ctx = { ...page, clientAt: () => appContext().client };
    const unary = contextAt(ctx, "127.0.0.1:6666");

    // Act
    page.quiesce();

    // Assert
    expect(unary?.isQuiesced()).toBe(true);
  });

  it("answers null on a page that cannot dial another daemon", () => {
    // Act, Assert
    expect(contextAt(appContext(), "127.0.0.1:6666")).toBeNull();
  });
});

describe("the sidebar's handover call sites", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  const sources = import.meta.glob("../../src/sidebar/**/*.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/handover.ts"));

  it("dial another daemon only through contextAt", () => {
    // Act
    const handRolled = others.filter(([, source]) => /\.clientAt\(/.test(codeOf(source))).map(([path]) => path);

    // Assert
    expect(handRolled).toEqual([]);
  });

  it.each(HANDOVER_ARMS)("never test the %s arm by hand", (arm) => {
    // Act
    const handRolled = others.filter(([, source]) => codeOf(source).includes(`=== "${arm}"`)).map(([path]) => path);

    // Assert
    expect(handRolled).toEqual([]);
  });
});
