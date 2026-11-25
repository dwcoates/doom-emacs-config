import { Code, ConnectError } from "@connectrpc/connect";
import { describe, expect, it } from "vitest";

import { isWithheld, WITHHELD_HEADER } from "../../src/rpc/withheld.js";

describe("isWithheld", () => {
  it("reads a refusal the daemon marked withheld", () => {
    expect(isWithheld(new ConnectError("x", Code.FailedPrecondition, { [WITHHELD_HEADER]: "1" }))).toBe(true);
  });

  it("answers false for a ConnectError with no mark", () => {
    expect(isWithheld(new ConnectError("x", Code.FailedPrecondition))).toBe(false);
  });

  it("answers false for an error that is not a ConnectError", () => {
    expect(isWithheld(new Error("x"))).toBe(false);
  });
});
