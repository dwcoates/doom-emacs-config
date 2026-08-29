/**
 * The fold SEAM. There is no conversion here yet — the fold agent owns
 * `convert/` — so what is pinned is the shape the seam promises: the three
 * destinations an SDK message's output can reach, and the empty answer the
 * exempt set produces.
 */
import { describe, expect, it } from "vitest";
import { EMPTY_FOLD_OUTPUT, type Fold, type FoldOutput } from "../../src/convert/fold.js";
import type { SdkMessage } from "../../src/sdk/types.js";

describe("EMPTY_FOLD_OUTPUT", () => {
  it("produces nothing on all three destinations, which is the exempt set's answer", () => {
    // Arrange, Act, Assert.
    expect(EMPTY_FOLD_OUTPUT).toEqual({ frames: [], sessionUpdates: [], residue: [] });
  });
});

describe("Fold", () => {
  it("is implementable with one synchronous method, so message order cannot interleave", () => {
    // Arrange.
    const seen: string[] = [];
    const fold: Fold = {
      onSdkMessage(message: SdkMessage): FoldOutput {
        seen.push(message.type);
        return EMPTY_FOLD_OUTPUT;
      },
    };

    // Act.
    fold.onSdkMessage({ type: "result" } as SdkMessage);

    // Assert.
    expect(seen).toEqual(["result"]);
  });
});
