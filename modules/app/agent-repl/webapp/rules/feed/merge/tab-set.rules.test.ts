/**
 * Every merge strip is built by createTabSet (src/feed/merge/tab-set.ts).
 */
import { describe, expect, it } from "vitest";
import MERGE_BODY from "../../../src/feed/merge/merge-body.ts?raw";
import TAB_PANELS from "../../../src/feed/merge/tab-panels.ts?raw";

// EVERY STRIP IS THE ONE TAB SET: the bubble's own and every nested one. A
// module that built a strip by hand would drift from the selection and the
// in-place drawing every other strip has.
describe("every merge strip is built by createTabSet", () => {
  it.each([
    ["merge-body.ts", MERGE_BODY],
    ["tab-panels.ts", TAB_PANELS],
  ])("%s builds its strip through createTabSet", (_name, source) => {
    expect([source.includes("createTabSet("), source.includes('"merge-strip')]).toEqual([true, false]);
  });
});
