// Whether two optional protobuf messages say the same thing. See src/rpc/optional-equal.ts.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterVersionSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { sameOptional } from "../../src/rpc/optional-equal.js";
import { codeOf } from "../source-text.js";

const version = (text: string) => create(RosterVersionSchema, { text });

describe("sameOptional", () => {
  it.each([
    { name: "agrees on two unset values", a: undefined, b: undefined, want: true },
    { name: "agrees on two equal messages", a: version("1.2.3"), b: version("1.2.3"), want: true },
    { name: "disagrees on two different messages", a: version("1.2.3"), b: version("1.2.4"), want: false },
    { name: "disagrees on a value set on the left only", a: version("1.2.3"), b: undefined, want: false },
    { name: "disagrees on a value set on the right only", a: undefined, b: version("1.2.3"), want: false },
  ])("$name", ({ a, b, want }) => {
    // Act
    const same = sameOptional(RosterVersionSchema, a, b);

    // Assert
    expect(same).toBe(want);
  });

  it("is how the rail's head and the feed's hover stamp compare their optional messages", () => {
    // Arrange
    const src = join(__dirname, "..", "..", "src");
    const sidebar = codeOf(readFileSync(join(src, "sidebar", "sidebar.ts"), "utf8"));
    const feed = codeOf(readFileSync(join(src, "feed", "feed-view.ts"), "utf8"));

    // Act / Assert
    expect([sidebar.includes("sameOptional(RosterVersionSchema"), feed.includes("sameOptional(FeedHoverStampSchema")]).toEqual([true, true]);
    expect([sidebar, feed].map((code) => /=== undefined \|\| \w+(\.\w+)? === undefined\) return/.test(code))).toEqual([false, false]);
  });
});
