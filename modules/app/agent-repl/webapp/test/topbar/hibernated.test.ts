// @vitest-environment jsdom
//
// The hibernated cell: the literal word, and the age it ticks from the wire's
// instant.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { TopbarHibernatedSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { drawTopbarHibernated } from "../../src/topbar/hibernated.js";
import { NOW, appContext, fakeTicker, topbarContext } from "./fixtures.js";

describe("drawTopbarHibernated", () => {
  it("draws the literal word with the age", () => {
    const { tc } = topbarContext();
    const cell = drawTopbarHibernated(
      create(TopbarHibernatedSchema, { sinceMs: BigInt(NOW - 90_000) }),
      tc,
    );
    expect(cell.textContent).toBe("hibernated 1m 30s");
  });

  it("ticks the age forward on the shared clock", () => {
    const ticker = fakeTicker();
    const { tc } = topbarContext(appContext({}, undefined, ticker));
    const cell = drawTopbarHibernated(
      create(TopbarHibernatedSchema, { sinceMs: BigInt(NOW) }),
      tc,
    );

    ticker.set(NOW + 5_000);

    expect(cell.textContent).toBe("hibernated 5s");
  });
});
