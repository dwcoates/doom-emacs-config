// @vitest-environment jsdom
//
// The fast-mode cell. One test per arm, plus the two absences that are not
// arms: the vendor having said nothing, and the arm being one this bundle
// cannot name.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarFastModeSchema,
  type TopbarFastMode,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawTopbarFastMode } from "../../src/topbar/fast-mode.js";

/** One fast-mode message at the given state. */
function fastMode(state: TopbarFastMode["state"]): TopbarFastMode {
  return { ...create(TopbarFastModeSchema, {}), state };
}

describe("drawTopbarFastMode", () => {
  it("draws the on state as its own cell", () => {
    const cell = drawTopbarFastMode(
      fastMode({
        case: "on",
        value: create(TopbarFastModeSchema, {}) as never,
      }),
    );
    expect(cell?.getAttribute("data-fast-mode")).toBe("on");
  });

  it("labels the on state", () => {
    const cell = drawTopbarFastMode(
      fastMode({
        case: "on",
        value: create(TopbarFastModeSchema, {}) as never,
      }),
    );
    expect(cell?.textContent).toBe("fast");
  });

  it("draws the off state as its own cell", () => {
    const cell = drawTopbarFastMode(
      fastMode({ case: "off", value: { reason: "preference" } as never }),
    );
    expect(cell?.getAttribute("data-fast-mode")).toBe("off");
  });

  it("keeps the vendor's off reason verbatim in the tooltip", () => {
    const cell = drawTopbarFastMode(
      fastMode({ case: "off", value: { reason: "preference" } as never }),
    );
    expect(cell?.title).toBe("preference");
  });

  it("says off plainly when the vendor gave no reason", () => {
    const cell = drawTopbarFastMode(
      fastMode({ case: "off", value: { reason: "" } as never }),
    );
    expect(cell?.title).toBe("fast mode is off");
  });

  // COOLDOWN IS NOT OFF. Drawing them the same would invite a reader to go
  // looking for a switch that cannot take effect.
  it("draws cooldown as its own cell, never as off", () => {
    const cell = drawTopbarFastMode(
      fastMode({
        case: "cooldown",
        value: create(TopbarFastModeSchema, {}) as never,
      }),
    );
    expect(cell?.getAttribute("data-fast-mode")).toBe("cooldown");
  });

  it("labels cooldown distinctly from off", () => {
    const cooldown = drawTopbarFastMode(
      fastMode({
        case: "cooldown",
        value: create(TopbarFastModeSchema, {}) as never,
      }),
    );
    const off = drawTopbarFastMode(
      fastMode({ case: "off", value: { reason: "" } as never }),
    );
    expect(cooldown?.textContent).not.toBe(off?.textContent);
  });

  // UNSET IS NOT OFF EITHER: the vendor has stated nothing, so the strip
  // states nothing.
  it("draws nothing at all when the view carries no fast mode", () => {
    expect(drawTopbarFastMode(undefined)).toBeNull();
  });

  it("draws nothing when the fast-mode message carries no arm", () => {
    expect(drawTopbarFastMode(create(TopbarFastModeSchema, {}))).toBeNull();
  });

  it("refuses an arm this bundle cannot name", () => {
    expect(() =>
      drawTopbarFastMode(fastMode({ case: "turbo", value: {} } as never)),
    ).toThrow(MalformedView);
  });
});
