/**
 * A DEPLOYS GROUP'S REPOSITORY HAS ONE READ: the fold's and the merged pages'
 * sources go through `deploysGroupRepository`.
 */
import { describe, expect, it } from "vitest";
import foldSource from "../../src/sidebar/deploys-fold.ts?raw";
import pagesSource from "../../src/sidebar/merged-pages.ts?raw";

describe("deploysGroupRepository", () => {
  it.each([
    { name: "the fold", source: foldSource },
    { name: "the merged pages", source: pagesSource },
  ])("is the one read of a group's repository for $name", ({ source }) => {
    const reads = source.match(/getAttribute\(DEPLOYS_REPOSITORY_ATTRIBUTE\)/g) ?? [];
    const calls = source.match(/deploysGroupRepository\(group, /g) ?? [];
    expect([reads.length, calls.length]).toEqual([source === foldSource ? 1 : 0, 1]);
  });
});
