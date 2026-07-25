import { describe, it, expect, vi } from "vitest";

vi.mock("@xterm/xterm", () => {
  return {
    Terminal: class {
      rows = 24;
      cols = 80;
      loadAddon() {}
      open() {}
      write() {}
      onData() {}
      focus() {}
      dispose() {}
    },
  };
});
vi.mock("@xterm/addon-fit", () => {
  return {
    FitAddon: class {
      fit() {}
    },
  };
});

describe("scratch", () => {
  it("imports login-terminal", async () => {
    const mod = await import("../src/login-terminal.js");
    expect(mod.attachLoginTerminal).toBeDefined();
  });
});
