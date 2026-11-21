import { describe, expect, it } from "vitest";
import { agentReplViteServer } from "../../../../bin/vite-server.mjs";
import unitConfig from "../vitest.config.js";
import integrationConfig from "../vitest.integration.config.js";

// Every config serves the agent-repl module through the one shared definition,
// so the generated stubs and images load wherever vitest runs.
describe.each([
  { name: "vitest.config.ts", config: unitConfig },
  { name: "vitest.integration.config.ts", config: integrationConfig },
])("$name", ({ config }) => {
  it("serves the agent-repl module through bin/vite-server.mjs", () => {
    expect(config.server).toBe(agentReplViteServer);
  });
});
