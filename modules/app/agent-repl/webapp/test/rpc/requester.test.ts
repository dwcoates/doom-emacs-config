import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { WorkspaceRequesterSchema } from "../../../proto/gen/ts/agentrepl/v1/workspace_topology_pb";
import { userRequester } from "../../src/rpc/requester.js";
import { codeUnder } from "../files-under.js";

const SRC = fileURLToPath(new URL("../../src", import.meta.url));

describe("userRequester", () => {
  it("names the user as the asker", () => {
    expect(create(WorkspaceRequesterSchema, userRequester()).asker.case).toBe("user");
  });

  it("answers a fresh value each call, so no caller can change another's", () => {
    expect(userRequester()).not.toBe(userRequester());
  });

  it("is the one place a webapp source names the user as its asker", () => {
    const handRolled = [...codeUnder(SRC)]
      .filter(([file, code]) => file !== "rpc/requester.ts" && code.includes('asker: { case: "user"'))
      .map(([file]) => file);
    expect(handRolled).toEqual([]);
  });
});
