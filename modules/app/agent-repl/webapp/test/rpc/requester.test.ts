import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { WorkspaceRequesterSchema } from "../../../proto/gen/ts/agentrepl/v1/workspace_topology_pb";
import { userRequester } from "../../src/rpc/requester.js";

describe("userRequester", () => {
  it("names the user as the asker", () => {
    expect(create(WorkspaceRequesterSchema, userRequester()).asker.case).toBe("user");
  });

  it("answers a fresh value each call, so no caller can change another's", () => {
    expect(userRequester()).not.toBe(userRequester());
  });
});
