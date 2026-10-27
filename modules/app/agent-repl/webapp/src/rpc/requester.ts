/**
 * requester — WHO ASKED, as every webapp action names it
 * (`agentrepl.v1.WorkspaceRequester`).
 *
 * The webapp acts for the user alone, and says so on every request that
 * names its asker, so the daemon's record of each action never reads as an
 * agent's. One author for the value, so no call site can name another asker
 * by a slip.
 */
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { WorkspaceRequesterSchema } from "../../../proto/gen/ts/agentrepl/v1/workspace_topology_pb";

/** The user, the one asker the webapp acts for. */
export function userRequester(): MessageInitShape<typeof WorkspaceRequesterSchema> {
  return { asker: { case: "user", value: {} } };
}
