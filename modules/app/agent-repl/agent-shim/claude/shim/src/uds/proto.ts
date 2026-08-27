/**
 * Single import site for the generated protobuf stubs the shim speaks.
 *
 * THE SURFACES CHANGED WHOLESALE. The three packages this file used to
 * re-export — `protocol.v1`, the old `conversation.v1` message model, and
 * `agentshim.v1` — no longer exist. The daemon<->shim boundary is now
 * `shim.v1`: an rpc service (`shim/v1/service.proto`) with one
 * request/response pair per endpoint, replacing the framed `Event` envelope
 * and its handshake/replay/paging commands outright. The shim-side record
 * layer is now `store.v1`, addressed through `store/v1/service.proto`.
 *
 * WHAT THIS FILE RE-EXPORTS TODAY. Only the leaf vocabulary packages, which
 * are the parts the surviving shim sources can name without asserting an
 * endpoint contract that has no implementation yet:
 *
 *   - `conversation.v1` — the vendor-neutral model the shim converts INTO.
 *   - `workspace.v1` — the daemon-minted workspace and repository refs.
 *
 * The `shim.v1` and `store.v1` endpoint modules are deliberately NOT
 * re-exported yet: re-exporting an endpoint whose handler does not exist would
 * make the boundary look implemented from here. They are added back, endpoint
 * by endpoint, by whoever implements the service.
 *
 * `frontend.v1` and `agentrepl.v1` stay absent for the original reason: the
 * shim serves no frontend and holds no daemon state, so a shim source that
 * named either would be reaching across a boundary rather than importing a
 * type.
 */

// conversation.v1 — the neutral message model.
export * from "../../../../../proto/gen/ts/conversation/v1/api_pb.js";
export * from "../../../../../proto/gen/ts/conversation/v1/slash_command_pb.js";

// workspace.v1 — the daemon-minted identity tokens the shim echoes back.
export * from "../../../../../proto/gen/ts/workspace/v1/workspace_pb.js";
