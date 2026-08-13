/**
 * Single import site for the generated protobuf stubs the shim speaks.
 *
 * The shim sits on THREE of the five surfaces, and re-exporting exactly those
 * three is what makes the surface boundary visible from here:
 *
 *   - `conversation.v1` — the vendor-neutral message model this shim CONVERTS
 *     INTO. It imports nothing, which is what makes it shareable.
 *   - `protocol.v1` — the daemon<->shim boundary in both directions: the
 *     handshake, the commands, the receipts, replay, paging, `ExternalEntry`
 *     (the half of a record allowed to leave) and `BookkeepingEntry`.
 *   - `agentshim.v1` — the shim-side internals: the stored `Entry`, its
 *     `InternalEntry` half, the observation `Plane`, the store write frame, and
 *     the three unconverted arms. THE DAEMON MUST NEVER IMPORT THIS; the shim
 *     is exactly who it is for (`proto/check-conversation-isolation.sh`).
 *
 * `frontend.v1` and `state.v1` are deliberately absent: the shim serves no
 * frontend and holds no daemon state, so a shim source that named either would
 * be reaching across a boundary rather than merely importing a type.
 *
 * The stubs are committed once at proto/gen/ts (shared by shim, sidecar and
 * webapp) and imported relatively; funnelling that long relative path through
 * here keeps the rest of src/ oblivious to the layout and gives one place to
 * update if the generated tree ever moves.
 */

// conversation.v1 — the neutral message model.
export * from "../../../../../proto/gen/ts/conversation/v1/content_pb.js";
export * from "../../../../../proto/gen/ts/conversation/v1/message_pb.js";
export * from "../../../../../proto/gen/ts/conversation/v1/payloads_pb.js";
export * from "../../../../../proto/gen/ts/conversation/v1/tokens_pb.js";

// protocol.v1 — the daemon<->shim boundary.
export * from "../../../../../proto/gen/ts/protocol/v1/bookkeeping_pb.js";
export * from "../../../../../proto/gen/ts/protocol/v1/core_pb.js";
export * from "../../../../../proto/gen/ts/protocol/v1/entry-delivery_pb.js";
export * from "../../../../../proto/gen/ts/protocol/v1/external_pb.js";
export * from "../../../../../proto/gen/ts/protocol/v1/message-page_pb.js";

// agentshim.v1 — shim-side internals, never forwarded.
export * from "../../../../../proto/gen/ts/agentshim/v1/cursor_pb.js";
export * from "../../../../../proto/gen/ts/agentshim/v1/entry_pb.js";
export * from "../../../../../proto/gen/ts/agentshim/v1/unsupported_pb.js";
export * from "../../../../../proto/gen/ts/agentshim/v1/write_pb.js";
