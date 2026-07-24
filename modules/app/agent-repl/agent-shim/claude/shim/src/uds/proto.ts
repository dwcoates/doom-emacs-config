/**
 * Single import site for the generated agentshim.core.v1 protobuf stubs.
 *
 * The stubs are committed once at proto/gen/ts (shared by claude-shim and
 * the webapp) and imported relatively; funnelling that long relative path
 * through here keeps the rest of src/uds/ oblivious to the layout and gives
 * one place to update if the generated tree ever moves.
 */
export * from "../../../../../proto/gen/ts/agentshim/core/v1/core_pb.js";
