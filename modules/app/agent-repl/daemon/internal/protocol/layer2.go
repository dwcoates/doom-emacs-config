package protocol

// Layer2Version is the wire-compatibility version of the Layer-2
// protocol, carried in every hello and in the GET /sessions envelope.
// Clients compare it against the version they were built for and
// surface a mismatch instead of mis-parsing frames. Bump on any
// breaking frame-shape change. Version 2 = boot-id era (1 = before).
const Layer2Version = 2

// Every Layer-2 daemon→webapp FRAME TYPE that used to live here (Envelope,
// L2Frame and the ~45 *Frame shapes) was deleted in the D-phase census: the
// Layer-2 streaming plane died with the agent-shim cutover and both frontends
// consume agentshim.frontend.v1 instead. Only the version constant survives,
// because GET /sessions still reports it.
