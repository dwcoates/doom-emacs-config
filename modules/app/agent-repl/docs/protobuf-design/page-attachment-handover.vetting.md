# Vetting register: page attachment at a daemon handover

Investigations owed on assumptions about systems outside the contract.

## WebKit opens a new connection for each unary call

- Assumption: because the daemon answers every HTTP/1.1 call with
  `Connection: close` (`daemon/cmd/agent-repld/run.go`, `closingHTTP1`), a page's
  unary call after the claim yield is always a new connection, accepted by the
  claim holder.
- Affected if false: nothing in the contract. Routing by
  `PageAttachment.instance` is correct whichever daemon accepts the call. The
  assumption only bears on how often a call is forwarded.
- How to verify: count `daemon.server.forward` records for `SubscribePage` per
  handover in the daemon logs.
- Status: OPEN.

## A connect-go client stream reports response headers when they arrive

- Assumption: `ServerStreamForClient.ResponseHeader()` returns once the
  upstream's headers are received, before any message, over the daemon's h2c
  forward client. The relay's acceptance rests on it.
- Affected if false: a relayed subscription whose upstream is accepted and
  quiet would not answer `SubscribePage` until its first push.
- How to verify: the server unit test that relays a quiet accepted stream and
  asserts the subscribe answers before any push.
- Status: OPEN until that test passes.
