# The not_yet_adopted refusal names the daemon that serves the workspace

A workspace must stay viewable whichever daemon serves it during a rolling
handover. Before this change, a joining successor refused every per-workspace
rpc for a workspace it had not adopted yet (`not_yet_adopted`), and that arm
carried nothing. Emacs and the webapp therefore waited for a reattach before
they opened the tab or made a switch. A workspace the incumbent still served
(mid-turn, merging) was unviewable for the whole handover. The sidebar even
drew the refusal as a red error under the row (2026-10-30 ~17:55 to 17:59 on
`restrict-workspace-test-runs`, `agent-repl-slack-formatting`,
`agent-repl-memory-measure`; the webapp recorded `sidebar.verbs.refused`
`SelectInEditor notYetAdopted` at 17:59:45, and Emacs recorded
`elisp.open-tabs.refused-handover ... cause=not-yet-adopted reattached=nil` at
17:41:19).

## Assumptions (autonomous design)

- **Objective:** a client can always view and act on a workspace against the
  daemon that currently serves it. A handover is a lifecycle state, and the
  client never treats it as an error.
- **Built:** the refusal names where to go, in the same way `transferring_away`
  already names the successor.
- **Frontend presence:** there is no new UI component. The handover is drawn by
  the existing handover and footer affordances, and the sidebar row draws
  nothing for it.
- **Systems:**
  - The proto (`agentrepl.v1`).
  - The daemon: the server's one refusal path (`server.fill`) and the rollout
    controller.
  - The elisp wire decoders and Emacs's host and open-tabs.
  - The webapp's refusal wording and the sidebar's verbs.
- **Non-additive changes:** none to the schema. The field is added to messages
  that were empty. Each client changes how it reacts to the arm: it routes to
  the named daemon instead of waiting.

## Iteration sequence

The protocol is unchanged (ConnectRPC). The endpoint inventory is unchanged.
The one cross-cutting convention is the `not_yet_adopted` arm's shape, which is
settled once and applied to every endpoint. No per-endpoint stage remains.

## Landed changes

### Every `<Rpc>NotYetAdopted` carries `optional string address = 1`

- **What:** all 67 `agentrepl.v1` `*NotYetAdopted` arm messages gain
  `optional string address = 1`. It holds the `127.0.0.1:<port>` of the daemon
  that serves the workspace until the joining daemon adopts it, which is the
  handover's incumbent.
- **Why:** this makes viewing structurally independent of which daemon serves
  the workspace. The refusal tells the client where the workspace is served
  now, so the client opens or acts on it there. The later `transferred` push
  from the incumbent re-homes the client through the existing adopt path.
- **Why optional:** a successor can be joining with no live incumbent. That is
  a forced takeover after the incumbent died. There is then no daemon to name,
  and absence means "wait for the adoption", which is today's behavior. An
  empty-string sentinel would be the forbidden null convention.
- **Why per endpoint and not a shared message:** every endpoint already owns its
  `TransferringAway { string address = 1; }` arm. The new field mirrors that
  established shape, so both handover arms read alike on every endpoint.
- **Consequences:**
  - The daemon fills the address in ONE place. `server.fill` already fills
    `transferring_away.address` from the successor address, and it now fills
    `not_yet_adopted.address` from the rollout controller's incumbent address.
    That address is the `--joining` value, and it is absent once the incumbent
    is known dead.
  - Every elisp decoder of a `NotYetAdopted` arm moves from the empty-message
    decoder to one shared decoder that reads `address`.
  - The webapp's cross-cutting refusal wording no longer draws `notYetAdopted`
    as a refusal. Each site routes it, as handover news, to the named daemon.
- **Obviated declarations:** none. The sweep found that every `NotYetAdopted`
  message is still referenced by its endpoint's error oneof.
