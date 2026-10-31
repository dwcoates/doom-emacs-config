# A merge queue subscription ends when its queue depletes

A workspace subscribed to a merge queue (`agentrepl.v1.SubscribeMergeQueue`)
was woken by every later change until it unsubscribed or closed. Each notice
costs the subscribing session a full read of its context, so a subscription
forgotten after the work it watched had landed was expensive. The owner asked
that a subscription end on its own when its queue depletes, with a final
notice saying so.

Design mode: autonomous; the change was pre-approved by the owner as a
comment-only contract change.

## Landed changes

### The subscription's documented lifetime gains the depletion (2026-10-31)

- WHAT: comments only, in `proto/src/agentrepl/v1/endpoint_subscribe_merge_queue.proto`:
  the file's LIFETIME paragraph, `SubscribeMergeQueueDepletes` and
  `SubscribeMergeQueueDeploys`. No message, field or arm changed, so the wire
  is unchanged and the regenerated bindings differ only in comments.
- WHY: a subscription must never outlive the work it watched, since every
  notice wakes the subscriber at the cost of a full context read.
- ASSUMPTION (recorded, not asked): "depleted" is the existing depletion edge
  the orchestrator already reports (`merge.QueueChange.Depleted`: a merge left
  and no merge is in line). A merge remediating out of line does not hold the
  queue open, exactly as it never did for the `depletes` detail.
- ASSUMPTION: an every-repository subscription ends at the first depletion of
  any queue, because each queue it watches is "the queue it watches" and the
  depletion edge carries only that queue's snapshot. Waiting for every queue
  to be empty would need a cross-queue read on the edge and was not asked for.
- CONSEQUENCES, accepted:
  - A `deploys` subscriber is not told a deploy that ends after the depletion
    that ended it; it subscribes again after the last notice to hear it.
  - The ending is enforced by the notifier
    (`daemon/internal/mergenotice/mergenotice.go`, `endDepleted`) before the
    last notice is submitted, through `wsm` `EndMergeQueueSubscription`, which
    deletes only the subscription as read (same detail and `subscribed_at`), so
    a subscription taken again after the read stands.
  - A subscription taken after a depletion happened but before the notifier
    delivered it is read at delivery time, as every notice already is, so it
    is ended with that depletion's notice.
