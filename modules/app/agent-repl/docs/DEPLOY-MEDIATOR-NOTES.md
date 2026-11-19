# Deploy mediator's notes

The deploy mediator's own standing notes on log records that need judgement:
what it has learned about a record that a rule in the store cannot express.
Its brief names this file, and it is the mediator's to write.

- A kind of record that is ignorable whatever the circumstances does not belong
  here. Record it in the store, which drops it before every check:
  `agent-repld merge-queue mediation ignore -operation <operation> [-message <text>] -why "<one sentence>"`.
- A note here is for a record that is ignorable only in some circumstance, or
  whose meaning took an investigation to learn. Say the operation, the message,
  the circumstance, and why.

## Notes

- `daemon.sidebar.merge_queue`, "the deploy mediator is one the registry does
  not carry": ignorable when the user has removed the deploy mediator
  workspace. The daemon makes a new mediator at the next deploy. In any other
  circumstance it is a real fault.
- `daemon.merge.mediation`, "the intake's turn could not be awaited" with the
  session watcher closed: ignorable right after the user removed the deploy
  mediator workspace, for the same reason. Otherwise it is a real fault that
  holds the queue.
