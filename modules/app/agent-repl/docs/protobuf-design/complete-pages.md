# Complete pages: a page carries the prompts of the turns it touches

## Problem

The daemon withholds every entry of a turn until that turn's prompt is drawn.
A long turn's newest store page holds only its steps, so an opening read 2-12
store pages before anything drew (172 of 1,332 openings, 2026-10-10..12;
+535ms p50, +1.7s p90).

## Core design principle (owner, 2026-10-12)

A PAGE CARRIES EVERYTHING IT TOUCHES THAT IS PERSISTED IN THE STORE. The
newest page is loaded immediately and completed before the workspace is
answered open. What the rows of a page depend on to draw is their turn's
prompt (entries upsert by identity, so a row's own state is already on the
page); so every page carries the prompt line of each turn it touches whose
prompt sits below it.

## Landed changes

### `turn_heads` on every page (store.v1 AgentSessionPage and ReadAgentPageSuccess, conversation.v1 HistoryPage)
- WHAT: the prompt line of each turn a line of the page is stamped with whose
  prompt is placed below the page, newest first, each turn once.
- WHY: the principle above; one read draws the page.
- CONSEQUENCE: a head is also an ordinary line of an older page; readers
  absorb it by identity when the walk meets it.

### FeedRow `earlier_steps` (field 24)
- WHAT: drawn under a carried prompt while the steps between it and the
  page's rows are on older pages; activating it runs the older-page walk; the
  daemon removes it (`removed`) once those steps are loaded.
- WHY: the prompt and the newest steps are drawn together without implying
  nothing happened between them.
