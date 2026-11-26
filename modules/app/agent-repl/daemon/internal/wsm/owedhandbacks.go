package wsm

// This file is the durable half of HANDING A LANDING BACK ONLY ONCE IT IS
// DEPLOYED: the report an async merge owes its requester when it landed in the
// daemon's own checkout, held until the build that carries it is running.
//
// THE ROW IS THE DEBT, AND DELETING IT IS THE PAYMENT. The merge owes the
// report at its landing, BEFORE it tells the deploy, so every deploy that
// starts after the landing finds the row. The deploy COVERS every row present
// when it commits to building the checkout (CoverOwedHandBacks), because what
// it builds carries every landing before that instant. Whoever finishes that
// deploy -- the daemon that stays, or the successor it hands over to -- pays
// the covered rows. A row a deploy never covered waits for the next deploy.
//
// THE PAYMENT IS EXACTLY ONCE. Each row carries the idempotency key minted
// when it was owed, so a payer that submitted the report and died before
// deleting the row submits the same key again, which the prompt handler
// answers with the turn it already made.

// owedHandBacksDDL adds owed_handbacks: one row per landed async merge whose
// report waits for its deploy.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const owedHandBacksDDL = `
CREATE TABLE owed_handbacks (
  merge_workspace TEXT PRIMARY KEY,
  requester       TEXT NOT NULL,
  merge_name      TEXT NOT NULL,
  branch          TEXT NOT NULL,
  summary         TEXT NOT NULL,
  compact_after   INTEGER NOT NULL,
  idempotency_key TEXT NOT NULL,
  owed_at         INTEGER NOT NULL,
  covered         INTEGER NOT NULL DEFAULT 0
);
`
