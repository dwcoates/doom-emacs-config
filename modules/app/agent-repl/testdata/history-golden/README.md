# Stored line to history entry: the shared golden cases

Each `*.json` file is one stored line (`store.v1.StoreLineAt`, as protojson)
and the history entry it renders as (`conversation.v1.HistoryEntryAt`). Two
copies of that mapping exist: the store's read-only face renders pages in Go
(`agent-shim/shim-store/internal/history`), and the shim's live tail renders
lines in TypeScript (`agent-shim/claude/shim/src/store/reader.ts`,
`toHistoryEntryAt`). Both test suites read every case here, so a change to one
mapping that the other does not make fails a test.
