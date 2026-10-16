package integration

// SUBJECT — `/clear`, end to end, and the prose that merely mentions it.
//
// THE LITERAL "/clear" NEVER APPEARS ON DISK. The harness expands a slash
// command into an envelope — `<command-name>/clear</command-name>` plus a
// message and an args element — and the reader detects a clear by UNWRAPPING
// that envelope and finding nothing else left. So the two halves of the rule
// are one subject: the real envelope produces a cleared cut, and a prompt whose
// text merely quotes the command produces none.
//
// THE FIXTURE IS A REAL CAPTURE. testdata/corpus/transcript-lines/user-clear-command.jsonl
// is the verbatim line the vendor wrote for a genuine `/clear` (see the corpus
// MANIFEST for its provenance), so the whitespace and element order this
// detection walks are the vendor's own rather than a reconstruction.
//
// WHAT THE CAPTURE ALSO SHOWS, and what nothing here may assume: the OLD
// transcript simply STOPS. There is no closing record of any kind; the session
// rotates by a NEW transcript file appearing under a new id. The cut record
// itself lands in a transcript like any other line, which is why one growing
// file is the whole arrangement below.
