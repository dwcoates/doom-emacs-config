package integration

// SUBJECT — a TRANSCRIPT that shrinks under the reader.
//
// The spool half of this is covered (TestATruncatedSpoolIsReReadFromItsNewStart)
// and the two files are read by DIFFERENT codecs: a spool is raw bytes and a
// transcript is JSONL, so a reset that works for one proves nothing about the
// other. A transcript that reset in memory but never re-offered the lower cursor
// would leave the store believing it had read records that no longer exist, and
// a restart would resume past the whole of the rewritten conversation.
