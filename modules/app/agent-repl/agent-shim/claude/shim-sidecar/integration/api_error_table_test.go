package integration

// SUBJECT — the api_error TAXONOMY table, end to end.
//
// apiErrorKind maps the vendor's own declared error type onto ApiRequestFailed's
// closed kind set, and a type this schema does not model arrives as `unmodeled`
// CARRYING THE NAME rather than as a silently mishandled value. This is the
// table that walks that mapping through the real binary.
//
// GROUNDING LIMIT — READ BEFORE ADDING A ROW. The checked-in corpus holds
// EXACTLY ONE api_error record: a CONNECTION failure
// (`error.connection.code = "StreamSuspended"`, with no vendor `type` at all),
// which is why the one grounded row below expects the `unmodeled` arm naming
// `connection/StreamSuspended`. rate_limit_error, overloaded_error,
// authentication_error, the numeric-status-only shape and an unknown type
// string HAVE NO FIXTURE. A row for any of them would have to be a record this
// project INVENTED and called a vendor capture, which is precisely the thing the
// golden corpus exists to prevent — so they are not written here. When a capture
// run records one, drop the fixture under
// testdata/corpus/transcript-lines/ and add its row to the table; nothing else
// about this subject changes. The unit-level table in
// internal/convert/system_test.go is where a mapping is exercised without a
// capture.
