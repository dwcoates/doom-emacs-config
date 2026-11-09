package wsm

// parkedTerminalDDL is the layout-107 step (owner ruling, 2026-11-09): the two
// park terminals become THE ONE, TerminalParked. A session the idle sweep
// stood down ("hibernated") and one the keep-alive cycle stood down
// ("keepalive_cycle") are both a session stood down while idle, and every
// reader now asks only Session.Parked.
//
// NOTHING IS THROWN AWAY. Only the kind is rewritten: the terminal's detail,
// which already names what stood the session down, and its instant stand as
// they were, so the trigger stays on the row as evidence.
//
// IT IS BREAKING: the previous build reads only the two old kinds as a park,
// so a row this step rewrites would read to it as a death. It must be gone
// before the step runs.
const parkedTerminalDDL = `
UPDATE sessions SET terminal_kind = 'parked' WHERE terminal_kind IN ('hibernated', 'keepalive_cycle');
`
