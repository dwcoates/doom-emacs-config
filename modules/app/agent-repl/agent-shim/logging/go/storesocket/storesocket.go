// Package storesocket names the store's read-only socket from its producers'
// socket: the one rule the store (which binds it) and the daemon (which dials
// it) both resolve it by, so the two can never disagree about where it is.
//
// THE READ-ONLY SOCKET SITS BESIDE THE PRODUCERS' SOCKET, never at a fixed
// path. Everything that points a store at a private socket (a test harness, a
// second checkout) then gives its read-only socket a private path too, with no
// second setting to forget: a fixed default would have every private store
// contend for the live store's read-only socket.
package storesocket

import "strings"

// ReadPath is the store's read-only socket (storeread.v1.StoreRead) for the
// store serving its producers on STORESOCKET: the same directory, the same
// name with "-read" before its ".sock" ("store.sock" is "store-read.sock").
// A name without the suffix gains "-read.sock".
func ReadPath(storeSocket string) string {
	return strings.TrimSuffix(storeSocket, ".sock") + "-read.sock"
}
