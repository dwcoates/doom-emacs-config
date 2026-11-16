// The Wi-Fi helper's Location Services seats, apart from main.swift so they
// are tested (test-seats.swift, bin/test-wifi-helper.sh).
//
// ONE CLIENT PER BUNDLE (2026-11-16): locationd lets one instance of a
// bundle be its client at a time, and refuses every other ("another valid
// client with the same bundle id is connected"), never calling the refused
// one's delegate. An authorize ask left waiting on the owner's answer held
// the bundle, and every scan launched meanwhile timed out unheard. So no
// instance becomes a client unless it holds the CLIENT seat, an flock the
// kernel releases when its holder exits:
//
//   ask     the ASK seat exclusive, then the client seat: one ask at a time,
//           and none while a scan is seated
//   scan    the ask seat SHARED, never waiting for it: while an ask holds it,
//           the standing is the owner's to give and the scan answers that at
//           once (awaiting_owner); else the client seat, waiting out a scan
//           ahead of it
//
// A scan holds the ask seat shared before it takes the client seat, so an
// ask that holds the ask seat finds the client seat free.

import Foundation

// Seats are the two lock files of one bundle's seats, open.
struct Seats {
    let ask: Int32
    let client: Int32
}

// ScanSeat is what a scan found.
enum ScanSeat: Equatable {
    // seated: the scan is the bundle's client.
    case seated
    // askWaits: an ask holds the bundle, waiting on the owner's answer.
    case askWaits
}

// SeatError is a seat that could not be opened or taken.
struct SeatError: Error, CustomStringConvertible {
    let description: String
}

// openSeats opens BUNDLE's two seat files in DIR, made if absent.
func openSeats(dir: String, bundle: String) throws -> Seats {
    func open(_ name: String) throws -> Int32 {
        let path = (dir as NSString).appendingPathComponent("\(bundle).\(name).lock")
        let fd = Darwin.open(path, O_RDWR | O_CREAT | O_CLOEXEC, 0o600)
        guard fd >= 0 else {
            throw SeatError(description: "could not open the \(name) seat \(path): \(String(cString: strerror(errno)))")
        }
        return fd
    }
    return Seats(ask: try open("ask"), client: try open("client"))
}

// lock takes OPERATION on FD, retrying only an interrupted call.
private func lock(_ fd: Int32, _ operation: Int32, _ seat: String) throws -> Bool {
    while true {
        if flock(fd, operation) == 0 { return true }
        if errno == EINTR { continue }
        if errno == EWOULDBLOCK && operation & LOCK_NB != 0 { return false }
        throw SeatError(description: "could not take the \(seat) seat: \(String(cString: strerror(errno)))")
    }
}

// takeScanSeat seats a scan, waiting out a scan ahead of it; it never waits
// on an ask.
func takeScanSeat(_ seats: Seats) throws -> ScanSeat {
    guard try lock(seats.ask, LOCK_SH | LOCK_NB, "ask") else { return .askWaits }
    _ = try lock(seats.client, LOCK_EX, "client")
    return .seated
}

// takeAskSeat seats an ask, waiting out every scan seated and any ask ahead
// of it, which waits on the owner.
func takeAskSeat(_ seats: Seats) throws {
    _ = try lock(seats.ask, LOCK_EX, "ask")
    _ = try lock(seats.client, LOCK_EX, "client")
}
