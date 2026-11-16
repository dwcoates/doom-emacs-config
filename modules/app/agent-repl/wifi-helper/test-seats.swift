// Tests of seats.swift, the helper's Location Services seats;
// bin/test-wifi-helper.sh compiles and runs them. Each case opens its seats
// in a fresh directory, and a second openSeats of the same files is a
// second instance: flock conflicts between two opens even in one process.

import Foundation

@main
struct SeatsTests {
    static var failures = 0

    static func check<T: Equatable>(_ name: String, _ got: T, _ want: T) {
        if got == want {
            print("ok - \(name)")
        } else {
            print("not ok - \(name): got \(got), want \(want)")
            failures += 1
        }
    }

    // fresh answers a new seat directory.
    static func fresh() -> String {
        let dir = (NSTemporaryDirectory() as NSString).appendingPathComponent("seats-\(UUID().uuidString)")
        try! FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
        return dir
    }

    // free reports whether another instance could take FD's lock as OPERATION now.
    static func free(_ fd: Int32, _ operation: Int32) -> Bool {
        flock(fd, operation | LOCK_NB) == 0
    }

    static func main() throws {
        do {
            // A scan with nothing seated takes the client seat.
            let dir = fresh()
            let scan = try openSeats(dir: dir, bundle: "b")
            let other = try openSeats(dir: dir, bundle: "b")
            check("a scan with nothing seated is seated", try takeScanSeat(scan), .seated)
            check("a seated scan holds the client seat", free(other.client, LOCK_EX), false)
            check("a seated scan keeps an ask out", free(other.ask, LOCK_EX), false)
            check("a seated scan lets another scan past the ask seat", free(other.ask, LOCK_SH), true)
        }
        do {
            // A scan while an ask is seated answers at once.
            let dir = fresh()
            let ask = try openSeats(dir: dir, bundle: "b")
            let scan = try openSeats(dir: dir, bundle: "b")
            try takeAskSeat(ask)
            check("a scan while an ask is seated answers that the owner is asked", try takeScanSeat(scan), .askWaits)
            close(ask.ask)
            close(ask.client)
            let third = try openSeats(dir: dir, bundle: "b")
            check("the scan that found an ask holds no ask seat", free(third.ask, LOCK_EX), true)
            check("the scan that found an ask holds no client seat", free(third.client, LOCK_EX), true)
        }
        do {
            // An ask holds both seats.
            let dir = fresh()
            let ask = try openSeats(dir: dir, bundle: "b")
            let other = try openSeats(dir: dir, bundle: "b")
            try takeAskSeat(ask)
            check("a seated ask holds the ask seat", free(other.ask, LOCK_SH), false)
            check("a seated ask holds the client seat", free(other.client, LOCK_EX), false)
        }
        do {
            // Seats are per bundle.
            let dir = fresh()
            let one = try openSeats(dir: dir, bundle: "one")
            let two = try openSeats(dir: dir, bundle: "two")
            try takeAskSeat(one)
            check("another bundle's ask leaves a scan seated", try takeScanSeat(two), .seated)
        }
        do {
            // A seat directory that cannot hold the files is an error naming it.
            let missing = (fresh() as NSString).appendingPathComponent("absent")
            _ = try openSeats(dir: missing, bundle: "b")
            check("a seat that cannot be opened is an error", "opened", "an error")
        } catch let error as SeatError {
            check("a seat that cannot be opened names its file",
                  error.description.contains("absent/b.ask.lock"), true)
        } catch {
            check("a seat that cannot be opened is a SeatError", "\(error)", "SeatError")
        }
        exit(failures == 0 ? 0 : 1)
    }
}
