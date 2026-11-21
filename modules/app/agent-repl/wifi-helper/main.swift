// agent-repl's Wi-Fi helper (owner ruling, 2026-11-14): the daemon's
// persistent-wifi failover joins only networks macOS knows that are visible
// now, then the hotspot, and macOS names Wi-Fi networks only to an app the
// owner allowed Location Services. This app is that app: it reads the
// joined network and the visible ones through CoreWLAN and writes them as
// one JSON document; joining stays the daemon's (`networksetup`).
//
// IT RUNS AS ITS OWN APP, launched with `open`, so macOS attributes the
// Location Services permission to this bundle and not to the daemon that
// asked. Its answer goes to the file `--out` names, since `open` hands back
// no stdout.
//
// Verbs:
//   scan --out FILE        the joined network and every visible one
//   authorize --out FILE   ask for Location Services (the one prompt), and
//                          write the answer once the owner gave it
//
// Every answer names the permission's standing, so the daemon can say why a
// scan carries no names: one of authorizationName's (standing.swift), or
// awaiting_owner, a scan launched while an ask waits on the owner's answer.
//
// AN INSTANCE IS A LOCATION SERVICES CLIENT ONLY FROM ITS SEAT (seats.swift):
// locationd serves one instance of the bundle at a time.

import CoreLocation
import CoreWLAN
import Foundation

struct Visible: Encodable {
    let ssid: String
    let rssi: Int
    // channel is the strongest sighting's channel number; nil when CoreWLAN
    // named none.
    let channel: Int?
}

struct Answer: Encodable {
    let verb: String
    let authorization: String
    let interface: String?
    let current: String?
    let visible: [Visible]
    let error: String?
    // standing_ms is how long CoreLocation took to tell the standing, from
    // the manager's creation; nil when it was never heard. It measures
    // standingBound under the host's real load.
    var standing_ms: Int? = standingMs
}

// standingMs is the time the listener heard the standing in, once it has.
var standingMs: Int?

func write(_ answer: Answer, to path: String) {
    let encoder = JSONEncoder()
    encoder.outputFormatting = [.sortedKeys]
    do {
        let data = try encoder.encode(answer)
        // ATOMIC, so the daemon never reads half an answer.
        try data.write(to: URL(fileURLWithPath: path), options: .atomic)
    } catch {
        FileHandle.standardError.write("agent-repl-wifi: could not write \(path): \(error)\n".data(using: .utf8)!)
        exit(3)
    }
}

func scan(verb: String, authorization: String) -> Answer {
    guard let iface = CWWiFiClient.shared().interface() else {
        return Answer(verb: verb, authorization: authorization, interface: nil, current: nil, visible: [], error: "no Wi-Fi interface")
    }
    var visible: [Visible] = []
    var scanError: String?
    do {
        // THE STRONGEST FIRST, one entry per name: a network seen on two
        // bands is one network to join, answered with its strongest
        // sighting's channel.
        var best: [String: Visible] = [:]
        for network in try iface.scanForNetworks(withName: nil) {
            guard let ssid = network.ssid, !ssid.isEmpty else { continue }
            if let seen = best[ssid], seen.rssi >= network.rssiValue { continue }
            best[ssid] = Visible(ssid: ssid, rssi: network.rssiValue, channel: network.wlanChannel?.channelNumber)
        }
        visible = best.values.sorted { $0.rssi > $1.rssi }
    } catch {
        scanError = "the scan failed: \(error.localizedDescription)"
    }
    return Answer(verb: verb, authorization: authorization, interface: iface.interfaceName, current: iface.ssid(), visible: visible, error: scanError)
}

// Listener hears the helper's standing (standing.swift) and does VERB's step:
// it answers the first standing a scan hears, and for authorize asks first
// when the owner never answered, then answers the owner's answer.
final class Listener: NSObject, CLLocationManagerDelegate {
    let manager = CLLocationManager()
    let verb: String
    let out: String
    var requested = false
    var heard = false
    let created = DispatchTime.now()
    init(verb: String, out: String) {
        self.verb = verb
        self.out = out
        super.init()
        manager.delegate = self
    }
    func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) {
        if !heard {
            standingMs = Int((DispatchTime.now().uptimeNanoseconds - created.uptimeNanoseconds) / 1_000_000)
        }
        heard = true
        let status = manager.authorizationStatus
        switch nextStep(verb: verb, status: status, requested: requested) {
        case .request:
            requested = true
            manager.requestAlwaysAuthorization()
        case .wait:
            break
        case .answer:
            write(scan(verb: verb, authorization: authorizationName(status)), to: out)
            exit(0)
        }
    }
}

// standingBound bounds hearing the standing, CoreLocation's call on the
// manager's creation; the owner's answer to the prompt is the owner's own and
// unbounded. Measured 16ms at worst over five launches (2026-11-15, the helper's own launch-to-call
// time on this host, lid open); the bound is 2.5 times that.
let standingBound: TimeInterval = 0.04

// seatBound bounds a scan's wait for the client seat: one scan ahead of it,
// its standing heard and its CoreWLAN scan run. Measured 3.6s at worst over
// five scans (2026-11-16, the helper's scan verb run directly on this host,
// lid open, the first one cold); the bound is 2.5 times that. An ask waits
// unbounded: what it waits on is the owner's answer to an ask ahead of it.
let seatBound: TimeInterval = 9

// Once decides who ends a bounded wait: the waiter, or its bound.
final class Once {
    private let lock = NSLock()
    private var claimed = false
    func claim() -> Bool {
        lock.lock()
        defer { lock.unlock() }
        if claimed { return false }
        claimed = true
        return true
    }
}

// fail answers ERROR for VERB in OUT, naming no standing, and exits with CODE.
func fail(verb: String, out: String, _ error: String, code: Int32) -> Never {
    write(Answer(verb: verb, authorization: "unknown", interface: nil, current: nil, visible: [], error: error), to: out)
    exit(code)
}

let args = CommandLine.arguments
guard args.count == 4, args[2] == "--out", ["scan", "authorize"].contains(args[1]) else {
    FileHandle.standardError.write("usage: agent-repl-wifi scan|authorize --out FILE\n".data(using: .utf8)!)
    exit(2)
}
let verb = args[1]
let out = args[3]
// THE HELPER RUNS AS ITS APP (`open`), so it always has its bundle's identity,
// which is what locationd serves one client of.
guard let bundle = Bundle.main.bundleIdentifier else {
    fail(verb: verb, out: out, "the helper ran outside its app bundle, so it has no bundle to take a seat for", code: 2)
}
do {
    let seats = try openSeats(dir: NSTemporaryDirectory(), bundle: bundle)
    if verb == "authorize" {
        try takeAskSeat(seats)
    } else {
        let once = Once()
        DispatchQueue.global().asyncAfter(deadline: .now() + seatBound) {
            if once.claim() {
                fail(verb: verb, out: out, "another scan held Location Services for over \(seatBound)s", code: 4)
            }
        }
        let seat = try takeScanSeat(seats)
        guard once.claim() else {
            // The bound answered and is exiting the process.
            while true { pause() }
        }
        if seat == .askWaits {
            write(Answer(verb: verb, authorization: "awaiting_owner", interface: nil, current: nil, visible: [], error: nil), to: out)
            exit(0)
        }
    }
} catch {
    fail(verb: verb, out: out, "its Location Services seat could not be taken: \(error)", code: 3)
}
let listener = Listener(verb: verb, out: out)
DispatchQueue.main.asyncAfter(deadline: .now() + standingBound) {
    if !listener.heard {
        fail(verb: verb, out: out, "Location Services did not tell the standing within \(standingBound)s", code: 4)
    }
}
// CoreLocation calls the listener on this run loop.
RunLoop.main.run()
