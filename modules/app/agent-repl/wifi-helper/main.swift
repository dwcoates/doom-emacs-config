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
// scan carries no names.

import CoreLocation
import CoreWLAN
import Foundation

struct Visible: Encodable {
    let ssid: String
    let rssi: Int
}

struct Answer: Encodable {
    let verb: String
    let authorization: String
    let interface: String?
    let current: String?
    let visible: [Visible]
    let error: String?
}

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
        // bands is one network to join.
        var best: [String: Int] = [:]
        for network in try iface.scanForNetworks(withName: nil) {
            guard let ssid = network.ssid, !ssid.isEmpty else { continue }
            best[ssid] = max(best[ssid] ?? Int.min, network.rssiValue)
        }
        visible = best.map { Visible(ssid: $0.key, rssi: $0.value) }.sorted { $0.rssi > $1.rssi }
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
    init(verb: String, out: String) {
        self.verb = verb
        self.out = out
        super.init()
        manager.delegate = self
    }
    func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) {
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

let args = CommandLine.arguments
guard args.count == 4, args[2] == "--out", ["scan", "authorize"].contains(args[1]) else {
    FileHandle.standardError.write("usage: agent-repl-wifi scan|authorize --out FILE\n".data(using: .utf8)!)
    exit(2)
}
let listener = Listener(verb: args[1], out: args[3])
DispatchQueue.main.asyncAfter(deadline: .now() + standingBound) {
    if !listener.heard {
        write(Answer(verb: listener.verb, authorization: "unknown", interface: nil, current: nil, visible: [],
                     error: "Location Services did not tell the standing within \(standingBound)s"), to: listener.out)
        exit(4)
    }
}
// CoreLocation calls the listener on this run loop.
RunLoop.main.run()
