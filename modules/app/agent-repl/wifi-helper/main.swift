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

func authorizationName(_ status: CLAuthorizationStatus) -> String {
    switch status {
    case .authorizedAlways: return "authorized"
    case .denied: return "denied"
    case .restricted: return "restricted"
    case .notDetermined: return "not_determined"
    @unknown default: return "unknown"
    }
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

final class Authorizer: NSObject, CLLocationManagerDelegate {
    let manager = CLLocationManager()
    let out: String
    init(out: String) {
        self.out = out
        super.init()
        manager.delegate = self
    }
    func start() {
        if manager.authorizationStatus == .notDetermined {
            manager.requestAlwaysAuthorization()
            return
        }
        finish(manager.authorizationStatus)
    }
    func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) {
        if manager.authorizationStatus != .notDetermined {
            finish(manager.authorizationStatus)
        }
    }
    func finish(_ status: CLAuthorizationStatus) {
        write(scan(verb: "authorize", authorization: authorizationName(status)), to: out)
        exit(0)
    }
}

let args = CommandLine.arguments
guard args.count == 4, args[2] == "--out" else {
    FileHandle.standardError.write("usage: agent-repl-wifi scan|authorize --out FILE\n".data(using: .utf8)!)
    exit(2)
}
let verb = args[1]
let out = args[3]
switch verb {
case "scan":
    let status = CLLocationManager().authorizationStatus
    write(scan(verb: verb, authorization: authorizationName(status)), to: out)
case "authorize":
    let authorizer = Authorizer(out: out)
    authorizer.start()
    // The owner answers the prompt; the run loop waits for the delegate.
    RunLoop.main.run()
default:
    FileHandle.standardError.write("agent-repl-wifi: unknown verb \(verb)\n".data(using: .utf8)!)
    exit(2)
}
