// Tests of standing.swift, the helper's decisions on hearing its Location
// Services standing; bin/test-wifi-helper.sh compiles and runs them.

import CoreLocation

@main
struct StandingTests {
    static var failures = 0

    static func check<T: Equatable>(_ name: String, _ got: T, _ want: T) {
        if got == want {
            print("ok - \(name)")
        } else {
            print("not ok - \(name): got \(got), want \(want)")
            failures += 1
        }
    }

    static func main() {
        // A scan answers the first standing it hears, whatever it is.
        check("a scan answers an authorized standing",
              nextStep(verb: "scan", status: .authorizedAlways, requested: false), .answer)
        check("a scan answers a standing the owner never gave",
              nextStep(verb: "scan", status: .notDetermined, requested: false), .answer)
        check("a scan answers a denied standing",
              nextStep(verb: "scan", status: .denied, requested: false), .answer)
        // Authorize asks once, waits for the owner, then answers.
        check("authorize asks while the owner never answered",
              nextStep(verb: "authorize", status: .notDetermined, requested: false), .request)
        check("authorize waits once it asked",
              nextStep(verb: "authorize", status: .notDetermined, requested: true), .wait)
        check("authorize answers the owner's allowing",
              nextStep(verb: "authorize", status: .authorizedAlways, requested: true), .answer)
        check("authorize answers a standing the owner gave before",
              nextStep(verb: "authorize", status: .denied, requested: false), .answer)
        // Each standing's name, as the daemon reads it (scan.go's wifiScan).
        check("authorized is named", authorizationName(.authorizedAlways), "authorized")
        check("denied is named", authorizationName(.denied), "denied")
        check("restricted is named", authorizationName(.restricted), "restricted")
        check("not determined is named", authorizationName(.notDetermined), "not_determined")
        exit(failures == 0 ? 0 : 1)
    }
}
