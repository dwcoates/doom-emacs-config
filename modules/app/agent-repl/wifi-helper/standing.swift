// The Wi-Fi helper's Location Services standing, apart from main.swift so
// its decisions are tested (test-standing.swift, bin/test-wifi-helper.sh).
//
// THE STANDING IS HEARD, NEVER READ AT ONCE (2026-11-15): a fresh
// CLLocationManager answers .notDetermined until it has loaded the app's
// standing, and tells its delegate the real one through
// locationManagerDidChangeAuthorization, which it calls once on creation. A
// scan that read the standing at once named every network yet reported
// not_determined while Location Services allowed the helper.

import CoreLocation

func authorizationName(_ status: CLAuthorizationStatus) -> String {
    switch status {
    case .authorizedAlways: return "authorized"
    case .denied: return "denied"
    case .restricted: return "restricted"
    case .notDetermined: return "not_determined"
    @unknown default: return "unknown"
    }
}

// Step is what the helper does on hearing its standing.
enum Step: Equatable {
    // request: ask the owner for Location Services (the one prompt).
    case request
    // wait: the owner has not answered the prompt yet.
    case wait
    // answer: scan, and write the answer naming the standing.
    case answer
}

// nextStep is VERB's step on hearing STATUS, REQUESTED once the prompt was
// asked: only authorize asks, and only while the owner never answered.
func nextStep(verb: String, status: CLAuthorizationStatus, requested: Bool) -> Step {
    guard verb == "authorize", status == .notDetermined else { return .answer }
    return requested ? .wait : .request
}
