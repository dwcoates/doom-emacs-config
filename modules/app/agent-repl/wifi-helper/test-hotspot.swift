// Tests of hotspot.swift, the helper's hotspot join; bin/test-wifi-helper.sh
// compiles and runs them. sharingd and the radio are fakes.

import Foundation

// FakePhones answers a scripted browse and enable, recording each call.
final class FakePhones: HotspotPhones {
    var heard: Result<[HotspotPhone], HotspotFailure> = .success([])
    var enabled: Result<HotspotCredentials, HotspotFailure> = .failure(HotspotFailure(detail: "unscripted"))
    var asked: [String] = []
    var bounds: [TimeInterval] = []
    func phones(within: TimeInterval) -> Result<[HotspotPhone], HotspotFailure> {
        bounds.append(within)
        return heard
    }
    func enable(_ phone: HotspotPhone, within: TimeInterval) -> Result<HotspotCredentials, HotspotFailure> {
        asked.append(phone.name)
        bounds.append(within)
        return enabled
    }
}

// FakeRadio answers a scripted scan and join, recording each call.
final class FakeRadio: HotspotRadio {
    var found: Result<AnyObject, HotspotFailure> = .failure(HotspotFailure(detail: "unscripted"))
    var joined: Result<Void, HotspotFailure> = .failure(HotspotFailure(detail: "unscripted"))
    var scanned: [String] = []
    var passwords: [String?] = []
    var bounds: [TimeInterval] = []
    func find(ssid: String, within: TimeInterval) -> Result<AnyObject, HotspotFailure> {
        scanned.append(ssid)
        bounds.append(within)
        return found
    }
    func join(_ network: AnyObject, password: String?, within: TimeInterval) -> Result<Void, HotspotFailure> {
        passwords.append(password)
        bounds.append(within)
        return joined
    }
}

// FakeInfo stands in for sharingd's SFRemoteHotspotInfo, read by key.
final class FakeInfo: NSObject {
    @objc var name: String?
    @objc var password: String?
    @objc var channel: NSNumber?
    init(name: String?, password: String?, channel: NSNumber?) {
        self.name = name
        self.password = password
        self.channel = channel
    }
}

// creds names a credentials result by its parts, since Result of a struct
// holding a password is compared field by field.
func creds(_ r: Result<HotspotCredentials, HotspotFailure>) -> String {
    switch r {
    case .success(let c): return "ok \(c.ssid) \(c.password ?? "-") \(c.channel.map(String.init) ?? "-")"
    case .failure(let why): return "failed \(why.detail)"
    }
}

@main
struct HotspotTests {
    static var failures = 0

    static func check<T: Equatable>(_ name: String, _ got: T, _ want: T) {
        if got == want {
            print("ok - \(name)")
        } else {
            print("not ok - \(name): got \(got), want \(want)")
            failures += 1
        }
    }

    // ticks is an elapsed clock where every step takes MS milliseconds.
    static func ticks(_ ms: Int) -> () -> () -> Int { { { ms } } }

    static let phone = HotspotPhone(name: "Dodge\u{2019}s iPhone", handle: nil)
    static let credentials = HotspotCredentials(ssid: "Dodge\u{2019}s iPhone", password: "secret", channel: 149)

    // ready answers fakes scripted through every step to a join.
    static func ready() -> (FakePhones, FakeRadio) {
        let phones = FakePhones()
        phones.heard = .success([HotspotPhone(name: "Other iPad", handle: nil), phone])
        phones.enabled = .success(credentials)
        let radio = FakeRadio()
        radio.found = .success(NSObject())
        radio.joined = .success(())
        return (phones, radio)
    }

    static func main() {
        do {
            let (phones, radio) = ready()
            let got = joinHotspot(named: "Dodge's iPhone", phones: phones, radio: radio, elapsed: ticks(7))
            check("a join asks the phone of the configured name, either apostrophe", phones.asked, ["Dodge\u{2019}s iPhone"])
            check("a join scans for the network sharingd named", radio.scanned, ["Dodge\u{2019}s iPhone"])
            check("a join joins with the password sharingd gave", radio.passwords, ["secret"])
            check("a join answers joined with the network and every step's time", got,
                  HotspotAnswer(stage: "joined", network: "Dodge\u{2019}s iPhone", browse_ms: 7, enable_ms: 7, scan_ms: 7, join_ms: 7))
            check("each step is bounded by its own bound", phones.bounds + radio.bounds, [6, 4, 9, 20])
        }
        do {
            let (phones, radio) = ready()
            phones.heard = .failure(HotspotFailure(detail: "sharingd is not there"))
            let got = joinHotspot(named: "Dodge's iPhone", phones: phones, radio: radio, elapsed: ticks(1))
            check("a browse that fails answers browse_failed with why", got,
                  HotspotAnswer(stage: "browse_failed", error: "sharingd is not there", browse_ms: 1))
            check("a browse that fails asks no phone", phones.asked, [])
        }
        do {
            let (phones, radio) = ready()
            phones.heard = .success([HotspotPhone(name: "Other iPad", handle: nil)])
            let got = joinHotspot(named: "Dodge's iPhone", phones: phones, radio: radio, elapsed: ticks(1))
            check("no phone of the name answers no_phone naming the phones heard", got,
                  HotspotAnswer(stage: "no_phone", phones_heard: ["Other iPad"], browse_ms: 1))
            check("no phone of the name scans nothing", radio.scanned, [])
        }
        do {
            let (phones, radio) = ready()
            phones.enabled = .failure(HotspotFailure(detail: "BLE client connect timeout"))
            let got = joinHotspot(named: "Dodge's iPhone", phones: phones, radio: radio, elapsed: ticks(2))
            check("a phone that does not start its hotspot answers enable_failed with why", got,
                  HotspotAnswer(stage: "enable_failed", error: "BLE client connect timeout", browse_ms: 2, enable_ms: 2))
            check("a phone that does not start its hotspot scans nothing", radio.scanned, [])
        }
        do {
            let (phones, radio) = ready()
            radio.found = .failure(HotspotFailure(detail: "no network named Dodge\u{2019}s iPhone is in range"))
            let got = joinHotspot(named: "Dodge's iPhone", phones: phones, radio: radio, elapsed: ticks(3))
            check("a network out of sight answers not_visible with why", got,
                  HotspotAnswer(stage: "not_visible", network: "Dodge\u{2019}s iPhone", error: "no network named Dodge\u{2019}s iPhone is in range",
                                browse_ms: 3, enable_ms: 3, scan_ms: 3))
            check("a network out of sight is not joined", radio.passwords, [])
        }
        do {
            let (phones, radio) = ready()
            radio.joined = .failure(HotspotFailure(detail: "tmpErr"))
            let got = joinHotspot(named: "Dodge's iPhone", phones: phones, radio: radio, elapsed: ticks(4))
            check("a join the radio refuses answers join_failed with why", got,
                  HotspotAnswer(stage: "join_failed", network: "Dodge\u{2019}s iPhone", error: "tmpErr",
                                browse_ms: 4, enable_ms: 4, scan_ms: 4, join_ms: 4))
        }
        check("sharingd's info is the network, its password and its channel",
              creds(hotspotCredentials(info: FakeInfo(name: "Phone", password: "secret", channel: 149), error: nil)), "ok Phone secret 149")
        check("sharingd's info without a password joins without one",
              creds(hotspotCredentials(info: FakeInfo(name: "Phone", password: nil, channel: nil), error: nil)), "ok Phone - -")
        check("sharingd's error is the failure's detail",
              creds(hotspotCredentials(info: nil, error: NSError(domain: "RPErrorDomain", code: -71143,
                                                          userInfo: [NSLocalizedDescriptionKey: "kNetworkErr (Connect failed)"]))),
              "failed the phone did not start its hotspot: kNetworkErr (Connect failed)")
        check("neither info nor error is a failure saying so",
              creds(hotspotCredentials(info: nil, error: nil)), "failed the phone did not start its hotspot: no hotspot and no error")
        check("info naming no network is a failure",
              creds(hotspotCredentials(info: FakeInfo(name: "", password: "secret", channel: nil), error: nil)).hasPrefix("failed sharingd's hotspot info names no network"), true)
        check("names match across apostrophes", sameHotspotName("Dodge\u{2019}s iPhone", "Dodge's iPhone"), true)
        check("different names do not match", sameHotspotName("Dodge's iPad", "Dodge's iPhone"), false)
        check("an empty name matches nothing", sameHotspotName("", ""), false)
        check("bounded work that answers in time answers its result",
              bounded("work", within: 1) { .success(5) }, Result<Int, HotspotFailure>.success(5))
        let release = DispatchSemaphore(value: 0)
        check("bounded work past its bound answers which work and the bound",
              bounded("the scan", within: 0.01) { () -> Result<Int, HotspotFailure> in release.wait(); return .success(5) },
              .failure(HotspotFailure(detail: "the scan did not answer within 0.01s")))
        release.signal()
        check("awaited work that answers through its callback answers its result",
              awaiting("the ask", within: 1) { answer in answer(.success(7)) }, Result<Int, HotspotFailure>.success(7))
        check("awaited work whose callback never comes answers which work and the bound",
              awaiting("the ask", within: 0.01) { (_: @escaping (Result<Int, HotspotFailure>) -> Void) in },
              .failure(HotspotFailure(detail: "the ask did not answer within 0.01s")))
        check("awaited work answered twice keeps its first answer",
              awaiting("the ask", within: 1) { answer in answer(.success(1)); answer(.success(2)) }, Result<Int, HotspotFailure>.success(1))
        exit(failures == 0 ? 0 : 1)
    }
}
