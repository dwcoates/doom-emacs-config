// Tests of sharing.swift's SharingPhones against a stand-in for sharingd's
// SFRemoteHotspotSession; bin/test-wifi-helper.sh compiles and runs them with
// hotspot.swift. CoreWLANRadio drives the real radio and is not run here.

import Foundation

final class FakeDevice: NSObject {
    @objc let deviceName: String?
    init(_ name: String?) { deviceName = name }
}

final class FakeHotspotInfo: NSObject {
    @objc var name: String? = "Dodge\u{2019}s iPhone"
    @objc var password: String? = "secret"
    @objc var channel: NSNumber? = 149
}

// FakeSession answers as sharingd does: the devices it hears on browsing,
// and an enable whose block it KEEPS and calls later from its own queue.
final class FakeSession: NSObject {
    weak var delegate: AnyObject?
    var devices: [AnyObject] = []
    var info: AnyObject? = FakeHotspotInfo()
    var error: AnyObject?
    var answers = true
    var kept: [(AnyObject?, AnyObject?) -> Void] = []
    var browsing: [String] = []
    @objc func setDelegate(_ delegate: AnyObject?) { self.delegate = delegate }
    @objc func startBrowsing() {
        browsing.append("start")
        _ = (delegate as? NSObject)?.perform(NSSelectorFromString("session:updatedFoundDevices:"), with: self, with: devices as NSArray)
    }
    @objc func stopBrowsing() { browsing.append("stop") }
    @objc func enableRemoteHotspotForDevice(_ device: AnyObject, withCompletionHandler handler: @escaping (AnyObject?, AnyObject?) -> Void) {
        kept.append(handler)
        guard answers else { return }
        let (info, error) = (self.info, self.error)
        DispatchQueue.global().async { handler(info, error) }
    }
}

@main
struct SharingTests {
    static var failures = 0

    static func check<T: Equatable>(_ name: String, _ got: T, _ want: T) {
        if got == want {
            print("ok - \(name)")
        } else {
            print("not ok - \(name): got \(got), want \(want)")
            failures += 1
        }
    }

    static func describe(_ r: Result<HotspotCredentials, HotspotFailure>) -> String {
        switch r {
        case .success(let c): return "ok \(c.ssid) \(c.password ?? "-") \(c.channel.map(String.init) ?? "-")"
        case .failure(let why): return "failed \(why.detail)"
        }
    }

    static func main() {
        do {
            let fake = FakeSession()
            fake.devices = [FakeDevice("An iPad"), FakeDevice("Dodge\u{2019}s iPhone")]
            let phones = SharingPhones(session: fake)
            let heard = phones.phones(within: 1)
            check("the phones sharingd hears are named", (try? heard.get())?.map(\.name) ?? [], ["An iPad", "Dodge\u{2019}s iPhone"])
            check("a browse starts and stops browsing", fake.browsing, ["start", "stop"])
            check("the session's delegate is the phones", fake.delegate === phones, true)
        }
        do {
            let fake = FakeSession()
            fake.devices = [FakeDevice(nil)]
            let heard = SharingPhones(session: fake).phones(within: 1)
            check("a device with no name is a failure", (try? heard.get()) == nil, true)
        }
        do {
            let fake = FakeSession()
            let phones = SharingPhones(session: fake)
            let got = phones.enable(HotspotPhone(name: "Dodge\u{2019}s iPhone", handle: FakeDevice("x")), within: 1)
            check("an enable sharingd keeps the block of, then answers, answers the hotspot",
                  describe(got), "ok Dodge\u{2019}s iPhone secret 149")
            check("sharingd kept the enable's block", fake.kept.count, 1)
        }
        do {
            let fake = FakeSession()
            fake.info = nil
            fake.error = NSError(domain: "RPErrorDomain", code: -71143, userInfo: [NSLocalizedDescriptionKey: "kNetworkErr (Connect failed)"])
            let got = SharingPhones(session: fake).enable(HotspotPhone(name: "P", handle: FakeDevice("x")), within: 1)
            check("an enable sharingd refuses names its error", describe(got),
                  "failed the phone did not start its hotspot: kNetworkErr (Connect failed)")
        }
        do {
            let fake = FakeSession()
            fake.answers = false
            let got = SharingPhones(session: fake).enable(HotspotPhone(name: "P", handle: FakeDevice("x")), within: 0.01)
            check("an enable sharingd never answers names the bound", describe(got),
                  "failed sharingd's ask to enable P's hotspot did not answer within 0.01s")
        }
        do {
            let got = SharingPhones(session: FakeSession()).enable(HotspotPhone(name: "P", handle: nil), within: 1)
            check("a phone with no sharingd device is a failure", describe(got),
                  "failed the phone P carries no sharingd device to enable")
        }
        exit(failures == 0 ? 0 : 1)
    }
}
