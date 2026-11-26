// The Wi-Fi helper's hotspot join, apart from main.swift so its decisions are
// tested (test-hotspot.swift, bin/test-wifi-helper.sh) against fakes of the
// two things it drives: sharingd's Instant Hotspot (HotspotPhones) and the
// Wi-Fi radio (HotspotRadio). sharing.swift is the real pair.
//
// THE PHONE IS ASKED FIRST, THEN JOINED (owner ruling, 2026-11-26). An idle
// iPhone stops broadcasting its hotspot, so a join by name alone found
// nothing ("Could not find network"), and the password the Mac had saved for
// it in 2023 no longer joined (-3900 tmpErr). sharingd asks the phone over
// Bluetooth to start the hotspot, which it did with the lid closed and the
// screen off, and answers the hotspot's network name and current password;
// the radio then joins that network with that password. Measured 2026-11-26
// 13:04-13:07 on this host, lid closed: the ask took 1.1-1.6s in five tries,
// the scan for the name 4.1-4.2s, and a join with the password 9.0s.

import Foundation

// A phone sharingd heard that offers Instant Hotspot.
struct HotspotPhone {
    let name: String
    // handle is sharingd's own device object, handed back to enable.
    let handle: AnyObject?
}

// What sharingd answers once the phone's hotspot is on.
struct HotspotCredentials {
    let ssid: String
    let password: String?
    let channel: Int?
}

// HotspotPhones is sharingd's Instant Hotspot: the phones it hears, and an ask
// that one start its hotspot. Each call is bounded by WITHIN and answers
// .failure(why) when it cannot.
protocol HotspotPhones {
    func phones(within: TimeInterval) -> Result<[HotspotPhone], HotspotFailure>
    func enable(_ phone: HotspotPhone, within: TimeInterval) -> Result<HotspotCredentials, HotspotFailure>
}

// HotspotRadio is the Wi-Fi interface: a scan for one network by name, and a
// join of the network it found.
protocol HotspotRadio {
    func find(ssid: String, within: TimeInterval) -> Result<AnyObject, HotspotFailure>
    func join(_ network: AnyObject, password: String?, within: TimeInterval) -> Result<Void, HotspotFailure>
}

struct HotspotFailure: Error, Equatable {
    let detail: String
}

// HotspotAnswer is the hotspot verb's say, which the daemon reads
// (darwinhost.go's hotspotAnswer): the stage it ended at, the network it
// joined or tried, the failure's detail, and each step's duration.
struct HotspotAnswer: Encodable, Equatable {
    // stage is one of HotspotStage's names.
    let stage: String
    var network: String?
    var error: String?
    // phones_heard names every phone sharingd heard, when none matched.
    var phones_heard: [String]?
    var browse_ms: Int?
    var enable_ms: Int?
    var scan_ms: Int?
    var join_ms: Int?
}

// HotspotStage is where a hotspot join ended.
enum HotspotStage: String {
    // joined: the hotspot's network was joined.
    case joined
    // browse_failed: sharingd could not be asked which phones it hears.
    case browseFailed = "browse_failed"
    // no_phone: sharingd heard no phone of the hotspot's name.
    case noPhone = "no_phone"
    // enable_failed: the phone did not start its hotspot.
    case enableFailed = "enable_failed"
    // not_visible: the radio did not see the hotspot's network once it was on.
    case notVisible = "not_visible"
    // join_failed: the radio did not join it.
    case joinFailed = "join_failed"
}

// HotspotBounds bound each step: two to three times its measured time (the
// header's measurements), the browse twice the 3.0s sharingd took to hear a
// phone that had just come back in range (2026-11-26 13:01:03-13:01:06).
struct HotspotBounds {
    var browse: TimeInterval = 6
    var enable: TimeInterval = 4
    var scan: TimeInterval = 9
    var join: TimeInterval = 20
}

// sameHotspotName matches two names as macOS writes them: an iPhone names its
// hotspot with a curly apostrophe the configured name may spell straight
// (the daemon's sameNetwork).
func sameHotspotName(_ a: String, _ b: String) -> Bool {
    func plain(_ s: String) -> String { s.replacingOccurrences(of: "\u{2019}", with: "'") }
    return !a.isEmpty && plain(a) == plain(b)
}

// joinHotspot asks the phone named NAME to start its hotspot, then joins the
// network it names with the password it gives. Each step's duration is
// measured on ELAPSED, which answers the milliseconds since its own call.
func joinHotspot(named name: String, phones: HotspotPhones, radio: HotspotRadio,
                 bounds: HotspotBounds = HotspotBounds(), elapsed: () -> () -> Int) -> HotspotAnswer {
    let browse = elapsed()
    let heard = phones.phones(within: bounds.browse)
    let browseMs = browse()
    var answer: HotspotAnswer
    switch heard {
    case .failure(let why):
        return HotspotAnswer(stage: HotspotStage.browseFailed.rawValue, error: why.detail, browse_ms: browseMs)
    case .success(let list):
        guard let phone = list.first(where: { sameHotspotName($0.name, name) }) else {
            return HotspotAnswer(stage: HotspotStage.noPhone.rawValue, phones_heard: list.map(\.name), browse_ms: browseMs)
        }
        answer = HotspotAnswer(stage: "", browse_ms: browseMs)
        let enable = elapsed()
        let enabled = phones.enable(phone, within: bounds.enable)
        answer.enable_ms = enable()
        guard case .success(let credentials) = enabled else {
            if case .failure(let why) = enabled { answer.error = why.detail }
            return answer.at(.enableFailed)
        }
        answer.network = credentials.ssid
        let scan = elapsed()
        let found = radio.find(ssid: credentials.ssid, within: bounds.scan)
        answer.scan_ms = scan()
        guard case .success(let network) = found else {
            if case .failure(let why) = found { answer.error = why.detail }
            return answer.at(.notVisible)
        }
        let join = elapsed()
        let joined = radio.join(network, password: credentials.password, within: bounds.join)
        answer.join_ms = join()
        if case .failure(let why) = joined {
            answer.error = why.detail
            return answer.at(.joinFailed)
        }
        return answer.at(.joined)
    }
}

extension HotspotAnswer {
    // at answers this answer ended at STAGE.
    func at(_ stage: HotspotStage) -> HotspotAnswer {
        HotspotAnswer(stage: stage.rawValue, network: network, error: error, phones_heard: phones_heard,
                      browse_ms: browse_ms, enable_ms: enable_ms, scan_ms: scan_ms, join_ms: join_ms)
    }
}

// hotspotCredentials reads sharingd's enable answer: the hotspot's info, or why not.
func hotspotCredentials(info: AnyObject?, error: AnyObject?) -> Result<HotspotCredentials, HotspotFailure> {
    guard let info = info as? NSObject else {
        let why = (error as? NSError)?.localizedDescription ?? error.map { "\($0)" } ?? "no hotspot and no error"
        return .failure(HotspotFailure(detail: "the phone did not start its hotspot: \(why)"))
    }
    guard let ssid = info.value(forKey: "name") as? String, !ssid.isEmpty else {
        return .failure(HotspotFailure(detail: "sharingd's hotspot info names no network: \(info)"))
    }
    return .success(HotspotCredentials(ssid: ssid, password: info.value(forKey: "password") as? String,
                                       channel: (info.value(forKey: "channel") as? NSNumber)?.intValue))
}

// bounded runs WORK off the calling thread and answers its result, or
// .failure naming WHAT and the bound when it ran past WITHIN. A late result
// is dropped: the helper exits once it answers, ending the work with it.
func bounded<T>(_ what: String, within: TimeInterval, _ work: @escaping () -> Result<T, HotspotFailure>) -> Result<T, HotspotFailure> {
    let done = DispatchSemaphore(value: 0)
    let lock = NSLock()
    var result: Result<T, HotspotFailure>?
    DispatchQueue.global().async {
        let r = work()
        lock.lock()
        result = r
        lock.unlock()
        done.signal()
    }
    guard done.wait(timeout: .now() + within) == .success else {
        return .failure(HotspotFailure(detail: "\(what) did not answer within \(within)s"))
    }
    lock.lock()
    defer { lock.unlock() }
    return result!
}
