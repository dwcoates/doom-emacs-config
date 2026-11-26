// The real pair hotspot.swift's join drives: sharingd's Instant Hotspot
// (SharingPhones) and the Wi-Fi radio through CoreWLAN (CoreWLANRadio).
//
// SHARINGD'S INSTANT HOTSPOT IS A PRIVATE API (Sharing.framework's
// SFRemoteHotspotSession, the one the Wi-Fi menu uses), reached through the
// Objective-C runtime; a macOS update can rename or remove it. Every piece it
// needs is checked before use, and a missing one answers a failure naming
// it, so a broken API is recorded loudly by the daemon (owner ruling,
// 2026-11-26) and never worked around. No entitlement is needed: an
// unsigned probe drove it on 2026-11-26.

import CoreWLAN
import Foundation

let sharingFramework = "/System/Library/PrivateFrameworks/Sharing.framework/Sharing"

// SharingPhones is one SFRemoteHotspotSession: sharingd tells it the phones it
// hears (session:updatedFoundDevices:), and enables a phone's hotspot.
final class SharingPhones: NSObject, HotspotPhones {
    private let session: NSObject
    private let lock = NSLock()
    private var heard: [AnyObject] = []
    private let heardPhone = DispatchSemaphore(value: 0)

    // init drives SESSION, sharingd's SFRemoteHotspotSession (connect) or a
    // test's stand-in for it.
    init(session: NSObject) {
        self.session = session
        super.init()
        session.perform(NSSelectorFromString("setDelegate:"), with: self)
    }

    // connect builds the session, or answers which piece of the private API is
    // missing.
    static func connect() -> Result<SharingPhones, HotspotFailure> {
        guard dlopen(sharingFramework, RTLD_NOW) != nil else {
            return .failure(HotspotFailure(detail: "Sharing.framework could not be loaded: \(String(cString: dlerror()))"))
        }
        guard let type = NSClassFromString("SFRemoteHotspotSession") as? NSObject.Type else {
            return .failure(HotspotFailure(detail: "Sharing.framework has no SFRemoteHotspotSession"))
        }
        let session = type.init()
        for selector in ["setDelegate:", "startBrowsing", "stopBrowsing", "enableRemoteHotspotForDevice:withCompletionHandler:"] {
            guard session.responds(to: NSSelectorFromString(selector)) else {
                return .failure(HotspotFailure(detail: "SFRemoteHotspotSession does not answer \(selector)"))
            }
        }
        return .success(SharingPhones(session: session))
    }

    // session:updatedFoundDevices: is sharingd's word on the phones it hears,
    // on its own queue: every phone it still hears, as an array or keyed by
    // identifier.
    @objc func session(_ session: AnyObject, updatedFoundDevices devices: AnyObject) {
        var list: [AnyObject] = []
        if let array = devices as? [AnyObject] {
            list = array
        } else if let byID = devices as? [AnyHashable: AnyObject] {
            list = Array(byID.values)
        }
        lock.lock()
        heard = list
        lock.unlock()
        if !list.isEmpty {
            heardPhone.signal()
        }
    }

    func phones(within: TimeInterval) -> Result<[HotspotPhone], HotspotFailure> {
        session.perform(NSSelectorFromString("startBrowsing"))
        defer { session.perform(NSSelectorFromString("stopBrowsing")) }
        // A phone in range is heard at once, or within 3.0s of coming back
        // into range (hotspot.swift's measurements); a bound passed with none
        // heard is no phone.
        _ = heardPhone.wait(timeout: .now() + within)
        lock.lock()
        let devices = heard
        lock.unlock()
        var phones: [HotspotPhone] = []
        for device in devices {
            guard let name = (device as? NSObject)?.value(forKey: "deviceName") as? String else {
                return .failure(HotspotFailure(detail: "sharingd named a device with no deviceName: \(device)"))
            }
            phones.append(HotspotPhone(name: name, handle: device))
        }
        return .success(phones)
    }

    func enable(_ phone: HotspotPhone, within: TimeInterval) -> Result<HotspotCredentials, HotspotFailure> {
        guard let device = phone.handle else {
            return .failure(HotspotFailure(detail: "the phone \(phone.name) carries no sharingd device to enable"))
        }
        let selector = NSSelectorFromString("enableRemoteHotspotForDevice:withCompletionHandler:")
        typealias Completion = @convention(block) (AnyObject?, AnyObject?) -> Void
        typealias Enable = @convention(c) (AnyObject, Selector, AnyObject, Completion) -> Void
        let call = unsafeBitCast(session.method(for: selector), to: Enable.self)
        // THE COMPLETION ANSWERS THE HOTSPOT'S INFO, or nil and an error
        // (2026-11-26: SFRemoteHotspotInfo with name, password and channel).
        return awaiting("sharingd's ask to enable \(phone.name)'s hotspot", within: within) { answer in
            // THE BLOCK IS BOUND BEFORE THE CALL, so it is an escaping
            // closure: sharingd keeps it until the phone answers, and a block
            // written inline in the call is non-escaping, which Swift traps
            // on the moment sharingd keeps it (2026-11-26 14:20:40, the
            // helper's first lid-closed join).
            let completion: Completion = { info, error in answer(hotspotCredentials(info: info, error: error)) }
            call(session, selector, device, completion)
        }
    }
}

// CoreWLANRadio scans for and joins one network on the Wi-Fi interface. The
// helper's Location Services permission is what lets a scan name networks.
struct CoreWLANRadio: HotspotRadio {
    let iface: CWInterface

    func current() -> String? { iface.ssid() }

    func find(ssid: String, within: TimeInterval) -> Result<AnyObject, HotspotFailure> {
        bounded("the scan for \(ssid)", within: within) {
            do {
                let strongest = try iface.scanForNetworks(withName: ssid).max { $0.rssiValue < $1.rssiValue }
                guard let network = strongest else {
                    return .failure(HotspotFailure(detail: "no network named \(ssid) is in range"))
                }
                return .success(network)
            } catch {
                return .failure(HotspotFailure(detail: "the scan for \(ssid) failed: \(error.localizedDescription)"))
            }
        }
    }

    func join(_ network: AnyObject, password: String?, within: TimeInterval) -> Result<Void, HotspotFailure> {
        // The network is find's own answer.
        let network = network as! CWNetwork
        return bounded("the join of \(network.ssid ?? "the hotspot")", within: within) {
            do {
                try iface.associate(to: network, password: password)
                return .success(())
            } catch {
                return .failure(HotspotFailure(detail: "the join failed: \(error.localizedDescription)"))
            }
        }
    }
}
