import Foundation

/// The daemon's advertisement in `<state>/daemon.addr` (logging-contract.md
/// "Daemon address advertisement"): the `host:port` it serves on, and the
/// pid of the daemon advertising it when the file names one.
public struct DaemonAdvertisement: Equatable, Sendable {
    /// The bare `host:port` a client dials.
    public let address: String
    /// The advertising daemon's pid; nil for a file that names none.
    public let pid: Int32?

    public init(address: String, pid: Int32?) {
        self.address = address
        self.pid = pid
    }
}

/// Parses an advertisement's text: the first line is the address, and a
/// later `pid=<n>` line names the daemon. Nil for a file with no address.
public func parseAdvertisement(_ text: String) -> DaemonAdvertisement? {
    let lines = text.split(whereSeparator: \.isNewline).map { $0.trimmingCharacters(in: .whitespaces) }
    guard let address = lines.first, !address.isEmpty else { return nil }
    var pid: Int32?
    for line in lines.dropFirst() where line.hasPrefix("pid=") {
        pid = Int32(line.dropFirst(4))
    }
    return DaemonAdvertisement(address: address, pid: pid)
}

/// The state root's advertisement file.
public func advertisementPath(stateDir: URL) -> URL {
    stateDir.appendingPathComponent("daemon.addr")
}

/// Reads the advertisement, or nil when no daemon advertises (the file is
/// absent, which is a daemon between runs, or names no address).
public func readAdvertisement(stateDir: URL) -> DaemonAdvertisement? {
    guard let data = FileManager.default.contents(atPath: advertisementPath(stateDir: stateDir).path),
          let text = String(data: data, encoding: .utf8) else { return nil }
    return parseAdvertisement(text)
}

/// Watches the state root for advertisement changes. The daemon writes
/// `daemon.addr` by atomic rename and removes it on orderly exit, both of
/// which change the directory, so the directory is what is watched; each
/// change re-reads the file and reports it when it differs from the last.
public final class AdvertisementWatcher: @unchecked Sendable {
    private let stateDir: URL
    private let queue = DispatchQueue(label: "zettel.advertisement")
    private var source: DispatchSourceFileSystemObject?
    private var last: DaemonAdvertisement?
    private let changed: @Sendable (DaemonAdvertisement?) -> Void

    /// `changed` is called on the watcher's queue with each new
    /// advertisement (nil when the file went away).
    public init(stateDir: URL, changed: @escaping @Sendable (DaemonAdvertisement?) -> Void) {
        self.stateDir = stateDir
        self.changed = changed
    }

    /// Starts watching and reports the advertisement standing now.
    public func start() throws {
        let fd = open(stateDir.path, O_EVTONLY)
        guard fd >= 0 else {
            throw NSError(domain: NSPOSIXErrorDomain, code: Int(errno),
                          userInfo: [NSLocalizedDescriptionKey: "watch the state root \(stateDir.path)"])
        }
        let source = DispatchSource.makeFileSystemObjectSource(fileDescriptor: fd, eventMask: [.write, .rename, .delete], queue: queue)
        source.setEventHandler { [weak self] in self?.reread() }
        source.setCancelHandler { close(fd) }
        self.source = source
        source.resume()
        queue.async { [weak self] in self?.report(readAdvertisement(stateDir: self!.stateDir)) }
    }

    private func reread() {
        let current = readAdvertisement(stateDir: stateDir)
        if current != last {
            report(current)
        }
    }

    private func report(_ advertisement: DaemonAdvertisement?) {
        last = advertisement
        changed(advertisement)
    }
}
