import Foundation
import ZettelNotifier
import ZettelProto

/// One record a RecordingLogger took.
struct Record: Equatable {
    let level: LogLevel
    let operation: String
    let message: String
    let context: [String: String]
}

/// A Logger that keeps every record, for asserting the canonical log.
final class RecordingLogger: Logger, @unchecked Sendable {
    let signals = Signals()
    private let lock = NSLock()
    private var taken: [Record] = []

    func log(_ level: LogLevel, _ operation: String, _ message: String, _ context: LogContext) {
        lock.lock()
        defer { lock.unlock() }
        taken.append(Record(level: level, operation: operation, message: message, context: context.mapValues { "\($0)" }))
        Task { [signals] in await signals.raise(message) }
    }

    var records: [Record] {
        lock.lock()
        defer { lock.unlock() }
        return taken
    }

    func first(_ level: LogLevel, _ message: String) -> Record? {
        records.first { $0.level == level && $0.message == message }
    }
}

/// A Daemon whose banner stream the test drives frame by frame, and which
/// records every report.
final class FakeDaemon: Daemon, @unchecked Sendable {
    let address: String
    let signals: Signals
    private let lock = NSLock()
    private var continuation: AsyncThrowingStream<Agentrepl_V1_WatchDesktopBannersResponse, Error>.Continuation?
    private var opened: [Agentrepl_V1_WatchDesktopBannersRequest] = []
    private var posted: [Agentrepl_V1_ReportDesktopBannerPostedRequest] = []
    private var clicked: [Agentrepl_V1_ReportDesktopBannerClickedRequest] = []
    var postedAnswer = Agentrepl_V1_ReportDesktopBannerPostedResponse.with { $0.success = .init() }
    var clickAnswer = Agentrepl_V1_ReportDesktopBannerClickedResponse.with { $0.success = .init() }
    var clickError: Error?

    init(address: String, signals: Signals) {
        self.address = address
        self.signals = signals
    }

    func watchBanners(_ request: Agentrepl_V1_WatchDesktopBannersRequest) -> AsyncThrowingStream<Agentrepl_V1_WatchDesktopBannersResponse, Error> {
        let (stream, continuation) = AsyncThrowingStream<Agentrepl_V1_WatchDesktopBannersResponse, Error>.makeStream()
        lock.lock()
        opened.append(request)
        self.continuation = continuation
        lock.unlock()
        Task { [signals, address] in await signals.raise("opened \(address)") }
        return stream
    }

    func reportPosted(_ request: Agentrepl_V1_ReportDesktopBannerPostedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerPostedResponse {
        let answer = lock.withLock {
            posted.append(request)
            return postedAnswer
        }
        Task { [signals] in await signals.raise("reported") }
        return answer
    }

    func reportClicked(_ request: Agentrepl_V1_ReportDesktopBannerClickedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerClickedResponse {
        let (answer, failure) = lock.withLock {
            clicked.append(request)
            return (clickAnswer, clickError)
        }
        Task { [signals, address] in await signals.raise("clicked \(address)") }
        if let failure { throw failure }
        return answer
    }

    func send(_ frame: Agentrepl_V1_WatchDesktopBannersResponse) {
        lock.lock()
        let continuation = self.continuation
        lock.unlock()
        continuation?.yield(frame)
    }

    func finish(throwing error: Error? = nil) {
        lock.lock()
        let continuation = self.continuation
        lock.unlock()
        continuation?.finish(throwing: error)
    }

    var opens: [Agentrepl_V1_WatchDesktopBannersRequest] { lock.lock(); defer { lock.unlock() }; return opened }
    var posts: [Agentrepl_V1_ReportDesktopBannerPostedRequest] { lock.lock(); defer { lock.unlock() }; return posted }
    var clicks: [Agentrepl_V1_ReportDesktopBannerClickedRequest] { lock.lock(); defer { lock.unlock() }; return clicked }
}

/// Dials one FakeDaemon per address, the same one every time.
final class FakeDialer: @unchecked Sendable {
    let signals = Signals()
    private let lock = NSLock()
    private var daemons: [String: FakeDaemon] = [:]
    private(set) var dials: [String] = []

    func daemon(_ address: String) -> FakeDaemon {
        lock.lock()
        defer { lock.unlock() }
        if let d = daemons[address] { return d }
        let d = FakeDaemon(address: address, signals: signals)
        daemons[address] = d
        return d
    }

    var dial: DaemonDialer {
        { [self] address in
            lock.lock()
            dials.append(address)
            lock.unlock()
            return daemon(address)
        }
    }

    var dialCount: Int { lock.lock(); defer { lock.unlock() }; return dials.count }
}

/// A BannerCenter answering one scripted result and keeping what it posted.
final class FakeCenter: BannerCenter, @unchecked Sendable {
    private let lock = NSLock()
    private var taken: [Agentrepl_V1_DesktopBanner] = []
    var result: PostResult = .posted

    func post(_ banner: Agentrepl_V1_DesktopBanner) async -> PostResult {
        lock.withLock {
            taken.append(banner)
            return result
        }
    }

    var posted: [Agentrepl_V1_DesktopBanner] { lock.lock(); defer { lock.unlock() }; return taken }
}

/// Named events the fakes raise, which a test awaits: the relay works on
/// tasks of its own, so a test waits for the event its act causes rather than
/// polling for the effect.
actor Signals {
    private var counts: [String: Int] = [:]
    private var waiters: [(name: String, count: Int, resume: CheckedContinuation<Void, Never>)] = []

    func raise(_ name: String) {
        counts[name, default: 0] += 1
        let ready = waiters.filter { counts[$0.name, default: 0] >= $0.count }
        waiters.removeAll { counts[$0.name, default: 0] >= $0.count }
        ready.forEach { $0.resume.resume() }
    }

    /// Returns once `name` has been raised `count` times in all.
    func wait(_ name: String, count: Int = 1) async {
        if counts[name, default: 0] >= count { return }
        await withCheckedContinuation { waiters.append((name, count, $0)) }
    }
}

func bannerFrame(post: UInt64, group: String = "agent-repl:ws1", workspace: String = "ws1") -> Agentrepl_V1_WatchDesktopBannersResponse {
    .with {
        $0.banner = .with { b in
            b.postID.value = post
            b.title.text = "✅ ws"
            b.body.text = "done"
            b.group.id = group
            b.click.workspace.id = workspace
            b.click.workspace.dir = "/work/\(workspace)"
        }
    }
}
