import Foundation
import ZettelProto

/// What the platform's notification center did with one banner.
public enum PostResult: Equatable, Sendable {
    case posted
    case notAllowed
    case failed(String)
}

/// The platform's notification center, as the launcher posts through it.
public protocol BannerCenter: Sendable {
    /// Posts one banner, storing its click token for the click to return.
    func post(_ banner: Agentrepl_V1_DesktopBanner) async -> PostResult
}

/// The operations the banner server's records carry.
enum ServerOp {
    static let accept = "launcher.server.accept"
    static let post = "launcher.server.post"
}

/// Why a request cannot be posted, or nil when it can.
public func refusal(of request: Launcher_V1_PostBannerRequest) -> String? {
    guard request.hasBanner else { return "banner is missing" }
    let banner = request.banner
    if banner.title.text.isEmpty { return "banner.title is empty" }
    if banner.body.text.isEmpty { return "banner.body is empty" }
    if banner.group.id.isEmpty { return "banner.group is empty" }
    if banner.click.workspace.id.isEmpty { return "banner.click.workspace is empty" }
    if request.daemon.pid <= 0 { return "daemon.pid is not positive" }
    return nil
}

/// The answer for one post's result.
public func answer(for result: PostResult) -> Launcher_V1_PostBannerResponse {
    .with { response in
        switch result {
        case .posted: response.outcome.posted = .init()
        case .notAllowed: response.outcome.notAllowed = .init()
        case .failed(let platformError): response.outcome.failed.platformError = platformError
        }
    }
}

/// One exchange of launcher.v1: a daemon's banner read, posted, and
/// answered with what macOS did, on one connection.
public struct BannerExchange: Sendable {
    let center: BannerCenter
    let log: Logger

    public init(center: BannerCenter, log: Logger) {
        self.center = center
        self.log = log
    }

    /// Serves the one exchange on `channel`.
    public func serve(_ channel: ByteChannel) async {
        let request: Launcher_V1_PostBannerRequest
        do {
            request = try readDelimited(Launcher_V1_PostBannerRequest.self, from: channel)
        } catch {
            log.error(ServerOp.post, "a daemon's banner could not be read; nothing was posted", ["cause": "\(error)"])
            return
        }
        let fields: LogContext = ["daemon_pid": request.daemon.pid, "group": request.banner.group.id]
        let response: Launcher_V1_PostBannerResponse
        if let reason = refusal(of: request) {
            log.error(ServerOp.post, "a daemon's banner was refused; nothing was posted", fields.merging(["reason": reason]) { $1 })
            response = .with { $0.refused.reason = reason }
        } else {
            let result = await center.post(request.banner)
            switch result {
            case .posted:
                log.info(ServerOp.post, "posted a banner", fields)
            case .notAllowed:
                log.error(ServerOp.post, "macOS refused the banner: Zettel is not allowed to notify (System Settings → Notifications → Zettel)", fields)
            case .failed(let cause):
                log.error(ServerOp.post, "macOS refused the banner", fields.merging(["cause": cause]) { $1 })
            }
            response = answer(for: result)
        }
        do {
            try writeDelimited(response, to: channel)
        } catch {
            log.error(ServerOp.post, "the banner's answer did not reach its daemon", fields.merging(["cause": "\(error)"]) { $1 })
        }
    }
}

/// How long one connection may take to hand over its banner and read its
/// answer, each way: a daemon writes its request at once and reads the answer
/// within its own post bound (5s, desktopnotify.LauncherPostBound).
public let exchangeTimeout: TimeInterval = 5

/// Accepts on the launcher's listening socket, serving each connection's one
/// exchange on a task of its own, until the process ends.
public final class BannerServer: @unchecked Sendable {
    let listener: Int32
    let exchange: BannerExchange
    let log: Logger

    public init(listener: Int32, exchange: BannerExchange, log: Logger) {
        self.listener = listener
        self.exchange = exchange
        self.log = log
    }

    /// Starts accepting on a thread of its own.
    public func start() {
        let thread = Thread { [self] in acceptLoop() }
        thread.name = "launcher.server.accept"
        thread.start()
    }

    private func acceptLoop() {
        while true {
            let fd = accept(listener, nil, nil)
            if fd < 0 {
                if errno == EINTR { continue }
                log.error(ServerOp.accept, "the launcher's socket stopped accepting; no banner posts until the launcher restarts", ["cause": String(cString: strerror(errno))])
                return
            }
            let channel: SocketChannel
            do {
                channel = try SocketChannel(fd: fd, timeout: exchangeTimeout)
            } catch {
                log.error(ServerOp.accept, "a daemon's connection could not be bounded; it was closed", ["cause": "\(error)"])
                continue
            }
            Task { [exchange] in await exchange.serve(channel) }
        }
    }
}
