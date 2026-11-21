import Foundation
import SwiftProtobuf

/// The daemon's Connect service, whose one procedure the launcher calls.
public let agentReplService = "agentrepl.v1.AgentRepl"

/// A Connect call that failed: the transport, an HTTP status, or the
/// protocol's own error (its code and message).
public struct ConnectError: Error, CustomStringConvertible, Equatable {
    public let code: String
    public let message: String
    public var description: String { "connect \(code): \(message)" }
}

/// Reads a Connect error object (`{"code": ..., "message": ...}`), whether
/// bare (a unary error body) or under `error` (an end-of-stream frame). Nil
/// for an end-of-stream frame that carries no error.
public func connectError(fromJSON data: Data) -> ConnectError? {
    guard let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
        return ConnectError(code: "unknown", message: String(decoding: data, as: UTF8.self))
    }
    let body = (object["error"] as? [String: Any]) ?? (object["code"] != nil ? object : nil)
    guard let body else { return nil }
    return ConnectError(code: body["code"] as? String ?? "unknown", message: body["message"] as? String ?? "")
}

/// The daemon's Connect endpoint at one advertised address, over HTTP/1.1
/// (the daemon serves Connect on HTTP/1.1 and h2c alike), with binary
/// protobuf bodies: unary calls only.
public struct ConnectClient: Sendable {
    public let address: String
    private let session: URLSession

    public init(address: String, session: URLSession = .shared) {
        self.address = address
        self.session = session
    }

    private func request(_ method: String, contentType: String) -> URLRequest {
        var request = URLRequest(url: URL(string: "http://\(address)/\(agentReplService)/\(method)")!)
        request.httpMethod = "POST"
        request.setValue(contentType, forHTTPHeaderField: "Content-Type")
        request.setValue("1", forHTTPHeaderField: "Connect-Protocol-Version")
        request.timeoutInterval = .infinity
        return request
    }

    /// Calls one unary procedure.
    public func unary<Req: SwiftProtobuf.Message, Resp: SwiftProtobuf.Message>(
        _ method: String, _ message: Req, timeout: TimeInterval
    ) async throws -> Resp {
        var request = request(method, contentType: "application/proto")
        request.timeoutInterval = timeout
        request.httpBody = try message.serializedData()
        let (data, response) = try await session.data(for: request)
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard status == 200 else {
            throw connectError(fromJSON: data) ?? ConnectError(code: "http_\(status)", message: String(decoding: data, as: UTF8.self))
        }
        return try Resp(serializedBytes: data)
    }
}
