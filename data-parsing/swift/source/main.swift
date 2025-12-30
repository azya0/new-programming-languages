import Foundation

struct DateParser {
    static let formatters: [DateFormatter] = {
        let formats = [
            "yyyy-MM-dd'T'HH:mm:ssZZZZZ",
            "yyyy-MM-dd'T'HH:mm:ss.SSSZZZZZ",
            "EEE, dd MMM yyyy HH:mm:ss Z",
            "dd.MM.yyyy",
            "MM/dd/yyyy",
            "yyyy-MM-dd",
            "yyyy/MM/dd HH:mm:ss"
        ]
        
        return formats.map { format in
            let formatter = DateFormatter()
            formatter.dateFormat = format
            formatter.locale = Locale(identifier: "en_US_POSIX")
            formatter.timeZone = TimeZone(secondsFromGMT: 0)
            return formatter
        }
    }()
    
    static func parseDate(_ dateString: String) -> Date? {
        for formatter in formatters {
            if let date = formatter.date(from: dateString) {
                return date
            }
        }
        
        if let date = ISO8601DateFormatter().date(from: dateString) {
            return date
        }
        
        return nil
    }
}

guard CommandLine.arguments.count == 3 else {
    print("Usage: \(CommandLine.arguments[0]) <date_string> <timezone>")
    exit(1)
}

let inputDateString = CommandLine.arguments[1]
let timeZoneIdentifier = CommandLine.arguments[2]

guard let timeZone = TimeZone(identifier: timeZoneIdentifier) else {
    print("INVALID TIMEZONE")
    exit(1)
}

guard let parsedDate = DateParser.parseDate(inputDateString) else {
    print("UNKNOWN FORMAT")
    exit(1)
}

let isoFormatter = ISO8601DateFormatter()
isoFormatter.timeZone = timeZone

let rfc822Formatter = DateFormatter()
rfc822Formatter.dateFormat = "EEE, dd MMM yyyy HH:mm:ss Z"
rfc822Formatter.locale = Locale(identifier: "en_US_POSIX")
rfc822Formatter.timeZone = timeZone

let customFormatter = DateFormatter()
customFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss ZZZZ"
customFormatter.locale = Locale(identifier: "en_US_POSIX")
customFormatter.timeZone = timeZone

let isoString = isoFormatter.string(from: parsedDate)
let rfc822String = rfc822Formatter.string(from: parsedDate)
let customString = customFormatter.string(from: parsedDate)

let output = """
ISO8601: \(isoString)
RFC822:  \(rfc822String)
CUSTOM:  \(customString)
"""

print(output)