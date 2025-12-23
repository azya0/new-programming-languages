@Grab('org.yaml:snakeyaml:1.30')
import java.util.regex.Pattern
import java.util.regex.Matcher
import java.nio.file.*

Map<String, String> loadCodes() {
    def codes = [:]
    Path codesPath = Paths.get("codes.txt")
    if (Files.exists(codesPath)) {
        Files.readAllLines(codesPath).each { line ->
            def parts = line.split(":", 2)
            if (parts.size() == 2) {
                codes[parts[0].trim()] = parts[1].trim()
            }
        }
    }
    return codes
}

String normalizePhone(String phone) {
    String digits = phone.replaceAll(/\D/, '')
    
    if (digits.length() >= 10) {
        if (digits.startsWith("7") || digits.startsWith("8")) {
            digits = "7" + digits.substring(digits.length() - 10)
        } else if (digits.length() == 10) {
            digits = "7" + digits
        }
    }
    
    if (digits.length() == 11 && (digits.startsWith("7") || digits.startsWith("8"))) {
        digits = "7" + digits.substring(1)
    }
    
    return digits
}

String formatPhone(String normalized) {
    if (normalized.length() == 11 && normalized.startsWith("7")) {
        def operator = normalized.substring(1, 4)
        def part1 = normalized.substring(4, 7)
        def part2 = normalized.substring(7, 9)
        def part3 = normalized.substring(9, 11)
        return "+7 ($operator) $part1-$part2-$part3"
    }
    return normalized
}

String detectOperator(String normalized, Map<String, String> codes) {
    if (normalized.length() >= 4 && normalized.startsWith("7")) {
        def prefix = normalized.substring(1, 4)
        return codes.getOrDefault(prefix, "Неизвестный оператор")
    }
    return "Неизвестный оператор"
}

boolean validatePhone(String phone) {
    def normalized = phone.replaceAll(/\s+/, "")
    
    Pattern mobilePattern = ~/^(8|\+7|7)?[-\s]?\(?9\d{2}\)?[-\s]?\d{3}[-\s]?\d{2}[-\s]?\d{2}$/
    Pattern cityPattern = ~/^(8|\+7|7)?[-\s]?\(?[348]\d{2}\)?[-\s]?\d{3}[-\s]?\d{2}[-\s]?\d{2}$/
    Pattern internationalPattern = ~/^\+(?:[1-9]\d{0,2}[-\s]?)?\(?([1-9]\d{0,2})\)?[-\s]?\d{1,4}[-\s]?\d{1,4}[-\s]?\d{1,4}$/
    
    if (mobilePattern.matcher(normalized).matches()) {
        return true
    }
    
    if (cityPattern.matcher(normalized).matches()) {
        return true
    }
    
    if (internationalPattern.matcher(normalized).matches()) {
        return true
    }
    
    return false
}

void processFile(String inputPath) {
    def codes = loadCodes()
    Path path = Paths.get(inputPath)
    
    if (!Files.exists(path)) {
        println "Файл $inputPath не найден"
        return
    }
    
    Files.readAllLines(path).eachWithIndex { line, index ->
        if (line.trim().isEmpty()) return
        
        println "Ввод: ${line.trim()}"
        
        if (validatePhone(line.trim())) {
            def normalized = normalizePhone(line.trim())
            def formatted = formatPhone(normalized)
            def operator = detectOperator(normalized, codes)
            
            println "Валидный номер: $formatted"
            println "Код оператора/региона: $operator"
        } else {
            println "Невалидный номер"
        }
        println "---"
    }
}

if (args.length > 0 && args[0] == "--docker") {
    processFile("input/input.txt")
} else {
    processFile("input/input.txt")
}