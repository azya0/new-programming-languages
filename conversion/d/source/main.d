import std.stdio;
import std.file;
import std.path;
import std.conv;
import dyaml;
import toml;

void main() {
    auto inputDir = "input";
    auto outputDir = "output";

    if (!inputDir.exists || !inputDir.isDir) {
        stderr.writeln("Директория 'input' не существует");
        return;
    }

    if (!outputDir.exists) {
        mkdir(outputDir);
    }

    foreach (entry; dirEntries(inputDir, SpanMode.shallow)) {
        if (entry.extension == ".yaml" || entry.extension == ".yml") {
            processFile(entry, outputDir);
        }
    }
}

void processFile(string inputPath, string outputDir) {
    try {
        auto node = YAML.load(inputPath);

        auto tomlTable = parseNode(node);
        auto outputName = outputDir ~ "/" ~ inputPath.baseName.changeExtension(".toml");
        
        std.file.write(outputName, toml.serialize(tomlTable));
        
    } catch (Exception e) {
        stderr.writeln("Ошибка обработки файла ", inputPath, ": ", e.msg);
    }
}

Toml parseNode(YAML.Node node) {
    Toml result;

    if (node.type == YAML.NodeType.Mapping) {
        foreach (key, value; node.as!YAML.Mapping) {
            result[key] = convertValue(value);
        }
    } else if (node.type == YAML.NodeType.Sequence) {
        result = Toml(convertValue(node));
    }

    return result;
}

Toml convertValue(YAML.Node node) {
    switch (node.type) {
        case YAML.NodeType.Scalar:
            return parseScalar(node.as!string);
        case YAML.NodeType.Sequence:
            Toml[] arr;
            foreach (elem; node.as!YAML.Sequence) {
                arr ~= convertValue(elem);
            }
            return Toml(arr);
        case YAML.NodeType.Mapping:
            Toml tbl;
            foreach (key, value; node.as!YAML.Mapping) {
                tbl[key] = convertValue(value);
            }
            return tbl;
        default:
            return Toml();
    }
}

Toml parseScalar(string value) {
    if (value.length == 0) return Toml(value);

    try {
        if (value.canParse!long) {
            return Toml(value.to!long);
        }
    } catch {}

    try {
        if (value.canParse!double) {
            return Toml(value.to!double);
        }
    } catch {}

    if (value == "true" || value == "false") {
        return Toml(value == "true");
    }

    if (value == "null") {
        return Toml();
    }

    return Toml(value);
}