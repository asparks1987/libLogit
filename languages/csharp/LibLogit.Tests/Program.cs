using System.Text.Json;
using System.Text.Json.Nodes;
using LibLogit;

static string TempDir()
{
    var path = Path.Combine(Path.GetTempPath(), "liblogit-csharp-" + Guid.NewGuid().ToString("N"));
    Directory.CreateDirectory(path);
    return path;
}

static string[] ReadLines(string path) => File.ReadAllLines(path);

static void AssertLines(string path, params string[] expected)
{
    var actual = ReadLines(path);
    if (!actual.SequenceEqual(expected))
    {
        throw new Exception($"Unexpected contents for {path}: {string.Join(" | ", actual)}");
    }
}

{
    var dir = TempDir();
    var logPath = Path.Combine(dir, "app.log");
    var logIT = new Logit
    {
        LocalPath = logPath,
        Level = Level.Debug,
        Timestamp = false,
        Sinks = new[] { "file" }
    };

    logIT.At(Level.Debug).Append("hello").Append(" world").Commit();
    AssertLines(logPath, "DEBUG hello world");
}

{
    var dir = TempDir();
    var logPath = Path.Combine(dir, "app.log");
    var configPath = Path.Combine(dir, "logit.json");
    File.WriteAllText(configPath, JsonSerializer.Serialize(new
    {
        version = "0.2",
        defaults = new
        {
            level = "info",
            timestamp = false,
            tag_level = true,
            format = "text",
            sinks = new[] { "file" }
        },
        logits = new Dictionary<string, object>
        {
            ["AppLog"] = new
            {
                path = logPath,
                level = "debug"
            }
        }
    }));

    var logits = Logit.LoadLogits(configPath);
    logits["AppLog"].At("debug").Append("from config").Commit();
    AssertLines(logPath, "DEBUG from config");
}

{
    var dir = TempDir();
    var logPath = Path.Combine(dir, "audit.jsonl");
    var audit = new Logit(new LogitOptions
    {
        Name = "AuditLog",
        LocalPath = logPath,
        Level = "info",
        Timestamp = false,
        Format = "json",
        Sinks = new[] { "file" },
        Metadata = new Dictionary<string, object?> { ["component"] = "audit" }
    });

    audit.Log(Level.Info, "user signed in");
    AssertLines(logPath, "{\"level\":\"info\",\"logger\":\"AuditLog\",\"message\":\"user signed in\",\"metadata\":{\"component\":\"audit\"}}");
}

RunConformanceFixtures();

Console.WriteLine("C# libLogit tests passed");

static void RunConformanceFixtures()
{
    var fixtureDir = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "..", "..", "tests", "conformance", "fixtures"));
    foreach (var fixturePath in Directory.GetFiles(fixtureDir, "*.json").Order())
    {
        var fixture = JsonNode.Parse(File.ReadAllText(fixturePath))!.AsObject();
        var dir = TempDir();
        var config = fixture["config"]!.DeepClone().AsObject();
        var logitsNode = config["logits"]!.AsObject();

        foreach (var logitNode in logitsNode.Select(item => item.Value!.AsObject()))
        {
            RewritePath(logitNode, "path", dir);
            RewritePath(logitNode, "localPath", dir);
            RewritePath(logitNode, "file_location", dir);
            RewritePath(logitNode, "remotePath", dir);
            RewritePath(logitNode, "remote_path", dir);
            RewritePath(logitNode, "network_path", dir);
            RewritePath(logitNode, "network_file_location", dir);
        }

        var configPath = Path.Combine(dir, "logit.json");
        File.WriteAllText(configPath, config.ToJsonString());
        var logits = Logit.LoadLogits(configPath);

        foreach (var message in fixture["messages"]!.AsArray().Select(item => item!.AsObject()))
        {
            var builder = logits[message["logger"]!.GetValue<string>()].At(message["level"]!.GetValue<string>());
            foreach (var fragment in message["fragments"]!.AsArray())
            {
                builder.Append(fragment!.GetValue<string>());
            }
            builder.Commit();
        }

        foreach (var expectedFile in fixture["expected_files"]!.AsObject())
        {
            var expectedLines = expectedFile.Value!.AsArray().Select(item => item!.GetValue<string>()).ToArray();
            AssertLines(Path.Combine(dir, expectedFile.Key), expectedLines);
        }
    }
}

static void RewritePath(JsonObject logitNode, string key, string dir)
{
    if (logitNode.TryGetPropertyValue(key, out var value) && value is not null && value.GetValueKind() == JsonValueKind.String)
    {
        var rawPath = value.GetValue<string>();
        if (!string.IsNullOrWhiteSpace(rawPath))
        {
            logitNode[key] = Path.Combine(dir, rawPath);
        }
    }
}
