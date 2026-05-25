using System.Text.Json;

namespace LibLogit;

public enum Level
{
    Trace = 0,
    Debug = 1,
    Info = 2,
    Warn = 3,
    Error = 4,
    Fatal = 5
}

public sealed class Logit
{
    private string? _localPath;
    private string? _remotePath;
    private readonly List<string> _sinks = new() { "console" };

    public string Name { get; set; } = "default";
    public Level Level { get; set; } = Level.Info;
    public bool Timestamp { get; set; } = true;
    public bool TagLevel { get; set; } = true;
    public string Format { get; set; } = "text";
    public Dictionary<string, object?> Metadata { get; set; } = new();
    public bool Enabled { get; set; } = true;

    public string? LocalPath
    {
        get => _localPath;
        set
        {
            _localPath = value;
            SetSink("file", !string.IsNullOrWhiteSpace(value));
        }
    }

    public string? RemotePath
    {
        get => _remotePath;
        set
        {
            _remotePath = value;
            SetSink("network", !string.IsNullOrWhiteSpace(value));
        }
    }

    public IReadOnlyList<string> Sinks
    {
        get => _sinks;
        set
        {
            _sinks.Clear();
            foreach (var sink in value)
            {
                AddSink(sink);
            }
        }
    }

    public Logit()
    {
    }

    public Logit(LogitOptions options)
    {
        Name = string.IsNullOrWhiteSpace(options.Name) ? "default" : options.Name;
        Level = ParseLevel(options.Level ?? "info");
        Timestamp = options.Timestamp ?? true;
        TagLevel = options.TagLevel ?? true;
        Format = options.Format ?? "text";
        Metadata = options.Metadata ?? new Dictionary<string, object?>();
        Enabled = options.Enabled ?? true;
        LocalPath = options.LocalPath ?? options.Path;
        RemotePath = options.RemotePath ?? options.NetworkPath;
        if (options.Sinks is not null)
        {
            Sinks = options.Sinks;
        }
    }

    public LogBuilder At(Level level) => new(this, level);

    public LogBuilder At(string level) => At(ParseLevel(level));

    public void Log(Level level, object? message)
    {
        if (!Enabled || level < Level)
        {
            return;
        }

        var rendered = Render(level, Stringify(message));
        if (_sinks.Contains("console"))
        {
            Console.Error.WriteLine(rendered);
        }
        if (_sinks.Contains("file") && !string.IsNullOrWhiteSpace(LocalPath))
        {
            AppendLine(LocalPath, rendered);
        }
        if (_sinks.Contains("network") && !string.IsNullOrWhiteSpace(RemotePath) && !LooksLikeSocket(RemotePath))
        {
            AppendLine(RemotePath, rendered);
        }
    }

    public void Log(string level, object? message) => Log(ParseLevel(level), message);

    private string Render(Level level, string message)
    {
        if (string.Equals(Format, "json", StringComparison.OrdinalIgnoreCase))
        {
            var payload = new SortedDictionary<string, object?>
            {
                ["level"] = CanonicalLevel(level),
                ["logger"] = Name,
                ["message"] = message
            };
            if (Metadata.Count > 0)
            {
                payload["metadata"] = Metadata;
            }
            if (Timestamp)
            {
                payload["timestamp"] = DateTimeOffset.UtcNow.ToString("O");
            }
            return JsonSerializer.Serialize(payload);
        }

        var parts = new List<string>();
        if (Timestamp)
        {
            parts.Add(DateTimeOffset.Now.ToString("yyyy-MM-ddTHH:mm:sszzz"));
        }
        if (TagLevel)
        {
            parts.Add(RenderedLevel(level));
        }
        parts.Add(message);
        return string.Join(" ", parts);
    }

    public static Dictionary<string, Logit> LoadLogits(string configPath)
    {
        using var document = JsonDocument.Parse(File.ReadAllText(configPath));
        var root = document.RootElement;
        if (!root.TryGetProperty("logits", out var logitsElement))
        {
            var level = "info";
            var tagLevel = true;
            if (root.TryGetProperty("level", out var levelElement))
            {
                if (levelElement.ValueKind == JsonValueKind.String)
                {
                    level = levelElement.GetString() ?? "info";
                }
                else
                {
                    level = levelElement.GetProperty("threshold").GetString() ?? "info";
                    tagLevel = !levelElement.TryGetProperty("tag", out var tagElement) || tagElement.GetBoolean();
                }
            }
            return new Dictionary<string, Logit>
            {
                ["default"] = new(new LogitOptions
                {
                    Name = "default",
                    Level = level,
                    TagLevel = tagLevel,
                    Timestamp = !root.TryGetProperty("timestamp", out var timestamp) || timestamp.GetBoolean(),
                    Path = root.TryGetProperty("file_location", out var file) && file.ValueKind == JsonValueKind.String ? file.GetString() : null,
                    RemotePath = root.TryGetProperty("network_file_location", out var remote) && remote.ValueKind == JsonValueKind.String ? remote.GetString() : null
                })
            };
        }

        var defaults = root.TryGetProperty("defaults", out var defaultsElement)
            ? ReadOptions(defaultsElement, "default")
            : new LogitOptions();

        var result = new Dictionary<string, Logit>();
        foreach (var logitProperty in logitsElement.EnumerateObject())
        {
            var options = Merge(defaults, ReadOptions(logitProperty.Value, logitProperty.Name));
            options.Name ??= logitProperty.Name;
            result[options.Name] = new Logit(options);
        }
        return result;
    }

    private static LogitOptions ReadOptions(JsonElement element, string name)
    {
        var options = new LogitOptions { Name = name };
        if (element.TryGetProperty("name", out var nameElement))
        {
            options.Name = nameElement.GetString();
        }
        if (element.TryGetProperty("level", out var levelElement))
        {
            options.Level = levelElement.GetString();
        }
        if (element.TryGetProperty("path", out var pathElement))
        {
            options.Path = pathElement.ValueKind == JsonValueKind.String ? pathElement.GetString() : null;
        }
        if (element.TryGetProperty("localPath", out var localPathElement))
        {
            options.LocalPath = localPathElement.ValueKind == JsonValueKind.String ? localPathElement.GetString() : null;
        }
        if (element.TryGetProperty("remotePath", out var remotePathElement))
        {
            options.RemotePath = remotePathElement.ValueKind == JsonValueKind.String ? remotePathElement.GetString() : null;
        }
        if (element.TryGetProperty("network_path", out var networkPathElement))
        {
            options.NetworkPath = networkPathElement.ValueKind == JsonValueKind.String ? networkPathElement.GetString() : null;
        }
        if (element.TryGetProperty("timestamp", out var timestampElement))
        {
            options.Timestamp = timestampElement.GetBoolean();
        }
        if (element.TryGetProperty("tag_level", out var tagLevelElement))
        {
            options.TagLevel = tagLevelElement.GetBoolean();
        }
        if (element.TryGetProperty("format", out var formatElement))
        {
            options.Format = formatElement.GetString();
        }
        if (element.TryGetProperty("sinks", out var sinksElement))
        {
            options.Sinks = sinksElement.EnumerateArray().Select(item => item.GetString() ?? "").Where(item => item.Length > 0).ToArray();
        }
        if (element.TryGetProperty("metadata", out var metadataElement))
        {
            options.Metadata = JsonSerializer.Deserialize<Dictionary<string, object?>>(metadataElement.GetRawText()) ?? new();
        }
        return options;
    }

    private static LogitOptions Merge(LogitOptions defaults, LogitOptions current)
    {
        return new LogitOptions
        {
            Name = current.Name ?? defaults.Name,
            Path = current.Path ?? defaults.Path,
            LocalPath = current.LocalPath ?? defaults.LocalPath,
            RemotePath = current.RemotePath ?? defaults.RemotePath,
            NetworkPath = current.NetworkPath ?? defaults.NetworkPath,
            Level = current.Level ?? defaults.Level,
            Enabled = current.Enabled ?? defaults.Enabled,
            Sinks = current.Sinks ?? defaults.Sinks,
            Timestamp = current.Timestamp ?? defaults.Timestamp,
            TagLevel = current.TagLevel ?? defaults.TagLevel,
            Format = current.Format ?? defaults.Format,
            Metadata = current.Metadata ?? defaults.Metadata
        };
    }

    private void SetSink(string sink, bool enabled)
    {
        if (enabled)
        {
            AddSink(sink);
            return;
        }
        _sinks.Remove(sink);
    }

    private void AddSink(string sink)
    {
        var normalized = sink.ToLowerInvariant();
        if (normalized is not ("console" or "file" or "network"))
        {
            throw new ArgumentException($"Unknown sink: {sink}");
        }
        if (!_sinks.Contains(normalized))
        {
            _sinks.Add(normalized);
        }
    }

    private static void AppendLine(string target, string line)
    {
        var parent = Path.GetDirectoryName(target);
        if (!string.IsNullOrEmpty(parent))
        {
            Directory.CreateDirectory(parent);
        }
        File.AppendAllText(target, line + Environment.NewLine);
    }

    private static bool LooksLikeSocket(string target) =>
        target.StartsWith("tcp://", StringComparison.OrdinalIgnoreCase)
        || target.StartsWith("udp://", StringComparison.OrdinalIgnoreCase);

    private static string Stringify(object? value) =>
        value switch
        {
            null => "null",
            string text => text,
            _ => JsonSerializer.Serialize(value)
        };

    public static Level ParseLevel(string level)
    {
        return level.Trim().ToLowerInvariant() switch
        {
            "trace" => Level.Trace,
            "debug" => Level.Debug,
            "info" => Level.Info,
            "warn" or "warning" => Level.Warn,
            "error" => Level.Error,
            "fatal" => Level.Fatal,
            _ => throw new ArgumentException($"Unknown level: {level}")
        };
    }

    private static string CanonicalLevel(Level level) => level switch
    {
        Level.Trace => "trace",
        Level.Debug => "debug",
        Level.Info => "info",
        Level.Warn => "warn",
        Level.Error => "error",
        Level.Fatal => "fatal",
        _ => "info"
    };

    private static string RenderedLevel(Level level) => level switch
    {
        Level.Trace => "TRACE",
        Level.Debug => "DEBUG",
        Level.Info => "INFO",
        Level.Warn => "WARNING",
        Level.Error => "ERROR",
        Level.Fatal => "CRITICAL",
        _ => "INFO"
    };
}

public sealed class LogBuilder
{
    private readonly Logit _logit;
    private readonly Level _level;
    private readonly List<string> _fragments = new();
    private bool _committed;

    internal LogBuilder(Logit logit, Level level)
    {
        _logit = logit;
        _level = level;
    }

    public LogBuilder Append(object? value)
    {
        if (!_committed)
        {
            _fragments.Add(value?.ToString() ?? "null");
        }
        return this;
    }

    public LogBuilder Commit()
    {
        if (!_committed && _fragments.Count > 0)
        {
            _committed = true;
            _logit.Log(_level, string.Concat(_fragments));
        }
        return this;
    }
}

public sealed class LogitOptions
{
    public string? Name { get; set; }
    public string? Path { get; set; }
    public string? LocalPath { get; set; }
    public string? RemotePath { get; set; }
    public string? NetworkPath { get; set; }
    public string? Level { get; set; }
    public bool? Enabled { get; set; }
    public IReadOnlyList<string>? Sinks { get; set; }
    public bool? Timestamp { get; set; }
    public bool? TagLevel { get; set; }
    public string? Format { get; set; }
    public Dictionary<string, object?>? Metadata { get; set; }
}
