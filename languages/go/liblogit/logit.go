package liblogit

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

type Level int

const (
	Trace Level = iota
	Debug
	Info
	Warn
	Error
	Fatal
)

type Logit struct {
	Name       string
	LocalPath string
	RemotePath string
	Level      Level
	Enabled    bool
	Sinks      []string
	Timestamp  bool
	TagLevel   bool
	Format     string
	Metadata   map[string]any
}

type Options struct {
	Name       string         `json:"name"`
	Path       string         `json:"path"`
	LocalPath  string         `json:"localPath"`
	RemotePath string         `json:"remotePath"`
	NetworkPath string        `json:"network_path"`
	Level      string         `json:"level"`
	Enabled    *bool          `json:"enabled"`
	Sinks      []string       `json:"sinks"`
	Timestamp  *bool          `json:"timestamp"`
	TagLevel   *bool          `json:"tag_level"`
	Format     string         `json:"format"`
	Metadata   map[string]any `json:"metadata"`
}

func New() *Logit {
	return &Logit{
		Name:      "default",
		Level:     Info,
		Enabled:   true,
		Sinks:     []string{"console"},
		Timestamp: true,
		TagLevel:  true,
		Format:    "text",
		Metadata:  map[string]any{},
	}
}

func FromOptions(options Options) (*Logit, error) {
	logit := New()
	if options.Name != "" {
		logit.Name = options.Name
	}
	if options.Level != "" {
		level, err := ParseLevel(options.Level)
		if err != nil {
			return nil, err
		}
		logit.Level = level
	}
	if options.Enabled != nil {
		logit.Enabled = *options.Enabled
	}
	if options.Timestamp != nil {
		logit.Timestamp = *options.Timestamp
	}
	if options.TagLevel != nil {
		logit.TagLevel = *options.TagLevel
	}
	if options.Format != "" {
		logit.Format = options.Format
	}
	if options.Metadata != nil {
		logit.Metadata = options.Metadata
	}
	localPath := first(options.LocalPath, options.Path)
	if localPath != "" {
		logit.SetLocalPath(localPath)
	}
	remotePath := first(options.RemotePath, options.NetworkPath)
	if remotePath != "" {
		logit.SetRemotePath(remotePath)
	}
	if options.Sinks != nil {
		logit.Sinks = normalizeSinks(options.Sinks)
	}
	return logit, nil
}

func (l *Logit) SetLocalPath(value string) {
	l.LocalPath = value
	l.setSink("file", value != "")
}

func (l *Logit) SetRemotePath(value string) {
	l.RemotePath = value
	l.setSink("network", value != "")
}

func (l *Logit) At(level Level) *Builder {
	return &Builder{logit: l, level: level}
}

func (l *Logit) AtString(level string) (*Builder, error) {
	parsed, err := ParseLevel(level)
	if err != nil {
		return nil, err
	}
	return l.At(parsed), nil
}

func (l *Logit) Log(level Level, message any) error {
	if !l.Enabled || level < l.Level {
		return nil
	}
	rendered, err := l.render(level, stringify(message))
	if err != nil {
		return err
	}
	if contains(l.Sinks, "console") {
		fmt.Fprintln(os.Stderr, rendered)
	}
	if contains(l.Sinks, "file") && l.LocalPath != "" {
		if err := appendLine(l.LocalPath, rendered); err != nil {
			return err
		}
	}
	if contains(l.Sinks, "network") && l.RemotePath != "" && !looksLikeSocket(l.RemotePath) {
		if err := appendLine(l.RemotePath, rendered); err != nil {
			return err
		}
	}
	return nil
}

func (l *Logit) render(level Level, message string) (string, error) {
	if strings.EqualFold(l.Format, "json") {
		event := map[string]any{
			"level":   canonicalLevel(level),
			"logger":  l.Name,
			"message": message,
		}
		if len(l.Metadata) > 0 {
			event["metadata"] = l.Metadata
		}
		if l.Timestamp {
			event["timestamp"] = time.Now().UTC().Format(time.RFC3339Nano)
		}
		bytes, err := json.Marshal(sortedMap(event))
		return string(bytes), err
	}

	parts := []string{}
	if l.Timestamp {
		parts = append(parts, time.Now().Format("2006-01-02T15:04:05-07:00"))
	}
	if l.TagLevel {
		parts = append(parts, renderedLevel(level))
	}
	parts = append(parts, message)
	return strings.Join(parts, " "), nil
}

func LoadLogits(configPath string) (map[string]*Logit, error) {
	bytes, err := os.ReadFile(configPath)
	if err != nil {
		return nil, err
	}
	var root map[string]json.RawMessage
	if err := json.Unmarshal(bytes, &root); err != nil {
		return nil, err
	}
	if _, ok := root["logits"]; !ok {
		var legacy struct {
			Level any `json:"level"`
			Timestamp *bool `json:"timestamp"`
			FileLocation string `json:"file_location"`
			NetworkFileLocation string `json:"network_file_location"`
		}
		if err := json.Unmarshal(bytes, &legacy); err != nil {
			return nil, err
		}
		options := Options{Name: "default", Timestamp: legacy.Timestamp, Path: legacy.FileLocation, RemotePath: legacy.NetworkFileLocation}
		switch level := legacy.Level.(type) {
		case string:
			options.Level = level
		case map[string]any:
			if threshold, ok := level["threshold"].(string); ok {
				options.Level = threshold
			}
		}
		logit, err := FromOptions(options)
		if err != nil {
			return nil, err
		}
		return map[string]*Logit{"default": logit}, nil
	}

	var config struct {
		Defaults Options            `json:"defaults"`
		Logits   map[string]Options `json:"logits"`
	}
	if err := json.Unmarshal(bytes, &config); err != nil {
		return nil, err
	}
	result := map[string]*Logit{}
	for name, options := range config.Logits {
		merged := mergeOptions(config.Defaults, options)
		if merged.Name == "" {
			merged.Name = name
		}
		logit, err := FromOptions(merged)
		if err != nil {
			return nil, err
		}
		result[logit.Name] = logit
	}
	return result, nil
}

type Builder struct {
	logit *Logit
	level Level
	fragments []string
	committed bool
}

func (b *Builder) Append(value any) *Builder {
	if !b.committed {
		b.fragments = append(b.fragments, stringify(value))
	}
	return b
}

func (b *Builder) Commit() error {
	if b.committed || len(b.fragments) == 0 {
		return nil
	}
	b.committed = true
	return b.logit.Log(b.level, strings.Join(b.fragments, ""))
}

func ParseLevel(value string) (Level, error) {
	switch strings.ToLower(strings.TrimSpace(value)) {
	case "trace":
		return Trace, nil
	case "debug":
		return Debug, nil
	case "info":
		return Info, nil
	case "warn", "warning":
		return Warn, nil
	case "error":
		return Error, nil
	case "fatal":
		return Fatal, nil
	default:
		return Info, fmt.Errorf("unknown level: %s", value)
	}
}

func (l *Logit) setSink(sink string, enabled bool) {
	if enabled && !contains(l.Sinks, sink) {
		l.Sinks = append(l.Sinks, sink)
	}
	if !enabled {
		filtered := []string{}
		for _, existing := range l.Sinks {
			if existing != sink {
				filtered = append(filtered, existing)
			}
		}
		l.Sinks = filtered
	}
}

func mergeOptions(defaults Options, current Options) Options {
	merged := defaults
	if current.Name != "" { merged.Name = current.Name }
	if current.Path != "" { merged.Path = current.Path }
	if current.LocalPath != "" { merged.LocalPath = current.LocalPath }
	if current.RemotePath != "" { merged.RemotePath = current.RemotePath }
	if current.NetworkPath != "" { merged.NetworkPath = current.NetworkPath }
	if current.Level != "" { merged.Level = current.Level }
	if current.Enabled != nil { merged.Enabled = current.Enabled }
	if current.Sinks != nil { merged.Sinks = current.Sinks }
	if current.Timestamp != nil { merged.Timestamp = current.Timestamp }
	if current.TagLevel != nil { merged.TagLevel = current.TagLevel }
	if current.Format != "" { merged.Format = current.Format }
	if current.Metadata != nil { merged.Metadata = current.Metadata }
	return merged
}

func normalizeSinks(values []string) []string {
	result := []string{}
	for _, value := range values {
		normalized := strings.ToLower(value)
		if !contains(result, normalized) {
			result = append(result, normalized)
		}
	}
	return result
}

func appendLine(target string, line string) error {
	if parent := filepath.Dir(target); parent != "." && parent != "" {
		if err := os.MkdirAll(parent, 0o755); err != nil {
			return err
		}
	}
	file, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return err
	}
	defer file.Close()
	_, err = fmt.Fprintln(file, line)
	return err
}

func stringify(value any) string {
	switch typed := value.(type) {
	case string:
		return typed
	default:
		bytes, err := json.Marshal(value)
		if err != nil {
			return fmt.Sprint(value)
		}
		return string(bytes)
	}
}

func sortedMap(value map[string]any) map[string]any {
	keys := make([]string, 0, len(value))
	for key := range value {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	result := map[string]any{}
	for _, key := range keys {
		result[key] = value[key]
	}
	return result
}

func first(values ...string) string {
	for _, value := range values {
		if value != "" {
			return value
		}
	}
	return ""
}

func contains(values []string, target string) bool {
	for _, value := range values {
		if value == target {
			return true
		}
	}
	return false
}

func looksLikeSocket(value string) bool {
	lower := strings.ToLower(value)
	return strings.HasPrefix(lower, "tcp://") || strings.HasPrefix(lower, "udp://")
}

func canonicalLevel(value Level) string {
	switch value {
	case Trace: return "trace"
	case Debug: return "debug"
	case Info: return "info"
	case Warn: return "warn"
	case Error: return "error"
	case Fatal: return "fatal"
	default: return "info"
	}
}

func renderedLevel(value Level) string {
	switch value {
	case Trace: return "TRACE"
	case Debug: return "DEBUG"
	case Info: return "INFO"
	case Warn: return "WARNING"
	case Error: return "ERROR"
	case Fatal: return "CRITICAL"
	default: return "INFO"
	}
}
