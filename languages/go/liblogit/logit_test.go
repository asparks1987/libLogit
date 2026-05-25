package liblogit

import (
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestBlankDeclaration(t *testing.T) {
	dir := t.TempDir()
	logPath := filepath.Join(dir, "app.log")
	logIT := New()
	logIT.SetLocalPath(logPath)
	logIT.Level = Debug
	logIT.Timestamp = false
	if err := logIT.At(Debug).Append("hello").Append(" world").Commit(); err != nil {
		t.Fatal(err)
	}
	assertLines(t, logPath, []string{"DEBUG hello world"})
}

func TestSharedFixtures(t *testing.T) {
	fixturePaths, err := filepath.Glob(filepath.Join("..", "..", "..", "tests", "conformance", "fixtures", "*.json"))
	if err != nil {
		t.Fatal(err)
	}
	for _, fixturePath := range fixturePaths {
		t.Run(filepath.Base(fixturePath), func(t *testing.T) {
			var fixture struct {
				Config map[string]any `json:"config"`
				Messages []struct {
					Logger string `json:"logger"`
					Level string `json:"level"`
					Fragments []string `json:"fragments"`
				} `json:"messages"`
				ExpectedFiles map[string][]string `json:"expected_files"`
			}
			bytes, err := os.ReadFile(fixturePath)
			if err != nil { t.Fatal(err) }
			if err := json.Unmarshal(bytes, &fixture); err != nil { t.Fatal(err) }
			dir := t.TempDir()
			logits := fixture.Config["logits"].(map[string]any)
			for _, rawLogit := range logits {
				logitConfig := rawLogit.(map[string]any)
				rewritePath(logitConfig, "path", dir)
				rewritePath(logitConfig, "localPath", dir)
				rewritePath(logitConfig, "file_location", dir)
				rewritePath(logitConfig, "remotePath", dir)
				rewritePath(logitConfig, "remote_path", dir)
				rewritePath(logitConfig, "network_path", dir)
				rewritePath(logitConfig, "network_file_location", dir)
			}
			configPath := filepath.Join(dir, "logit.json")
			configBytes, err := json.Marshal(fixture.Config)
			if err != nil { t.Fatal(err) }
			if err := os.WriteFile(configPath, configBytes, 0o644); err != nil { t.Fatal(err) }
			loaded, err := LoadLogits(configPath)
			if err != nil { t.Fatal(err) }
			for _, message := range fixture.Messages {
				builder, err := loaded[message.Logger].AtString(message.Level)
				if err != nil { t.Fatal(err) }
				for _, fragment := range message.Fragments {
					builder.Append(fragment)
				}
				if err := builder.Commit(); err != nil { t.Fatal(err) }
			}
			for relativePath, expected := range fixture.ExpectedFiles {
				assertLines(t, filepath.Join(dir, relativePath), expected)
			}
		})
	}
}

func rewritePath(config map[string]any, key string, dir string) {
	value, ok := config[key].(string)
	if ok && value != "" {
		config[key] = filepath.Join(dir, value)
	}
}

func assertLines(t *testing.T, path string, expected []string) {
	t.Helper()
	bytes, err := os.ReadFile(path)
	if err != nil { t.Fatal(err) }
	lines := splitLines(string(bytes))
	if !reflect.DeepEqual(lines, expected) {
		t.Fatalf("unexpected contents for %s: %#v", path, lines)
	}
}

func splitLines(value string) []string {
	lines := []string{}
	for _, line := range strings.Split(strings.TrimSpace(value), "\n") {
		lines = append(lines, strings.TrimRight(line, "\r"))
	}
	return lines
}
