package main

import (
	"os"

	"github.com/asparks1987/liblogit/languages/go/liblogit"
)

func main() {
	logIT := liblogit.New()
	logIT.SetLocalPath("logs/go-app.log")
	logIT.Level = liblogit.Debug
	_ = logIT.At(liblogit.Info).Append("Go app started").Commit()

	configured, err := liblogit.LoadLogits(configPath())
	if err != nil {
		panic(err)
	}
	if err := configured["AppLog"].At(liblogit.Info).Append("configured Go log").Commit(); err != nil {
		panic(err)
	}
}

func configPath() string {
	for _, path := range []string{"examples/config/v2-basic.json", "../../examples/config/v2-basic.json"} {
		if _, err := os.Stat(path); err == nil {
			return path
		}
	}
	return "examples/config/v2-basic.json"
}
