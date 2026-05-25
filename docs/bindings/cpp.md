# C++ Alpha Binding

The C++ Alpha binding is header-only and keeps the v0.1-compatible root
`libLogit.h` available for existing users. New installed consumers should prefer
the public include wrapper and CMake target:

```cpp
#include <liblogit/logit.hpp>
```

```cmake
find_package(libLogit CONFIG REQUIRED)
target_link_libraries(my_app PRIVATE libLogit::cpp)
```

## Layout

| Path | Purpose |
|------|---------|
| `libLogit.h` | Compatibility and source-vendoring header for current C++ users. |
| `include/liblogit/logit.hpp` | Preferred installed include wrapper. |
| `include/liblogit/cpp/libLogit.h` | Compatibility include path under the installed include tree. |
| `languages/cpp/test_logit.cpp` | C++ direct, config, JSON, and shared-fixture verification. |
| `examples/cpp/basic.cpp` | Direct and config-loaded quick start. |
| `CMakeLists.txt` | Defines `libLogit::cpp` as an interface target. |

## Migration Path

Existing vendored users can keep:

```cpp
#include "libLogit.h"
```

When moving to an installed package, change the include and link the exported
target:

```cpp
#include <liblogit/logit.hpp>
```

No namespace or object-model change is required. `liblogit::LOGIT`,
`liblogit::Level`, `LOGIT::load_from_file(...)`, and object-bound streaming keep
the same semantics.

## Dependency

Config loading uses `nlohmann/json.hpp`. The local Alpha verifier uses the
vcpkg package and the generated CMake package config calls
`find_dependency(nlohmann_json CONFIG)` for installed consumers.

## Verification

The local Alpha verifier checks the source-tree and installed-consumer paths:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1
```

That matrix builds C++ tests and examples, runs shared fixtures, installs the
CMake package, and builds a consumer with `find_package(libLogit CONFIG
REQUIRED)` linked against `libLogit::cpp`.
