[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $root

$machinePath = [Environment]::GetEnvironmentVariable("Path", "Machine")
$userPath = [Environment]::GetEnvironmentVariable("Path", "User")
$toolPaths = @(
    "C:\Program Files\LLVM\bin",
    "C:\Program Files\CMake\bin",
    "C:\Program Files\Go\bin",
    "C:\Program Files\nodejs",
    "$env:USERPROFILE\.local\tools\apache-maven-3.9.15\bin",
    "$env:USERPROFILE\.local\tools\kotlinc\bin",
    "$env:USERPROFILE\.local\tools\vcpkg"
)
$env:Path = (@($machinePath, $userPath) + $toolPaths + @($env:Path)) -join ";"

function Resolve-LibLogitPython {
    if ($env:LIBLOGIT_PYTHON -and (Test-Path $env:LIBLOGIT_PYTHON)) {
        return (Resolve-Path $env:LIBLOGIT_PYTHON).Path
    }

    $pythonCommand = Get-Command python -ErrorAction SilentlyContinue
    if ($pythonCommand) {
        return $pythonCommand.Source
    }

    $bundledPython = Join-Path $env:USERPROFILE ".cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe"
    if (Test-Path $bundledPython) {
        return (Resolve-Path $bundledPython).Path
    }

    throw "Python was not found. Install Python or set LIBLOGIT_PYTHON to python.exe."
}

$Python = Resolve-LibLogitPython

function Invoke-AlphaStep {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Name,

        [Parameter(Mandatory = $true)]
        [scriptblock]$Command
    )

    Write-Host ""
    Write-Host "==> $Name"
    & $Command
}

function Invoke-CheckedNative {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Name,

        [Parameter(Mandatory = $true)]
        [scriptblock]$Command
    )

    & $Command
    if ($LASTEXITCODE -ne 0) {
        throw "$Name failed with exit code $LASTEXITCODE."
    }
}

function New-AlphaTempDirectory {
    $path = Join-Path ([IO.Path]::GetTempPath()) ("liblogit-alpha-" + [Guid]::NewGuid().ToString("N"))
    New-Item -ItemType Directory -Path $path -Force | Out-Null
    return (Resolve-Path $path).Path
}

function Remove-AlphaTempDirectory {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    $resolved = (Resolve-Path $Path).Path
    $tempRoot = (Resolve-Path ([IO.Path]::GetTempPath())).Path
    if (-not $resolved.StartsWith($tempRoot, [StringComparison]::OrdinalIgnoreCase)) {
        throw "Refusing to remove non-temp directory: $resolved"
    }
    Remove-Item -LiteralPath $resolved -Recurse -Force
}

Invoke-AlphaStep "Python tests" {
    $pytestTemp = New-AlphaTempDirectory
    $pytestCache = New-AlphaTempDirectory
    try {
        Invoke-CheckedNative "Python tests" {
            & $Python -m pytest `
                tests/test_python_logging.py `
                tests/test_python_conformance.py `
                tests/test_python_packaging.py `
                --basetemp $pytestTemp `
                -o "cache_dir=$pytestCache"
        }
    }
    finally {
        Remove-AlphaTempDirectory $pytestTemp
        Remove-AlphaTempDirectory $pytestCache
    }
}

Invoke-AlphaStep "Static checks" {
    Invoke-CheckedNative "Static checks" {
        powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1
    }
}

Invoke-AlphaStep "JavaScript tests" {
    $npmCache = New-AlphaTempDirectory
    try {
        Push-Location languages/javascript
        try {
            Invoke-CheckedNative "JavaScript tests" {
                node test/liblogit.test.js
            }
        }
        finally {
            Pop-Location
        }
        $npmCommand = if ($env:OS -eq "Windows_NT") { "npm.cmd" } else { "npm" }
        Push-Location languages/javascript
        try {
            Invoke-CheckedNative "npm pack dry run" {
                & $npmCommand pack --dry-run --cache $npmCache
            }
        }
        finally {
            Pop-Location
        }
        Invoke-CheckedNative "JavaScript installed package smoke" {
            node scripts/verify-javascript-package-smoke.js languages/javascript
        }
    }
    finally {
        Remove-AlphaTempDirectory $npmCache
    }
}

Invoke-AlphaStep "C# tests" {
    Invoke-CheckedNative "C# tests" {
        dotnet run --project languages/csharp/LibLogit.Tests/LibLogit.Tests.csproj
    }
    $nugetOut = New-AlphaTempDirectory
    try {
        Invoke-CheckedNative "C# NuGet pack" {
            dotnet pack languages/csharp/LibLogit/LibLogit.csproj --configuration Release --output $nugetOut
        }
    }
    finally {
        Remove-AlphaTempDirectory $nugetOut
    }
}

Invoke-AlphaStep "Go tests" {
    Push-Location languages/go
    try {
        Invoke-CheckedNative "Go tests" {
            go test ./...
        }
    }
    finally {
        Pop-Location
    }
}

Invoke-AlphaStep "Java tests" {
    $out = New-AlphaTempDirectory
    try {
        Invoke-CheckedNative "Java compile" {
            javac -d $out `
                languages/java/src/main/java/dev/liblogit/LibLogIt.java `
                languages/java/src/main/java/dev/liblogit/Logit.java `
                languages/java/src/test/java/dev/liblogit/LogitTest.java
        }
        Invoke-CheckedNative "Java tests" {
            java -cp $out dev.liblogit.LogitTest
        }
        Push-Location languages/java
        try {
            Invoke-CheckedNative "Java Maven install" {
                mvn -B -DskipTests install
            }
        }
        finally {
            Pop-Location
        }
    }
    finally {
        Remove-AlphaTempDirectory $out
    }
}

Invoke-AlphaStep "Kotlin tests" {
    $out = New-AlphaTempDirectory
    try {
        Invoke-CheckedNative "Kotlin Java dependency compile" {
            javac -d $out languages/java/src/main/java/dev/liblogit/Logit.java
        }
        Invoke-CheckedNative "Kotlin compile" {
            kotlinc `
                languages/kotlin/src/main/kotlin/dev/liblogit/LibLogItK.kt `
                languages/kotlin/src/test/kotlin/dev/liblogit/LibLogItKTest.kt `
                -classpath $out `
                -d $out
        }
        Invoke-CheckedNative "Kotlin tests" {
            kotlin -classpath $out dev.liblogit.LibLogItKTestKt
        }
        Push-Location languages/kotlin
        try {
            Invoke-CheckedNative "Kotlin Maven package" {
                mvn -B -DskipTests package
            }
        }
        finally {
            Pop-Location
        }
    }
    finally {
        Remove-AlphaTempDirectory $out
    }
}

Invoke-AlphaStep "C tests" {
    $out = Join-Path ([IO.Path]::GetTempPath()) ("liblogit-c-test-" + [Guid]::NewGuid().ToString("N") + ".exe")
    try {
        Invoke-CheckedNative "C compile" {
            clang -std=c17 -Wall -Wextra languages/c/liblogit.c languages/c/test_logit.c -o $out
        }
        Invoke-CheckedNative "C tests" {
            & $out
        }
    }
    finally {
        if (Test-Path $out) {
            Remove-Item -LiteralPath $out -Force
        }
    }
}

Invoke-AlphaStep "C++ tests" {
    $out = Join-Path ([IO.Path]::GetTempPath()) ("liblogit-cpp-test-" + [Guid]::NewGuid().ToString("N") + ".exe")
    $includeArgs = @()
    $vcpkgInclude = Join-Path $env:USERPROFILE ".local\tools\vcpkg\installed\x64-windows\include"
    if (Test-Path $vcpkgInclude) {
        $includeArgs += "-I$vcpkgInclude"
    }
    try {
        Invoke-CheckedNative "C++ compile" {
            & clang++ -std=c++20 -Wall -Wextra @includeArgs languages/cpp/test_logit.cpp -o $out
        }
        Invoke-CheckedNative "C++ tests" {
            & $out
        }
    }
    finally {
        if (Test-Path $out) {
            Remove-Item -LiteralPath $out -Force
        }
    }
}

Invoke-AlphaStep "Native CMake build, tests, and install" {
    $build = New-AlphaTempDirectory
    $install = New-AlphaTempDirectory
    $cExample = Join-Path $install "c-basic-installed.exe"
    $cppExample = Join-Path $install "cpp-basic-installed.exe"
    $vcpkgRoot = Join-Path $env:USERPROFILE ".local\tools\vcpkg\installed\x64-windows"
    $vcpkgInclude = Join-Path $vcpkgRoot "include"
    try {
        Invoke-CheckedNative "Native CMake configure" {
            cmake -S $root -B $build `
                -G Ninja `
                -DCMAKE_C_COMPILER=clang `
                -DCMAKE_CXX_COMPILER=clang++ `
                "-DCMAKE_PREFIX_PATH=$vcpkgRoot"
        }
        Invoke-CheckedNative "Native CMake build" {
            cmake --build $build
        }
        Invoke-CheckedNative "Native CTest" {
            ctest --test-dir $build --output-on-failure
        }
        Invoke-CheckedNative "Native CMake install" {
            cmake --install $build --prefix $install
        }

        $installedInclude = Join-Path $install "include"
        $installedLib = Join-Path $install "lib\logit.lib"
        Invoke-CheckedNative "Installed C example compile" {
            clang -std=c17 -Wall -Wextra -I $installedInclude examples/c/basic.c $installedLib -o $cExample
        }
        Invoke-CheckedNative "Installed C++ example compile" {
            clang++ -std=c++20 -Wall -Wextra -I $installedInclude -I $vcpkgInclude examples/cpp/basic.cpp -o $cppExample
        }

        $consumer = New-AlphaTempDirectory
        try {
            $consumerBuild = Join-Path $consumer "build"
            @"
cmake_minimum_required(VERSION 3.20)
project(liblogit_consumer LANGUAGES C CXX)
find_package(libLogit CONFIG REQUIRED)
add_executable(c_consumer c_consumer.c)
target_link_libraries(c_consumer PRIVATE libLogit::c)
add_executable(cpp_consumer cpp_consumer.cpp)
target_link_libraries(cpp_consumer PRIVATE libLogit::cpp)
"@ | Set-Content -LiteralPath (Join-Path $consumer "CMakeLists.txt") -Encoding UTF8
            @"
#include <liblogit/logit.h>
int main(void) {
    liblogit_logit logit = liblogit_logit_default();
    liblogit_logit_set_timestamp(&logit, 0);
    return liblogit_logit_log(&logit, LIBLOGIT_INFO, "consumer c log");
}
"@ | Set-Content -LiteralPath (Join-Path $consumer "c_consumer.c") -Encoding UTF8
            @"
#include <liblogit/logit.hpp>
int main() {
    liblogit::LOGIT logit;
    logit.timestamp = false;
    logit(liblogit::Level::INFO) << "consumer cpp log";
    return 0;
}
"@ | Set-Content -LiteralPath (Join-Path $consumer "cpp_consumer.cpp") -Encoding UTF8

            Invoke-CheckedNative "Installed CMake consumer configure" {
                cmake -S $consumer -B $consumerBuild `
                    -G Ninja `
                    -DCMAKE_C_COMPILER=clang `
                    -DCMAKE_CXX_COMPILER=clang++ `
                    "-DCMAKE_PREFIX_PATH=$install;$vcpkgRoot"
            }
            Invoke-CheckedNative "Installed CMake consumer build" {
                cmake --build $consumerBuild
            }
            Invoke-CheckedNative "Installed C consumer run" {
                & (Join-Path $consumerBuild "c_consumer.exe")
            }
            Invoke-CheckedNative "Installed C++ consumer run" {
                & (Join-Path $consumerBuild "cpp_consumer.exe")
            }
        }
        finally {
            Remove-AlphaTempDirectory $consumer
        }
    }
    finally {
        Remove-AlphaTempDirectory $build
        Remove-AlphaTempDirectory $install
    }
}

Write-Host ""
Write-Host "Alpha verification matrix passed."
