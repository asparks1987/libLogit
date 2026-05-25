[CmdletBinding()]
param(
    [switch]$SkipAlphaMatrix,
    [switch]$RequireCleanWorktree
)

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

function Invoke-RcStep {
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

function New-RcTempDirectory {
    $path = Join-Path ([IO.Path]::GetTempPath()) ("liblogit-rc-" + [Guid]::NewGuid().ToString("N"))
    New-Item -ItemType Directory -Path $path -Force | Out-Null
    return (Resolve-Path $path).Path
}

function Remove-RcTempDirectory {
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

function Resolve-ExecutableCandidate {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Candidate
    )

    if (Test-Path -LiteralPath $Candidate) {
        return (Resolve-Path -LiteralPath $Candidate).Path
    }

    $command = Get-Command $Candidate -ErrorAction SilentlyContinue
    if ($command) {
        return $command.Source
    }

    return $null
}

function Test-PythonPackageBuilder {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Python
    )

    & $Python -c "import setuptools, wheel" 2>$null
    return $LASTEXITCODE -eq 0
}

function Resolve-LibLogitBuildPython {
    $bundledPython = Join-Path $env:USERPROFILE ".cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe"
    $candidates = @(
        $env:LIBLOGIT_BUILD_PYTHON,
        $env:LIBLOGIT_PYTHON,
        $bundledPython,
        "python"
    ) | Where-Object { $_ }

    $seen = [System.Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)
    foreach ($candidate in $candidates) {
        $resolved = Resolve-ExecutableCandidate $candidate
        if (-not $resolved) {
            continue
        }
        if (-not $seen.Add($resolved)) {
            continue
        }
        if (Test-PythonPackageBuilder $resolved) {
            return $resolved
        }
    }

    throw "Python with setuptools and wheel was not found. Install packaging tooling or set LIBLOGIT_BUILD_PYTHON."
}

function Copy-CurrentTree {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Destination
    )

    $files = @(git ls-files --cached --others --exclude-standard)
    if ($LASTEXITCODE -ne 0 -or $files.Count -eq 0) {
        throw "Could not enumerate repository files with git ls-files."
    }

    foreach ($relative in $files) {
        $sourcePath = Join-Path $root $relative
        if (-not (Test-Path -LiteralPath $sourcePath -PathType Leaf)) {
            continue
        }

        $targetPath = Join-Path $Destination $relative
        $targetDirectory = Split-Path -Path $targetPath -Parent
        New-Item -ItemType Directory -Path $targetDirectory -Force | Out-Null
        Copy-Item -LiteralPath $sourcePath -Destination $targetPath -Force
    }
}

function Assert-TextContains {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,

        [Parameter(Mandatory = $true)]
        [string]$Pattern,

        [Parameter(Mandatory = $true)]
        [string]$Description
    )

    if (-not (Select-String -LiteralPath $Path -Pattern $Pattern -Quiet)) {
        throw "Package metadata check failed: $Description"
    }
}

function Assert-ZipEntryExists {
    param(
        [Parameter(Mandatory = $true)]
        [System.IO.Compression.ZipArchive]$Archive,

        [Parameter(Mandatory = $true)]
        [string]$EntryName,

        [Parameter(Mandatory = $true)]
        [string]$Description
    )

    if (-not $Archive.GetEntry($EntryName)) {
        throw "Package artifact check failed: $Description"
    }
}

function Read-ZipEntryText {
    param(
        [Parameter(Mandatory = $true)]
        [System.IO.Compression.ZipArchive]$Archive,

        [Parameter(Mandatory = $true)]
        [string]$EntryName
    )

    $entry = $Archive.GetEntry($EntryName)
    if (-not $entry) {
        throw "Package artifact check failed: missing zip entry $EntryName"
    }

    $stream = $entry.Open()
    try {
        $reader = [System.IO.StreamReader]::new($stream)
        try {
            return $reader.ReadToEnd()
        }
        finally {
            $reader.Dispose()
        }
    }
    finally {
        $stream.Dispose()
    }
}

function Assert-TextMatches {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Text,

        [Parameter(Mandatory = $true)]
        [string]$Pattern,

        [Parameter(Mandatory = $true)]
        [string]$Description
    )

    if ($Text -notmatch $Pattern) {
        throw "Package artifact check failed: $Description"
    }
}

if ($RequireCleanWorktree) {
    Invoke-RcStep "Clean worktree check" {
        $status = @(git status --porcelain=v1)
        if ($LASTEXITCODE -ne 0) {
            throw "Could not read git worktree status."
        }
        if ($status.Count -gt 0) {
            $status | ForEach-Object { Write-Host $_ }
            throw "Worktree is not clean. Commit or remove local changes before final RC verification."
        }
    }
}

if (-not $SkipAlphaMatrix) {
    Invoke-RcStep "Alpha verification matrix" {
        powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1
        if ($LASTEXITCODE -ne 0) {
            throw "Alpha verification matrix failed with exit code $LASTEXITCODE."
        }
    }
}
else {
    Invoke-RcStep "Static release gates" {
        powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1
        if ($LASTEXITCODE -ne 0) {
            throw "Static release gates failed with exit code $LASTEXITCODE."
        }
    }
}

Invoke-RcStep "Package metadata sanity" {
    Assert-TextContains "CMakeLists.txt" "VERSION 1\.0\.0" "native CMake package version"
    Assert-TextContains "setup.cfg" "^name = liblogit$" "Python package name"
    Assert-TextContains "setup.cfg" "^version = 1\.0\.0a1$" "Python package version"
    Assert-TextContains "languages/csharp/LibLogit/LibLogit.csproj" "<PackageId>LibLogit</PackageId>" "C# package id"
    Assert-TextContains "languages/csharp/LibLogit/LibLogit.csproj" "<Version>1\.0\.0-alpha\.1</Version>" "C# package version"
    Assert-TextContains "languages/javascript/package.json" '"name": "@liblogit/liblogit"' "npm package name"
    Assert-TextContains "languages/javascript/package.json" '"version": "1\.0\.0-alpha\.1"' "npm package version"
    Assert-TextContains "languages/java/pom.xml" "<artifactId>liblogit</artifactId>" "Java artifact id"
    Assert-TextContains "languages/java/pom.xml" "<version>1\.0\.0-alpha\.1</version>" "Java package version"
    Assert-TextContains "languages/kotlin/pom.xml" "<artifactId>liblogit-kotlin</artifactId>" "Kotlin artifact id"
    Assert-TextContains "languages/kotlin/pom.xml" "<version>1\.0\.0-alpha\.1</version>" "Kotlin package version"
    Assert-TextContains "languages/go/go.mod" "^module github\.com/asparks1987/liblogit/languages/go$" "Go module path"
}

Invoke-RcStep "JavaScript release artifact smoke" {
    node .\scripts\verify-javascript-package-smoke.js .\languages\javascript
    if ($LASTEXITCODE -ne 0) {
        throw "JavaScript release artifact smoke failed with exit code $LASTEXITCODE."
    }
}

Invoke-RcStep "C# NuGet artifact build and inspection" {
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $sourceCopy = New-RcTempDirectory
    $artifactOut = New-RcTempDirectory
    try {
        Copy-CurrentTree $sourceCopy
        $projectPath = Join-Path $sourceCopy "languages/csharp/LibLogit/LibLogit.csproj"
        dotnet pack $projectPath --configuration Release --output $artifactOut
        if ($LASTEXITCODE -ne 0) {
            throw "C# NuGet artifact build failed with exit code $LASTEXITCODE."
        }

        $packagePath = Join-Path $artifactOut "LibLogit.1.0.0-alpha.1.nupkg"
        if (-not (Test-Path -LiteralPath $packagePath -PathType Leaf)) {
            throw "Expected C# NuGet package was not created: $packagePath"
        }

        $archive = [System.IO.Compression.ZipFile]::OpenRead($packagePath)
        try {
            Assert-ZipEntryExists $archive "LibLogit.nuspec" "C# package nuspec"
            Assert-ZipEntryExists $archive "README.md" "C# package README"
            Assert-ZipEntryExists $archive "lib/net10.0/LibLogit.dll" "C# net10.0 assembly"
            $nuspec = Read-ZipEntryText $archive "LibLogit.nuspec"
            Assert-TextMatches $nuspec "<id>LibLogit</id>" "C# package id"
            Assert-TextMatches $nuspec "<version>1\.0\.0-alpha\.1</version>" "C# package version"
            Assert-TextMatches $nuspec "<license type=`"expression`">MIT</license>" "C# package license"
            Assert-TextMatches $nuspec "<readme>README\.md</readme>" "C# package readme"
            Assert-TextMatches $nuspec "<description>Alpha LOGIT logging object for \.NET projects\.</description>" "C# package description"
            Assert-TextMatches $nuspec "<group targetFramework=`"net10\.0`"\s*/>" "C# package target framework"
        }
        finally {
            $archive.Dispose()
        }

        Write-Host "Inspected LibLogit.1.0.0-alpha.1.nupkg."
    }
    finally {
        Remove-RcTempDirectory $sourceCopy
        Remove-RcTempDirectory $artifactOut
    }
}

Invoke-RcStep "Java Maven artifact build and inspection" {
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $sourceCopy = New-RcTempDirectory
    try {
        Copy-CurrentTree $sourceCopy
        $javaPath = Join-Path $sourceCopy "languages/java"
        Push-Location $javaPath
        try {
            mvn -B -DskipTests package
            if ($LASTEXITCODE -ne 0) {
                throw "Java Maven artifact build failed with exit code $LASTEXITCODE."
            }
        }
        finally {
            Pop-Location
        }

        $jarPath = Join-Path $javaPath "target/liblogit-1.0.0-alpha.1.jar"
        if (-not (Test-Path -LiteralPath $jarPath -PathType Leaf)) {
            throw "Expected Java Maven jar was not created: $jarPath"
        }

        $archive = [System.IO.Compression.ZipFile]::OpenRead($jarPath)
        try {
            Assert-ZipEntryExists $archive "dev/liblogit/Logit.class" "Java Logit class"
            Assert-ZipEntryExists $archive "dev/liblogit/LibLogIt.class" "Java compatibility facade class"
            Assert-ZipEntryExists $archive "META-INF/maven/dev.liblogit/liblogit/pom.xml" "Java embedded POM"
            Assert-ZipEntryExists $archive "META-INF/maven/dev.liblogit/liblogit/pom.properties" "Java Maven properties"
            $pom = Read-ZipEntryText $archive "META-INF/maven/dev.liblogit/liblogit/pom.xml"
            Assert-TextMatches $pom "<groupId>dev\.liblogit</groupId>" "Java package group id"
            Assert-TextMatches $pom "<artifactId>liblogit</artifactId>" "Java package artifact id"
            Assert-TextMatches $pom "<version>1\.0\.0-alpha\.1</version>" "Java package version"
            Assert-TextMatches $pom "<packaging>jar</packaging>" "Java package type"
            Assert-TextMatches $pom "<description>Alpha LOGIT logging object for Java projects\.</description>" "Java package description"
            $properties = Read-ZipEntryText $archive "META-INF/maven/dev.liblogit/liblogit/pom.properties"
            Assert-TextMatches $properties "(?m)^groupId=dev\.liblogit\r?$" "Java Maven properties group id"
            Assert-TextMatches $properties "(?m)^artifactId=liblogit\r?$" "Java Maven properties artifact id"
            Assert-TextMatches $properties "(?m)^version=1\.0\.0-alpha\.1\r?$" "Java Maven properties version"
        }
        finally {
            $archive.Dispose()
        }

        Write-Host "Inspected liblogit-1.0.0-alpha.1.jar."
    }
    finally {
        Remove-RcTempDirectory $sourceCopy
    }
}

Invoke-RcStep "Python release artifact build and inspection" {
    $buildPython = Resolve-LibLogitBuildPython
    $sourceCopy = New-RcTempDirectory
    $artifactDist = New-RcTempDirectory
    try {
        Copy-CurrentTree $sourceCopy
        Push-Location $sourceCopy
        try {
            & $buildPython setup.py sdist --dist-dir $artifactDist bdist_wheel --dist-dir $artifactDist
            if ($LASTEXITCODE -ne 0) {
                throw "Python release artifact build failed."
            }

            & $buildPython .\scripts\verify-python-wheel-smoke.py $artifactDist
            if ($LASTEXITCODE -ne 0) {
                throw "Python release artifact inspection or installed-wheel smoke test failed."
            }
        }
        finally {
            Pop-Location
        }
    }
    finally {
        Remove-RcTempDirectory $sourceCopy
        Remove-RcTempDirectory $artifactDist
    }
}

Write-Host ""
Write-Host "Local release-candidate verification passed."
