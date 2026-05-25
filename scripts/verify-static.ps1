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

function Invoke-StaticStep {
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

$Python = Resolve-LibLogitPython

Invoke-StaticStep "PowerShell syntax check" {
    $scripts = @(Get-ChildItem -Path scripts -Filter "*.ps1" -File | Sort-Object FullName)
    if ($scripts.Count -eq 0) {
        throw "No PowerShell scripts were found."
    }

    $parseFailures = [System.Collections.Generic.List[string]]::new()
    foreach ($script in $scripts) {
        $tokens = $null
        $errors = $null
        [System.Management.Automation.Language.Parser]::ParseFile(
            $script.FullName,
            [ref]$tokens,
            [ref]$errors
        ) | Out-Null

        foreach ($parseError in @($errors)) {
            $parseFailures.Add(
                "$($script.FullName):$($parseError.Extent.StartLineNumber): $($parseError.Message)"
            )
        }
    }

    if ($parseFailures.Count -gt 0) {
        $parseFailures | ForEach-Object { Write-Host $_ }
        throw "PowerShell syntax check failed."
    }
}

Invoke-StaticStep "JavaScript syntax check" {
    $node = Get-Command node -ErrorAction SilentlyContinue
    if (-not $node) {
        throw "node was not found. Install Node.js or add node to PATH."
    }

    $scripts = @(
        "scripts/verify-javascript-package-smoke.js",
        "languages/javascript/src/liblogit.js",
        "languages/javascript/test/liblogit.test.js"
    )
    foreach ($script in $scripts) {
        & $node.Source --check $script
        if ($LASTEXITCODE -ne 0) {
            throw "JavaScript syntax check failed for $script."
        }
    }
}

Invoke-StaticStep "Hosted workflow release gate check" {
    $workflowPath = ".github/workflows/alpha-matrix.yml"
    $helperPath = "scripts/check-hosted-ci.ps1"
    if (-not (Test-Path -LiteralPath $workflowPath)) {
        throw "Hosted Alpha workflow was not found at $workflowPath."
    }
    if (-not (Test-Path -LiteralPath $helperPath)) {
        throw "Hosted CI helper was not found at $helperPath."
    }

    $workflow = Get-Content -LiteralPath $workflowPath -Raw
    $helper = Get-Content -LiteralPath $helperPath -Raw
    $requiredJobs = @(
        "Schema and config examples",
        "Static checks",
        "Python",
        "JavaScript",
        "C#",
        "Go",
        "Java",
        "Kotlin",
        "C",
        "C++",
        "Native CMake package"
    )

    foreach ($jobName in $requiredJobs) {
        $escaped = [regex]::Escape($jobName)
        if ($workflow -notmatch "(?m)^\s+name:\s+$escaped\s*$") {
            throw "Hosted Alpha workflow is missing required job name '$jobName'."
        }
        if ($helper -notmatch [regex]::Escape("""$jobName""")) {
            throw "Hosted CI helper is missing required job '$jobName'."
        }
    }

    $requiredWorkflowText = @(
        "actions/setup-node@v4",
        "Check JavaScript syntax",
        "node --check scripts/verify-javascript-package-smoke.js",
        "node --check languages/javascript/src/liblogit.js",
        "node --check languages/javascript/test/liblogit.test.js",
        "python scripts/verify-python-wheel-smoke.py dist",
        "node scripts/verify-javascript-package-smoke.js languages/javascript"
    )
    foreach ($text in $requiredWorkflowText) {
        if (-not $workflow.Contains($text)) {
            throw "Hosted Alpha workflow is missing release gate text: $text"
        }
    }
}

Invoke-StaticStep "Python syntax check" {
    & $Python -m compileall -q liblogit tests scripts
    if ($LASTEXITCODE -ne 0) {
        throw "Python compileall failed."
    }
}

Invoke-StaticStep "Python lint check" {
    & $Python -m ruff check liblogit tests scripts
    if ($LASTEXITCODE -ne 0) {
        throw "ruff check failed."
    }
}

Invoke-StaticStep "Python type check" {
    & $Python -m mypy --explicit-package-bases liblogit tests scripts
    if ($LASTEXITCODE -ne 0) {
        throw "mypy check failed."
    }
}

Invoke-StaticStep "Whitespace diff check" {
    git diff --check -- .
    if ($LASTEXITCODE -ne 0) {
        throw "git diff --check failed."
    }
}

Invoke-StaticStep "Schema and config validation" {
    $code = @'
import json
import os
from pathlib import Path

from liblogit import LogConfigurationError
from liblogit import _Config

for key in list(os.environ):
    if key.startswith("LIBLOGIT_"):
        del os.environ[key]

root = Path.cwd()


def read_json(path):
    return json.loads(path.read_text(encoding="utf-8"))


schema_paths = {
    "v1": root / "schema" / "logit.v1.schema.json",
    "v2": root / "schema" / "logit.v2.schema.json",
}
for path in schema_paths.values():
    read_json(path)

valid_v1 = [
    root / "examples" / "config" / "v1-legacy.json",
    root / "liblogit" / "data" / "logit.v1.sample.json",
]
valid_v2 = [
    root / "examples" / "config" / "v2-basic.json",
    root / "examples" / "config" / "v2-buffering.json",
    root / "examples" / "config" / "v2-database.json",
    root / "examples" / "config" / "v2-directory-path.json",
    root / "examples" / "config" / "v2-failure-policy.json",
    root / "examples" / "config" / "v2-redaction.json",
    root / "examples" / "config" / "v2-rotation.json",
    root / "examples" / "config" / "v2-multi-logit.json",
    root / "liblogit" / "data" / "logit.sample.json",
]
invalid_v2 = root / "examples" / "config" / "invalid-extra-key.json"


def parser_validate(payload):
    if "logits" not in payload:
        _Config.from_dict(payload)
        return

    extra = set(payload.keys()) - {"version", "defaults", "logits"}
    if extra:
        raise LogConfigurationError(f"Unsupported configuration keys: {sorted(extra)}")
    defaults = payload.get("defaults", {})
    if not isinstance(defaults, dict):
        raise LogConfigurationError("defaults must be an object")
    logits = payload.get("logits")
    if not isinstance(logits, dict) or not logits:
        raise LogConfigurationError("logits must be a non-empty object")
    for name, logit_payload in logits.items():
        if not isinstance(logit_payload, dict):
            raise LogConfigurationError(f"LOGIT '{name}' must be an object")
        _Config.from_logit_dict(logit_payload, name=name, defaults=defaults)


try:
    from jsonschema import ValidationError
    from jsonschema.validators import validator_for
except ModuleNotFoundError:
    for path in valid_v1 + valid_v2:
        parser_validate(read_json(path))
    for fixture_path in sorted((root / "tests" / "conformance" / "fixtures").glob("*.json")):
        parser_validate(read_json(fixture_path)["config"])
    try:
        parser_validate(read_json(invalid_v2))
    except LogConfigurationError:
        pass
    else:
        raise AssertionError("invalid-extra-key.json unexpectedly passed parser validation")
    print("jsonschema is not installed; parser-backed config validation passed")
else:
    validators = {}
    for name, path in schema_paths.items():
        schema = read_json(path)
        cls = validator_for(schema)
        cls.check_schema(schema)
        validators[name] = cls(schema)

    for path in valid_v1:
        validators["v1"].validate(read_json(path))
    for path in valid_v2:
        validators["v2"].validate(read_json(path))
    for fixture_path in sorted((root / "tests" / "conformance" / "fixtures").glob("*.json")):
        validators["v2"].validate(read_json(fixture_path)["config"])
    try:
        validators["v2"].validate(read_json(invalid_v2))
    except ValidationError:
        pass
    else:
        raise AssertionError("invalid-extra-key.json unexpectedly passed JSON Schema validation")
    print("JSON Schema validation passed")
'@
    $code | & $Python -
    if ($LASTEXITCODE -ne 0) {
        throw "Schema and config validation failed."
    }
}

Invoke-StaticStep "C and C++ clang-format check" {
    $clangFormat = Get-Command clang-format -ErrorAction SilentlyContinue
    if (-not $clangFormat) {
        throw "clang-format was not found. Install LLVM or add clang-format to PATH."
    }

    $files = @(rg --files -g "*.c" -g "*.h" -g "*.hpp" -g "*.cpp" languages include examples tests | Sort-Object)
    if ($files.Count -eq 0) {
        throw "No C/C++ files were found for clang-format."
    }
    & $clangFormat.Source --dry-run --Werror @files
    if ($LASTEXITCODE -ne 0) {
        throw "clang-format check failed."
    }
}

Write-Host ""
Write-Host "Static verification passed."
