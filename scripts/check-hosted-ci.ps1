[CmdletBinding()]
param(
    [string]$Workflow = "Alpha Matrix",
    [string]$Branch = ""
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$root = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $root

if (-not $Branch) {
    $Branch = (git branch --show-current).Trim()
}
if (-not $Branch) {
    throw "Could not determine the current Git branch. Pass -Branch explicitly."
}

$gh = Get-Command gh -ErrorAction SilentlyContinue
if (-not $gh) {
    throw "GitHub CLI was not found. Install gh and authenticate before checking hosted CI."
}

gh auth status | Out-Null
if ($LASTEXITCODE -ne 0) {
    throw "GitHub CLI is not authenticated. Run 'gh auth login' before checking hosted CI."
}

$runJson = gh run list `
    --workflow $Workflow `
    --branch $Branch `
    --limit 1 `
    --json databaseId,headSha,status,conclusion,url,createdAt
if ($LASTEXITCODE -ne 0) {
    throw "Unable to read hosted GitHub Actions runs for workflow '$Workflow' on branch '$Branch'."
}

$runs = $runJson | ConvertFrom-Json
if (-not $runs -or $runs.Count -eq 0) {
    throw "No hosted GitHub Actions runs found for workflow '$Workflow' on branch '$Branch'."
}

$run = @($runs)[0]
if ($run.status -ne "completed") {
    throw "Latest hosted CI run is not complete. Status: $($run.status). URL: $($run.url)"
}
if ($run.conclusion -ne "success") {
    throw "Latest hosted CI run did not succeed. Conclusion: $($run.conclusion). URL: $($run.url)"
}

Write-Host "Hosted CI passed for workflow '$Workflow' on branch '$Branch'."
Write-Host "Run: $($run.url)"
Write-Host "Head SHA: $($run.headSha)"
