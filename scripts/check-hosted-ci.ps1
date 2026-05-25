[CmdletBinding()]
param(
    [string]$Workflow = "Alpha Matrix",
    [string]$Branch = "",
    [string]$HeadSha = "",
    [int]$RunLimit = 10,
    [string[]]$RequiredJobs = @(
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
    --limit $RunLimit `
    --json databaseId,headSha,status,conclusion,url,createdAt
if ($LASTEXITCODE -ne 0) {
    throw "Unable to read hosted GitHub Actions runs for workflow '$Workflow' on branch '$Branch'."
}

$runs = $runJson | ConvertFrom-Json
if (-not $runs -or $runs.Count -eq 0) {
    throw "No hosted GitHub Actions runs found for workflow '$Workflow' on branch '$Branch'."
}

$runCandidates = @($runs)
if ($HeadSha) {
    $runCandidates = @($runCandidates | Where-Object { $_.headSha -eq $HeadSha })
    if ($runCandidates.Count -eq 0) {
        throw "No hosted GitHub Actions runs found for workflow '$Workflow' on branch '$Branch' at head SHA '$HeadSha'."
    }
}

$run = $runCandidates[0]
if ($run.status -ne "completed") {
    throw "Latest hosted CI run is not complete. Status: $($run.status). URL: $($run.url)"
}
if ($run.conclusion -ne "success") {
    throw "Latest hosted CI run did not succeed. Conclusion: $($run.conclusion). URL: $($run.url)"
}

$runDetailsJson = gh run view $run.databaseId --json conclusion,headSha,jobs,status,url
if ($LASTEXITCODE -ne 0) {
    throw "Unable to read hosted GitHub Actions job details for run $($run.databaseId)."
}

$runDetails = $runDetailsJson | ConvertFrom-Json
if ($runDetails.status -ne "completed") {
    throw "Hosted CI run details are not complete. Status: $($runDetails.status). URL: $($runDetails.url)"
}
if ($runDetails.conclusion -ne "success") {
    throw "Hosted CI run details did not succeed. Conclusion: $($runDetails.conclusion). URL: $($runDetails.url)"
}
if ($HeadSha -and $runDetails.headSha -ne $HeadSha) {
    throw "Hosted CI run head SHA mismatch. Expected '$HeadSha' but saw '$($runDetails.headSha)'."
}

$jobs = @($runDetails.jobs)
$jobsByName = @{}
foreach ($job in $jobs) {
    $jobsByName[$job.name] = $job
}

$missingJobs = @($RequiredJobs | Where-Object { -not $jobsByName.ContainsKey($_) })
if ($missingJobs.Count -gt 0) {
    throw "Hosted CI run is missing required jobs: $($missingJobs -join ', ')"
}

foreach ($name in $RequiredJobs) {
    $job = $jobsByName[$name]
    if ($job.status -ne "completed") {
        throw "Hosted CI job '$name' is not complete. Status: $($job.status). URL: $($job.url)"
    }
    if ($job.conclusion -ne "success") {
        throw "Hosted CI job '$name' did not succeed. Conclusion: $($job.conclusion). URL: $($job.url)"
    }
}

Write-Host "Hosted CI passed for workflow '$Workflow' on branch '$Branch'."
Write-Host "Run: $($runDetails.url)"
Write-Host "Head SHA: $($runDetails.headSha)"
Write-Host "Required jobs passed: $($RequiredJobs -join ', ')"
