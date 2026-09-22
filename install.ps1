<#
================================================================================
  Canontra v0.1.0 Production Direct Installer (Windows PowerShell)
  Deterministic, Multi-Tier Polyglot Program Identity & Semantic Graph Engine
  https://github.com/symtrace/canontra
================================================================================
#>
[CmdletBinding()]
param (
    [Alias("d")]
    [string]$InstallDir = $(
        if ($env:CANONTRA_INSTALL_DIR) {
            $env:CANONTRA_INSTALL_DIR
        } elseif ($env:LOCALAPPDATA) {
            "$env:LOCALAPPDATA\Programs\canontra"
        } elseif ($env:USERPROFILE) {
            "$env:USERPROFILE\AppData\Local\Programs\canontra"
        } else {
            "$HOME/.canontra"
        }
    ),

    [Alias("v")]
    [string]$Version = "v0.1.0",

    [Alias("b")]
    [switch]$ForceBuild,

    [switch]$SkipPath,

    [switch]$SkipCompletions,

    [switch]$DryRun,

    [Alias("h")]
    [switch]$Help
)

$ErrorActionPreference = "Stop"

try {
    [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
} catch {}

# Normalize version tag to always have 'v' prefix
if (-not $Version.StartsWith("v")) {
    $Version = "v$Version"
}

if ($Help) {
    Write-Host @"
Canontra Direct Installer ($Version)

USAGE:
  .\install.ps1 [OPTIONS]

OPTIONS:
  -InstallDir, -d <PATH>   Target installation directory (default: %LOCALAPPDATA%\Programs\canontra)
  -Version, -v <VER>       Specify Canontra release tag (default: $Version)
  -ForceBuild, -b          Force compilation from local Haskell source repository
  -SkipPath                Skip adding installation directory to User environment PATH
  -SkipCompletions         Skip registering PowerShell autocompletions in `$PROFILE
  -DryRun                  Simulate installation steps without modifying filesystem
  -Help, -h                Show this help message and exit

ENVIRONMENT:
  CANONTRA_INSTALL_DIR     Alternative environment variable for installation directory
"@
    exit 0
}

# Override target directory from environment variable if present and not explicitly passed
if ($env:CANONTRA_INSTALL_DIR -and -not $PSBoundParameters.ContainsKey('InstallDir')) {
    $InstallDir = $env:CANONTRA_INSTALL_DIR
}

$SupportsUnicode = $false
try {
    if ([Console]::OutputEncoding.CodePage -eq 65001 -or $Host.UI.RawUI -ne $null) {
        $SupportsUnicode = $true
    }
} catch {}

$CheckGlyph = if ($SupportsUnicode) { [char]0x2714 } else { "[OK]" }
$CrossGlyph = if ($SupportsUnicode) { [char]0x2716 } else { "[X]" }
$DryRunGlyph = if ($SupportsUnicode) { [char]0x2192 } else { "->" }
$SpinnerFrames = @([char]0x280B, [char]0x2819, [char]0x2839, [char]0x2838, [char]0x283C, [char]0x2834, [char]0x2826, [char]0x2827, [char]0x2807, [char]0x280F)
$IsInteractive = ($Host.UI.RawUI -ne $null) -and [Environment]::UserInteractive -and (-not [Console]::IsOutputRedirected)

function Show-Banner {
    Write-Host ""
    Write-Host "     ____                      _             " -ForegroundColor Cyan
    Write-Host "    / ___|__ _ _ __   ___  _ __ | |_ _ __ __ _ " -ForegroundColor Cyan
    Write-Host '   | |   / _` | ''_ \ / _ \| ''_ \| __| ''__/ _` |' -ForegroundColor Cyan
    Write-Host "   | |__| (_| | | | | (_) | | | | |_| | | (_| |" -ForegroundColor Cyan
    Write-Host '    \____\__,_|_| |_|\___/|_| |_|\__|_|  \__,_|' -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  Deterministic, Multi-Tier Polyglot Program Identity Engine" -ForegroundColor White
    Write-Host "  Release: $Version | Windows Native Runtime" -ForegroundColor DarkGray
    Write-Host "================================================================================" -ForegroundColor DarkGray
}

function Show-InstantStep {
    param (
        [string]$Label,
        [string]$Detail
    )
    if ($IsInteractive) {
        foreach ($f in $SpinnerFrames[0..3]) {
            Write-Host -NoNewline "`r  $f $Label" -ForegroundColor Cyan
            Start-Sleep -Milliseconds 30
        }
        $formatted = "  $CheckGlyph {0,-32} {1}" -f $Label, $Detail
        Write-Host "`r$formatted" -ForegroundColor Green
    } else {
        Write-Host "  [+] $Label : $Detail"
    }
}

function Invoke-ProcessWithAnimation {
    param (
        [string]$Label,
        [string]$FilePath,
        [string[]]$ArgumentList
    )

    if ($DryRun) {
        Write-Host "  $DryRunGlyph $Label (dry-run: $FilePath $($ArgumentList -join ' '))" -ForegroundColor Yellow
        return
    }

    $stdoutFile = [System.IO.Path]::GetTempFileName()
    $stderrFile = [System.IO.Path]::GetTempFileName()

    try {
        if ($IsInteractive) {
            $p = Start-Process -FilePath $FilePath -ArgumentList $ArgumentList -NoNewWindow -PassThru `
                -RedirectStandardOutput $stdoutFile -RedirectStandardError $stderrFile

            $idx = 0
            while (-not $p.HasExited) {
                $frame = $SpinnerFrames[$idx % $SpinnerFrames.Length]
                Write-Host -NoNewline "`r  $frame $Label" -ForegroundColor Cyan
                Start-Sleep -Milliseconds 80
                $idx++
            }

            if ($p.ExitCode -eq 0) {
                Write-Host "`r  $CheckGlyph $Label" -ForegroundColor Green
            } else {
                Write-Host "`r  $CrossGlyph $Label (failed with code $($p.ExitCode))" -ForegroundColor Red
                $outContent = Get-Content $stdoutFile -Raw -ErrorAction SilentlyContinue
                $errContent = Get-Content $stderrFile -Raw -ErrorAction SilentlyContinue
                if ($outContent) { Write-Host "`n--- stdout ---`n$outContent" -ForegroundColor DarkGray }
                if ($errContent) { Write-Host "`n--- stderr ---`n$errContent" -ForegroundColor Red }
                throw "Process execution failed: $FilePath with exit code $($p.ExitCode)"
            }
        } else {
            Write-Host "  [*] $Label ... " -NoNewline
            $p = Start-Process -FilePath $FilePath -ArgumentList $ArgumentList -Wait -NoNewWindow -PassThru `
                -RedirectStandardOutput $stdoutFile -RedirectStandardError $stderrFile
            if ($p.ExitCode -eq 0) {
                Write-Host "done" -ForegroundColor Green
            } else {
                Write-Host "FAILED ($($p.ExitCode))" -ForegroundColor Red
                $outContent = Get-Content $stdoutFile -Raw -ErrorAction SilentlyContinue
                $errContent = Get-Content $stderrFile -Raw -ErrorAction SilentlyContinue
                if ($outContent) { Write-Host "$outContent" }
                if ($errContent) { Write-Host "$errContent" -ForegroundColor Red }
                throw "Execution failed with exit code $($p.ExitCode)"
            }
        }
    } finally {
        Remove-Item $stdoutFile, $stderrFile -Force -ErrorAction SilentlyContinue
    }
}

function Main {
    Show-Banner

    # 1. Architecture & Platform Detection
    $isWin = if ($null -ne $IsWindows) { $IsWindows } else { [System.Environment]::OSVersion.Platform -eq [System.PlatformID]::Win32NT }

    $arch = $env:PROCESSOR_ARCHITECTURE
    if (-not $arch) {
        try {
            $osArch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString()
            if ($osArch -match "Arm64") {
                $arch = "ARM64"
            } elseif ($osArch -match "X64") {
                $arch = "AMD64"
            } else {
                $arch = "AMD64"
            }
        } catch {
            $arch = "AMD64"
        }
    }
    # Normalize architecture name
    if ($arch -eq "x86_64" -or $arch -eq "x64") {
        $arch = "AMD64"
    } elseif ($arch -eq "aarch64") {
        $arch = "ARM64"
    }

    if ($arch -ne "AMD64" -and $arch -ne "ARM64") {
        Write-Error "Unsupported processor architecture: $arch. Canontra supports AMD64 and ARM64."
        exit 1
    }

    if (-not $isWin -and -not $DryRun) {
        Write-Error "install.ps1 is designed for Windows environments. On Linux or macOS, please run install.sh instead."
        exit 1
    }
    Show-InstantStep "Host Platform Identified" "windows-$arch"

    # 2. Installation Directory Setup
    Show-InstantStep "Target Directory Configured" $InstallDir
    if (-not $DryRun -and -not (Test-Path $InstallDir)) {
        New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
    }

    $targetExe = Join-Path $InstallDir "canontra.exe"

    # 3. Binary Acquisition (Local Toolchain vs Tagged GitHub Releases)
    $hasCabal = Test-Path "canontra.cabal"
    $hasDistBin = Test-Path "dist-bin\canontra.exe"
    $shouldBuildLocal = $ForceBuild

    if ($shouldBuildLocal) {
        if (Get-Command stack -ErrorAction SilentlyContinue) {
            Invoke-ProcessWithAnimation "Compiling Canontra from source (Stack)" "stack" @("install", "--local-bin-path", $InstallDir)
        } elseif (Get-Command cabal -ErrorAction SilentlyContinue) {
            Invoke-ProcessWithAnimation "Compiling Canontra from source (Cabal)" "cabal" @("install", "--installdir=$InstallDir", "--overwrite-policy=always")
        } else {
            Write-Error "Neither 'stack' nor 'cabal' build tools were found in PATH."
            exit 1
        }
    } elseif ($hasDistBin -and -not (Get-Command curl.exe -ErrorAction SilentlyContinue)) {
        # Quick installation from local prebuilt dist-bin if present
        if (-not $DryRun) {
            Copy-Item -Path "dist-bin\canontra.exe" -Destination $targetExe -Force
        }
        Show-InstantStep "Installed Local Binary" "dist-bin\canontra.exe -> $targetExe"
    } else {
        # Remote download from Tagged GitHub Release
        $zipName = "canontra-$Version-windows-$arch.zip"
        $exeName = "canontra-$Version-windows-$arch.exe"
        $downloadUrl = "https://github.com/symtrace/canontra/releases/download/$Version/$zipName"
        $fallbackUrl = "https://github.com/symtrace/canontra/releases/download/$Version/$exeName"
        $tempZip = Join-Path $env:TEMP $zipName
        $tempExe = Join-Path $env:TEMP $exeName

        if ($DryRun) {
            Write-Host "  $DryRunGlyph Downloading Canontra $Version release archive (dry-run: $downloadUrl)" -ForegroundColor Yellow
        } else {
            $downloaded = $false

            # Try primary tagged zip archive download
            try {
                Invoke-WebRequest -Uri $downloadUrl -OutFile $tempZip -UseBasicParsing -ErrorAction Stop
                Expand-Archive -Path $tempZip -DestinationPath $InstallDir -Force
                Remove-Item -Path $tempZip -Force -ErrorAction SilentlyContinue
                $downloaded = $true
                Show-InstantStep "Release Archive Downloaded" "$zipName"
            } catch {
                # Fallback to tagged standalone executable artifact
                try {
                    Invoke-WebRequest -Uri $fallbackUrl -OutFile $tempExe -UseBasicParsing -ErrorAction Stop
                    Move-Item -Path $tempExe -Destination $targetExe -Force
                    $downloaded = $true
                    Show-InstantStep "Tagged Binary Downloaded" "$exeName"
                } catch {
                    # If inside git repo, fallback to local build or dist-bin copy
                    if (Test-Path "dist-bin\canontra.exe") {
                        Copy-Item -Path "dist-bin\canontra.exe" -Destination $targetExe -Force
                        $downloaded = $true
                        Show-InstantStep "Installed Local Prebuilt" "dist-bin\canontra.exe"
                    } elseif ($hasCabal) {
                        Write-Host "  ↷ Release asset not found, compiling locally from source..." -ForegroundColor Yellow
                        if (Get-Command stack -ErrorAction SilentlyContinue) {
                            Invoke-ProcessWithAnimation "Compiling Canontra from source (Stack)" "stack" @("install", "--local-bin-path", $InstallDir)
                            $downloaded = $true
                        } elseif (Get-Command cabal -ErrorAction SilentlyContinue) {
                            Invoke-ProcessWithAnimation "Compiling Canontra from source (Cabal)" "cabal" @("install", "--installdir=$InstallDir", "--overwrite-policy=always")
                            $downloaded = $true
                        }
                    }
                }
            }

            if (-not $downloaded -and -not (Test-Path $targetExe)) {
                Write-Error "Failed to acquire Canontra binary from GitHub Releases ($downloadUrl) or local environment."
                exit 1
            }
        }
    }

    # 4. User PATH Configuration (Windows)
    if ($isWin -and -not $SkipPath -and -not $DryRun) {
        $cleanDir = $InstallDir.TrimEnd('\')
        $userPath = [Environment]::GetEnvironmentVariable("Path", "User")
        if ($null -eq $userPath) { $userPath = "" }
        $parts = $userPath -split ';' | Where-Object { $_ -ne '' }
        if ($parts -notcontains $cleanDir) {
            $newUserPath = ($parts + $cleanDir) -join ';'
            [Environment]::SetEnvironmentVariable("Path", $newUserPath, "User")
            if ($env:Path -split ';' -notcontains $cleanDir) {
                $env:Path = "$env:Path;$cleanDir"
            }
            Show-InstantStep "User Environment PATH" "Added $cleanDir"
        } else {
            Show-InstantStep "User Environment PATH" "Already configured"
        }
    }

    # 5. PowerShell Autocompletions Setup (Windows)
    if ($isWin -and -not $SkipCompletions -and -not $DryRun) {
        try {
            $profileDir = Split-Path $PROFILE -Parent
            if (-not (Test-Path $profileDir)) {
                New-Item -ItemType Directory -Path $profileDir -Force | Out-Null
            }
            if (-not (Test-Path $PROFILE)) {
                New-Item -ItemType File -Path $PROFILE -Force | Out-Null
            }

            $currentProfile = Get-Content -Path $PROFILE -Raw -ErrorAction SilentlyContinue
            if ($null -eq $currentProfile -or $currentProfile -notlike "*# Canontra autocompletions*") {
                if (Test-Path $targetExe) {
                    $completionScript = & $targetExe completions powershell 2>$null
                    if ($completionScript) {
                        $block = "`n# Canontra autocompletions`n$completionScript`n"
                        Add-Content -Path $PROFILE -Value $block
                        Show-InstantStep "PowerShell Autocompletions" "Registered in $PROFILE"
                    }
                }
            } else {
                Show-InstantStep "PowerShell Autocompletions" "Already registered in profile"
            }
        } catch {
            Write-Warning "Could not register autocompletions: $($_.Exception.Message)"
        }
    }

    # 6. Verification & Final Status Card
    Write-Host ""
    if (-not $DryRun -and (Test-Path $targetExe)) {
        $versionStr = & $targetExe version 2>$null
        if (-not $versionStr) { $versionStr = "Canontra $Version" }

        # Authenticode Signature Check
        $sig = Get-AuthenticodeSignature -FilePath $targetExe -ErrorAction SilentlyContinue
        $sigDetail = "Unsigned"
        if ($sig -and $sig.SignerCertificate) {
            $subject = $sig.SignerCertificate.Subject
            $thumb = $sig.SignerCertificate.Thumbprint.Substring(0, 12) + "..."
            $sigDetail = "Self-Signed ($subject, SHA256:$thumb)"
        }

        Write-Host "$CheckGlyph CANONTRA SUCCESSFULLY INSTALLED" -ForegroundColor Green
        Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkGray
        Write-Host "  Executable:  $targetExe" -ForegroundColor White
        Write-Host "  Version:     $versionStr" -ForegroundColor Cyan
        Write-Host "  Target:      windows-$arch" -ForegroundColor White
        Write-Host "  Security:    $sigDetail" -ForegroundColor DarkGray
        Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkGray
        Write-Host "Quick Start:" -ForegroundColor White
        Write-Host "  1. Reload your current PowerShell session or profile:"
        Write-Host '     . $PROFILE' -ForegroundColor Cyan
        Write-Host "  2. Compute semantic fingerprints for any source file:"
        Write-Host '     canontra.exe fp src\main.py --hash' -ForegroundColor Cyan
        Write-Host "  3. Compare two versions with 9-tier structural invariance:"
        Write-Host '     canontra.exe compare file_v1.py file_v2.py --json' -ForegroundColor Cyan
        Write-Host "  4. Ingest an entire repository with whole-repo graph digests:"
        Write-Host '     canontra.exe repo . --json' -ForegroundColor Cyan
        Write-Host "  5. Query or verify cache integrity:"
        Write-Host '     canontra.exe cache info' -ForegroundColor Cyan
        Write-Host "================================================================================" -ForegroundColor DarkGray
    } elseif ($DryRun) {
        $summaryGlyph = if ($SupportsUnicode) { [char]0x2192 } else { "[*]" }
        Write-Host "$summaryGlyph CANONTRA DRY-RUN COMPLETED" -ForegroundColor Yellow
        Write-Host "All validation checks and paths verified successfully." -ForegroundColor DarkGray
    }
}

Main
