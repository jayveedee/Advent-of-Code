param(
    [string]$SessionCookie,  # Your session cookie for Advent of Code
    [int]$Year,              # Year for Advent of Code
    [int]$Day = 0            # Specific day (optional, 0 means all days)
)

# Validate the session cookie
if (-not $SessionCookie) {
    Write-Error "SessionCookie parameter is required."
    return
}

# Validate the year
if (-not $Year) {
    Write-Error "Year parameter is required."
    return
}

# Determine the range of days
$StartDay = if ($Day -gt 0) { $Day } else { 1 }
$EndDay = if ($Day -gt 0) { $Day } else { 25 }

function Fetch-Input {
    param (
        [int]$Day,
        [int]$Year,
        [string]$SessionCookie
    )
    $FilePath = "../aoc/${Year}/inputs/day${Day}.txt"

    if (Test-Path -Path $FilePath) {
        Write-Host "Input for Day ${Day} already exists. Skipping."
        return
    }

    $Directory = Split-Path -Path $FilePath
    if (-not (Test-Path -Path $Directory)) {
        New-Item -ItemType Directory -Path $Directory | Out-Null
    }

    $Url = "https://adventofcode.com/${Year}/day/${Day}/input"

    # Create and configure a WebRequestSession with the session cookie
    $Cookie = [System.Net.Cookie]::new('session', $SessionCookie, '/', 'adventofcode.com')
    $Session = [Microsoft.PowerShell.Commands.WebRequestSession]::new()
    $Session.Cookies.Add($Cookie)

    Write-Host "Fetching URL: ${Url}" -ForegroundColor Cyan
    try {
        $Response = Invoke-WebRequest -Uri $Url -WebSession $Session -Method GET -ErrorAction Stop -Verbose
        $Response.Content | Set-Content -Path $FilePath
        Write-Host "Input for Day ${Day} saved."
    } catch {
        if ($_.Exception.Response.StatusCode -eq 404) {
            Write-Warning "Day ${Day} has not been published yet. Skipping."
        } elseif ($_.Exception.Response.StatusCode -eq 400) {
            Write-Error "Bad Request: Check the session cookie or URL."
        } else {
            Write-Error "Error fetching input for Day ${Day}: $(${_.Exception.Message})"
        }
    }
}

# Fetch inputs for the specified range of days
foreach ($Day in $StartDay..$EndDay) {
    Write-Host "Fetching input for Day ${Day} of ${Year}..."
    Fetch-Input -Day $Day -Year $Year -SessionCookie ${SessionCookie}
}
