Notes to self:

This HRRR-based version 1 of the app was disabled on 1/22/2026
Version 2 (cnealy-mt/smoke_app_connect_v2) went operational 1/16/2026

This app was redundant and scheduled to be turned off, but it additionally encountered errors beginning 1/22/2026 due possibly to a change in the Utah MesoWest HRRR Zarr metadata (see https://mesowest.utah.edu/html/hrrr/ and GitHub Actions logs)
  App was disabled by commenting out the schedule in smoke_app_connect/.github/workflows/hourly_update.yml
  Currently (1/22/2026) app is still "live" on Posit Connect, though it's static with the last updated model data (1/21)
