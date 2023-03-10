## This code prepares datasets from Movebank for testing.

## 3-day datasets
base::load("~/Desktop/otherDir/movebankCredentials/pw.Rda")
MB.LoginObject <- move::movebankLogin(username = 'kaijagahm', password = pw)
rm(pw)

# Download two 3-day datasets from movebank (note: will have to separately ensure that downloadVultures is functioning as expected.)
jan01_03_2021_raw <- downloadVultures(loginObject = MB.LoginObject, dateTimeStartUTC = "2021-01-01 00:00", dateTimeEndUTC = "2021-03-01 11:59")
jun01_03_2022_raw <- downloadVultures(loginObject = MB.LoginObject, dateTimeStartUTC = "2022-06-01 00:00", dateTimeEndUTC = "2022-06-03 11:59")

# Save these as internal datasets
save(jan01_03_2021_raw, file = "~/Desktop/otherDir/jan01_03_2021_raw.Rda")
save(jun01_03_2022_raw, file = "~/Desktop/otherDir/jun01_03_2022_raw.Rda")
