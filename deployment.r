# ===========================================================
# Rwanda Nutrition Intelligence Dashboard Deployment Script
# Developer: Thacien Haragirimana
# ===========================================================

# 1️⃣ Load package
library(rsconnect)

# 2️⃣ Set account info
rsconnect::setAccountInfo(
  name = 'dataverseanalytics',
  token = 'B3DB0ADE296CB61D82C149553EF776D4',
  secret = 'xPAwxCkrKVrJLi+hTKuVp8Vvq3St1TZzmzRJ/fqQ'
)

# 3️⃣ Define your app directory (path where app.R is saved)
app_directory <- "C:/Users/harag/OneDrive/Desktop/HACKTON/thacien project/thacien project/R project"

# 4️⃣ Deploy your app — use a valid appName (no spaces or special characters)
rsconnect::deployApp(
  appDir = app_directory,
  appName = "rwa1",   # ✅ simple and valid name
  account = "dataverseanalytics"
)

# 5️⃣ Confirmation message
cat("✅ Deployment initiated successfully! Please wait — your app will be live soon on shinyapps.io\n")
