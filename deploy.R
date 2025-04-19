library(rsconnect)

# Set account info (best to set these as env vars in Bitbucket)
rsconnect::setAccountInfo(
  name = Sys.getenv("ramkiapps"),
  token = Sys.getenv("94A259A8B02489BAFC5E2CE3442D164C"),
  secret = Sys.getenv("0rZk4dmGFbNu8EZlcbdCNqt6SG0axN/TdUt0sMTK")
)

# Deploy app (adjust appDir as needed)
rsconnect::deployApp(appDir = "C:/Users/admin/source/repos/Demand Forecasting")

