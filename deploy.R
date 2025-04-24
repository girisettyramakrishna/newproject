library(rsconnect)

rsconnect::setAccountInfo(name='ramkilabs',
                          token='94A259A8B02489BAFC5E2CE3442D164C',
                          secret='0rZk4dmGFbNu8EZlcbdCNqt6SG0axN/TdUt0sMTK')

# Assuming app is in repo root
rsconnect::deployApp('.', appName = "DF_v1")











