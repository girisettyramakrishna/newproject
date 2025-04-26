library(rsconnect)

rsconnect::setAccountInfo(name='psmlabs',
                          token='8FAB74BDBB46C9CA05D75B7102711773',
                          secret='9a6AwJZn3QacJOlVCdzWv6Ptj34mOsuIRE3OuNDI')

# Assuming app is in repo root
rsconnect::deployApp('.', account = "psmlabs", appName = "DF_v1",forceUpdate = TRUE)







