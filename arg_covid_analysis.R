#
#
library(incidence)
library(projections)
library(epitrix)
library(EpiModel)
library(EpiEstim)
# load RData
load("CovidEpiFile.RData")
mu = 5.08  # mean incubation period
sigma = .18 # deviation in meta analysis
cv = mu/sigma
params <- gamma_mucv2shapescale(mu,cv)

confirmed <- raw_data[clasificacion_resumen %in% c("Confirmado")]
incid <- incidence(confirmed$fecha_apertura)
#incid <- incidence(confirmed$fecha_apertura,groups=confirmed$residencia_provincia_nombre)
incid.tail <- subset(incid,from=as.Date("2020-07-15"))
# incid <- as.incidence(confirmed,
#                       dates=confirmed$fecha_apertura,
#                       groups=TRUE,
#                       interval=1)
fit.incid <- fit(incid[110:165])
fit.incid
plot(incid[160:175],fit=fit.incid)
plot(fit.incid)
plot(incid.tail)
res_parametric_si <- estimate_R(incid[1:170],
                                method = "parametric_si",
                                config = make_config(list(
                                      mean_si=mu,
                                      std_si=sigma)))
res_parametric_si$R[,3]
plot(res_parametric_si)
    plot(res_parametric_si$R[,3],
         ylab="R0",
         xlab="Days",
         type="l",
         main=paste("R0 over time,",locality["title",val],incid_title[n]),
         col=locality["dark",val]
    )
    minor.tick(ny=2)
    #axis(1,incid$date,format(incid$date, "%b %d"), cex.axis = .7)
    grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted",
         lwd = par("lwd"), equilogs = TRUE)
    
# Run reports on CABA, San Isidro, and immediately surrounding



# incid_x_start <- c(0,21,37)
# #incid_x_start <- c(0,21,37,67) # ends on 7-day rolling window
# incid_title <- c("overall","past 14 days","past 30 days","past 60 days")
# n=0
# 
# for(i in incid_x_start) {
#   n=n+1
#   for(val in x) {
#     # hardcode at 7 to avoid rolling 7-day NAs
#     incid <- as.incidence(covid_frame[7:rows,locality["avg",val]], 
#                           dates=covid_frame[7:rows,1], 
#                           interval=1)
#     incid_rows <- dim(incid)
#     end_x <- as.numeric(incid_rows[1])
#     if(i==0) {
#       start_x <- 1
#     }
#     else {
#       start_x <- end_x-i
#     }
#     si <- distcrete("gamma", 
#                     shape=params$shape, 
#                     scale=params$scale, 
#                     interval=1, w=0.5)
#     #pred_1 <- project(incid, R=1.15, si=si, n_days=30, n_sim=1000)
#     #f <- fit(incid[start_x:end_x])
#     #plot (incid[start_x:end_x], fit=f, color=locality["light",val])
#     #f
#     res_parametric_si <- estimate_R(incid, 
#                                     method = "parametric_si",
#                                     config = make_config(list(
#                                       mean_si=mu,
#                                       std_si=sigma)))
#     
#     #@TODO get dates in the x axis labels
#     #plot(res_parametric_si, legend=FALSE)
#     plot(res_parametric_si$R[start_x:end_x,3],
#          ylab="R0",
#          xlab="Days",
#          type="l",
#          main=paste("R0 over time,",locality["title",val],incid_title[n]),
#          col=locality["dark",val]
#     )
#     minor.tick(ny=2)
#     #axis(1,incid$date,format(incid$date, "%b %d"), cex.axis = .7)
#     grid(nx = 0, ny = NULL, col = "lightgray", lty = "dotted",
#          lwd = par("lwd"), equilogs = TRUE)
#   }
# }

# Find R for each province

# Run reports on 5x worst provinces

# Find R for each PBA municipality

# Run reports on 5x worst, 3x best

