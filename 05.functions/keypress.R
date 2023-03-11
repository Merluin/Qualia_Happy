keypress <- function(nmax_resp)

{
    string <- "KeyPress"
    
    x<-rep(string,nmax_resp )
    
    x<-paste0(x, 1:nmax_resp)
    
    RT<-paste0(x, "RT")
    
    assign("RT",RT,envir=.GlobalEnv)
    
    y<-rep(string, nmax_resp)
    
    y<-paste0(y, 1:nmax_resp)
    
    RESP<-paste0(y, "RESP")
    
    assign("RESP",RESP,envir=.GlobalEnv)
    
    key<-c(RESP,RT)

    string <- "dur_"
    
    z<-rep(string,nmax_resp )
    
    z<-paste0(z, 1:nmax_resp)
    
    w<-rep(string, nmax_resp)
    
    DUR<-paste0(w, 1:nmax_resp)
    
    assign("DUR",DUR,envir=.GlobalEnv)
}
