#' Growth accounting using CES production function (with labour augmenting technical progress)
#'
#' @param y A ts object.
#' @param k A ts object.
#' @param l A ts object.
#' @param beta A numeric value.
#' @param sigma A numeric value.
#' @param figure A boolean value.
#'
#' @return CES object.
#' @export
#'
#' @examples
ces <- function(y, k, l, beta, sigma, figure=FALSE){

  ces_obj <- new("CES", output = y, capital = k, labour = l, beta = beta, sigma = sigma)

  # Compute substitution parameter
  sp <- (1 - sigma) / sigma

  # Compute Solow residual
  a <- (1/l) * (beta / (y^(-sp) - (1 - beta) * k^(-sp)))^(1/sp)
  ces_obj@solow <- a

  # Compute growth rates
  y_pc <- log(y) - stats::lag(log(y), k = -1)
  k_pc <- log(k) - stats::lag(log(k), k = -1)
  l_pc <- log(l) - stats::lag(log(l), k = -1)
  a_pc <- log(a) - stats::lag(log(a), k = -1)

  # y_pc <- diff(y)/lag(y,k=-1)
  # k_pc <- diff(k)/lag(k,k=-1)
  # l_pc <- diff(l)/lag(l,k=-1)
  # a_pc <- diff(a)/lag(a,k=-1)

  # Compute contributions to growth (percent of y)
  k_ctgy <- (((1 - beta) * k^(-sp)) / ((1 - beta) * k^(-sp) + beta * (a * l)^(-sp)) * k_pc) / y_pc
  l_ctgy <- ((beta * (a * l)^(-sp)) / ((1 - beta) * k^(-sp) + beta * (a * l)^(-sp)) * l_pc) / y_pc
  a_ctgy <- ((beta * (a * l)^(-sp)) / ((1 - beta) * k^(-sp) + beta * (a * l)^(-sp)) * a_pc) / y_pc

  # Compute contributions to growth
  k_ctg <- k_ctgy * y_pc
  l_ctg <- l_ctgy * y_pc
  a_ctg <- a_ctgy * y_pc
  ctg <- cbind(y_pc, k_ctg, l_ctg, a_ctg)
  ces_obj@contribs <- ctg

  # Plot the contributions to growth
  chart_title <- paste0("CES - contributions to growth (beta = ",beta,", sigma = ",sigma,")")
  chart_subtitle <- paste0(stats::start(ces_obj@contribs)[1]," - ", stats::end(ces_obj@contribs)[1])
  chart_main <- paste0(chart_title,"\n",chart_subtitle)
  if(TRUE){
    # Base R
    test <- t(ces_obj@contribs[,2:4]*100)
    colnames(test) <- c(stats::start(ces_obj@contribs)[1]:stats::end(ces_obj@contribs)[1])
    posvals <- negvals <- test
    posvals[posvals<0] <- 0
    negvals[negvals>0] <- 0
    myrange <- c(min(colSums(negvals)),max(colSums(posvals)))
    barplot(posvals,ylim=myrange,col=c("blue","yellow","green"),las=1,main = chart_main)
    zzz<-barplot(negvals,add=TRUE,ylim=rev(myrange),col=c("blue","yellow","green","red"),las=1)
    legend("topleft",legend = c("K","L","A","Y"),col = c("blue","yellow","green","red"),pch = c(rep(15,3),20))
    lines(zzz,t(ces_obj@contribs[,1]*100),lwd=2,col="red")
    points(zzz,t(ces_obj@contribs[,1]*100),pch=20,col="red")
  }

  if(figure){
    # Plotly
    fig <- plot_ly(as.data.frame(ctg), x = (start(y)[1]+1):end(y)[1], y = ~k_ctg, type = 'bar', name = 'K')
    fig <- fig %>% add_trace(y = ~l_ctg, name = 'L')
    fig <- fig %>% add_trace(y = ~a_ctg, name = 'A')
    fig <- fig %>% layout(title = chart_title, yaxis = list(title = '%'), barmode = 'relative')
    fig <- fig %>% add_trace(y = ~y_pc, name = 'Y', type = 'scatter', mode ='lines+markers')
    #orca(last_plot(),file = paste0("./CES_ctg_",beta,"_",sigma,".png"))
    print(fig)
  }

  # Compute the historical average growth rate contributions
  avg_gr <- cbind(mean(y_pc[-1]), mean(a_ctg[-1]), mean(k_ctg[-1]), mean(l_ctg[-1]))
  colnames(avg_gr) <- c("Y", "A", "K", "L")
  rownames(avg_gr) <- c("Average growth rate")
  # cat("Contributions to growth:",(start(y)[1]+1)," to ", end(y)[1],"\n")
  # print(avg_gr)
  ces_obj@avg_gr <- avg_gr

  return(ces_obj)
}
