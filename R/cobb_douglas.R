#' Growth accounting using Cobb-Douglas production function
#'
#' @param y A ts object.
#' @param k A ts object.
#' @param l A ts object.
#' @param beta A numeric value.
#' @param figure A boolean value.
#'
#' @return Cobb-Douglas object.
#' @export
#'
#' @examples
cobb_douglas <- function(y, k, l, beta, figure=FALSE){

  # Create a new Cobb-Douglas object with specified data
  cd_obj <- new("Cobb-Douglas", output = y, capital = k, labour = l, beta = beta)

  # Compute Solow residual
  a <- y / (k^(1 - beta) * l^beta)
  cd_obj@solow <- a

  # Compute growth rates (percentage changes)
  y_pc <- log(y) - lag(log(y), k = -1)
  k_pc <- log(k) - lag(log(k), k = -1)
  l_pc <- log(l) - lag(log(l), k = -1)
  a_pc <- log(a) - lag(log(a), k = -1)

  # y_pc <- diff(y)/lag(y,k=-1)
  # k_pc <- diff(k)/lag(k,k=-1)
  # l_pc <- diff(l)/lag(l,k=-1)
  # a_pc <- diff(a)/lag(a,k=-1)

  # Compute contributions to growth (percent of y)
  k_ctgy <- ((1 - beta) * k_pc) / y_pc
  l_ctgy <- (beta * l_pc) / y_pc
  a_ctgy <- a_pc / y_pc

  # Compute contributions to growth
  k_ctg <- k_ctgy * y_pc
  l_ctg <- l_ctgy * y_pc
  a_ctg <- a_ctgy * y_pc
  ctg <- cbind(y_pc, k_ctg, l_ctg, a_ctg)
  cd_obj@contribs <- ctg

  # Plot the contributions to growth
  chart_title <- paste0("Cobb-Douglas - contributions to growth (beta = ",beta,")")
  chart_subtitle <- paste0(start(cd_obj@contribs)[1]," - ", end(cd_obj@contribs)[1])
  chart_main <- paste0(chart_title,"\n",chart_subtitle)
  # Base R
  test <- t(cd_obj@contribs[,2:4]*100)
  colnames(test) <- c(start(cd_obj@contribs)[1]:end(cd_obj@contribs)[1])
  posvals <- negvals <- test
  posvals[posvals<0] <- 0
  negvals[negvals>0] <- 0
  myrange <- c(min(colSums(negvals)),max(colSums(posvals)))
  barplot(posvals,ylim=myrange,col=c("blue","yellow","green"),las=1,main = chart_main)
  zzz<-barplot(negvals,add=TRUE,ylim=rev(myrange),col=c("blue","yellow","green","red"),las=1)
  legend("topleft",legend = c("K","L","A","Y"),col = c("blue","yellow","green","red"),pch = c(rep(15,3),20))
  lines(zzz,t(cd_obj@contribs[,1]*100),lwd=2,col="red")
  points(zzz,t(cd_obj@contribs[,1]*100),pch=20,col="red")

  if(figure){
    # Plotly
    fig <- plot_ly(as.data.frame(ctg), x = (start(y)[1]+1):end(y)[1], y = ~k_ctg, type = 'bar', name = 'K')
    fig <- fig %>% add_trace(y = ~l_ctg, name = 'L')
    fig <- fig %>% add_trace(y = ~a_ctg, name = 'A')
    fig <- fig %>% layout(title=chart_title,yaxis = list(title = '%'), barmode = 'relative')
    fig <- fig %>% add_trace(y = ~y_pc, name = 'Y', type = 'scatter', mode ='lines+markers')
    print(fig)
    #orca(last_plot(),file = paste0("./CD_ctg_",beta,".png"))
  }

  # Compute the historical average growth rate contributions
  avg_gr <- cbind(mean(y_pc[-1]), mean(a_ctg[-1]), mean(k_ctg[-1]), mean(l_ctg[-1]))
  colnames(avg_gr) <- c("Y", "A", "K", "L")
  rownames(avg_gr) <- c("Average growth rate")
  #cat("Contributions to growth:",(start(y)[1]+1)," to ", end(y)[1],"\n")
  #print(avg_gr)
  cd_obj@avg_gr <- avg_gr

  return(cd_obj)
}
