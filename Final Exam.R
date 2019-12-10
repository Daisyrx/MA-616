# Final Exam
# Runxin Yu
pacman::p_load(tidyverse,gridExtra,reshape2,DEoptim,fitdistrplus,data.tree,networkD3,DiagrammeR,MASS)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

density_plot <- function(arr){
  return(ggplot()+geom_density(aes(arr),fill='green',alpha=0.2) + theme_bw() +
           geom_vline(xintercept = mean(arr),linetype='dashed',color='pink') + annotate("text",x=mean(arr)+2,y=0.2,label=paste("mean =",round(mean(arr),0))))
}

update_payoff <- function(j){
  payoff <- function(node) {
    if (node$type == 'chance') node$payoff <- sum(sapply(node$children, function(child) child$payoff * child$p))
    else if (node$type == 'decision') node$payoff <-  max(sapply(node$children, function(child) child$payoff))
  }
  
  j$Do(payoff, traversal = "post-order", filterFun = isNotLeaf)
  GetNodeLabel<- function(node) switch(node$type, terminal=node$payoff, chance=paste("EV =",node$payoff),decision=paste(node$name,"EMV =",node$payoff),branch=node$name)
  SetNodeStyle(j, fontname = 'helvetica', fontsize=18,shape = GetNodeShape, label = GetNodeLabel)
}



GetEdgeLabel <- function(node) {
  if (!node$isRoot && node$parent$type == 'chance') {
    label = paste0(node$name, " (", node$p, ")")
  } else if (!node$isRoot && node$parent$type == 'decision'){
    label = paste0(node$route)
  }else {
    label = ""
  }
  return (label)
}
GetNodeShape <- function(node) switch(node$type, decision = "box", chance = "ellipse",branch='none',terminal='none')
GetNodeLabel<- function(node) switch(node$type, terminal=node$payoff, chance=node$name,decision=node$name,branch=node$name)

## Question 1
# a. Develop a decision tree for this problem
BID <- Node$new("Place",type="decision")
HIGH <- BID$AddChild("High",type="decision",route='Yes')
HIGH$AddChild("Stop",type="terminal",route='No',payoff=0)

SCG_ <- HIGH$AddChild("SGC",type="decision",route="Yes")

HIGH_ <- SCG_$AddChild("Outcome",type="chance",route="Yes")
HIGH_$AddChild("Large",type="terminal",p=0.15,payoff=136-5)
HIGH_$AddChild("Average",type="terminal",p=0.35,payoff=7+28-5)
HIGH_$AddChild("Unusable",type="terminal",p=0.5,payoff=0)

NGC_ <- SCG_$AddChild("NGC",type="chance",route='No')

NGC_B <- NGC_$AddChild("Option 1",type="decision",p=0.4)
NGC_B$AddChild("Stop",type="terminal",route='No',payoff=0)

NGC_B <- NGC_B$AddChild("Outcome",type="chance",route="Yes")
NGC_B$AddChild("Large",type="terminal",p=0.15,payoff=10+120-5)
NGC_B$AddChild("Average",type="terminal",p=0.35,payoff=6+28-5)
NGC_B$AddChild("Unusable",type="terminal",p=0.5,payoff=0)

NGC_BO <- NGC_$AddChild("Option 2",type="decision",p=0.6)
NGC_BO$AddChild("Stop",type="terminal",route='No',payoff=0)

NGC_BO <- NGC_BO$AddChild("Outcome",type="chance",route="Yes")
NGC_BO$AddChild("Large",type="terminal",p=0.15,payoff=10+120-5)
NGC_BO$AddChild("Average",type="terminal",p=0.35,payoff=6+28-5)
NGC_BO$AddChild("Unusable",type="terminal",p=0.5,payoff=0)

BID_ <- BID$AddChild("Low",type="decision",route="No")
BID_L <- BID_$AddChild("SGC",type="decision",route = "Yes")

SGC_L <- BID_L$AddChild("Outcome",type="chance",route="Yes")
BID_L$AddChild("Stop",type="terminal",route="No",payoff=0)
SGC_L$AddChild("Large",type="terminal",p=0.15,payoff=136-5)
SGC_L$AddChild("Average",type="terminal",p=0.35,payoff=7+28-5)
SGC_L$AddChild("Unusable",type="terminal",p=0.5,payoff=0)

NGC_L <- BID_$AddChild("NGC",type="chance",route='No')

NGC_LB <- NGC_L$AddChild("Option 1",type="decision",p=0.4)
NGC_LB$AddChild("Stop",type="terminal",route='No',payoff=0)

NGC_LB <- NGC_LB$AddChild("Outcome",type="chance",route="Yes")
NGC_LB$AddChild("Large",type="terminal",p=0.15,payoff=10+120-5)
NGC_LB$AddChild("Average",type="terminal",p=0.35,payoff=6+28-5)
NGC_LB$AddChild("Unusable",type="terminal",p=0.5,payoff=0)

NGC_LBO <- NGC_L$AddChild("Option 2",type="decision",p=0.6)
NGC_LBO$AddChild("Stop",type="terminal",route='No',payoff=0)

NGC_LBO <- NGC_LBO$AddChild("Outcome",type="chance",route="Yes")
NGC_LBO$AddChild("Large",type="terminal",p=0.15,payoff=10+120-5)
NGC_LBO$AddChild("Average",type="terminal",p=0.35,payoff=6+28-5)
NGC_LBO$AddChild("Unusable",type="terminal",p=0.5,payoff=0)



SetEdgeStyle(BID, fontname = 'helvetica',fontsize=18, label = GetEdgeLabel,color='blue')
SetNodeStyle(BID, fontname = 'helvetica', fontsize=18,shape = GetNodeShape, label = GetNodeLabel)

plot(BID)

update_payoff(BID)
plot(BID)










