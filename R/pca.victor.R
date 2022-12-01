
#-----------------------------------------

# This function is built on pca output of vegan rda function
# metadata comprises the info on the samples and must be in the same order than the PCA coordinates
# goodness.axis : the number of axis used to calculate the goodness of fit of the species
# main.group define the grouping variable used to calculate the densities and to define the colors, it cannot be empty
# second.group is used to define a second level of goruping that, if present, will be highligthed with different shapes
# nudge.x and nudge.y define the gap between the labels and the group centroid, it must have the same length as the grouping variable defined in main.group
# labels are use to labels the main groups while different color are given to the labels according to the second grouping variable if there is one

pca.victor <- function(pca,axes=c(1,2), metadata= NULL, site.scaling = 1, species.scaling = 2, goodness.axis = 2, goodness.tresh = 0.3, main.group = NULL, second.group = NULL, nudge.x = NULL, nudge.y = NULL, scale.fill = NULL, scale.shape = NULL, scale.colour = NULL, labels = NULL, scale.col.labels = NULL, print.sp.tresh = FALSE, ext.plot.scale = 5, add.stat1 = TRUE, stat1 = "chull", add.stat2 = FALSE, stat2 = NULL, add.centroids = FALSE, point.size = 2.5, font.size = 18/.pt, axis.size = 18, conf.level = .95){
  
  require(ggplot2)
  require(dplyr)
  require(cowplot)
  require(data.table)
  require(ggrepel)
  require(gridExtra)
  
  metadata <- as.data.frame(metadata)
  
  if(length(axes) > 2){
    stop("This function can only plot two axes simultenaously, please select only 2 axes")
  }
  if(!is.null(labels)){
    if(length(labels) != length(unique(metadata[,main.group]))){
      stop("Number of labels provided do not match with the number of groups provided in the `main.group` variable")
    }
  }
  
  #-----------------------------
  
  # Retrieve sites' scores (SCALING 1)
  site.scores <- scores(pca,scaling=site.scaling,display="sites",choices=axes)
  
  axes.name <- colnames(site.scores)
  
  if(nrow(metadata) != nrow(site.scores)){
    stop("There are no metadata available or the metadata do not match the number of sites in the pca output. This function needs metadata to customize the pca plot")
  }
  
  # Add the metadata to sites' scores (Scaling 1)
  site.scores <- cbind(metadata,site.scores)
  
  # Retrieve the site's scores in scaling 2 for the left plot and bind it to the metadata
  site.scores2 <- cbind(metadata, scores(pca, scaling= species.scaling, display="sites",choices=axes))
  
  # Retrieve species' scores (Scaling 2)
  sp.scores <- scores(pca,scaling = species.scaling, display="species", choices=axes)
  
  # Retrieve the goodness of fit of the species
  sp.fit <- goodness(pca,model="CA",statistic="explained")
  
  # Assemble species' scores and the goodness of fit for the selected axis
  sp.scores <- data.frame(species=rownames(sp.scores),sp.scores, fit=sp.fit[,goodness.axis])
  
  # Calculate the variance represented by each axis
  var.axes <- round(pca$CA$eig/sum(pca$CA$eig)*100,2)
  
  if(is.null(main.group)){
    stop("main.group cannot be empty as it defines the group on which densities and colours are defined")
  }
  
  # Density of sites along the first axis selected in "axes"
  PC1.dens <- site.scores %>%
    group.by.(main.group) %>%
    do(ggplot2:::compute.density(select.(.,axes.name[1]) %>% pull(), NULL)) %>%
    setnames("x", "PC1")
  
  # Density along the second axis selected in "axes"
  PC2.dens <- site.scores %>%
    group.by.(main.group) %>%
    do(ggplot2:::compute.density(select.(.,axes.name[2]) %>% pull(), NULL)) %>%
    setnames("x", "PC2")
  
  # Upper limit of the density curves
  dens.limit <- max(PC1.dens$density, PC2.dens$density) * 1.2
  
  # Define labels position
  if(is.null(nudge.x) | is.null(nudge.y)){
    # If there is no second.group
    if(is.null(second.group)){
      label.pos <- site.scores %>%
        group.by.(main.group) %>%
        summarise.at(vars(contains("PC")),mean)  %>%
        ungroup() %>%
        mutate(nudge.x = rep(0,nrow(.)),
               nudge.y = rep(0,nrow(.)))
    }else{ # If there is a second group
      label.pos <- site.scores %>%
        group.by.(main.group,second.group) %>%
        summarise.at(vars(contains("PC")),mean)  %>%
        ungroup() %>%
        mutate(nudge.x = rep(0,nrow(.)),
               nudge.y = rep(0,nrow(.)))
    }
  }else{
    # If there is no second.group
    if(is.null(second.group)){
      label.pos <- site.scores %>%
        group.by.(main.group) %>%
        summarise.at(vars(contains("PC")),mean)  %>%
        ungroup() %>%
        mutate(nudge.x = nudge.x,
               nudge.y = nudge.y)
    }else{ # If there is a second group
      label.pos <- site.scores %>%
        group.by.(main.group,second.group) %>%
        summarise.at(vars(contains("PC")),mean)  %>%
        ungroup() %>%
        mutate(nudge.x = nudge.x,
               nudge.y = nudge.y)
    }
  }
  
  # Renaming the two axis to make their selection easier for labelling
  colnames(label.pos)[which(colnames(label.pos)%in%axes.name)] <- c("PC1","PC2")
  
  # Calculate the coordinates of the labels
  label.pos <- label.pos %>%
    mutate(x = PC1 + nudge.x, y = PC2 + nudge.y)
  
  #-----------------------------
  
  # Main plot of sites
  #-------------------
  p.sites <- ggplot()
  if(is.null(second.group)){
    p.sites <- p.sites + geom.point(data=site.scores, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, colour = main.group), alpha=1, size = point.size)
  }else{
    p.sites <- p.sites + geom.point(data=site.scores, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, shape=second.group), alpha=0.3)
  }
  # Add a convex hull or ellipses around the main group
  if(add.stat1 == TRUE){
    if(!is.null(stat1)){
      if(stat1 == "chull"){
        p.sites <- p.sites + stat.chull(data = site.scores, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, col = main.group), geom="polygon", alpha=0.3, linetype = 2)+
          scale.colour.manual(values = scale.colour)
      }else{
        p.sites <- p.sites + stat.ellipse(data = site.scores, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, col = main.group), geom="polygon", alpha=0.3, linetype = 2, level = conf.level)+
          scale.colour.manual(values = scale.colour)
      }
    }
  }
  # Add centroids
  if(add.centroids == TRUE){
    centroids <-  site.scores %>%
      group.by.(main.group) %>%
      summarise.at(vars(contains("PC")),mean)  %>%
      ungroup()
    p.sites <- p.sites + geom.point(data = centroids, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group), shape = 24, col = "black", size = 5)
  }
  
  # Custmize colour of fill if provided
  if(!is.null(scale.fill)){
    p.sites <- p.sites + scale.fill.manual(values=scale.fill)
  }
  # Customize shape if needed and values are provided
  if(!is.null(second.group) & !is.null(scale.shape)){
    p.sites <- p.sites + scale.shape.manual(values=scale.shape)
  }
  # Add variance of axes
  p.sites <- p.sites + theme(legend.position="none") + xlab(paste(axes.name[1],": ",var.axes[axes.name[1]],"%")) + ylab(paste(axes.name[2],": ",var.axes[axes.name[2]],"%"))
  
  # Add labels of main group as provided in the variable if there is no labels provided to override this. Color of labels are unique if there is not a second grouping variable
  if(is.null(labels) & is.null(second.group)){
    p.sites <- p.sites + geom.label(data = label.pos, aes.string(x = "x", y = "y", label = main.group, fill = main.group), alpha= 1, fontface="bold", size = font.size, inherit.aes = FALSE)
  }
  # If there are labels provided and no second group
  if(!is.null(labels) & is.null(second.group)){
    p.sites <- p.sites + geom.label(data = label.pos, aes.string(x = "x", y = "y", label = "labels", fill = main.group), alpha= 1, fontface="bold", size = font.size, inherit.aes = FALSE)
  }
  # If there are no labels provided and a second grouping variable
  if(is.null(labels) & !is.null(second.group)){
    p.sites <- p.sites + geom.label(data = label.pos, aes.string(x = "x", y = "y", label = main.group, fill = main.group, colour = main.group), alpha= 1, fontface="bold", size = font.size, inherit.aes = FALSE)
  }
  # If there are labels provided and a second grouping variable
  if(!is.null(labels) & !is.null(second.group)){
    p.sites <- p.sites + geom.label(data = label.pos, aes.string(x = "x", y = "y", label = "labels", fill = main.group, colour = main.group), alpha= 1, fontface="bold", size = font.size, inherit.aes = FALSE)
  }
  
  if(!is.null(scale.col.labels)){
    p.sites <- p.sites + scale.colour.manual(values=scale.col.labels)
  }
  
  p.sites <- p.sites + theme(axis.title = element.text(size = axis.size, face = "bold"))
  # Density plot of sites along the first axis selected
  #----------------------------------------------------
  x.dens <- axis.canvas(p.sites, axis = "x")
  x.dens <- x.dens + geom.density(data = PC1.dens, aes.string(x = "PC1", y = "density", fill = main.group), alpha = 0.4,stat = "identity")
  if(!is.null(scale.fill)){
    x.dens <- x.dens + scale.fill.manual(values=scale.fill)
  }
  x.dens <- x.dens + scale.y.continuous(limits = c(0, dens.limit), expand = c(0, 0)) + theme(legend.position="FALSE")
  
  # Density plot of sites along the second axis selected
  #-----------------------------------------------------
  y.dens <- axis.canvas(p.sites, axis = "y", coord.flip = TRUE)
  y.dens <- y.dens + geom.density(data = PC2.dens, aes.string(x = "PC2", y = "density", fill = main.group), alpha = 0.4,stat = "identity")
  if(!is.null(scale.fill)){
    y.dens <- y.dens + scale.fill.manual(values=scale.fill)
  }
  y.dens <- y.dens + scale.y.continuous(limits = c(0, dens.limit), expand = c(0, 0))
  y.dens <- y.dens + coord.flip() + theme(legend.position="FALSE")
  
  # Assembly the 3 plots for the site
  #----------------------------------
  p1 <- insert.xaxis.grob(p.sites +
                            theme(legend.position = "none"),
                          x.dens, grid::unit(ext.plot.scale*14, "pt"), position = "top")
  
  p2 <- insert.yaxis.grob(p1, y.dens, grid::unit(ext.plot.scale*14, "pt"), position = "right")
  
  p.sites <- ggdraw(p2)
  
  #-----------------------------
  
  # Main plot of species
  #-------------------
  
  p.sp <- ggplot()
  if(is.null(second.group)){
    p.sp <- p.sp + geom.point(data = site.scores2, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, colour = main.group), alpha=1, size = 2.5)
  }else{
    p.sp <- p.sp + geom.point(data=site.scores2, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, shape=second.group), alpha=0.3)
  }
  #Add a convex hull or ellipses
  if(add.stat2 == TRUE){
    if(!is.null(stat2)){
      if(stat2 == "chull"){
        p.sp <- p.sp + stat.chull(data = site.scores2, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, col = main.group), geom="polygon", alpha=0.3, linetype = 2)+
          scale.colour.manual(values = scale.colour)
      }else{
        p.sp <- p.sp + stat.ellipse(data = site.scores2, aes.string(x = axes.name[1], y = axes.name[2], fill = main.group, col = main.group), geom="polygon", alpha=0.3, linetype = 2, level = conf.level)+
          scale.colour.manual(values = scale.colour)
      }
    }
  }
  
  # Customize shape if needed and values are provided
  if(!is.null(second.group) & !is.null(scale.shape)){
    p.sp <- p.sp + scale.shape.manual(values=scale.shape)
  }else{
    p.sp <- p.sp + scale.shape.manual(values=21)
  }
  if(!is.null(scale.fill)){
    p.sp <- p.sp + scale.fill.manual(values=scale.fill)
    p.sp <- p.sp + scale.colour.manual(values=scale.fill)
  }
  p.sp <- p.sp + geom.segment(data=sp.scores[sp.scores$fit > goodness.tresh,], aes.string(x=0, y=0, xend=axes.name[1], yend=axes.name[2]), colour="black")
  p.sp <- p.sp + geom.point(data=sp.scores[sp.scores$fit > goodness.tresh,], aes.string(x = axes.name[1], y = axes.name[2]), fill="black",shape=21,size=1.5)
  p.sp <- p.sp + geom.text.repel(data=sp.scores[sp.scores$fit > goodness.tresh,], aes.string(x = axes.name[1], y = axes.name[2], label = "species"), fontface="bold", colour="black", segment.colour="black", max.overlaps = Inf, size = font.size)
  p.sp <- p.sp + theme(legend.position="none") + xlab(paste(axes.name[1],": ",var.axes[axes.name[1]],"%")) + ylab(paste(axes.name[2],": ",var.axes[axes.name[2]],"%"))
  
  p.sp <- p.sp + theme(axis.title = element.text(size = axis.size, face = "bold"))
  # Trick to scale the species plot as the site plot
  blank.plot <- ggplot()+
    theme.void()
  
  if(print.sp.tresh){
    p1 <- insert.xaxis.grob(p.sp +
                              theme(legend.position = "none"),
                            blank.plot + annotate(geom="text",label=paste("Threshold for modalities representation :", goodness.tresh * 100, "% on the first",goodness.axis,"axes"), x = 0, y =0,family="Comic Sans MS"), grid::unit(ext.plot.scale*14, "pt"), position = "top")
  }else{
    p1 <- insert.xaxis.grob(p.sp +
                              theme(legend.position = "none"),
                            blank.plot, grid::unit(ext.plot.scale*14, "pt"), position = "top")
  }
  
  
  p2 <- insert.yaxis.grob(p1, blank.plot, grid::unit(ext.plot.scale*14, "pt"), position = "right")
  
  p.sp <- ggdraw(p2)
  
  #-----------------------------
  
  # Final plots
  #------------
  grid.arrange(p.sites,p.sp, ncol=2)
}


