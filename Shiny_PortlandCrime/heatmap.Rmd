heatmap = function(ds){
  
  ggplot(ds) +
    aes(x = weekday, y = time) +
    geom_tile(aes(fill=count), colour = "white") +
    scale_fill_distiller(name= "# Offenses", palette = "YlGnBu", direction = 1) +
    scale_x_discrete(breaks = c("0", "1", "2", "3", "4", "5", "6"),
                     label = c("Sunday", "Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday"), expand=c(0,0))+
    
    labs(title="Number of Cases Reported in Portland, OR during January & February 2023",
         x="", 
         y="Hour")+
    theme_classic()+
    
    theme(
      
      axis.line=element_blank(),                                               
      axis.ticks=element_line(size=0.4),
      axis.text = element_text(size= 10, color= "#1e1b25"),
      axis.line.x = element_line(color= "#1e1b25" ),
      
      plot.background=element_blank(),         
      plot.title = element_text(size =11, hjust = 0, color= "#1e1b25", face = "bold"),
      
      panel.grid = element_blank(),
      
      legend.position = "bottom",
      legend.title = element_text(size= 8),
      legend.margin=margin(grid::unit(0,"cm")),
      legend.key.width=grid::unit(2,"cm"),
      legend.key.height=grid::unit(0.2,"cm")
    )+
    coord_flip() 
}