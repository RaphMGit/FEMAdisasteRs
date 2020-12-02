# map_fema_data
# GMU team 2 MC
# get_fema_data returns DisasterDeclarationsSummaries
#
# get_fema_data into a DataFrame for our test case.
entity <- "DisasterDeclarationsSummaries"
api_params <-  "filter=fyDeclared gt 1979"
FEMAdf <- get_fema_data(entity, api_params)

#library("ggplot2")
#library(RColorBrewer)
map_fema_data <- function(x) {
    morecolors = c(RColorBrewer::brewer.pal(12,"Paired"),"#000000")
    MapGG <- ggplot2::ggplot(FEMAdf, aes(state),fill=incidentType) + ggplot2::geom_bar(aes(fill=FEMAdf$incidentType), position=position_stack(), show.legend=TRUE) + 
        ggplot2::scale_fill_manual(values = morecolors)
    return(MapGG)
    } 
   
# try it 
map_fema_data(FEMAdf)
# END OF FILE
