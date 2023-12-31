
#' Create a map of the provided variable on the RESOURCECODE field grid
#'
#' @param z the data ro plot: a vector of the same size as the grid (328,030 rows)
#' @param name name of the variable plored, to be included in the legend
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#'\dontrun{
#'  rscd_mapplot(rscd_field$depth)
#'}
rscd_mapplot = function(z,name="Depth (m)"){

  xyzgz <- tibble::tibble(x = resourcecode::rscd_field$longitude[resourcecode::rscd_triangles],
                                y = resourcecode::rscd_field$latitude[resourcecode::rscd_triangles],
                                z = z[resourcecode::rscd_triangles],
                                g = rep(seq_len(ncol(resourcecode::rscd_triangles)), each = nrow(resourcecode::rscd_triangles)))

  ggplot(data=xyzgz,aes(.data$x, .data$y, group = .data$g, fill = .data$z,col=.data$z))+
    geom_polygon()+
    scale_fill_distiller(name=name,palette = "YlOrRd",na.value = "transparent")+
    scale_color_distiller(guide='none',palette = "YlOrRd",na.value = "transparent")+
    geom_path(data=resourcecode::rscd_coastline,aes(x=.data$longitude,y=.data$latitude),linewidth=.2,inherit.aes = FALSE)+
    geom_path(data=resourcecode::rscd_islands,aes(x=.data$longitude,y=.data$latitude,group=.data$ID),linewidth=.2,inherit.aes = FALSE)+
    coord_sf(expand=F,crs = sf::st_crs(4326))+
    theme_void()+
    theme(legend.position = c(.8, 0.2),
          #legend.direction = "horizontal",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          #legend.box="vertical",
          panel.border =  element_rect(color = "black",
                                       fill = NA,
                                       linewidth = 1))
}
