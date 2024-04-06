library(tidyverse)
library(plotly)
library(ggpubr)
library(htmlwidgets)


data("happiness", package = "zenplots")
d <- highlight_key(happiness, ~Region)

p <- ggplot() + theme_bw() +
  geom_point(data=d, aes(x = Family, y = Happiness, color = Region, size = GDP),
             show.legend=FALSE) +
  scale_color_manual(values = rainbow(10, alpha = 0.6)) +
  scale_size_continuous(range = c(0, 10), name = '')

gg <- ggplotly(p, tooltip = "country")

# Custom JavaScript to increase point size on click
jsCode <- "function(el, x) {
  el.on('plotly_click', function(data) {
    var points = data.points;
    var newSize = 20; // New size for the clicked point
    for(var i = 0; i < points.length; i++) {
      var point = points[i];
      var update = {'marker.size': newSize};
      Plotly.restyle(el.id, update, [point.pointNumber]);
    }
  });
}"

gg <- gg %>% onRender(jsCode)

gg
