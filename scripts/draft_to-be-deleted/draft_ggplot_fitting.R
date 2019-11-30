ggplot(mtcars, aes(wt, mpg)) +
  geom_point() + stat_function(fun = function(x_val){x_val^2}) + # by
                                 scale_x_continuous
                               
                               ## ~ .x^2
