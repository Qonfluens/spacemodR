
net <- trophic() |>
  add_link("a", "b", 1) |>
  add_link("c", c("b","e","f","g"),
           weight = c(0.25, 0.3, 0.2, 0.25))
attr(net, "level")



net2 <- trophic() |>
  add_link("a", "b", 1) |>
  add_link("b", "c", 1) |>
  add_link("c", "a", 1)

