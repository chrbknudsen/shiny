# Four approved random label motifs, drawn entirely with base R.
#
# type = 1: suspicious seal
# type = 2: mechanical eye
# type = 3: astrolabe
# type = 4: pseudo-scientific diagram
#
# Use seed = NULL for a new drawing every time.
# Use an integer seed to reproduce a particular drawing.

circle <- function(x, y, r, n = 240, ...) {
  a <- seq(0, 2 * pi, length.out = n)
  polygon(x + r * cos(a), y + r * sin(a), ...)
}

arc <- function(x, y, r, from, to, n = 80, ...) {
  a <- seq(from, to, length.out = n)
  lines(x + r * cos(a), y + r * sin(a), ...)
}

ellipse <- function(x, y, rx, ry, angle = 0, n = 200, ...) {
  a <- seq(0, 2 * pi, length.out = n)
  xx <- rx * cos(a)
  yy <- ry * sin(a)

  polygon(
    x + xx * cos(angle) - yy * sin(angle),
    y + xx * sin(angle) + yy * cos(angle),
    ...
  )
}

regular_polygon <- function(x, y, r, sides, angle = pi / 2, ...) {
  a <- angle + seq(0, 2 * pi, length.out = sides + 1)
  polygon(x + r * cos(a), y + r * sin(a), ...)
}

gear <- function(x, y, inner_r, outer_r, teeth, angle = 0, ...) {
  a <- angle + seq(0, 2 * pi, length.out = teeth * 4 + 1)
  radii <- rep(c(inner_r, outer_r, outer_r, inner_r), teeth)
  radii <- c(radii, radii[1])

  polygon(
    x + radii * cos(a),
    y + radii * sin(a),
    ...
  )
}

rotate_xy <- function(x, y, angle) {
  cbind(
    x = x * cos(angle) - y * sin(angle),
    y = x * sin(angle) + y * cos(angle)
  )
}

draw_small_star <- function(x, y, r, points = 5, angle = pi / 2) {
  a <- angle + seq(0, 2 * pi, length.out = points * 2 + 1)
  radii <- rep(c(r, r * 0.38), points)
  radii <- c(radii, radii[1])

  polygon(
    x + radii * cos(a),
    y + radii * sin(a),
    border = "black",
    col = NA,
    lwd = 1
  )
}

draw_suspicious_seal <- function() {
  circle(50, 50, 38, border = "black", lwd = 4)
  circle(50, 50, 34, border = "black", lwd = 1.2)

  point_n <- sample(5:9, 1)
  angles <- sort(runif(point_n, 0, 2 * pi))
  radii <- runif(point_n, 24, 32)
  points_xy <- cbind(
    x = 50 + radii * cos(angles),
    y = 50 + radii * sin(angles)
  )

  # Construct a star-like path. Only use a step coprime to point_n;
  # otherwise the path could split into separate cycles.
  possible_steps <- 2:(point_n - 2)
  possible_steps <- possible_steps[
    vapply(
      possible_steps,
      function(step) {
        length(unique(((seq_len(point_n) - 1) * step) %% point_n)) ==
          point_n
      },
      logical(1)
    )
  ]

  if (length(possible_steps) == 0) {
    step <- 1
  } else {
    step <- sample(possible_steps, 1)
  }

  point_order <- ((seq_len(point_n) - 1) * step) %% point_n + 1
  lines(
    points_xy[c(point_order, point_order[1]), ],
    lwd = runif(1, 1.5, 3)
  )

  # Additional unexplained chords
  for (i in seq_len(sample(3:7, 1))) {
    pair <- sample(seq_len(point_n), 2)
    lines(points_xy[pair, ], lwd = runif(1, 0.8, 2))
  }

  # Ringed nodes
  node_ids <- sample(
    seq_len(point_n),
    sample(2:min(4, point_n), 1)
  )

  for (i in node_ids) {
    circle(
      points_xy[i, 1],
      points_xy[i, 2],
      runif(1, 1.6, 3),
      border = "black",
      lwd = 2
    )

    if (runif(1) < 0.6) {
      points(
        points_xy[i, 1],
        points_xy[i, 2],
        pch = 16,
        cex = 0.7
      )
    }
  }

  if (runif(1) < 0.7) {
    regular_polygon(
      50,
      50,
      runif(1, 7, 13),
      sample(3:6, 1),
      angle = runif(1, 0, pi),
      border = "black",
      lwd = 2
    )
  }
}

draw_mechanical_eye <- function() {
  angle <- runif(1, -0.18, 0.18)

  eye_x <- c(
    seq(-32, 0, length.out = 80),
    seq(0, 32, length.out = 80)
  )
  eye_y <- c(
    15 * sin(seq(0, pi / 2, length.out = 80)),
    15 * cos(seq(0, pi / 2, length.out = 80))
  )

  upper <- rotate_xy(eye_x, eye_y, angle)
  lower <- rotate_xy(eye_x, -eye_y, angle)

  lines(upper[, 1] + 50, upper[, 2] + 50, lwd = 3)
  lines(lower[, 1] + 50, lower[, 2] + 50, lwd = 3)

  gear(
    50,
    50,
    inner_r = 10,
    outer_r = 13,
    teeth = sample(10:18, 1),
    angle = runif(1, 0, pi),
    border = "black",
    col = NA,
    lwd = 2
  )
  circle(
    50,
    50,
    runif(1, 6, 9),
    border = "black",
    lwd = 2
  )
  circle(
    50,
    50,
    runif(1, 2, 4),
    border = "black",
    col = "black"
  )

  # Partial graduated scales
  scale_radii <- sort(runif(sample(2:4, 1), 19, 34))

  for (r in scale_radii) {
    start <- runif(1, 0, 1.5 * pi)
    span <- runif(1, pi / 5, pi)
    a <- seq(start, start + span, length.out = 80)

    lines(
      50 + r * cos(a),
      50 + r * sin(a),
      lwd = runif(1, 0.8, 2)
    )

    tick_angles <- seq(
      start,
      start + span,
      length.out = sample(5:10, 1)
    )

    for (tick_angle in tick_angles) {
      lines(
        50 + c(r - 1.5, r + 1.5) * cos(tick_angle),
        50 + c(r - 1.5, r + 1.5) * sin(tick_angle),
        lwd = 0.8
      )
    }
  }

  # Smaller gears around the eye
  for (i in seq_len(sample(2:4, 1))) {
    gear_angle <- runif(1, 0, 2 * pi)
    gear_distance <- runif(1, 25, 34)
    gear_x <- 50 + gear_distance * cos(gear_angle)
    gear_y <- 50 + gear_distance * sin(gear_angle)
    gear_r <- runif(1, 4, 7)

    gear(
      gear_x,
      gear_y,
      inner_r = gear_r * 0.75,
      outer_r = gear_r,
      teeth = sample(7:11, 1),
      angle = gear_angle,
      border = "black",
      col = NA,
      lwd = 1.2
    )
    circle(
      gear_x,
      gear_y,
      gear_r * 0.25,
      border = "black"
    )
  }
}

draw_astrolabe <- function() {
  outer_r <- 39
  circle(50, 50, outer_r, border = "black", lwd = 3)
  circle(50, 50, outer_r - 3, border = "black", lwd = 1.2)

  # Graduated outer scale
  tick_n <- sample(c(36, 48, 60, 72), 1)
  tick_angles <- seq(
    0,
    2 * pi,
    length.out = tick_n + 1
  )[-(tick_n + 1)]

  for (i in seq_along(tick_angles)) {
    tick_angle <- tick_angles[i]

    tick_length <- if (i %% 6 == 1) {
      4.0
    } else if (i %% 3 == 1) {
      2.8
    } else {
      1.5
    }

    lines(
      50 + c(outer_r - 3, outer_r - 3 - tick_length) *
        cos(tick_angle),
      50 + c(outer_r - 3, outer_r - 3 - tick_length) *
        sin(tick_angle),
      lwd = if (i %% 6 == 1) 1.4 else 0.7
    )
  }

  # Concentric measuring circles
  measuring_radii <- sort(runif(sample(2:4, 1), 12, 29))

  for (r in measuring_radii) {
    circle(
      50,
      50,
      r,
      border = "black",
      lwd = runif(1, 0.6, 1.3)
    )
  }

  # Offset celestial circles
  for (i in seq_len(sample(2:4, 1))) {
    offset_angle <- runif(1, 0, 2 * pi)
    offset_distance <- runif(1, 3, 9)

    ellipse(
      50 + offset_distance * cos(offset_angle),
      50 + offset_distance * sin(offset_angle),
      rx = runif(1, 20, 30),
      ry = runif(1, 8, 16),
      angle = runif(1, 0, pi),
      border = "black",
      col = NA,
      lwd = runif(1, 0.6, 1.2)
    )
  }

  # Moving pointers
  pointer_angles <- runif(sample(2:4, 1), 0, 2 * pi)

  for (pointer_angle in pointer_angles) {
    short_end <- runif(1, 8, 15)
    long_end <- runif(1, 26, 34)

    arrows(
      50 - short_end * cos(pointer_angle),
      50 - short_end * sin(pointer_angle),
      50 + long_end * cos(pointer_angle),
      50 + long_end * sin(pointer_angle),
      length = 0.08,
      angle = 20,
      lwd = runif(1, 1.1, 2.2)
    )
  }

  # Fixed stars
  for (i in seq_len(sample(4:8, 1))) {
    star_angle <- runif(1, 0, 2 * pi)
    star_distance <- sqrt(runif(1, 0.08, 1)) * 27

    draw_small_star(
      50 + star_distance * cos(star_angle),
      50 + star_distance * sin(star_angle),
      r = runif(1, 1.2, 2.6),
      points = sample(c(4, 5, 6, 8), 1),
      angle = runif(1, 0, pi)
    )
  }

  # Central pin and suspension ring
  circle(
    50,
    50,
    3.2,
    border = "black",
    col = "#efe3c5",
    lwd = 2
  )
  circle(50, 50, 1, border = "black", col = "black")
  lines(c(46, 54), c(89, 89), lwd = 2)
  arc(50, 92, 4, 0, pi, lwd = 2)
}

draw_diagram_node <- function(x, y, kind, size) {
  if (kind == "circle") {
    circle(
      x,
      y,
      size,
      border = "black",
      col = "#efe3c5",
      lwd = 1.5
    )
  } else if (kind == "triangle") {
    regular_polygon(
      x,
      y,
      size * 1.2,
      sides = 3,
      angle = pi / 2,
      border = "black",
      col = "#efe3c5",
      lwd = 1.5
    )
  } else if (kind == "square") {
    regular_polygon(
      x,
      y,
      size,
      sides = 4,
      angle = pi / 4,
      border = "black",
      col = "#efe3c5",
      lwd = 1.5
    )
  } else {
    lines(c(x - size, x + size), c(y, y), lwd = 1.5)
    lines(c(x, x), c(y - size, y + size), lwd = 1.5)
    circle(x, y, size * 0.65, border = "black", lwd = 1)
  }
}

draw_pseudoscientific_diagram <- function() {
  circle(50, 50, 39, border = "black", lwd = 2.5)

  node_n <- sample(7:11, 1)
  node_angles <- sort(runif(node_n, 0, 2 * pi))
  node_radii <- runif(node_n, 14, 31)
  nodes <- cbind(
    x = 50 + node_radii * cos(node_angles),
    y = 50 + node_radii * sin(node_angles)
  )

  # A connected backbone
  node_order <- sample(seq_len(node_n))

  for (i in seq_len(node_n - 1)) {
    from <- nodes[node_order[i], ]
    to <- nodes[node_order[i + 1], ]

    arrows(
      from[1],
      from[2],
      to[1],
      to[2],
      length = 0.07,
      angle = 18,
      lwd = runif(1, 0.7, 1.4)
    )
  }

  # Additional unexplained relationships
  for (i in seq_len(sample(3:7, 1))) {
    pair <- sample(seq_len(node_n), 2)
    from <- nodes[pair[1], ]
    to <- nodes[pair[2], ]

    if (runif(1) < 0.7) {
      arrows(
        from[1],
        from[2],
        to[1],
        to[2],
        length = 0.06,
        angle = 18,
        lty = sample(c(1, 2, 3), 1),
        lwd = runif(1, 0.6, 1.2)
      )
    } else {
      midpoint <- (from + to) / 2 + runif(2, -6, 6)

      xspline(
        x = c(from[1], midpoint[1], to[1]),
        y = c(from[2], midpoint[2], to[2]),
        shape = 0.5,
        open = TRUE,
        lty = 2,
        lwd = 1
      )
    }
  }

  node_kinds <- sample(
    c("circle", "triangle", "square", "crosshair"),
    node_n,
    replace = TRUE
  )
  node_sizes <- runif(node_n, 2.5, 5)

  for (i in seq_len(node_n)) {
    draw_diagram_node(
      nodes[i, 1],
      nodes[i, 2],
      node_kinds[i],
      node_sizes[i]
    )
  }

  node_labels <- paste0(
    sample(LETTERS, node_n, replace = TRUE),
    sample(c(seq_len(9), "", ""), node_n, replace = TRUE)
  )

  text(
    nodes[, 1],
    nodes[, 2] - node_sizes - 2.2,
    labels = node_labels,
    family = "mono",
    cex = 0.65
  )

  # Two unrelated measuring arcs
  for (i in seq_len(2)) {
    radius <- runif(1, 20, 34)
    start <- runif(1, 0, 1.4 * pi)
    finish <- start + runif(1, pi / 5, pi / 1.7)

    arc(50, 50, radius, start, finish, lty = 2, lwd = 0.8)

    tick_angles <- seq(
      start,
      finish,
      length.out = sample(5:9, 1)
    )

    for (tick_angle in tick_angles) {
      lines(
        50 + c(radius - 1.3, radius + 1.3) * cos(tick_angle),
        50 + c(radius - 1.3, radius + 1.3) * sin(tick_angle),
        lwd = 0.8
      )
    }
  }

  # Central reference axes
  lines(c(45, 55), c(50, 50), lwd = 1.2)
  lines(c(50, 50), c(45, 55), lwd = 1.2)
  circle(
    50,
    50,
    2.5,
    border = "black",
    col = "#efe3c5",
    lwd = 1.5
  )
}

random_label_motif <- function(type = 1, seed = NULL) {
  stopifnot(
    length(type) == 1,
    type %in% 1:4
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  old_par <- par(
    mar = rep(0.4, 4),
    xaxs = "i",
    yaxs = "i",
    bg = "#efe3c5"
  )
  on.exit(par(old_par), add = TRUE)

  plot.new()
  plot.window(
    xlim = c(0, 100),
    ylim = c(0, 100),
    asp = 1
  )

  switch(
    as.character(type),
    "1" = draw_suspicious_seal(),
    "2" = draw_mechanical_eye(),
    "3" = draw_astrolabe(),
    "4" = draw_pseudoscientific_diagram()
  )

  invisible(NULL)
}
