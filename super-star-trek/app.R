library(shiny)

rand_sector <- function() sample(1:8, 1)

quadrant_name <- function(q1, q2, region_only = FALSE) {
  left <- c("ANTARES", "RIGEL", "PROCYON", "VEGA", "CANOPUS", "ALTAIR", "SAGITTARIUS", "POLLUX")
  right <- c("SIRIUS", "DENEB", "CAPELLA", "BETELGEUSE", "ALDEBARAN", "REGULUS", "ARCTURUS", "SPICA")
  region <- if (q2 <= 4) left[q1] else right[q1]
  if (region_only) return(region)
  suffix <- c(" I", " II", " III", " IV")[(q2 - 1) %% 4 + 1]
  paste0(region, suffix)
}

init_game <- function() {
  stardate <- sample(20:39, 1) * 100
  mission_days <- 25 + sample(0:9, 1)

  galaxy <- matrix(0L, nrow = 8, ncol = 8)
  known <- matrix(0L, nrow = 8, ncol = 8)

  k_total <- 0
  b_total <- 2

  for (i in 1:8) {
    for (j in 1:8) {
      r <- runif(1)
      k <- if (r > 0.98) 3 else if (r > 0.95) 2 else if (r > 0.80) 1 else 0
      b <- if (runif(1) > 0.96) 1 else 0
      s <- rand_sector()
      k_total <- k_total + k
      b_total <- b_total + b
      galaxy[i, j] <- k * 100 + b * 10 + s
    }
  }

  q1 <- rand_sector(); q2 <- rand_sector(); s1 <- rand_sector(); s2 <- rand_sector()

  if (b_total == 0) {
    if (galaxy[q1, q2] < 200) {
      galaxy[q1, q2] <- galaxy[q1, q2] + 120
      k_total <- k_total + 1
    }
    b_total <- 1
    galaxy[q1, q2] <- galaxy[q1, q2] + 10
  }

  mission_days <- max(mission_days, k_total + 1)

  list(
    stardate = stardate,
    initial_stardate = stardate,
    mission_days = mission_days,
    game_over = FALSE,
    won = FALSE,
    galaxy = galaxy,
    known = known,
    q1 = q1, q2 = q2, s1 = s1, s2 = s2,
    energy = 3000,
    energy0 = 3000,
    shields = 0,
    torpedoes = 10,
    torpedoes0 = 10,
    shield_max_hit = 200,
    docked = FALSE,
    devices = rep(0, 8),
    devices_names = c(
      "WARP ENGINES", "SHORT RANGE SENSORS", "LONG RANGE SENSORS", "PHASER CONTROL",
      "PHOTON TUBES", "DAMAGE CONTROL", "SHIELD CONTROL", "LIBRARY-COMPUTER"
    ),
    k_total = k_total,
    k_initial = k_total,
    b_total = b_total,
    klingons = matrix(0, nrow = 3, ncol = 3),
    quadrant_grid = matrix("   ", nrow = 8, ncol = 8)
  )
}

place_quadrant <- function(g) {
  k3 <- g$galaxy[g$q1, g$q2] %/% 100
  b3 <- (g$galaxy[g$q1, g$q2] %/% 10) %% 10
  s3 <- g$galaxy[g$q1, g$q2] %% 10

  grid <- matrix("   ", nrow = 8, ncol = 8)
  klingons <- matrix(0, nrow = 3, ncol = 3)

  grid[g$s1, g$s2] <- "<*>"

  sample_empty <- function() {
    repeat {
      r1 <- rand_sector(); r2 <- rand_sector()
      if (grid[r1, r2] == "   ") return(c(r1, r2))
    }
  }

  if (k3 > 0) {
    for (i in 1:k3) {
      p <- sample_empty()
      grid[p[1], p[2]] <- "+K+"
      klingons[i, ] <- c(p[1], p[2], g$shield_max_hit * (0.5 + runif(1)))
    }
  }

  if (b3 > 0) {
    p <- sample_empty()
    grid[p[1], p[2]] <- ">!<"
  }

  if (s3 > 0) {
    for (i in 1:s3) {
      p <- sample_empty()
      grid[p[1], p[2]] <- " * "
    }
  }

  g$quadrant_grid <- grid
  g$klingons <- klingons
  g$known[g$q1, g$q2] <- g$galaxy[g$q1, g$q2]
  g
}

short_scan <- function(g) {
  lines <- character()
  g$docked <- FALSE

  for (i in max(1, g$s1 - 1):min(8, g$s1 + 1)) {
    for (j in max(1, g$s2 - 1):min(8, g$s2 + 1)) {
      if (g$quadrant_grid[i, j] == ">!<") g$docked <- TRUE
    }
  }

  k3 <- sum(g$klingons[, 3] > 0)
  cond <- if (g$docked) "DOCKED" else if (k3 > 0) "*RED*" else if (g$energy < g$energy0 * 0.1) "YELLOW" else "GREEN"

  if (g$docked) {
    g$energy <- g$energy0
    g$torpedoes <- g$torpedoes0
    g$shields <- 0
    lines <- c(lines, "SHIELDS DROPPED FOR DOCKING PURPOSES")
  }

  if (g$devices[2] < 0) {
    lines <- c(lines, "*** SHORT RANGE SENSORS ARE OUT ***")
    return(list(game = g, text = paste(lines, collapse = "\n")))
  }

  board <- apply(g$quadrant_grid, 2, paste0, collapse = "")
  board_lines <- vapply(1:8, function(r) paste(g$quadrant_grid[r, ], collapse = ""), character(1))

  status <- c(
    sprintf("STARDATE           %.1f", floor(g$stardate * 10) / 10),
    sprintf("CONDITION          %s", cond),
    sprintf("QUADRANT           %d,%d", g$q1, g$q2),
    sprintf("SECTOR             %d,%d", g$s1, g$s2),
    sprintf("PHOTON TORPEDOES   %d", g$torpedoes),
    sprintf("TOTAL ENERGY       %d", floor(g$energy + g$shields)),
    sprintf("SHIELDS            %d", floor(g$shields)),
    sprintf("KLINGONS REMAINING %d", floor(g$k_total))
  )

  lines <- c(lines, "---------------------------------",
             sprintf("%s    %s", board_lines[1], status[1]),
             sprintf("%s    %s", board_lines[2], status[2]),
             sprintf("%s    %s", board_lines[3], status[3]),
             sprintf("%s    %s", board_lines[4], status[4]),
             sprintf("%s    %s", board_lines[5], status[5]),
             sprintf("%s    %s", board_lines[6], status[6]),
             sprintf("%s    %s", board_lines[7], status[7]),
             sprintf("%s    %s", board_lines[8], status[8]),
             "---------------------------------")

  list(game = g, text = paste(lines, collapse = "\n"))
}

klingon_attack <- function(g) {
  if (sum(g$klingons[, 3] > 0) <= 0) return(list(game = g, text = NULL))
  if (g$docked) return(list(game = g, text = "STARBASE SHIELDS PROTECT THE ENTERPRISE"))

  lines <- character()
  for (i in 1:3) {
    if (g$klingons[i, 3] <= 0) next
    d <- sqrt((g$klingons[i, 1] - g$s1)^2 + (g$klingons[i, 2] - g$s2)^2)
    hit <- floor((g$klingons[i, 3] / max(0.5, d)) * (2 + runif(1)))
    g$shields <- g$shields - hit
    g$klingons[i, 3] <- g$klingons[i, 3] / (3 + runif(1))

    lines <- c(lines, sprintf("%d UNIT HIT ON ENTERPRISE FROM SECTOR %d,%d", hit, g$klingons[i, 1], g$klingons[i, 2]))
    if (g$shields <= 0) {
      g$game_over <- TRUE
      lines <- c(lines, "THE ENTERPRISE HAS BEEN DESTROYED. THE FEDERATION WILL BE CONQUERED.")
      return(list(game = g, text = paste(lines, collapse = "\n")))
    }
    lines <- c(lines, sprintf("<SHIELDS DOWN TO %d UNITS>", floor(g$shields)))

    if (hit >= 20 && runif(1) <= 0.4 && hit / max(1, g$shields) > 0.02) {
      r1 <- sample(1:8, 1)
      g$devices[r1] <- g$devices[r1] - hit / max(1, g$shields) - 0.5 * runif(1)
      lines <- c(lines, sprintf("DAMAGE CONTROL REPORTS %s DAMAGED BY THE HIT", g$devices_names[r1]))
    }
  }

  list(game = g, text = paste(lines, collapse = "\n"))
}

enter_quadrant <- function(g, new_game = FALSE) {
  if (g$q1 < 1 || g$q1 > 8 || g$q2 < 1 || g$q2 > 8) {
    g$game_over <- TRUE
    return(list(game = g, text = "INVALID QUADRANT"))
  }

  g <- place_quadrant(g)
  k3 <- sum(g$klingons[, 3] > 0)
  lines <- character()

  if (new_game) {
    lines <- c(lines,
      "YOUR ORDERS ARE AS FOLLOWS:",
      sprintf("DESTROY THE %d KLINGON WARSHIPS BEFORE STARDATE %.1f.", g$k_total, g$initial_stardate + g$mission_days),
      sprintf("THIS GIVES YOU %d DAYS. THERE %s %d STARBASE%s IN THE GALAXY.",
              g$mission_days,
              ifelse(g$b_total == 1, "IS", "ARE"),
              g$b_total,
              ifelse(g$b_total == 1, "", "S")),
      sprintf("YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED IN %s.", quadrant_name(g$q1, g$q2))
    )
  } else {
    lines <- c(lines, sprintf("NOW ENTERING %s QUADRANT...", quadrant_name(g$q1, g$q2)))
  }

  if (k3 > 0) {
    lines <- c(lines, "COMBAT AREA CONDITION RED")
    if (g$shields <= 200) lines <- c(lines, "SHIELDS DANGEROUSLY LOW")
  }

  ss <- short_scan(g)
  g <- ss$game
  lines <- c(lines, ss$text)

  list(game = g, text = paste(lines, collapse = "\n"))
}

course_vectors <- data.frame(
  dx = c(0, -1, -1, -1, 0, 1, 1, 1, 0),
  dy = c(1, 1, 0, -1, -1, -1, 0, 1, 1)
)

nav_move <- function(g, course, warp) {
  if (course == 9) course <- 1
  if (!(course >= 1 && course < 9)) return(list(game = g, text = "INCORRECT COURSE DATA, SIR!"))

  max_warp <- if (g$devices[1] < 0) 0.2 else 8
  if (warp > max_warp) return(list(game = g, text = sprintf("WARP ENGINES ARE DAMAGED. MAXIMUM SPEED = WARP %.1f", max_warp)))
  if (warp <= 0 || warp > 8) return(list(game = g, text = "THE ENGINES WON'T TAKE THAT WARP FACTOR."))

  n <- floor(warp * 8 + 0.5)
  if (g$energy < n) return(list(game = g, text = "INSUFFICIENT ENERGY AVAILABLE FOR MANEUVERING."))

  # klingons shuffle
  for (i in 1:3) {
    if (g$klingons[i, 3] <= 0) next
    g$quadrant_grid[g$klingons[i, 1], g$klingons[i, 2]] <- "   "
    repeat {
      r1 <- rand_sector(); r2 <- rand_sector()
      if (g$quadrant_grid[r1, r2] == "   ") break
    }
    g$klingons[i, 1:2] <- c(r1, r2)
    g$quadrant_grid[r1, r2] <- "+K+"
  }

  ci <- floor(course)
  frac <- course - ci
  dx <- course_vectors$dx[ci] + (course_vectors$dx[ci + 1] - course_vectors$dx[ci]) * frac
  dy <- course_vectors$dy[ci] + (course_vectors$dy[ci + 1] - course_vectors$dy[ci]) * frac

  g$quadrant_grid[g$s1, g$s2] <- "   "
  x <- g$s1; y <- g$s2
  old_q <- c(g$q1, g$q2)
  lines <- character()

  for (i in 1:n) {
    x <- x + dx; y <- y + dy
    if (x < 1 || x >= 9 || y < 1 || y >= 9) break
    xi <- floor(x); yi <- floor(y)
    if (g$quadrant_grid[xi, yi] != "   ") {
      x <- floor(x - dx); y <- floor(y - dy)
      lines <- c(lines, sprintf("WARP ENGINES SHUT DOWN AT SECTOR %d,%d DUE TO BAD NAVIGATION", x, y))
      break
    }
  }

  if (x < 1 || x >= 9 || y < 1 || y >= 9) {
    gx <- 8 * g$q1 + g$s1 + n * dx
    gy <- 8 * g$q2 + g$s2 + n * dy
    g$q1 <- floor(gx / 8)
    g$q2 <- floor(gy / 8)
    g$s1 <- floor(gx - g$q1 * 8)
    g$s2 <- floor(gy - g$q2 * 8)
    if (g$s1 == 0) { g$q1 <- g$q1 - 1; g$s1 <- 8 }
    if (g$s2 == 0) { g$q2 <- g$q2 - 1; g$s2 <- 8 }
    g$q1 <- min(max(g$q1, 1), 8)
    g$q2 <- min(max(g$q2, 1), 8)
    g$s1 <- min(max(g$s1, 1), 8)
    g$s2 <- min(max(g$s2, 1), 8)
    g$stardate <- g$stardate + 1
  } else {
    g$s1 <- floor(x); g$s2 <- floor(y)
  }

  g$quadrant_grid[g$s1, g$s2] <- "<*>"
  g$energy <- g$energy - n - 10
  if (g$energy < 0) {
    g$shields <- g$shields + g$energy
    g$energy <- 0
  }

  t8 <- if (warp < 1) 0.1 * floor(10 * warp) else 1
  g$stardate <- g$stardate + t8

  # random device repair/damage
  for (i in 1:8) {
    if (g$devices[i] < 0) {
      g$devices[i] <- g$devices[i] + ifelse(warp >= 1, 1, warp)
      if (g$devices[i] > -0.1 && g$devices[i] < 0) g$devices[i] <- -0.1
      if (g$devices[i] >= 0) lines <- c(lines, sprintf("DAMAGE CONTROL REPORT: %s REPAIR COMPLETED.", g$devices_names[i]))
    }
  }

  if (runif(1) <= 0.2) {
    r <- sample(1:8, 1)
    if (runif(1) < 0.6) {
      g$devices[r] <- g$devices[r] - (runif(1) * 5 + 1)
      lines <- c(lines, sprintf("DAMAGE CONTROL REPORT: %s DAMAGED", g$devices_names[r]))
    } else {
      g$devices[r] <- g$devices[r] + runif(1) * 3 + 1
      lines <- c(lines, sprintf("DAMAGE CONTROL REPORT: %s STATE OF REPAIR IMPROVED", g$devices_names[r]))
    }
  }

  if (g$stardate > g$initial_stardate + g$mission_days) g$game_over <- TRUE

  if (!all(old_q == c(g$q1, g$q2))) {
    eq <- enter_quadrant(g, new_game = FALSE)
    g <- eq$game
    lines <- c(lines, eq$text)
  } else {
    ss <- short_scan(g)
    g <- ss$game
    lines <- c(lines, ss$text)
  }

  atk <- klingon_attack(g)
  g <- atk$game
  if (!is.null(atk$text) && nzchar(atk$text)) lines <- c(lines, atk$text)

  list(game = g, text = paste(lines, collapse = "\n"))
}

long_range_scan <- function(g) {
  if (g$devices[3] < 0) return(list(game = g, text = "LONG RANGE SENSORS ARE INOPERABLE"))

  lines <- c(sprintf("LONG RANGE SCAN FOR QUADRANT %d,%d", g$q1, g$q2), "-------------------")
  for (i in (g$q1 - 1):(g$q1 + 1)) {
    row <- c()
    for (j in (g$q2 - 1):(g$q2 + 1)) {
      if (i > 0 && i < 9 && j > 0 && j < 9) {
        row <- c(row, sprintf("%03d", g$galaxy[i, j]))
        g$known[i, j] <- g$galaxy[i, j]
      } else row <- c(row, "***")
    }
    lines <- c(lines, paste0(": ", paste(row, collapse = " "), " :"), "-------------------")
  }

  list(game = g, text = paste(lines, collapse = "\n"))
}

fire_phasers <- function(g, x) {
  if (g$devices[4] < 0) return(list(game = g, text = "PHASERS INOPERATIVE"))
  k3 <- sum(g$klingons[, 3] > 0)
  if (k3 <= 0) return(list(game = g, text = "SENSORS SHOW NO ENEMY SHIPS IN THIS QUADRANT"))
  if (x <= 0 || x > g$energy) return(list(game = g, text = "INVALID PHASER ENERGY"))

  lines <- character()
  if (g$devices[8] < 0) lines <- c(lines, "COMPUTER FAILURE HAMPERS ACCURACY")

  g$energy <- g$energy - x
  if (g$devices[7] < 0) x <- x * runif(1)
  h1 <- floor(x / k3)

  for (i in 1:3) {
    if (g$klingons[i, 3] <= 0) next
    d <- sqrt((g$klingons[i, 1] - g$s1)^2 + (g$klingons[i, 2] - g$s2)^2)
    h <- floor((h1 / max(0.5, d)) * (runif(1) + 2))

    if (h <= 0.15 * g$klingons[i, 3]) {
      lines <- c(lines, sprintf("SENSORS SHOW NO DAMAGE TO ENEMY AT %d,%d", g$klingons[i, 1], g$klingons[i, 2]))
      next
    }

    g$klingons[i, 3] <- g$klingons[i, 3] - h
    lines <- c(lines, sprintf("%d UNIT HIT ON KLINGON AT SECTOR %d,%d", h, g$klingons[i, 1], g$klingons[i, 2]))

    if (g$klingons[i, 3] <= 0) {
      lines <- c(lines, "*** KLINGON DESTROYED ***")
      g$k_total <- g$k_total - 1
      g$quadrant_grid[g$klingons[i, 1], g$klingons[i, 2]] <- "   "
      g$klingons[i, 3] <- 0
      g$galaxy[g$q1, g$q2] <- g$galaxy[g$q1, g$q2] - 100
      g$known[g$q1, g$q2] <- g$galaxy[g$q1, g$q2]
    } else {
      lines <- c(lines, sprintf("(SENSORS SHOW %d UNITS REMAINING)", floor(g$klingons[i, 3])))
    }
  }

  if (g$k_total <= 0) {
    g$won <- TRUE
    g$game_over <- TRUE
    lines <- c(lines, sprintf("CONGRATULATIONS, CAPTAIN! EFFICIENCY RATING %.1f", 1000 * (g$k_initial / (g$stardate - g$initial_stardate))^2))
    return(list(game = g, text = paste(lines, collapse = "\n")))
  }

  atk <- klingon_attack(g)
  g <- atk$game
  if (!is.null(atk$text) && nzchar(atk$text)) lines <- c(lines, atk$text)

  list(game = g, text = paste(lines, collapse = "\n"))
}

fire_torpedo <- function(g, course) {
  if (g$torpedoes <= 0) return(list(game = g, text = "ALL PHOTON TORPEDOES EXPENDED"))
  if (g$devices[5] < 0) return(list(game = g, text = "PHOTON TUBES ARE NOT OPERATIONAL"))
  if (course == 9) course <- 1
  if (!(course >= 1 && course < 9)) return(list(game = g, text = "INCORRECT COURSE DATA, SIR!"))

  ci <- floor(course)
  frac <- course - ci
  dx <- course_vectors$dx[ci] + (course_vectors$dx[ci + 1] - course_vectors$dx[ci]) * frac
  dy <- course_vectors$dy[ci] + (course_vectors$dy[ci + 1] - course_vectors$dy[ci]) * frac

  g$energy <- g$energy - 2
  g$torpedoes <- g$torpedoes - 1

  x <- g$s1; y <- g$s2
  lines <- "TORPEDO TRACK:"
  repeat {
    x <- x + dx; y <- y + dy
    x3 <- floor(x + 0.5); y3 <- floor(y + 0.5)
    if (x3 < 1 || x3 > 8 || y3 < 1 || y3 > 8) {
      lines <- c(lines, "TORPEDO MISSED")
      break
    }
    lines <- c(lines, sprintf("%d,%d", x3, y3))
    cell <- g$quadrant_grid[x3, y3]
    if (cell == "   ") next

    if (cell == "+K+") {
      lines <- c(lines, "*** KLINGON DESTROYED ***")
      for (i in 1:3) {
        if (g$klingons[i, 3] > 0 && g$klingons[i, 1] == x3 && g$klingons[i, 2] == y3) {
          g$klingons[i, 3] <- 0
          break
        }
      }
      g$quadrant_grid[x3, y3] <- "   "
      g$k_total <- g$k_total - 1
      g$galaxy[g$q1, g$q2] <- g$galaxy[g$q1, g$q2] - 100
      g$known[g$q1, g$q2] <- g$galaxy[g$q1, g$q2]
      break
    }

    if (cell == " * ") {
      lines <- c(lines, sprintf("STAR AT %d,%d ABSORBED TORPEDO ENERGY.", x3, y3))
      break
    }

    if (cell == ">!<") {
      lines <- c(lines, "*** STARBASE DESTROYED ***", "STARFLEET COMMAND IS REVIEWING YOUR RECORD FOR COURT MARTIAL!")
      g$b_total <- g$b_total - 1
      g$quadrant_grid[x3, y3] <- "   "
      g$galaxy[g$q1, g$q2] <- g$galaxy[g$q1, g$q2] - 10
      g$known[g$q1, g$q2] <- g$galaxy[g$q1, g$q2]
      break
    }
  }

  if (g$k_total <= 0) {
    g$won <- TRUE
    g$game_over <- TRUE
    lines <- c(lines, sprintf("CONGRATULATIONS, CAPTAIN! EFFICIENCY RATING %.1f", 1000 * (g$k_initial / (g$stardate - g$initial_stardate))^2))
    return(list(game = g, text = paste(lines, collapse = "\n")))
  }

  atk <- klingon_attack(g)
  g <- atk$game
  if (!is.null(atk$text) && nzchar(atk$text)) lines <- c(lines, atk$text)

  list(game = g, text = paste(lines, collapse = "\n"))
}

shield_control <- function(g, x) {
  if (g$devices[7] < 0) return(list(game = g, text = "SHIELD CONTROL INOPERABLE"))
  if (x < 0 || x > (g$energy + g$shields)) return(list(game = g, text = "SHIELDS UNCHANGED"))
  g$energy <- g$energy + g$shields - x
  g$shields <- x
  list(game = g, text = sprintf("SHIELDS NOW AT %d UNITS.", floor(g$shields)))
}

damage_report <- function(g) {
  lines <- c("DEVICE                 STATE OF REPAIR")
  for (i in 1:8) lines <- c(lines, sprintf("%-22s %.2f", g$devices_names[i], floor(g$devices[i] * 100) / 100))
  list(game = g, text = paste(lines, collapse = "\n"))
}

computer_report <- function(g, mode) {
  mode <- as.integer(mode)
  if (g$devices[8] < 0) return(list(game = g, text = "COMPUTER DISABLED"))

  if (is.na(mode) || mode < 0 || mode > 5) {
    return(list(game = g, text = paste(
      "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:",
      "0 = CUMULATIVE GALACTIC RECORD",
      "1 = STATUS REPORT",
      "2 = PHOTON TORPEDO DATA",
      "3 = STARBASE NAV DATA",
      "4 = DIRECTION/DISTANCE CALCULATOR (use COM 4 x1 y1 x2 y2)",
      "5 = GALAXY REGION NAME MAP",
      sep = "\n"
    )))
  }

  if (mode == 0) {
    lines <- c(sprintf("COMPUTER RECORD OF GALAXY FOR QUADRANT %d,%d", g$q1, g$q2),
               "     1   2   3   4   5   6   7   8")
    for (i in 1:8) {
      row <- vapply(1:8, function(j) if (g$known[i, j] == 0) "***" else sprintf("%03d", g$known[i, j]), character(1))
      lines <- c(lines, sprintf("%d  %s", i, paste(row, collapse = " ")))
    }
    return(list(game = g, text = paste(lines, collapse = "\n")))
  }

  if (mode == 1) {
    lines <- c("STATUS REPORT:",
               sprintf("KLINGONS LEFT: %d", g$k_total),
               sprintf("MISSION MUST BE COMPLETED IN %.1f STARDATES", floor((g$initial_stardate + g$mission_days - g$stardate) * 10) / 10),
               sprintf("STARBASES IN GALAXY: %d", g$b_total))
    dr <- damage_report(g)
    return(list(game = g, text = paste(c(lines, "", dr$text), collapse = "\n")))
  }

  if (mode == 2) {
    lines <- "FROM ENTERPRISE TO KLINGON BATTLE CRUISER(S):"
    for (i in 1:3) {
      if (g$klingons[i, 3] <= 0) next
      dx <- g$klingons[i, 1] - g$s1
      dy <- g$klingons[i, 2] - g$s2
      direction <- atan2(dx, dy) * 180 / pi
      direction <- (direction %% 360) / 45 + 1
      dist <- sqrt(dx^2 + dy^2)
      lines <- c(lines, sprintf("Target at %d,%d -> direction %.2f distance %.2f", g$klingons[i, 1], g$klingons[i, 2], direction, dist))
    }
    return(list(game = g, text = paste(lines, collapse = "\n")))
  }

  if (mode == 3) {
    idx <- which(g$quadrant_grid == ">!<", arr.ind = TRUE)
    if (nrow(idx) == 0) return(list(game = g, text = "SENSORS SHOW NO STARBASES IN THIS QUADRANT."))
    dx <- idx[1, 1] - g$s1
    dy <- idx[1, 2] - g$s2
    direction <- (atan2(dx, dy) * 180 / pi %% 360) / 45 + 1
    dist <- sqrt(dx^2 + dy^2)
    return(list(game = g, text = sprintf("FROM ENTERPRISE TO STARBASE: direction %.2f distance %.2f", direction, dist)))
  }

  if (mode == 5) {
    lines <- "THE GALAXY"
    for (i in 1:8) {
      lines <- c(lines, sprintf("%s | %s", quadrant_name(i, 1, TRUE), quadrant_name(i, 5, TRUE)))
    }
    return(list(game = g, text = paste(lines, collapse = "\n")))
  }

  list(game = g, text = "Use COM 4 x1 y1 x2 y2 for manual direction/distance calculation.")
}

parse_command <- function(g, input_line) {
  tokens <- strsplit(trimws(input_line), "\\s+")[[1]]
  if (length(tokens) == 0 || tokens[1] == "") return(list(game = g, text = "ENTER A COMMAND"))

  cmd <- toupper(tokens[1])
  n <- function(i, default = NA_real_) if (length(tokens) >= i) suppressWarnings(as.numeric(tokens[i])) else default

  out <- switch(cmd,
    "NAV" = nav_move(g, n(2), n(3)),
    "SRS" = short_scan(g),
    "LRS" = long_range_scan(g),
    "PHA" = fire_phasers(g, n(2)),
    "TOR" = fire_torpedo(g, n(2)),
    "SHE" = shield_control(g, n(2)),
    "DAM" = damage_report(g),
    "COM" = computer_report(g, n(2)),
    "XXX" = {
      g$game_over <- TRUE
      list(game = g, text = "YOU RESIGNED YOUR COMMAND.")
    },
    list(game = g, text = "UNKNOWN COMMAND. USE NAV/SRS/LRS/PHA/TOR/SHE/DAM/COM/XXX")
  )

  g <- out$game
  if (!g$game_over && g$stardate > g$initial_stardate + g$mission_days) {
    g$game_over <- TRUE
    out$text <- paste(out$text, "\nTIME HAS RUN OUT. THE FEDERATION HAS BEEN CONQUERED.")
    out$game <- g
  }

  out
}

ui <- fluidPage(
  titlePanel("Super Star Trek (BASIC → R Shiny)"),
  fluidRow(
    column(
      7,
      tags$p("Enter commands as in the original game, e.g. NAV 8 1, PHA 500, TOR 6, COM 0."),
      textInput("cmd", "Command", placeholder = "NAV 8 1"),
      actionButton("run", "Execute"),
      actionButton("restart", "New Mission"),
      tags$hr(),
      verbatimTextOutput("console")
    ),
    column(
      5,
      h4("Quick reference"),
      tags$ul(
        tags$li("NAV course warp"),
        tags$li("SRS, LRS"),
        tags$li("PHA energy"),
        tags$li("TOR course"),
        tags$li("SHE shield_energy"),
        tags$li("DAM"),
        tags$li("COM 0..5"),
        tags$li("XXX")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(game = NULL, log = character())

  boot <- function() {
    g <- init_game()
    eq <- enter_quadrant(g, new_game = TRUE)
    rv$game <- eq$game
    rv$log <- c("SUPER STAR TREK - 1978 BASIC TRANSCRIPTION", eq$text)
  }

  boot()

  observeEvent(input$restart, {
    boot()
  })

  observeEvent(input$run, {
    req(rv$game)
    if (rv$game$game_over) {
      rv$log <- c(rv$log, "GAME OVER. CLICK 'NEW MISSION' TO PLAY AGAIN.")
      return()
    }

    line <- input$cmd
    rv$log <- c(rv$log, paste0("> ", line))
    res <- parse_command(rv$game, line)
    rv$game <- res$game
    rv$log <- c(rv$log, res$text)

    if (rv$game$game_over) {
      if (rv$game$won) {
        rv$log <- c(rv$log, "MISSION COMPLETE. VICTORY!")
      } else {
        rv$log <- c(rv$log, sprintf("MISSION FAILED. %d KLINGON BATTLE CRUISERS REMAIN.", rv$game$k_total))
      }
    }

    updateTextInput(session, "cmd", value = "")
  })

  output$console <- renderText({
    paste(tail(rv$log, 120), collapse = "\n")
  })
}

shinyApp(ui, server)
