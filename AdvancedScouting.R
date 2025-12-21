# ============================================================
# MATCHUP EXPLORER v6.0 - HITTER SCOUTING CARDS
# Based on actual TrackMan data structure and splits logic
# ============================================================

library(tidyverse)
library(shiny)
library(gt)
library(gtExtras)
library(bslib)

# ============================================================
# 1. LOAD DATA AND CALCULATE GRADES LIVE
# ============================================================

cat("Loading TrackMan data...\n")

# Load TrackMan data
if (exists("TM25")) {
  tm_data <- TM25
  cat("Using TM25 from environment\n")
} else if (file.exists("trackman_combined.rds")) {
  tm_data <- readRDS("trackman_combined.rds")
} else if (file.exists("TM25.rds")) {
  tm_data <- readRDS("TM25.rds")
} else {
  stop("No TrackMan data found! Need TM25 in environment or trackman_combined.rds/TM25.rds file.")
}

# ---- RV/100 Calculation (EXACT USER CODE) ----
weights_vec <- c(
  "Ball" = -0.0892681735045099,
  "Called Strike" = 0.121194020810818,
  "Double" = -0.859369443968486,
  "Field Out" = 0.327827741132251,
  "Foul Ball" = 0.0709399019613328,
  "HBP" = -0.423817625210998,
  "Home Run" = -1.42174059796598,
  "Single" = -0.556264454815573,
  "Triple" = -1.09689513374671,
  "Whiff" = 0.174095217814355,
  "NA" = 0.0110712767697598
)

map_dre_event <- function(PitchCall, PlayResult) {
  dplyr::case_when(
    PitchCall %in% c("BallCalled","BallinDirt") ~ "Ball",
    PitchCall == "StrikeCalled" ~ "Called Strike",
    PitchCall == "StrikeSwinging" ~ "Whiff",
    PitchCall %in% c("FoulBall", "FoulBallNotFieldable","FoulBallFieldable") ~ "Foul Ball",
    PitchCall == "HitByPitch" ~ "HBP",
    PitchCall == "InPlay" & PlayResult == "Single" ~ "Single",
    PitchCall == "InPlay" & PlayResult == "Double" ~ "Double",
    PitchCall == "InPlay" & PlayResult == "Triple" ~ "Triple",
    PitchCall == "InPlay" & PlayResult == "HomeRun" ~ "Home Run",
    PitchCall == "InPlay" & PlayResult %in% c("Out", "FieldersChoice", "Error") ~ "Field Out",
    TRUE ~ "NA"
  )
}

calculate_rv100 <- function(df) {
  df %>%
    mutate(
      dre_event = map_dre_event(PitchCall, PlayResult),
      rv_pitcher = as.numeric(weights_vec[dre_event]),
      rv_pitcher = ifelse(is.na(rv_pitcher), weights_vec["NA"], rv_pitcher),
      hitter_rv = -rv_pitcher
    )
}

# Get lists
all_hitters <- sort(unique(tm_data$Batter[!is.na(tm_data$Batter)]))
all_pitchers <- sort(unique(tm_data$Pitcher[!is.na(tm_data$Pitcher)]))
hitter_hands <- tm_data %>% 
  filter(!is.na(Batter), !is.na(BatterSide)) %>%
  group_by(Batter) %>%
  summarise(BatterSide = names(which.max(table(BatterSide))), .groups = "drop")
pitcher_hands <- tm_data %>%
  filter(!is.na(Pitcher), !is.na(PitcherThrows)) %>%
  group_by(Pitcher) %>%
  summarise(PitcherThrows = names(which.max(table(PitcherThrows))), .groups = "drop")

# ---- CALCULATE LEAGUE-WIDE GRADE METRICS (RV-BASED) ----
cat("Calculating league-wide grade metrics...\n")

# First, add indicators to the full dataset if not present
if (!"HitIndicator" %in% names(tm_data)) {
  tm_data <- tm_data %>%
    mutate(
      WhiffIndicator = ifelse(PitchCall == "StrikeSwinging", 1, 0),
      StrikeZoneIndicator = ifelse(PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
                                     PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5, 1, 0),
      SwingIndicator = ifelse(PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", 
                                               "FoulBall", "InPlay"), 1, 0),
      ABindicator = ifelse(PlayResult %in% c("Error", "FieldersChoice", "Out", "Single", 
                                             "Double", "Triple", "HomeRun") |
                             KorBB == "Strikeout", 1, 0),
      HitIndicator = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0),
      PAindicator = ifelse(PitchCall %in% c("InPlay", "HitByPitch") |
                             KorBB %in% c("Walk", "Strikeout"), 1, 0),
      totalbases = case_when(
        PlayResult == "Single" ~ 1,
        PlayResult == "Double" ~ 2,
        PlayResult == "Triple" ~ 3,
        PlayResult == "HomeRun" ~ 4,
        TRUE ~ 0
      ),
      Zswing = ifelse(StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0),
      Zwhiffind = ifelse(WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
      Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
      OutofZone = ifelse(StrikeZoneIndicator == 0, 1, 0),
      TwoStrikeInd = ifelse(Strikes == 2, 1, 0)
    )
}

# Calculate per-batter stats with minimum PA threshold
batter_stats <- tm_data %>%
  filter(!is.na(TaggedPitchType)) %>%
  calculate_rv100() %>%
  group_by(Batter) %>%
  summarise(
    n_pitches = n(),
    n_pa = sum(PAindicator, na.rm = TRUE),
    # RV/100 - the core metric
    rv100 = 100 * mean(hitter_rv, na.rm = TRUE),
    # Power metrics
    iso = (sum(totalbases, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))) - 
      (sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))),
    ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
    # Contact metrics
    ba = sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
    zone_con = 100 * (1 - sum(Zwhiffind, na.rm = TRUE) / pmax(1, sum(Zswing, na.rm = TRUE))),
    # Avoid K metrics
    k_pct = 100 * sum(KorBB == "Strikeout", na.rm = TRUE) / pmax(1, sum(PAindicator, na.rm = TRUE)),
    con2k = 100 * (1 - sum(WhiffIndicator[Strikes == 2], na.rm = TRUE) / 
                     pmax(1, sum(SwingIndicator[Strikes == 2], na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  filter(n_pa >= 20)

cat("Found", nrow(batter_stats), "batters with 20+ PA for grade baseline\n")

# Calculate league percentiles for grading (more robust than z-scores)
grade_metrics <- list(
  rv100_p20 = quantile(batter_stats$rv100, 0.10, na.rm = TRUE),
  rv100_p50 = quantile(batter_stats$rv100, 0.50, na.rm = TRUE),
  rv100_p80 = quantile(batter_stats$rv100, 0.90, na.rm = TRUE),
  
  iso_p20 = quantile(batter_stats$iso, 0.10, na.rm = TRUE),
  iso_p50 = quantile(batter_stats$iso, 0.50, na.rm = TRUE),
  iso_p80 = quantile(batter_stats$iso, 0.90, na.rm = TRUE),
  
  ev90_p20 = quantile(batter_stats$ev90, 0.10, na.rm = TRUE),
  ev90_p50 = quantile(batter_stats$ev90, 0.50, na.rm = TRUE),
  ev90_p80 = quantile(batter_stats$ev90, 0.90, na.rm = TRUE),
  
  ba_p20 = quantile(batter_stats$ba, 0.10, na.rm = TRUE),
  ba_p50 = quantile(batter_stats$ba, 0.50, na.rm = TRUE),
  ba_p80 = quantile(batter_stats$ba, 0.90, na.rm = TRUE),
  
  zone_con_p20 = quantile(batter_stats$zone_con, 0.10, na.rm = TRUE),
  zone_con_p50 = quantile(batter_stats$zone_con, 0.50, na.rm = TRUE),
  zone_con_p80 = quantile(batter_stats$zone_con, 0.90, na.rm = TRUE),
  
  k_p20 = quantile(batter_stats$k_pct, 0.90, na.rm = TRUE),  # Inverted - lower is better
  k_p50 = quantile(batter_stats$k_pct, 0.50, na.rm = TRUE),
  k_p80 = quantile(batter_stats$k_pct, 0.10, na.rm = TRUE),
  
  con2k_p20 = quantile(batter_stats$con2k, 0.10, na.rm = TRUE),
  con2k_p50 = quantile(batter_stats$con2k, 0.50, na.rm = TRUE),
  con2k_p80 = quantile(batter_stats$con2k, 0.90, na.rm = TRUE)
)

cat("\nGrade percentile thresholds:\n")
cat("  RV/100: 20th=", round(grade_metrics$rv100_p20, 2), " 50th=", round(grade_metrics$rv100_p50, 2), " 80th=", round(grade_metrics$rv100_p80, 2), "\n")
cat("  ISO: 20th=", round(grade_metrics$iso_p20, 3), " 50th=", round(grade_metrics$iso_p50, 3), " 80th=", round(grade_metrics$iso_p80, 3), "\n")
cat("  EV90: 20th=", round(grade_metrics$ev90_p20, 1), " 50th=", round(grade_metrics$ev90_p50, 1), " 80th=", round(grade_metrics$ev90_p80, 1), "\n")
cat("  BA: 20th=", round(grade_metrics$ba_p20, 3), " 50th=", round(grade_metrics$ba_p50, 3), " 80th=", round(grade_metrics$ba_p80, 3), "\n")
cat("  ZCon: 20th=", round(grade_metrics$zone_con_p20, 1), " 50th=", round(grade_metrics$zone_con_p50, 1), " 80th=", round(grade_metrics$zone_con_p80, 1), "\n")
cat("  2KCon: 20th=", round(grade_metrics$con2k_p20, 1), " 50th=", round(grade_metrics$con2k_p50, 1), " 80th=", round(grade_metrics$con2k_p80, 1), "\n")

# Helper function to convert value to 20-80 grade using percentiles
value_to_grade <- function(val, p20, p50, p80) {
  if (is.na(val)) return(50)
  # Linear interpolation: p20 -> 30, p50 -> 50, p80 -> 70
  if (val <= p20) {
    grade <- 20 + (val - (p20 - (p50 - p20))) / (p50 - p20) * 10
  } else if (val <= p50) {
    grade <- 30 + (val - p20) / (p50 - p20) * 20
  } else if (val <= p80) {
    grade <- 50 + (val - p50) / (p80 - p50) * 20
  } else {
    grade <- 70 + (val - p80) / (p80 - p50) * 10
  }
  round(pmax(20, pmin(80, grade)))
}

# ---- FUNCTION TO CALCULATE GRADES FOR A BATTER (RV-BASED PERCENTILE APPROACH) ----
calculate_hitter_grades <- function(batter_name, team_data, debug = FALSE) {
  batter_data <- team_data %>%
    filter(!is.na(TaggedPitchType), Batter == batter_name) %>%
    calculate_rv100()
  
  if (nrow(batter_data) < 20) {
    return(tibble(
      PitcherThrows = c("Right", "Left"),
      swing_decisions = c(50, 50),
      game_power = c(50, 50),
      raw_power = c(50, 50),
      avoid_k = c(50, 50),
      contact = c(50, 50)
    ))
  }
  
  batter_split <- batter_data %>%
    group_by(PitcherThrows) %>%
    summarise(
      n = n(),
      # RV/100 - core value metric
      rv100 = 100 * mean(hitter_rv, na.rm = TRUE),
      # Power metrics
      iso = (sum(totalbases, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))) - 
        (sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))),
      ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
      # Contact metrics
      ba = sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      zone_con = 100 * (1 - sum(Zwhiffind, na.rm = TRUE) / pmax(1, sum(Zswing, na.rm = TRUE))),
      # Avoid K metrics
      k_pct = 100 * sum(KorBB == "Strikeout", na.rm = TRUE) / pmax(1, sum(PAindicator, na.rm = TRUE)),
      con2k = 100 * (1 - sum(WhiffIndicator[Strikes == 2], na.rm = TRUE) / 
                       pmax(1, sum(SwingIndicator[Strikes == 2], na.rm = TRUE))),
      .groups = "drop"
    )
  
  if (debug) {
    cat("\n--- Debug for", batter_name, "---\n")
    cat("Raw stats:\n")
    print(batter_split)
  }
  
  # Calculate grades using percentile-based approach
  batter_split <- batter_split %>%
    rowwise() %>%
    mutate(
      # Swing Decisions: Based on RV/100 (how much value from pitch selection)
      swing_decisions = value_to_grade(rv100, grade_metrics$rv100_p20, grade_metrics$rv100_p50, grade_metrics$rv100_p80),
      
      # Game Power: ISO + EV90 combined (actual in-game power production)
      iso_grade = value_to_grade(iso, grade_metrics$iso_p20, grade_metrics$iso_p50, grade_metrics$iso_p80),
      ev90_grade = value_to_grade(ev90, grade_metrics$ev90_p20, grade_metrics$ev90_p50, grade_metrics$ev90_p80),
      game_power = round((iso_grade + ev90_grade) / 2),
      
      # Raw Power: EV90 only
      raw_power = value_to_grade(ev90, grade_metrics$ev90_p20, grade_metrics$ev90_p50, grade_metrics$ev90_p80),
      
      # Avoid K: K% (inverted) + 2K Contact Rate
      k_grade = value_to_grade(k_pct, grade_metrics$k_p20, grade_metrics$k_p50, grade_metrics$k_p80),
      con2k_grade = value_to_grade(con2k, grade_metrics$con2k_p20, grade_metrics$con2k_p50, grade_metrics$con2k_p80),
      avoid_k = round((k_grade + con2k_grade) / 2),
      
      # Contact: Zone Contact + Batting Average
      zcon_grade = value_to_grade(zone_con, grade_metrics$zone_con_p20, grade_metrics$zone_con_p50, grade_metrics$zone_con_p80),
      ba_grade = value_to_grade(ba, grade_metrics$ba_p20, grade_metrics$ba_p50, grade_metrics$ba_p80),
      contact = round((zcon_grade + ba_grade) / 2)
    ) %>%
    ungroup()
  
  if (debug) {
    cat("Final grades:\n")
    print(batter_split %>% select(PitcherThrows, swing_decisions, game_power, raw_power, avoid_k, contact))
  }
  
  return(batter_split)
}

cat("Loaded", length(all_pitchers), "pitchers,", length(all_hitters), "hitters\n\n")

# ============================================================
# 2. DATA PROCESSING HELPERS
# ============================================================

# Pitch family classification
classify_pitch_family <- function(pitch_type) {
  case_when(
    pitch_type %in% c("Fastball", "Four-Seam", "Sinker", "FourSeamFastBall", 
                      "TwoSeamFastBall", "FF", "SI", "FB") ~ "FB",
    pitch_type %in% c("Cutter", "Curveball", "Slider", "Sweeper", "Slurve",
                      "CU", "SL", "FC", "SW", "KC") ~ "BB",
    pitch_type %in% c("Changeup", "ChangeUp", "Splitter", "CH", "FS") ~ "OS",
    TRUE ~ "Other"
  )
}

# Add indicators if not present (matches your process_dataset)
# NOTE: Assumes PlateLocSide/Height are in FEET from preprocessing
add_indicators <- function(df) {
  df %>%
    mutate(
      WhiffIndicator = ifelse(PitchCall == "StrikeSwinging", 1, 0),
      StrikeZoneIndicator = ifelse(PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
                                     PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5, 1, 0),
      SwingIndicator = ifelse(PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", 
                                               "FoulBall", "InPlay"), 1, 0),
      BIPind = ifelse(PitchCall == "InPlay" & (is.na(TaggedHitType) | TaggedHitType != "Bunt"), 1, 0),
      ABindicator = ifelse(PlayResult %in% c("Error", "FieldersChoice", "Out", "Single", 
                                             "Double", "Triple", "HomeRun") |
                             KorBB == "Strikeout", 1, 0),
      HitIndicator = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0),
      PAindicator = ifelse(PitchCall %in% c("InPlay", "HitByPitch", "CatchersInterference") |
                             KorBB %in% c("Walk", "Strikeout"), 1, 0),
      totalbases = case_when(
        PlayResult == "Single" ~ 1,
        PlayResult == "Double" ~ 2,
        PlayResult == "Triple" ~ 3,
        PlayResult == "HomeRun" ~ 4,
        TRUE ~ 0
      ),
      Zswing = ifelse(StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0),
      Zwhiffind = ifelse(WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
      Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
      OutofZone = ifelse(StrikeZoneIndicator == 0, 1, 0),
      SCind = ifelse((PitchCall == "InPlay" &
                        ((ExitSpeed > 95 & Angle >= 0 & Angle <= 35) |
                           (ExitSpeed > 92 & Angle >= 8 & Angle <= 35))), 1, 0),
      LA1030ind = ifelse(PitchCall == "InPlay" & Angle >= 10 & Angle <= 30, 1, 0),
      FPindicator = ifelse(Balls == 0 & Strikes == 0, 1, 0),
      TwoStrikeInd = ifelse(Strikes == 2, 1, 0),
      PitchFamily = classify_pitch_family(TaggedPitchType)
    )
}

# Add zone bins for splits analysis (in feet)
add_zone_bins <- function(data) {
  data %>%
    mutate(
      xbin = case_when(
        is.na(PlateLocSide) | is.na(BatterSide) ~ NA_integer_,
        BatterSide == "Right" & PlateLocSide <= -0.28 ~ 1L,
        BatterSide == "Right" & PlateLocSide >= 0.28 ~ 3L,
        BatterSide == "Left" & PlateLocSide >= 0.28 ~ 1L,
        BatterSide == "Left" & PlateLocSide <= -0.28 ~ 3L,
        TRUE ~ 2L
      ),
      ybin = case_when(
        is.na(PlateLocHeight) ~ NA_integer_,
        PlateLocHeight <= 2.0 ~ 1L,
        PlateLocHeight >= 3.0 ~ 3L,
        TRUE ~ 2L
      ),
      zone_name = case_when(
        xbin == 2 & ybin == 2 ~ "Heart",
        xbin == 1 ~ "In",
        xbin == 3 ~ "Away",
        ybin == 1 ~ "Down",
        ybin == 3 ~ "Up",
        TRUE ~ "Middle"
      )
    )
}

# Calculate RV/100 helper - using correct DRE weights
calculate_rv100 <- function(data) {
  if (!"hitter_rv" %in% names(data)) {
    # DRE weights (pitcher perspective)
    weights_vec <- c(
      "Ball" = -0.0892681735045099,
      "Called Strike" = 0.121194020810818,
      "Double" = -0.859369443968486,
      "Field Out" = 0.327827741132251,
      "Foul Ball" = 0.0709399019613328,
      "HBP" = -0.423817625210998,
      "Home Run" = -1.42174059796598,
      "Single" = -0.556264454815573,
      "Triple" = -1.09689513374671,
      "Whiff" = 0.174095217814355,
      "NA" = 0.0110712767697598
    )
    
    data <- data %>%
      mutate(
        dre_event = case_when(
          PitchCall %in% c("BallCalled", "BallinDirt") ~ "Ball",
          PitchCall == "StrikeCalled" ~ "Called Strike",
          PitchCall == "StrikeSwinging" ~ "Whiff",
          PitchCall %in% c("FoulBall", "FoulBallNotFieldable", "FoulBallFieldable") ~ "Foul Ball",
          PitchCall == "HitByPitch" ~ "HBP",
          PitchCall == "InPlay" & PlayResult == "Single" ~ "Single",
          PitchCall == "InPlay" & PlayResult == "Double" ~ "Double",
          PitchCall == "InPlay" & PlayResult == "Triple" ~ "Triple",
          PitchCall == "InPlay" & PlayResult == "HomeRun" ~ "Home Run",
          PitchCall == "InPlay" & PlayResult %in% c("Out", "FieldersChoice", "Error") ~ "Field Out",
          TRUE ~ "NA"
        ),
        rv_pitcher = weights_vec[dre_event],
        rv_pitcher = ifelse(is.na(rv_pitcher), weights_vec["NA"], rv_pitcher),
        hitter_rv = -rv_pitcher
      )
  }
  data
}

# Create compact FB splits for scouting card
create_fb_splits_mini <- function(player_name, team_data) {
  if (is.null(team_data)) return(NULL)
  
  # Include ALL fastball variants
  
  fb_types <- c("Fastball", "Four-Seam", "Sinker", "FourSeamFastBall", "TwoSeamFastBall", "FF", "SI")
  
  df <- team_data %>%
    filter(Batter == player_name, TaggedPitchType %in% fb_types) %>%
    mutate(
      # Movement-based splits
      fb_ride = InducedVertBreak >= 18,
      fb_cut = HorzBreak >= -5 & HorzBreak <= 5,
      fb_soft = RelSpeed <= 88,
      fb_hard = RelSpeed >= 92,
      fb_elevated = PlateLocHeight >= 2.8667,  # ~34.4 inches in feet
      fb_sink = InducedVertBreak < 9 & abs(HorzBreak) >= 10,
      fb_deadzone = InducedVertBreak >= 10 & InducedVertBreak <= 16 & abs(HorzBreak) >= 8 & abs(HorzBreak) <= 15,
      
      # Zone splits - MUST account for batter side (in feet)
      pitch_inside = case_when(
        is.na(PlateLocSide) | is.na(BatterSide) ~ FALSE,
        BatterSide == "Right" & PlateLocSide <= -0.28 ~ TRUE,
        BatterSide == "Left" & PlateLocSide >= 0.28 ~ TRUE,
        TRUE ~ FALSE
      ),
      pitch_outside = case_when(
        is.na(PlateLocSide) | is.na(BatterSide) ~ FALSE,
        BatterSide == "Right" & PlateLocSide >= 0.28 ~ TRUE,
        BatterSide == "Left" & PlateLocSide <= -0.28 ~ TRUE,
        TRUE ~ FALSE
      ),
      pitch_down = !is.na(PlateLocHeight) & PlateLocHeight <= 2.0,
      pitch_heart = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
        abs(PlateLocSide) <= 0.28 & PlateLocHeight >= 2.0 & PlateLocHeight <= 3.0
    )
  
  if (nrow(df) < 10) return(NULL)
  
  calc_fb_stats <- function(data) {
    if (nrow(data) < 5) return(tibble(P = 0, BA = NA, SLG = NA, wOBA = NA, `Whiff%` = NA, `Chase%` = NA, `Z-Whiff%` = NA, EV = NA, `CS%` = NA))
    
    # Use summarise like the OLD WORKING code - this calculates everything in one pass
    data %>%
      summarise(
        P = n(),
        BA = sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE),
        SLG = sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE),
        wOBA = (0.69 * sum(KorBB == "Walk", na.rm = TRUE) + 
                  0.72 * sum(PitchCall == "HitByPitch", na.rm = TRUE) +
                  0.89 * sum(PlayResult == "Single", na.rm = TRUE) + 
                  1.27 * sum(PlayResult == "Double", na.rm = TRUE) + 
                  1.62 * sum(PlayResult == "Triple", na.rm = TRUE) + 
                  2.10 * sum(PlayResult == "HomeRun", na.rm = TRUE)) / 
          pmax(1, sum(ABindicator, na.rm = TRUE) + sum(KorBB == "Walk", na.rm = TRUE) + sum(PitchCall == "HitByPitch", na.rm = TRUE)),
        `Whiff%` = sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE) * 100,
        `Chase%` = sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE) * 100,
        `Z-Whiff%` = sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE) * 100,
        EV = mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE),
        `CS%` = sum(PitchCall == "StrikeCalled", na.rm = TRUE) / n() * 100,
        .groups = "drop"
      )
  }
  
  splits <- bind_rows(
    calc_fb_stats(df) %>% mutate(Split = "All FB"),
    calc_fb_stats(df %>% filter(PitcherThrows == "Left")) %>% mutate(Split = "Lefty"),
    calc_fb_stats(df %>% filter(PitcherThrows == "Right")) %>% mutate(Split = "Righty"),
    calc_fb_stats(df %>% filter(fb_ride)) %>% mutate(Split = "Ride"),
    calc_fb_stats(df %>% filter(fb_sink)) %>% mutate(Split = "Sink"),
    calc_fb_stats(df %>% filter(fb_cut)) %>% mutate(Split = "Cut FB"),
    calc_fb_stats(df %>% filter(fb_hard)) %>% mutate(Split = "Hard 92+"),
    calc_fb_stats(df %>% filter(fb_soft)) %>% mutate(Split = "Soft -88"),
    calc_fb_stats(df %>% filter(fb_elevated)) %>% mutate(Split = "Elevated"),
    calc_fb_stats(df %>% filter(pitch_down)) %>% mutate(Split = "Down"),
    calc_fb_stats(df %>% filter(pitch_inside)) %>% mutate(Split = "In"),
    calc_fb_stats(df %>% filter(pitch_outside)) %>% mutate(Split = "Out"),
    calc_fb_stats(df %>% filter(pitch_heart)) %>% mutate(Split = "Heart"),
    calc_fb_stats(df %>% filter(fb_deadzone)) %>% mutate(Split = "Deadzone")
  ) %>%
    filter(P >= 5) %>%
    select(Split, P, BA, SLG, wOBA, `Whiff%`, `Chase%`, `Z-Whiff%`, EV, `CS%`) %>%
    mutate(across(c(BA, SLG, wOBA), ~ifelse(is.na(.) | is.nan(.), NA, round(., 3))),
           across(c(`Whiff%`, `Chase%`, `Z-Whiff%`, `CS%`), ~ifelse(is.na(.) | is.nan(.), NA, round(., 1))),
           EV = ifelse(is.na(EV) | is.nan(EV), NA, round(EV, 1)))
  
  if (nrow(splits) == 0) return(NULL)
  splits
}

# Create compact OS splits for scouting card
create_os_splits_mini <- function(player_name, team_data) {
  if (is.null(team_data)) return(NULL)
  
  # Exclude fastballs and cutter
  fb_types <- c("Fastball", "Four-Seam", "Sinker", "FourSeamFastBall", "TwoSeamFastBall", "FF", "SI", "Cutter", "FC", "Other")
  
  df <- team_data %>%
    filter(Batter == player_name, !TaggedPitchType %in% fb_types, !is.na(TaggedPitchType)) %>%
    mutate(
      is_breaker = TaggedPitchType %in% c("Slider", "Curveball", "Sweeper", "Slurve", "SL", "CU", "KC", "SW"),
      is_change = TaggedPitchType %in% c("ChangeUp", "Changeup", "Splitter", "CH", "FS", "SC"),
      os_hard = is_breaker & RelSpeed >= 84,
      os_soft = is_breaker & RelSpeed < 76,
      os_sweep = is_breaker & abs(HorzBreak) >= 12,
      os_downer = is_breaker & InducedVertBreak <= -8,
      os_gyro = InducedVertBreak >= -3 & InducedVertBreak <= 3 & HorzBreak >= -4 & HorzBreak <= 4
    )
  
  if (nrow(df) < 10) return(NULL)
  
  calc_os_stats <- function(data) {
    if (nrow(data) < 5) return(tibble(P = 0, BA = NA, SLG = NA, wOBA = NA, `Whiff%` = NA, `Chase%` = NA, `Z-Whiff%` = NA, EV = NA, `CS%` = NA))
    
    # Use summarise like the OLD WORKING code
    data %>%
      summarise(
        P = n(),
        BA = sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE),
        SLG = sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE),
        wOBA = (0.69 * sum(KorBB == "Walk", na.rm = TRUE) + 
                  0.72 * sum(PitchCall == "HitByPitch", na.rm = TRUE) +
                  0.89 * sum(PlayResult == "Single", na.rm = TRUE) + 
                  1.27 * sum(PlayResult == "Double", na.rm = TRUE) + 
                  1.62 * sum(PlayResult == "Triple", na.rm = TRUE) + 
                  2.10 * sum(PlayResult == "HomeRun", na.rm = TRUE)) / 
          pmax(1, sum(ABindicator, na.rm = TRUE) + sum(KorBB == "Walk", na.rm = TRUE) + sum(PitchCall == "HitByPitch", na.rm = TRUE)),
        `Whiff%` = sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE) * 100,
        `Chase%` = sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE) * 100,
        `Z-Whiff%` = sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE) * 100,
        EV = mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE),
        `CS%` = sum(PitchCall == "StrikeCalled", na.rm = TRUE) / n() * 100,
        .groups = "drop"
      )
  }
  
  splits <- bind_rows(
    calc_os_stats(df) %>% mutate(Split = "All OS"),
    calc_os_stats(df %>% filter(is_breaker, PitcherThrows == "Right")) %>% mutate(Split = "RH Break"),
    calc_os_stats(df %>% filter(is_breaker, PitcherThrows == "Left")) %>% mutate(Split = "LH Break"),
    calc_os_stats(df %>% filter(is_change, PitcherThrows == "Right")) %>% mutate(Split = "RH CH/SPL"),
    calc_os_stats(df %>% filter(is_change, PitcherThrows == "Left")) %>% mutate(Split = "LH CH/SPL"),
    calc_os_stats(df %>% filter(TaggedPitchType %in% c("Slider", "SL"))) %>% mutate(Split = "SL"),
    calc_os_stats(df %>% filter(TaggedPitchType %in% c("Curveball", "CU", "KC"))) %>% mutate(Split = "CB"),
    calc_os_stats(df %>% filter(os_hard)) %>% mutate(Split = "Hard 84+"),
    calc_os_stats(df %>% filter(os_soft)) %>% mutate(Split = "Soft <76"),
    calc_os_stats(df %>% filter(os_gyro)) %>% mutate(Split = "Gyro"),
    calc_os_stats(df %>% filter(os_downer)) %>% mutate(Split = "Downer"),
    calc_os_stats(df %>% filter(os_sweep)) %>% mutate(Split = "Sweep 12+")
  ) %>%
    filter(P >= 5) %>%
    select(Split, P, BA, SLG, wOBA, `Whiff%`, `Chase%`, `Z-Whiff%`, EV, `CS%`) %>%
    mutate(across(c(BA, SLG, wOBA), ~ifelse(is.na(.) | is.nan(.), NA, round(., 3))),
           across(c(`Whiff%`, `Chase%`, `Z-Whiff%`, `CS%`), ~ifelse(is.na(.) | is.nan(.), NA, round(., 1))),
           EV = ifelse(is.na(EV) | is.nan(EV), NA, round(EV, 1)))
  
  if (nrow(splits) == 0) return(NULL)
  splits
}

# Calculate stats for a filtered dataset
calc_split_stats <- function(data) {
  if (nrow(data) == 0) {
    return(tibble(
      P = 0, BA = NA, SLG = NA, wOBA = NA, 
      Whiff_pct = NA, Chase_pct = NA, ZWhiff_pct = NA,
      RV = NA, RV100 = NA, EV = NA, SC_pct = NA
    ))
  }
  
  data %>%
    summarise(
      P = n(),
      BA = sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE),
      SLG = sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE),
      Whiff_pct = sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE) * 100,
      Chase_pct = sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE) * 100,
      ZWhiff_pct = sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE) * 100,
      Zswing_pct = sum(Zswing, na.rm = TRUE) / sum(StrikeZoneIndicator, na.rm = TRUE) * 100,
      RV = if ("hitter_rv" %in% names(data)) sum(hitter_rv, na.rm = TRUE) else NA,
      RV100 = if ("hitter_rv" %in% names(data)) 100 * mean(hitter_rv, na.rm = TRUE) else NA,
      EV = mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE),
      SC_pct = sum(SCind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

# ============================================================
# 3. HITTER SCOUTING PROFILE BUILDER
# ============================================================

build_hitter_scouting_data <- function(h_name, tm_data) {
  
  if (is.null(tm_data)) return(NULL)
  
  # Filter to this hitter
  h_data <- tm_data %>%
    filter(Batter == h_name) %>%
    add_indicators()
  
  if (nrow(h_data) < 10) return(NULL)
  
  # Calculate grades LIVE using the exact user formula
  grades_df <- calculate_hitter_grades(h_name, tm_data)
  rhp_grades <- grades_df %>% filter(PitcherThrows == "Right")
  lhp_grades <- grades_df %>% filter(PitcherThrows == "Left")
  
  # Calculate RV/100 for overall
  h_data_with_rv <- h_data %>% calculate_rv100()
  rv_per_100 <- 100 * mean(h_data_with_rv$hitter_rv, na.rm = TRUE)
  
  # Get hand
  h_side <- hitter_hands %>% filter(Batter == h_name) %>% pull(BatterSide)
  h_side <- if (length(h_side) > 0) substr(h_side[1], 1, 1) else "R"
  
  # Sample sizes
  n_total <- nrow(h_data)
  n_rhp <- sum(h_data$PitcherThrows == "Right", na.rm = TRUE)
  n_lhp <- sum(h_data$PitcherThrows == "Left", na.rm = TRUE)
  
  # ---- AGGRESSIVENESS (from actual data) ----
  
  # First pitch aggressiveness (Zswing% on 0-0)
  # League avg first pitch swing% ~30%, so Zswing% ~50-55% is average
  fp_data <- h_data %>% filter(FPindicator == 1)
  fp_zone_sum <- if (nrow(fp_data) > 0) sum(fp_data$StrikeZoneIndicator, na.rm = TRUE) else 0
  fp_zswing <- if (nrow(fp_data) > 10 && fp_zone_sum > 0) {
    sum(fp_data$Zswing, na.rm = TRUE) / fp_zone_sum * 100
  } else NA
  
  # First pitch swing rate (overall, not just zone)
  fp_swing_rate <- if (nrow(fp_data) > 10) {
    sum(fp_data$SwingIndicator, na.rm = TRUE) / nrow(fp_data) * 100
  } else NA
  
  # Two-strike chase rate
  ts_data <- h_data %>% filter(TwoStrikeInd == 1)
  ts_ooz_sum <- if (nrow(ts_data) > 0) sum(ts_data$OutofZone, na.rm = TRUE) else 0
  ts_chase <- if (nrow(ts_data) > 10 && ts_ooz_sum > 0) {
    sum(ts_data$Chaseindicator, na.rm = TRUE) / ts_ooz_sum * 100
  } else NA
  
  # Overall zone swing
  zone_sum <- sum(h_data$StrikeZoneIndicator, na.rm = TRUE)
  overall_zswing <- if (zone_sum > 0) sum(h_data$Zswing, na.rm = TRUE) / zone_sum * 100 else NA
  
  # Classify aggressiveness - more loose thresholds
  # League avg FP swing ~30%, so aggressive >35%, patient <25%
  aggro_1p <- if (is.na(fp_swing_rate) || is.nan(fp_swing_rate)) "Avg" else if (fp_swing_rate >= 35) "Aggressive" else if (fp_swing_rate <= 25) "Patient" else "Average"
  # League avg 2K chase ~28%, so high >32%, low <24%
  chase_2k <- if (is.na(ts_chase) || is.nan(ts_chase)) "Avg" else if (ts_chase >= 32) "High" else if (ts_chase <= 24) "Low" else "Average"
  # League avg zone swing ~68%, so aggressive >73%, passive <63%
  zone_aggro <- if (is.na(overall_zswing) || is.nan(overall_zswing)) "Average" else if (overall_zswing >= 73) "Aggressive" else if (overall_zswing <= 63) "Passive" else "Average"
  
  # ---- SPLITS BY PITCH FAMILY AND PITCHER HAND ----
  
  splits_list <- list()
  
  for (p_hand in c("Right", "Left")) {
    hand_data <- h_data %>% filter(PitcherThrows == p_hand)
    hand_label <- ifelse(p_hand == "Right", "RHP", "LHP")
    
    for (fam in c("FB", "BB", "OS")) {
      fam_data <- hand_data %>% filter(PitchFamily == fam)
      
      if (nrow(fam_data) >= 5) {
        stats <- calc_split_stats(fam_data)
        n_pitches <- stats$P
        
        # ---- SHRINKAGE: Regress toward league average based on sample size ----
        # k = prior strength (higher = more regression to mean)
        # Using k=150 means ~50% weight on observed at 150 pitches
        k <- 150
        shrinkage_factor <- n_pitches / (n_pitches + k)
        
        # League average benchmarks (neutral values)
        league_avg <- list(
          Whiff_pct = 25,
          BA = 0.260,
          SLG = 0.400,
          RV100 = 0,
          Chase_pct = 28
        )
        
        # Apply shrinkage to each stat before computing composite
        shrunk_whiff <- if (!is.na(stats$Whiff_pct)) {
          shrinkage_factor * stats$Whiff_pct + (1 - shrinkage_factor) * league_avg$Whiff_pct
        } else league_avg$Whiff_pct
        
        shrunk_ba <- if (!is.na(stats$BA)) {
          shrinkage_factor * stats$BA + (1 - shrinkage_factor) * league_avg$BA
        } else league_avg$BA
        
        shrunk_slg <- if (!is.na(stats$SLG)) {
          shrinkage_factor * stats$SLG + (1 - shrinkage_factor) * league_avg$SLG
        } else league_avg$SLG
        
        shrunk_rv100 <- if (!is.na(stats$RV100)) {
          shrinkage_factor * stats$RV100 + (1 - shrinkage_factor) * league_avg$RV100
        } else league_avg$RV100
        
        shrunk_chase <- if (!is.na(stats$Chase_pct)) {
          shrinkage_factor * stats$Chase_pct + (1 - shrinkage_factor) * league_avg$Chase_pct
        } else league_avg$Chase_pct
        
        # Calculate composite score using SHRUNK values
        # (higher = hitter struggles = good for pitcher)
        composite <- 0
        composite <- composite + shrunk_whiff * 1.5
        composite <- composite - shrunk_ba * 100
        composite <- composite - shrunk_slg * 50
        composite <- composite - shrunk_rv100 * 10
        composite <- composite + shrunk_chase * 0.5
        
        splits_list[[paste0(hand_label, "_", fam)]] <- list(
          stats = stats,
          composite = composite,
          shrinkage_factor = shrinkage_factor,
          n = n_pitches
        )
      } else {
        splits_list[[paste0(hand_label, "_", fam)]] <- list(
          stats = calc_split_stats(tibble()),
          composite = 0,
          shrinkage_factor = 0,
          n = 0
        )
      }
    }
  }
  
  # ---- KEY STATS (most significant splits) - SEPARATE FB AND OS ----
  
  key_stats_fb <- list(RHP = list(), LHP = list())
  key_stats_os <- list(RHP = list(), LHP = list())
  
  # ---- KEY STATS: COMPOSITE SCORES FOR SPLITS (n >= 50) ----
  # For each pitch family (FB, BB, OS), calculate composite scores for splits
  # Score = weighted combo of whiff, chase, BA, SLG (normalized 20-80 scale)
  # Higher score = better for hitter, Lower = worse for hitter
  # ALWAYS show key splits: Hard (92+), Sink, Sweep, Ride, Fade with BA
  
  pitch_group_stats <- list()
  
  # Key splits to ALWAYS show (velo-based, not movement)
  # FB: Hard (92+), Soft (<88)
  # BB/OS: Hard (84+), Soft (<76)
  key_splits_by_fam <- list(
    FB = c("Hard", "Soft"),
    BB = c("HardBB", "SoftBB"),
    OS = c("HardOS", "SoftOS")
  )
  
  for (p_hand in c("Right", "Left")) {
    hand_data <- h_data %>% filter(PitcherThrows == p_hand)
    hand_label <- ifelse(p_hand == "Right", "RHP", "LHP")
    
    for (fam in c("FB", "BB", "OS")) {
      fam_data <- hand_data %>% filter(PitchFamily == fam)
      
      key <- paste0(hand_label, "_", fam)
      pitch_group_stats[[key]] <- list(
        key_splits = list(),   # Always-show splits with BA
        strengths = NULL,      # High composite (good for hitter)
        weaknesses = NULL      # Low composite (bad for hitter)
      )
      
      if (nrow(fam_data) < 20) next
      
      # Add pitch trait split columns
      fam_data <- fam_data %>%
        mutate(
          # FB velo splits (92+ hard, <88 soft)
          trait_hard = if (fam == "FB") RelSpeed >= 92 else FALSE,
          trait_soft = if (fam == "FB") RelSpeed < 88 else FALSE,
          # BB velo splits (84+ hard, <76 soft)
          trait_hard_bb = if (fam == "BB") RelSpeed >= 84 else FALSE,
          trait_soft_bb = if (fam == "BB") RelSpeed < 76 else FALSE,
          # OS velo splits (84+ hard, <76 soft)
          trait_hard_os = if (fam == "OS") RelSpeed >= 84 else FALSE,
          trait_soft_os = if (fam == "OS") RelSpeed < 76 else FALSE,
          # FB movement traits (for composite scoring)
          trait_ride = if (fam == "FB") InducedVertBreak >= 18 else FALSE,
          trait_sink = if (fam == "FB") InducedVertBreak < 9 & abs(HorzBreak) >= 10 else FALSE,
          # BB movement traits
          trait_sweep = if (fam == "BB") abs(HorzBreak) >= 12 else FALSE,
          trait_downer = if (fam == "BB") InducedVertBreak <= -8 else FALSE,
          # OS movement traits
          trait_fade = if (fam == "OS") abs(HorzBreak) >= 12 else FALSE,
          trait_drop = if (fam == "OS") InducedVertBreak <= -5 else FALSE
        )
      
      # All splits for composite scoring
      all_splits <- list(
        # FB velo
        list(name = "Hard", col = "trait_hard"),
        list(name = "Soft", col = "trait_soft"),
        # BB velo
        list(name = "HardBB", col = "trait_hard_bb"),
        list(name = "SoftBB", col = "trait_soft_bb"),
        # OS velo
        list(name = "HardOS", col = "trait_hard_os"),
        list(name = "SoftOS", col = "trait_soft_os"),
        # Movement traits
        list(name = "Ride", col = "trait_ride"),
        list(name = "Sink", col = "trait_sink"),
        list(name = "Sweep", col = "trait_sweep"),
        list(name = "Downer", col = "trait_downer"),
        list(name = "Fade", col = "trait_fade"),
        list(name = "Drop", col = "trait_drop")
      )
      
      # Calculate composite scores for each valid split
      split_scores <- list()
      key_splits_data <- list()  # For always-show splits
      
      for (split_info in all_splits) {
        split_data <- fam_data %>% filter(.data[[split_info$col]] == TRUE)
        
        # For KEY SPLITS, show stats even with lower sample size (n >= 10)
        is_key_split <- split_info$name %in% key_splits_by_fam[[fam]]
        min_n <- if (is_key_split) 10 else 50
        
        if (nrow(split_data) >= min_n) {
          n_swings <- sum(split_data$SwingIndicator, na.rm = TRUE)
          n_chase_opp <- sum(split_data$OutofZone, na.rm = TRUE)
          n_ab <- sum(split_data$ABindicator, na.rm = TRUE)
          n_pitches <- nrow(split_data)
          
          # Calculate raw metrics
          whiff_pct <- if (n_swings >= 15) sum(split_data$WhiffIndicator, na.rm = TRUE) / n_swings * 100 else NA
          chase_pct <- if (n_chase_opp >= 15) sum(split_data$Chaseindicator, na.rm = TRUE) / n_chase_opp * 100 else NA
          ba <- if (n_ab >= 5) sum(split_data$HitIndicator, na.rm = TRUE) / n_ab else NA
          slg <- if (n_ab >= 5) sum(split_data$totalbases, na.rm = TRUE) / n_ab else NA
          
          # Calculate RV/100 for this split
          rv_split <- (sum(split_data$totalbases, na.rm = TRUE) * 0.3 + 
                         sum(split_data$KorBB == "Walk", na.rm = TRUE) * 0.3 -
                         sum(split_data$KorBB == "Strikeout", na.rm = TRUE) * 0.25 -
                         sum(split_data$PitchCall == "InPlay" & split_data$HitIndicator == 0, na.rm = TRUE) * 0.15) / n_pitches * 100
          
          # Store key split data with RV/100 for display
          if (is_key_split && n_pitches >= 10) {
            key_splits_data[[split_info$name]] <- list(
              ba = ba,
              slg = slg,
              rv100 = round(rv_split, 2),
              n = n_pitches,
              whiff = whiff_pct
            )
          }
          
          # Normalize to 20-80 scale (higher = better for hitter)
          ba_score <- if (!is.na(ba)) max(20, min(80, (ba - 0.15) / 0.20 * 60 + 20)) else NA
          slg_score <- if (!is.na(slg)) max(20, min(80, (slg - 0.20) / 0.40 * 60 + 20)) else NA
          whiff_score <- if (!is.na(whiff_pct)) max(20, min(80, (40 - whiff_pct) / 30 * 60 + 20)) else NA
          chase_score <- if (!is.na(chase_pct)) max(20, min(80, (45 - chase_pct) / 30 * 60 + 20)) else NA
          
          # Composite: weighted average (BA 25%, SLG 35%, Whiff 25%, Chase 15%)
          scores <- c(ba_score, slg_score, whiff_score, chase_score)
          weights <- c(0.25, 0.35, 0.25, 0.15)
          valid_idx <- !is.na(scores)
          
          if (sum(valid_idx) >= 2) {
            composite <- sum(scores[valid_idx] * weights[valid_idx]) / sum(weights[valid_idx])
            split_scores[[split_info$name]] <- round(composite)
          }
        }
      }
      
      # Store key splits data
      pitch_group_stats[[key]]$key_splits <- key_splits_data
      
      # Sort and get top 2 strengths (highest) and top 2 weaknesses (lowest)
      if (length(split_scores) > 0) {
        sorted_scores <- sort(unlist(split_scores), decreasing = TRUE)
        
        # Strengths (composite >= 55)
        strengths <- sorted_scores[sorted_scores >= 55]
        if (length(strengths) > 0) {
          pitch_group_stats[[key]]$strengths <- paste(names(head(strengths, 2)), collapse = ", ")
        }
        
        # Weaknesses (composite <= 45)
        weaknesses <- sort(sorted_scores)[sort(sorted_scores) <= 45]
        if (length(weaknesses) > 0) {
          pitch_group_stats[[key]]$weaknesses <- paste(names(head(weaknesses, 2)), collapse = ", ")
        }
      }
    }
  }
  
  # ---- CALCULATE HITTER RUN VALUE FOR BORDER ----
  # Run value per 100 pitches - estimate from outcomes
  # Positive = good for hitter, Negative = bad for hitter
  rv_estimate <- (sum(h_data$totalbases, na.rm = TRUE) * 0.3 + 
                    sum(h_data$KorBB == "Walk", na.rm = TRUE) * 0.3 -
                    sum(h_data$KorBB == "Strikeout", na.rm = TRUE) * 0.25 -
                    sum(h_data$PlayResult == "Out", na.rm = TRUE) * 0.1)
  rv_per_100 <- rv_estimate / max(1, n_total) * 100
  
  # ---- RAW STATS FOR DISPLAY ----
  calc_raw_stats <- function(data) {
    if (nrow(data) < 10) return(list(
      ba = NA, slg = NA, ev = NA, ev90 = NA, whiff = NA, chase = NA,
      bb_pct = NA, k_pct = NA, z_swing = NA, fp_swing = NA,
      chase_2k = NA, pull_pct = NA, oppo_pct = NA
    ))
    
    # Calculate PA-based stats
    n_ab <- sum(data$ABindicator, na.rm = TRUE)
    n_k <- sum(data$KorBB == "Strikeout", na.rm = TRUE)
    n_bb <- sum(data$KorBB == "Walk", na.rm = TRUE)
    n_pa <- max(1, n_ab + n_bb)
    
    # EV stats
    bip_ev <- data %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed)) %>% pull(ExitSpeed)
    ev_mean <- if (length(bip_ev) >= 3) mean(bip_ev, na.rm = TRUE) else NA
    ev90 <- if (length(bip_ev) >= 10) quantile(bip_ev, 0.90, na.rm = TRUE) else NA
    
    # First pitch swing rate
    fp_data <- data %>% filter(FPindicator == 1)
    n_fp <- nrow(fp_data)
    fp_swing_pct <- if (n_fp >= 10) sum(fp_data$SwingIndicator, na.rm = TRUE) / n_fp * 100 else NA
    
    # Zone swing % (how often they swing at pitches in the zone)
    n_zone <- sum(data$StrikeZoneIndicator, na.rm = TRUE)
    z_swing <- if (n_zone >= 20) sum(data$Zswing, na.rm = TRUE) / n_zone * 100 else NA
    
    # 2 Strike Chase Rate
    ts_data <- data %>% filter(TwoStrikeInd == 1)
    ts_ooz <- sum(ts_data$OutofZone, na.rm = TRUE)
    chase_2k <- if (ts_ooz >= 10) sum(ts_data$Chaseindicator, na.rm = TRUE) / ts_ooz * 100 else NA
    
    # Pull% and Oppo% - based on Bearing
    # For RHH: Pull = negative bearing (left field), Oppo = positive bearing (right field)
    # For LHH: Pull = positive bearing (right field), Oppo = negative bearing (left field)
    bip_data <- data %>% filter(PitchCall == "InPlay", !is.na(Bearing))
    n_bip <- nrow(bip_data)
    
    pull_pct <- NA
    oppo_pct <- NA
    if (n_bip >= 10) {
      # Get batter side from the data
      batter_side <- if ("BatterSide" %in% names(data)) {
        names(which.max(table(data$BatterSide)))
      } else "Right"
      
      if (batter_side == "Right") {
        # RHH: Pull = Bearing < -15, Oppo = Bearing > 15
        pull_pct <- sum(bip_data$Bearing < -15, na.rm = TRUE) / n_bip * 100
        oppo_pct <- sum(bip_data$Bearing > 15, na.rm = TRUE) / n_bip * 100
      } else {
        # LHH: Pull = Bearing > 15, Oppo = Bearing < -15
        pull_pct <- sum(bip_data$Bearing > 15, na.rm = TRUE) / n_bip * 100
        oppo_pct <- sum(bip_data$Bearing < -15, na.rm = TRUE) / n_bip * 100
      }
    }
    
    list(
      ev = ev_mean,
      ev90 = ev90,
      fp_swing = fp_swing_pct,
      whiff = sum(data$WhiffIndicator, na.rm = TRUE) / max(1, sum(data$SwingIndicator, na.rm = TRUE)) * 100,
      chase = sum(data$Chaseindicator, na.rm = TRUE) / max(1, sum(data$OutofZone, na.rm = TRUE)) * 100,
      bb_pct = n_bb / n_pa * 100,
      k_pct = n_k / n_pa * 100,
      z_swing = z_swing,
      chase_2k = chase_2k,
      pull_pct = pull_pct,
      oppo_pct = oppo_pct
    )
  }
  
  raw_stats_overall <- calc_raw_stats(h_data)
  raw_stats_rhp <- calc_raw_stats(h_data %>% filter(PitcherThrows == "Right"))
  raw_stats_lhp <- calc_raw_stats(h_data %>% filter(PitcherThrows == "Left"))
  
  # ---- RECENT PERFORMANCE (last 15 games) ----
  # Calculate hot/cold indicator based on recent games
  # Hot = BA >= .400 in last 15 games, Cold = BA <= .150
  recent_trend <- "neutral"
  recent_ba_15 <- NA
  
  if ("Date" %in% names(h_data) || "GameDate" %in% names(h_data)) {
    date_col <- if ("GameDate" %in% names(h_data)) "GameDate" else "Date"
    
    # Get unique game dates
    game_dates <- h_data %>%
      select(all_of(date_col)) %>%
      distinct() %>%
      arrange(desc(.data[[date_col]])) %>%
      head(15) %>%
      pull(.data[[date_col]])
    
    if (length(game_dates) >= 5) {
      recent_data <- h_data %>% filter(.data[[date_col]] %in% game_dates)
      
      recent_ab <- sum(recent_data$ABindicator, na.rm = TRUE)
      
      if (recent_ab >= 10) {
        recent_ba_15 <- sum(recent_data$HitIndicator, na.rm = TRUE) / recent_ab
        
        # Simple threshold: .400+ = hot, .150 or below = cold
        if (recent_ba_15 >= 0.400) {
          recent_trend <- "hot"
        } else if (recent_ba_15 <= 0.150) {
          recent_trend <- "cold"
        }
      }
    }
  }
  
  # ---- PITCH RECOMMENDATIONS BY COUNT (Composite Score Approach) ----
  
  # Calculate confidence level based on sample sizes
  calc_recommendation_confidence <- function(n_fb, n_bb, n_os) {
    total_n <- n_fb + n_bb + n_os
    min_n <- min(n_fb, n_bb, n_os)
    
    if (total_n < 30 || min_n < 5) {
      return(list(level = "Low", shrinkage = "Heavy", color = "#D73027"))
    } else if (total_n < 80 || min_n < 15) {
      return(list(level = "Medium", shrinkage = "Moderate", color = "#FDB863"))
    } else {
      return(list(level = "High", shrinkage = "Light", color = "#1A9850"))
    }
  }
  
  # Main recommendation function using composite scores with correlation inference
  get_recommendations_by_hand <- function(splits, p_hand, mode = "overall") {
    prefix <- ifelse(p_hand == "Right", "RHP", "LHP")
    
    # Get composite scores and sample sizes
    fb_score <- splits[[paste0(prefix, "_FB")]]$composite
    bb_score <- splits[[paste0(prefix, "_BB")]]$composite
    os_score <- splits[[paste0(prefix, "_OS")]]$composite
    
    fb_n <- splits[[paste0(prefix, "_FB")]]$n
    bb_n <- splits[[paste0(prefix, "_BB")]]$n
    os_n <- splits[[paste0(prefix, "_OS")]]$n
    
    # ---- CORRELATION-BASED INFERENCE ----
    # When one pitch family has small sample, borrow info from others
    # Empirical correlations (approximate from baseball research):
    # FB <-> BB: 0.35 (some batters are just good/bad overall)
    # FB <-> OS: 0.40 (timing-based, changeup looks like fastball)
    # BB <-> OS: 0.50 (both require patience/recognition)
    
    cor_fb_bb <- 0.35
    cor_fb_os <- 0.40
    cor_bb_os <- 0.50
    
    min_sample_for_inference <- 30  # Below this, borrow from correlated families
    
    # Adjust scores using correlation when sample is small
    if (fb_n < min_sample_for_inference && (bb_n >= min_sample_for_inference || os_n >= min_sample_for_inference)) {
      # Infer FB from BB and OS
      inferred_fb <- 0
      infer_weight <- 0
      if (bb_n >= min_sample_for_inference) {
        inferred_fb <- inferred_fb + bb_score * cor_fb_bb
        infer_weight <- infer_weight + cor_fb_bb
      }
      if (os_n >= min_sample_for_inference) {
        inferred_fb <- inferred_fb + os_score * cor_fb_os
        infer_weight <- infer_weight + cor_fb_os
      }
      if (infer_weight > 0) {
        inferred_fb <- inferred_fb / infer_weight
        # Blend observed (weighted by n) with inferred
        blend_weight <- fb_n / (fb_n + min_sample_for_inference)
        fb_score <- blend_weight * fb_score + (1 - blend_weight) * inferred_fb
      }
    }
    
    if (bb_n < min_sample_for_inference && (fb_n >= min_sample_for_inference || os_n >= min_sample_for_inference)) {
      inferred_bb <- 0
      infer_weight <- 0
      if (fb_n >= min_sample_for_inference) {
        inferred_bb <- inferred_bb + fb_score * cor_fb_bb
        infer_weight <- infer_weight + cor_fb_bb
      }
      if (os_n >= min_sample_for_inference) {
        inferred_bb <- inferred_bb + os_score * cor_bb_os
        infer_weight <- infer_weight + cor_bb_os
      }
      if (infer_weight > 0) {
        inferred_bb <- inferred_bb / infer_weight
        blend_weight <- bb_n / (bb_n + min_sample_for_inference)
        bb_score <- blend_weight * bb_score + (1 - blend_weight) * inferred_bb
      }
    }
    
    if (os_n < min_sample_for_inference && (fb_n >= min_sample_for_inference || bb_n >= min_sample_for_inference)) {
      inferred_os <- 0
      infer_weight <- 0
      if (fb_n >= min_sample_for_inference) {
        inferred_os <- inferred_os + fb_score * cor_fb_os
        infer_weight <- infer_weight + cor_fb_os
      }
      if (bb_n >= min_sample_for_inference) {
        inferred_os <- inferred_os + bb_score * cor_bb_os
        infer_weight <- infer_weight + cor_bb_os
      }
      if (infer_weight > 0) {
        inferred_os <- inferred_os / infer_weight
        blend_weight <- os_n / (os_n + min_sample_for_inference)
        os_score <- blend_weight * os_score + (1 - blend_weight) * inferred_os
      }
    }
    
    # Check OS BA/SLG directly - penalize if hitter crushes offspeed
    os_ba <- splits[[paste0(prefix, "_OS")]]$stats$BA
    os_slg <- splits[[paste0(prefix, "_OS")]]$stats$SLG
    
    os_penalty <- 0
    if (!is.na(os_ba) && os_ba > 0.280 && os_n >= 20) os_penalty <- os_penalty + 20
    if (!is.na(os_slg) && os_slg > 0.450 && os_n >= 20) os_penalty <- os_penalty + 15
    
    # Calculate percentages based on composite scores
    # Higher composite = hitter struggles = recommend MORE of that pitch
    total_score <- abs(fb_score) + abs(bb_score) + abs(os_score) + 1
    
    fb_pct <- 35 + (fb_score / total_score) * 20
    bb_pct <- 35 + (bb_score / total_score) * 20
    os_pct <- 30 + (os_score / total_score) * 15 - os_penalty
    
    # Adjust for count situation
    if (mode == "first_pitch") {
      # First pitch: more fastballs, establish strike, be unpredictable
      fb_pct <- fb_pct + 8
      bb_pct <- bb_pct - 3
      os_pct <- os_pct - 5
    } else if (mode == "putaway") {
      # 2 strikes: go to putaway pitches, prioritize whiff potential
      fb_pct <- fb_pct - 8
      bb_pct <- bb_pct + 6
      os_pct <- os_pct + 4
    }
    
    # Ensure reasonable ranges
    fb_pct <- max(15, min(60, fb_pct))
    bb_pct <- max(15, min(55, bb_pct))
    os_pct <- max(10, min(40, os_pct))
    
    # Normalize to 100%
    total <- fb_pct + bb_pct + os_pct
    fb_final <- round(100 * fb_pct / total)
    bb_final <- round(100 * bb_pct / total)
    os_final <- 100 - fb_final - bb_final  # Ensure exact sum of 100
    
    # Calculate confidence
    confidence <- calc_recommendation_confidence(fb_n, bb_n, os_n)
    
    # Return as named vector with confidence attribute
    result <- c(FB = fb_final, BB = bb_final, OS = os_final)
    attr(result, "confidence") <- confidence
    
    return(result)
  }
  
  # Legacy wrapper for compatibility (averages both hands)
  get_recommendations <- function(splits, mode = "overall") {
    rhp_rec <- get_recommendations_by_hand(splits, "Right", mode)
    lhp_rec <- get_recommendations_by_hand(splits, "Left", mode)
    
    # Average the recommendations
    fb_avg <- round((rhp_rec["FB"] + lhp_rec["FB"]) / 2)
    bb_avg <- round((rhp_rec["BB"] + lhp_rec["BB"]) / 2)
    os_avg <- 100 - fb_avg - bb_avg
    
    c(FB = fb_avg, BB = bb_avg, OS = os_avg)
  }
  
  # Build recommendations
  rec_rhp_overall <- get_recommendations_by_hand(splits_list, "Right", "overall")
  rec_rhp_1p <- get_recommendations_by_hand(splits_list, "Right", "first_pitch")
  rec_rhp_putaway <- get_recommendations_by_hand(splits_list, "Right", "putaway")
  rec_lhp_overall <- get_recommendations_by_hand(splits_list, "Left", "overall")
  rec_lhp_1p <- get_recommendations_by_hand(splits_list, "Left", "first_pitch")
  rec_lhp_putaway <- get_recommendations_by_hand(splits_list, "Left", "putaway")
  
  # ---- DETERMINE PUNISHES/STRUGGLES ----
  
  punishes <- c()
  struggles <- c()
  
  # Use composite scores - negative composite = hitter punishes, positive = struggles
  for (fam in c("FB", "BB", "OS")) {
    rhp_comp <- splits_list[[paste0("RHP_", fam)]]$composite
    lhp_comp <- splits_list[[paste0("LHP_", fam)]]$composite
    rhp_n <- splits_list[[paste0("RHP_", fam)]]$n
    lhp_n <- splits_list[[paste0("LHP_", fam)]]$n
    
    avg_comp <- (rhp_comp * rhp_n + lhp_comp * lhp_n) / max(1, rhp_n + lhp_n)
    total_n <- rhp_n + lhp_n
    
    if (total_n >= 30) {
      if (avg_comp <= -15) punishes <- c(punishes, fam)
      if (avg_comp >= 15) struggles <- c(struggles, fam)
    }
  }
  
  # ---- CONFIDENCE LEVEL ----
  confidence <- if (n_total < 50) 1 else if (n_total < 150) 2 else 3
  
  # Return complete profile
  list(
    name = h_name,
    hand = h_side,
    n = n_total,
    n_rhp = n_rhp,
    n_lhp = n_lhp,
    confidence = confidence,
    
    # RV/100 from live calculation
    rv_per_100 = rv_per_100,
    
    # Grades calculated LIVE (not from pre-computed)
    overall_grade = if (nrow(rhp_grades) > 0 && nrow(lhp_grades) > 0) {
      round((rhp_grades$game_power[1] + lhp_grades$game_power[1] +
               rhp_grades$contact[1] + lhp_grades$contact[1] +
               rhp_grades$avoid_k[1] + lhp_grades$avoid_k[1] +
               rhp_grades$swing_decisions[1] + lhp_grades$swing_decisions[1]) / 8)
    } else 50,
    
    # Grades by hand - LIVE
    rhp_power = if (nrow(rhp_grades) > 0) rhp_grades$game_power[1] else 50,
    rhp_raw_power = if (nrow(rhp_grades) > 0) rhp_grades$raw_power[1] else 50,
    rhp_contact = if (nrow(rhp_grades) > 0) rhp_grades$contact[1] else 50,
    rhp_avoid_k = if (nrow(rhp_grades) > 0) rhp_grades$avoid_k[1] else 50,
    rhp_swing_dec = if (nrow(rhp_grades) > 0) rhp_grades$swing_decisions[1] else 50,
    lhp_power = if (nrow(lhp_grades) > 0) lhp_grades$game_power[1] else 50,
    lhp_raw_power = if (nrow(lhp_grades) > 0) lhp_grades$raw_power[1] else 50,
    lhp_contact = if (nrow(lhp_grades) > 0) lhp_grades$contact[1] else 50,
    lhp_avoid_k = if (nrow(lhp_grades) > 0) lhp_grades$avoid_k[1] else 50,
    lhp_swing_dec = if (nrow(lhp_grades) > 0) lhp_grades$swing_decisions[1] else 50,
    
    # Aggressiveness
    aggro_1p = aggro_1p,
    chase_2k = chase_2k,
    zone_aggro = zone_aggro,
    fp_zswing = fp_zswing,
    ts_chase = ts_chase,
    
    # Recent performance trend
    recent_trend = recent_trend,
    
    # Punishes/Struggles
    punishes = punishes,
    struggles = struggles,
    
    # Splits data
    splits = splits_list,
    pitch_group_stats = pitch_group_stats,
    
    # Run value for border color
    rv_per_100 = rv_per_100,
    
    # Raw stats
    raw_stats_overall = raw_stats_overall,
    raw_stats_rhp = raw_stats_rhp,
    raw_stats_lhp = raw_stats_lhp,
    
    # Recommendations
    rec_rhp_overall = rec_rhp_overall,
    rec_rhp_1p = rec_rhp_1p,
    rec_rhp_putaway = rec_rhp_putaway,
    rec_lhp_overall = rec_lhp_overall,
    rec_lhp_1p = rec_lhp_1p,
    rec_lhp_putaway = rec_lhp_putaway
  )
}

# ============================================================
# 4. VISUALIZATION FUNCTIONS
# ============================================================

# Grade color function
grade_color <- function(grade) {
  if (is.na(grade)) return("#999999")
  if (grade >= 70) return("#1A9850")
  if (grade >= 60) return("#91CF60")
  if (grade >= 55) return("#D9EF8B")
  if (grade >= 45) return("#FFFFBF")
  if (grade >= 40) return("#FEE08B")
  if (grade >= 30) return("#FC8D59")
  return("#D73027")
}

# Create heatmap for SLG or Whiff by location - improved small sample handling
create_metric_heatmap <- function(tm_data, batter_name, pitch_family, pitcher_hand, 
                                  metric = "Whiff", title = "") {
  
  # Base plot elements for empty/low data cases
  base_plot <- function(label = "No Data") {
    ggplot() +
      annotate("text", x = 0, y = 2.55, label = label, size = 3, color = "gray50") +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
               fill = NA, color = "black", linewidth = 1) +
      annotate("polygon", 
               x = c(-0.708, 0.708, 0.708, 0, -0.708),
               y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
               fill = NA, color = "black", linewidth = 0.8) +
      coord_fixed(ratio = 1) + 
      xlim(-1.6, 1.6) + ylim(0.2, 4.0) +
      theme_void() +
      theme(plot.margin = margin(2, 2, 2, 2))
  }
  
  if (is.null(tm_data)) return(base_plot("No Data"))
  
  # Filter data
  plot_data <- tm_data %>%
    filter(Batter == batter_name, PitcherThrows == pitcher_hand) %>%
    add_indicators() %>%
    filter(PitchFamily == pitch_family) %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight),
           PlateLocSide >= -2, PlateLocSide <= 2,
           PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  n_pitches <- nrow(plot_data)
  if (n_pitches < 3) return(base_plot(paste0("n=", n_pitches)))
  
  # For very small samples, use point plot instead of density
  use_points <- n_pitches < 15
  
  # Adjust bandwidth based on sample size (smaller sample = more smoothing)
  bw_adjust <- case_when(
    n_pitches < 20 ~ 2.0,
    n_pitches < 40 ~ 1.5,
    n_pitches < 80 ~ 1.2,
    n_pitches < 150 ~ 1.0,
    TRUE ~ 0.8
  )
  
  if (metric == "Whiff") {
    # WHIFF HEATMAP - where they whiff (good for pitcher)
    # Blue = whiffs (where to throw), shows contact vs whiff locations
    swing_data <- plot_data %>% filter(SwingIndicator == 1)
    whiff_data <- swing_data %>% filter(WhiffIndicator == 1)
    contact_data <- swing_data %>% filter(WhiffIndicator == 0)
    
    if (nrow(swing_data) < 3) return(base_plot(paste0("n=", nrow(swing_data), " sw")))
    
    if (use_points || nrow(swing_data) < 15) {
      # Point plot showing whiffs (blue) vs contact (light)
      p <- ggplot() +
        geom_point(data = contact_data, aes(x = PlateLocSide, y = PlateLocHeight), 
                   color = "#FDAE61", size = 1.5, alpha = 0.5, shape = 16) +
        geom_point(data = whiff_data, aes(x = PlateLocSide, y = PlateLocHeight), 
                   color = "#4575B4", size = 2.5, alpha = 0.8, shape = 16)
    } else if (nrow(whiff_data) >= 3) {
      # Density of whiffs - darker blue = more whiffs (good for pitcher)
      p <- ggplot(whiff_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(
          aes(fill = after_stat(density)),
          geom = "raster", 
          contour = FALSE,
          h = c(0.5 * bw_adjust, 0.6 * bw_adjust),
          n = 80
        ) +
        scale_fill_gradientn(
          colours = c("white", "#DEEBF7", "#9ECAE1", "#4292C6", "#2171B5", "#084594"),
          na.value = "white",
          guide = "none"
        )
    } else {
      # Not enough whiffs - show swing density instead
      p <- ggplot(swing_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(
          aes(fill = after_stat(density)),
          geom = "raster", 
          contour = FALSE,
          h = c(0.5 * bw_adjust, 0.6 * bw_adjust),
          n = 80
        ) +
        scale_fill_gradientn(
          colours = c("white", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26"),
          na.value = "white",
          guide = "none"
        )
    }
    
  } else if (metric == "xValue") {
    # xVALUE HEATMAP - Combined pitch value (PITCHER perspective)
    # Blue = good for PITCHER (called strikes, whiffs)
    # Red = bad for PITCHER (hitter damage)
    
    # Add run values if not present
    plot_data <- plot_data %>%
      calculate_rv100()
    
    if (use_points || n_pitches < 15) {
      # Point plot colored by run value (pitcher perspective)
      p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
        geom_point(aes(color = rv_pitcher), size = 2, alpha = 0.7) +
        scale_color_gradientn(
          colours = c("#D73027", "#FC8D59", "#FEE090", "#FFFFBF", "#E0F3F8", "#91BFDB", "#4575B4"),
          limits = c(-1.5, 0.5),
          oob = scales::squish,
          guide = "none"
        )
    } else {
      # Density weighted by pitcher run value (positive = good for pitcher)
      p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(
          aes(fill = after_stat(density), weight = rv_pitcher + 0.5),
          geom = "raster", 
          contour = FALSE,
          h = c(0.5 * bw_adjust, 0.6 * bw_adjust),
          n = 80
        ) +
        scale_fill_gradientn(
          colours = c("white", "#E0F3F8", "#91BFDB", "#4575B4", "#2166AC", "#053061"),
          na.value = "white",
          guide = "none"
        )
    }
    
  } else if (metric == "xDamage") {
    # xDAMAGE HEATMAP - Expected damage IF contact
    # Shows where NOT to throw - weighted by EV and total bases
    # Red = high damage zone, Blue = low damage
    
    bip_data <- plot_data %>% 
      filter(PitchCall == "InPlay", !is.na(ExitSpeed)) %>%
      mutate(
        # Damage score: combine EV and actual outcome
        damage = case_when(
          PlayResult == "HomeRun" ~ 4,
          ExitSpeed >= 100 & Angle >= 10 & Angle <= 30 ~ 3.5,  # Barrel
          ExitSpeed >= 95 ~ 2.5,  # Hard hit
          PlayResult == "Triple" ~ 3,
          PlayResult == "Double" ~ 2,
          PlayResult == "Single" ~ 1,
          TRUE ~ 0.3
        )
      )
    
    if (nrow(bip_data) < 3) return(base_plot(paste0("n=", nrow(bip_data), " BIP")))
    
    if (use_points || nrow(bip_data) < 10) {
      # Point plot colored/sized by damage
      p <- ggplot(bip_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
        geom_point(aes(color = damage, size = damage), alpha = 0.7) +
        scale_color_gradientn(
          colours = c("#FEE0D2", "#FC9272", "#DE2D26", "#A50F15", "#67001F"),
          guide = "none"
        ) +
        scale_size_continuous(range = c(1.5, 4), guide = "none")
    } else {
      # Density weighted by damage
      p <- ggplot(bip_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
        stat_density_2d(
          aes(fill = after_stat(density), weight = damage),
          geom = "raster", 
          contour = FALSE,
          h = c(0.5 * bw_adjust, 0.6 * bw_adjust),
          n = 80
        ) +
        scale_fill_gradientn(
          colours = c("white", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26", "#A50F15"),
          na.value = "white",
          guide = "none"
        )
    }
    
  } else {
    # Fallback - should not happen
    return(base_plot("Unknown metric"))
  }
  
  # Add strike zone and plate to all plots
  p <- p +
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
             fill = NA, color = "black", linewidth = 1) +
    annotate("polygon", 
             x = c(-0.708, 0.708, 0.708, 0, -0.708),
             y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
             fill = NA, color = "black", linewidth = 0.8) +
    coord_fixed(ratio = 1) + 
    xlim(-1.6, 1.6) + ylim(0.2, 4.0) +
    theme_void() +
    theme(plot.margin = margin(2, 2, 2, 2))
  
  return(p)
}

# Create pie chart for usage recommendations
create_usage_pie <- function(usage, border_color = "gray50") {
  # Handle NULL or invalid input
  if (is.null(usage) || length(usage) < 3) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 0, label = "No Data", size = 2, color = "gray50"))
  }
  
  # Extract values safely
  fb_val <- if ("FB" %in% names(usage)) as.numeric(usage["FB"]) else 33
  bb_val <- if ("BB" %in% names(usage)) as.numeric(usage["BB"]) else 34
  os_val <- if ("OS" %in% names(usage)) as.numeric(usage["OS"]) else 33
  
  # Check for NA values
  if (any(is.na(c(fb_val, bb_val, os_val)))) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 0, label = "No Data", size = 2, color = "gray50"))
  }
  
  df <- tibble(
    Family = factor(c("FB", "BB", "OS"), levels = c("FB", "BB", "OS")),
    Pct = c(fb_val, bb_val, os_val)
  )
  
  # Get confidence if available
  confidence <- attr(usage, "confidence")
  conf_color <- if (!is.null(confidence)) confidence$color else border_color
  
  ggplot(df, aes(x = "", y = Pct, fill = Family)) +
    geom_bar(stat = "identity", width = 1, color = conf_color, linewidth = 1.5) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("FB" = "#FA8072", "BB" = "#A020F0", "OS" = "#2E8B57"),
                      guide = "none") +
    geom_text(aes(label = ifelse(Pct >= 15, paste0(round(Pct), "%"), "")),
              position = position_stack(vjust = 0.5), 
              color = "white", fontface = "bold", size = 2.5) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1))
}

# Create mini spray chart for scouting cards
create_mini_spray <- function(tm_data, batter_name, filter_type = "all", filter_value = NULL) {
  if (is.null(tm_data)) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 0, label = "No Data", size = 2, color = "gray50"))
  }
  
  # Filter data
  chart_data <- tm_data %>%
    filter(Batter == batter_name, PitchCall == "InPlay",
           !is.na(Distance), !is.na(Bearing))
  
  # Apply filter based on type
  if (filter_type == "hand" && !is.null(filter_value)) {
    chart_data <- chart_data %>% filter(PitcherThrows == filter_value)
  } else if (filter_type == "strikes" && !is.null(filter_value)) {
    if (filter_value == "0-1") {
      chart_data <- chart_data %>% filter(Strikes %in% c(0, 1))
    } else if (filter_value == "2") {
      chart_data <- chart_data %>% filter(Strikes == 2)
    }
  }
  
  if (nrow(chart_data) < 3) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 150, label = paste0("n=", nrow(chart_data)), 
                      size = 2, color = "gray50"))
  }
  
  # Calculate spray coordinates
  chart_data <- chart_data %>%
    mutate(
      event_type = case_when(
        PlayResult == "Single" ~ "1B",
        PlayResult == "Double" ~ "2B",
        PlayResult == "Triple" ~ "3B",
        PlayResult == "HomeRun" ~ "HR",
        PlayResult %in% c("Error", "FieldersChoice") ~ "ROE",
        TRUE ~ "OUT"
      ),
      Bearing2 = Bearing * pi / 180,
      x = Distance * sin(Bearing2),
      y = Distance * cos(Bearing2)
    )
  
  event_colors <- c("1B" = "#228B22", "2B" = "#0000CD", "3B" = "#CD853F",
                    "HR" = "#008B8B", "ROE" = "#FFD700", "OUT" = "#808080")
  
  n_bip <- nrow(chart_data)
  n_hits <- sum(chart_data$event_type %in% c("1B", "2B", "3B", "HR"))
  ba <- round(n_hits / n_bip, 3)
  
  ggplot(chart_data, aes(x, y)) +
    # Foul lines
    geom_segment(aes(x = 0, y = 0, xend = 247.487, yend = 247.487), color = "gray60", linewidth = 0.3) +
    geom_segment(aes(x = 0, y = 0, xend = -247.487, yend = 247.487), color = "gray60", linewidth = 0.3) +
    # Infield
    geom_segment(aes(x = 63.6, y = 63.6, xend = 0, yend = 127.3), color = "gray60", linewidth = 0.3) +
    geom_segment(aes(x = -63.6, y = 63.6, xend = 0, yend = 127.3), color = "gray60", linewidth = 0.3) +
    # Outfield fence arc
    annotate("curve", x = -247.487, y = 247.487, xend = 247.487, yend = 247.487,
             curvature = -0.65, linewidth = 0.3, color = "gray60") +
    # Data points
    geom_point(aes(fill = event_type), shape = 21, size = 1.5, color = "black", stroke = 0.2) +
    # Stats label
    annotate("text", x = 0, y = -15, label = paste0(n_bip, " | .", sprintf("%03d", ba * 1000)),
             size = 2, fontface = "bold") +
    scale_fill_manual(values = event_colors, guide = "none") +
    coord_fixed(xlim = c(-280, 280), ylim = c(-30, 350)) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1))
}

# Create hit/out location chart (like LSU scouting sheet)
create_hit_out_chart <- function(tm_data, batter_name, chart_type = "overall_hits") {
  
  base_plot <- function(title = "", n = 0) {
    ggplot() +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
               fill = NA, color = "black", linewidth = 0.8) +
      annotate("polygon", 
               x = c(-0.708, 0.708, 0.708, 0, -0.708),
               y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
               fill = NA, color = "black", linewidth = 0.6) +
      annotate("text", x = 0, y = 4.2, label = title, size = 2.5, fontface = "bold") +
      annotate("text", x = 1.3, y = 3.8, label = paste0("n=", n), size = 2, color = "gray50") +
      coord_fixed(ratio = 1) + 
      xlim(-1.8, 1.8) + ylim(0, 4.5) +
      theme_void() +
      theme(plot.margin = margin(2, 2, 2, 2))
  }
  
  if (is.null(tm_data)) return(base_plot("No Data"))
  
  h_data <- tm_data %>%
    filter(Batter == batter_name) %>%
    add_indicators() %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight),
           PlateLocSide >= -2.5, PlateLocSide <= 2.5,
           PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  # Filter based on chart type
  if (chart_type == "overall_hits") {
    plot_data <- h_data %>% filter(HitIndicator == 1)
    title <- "Overall Hits"
  } else if (chart_type == "overall_outs") {
    plot_data <- h_data %>% filter(PitchCall == "InPlay", HitIndicator == 0)
    title <- "Overall Outs"
  } else if (chart_type == "2k_hits") {
    plot_data <- h_data %>% filter(TwoStrikeInd == 1, HitIndicator == 1)
    title <- "2 Strike Hits"
  } else if (chart_type == "2k_outs") {
    plot_data <- h_data %>% filter(TwoStrikeInd == 1, PitchCall == "InPlay", HitIndicator == 0)
    title <- "2 Strike Outs"
  } else {
    return(base_plot("Invalid type"))
  }
  
  n_results <- nrow(plot_data)
  if (n_results < 1) return(base_plot(title, 0))
  
  # Color by pitch type (like your LSU sheet)
  plot_data <- plot_data %>%
    mutate(
      PitchFamily = classify_pitch_family(TaggedPitchType)
    )
  
  ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    # Strike zone
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
             fill = NA, color = "black", linewidth = 0.8) +
    # Home plate
    annotate("polygon", 
             x = c(-0.708, 0.708, 0.708, 0, -0.708),
             y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
             fill = NA, color = "black", linewidth = 0.6) +
    # Points colored by pitch type
    geom_point(aes(color = PitchFamily), size = 2, alpha = 0.7) +
    scale_color_manual(values = c("FB" = "#FA8072", "BB" = "#A020F0", "OS" = "#2E8B57", "Other" = "#888888"),
                       guide = "none") +
    # Title
    annotate("text", x = 0, y = 4.2, label = title, size = 2.5, fontface = "bold") +
    # Sample size
    annotate("text", x = 1.3, y = 3.8, label = paste0("n=", n_results), size = 2, color = "gray50") +
    coord_fixed(ratio = 1) + 
    xlim(-1.8, 1.8) + ylim(0, 4.5) +
    theme_void() +
    theme(plot.margin = margin(2, 2, 2, 2))
}

# Create hard hit heatmap (95+ EV) for scouting cards
create_hard_hit_heatmap <- function(tm_data, batter_name, pitcher_hand, title = "") {
  base_plot <- function(label = "No Data") {
    ggplot() +
      annotate("text", x = 0, y = 2.55, label = label, size = 3, color = "gray50") +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
               fill = NA, color = "black", linewidth = 1) +
      annotate("polygon", 
               x = c(-0.708, 0.708, 0.708, 0, -0.708),
               y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
               fill = NA, color = "black", linewidth = 0.8) +
      coord_fixed(ratio = 1) + 
      xlim(-1.6, 1.6) + ylim(0.2, 4.0) +
      theme_void() +
      theme(plot.margin = margin(2, 2, 2, 2))
  }
  
  if (is.null(tm_data)) return(base_plot("No Data"))
  
  # Filter for hard hit balls (95+ EV)
  plot_data <- tm_data %>%
    filter(Batter == batter_name, PitcherThrows == pitcher_hand) %>%
    add_indicators() %>%
    filter(PitchCall == "InPlay", ExitSpeed >= 95,
           !is.na(PlateLocSide), !is.na(PlateLocHeight),
           PlateLocSide >= -2, PlateLocSide <= 2,
           PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  n_hard_hit <- nrow(plot_data)
  if (n_hard_hit < 3) return(base_plot(paste0("n=", n_hard_hit)))
  
  # Point plot with EV coloring
  p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_point(aes(color = ExitSpeed), size = 3, alpha = 0.8) +
    scale_color_gradientn(
      colours = c("#FEE0D2", "#FC9272", "#DE2D26", "#67001F"),
      limits = c(95, 115),
      guide = "none"
    )
  
  # Add count annotation
  p <- p +
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
             fill = NA, color = "black", linewidth = 1) +
    annotate("polygon", 
             x = c(-0.708, 0.708, 0.708, 0, -0.708),
             y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
             fill = NA, color = "black", linewidth = 0.8) +
    annotate("text", x = 0, y = 3.8, label = paste0("95+ EV: n=", n_hard_hit), 
             size = 2.5, fontface = "bold") +
    coord_fixed(ratio = 1) +
    xlim(-1.6, 1.6) + ylim(0.2, 4.0) +
    theme_void() +
    theme(plot.margin = margin(2, 2, 2, 2))
  
  return(p)
}

# Create movement plot for a specific pitch family
# Shows all pitches with whiffs in red, 95+ EV in green, others in gray
create_movement_plot <- function(tm_data, batter_name, pitcher_hand, pitch_family, title = "", show_legend = FALSE) {
  base_plot <- function(label = "No Data") {
    ggplot() +
      geom_vline(xintercept = 0, color = "gray70", linewidth = 0.5) +
      geom_hline(yintercept = 0, color = "gray70", linewidth = 0.5) +
      annotate("text", x = 0, y = 0, label = label, size = 2.5, color = "gray50") +
      coord_equal(xlim = c(-30, 30), ylim = c(-30, 30)) +
      theme_void() +
      theme(plot.margin = margin(1, 1, 1, 1))
  }
  
  if (is.null(tm_data)) return(base_plot("No Data"))
  
  # Filter data for this hitter, pitcher hand, and pitch family
  plot_data <- tm_data %>%
    filter(Batter == batter_name, PitcherThrows == pitcher_hand,
           !is.na(HorzBreak), !is.na(InducedVertBreak)) %>%
    add_indicators() %>%
    filter(PitchFamily == pitch_family)
  
  if (nrow(plot_data) < 5) return(base_plot(paste0("n=", nrow(plot_data))))
  
  # Mark outcomes: whiff (red), hard hit 95+ EV (green), other (gray)
  plot_data <- plot_data %>%
    mutate(
      is_whiff = SwingIndicator == 1 & WhiffIndicator == 1,
      is_hard_hit = PitchCall == "InPlay" & !is.na(ExitSpeed) & ExitSpeed >= 95,
      outcome = case_when(
        is_whiff ~ "Whiff",
        is_hard_hit ~ "HardHit",
        TRUE ~ "Other"
      )
    )
  
  n_total <- nrow(plot_data)
  n_swings <- sum(plot_data$SwingIndicator == 1)
  n_whiffs <- sum(plot_data$is_whiff)
  n_hard <- sum(plot_data$is_hard_hit)
  whiff_pct <- if (n_swings > 0) round(n_whiffs / n_swings * 100, 1) else 0
  
  # Create plot - layer order: gray first, then green, then red on top
  p <- ggplot() +
    geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
    # Other pitches (gray) plotted first
    geom_point(data = filter(plot_data, outcome == "Other"),
               aes(x = HorzBreak, y = InducedVertBreak),
               color = "gray70", size = 1.8, alpha = 0.4) +
    # Hard hits (green) plotted second
    geom_point(data = filter(plot_data, outcome == "HardHit"),
               aes(x = HorzBreak, y = InducedVertBreak),
               color = "#1A9850", size = 2.0, alpha = 0.85) +
    # Whiffs (red) plotted on top
    geom_point(data = filter(plot_data, outcome == "Whiff"),
               aes(x = HorzBreak, y = InducedVertBreak),
               color = "#D73027", size = 2.0, alpha = 0.85) +
    # Corner labels
    annotate("text", x = -26, y = 27, label = "← 1B", size = 1.6, color = "gray50") +
    annotate("text", x = 26, y = 27, label = "3B →", size = 1.6, color = "gray50") +
    # N and stats label at bottom
    annotate("text", x = 0, y = -27, 
             label = paste0("n=", n_total, " | Wh:", whiff_pct, "% | HH:", n_hard), 
             size = 1.6, color = "gray40") +
    # Axis labels
    scale_x_continuous(breaks = c(-20, 0, 20), limits = c(-30, 30)) +
    scale_y_continuous(breaks = c(-20, 0, 20), limits = c(-30, 30)) +
    coord_equal() +
    theme_void() +
    theme(
      plot.margin = margin(1, 1, 1, 1),
      axis.text = element_text(size = 5, color = "gray60"),
      legend.position = "none"
    )
  
  return(p)
}

# Create hard hit heatmap by pitch family (95+ EV)
create_hard_hit_by_pitch <- function(tm_data, batter_name, pitcher_hand, pitch_family) {
  base_plot <- function(label = "n<3") {
    ggplot() +
      annotate("text", x = 0, y = 2.55, label = label, size = 2, color = "gray50") +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
               fill = NA, color = "black", linewidth = 0.5) +
      annotate("polygon", 
               x = c(-0.708, 0.708, 0.708, 0, -0.708),
               y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
               fill = NA, color = "black", linewidth = 0.4) +
      coord_fixed(ratio = 1) + 
      xlim(-1.6, 1.6) + ylim(0.2, 4.0) +
      theme_void() +
      theme(plot.margin = margin(1, 1, 1, 1))
  }
  
  if (is.null(tm_data)) return(base_plot())
  
  # Add indicators FIRST (creates PitchFamily), THEN filter by pitch family
  plot_data <- tm_data %>%
    filter(Batter == batter_name, PitcherThrows == pitcher_hand) %>%
    add_indicators() %>%
    filter(PitchFamily == pitch_family, PitchCall == "InPlay", ExitSpeed >= 95,
           !is.na(PlateLocSide), !is.na(PlateLocHeight),
           PlateLocSide >= -2, PlateLocSide <= 2,
           PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  n_hard <- nrow(plot_data)
  if (n_hard < 3) return(base_plot(paste0("n=", n_hard)))
  
  p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_point(aes(color = ExitSpeed), size = 2, alpha = 0.8) +
    scale_color_gradientn(
      colours = c("#FEE0D2", "#FC9272", "#DE2D26", "#67001F"),
      limits = c(95, 115),
      guide = "none"
    ) +
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, 
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("polygon", 
             x = c(-0.708, 0.708, 0.708, 0, -0.708),
             y = c(0.4, 0.4, 0.55, 0.75, 0.55), 
             fill = NA, color = "black", linewidth = 0.4) +
    annotate("text", x = 0, y = 3.8, label = paste0("n=", n_hard), size = 1.8) +
    coord_fixed(ratio = 1) +
    xlim(-1.6, 1.6) + ylim(0.2, 4.0) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1))
  
  return(p)
}

# ============================================================
# 5. MAC-STYLE MATCHUP FUNCTIONS (Similarity-Based)
# ============================================================

# Pre-compute standardized pitch features for similarity matching
cat("Pre-computing pitch similarity features...\n")

# Features for similarity matching (what batter perceives)
similarity_features <- c("RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate", "RelHeight", "RelSide")

# Calculate z-scores for standardization
feature_means <- tm_data %>%
  summarise(across(all_of(similarity_features), ~mean(., na.rm = TRUE))) %>%
  as.list()

feature_sds <- tm_data %>%
  summarise(across(all_of(similarity_features), ~sd(., na.rm = TRUE))) %>%
  as.list()

# Pre-compute pitcher arsenal profiles (average characteristics by pitch family)
pitcher_arsenal <- tm_data %>%
  filter(!is.na(Pitcher)) %>%
  add_indicators() %>%
  group_by(Pitcher, PitcherThrows, PitchFamily) %>%
  summarise(
    n_pitches = n(),
    usage_pct = n(),  # Will normalize later
    RelSpeed = mean(RelSpeed, na.rm = TRUE),
    InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
    HorzBreak = mean(HorzBreak, na.rm = TRUE),
    SpinRate = mean(SpinRate, na.rm = TRUE),
    RelHeight = mean(RelHeight, na.rm = TRUE),
    RelSide = mean(RelSide, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(PitchFamily %in% c("FB", "BB", "OS"), n_pitches >= 10) %>%
  group_by(Pitcher) %>%
  mutate(usage_pct = n_pitches / sum(n_pitches)) %>%
  ungroup()

# Pre-compute batter performance by pitch characteristics (binned)
# This creates a lookup table of how batters perform against different pitch profiles
cat("Building batter pitch profile lookup...\n")

batter_pitch_performance <- tm_data %>%
  filter(!is.na(Batter), !is.na(RelSpeed), !is.na(InducedVertBreak), !is.na(HorzBreak)) %>%
  add_indicators() %>%
  calculate_rv100() %>%
  mutate(
    # Standardize features
    RelSpeed_z = (RelSpeed - feature_means$RelSpeed) / feature_sds$RelSpeed,
    IVB_z = (InducedVertBreak - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak,
    HB_z = (HorzBreak - feature_means$HorzBreak) / feature_sds$HorzBreak,
    SpinRate_z = (SpinRate - feature_means$SpinRate) / feature_sds$SpinRate,
    RelHeight_z = (RelHeight - feature_means$RelHeight) / feature_sds$RelHeight,
    RelSide_z = (RelSide - feature_means$RelSide) / feature_sds$RelSide
  )

# Function to calculate Euclidean distance between pitch profiles
calc_pitch_distance <- function(pitch_profile, target_profile) {
  # pitch_profile and target_profile are named vectors with z-scored features
  features <- c("RelSpeed_z", "IVB_z", "HB_z", "SpinRate_z", "RelHeight_z", "RelSide_z")
  
  diff_sq <- 0
  n_features <- 0
  for (f in features) {
    if (!is.na(pitch_profile[[f]]) && !is.na(target_profile[[f]])) {
      diff_sq <- diff_sq + (pitch_profile[[f]] - target_profile[[f]])^2
      n_features <- n_features + 1
    }
  }
  if (n_features == 0) return(NA)
  sqrt(diff_sq)
}

# MAC-style matchup calculation
# Distance threshold: With 6 standardized features, random distance averages ~2.45
# Threshold of 2.5 captures roughly similar 40-45% of pitches
calculate_mac_matchup <- function(p_name, h_name, distance_threshold = 2.5) {
  
  # Get pitcher's arsenal
  p_arsenal <- pitcher_arsenal %>% filter(Pitcher == p_name)
  
  if (nrow(p_arsenal) == 0) {
    return(list(score = 50, rv100 = 0, n_similar = 0, by_pitch = NULL, 
                similar_pitches_data = NULL, hitter_stats = NULL, pitcher_stats = NULL,
                woba = NA, wobacon = NA, stuff_plus = NA))
  }
  
  p_hand <- p_arsenal$PitcherThrows[1]
  
  # Get batter's pitch history
  batter_pitches <- batter_pitch_performance %>%
    filter(Batter == h_name)
  
  if (nrow(batter_pitches) < 20) {
    return(list(score = 50, rv100 = 0, n_similar = 0, by_pitch = NULL,
                similar_pitches_data = NULL, hitter_stats = NULL, pitcher_stats = NULL,
                woba = NA, wobacon = NA, stuff_plus = NA))
  }
  
  # ---- GET HITTER OVERALL STATS ----
  hitter_overall <- tm_data %>%
    filter(Batter == h_name) %>%
    add_indicators() %>%
    summarise(
      PA = sum(PAindicator, na.rm = TRUE),
      AB = sum(ABindicator, na.rm = TRUE),
      H = sum(HitIndicator, na.rm = TRUE),
      K = sum(KorBB == "Strikeout", na.rm = TRUE),
      BB = sum(KorBB == "Walk", na.rm = TRUE),
      TB = sum(totalbases, na.rm = TRUE),
      BA = H / pmax(1, AB),
      SLG = TB / pmax(1, AB),
      K_pct = 100 * K / pmax(1, PA),
      BB_pct = 100 * BB / pmax(1, PA),
      wOBA = if("woba" %in% names(tm_data)) mean(woba, na.rm = TRUE) else NA,
      .groups = "drop"
    )
  
  # ---- GET PITCHER OVERALL STATS ----
  pitcher_overall <- tm_data %>%
    filter(Pitcher == p_name) %>%
    add_indicators() %>%
    summarise(
      BF = sum(PAindicator, na.rm = TRUE),
      K = sum(KorBB == "Strikeout", na.rm = TRUE),
      BB = sum(KorBB == "Walk", na.rm = TRUE),
      H = sum(HitIndicator, na.rm = TRUE),
      K_pct = 100 * K / pmax(1, BF),
      BB_pct = 100 * BB / pmax(1, BF),
      BA_against = H / pmax(1, sum(ABindicator, na.rm = TRUE)),
      Stuff_plus = if("stuff_plus" %in% names(tm_data)) mean(stuff_plus, na.rm = TRUE) else NA,
      .groups = "drop"
    )
  
  # For each pitch type in pitcher's arsenal, find similar pitches batter has faced
  results_by_pitch <- list()
  all_similar_pitches <- tibble()
  
  for (i in 1:nrow(p_arsenal)) {
    pitch_type <- p_arsenal$PitchFamily[i]
    usage <- p_arsenal$usage_pct[i]
    
    # Target pitch profile (standardized)
    target <- list(
      RelSpeed_z = (p_arsenal$RelSpeed[i] - feature_means$RelSpeed) / feature_sds$RelSpeed,
      IVB_z = (p_arsenal$InducedVertBreak[i] - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak,
      HB_z = (p_arsenal$HorzBreak[i] - feature_means$HorzBreak) / feature_sds$HorzBreak,
      SpinRate_z = (p_arsenal$SpinRate[i] - feature_means$SpinRate) / feature_sds$SpinRate,
      RelHeight_z = (p_arsenal$RelHeight[i] - feature_means$RelHeight) / feature_sds$RelHeight,
      RelSide_z = (p_arsenal$RelSide[i] - feature_means$RelSide) / feature_sds$RelSide
    )
    
    # Handle NA in target
    target <- lapply(target, function(x) if(is.na(x) || is.nan(x)) 0 else x)
    
    # Calculate distance for each pitch batter has seen (vectorized)
    batter_pitches_calc <- batter_pitches %>%
      mutate(
        RelSpeed_z_safe = ifelse(is.na(RelSpeed_z), 0, RelSpeed_z),
        IVB_z_safe = ifelse(is.na(IVB_z), 0, IVB_z),
        HB_z_safe = ifelse(is.na(HB_z), 0, HB_z),
        SpinRate_z_safe = ifelse(is.na(SpinRate_z), 0, SpinRate_z),
        RelHeight_z_safe = ifelse(is.na(RelHeight_z), 0, RelHeight_z),
        RelSide_z_safe = ifelse(is.na(RelSide_z), 0, RelSide_z),
        distance = sqrt(
          (RelSpeed_z_safe - target$RelSpeed_z)^2 +
            (IVB_z_safe - target$IVB_z)^2 +
            (HB_z_safe - target$HB_z)^2 +
            (SpinRate_z_safe - target$SpinRate_z)^2 +
            (RelHeight_z_safe - target$RelHeight_z)^2 +
            (RelSide_z_safe - target$RelSide_z)^2
        )
      )
    
    # Filter to similar pitches
    similar_pitches <- batter_pitches_calc %>%
      filter(!is.na(distance), distance <= distance_threshold) %>%
      mutate(matched_pitch_type = pitch_type, pitcher_usage = usage)
    
    # Collect all similar pitches for visualization
    if (nrow(similar_pitches) > 0) {
      all_similar_pitches <- bind_rows(all_similar_pitches, similar_pitches)
    }
    
    n_similar_pitch <- nrow(similar_pitches)
    
    if (n_similar_pitch >= 5) {
      # Calculate batter's stats on similar pitches using actual columns
      rv100_similar <- 100 * mean(similar_pitches$hitter_rv, na.rm = TRUE)
      
      swings <- sum(similar_pitches$SwingIndicator, na.rm = TRUE)
      whiffs <- sum(similar_pitches$WhiffIndicator, na.rm = TRUE)
      whiff_pct <- if(swings > 0) 100 * whiffs / swings else NA
      
      ab <- sum(similar_pitches$ABindicator, na.rm = TRUE)
      hits <- sum(similar_pitches$HitIndicator, na.rm = TRUE)
      ba_similar <- if(ab > 0) hits / ab else NA
      
      tb <- sum(similar_pitches$totalbases, na.rm = TRUE)
      slg_similar <- if(ab > 0) tb / ab else NA
      
      # wOBA and wOBAcon if available
      woba_similar <- if("woba" %in% names(similar_pitches)) mean(similar_pitches$woba, na.rm = TRUE) else NA
      wobacon_similar <- if("wobacon" %in% names(similar_pitches)) mean(similar_pitches$wobacon, na.rm = TRUE) else NA
      
      # K% and BB% on similar pitches
      pa <- sum(similar_pitches$PAindicator, na.rm = TRUE)
      k_similar <- sum(similar_pitches$KorBB == "Strikeout", na.rm = TRUE)
      bb_similar <- sum(similar_pitches$KorBB == "Walk", na.rm = TRUE)
      k_pct_similar <- if(pa > 0) 100 * k_similar / pa else NA
      bb_pct_similar <- if(pa > 0) 100 * bb_similar / pa else NA
      
      # Apply shrinkage based on sample size (reduced k for more responsiveness)
      k <- 60
      shrinkage <- n_similar_pitch / (n_similar_pitch + k)
      rv100_shrunk <- shrinkage * rv100_similar + (1 - shrinkage) * 0
      
      results_by_pitch[[pitch_type]] <- list(
        n = n_similar_pitch,
        rv100 = rv100_shrunk,
        rv100_raw = rv100_similar,
        whiff_pct = whiff_pct,
        ba = ba_similar,
        slg = slg_similar,
        woba = woba_similar,
        wobacon = wobacon_similar,
        k_pct = k_pct_similar,
        bb_pct = bb_pct_similar,
        usage = usage
      )
    } else {
      # Not enough similar pitches - use neutral
      results_by_pitch[[pitch_type]] <- list(
        n = n_similar_pitch,
        rv100 = 0,
        rv100_raw = NA,
        whiff_pct = NA,
        ba = NA,
        slg = NA,
        woba = NA,
        wobacon = NA,
        k_pct = NA,
        bb_pct = NA,
        usage = usage
      )
    }
  }
  
  # ---- CALCULATE WEIGHTED RV/100 WITH PROPER USAGE WEIGHTING ----
  # Only weight by usage for pitch types with enough data
  weighted_rv100 <- 0
  total_usage_weight <- 0
  total_similar <- 0
  
  for (pt in names(results_by_pitch)) {
    pitch_data <- results_by_pitch[[pt]]
    usage <- pitch_data$usage
    
    if (pitch_data$n >= 5 && !is.na(pitch_data$rv100_raw)) {
      # Weight by SQUARED usage - so 50% usage pitch matters 6.25x more than 20% usage
      # This prevents low-usage pitches from swinging the score
      usage_weight <- usage^1.5  # Use 1.5 power for moderate weighting
      weighted_rv100 <- weighted_rv100 + pitch_data$rv100 * usage_weight
      total_usage_weight <- total_usage_weight + usage_weight
      total_similar <- total_similar + pitch_data$n
    }
  }
  
  if (total_usage_weight > 0) {
    weighted_rv100 <- weighted_rv100 / total_usage_weight
  } else {
    weighted_rv100 <- 0
  }
  
  # ---- K%/BB% ADJUSTMENT ----
  k_bb_adjustment <- 0
  if (nrow(hitter_overall) > 0 && nrow(pitcher_overall) > 0) {
    hitter_k_pct <- hitter_overall$K_pct[1]
    hitter_bb_pct <- hitter_overall$BB_pct[1]
    pitcher_k_pct <- pitcher_overall$K_pct[1]
    pitcher_bb_pct <- pitcher_overall$BB_pct[1]
    
    league_k_pct <- 22
    league_bb_pct <- 8
    
    # High K hitter vs High K pitcher -> pitcher boost
    if (!is.na(hitter_k_pct) && !is.na(pitcher_k_pct)) {
      if (hitter_k_pct > league_k_pct + 5 && pitcher_k_pct > league_k_pct + 3) {
        k_bb_adjustment <- k_bb_adjustment - 4
      }
      if (hitter_k_pct < league_k_pct - 5 && pitcher_k_pct < league_k_pct - 3) {
        k_bb_adjustment <- k_bb_adjustment + 4
      }
    }
    
    # High BB hitter vs High BB pitcher -> hitter boost
    if (!is.na(hitter_bb_pct) && !is.na(pitcher_bb_pct)) {
      if (hitter_bb_pct > league_bb_pct + 3 && pitcher_bb_pct > league_bb_pct + 2) {
        k_bb_adjustment <- k_bb_adjustment + 3
      }
    }
  }
  
  # ---- CONVERT RV/100 TO 0-100 SCORE ----
  # SCORING: 0 = hitter advantage, 100 = pitcher advantage
  # hitter_rv positive = good for hitter = LOW score
  # hitter_rv negative = good for pitcher = HIGH score
  
  # Linear scaling with moderate multiplier to avoid extremes
  # RV/100 of +1 (good for hitter) -> score ~35
  # RV/100 of -1 (good for pitcher) -> score ~65
  # RV/100 of 0 -> score 50
  
  rv_score_contribution <- -weighted_rv100 * 15  # Negative because positive RV = low score
  
  base_score <- 50 + rv_score_contribution + k_bb_adjustment
  
  # Clamp to 15-85 to avoid extreme scores (reserve 0-15 and 85-100 for truly extreme cases)
  final_score <- round(max(15, min(85, base_score)))
  
  # Only allow scores outside 15-85 for very extreme RV values (> 2 or < -2)
  if (abs(weighted_rv100) > 2) {
    final_score <- round(max(5, min(95, 50 + rv_score_contribution + k_bb_adjustment)))
  }
  
  # Calculate overall wOBA and wOBAcon from similar pitches
  overall_woba <- if(nrow(all_similar_pitches) > 0 && "woba" %in% names(all_similar_pitches)) {
    mean(all_similar_pitches$woba, na.rm = TRUE)
  } else NA
  
  overall_wobacon <- if(nrow(all_similar_pitches) > 0 && "wobacon" %in% names(all_similar_pitches)) {
    mean(all_similar_pitches$wobacon, na.rm = TRUE)
  } else NA
  
  overall_stuff <- if(nrow(pitcher_overall) > 0) pitcher_overall$Stuff_plus[1] else NA
  
  list(
    score = final_score,
    rv100 = round(weighted_rv100, 2),
    n_similar = total_similar,
    by_pitch = results_by_pitch,
    similar_pitches_data = all_similar_pitches,
    hitter_stats = hitter_overall,
    pitcher_stats = pitcher_overall,
    k_bb_adj = k_bb_adjustment,
    woba = overall_woba,
    wobacon = overall_wobacon,
    stuff_plus = overall_stuff
  )
}

# Legacy function wrapper for compatibility
get_hitter_grade <- function(h_name, split_name, metric = "overall") {
  # Handle case where split doesn't exist
  if (is.null(split_name) || length(split_name) == 0 || split_name == "") return(50)
  
  grade_row <- hitter_grades %>%
    filter(Batter == h_name & split == split_name)
  
  if (nrow(grade_row) == 0) return(50)
  
  val <- grade_row[[metric]][1]
  if (is.null(val) || is.na(val)) return(50)
  val
}

# Updated calculate_matchup to use MAC method
calculate_matchup <- function(p_name, h_name) {
  mac_result <- calculate_mac_matchup(p_name, h_name)
  
  # Build details table - use advanced stats (no BA)
  by_pitch <- mac_result$by_pitch
  if (!is.null(by_pitch) && length(by_pitch) > 0) {
    details <- tibble(
      `Pitch Type` = names(by_pitch),
      `Similar Pitches` = sapply(by_pitch, function(x) x$n),
      `RV/100` = sapply(by_pitch, function(x) round(x$rv100, 2)),
      `wOBA` = sapply(by_pitch, function(x) if(is.na(x$woba)) "-" else sprintf(".%03d", round(x$woba * 1000))),
      `SLG` = sapply(by_pitch, function(x) if(is.na(x$slg)) "-" else sprintf(".%03d", round(x$slg * 1000))),
      `Whiff%` = sapply(by_pitch, function(x) if(is.na(x$whiff_pct)) "-" else sprintf("%.1f%%", x$whiff_pct)),
      `Usage` = sapply(by_pitch, function(x) sprintf("%.0f%%", x$usage * 100))
    )
  } else {
    details <- NULL
  }
  
  list(
    score = mac_result$score, 
    hitter_grade = 100 - mac_result$score,  # Invert: higher hitter_grade = better for hitter
    rv100 = mac_result$rv100,
    n_similar = mac_result$n_similar,
    details = details,
    similar_pitches_data = mac_result$similar_pitches_data,
    hitter_stats = mac_result$hitter_stats,
    pitcher_stats = mac_result$pitcher_stats,
    k_bb_adj = mac_result$k_bb_adj,
    by_pitch = by_pitch,
    woba = mac_result$woba,
    wobacon = mac_result$wobacon,
    stuff_plus = mac_result$stuff_plus
  )
}

# ---- VISUALIZATION FUNCTIONS FOR MATCHUP DETAIL ----

# Create heatmap of similar pitches using density
create_similar_pitches_heatmap <- function(similar_data, color_by = "density") {
  if (is.null(similar_data) || nrow(similar_data) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "Not enough data", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  # Filter to valid locations
  plot_data <- similar_data %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight),
           PlateLocSide >= -2, PlateLocSide <= 2,
           PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  if (nrow(plot_data) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "Not enough data", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  # Use density heatmap approach
  ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
    scale_fill_gradientn(colours = c("white", "blue", "#FF9999", "red", "darkred"), name = "Density") +
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.6, ymax = 3.5, fill = NA, color = "black", linewidth = 1) +
    annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.8) +
    coord_fixed(ratio = 1) + xlim(-2, 2) + ylim(0, 4.5) +
    labs(title = paste0("Pitch Locations (n=", nrow(plot_data), ")")) +
    theme_void() +
    theme(legend.position = "right", plot.margin = margin(3, 3, 3, 3),
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
}

# Create density heatmap for specific metrics
create_similar_pitches_density_heatmap <- function(similar_data, metric = "rv") {
  if (is.null(similar_data) || nrow(similar_data) < 10) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "Not enough data", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  plot_data <- similar_data %>%
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight),
           PlateLocSide >= -2, PlateLocSide <= 2,
           PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  if (nrow(plot_data) < 10) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "Not enough data", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  base_theme <- theme_void() +
    theme(legend.position = "right", plot.margin = margin(3, 3, 3, 3),
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
          legend.key.height = unit(0.6, "cm"), legend.key.width = unit(0.3, "cm"))
  
  strike_zone <- list(
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.6, ymax = 3.5, fill = NA, color = "black", linewidth = 1),
    annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.8),
    coord_fixed(ratio = 1), xlim(-2, 2), ylim(0, 4.5)
  )
  
  if (metric == "rv") {
    p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_summary_2d(aes(z = hitter_rv), fun = mean, bins = 10, alpha = 0.85) +
      scale_fill_gradientn(
        colors = c("#1A9850", "#91CF60", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FC8D59", "#D73027"),
        limits = c(-0.3, 0.3), oob = scales::squish, name = "RV"
      ) +
      labs(title = "Run Value")
    
  } else if (metric == "whiff") {
    swing_data <- plot_data %>% filter(SwingIndicator == 1)
    if (nrow(swing_data) < 10) {
      return(ggplot() + annotate("text", x = 0, y = 2.5, label = "Not enough swings", size = 4, color = "gray50") + theme_void())
    }
    p <- ggplot(swing_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_summary_2d(aes(z = WhiffIndicator), fun = mean, bins = 10, alpha = 0.85) +
      scale_fill_gradientn(
        colors = c("#FFFFBF", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"),
        limits = c(0, 0.6), oob = scales::squish, name = "Whiff%"
      ) +
      labs(title = "Whiff Rate")
    
  } else if (metric == "damage") {
    bip_data <- plot_data %>% filter(PitchCall == "InPlay")
    if (nrow(bip_data) < 10) {
      return(ggplot() + annotate("text", x = 0, y = 2.5, label = "Not enough BIP", size = 4, color = "gray50") + theme_void())
    }
    bip_data <- bip_data %>%
      mutate(damage = case_when(
        PlayResult == "HomeRun" ~ 4,
        !is.na(ExitSpeed) & ExitSpeed >= 100 ~ 3,
        !is.na(ExitSpeed) & ExitSpeed >= 95 ~ 2,
        PlayResult %in% c("Double", "Triple") ~ 2,
        PlayResult == "Single" ~ 1,
        TRUE ~ 0.3
      ))
    p <- ggplot(bip_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_summary_2d(aes(z = damage), fun = mean, bins = 10, alpha = 0.85) +
      scale_fill_gradientn(
        colors = c("#FFFFBF", "#FED976", "#FEB24C", "#FD8D3C", "#E31A1C", "#800026"),
        limits = c(0, 3), oob = scales::squish, name = "Damage"
      ) +
      labs(title = "Damage on Contact")
  } else {
    # Default to density
    p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
      scale_fill_gradientn(colours = c("white", "blue", "#FF9999", "red", "darkred"), name = "Density") +
      labs(title = "Pitch Density")
  }
  
  p + strike_zone + base_theme
}

# Create movement chart with user-selectable coloring
create_similar_pitches_movement <- function(similar_data, color_by = "pitch_type") {
  if (is.null(similar_data) || nrow(similar_data) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Not enough data", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  plot_data <- similar_data %>%
    filter(!is.na(HorzBreak), !is.na(InducedVertBreak))
  
  if (nrow(plot_data) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Not enough data", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  base_plot <- ggplot(plot_data, aes(x = HorzBreak, y = InducedVertBreak)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8, color = "gray50"),
      legend.position = "bottom",
      legend.text = element_text(size = 7)
    )
  
  if (color_by == "pitch_type") {
    plot_data <- plot_data %>%
      mutate(pitch_type_label = case_when(
        matched_pitch_type == "FB" ~ "Fastball",
        matched_pitch_type == "BB" ~ "Breaking",
        matched_pitch_type == "OS" ~ "Offspeed",
        TRUE ~ "Other"
      ))
    base_plot + 
      geom_point(aes(color = pitch_type_label), size = 2.5, alpha = 0.7) +
      scale_color_manual(values = c("Fastball" = "#D73027", "Breaking" = "#4575B4", "Offspeed" = "#1A9850", "Other" = "gray60"), name = "") +
      labs(title = "Pitch Movement", subtitle = "By pitch type", x = "Horz Break", y = "IVB")
    
  } else if (color_by == "rv") {
    base_plot + 
      geom_point(aes(color = hitter_rv), size = 2.5, alpha = 0.8) +
      scale_color_gradientn(
        colors = c("#1A9850", "#91CF60", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FC8D59", "#D73027"),
        limits = c(-0.5, 0.5), oob = scales::squish, name = "RV"
      ) +
      labs(title = "Pitch Movement", subtitle = "By Run Value", x = "Horz Break", y = "IVB")
    
  } else if (color_by == "outcome") {
    plot_data <- plot_data %>%
      mutate(outcome_label = case_when(
        HitIndicator == 1 ~ "Hit",
        WhiffIndicator == 1 ~ "Whiff",
        ABindicator == 1 ~ "Out",
        TRUE ~ "Other"
      ))
    base_plot + 
      geom_point(aes(color = outcome_label), size = 2.5, alpha = 0.7) +
      scale_color_manual(values = c("Hit" = "#1A9850", "Whiff" = "#4575B4", "Out" = "#D73027", "Other" = "gray60"), name = "") +
      labs(title = "Pitch Movement", subtitle = "By outcome", x = "Horz Break", y = "IVB")
  } else {
    # Default
    plot_data <- plot_data %>%
      mutate(pitch_type_label = case_when(
        matched_pitch_type == "FB" ~ "Fastball",
        matched_pitch_type == "BB" ~ "Breaking",
        matched_pitch_type == "OS" ~ "Offspeed",
        TRUE ~ "Other"
      ))
    base_plot + 
      geom_point(aes(color = pitch_type_label), size = 2.5, alpha = 0.7) +
      scale_color_manual(values = c("Fastball" = "#D73027", "Breaking" = "#4575B4", "Offspeed" = "#1A9850", "Other" = "gray60"), name = "") +
      labs(title = "Pitch Movement", x = "Horz Break", y = "IVB")
  }
}

# Create spray chart for similar pitches (balls in play only)
create_similar_pitches_spray <- function(similar_data) {
  if (is.null(similar_data) || nrow(similar_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 150, label = "Not enough BIP", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  # Filter to balls in play with valid coordinates
  bip_data <- similar_data %>%
    filter(PitchCall == "InPlay", !is.na(Bearing), !is.na(Distance)) %>%
    mutate(
      # Convert bearing/distance to x/y
      x = Distance * sin(Bearing * pi / 180),
      y = Distance * cos(Bearing * pi / 180),
      hit_type = case_when(
        PlayResult == "HomeRun" ~ "HR",
        PlayResult == "Triple" ~ "3B",
        PlayResult == "Double" ~ "2B",
        PlayResult == "Single" ~ "1B",
        TRUE ~ "Out"
      )
    )
  
  if (nrow(bip_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 150, label = "Not enough BIP", size = 4, color = "gray50") +
        theme_void()
    )
  }
  
  # Field outline
  field_df <- tibble(
    x = c(-225, 0, 225),
    y = c(225, 400, 225)
  )
  
  ggplot(bip_data, aes(x = x, y = y)) +
    # Field arc
    annotate("path",
             x = 320 * cos(seq(pi/4, 3*pi/4, length.out = 50)),
             y = 320 * sin(seq(pi/4, 3*pi/4, length.out = 50)),
             color = "gray70", linewidth = 0.5) +
    # Infield
    annotate("path",
             x = 150 * cos(seq(pi/4, 3*pi/4, length.out = 50)),
             y = 150 * sin(seq(pi/4, 3*pi/4, length.out = 50)),
             color = "gray70", linewidth = 0.5, linetype = "dashed") +
    # Foul lines
    annotate("segment", x = 0, y = 0, xend = -320, yend = 320, color = "gray70") +
    annotate("segment", x = 0, y = 0, xend = 320, yend = 320, color = "gray70") +
    # Points
    geom_point(aes(color = hit_type), size = 3, alpha = 0.8) +
    scale_color_manual(
      values = c("HR" = "#67001F", "3B" = "#D73027", "2B" = "#FC8D59", 
                 "1B" = "#1A9850", "Out" = "gray50"),
      name = ""
    ) +
    coord_fixed(xlim = c(-350, 350), ylim = c(-20, 420)) +
    labs(
      title = paste0("Spray Chart (n=", nrow(bip_data), " BIP)"),
      subtitle = "Where batter hit similar pitches"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "gray50", hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )
}

build_matchup_matrix <- function(pitchers, hitters) {
  expand_grid(Pitcher = pitchers, Hitter = hitters) %>%
    rowwise() %>%
    mutate(
      result = list(calculate_matchup(Pitcher, Hitter)),
      score = result$score,
      hitter_grade = result$hitter_grade,
      rv100 = result$rv100,
      n_similar = result$n_similar
    ) %>%
    select(-result) %>%
    ungroup()
}

# ============================================================
# 6. SHINY UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
     body, table { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
     .app-header { display:flex; justify-content:center; align-items:center; padding:15px 20px; background:#ffffff; border-bottom:3px solid #006F71; margin-bottom:15px; }
     .header-title { font-size: 28px; font-weight: 700; color: #006F71; letter-spacing: 0.5px; }
     .nav-tabs{ border:none !important; border-radius:50px; padding:6px 12px; margin:10px auto; max-width:100%;
       background:linear-gradient(135deg, #e0f2f1 0%, #b2dfdb 50%, #e0f2f1 100%); box-shadow:0 4px 16px rgba(0,111,113,.12);
       border:1px solid rgba(0,111,113,.2); display:flex; justify-content:center; flex-wrap:wrap; gap:6px; }
     .nav-tabs>li>a{ color:#006F71 !important; border:none !important; border-radius:50px !important; background:transparent !important; font-weight:700; font-size:14px; padding:10px 22px; white-space:nowrap; transition:all .2s ease; }
     .nav-tabs>li>a:hover{ color:#004d4d !important; background:rgba(255,255,255,.5) !important; transform:translateY(-1px); }
     .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover{
       background:linear-gradient(135deg,#006F71 0%,#00897b 50%,#006F71 100%) !important;
       color:#fff !important; box-shadow:0 4px 16px rgba(0,111,113,.4); }
     .tab-content{ background:linear-gradient(135deg, rgba(255,255,255,.98), rgba(248,249,250,.98)); border-radius:15px; padding:20px; margin-top:10px; box-shadow:0 10px 30px rgba(0,111,113,.1); border:1px solid rgba(0,111,113,.1); }
     .well { background:#ffffff; border-radius:12px; border:1px solid rgba(0,111,113,.15); box-shadow:0 4px 12px rgba(0,0,0,.06); padding: 15px; }
     .control-label{ font-weight:700; color:#006F71; font-size: 13px; }
     .selectize-input, .form-control{ border-radius:8px; border:1px solid rgba(0,111,113,.3); }
     .btn-primary { background: linear-gradient(135deg,#006F71,#00897b); border: none; border-radius: 8px; }
     .btn-primary:hover { background: linear-gradient(135deg,#004d4d,#006F71); }
     .card-header.bg-dark { background: linear-gradient(135deg,#006F71,#00897b) !important; }
     hr { border-color: rgba(0,111,113,.2); }
   "))
  ),
  
  div(class = "app-header",
      span(class = "header-title", "Matchup Explorer - Scouting Cards")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Scouting Cards", style = "color: #006F71; margin-bottom: 15px;"),
      
      selectizeInput(
        "scouting_hitters",
        "Select Hitters",
        choices = all_hitters,
        selected = all_hitters[1:min(3, length(all_hitters))],
        multiple = TRUE,
        options = list(maxItems = 12, placeholder = "Choose hitters...")
      ),
      
      hr(),
      
      h5("PDF Export", style = "color: #006F71;"),
      textInput("pdf_title", "Report Title", value = "Opponent Scouting Report"),
      textInput("pdf_subtitle", "Subtitle", value = ""),
      fileInput("pdf_logo", "Upload Logo", accept = c("image/png", "image/jpeg")),
      downloadButton("download_pdf", "Download PDF", class = "btn-primary btn-sm w-100"),
      
      hr(),
      
      h5("Legend", style = "color: #006F71;"),
      div(style = "font-size: 12px;",
          p(tags$span(style = "color: #FA8072; font-weight: bold;", "FB"), " Fastball/Sinker"),
          p(tags$span(style = "color: #A020F0; font-weight: bold;", "BB"), " Breaking Balls"),
          p(tags$span(style = "color: #2E8B57; font-weight: bold;", "OS"), " Offspeed")
      ),
      
      hr(),
      
      h5("Confidence (Border)", style = "color: #006F71;"),
      div(style = "font-size: 12px;",
          p(tags$span(style = "border: 2px solid #1A9850; padding: 2px 6px; border-radius: 4px;", "High"), " n ≥ 80"),
          p(tags$span(style = "border: 2px solid #FDB863; padding: 2px 6px; border-radius: 4px;", "Med"), " 30-80"),
          p(tags$span(style = "border: 2px solid #D73027; padding: 2px 6px; border-radius: 4px;", "Low"), " n < 30")
      ),
      
      hr(),
      
      h5("Other Tools", style = "color: #006F71;"),
      selectizeInput(
        "pitcher_choices",
        "Pitchers (Matrix)",
        choices = all_pitchers,
        selected = all_pitchers[1:min(5, length(all_pitchers))],
        multiple = TRUE,
        options = list(maxItems = 10)
      ),
      selectizeInput(
        "hitter_choices",
        "Hitters (Matrix)",
        choices = all_hitters,
        selected = all_hitters[1:min(8, length(all_hitters))],
        multiple = TRUE,
        options = list(maxItems = 15)
      ),
      selectInput("profile_hitter", "Hitter Profile", choices = all_hitters, selected = all_hitters[1]),
      selectInput("profile_pitcher", "Pitcher Profile", choices = all_pitchers, selected = all_pitchers[1])
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        
        # Tab 1: Scouting Cards
        tabPanel(
          "Scouting Cards",
          div(style = "margin-top: 15px;",
              div(style = "overflow-x: auto; overflow-y: auto; max-height: 900px;",
                  uiOutput("scouting_cards_ui"))
          )
        ),
        
        # Tab 2: Matchup Matrix (MAC-Style)
        tabPanel(
          "Matchup Matrix",
          div(style = "margin-top: 15px;",
              
              # Pitching Staff Inputs
              div(style = "background: #e8f5e9; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #a5d6a7;",
                  h5("Pitching Staff", style = "color: #2e7d32; margin: 0 0 10px 0;"),
                  fluidRow(
                    column(3,
                           selectizeInput("matrix_sp", "Starting Pitcher",
                                          choices = all_pitchers, selected = NULL,
                                          options = list(placeholder = "Select SP..."))
                    ),
                    column(9,
                           selectizeInput("matrix_rp", "Relief Pitchers (RP1, RP2, ...)",
                                          choices = all_pitchers, selected = NULL, multiple = TRUE,
                                          options = list(maxItems = 10, placeholder = "Select relievers in order..."))
                    )
                  )
              ),
              
              # Lineup Inputs
              div(style = "background: #fff3e0; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #ffcc80;",
                  h5("Batting Lineup", style = "color: #e65100; margin: 0 0 10px 0;"),
                  fluidRow(
                    column(12,
                           selectizeInput("matrix_lineup", "Starting Lineup (1-9 in order)",
                                          choices = all_hitters, selected = NULL, multiple = TRUE,
                                          options = list(maxItems = 9, placeholder = "Select 9 hitters in batting order..."))
                    )
                  ),
                  fluidRow(
                    column(12,
                           selectizeInput("matrix_bench", "Bench / Pinch Hitters",
                                          choices = all_hitters, selected = NULL, multiple = TRUE,
                                          options = list(maxItems = 10, placeholder = "Select bench players..."))
                    )
                  )
              ),
              
              # Options row
              fluidRow(
                column(6,
                       radioButtons(
                         "matrix_color_perspective",
                         "Color Perspective",
                         choices = c("Hitter (green = hitter advantage)" = "hitter",
                                     "Pitcher (green = pitcher advantage)" = "pitcher"),
                         selected = "hitter",
                         inline = TRUE
                       )
                ),
                column(6,
                       div(style = "padding-top: 5px;",
                           checkboxInput("matrix_show_rv", "Show RV/100 instead of score", value = FALSE)
                       )
                )
              ),
              
              # Info box
              div(style = "background: #f8f9fa; padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                  h5("MAC Matchup Analysis", style = "color: #006F71; margin: 0 0 8px 0;"),
                  uiOutput("matrix_legend_text")
              ),
              
              # Matrix table
              gt_output("matchup_matrix_table"),
              
              hr(),
              
              # Detail breakdown
              h5("Matchup Detail", style = "color: #006F71;"),
              fluidRow(
                column(3,
                       selectInput("detail_matchup_pitcher", "Pitcher", choices = NULL)
                ),
                column(3,
                       selectInput("detail_matchup_hitter", "Hitter", choices = NULL)
                ),
                column(6,
                       div(style = "padding-top: 25px;",
                           actionButton("show_matchup_detail", "Show Breakdown", class = "btn-primary btn-sm"))
                )
              ),
              uiOutput("matrix_detail_ui")
          )
        ),
        
        # Tab 3: Hitter Profile
        tabPanel(
          "Hitter Profile",
          div(style = "margin-top: 15px; text-align: center;",
              h4("Hitter Profile - Coming Soon"),
              p("Detailed hitter analytics dashboard"))
        ),
        
        # Tab 4: Pitcher Profile
        tabPanel(
          "Pitcher Profile",
          div(style = "margin-top: 15px; text-align: center;",
              h4("Pitcher Profile - Coming Soon"),
              p("Detailed pitcher analytics dashboard"))
        ),
        
        # Tab 5: Detailed Matchup
        tabPanel(
          "Matchup Detail",
          div(style = "margin-top: 15px;",
              fluidRow(
                column(6, selectInput("detail_pitcher", "Select Pitcher", choices = all_pitchers)),
                column(6, selectInput("detail_hitter", "Select Hitter", choices = all_hitters))
              ),
              uiOutput("matchup_detail_ui"))
        )
      )
    )
  )
)

# ============================================================
# 7. SHINY SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Cache for scouting profiles
  scouting_cache <- reactiveVal(list())
  
  # Get or compute scouting profile
  get_scouting_profile <- function(h_name) {
    cache <- scouting_cache()
    if (h_name %in% names(cache)) {
      return(cache[[h_name]])
    }
    profile <- tryCatch(
      build_hitter_scouting_data(h_name, tm_data),
      error = function(e) {
        message("Error building profile for ", h_name, ": ", e$message)
        NULL
      }
    )
    cache[[h_name]] <- profile
    scouting_cache(cache)
    return(profile)
  }
  
  # --------------------------------------------------------
  # SCOUTING CARDS TAB
  # --------------------------------------------------------
  
  output$scouting_cards_ui <- renderUI({
    req(input$scouting_hitters)
    
    # Helper function for stat coloring based on SEC benchmarks
    # White = average, Green = above average (good for hitter), Red = below average
    # For called strikes: INVERSE - higher CS% = pitcher-friendly = red for hitter
    stat_color <- function(val, metric, inverse = FALSE) {
      if (is.na(val)) return("#E0E0E0")
      
      # SEC benchmark averages
      benchmarks <- list(
        fp_swing = 29,      # First pitch swing %
        ev = 88.9,
        ev90 = 104.8,
        whiff = 25,         # inverse - lower is better for hitter
        chase = 24,         # inverse
        z_swing = 68,       # Zone swing % - ~68% is average
        chase_2k = 36,      # 2K chase rate - inverse (36% is league avg)
        bb_pct = 12.1,
        k_pct = 21.2,       # inverse
        pull_pct = 40,      # Pull % - neutral around 40%
        oppo_pct = 25       # Oppo % - neutral around 25%
      )
      
      bm <- benchmarks[[metric]]
      if (is.null(bm)) return("#F5F5F5")  # Light gray for unknown
      
      # Calculate deviation from benchmark (as percentage)
      if (inverse) {
        # For inverse metrics, being below benchmark is GOOD
        pct_diff <- (bm - val) / bm * 100
      } else {
        # For normal metrics, being above benchmark is GOOD
        pct_diff <- (val - bm) / bm * 100
      }
      
      # Color scale: green-white-red (no yellow)
      if (pct_diff > 15) return("#1A9850")      # Dark green - well above avg
      if (pct_diff > 5) return("#91CF60")       # Light green - above avg
      if (pct_diff >= -5) return("#F5F5F5")     # Light gray - average
      if (pct_diff >= -15) return("#FC8D59")    # Light orange/red - below avg
      return("#D73027")                          # Red - well below avg
    }
    
    card_list <- lapply(input$scouting_hitters, function(h_name) {
      
      profile <- get_scouting_profile(h_name)
      if (is.null(profile)) return(NULL)
      
      # Border color based on run value per 100 pitches
      # Positive RV = good for hitter = bad matchup = RED
      # Negative RV = bad for hitter = good matchup = GREEN
      rv <- profile$rv_per_100
      if (is.na(rv) || is.null(rv)) rv <- 0
      
      border_color <- if (rv >= 1.5) {
        "#D73027"  # Strong hitter - avoid
      } else if (rv >= 0.5) {
        "#FC8D59"  # Above avg hitter
      } else if (rv >= -0.5) {
        "#F5F5F5"  # Average - light gray
      } else if (rv >= -1.5) {
        "#91CF60"  # Below avg hitter - decent matchup
      } else {
        "#1A9850"  # Weak hitter - good matchup
      }
      
      punishes_text <- if (length(profile$punishes) > 0) paste(profile$punishes, collapse = ", ") else "—"
      struggles_text <- if (length(profile$struggles) > 0) paste(profile$struggles, collapse = ", ") else "—"
      
      rs <- profile$raw_stats_overall
      safe_name <- gsub("[^A-Za-z0-9]", "", h_name)
      
      # COMPACT CARD
      div(
        style = paste0(
          "border: 2px solid ", border_color, "; ",
          "border-radius: 6px; ",
          "margin-bottom: 6px; ",
          "padding: 6px; ",
          "background: #fafafa; ",
          "font-size: 10px;"
        ),
        
        # ROW 1: Header with name, stats bar, and badges
        div(
          style = "display: grid; grid-template-columns: 180px 1fr auto; gap: 8px; align-items: center; margin-bottom: 4px; padding-bottom: 4px; border-bottom: 1px solid #ddd;",
          
          # Name + sample + RV + Hot/Cold
          div(
            span(style = "font-weight: bold; font-size: 12px; color: black;", paste0(profile$name, " (", profile$hand, ")")),
            # Hot/Cold indicator
            if (!is.null(profile$recent_trend) && profile$recent_trend == "hot") {
              span(style = "font-size: 14px; margin-left: 4px;", "🔥")
            } else if (!is.null(profile$recent_trend) && profile$recent_trend == "cold") {
              span(style = "font-size: 14px; margin-left: 4px;", "❄️")
            } else NULL,
            br(),
            span(style = "font-size: 9px; color: #666;", paste0("n=", profile$n)),
            span(style = paste0("font-size: 9px; font-weight: bold; margin-left: 8px; color: ", border_color, ";"), 
                 paste0("RV/100: ", if (!is.na(rv)) sprintf("%+.2f", rv) else "—"))
          ),
          
          # Stats bar - pill style (updated stats)
          div(
            style = "display: flex; gap: 4px; flex-wrap: wrap; font-size: 8px;",
            span(style = paste0("background: ", stat_color(rs$ev, "ev"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("EV:", if (!is.na(rs$ev)) round(rs$ev, 1) else "—")),
            span(style = paste0("background: ", stat_color(rs$ev90, "ev90"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("EV90:", if (!is.na(rs$ev90)) round(rs$ev90, 1) else "—")),
            span(style = paste0("background: ", stat_color(rs$fp_swing, "fp_swing"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("FPSw:", if (!is.na(rs$fp_swing)) paste0(round(rs$fp_swing, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$whiff, "whiff", TRUE), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("Whiff:", if (!is.na(rs$whiff)) paste0(round(rs$whiff, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$chase, "chase", TRUE), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("Chase:", if (!is.na(rs$chase)) paste0(round(rs$chase, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$z_swing, "z_swing"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("ZSw:", if (!is.na(rs$z_swing)) paste0(round(rs$z_swing, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$chase_2k, "chase_2k", TRUE), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("2KCh:", if (!is.na(rs$chase_2k)) paste0(round(rs$chase_2k, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$bb_pct, "bb_pct"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("BB%:", if (!is.na(rs$bb_pct)) paste0(round(rs$bb_pct, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$k_pct, "k_pct", TRUE), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("K%:", if (!is.na(rs$k_pct)) paste0(round(rs$k_pct, 1), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$pull_pct, "pull_pct"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("Pull:", if (!is.na(rs$pull_pct)) paste0(round(rs$pull_pct, 0), "%") else "—")),
            span(style = paste0("background: ", stat_color(rs$oppo_pct, "oppo_pct"), "; padding: 2px 4px; border-radius: 3px; font-weight: bold;"), 
                 paste0("Oppo:", if (!is.na(rs$oppo_pct)) paste0(round(rs$oppo_pct, 0), "%") else "—"))
          ),
          
          # Badges
          div(
            style = "display: flex; gap: 3px;",
            div(style = paste0("padding: 2px 5px; border-radius: 3px; font-size: 8px; font-weight: bold; color: black; background: ", 
                               if (profile$aggro_1p == "Aggressive") "#FC8D59" else if (profile$aggro_1p == "Patient") "#91CF60" else "#E0E0E0"), 
                paste0(substr(profile$aggro_1p, 1, 3), " 1P")),
            div(style = paste0("padding: 2px 5px; border-radius: 3px; font-size: 8px; font-weight: bold; color: black; background: ", 
                               if (profile$chase_2k == "High") "#FC8D59" else if (profile$chase_2k == "Low") "#91CF60" else "#E0E0E0"), 
                paste0(substr(profile$chase_2k, 1, 2), " 2K")),
            div(style = paste0("padding: 2px 5px; border-radius: 3px; font-size: 8px; font-weight: bold; color: black; background: ", 
                               if (profile$zone_aggro == "Aggressive") "#FC8D59" else if (profile$zone_aggro == "Passive") "#91CF60" else "#E0E0E0"), 
                paste0(substr(profile$zone_aggro, 1, 3), " IZ"))
          )
        ),
        
        # ROW 2: Main content - 4 columns (Grades | RHP Heatmaps | LHP Heatmaps | Movement Plots)
        div(
          style = "display: grid; grid-template-columns: 140px 1fr 1fr 280px; gap: 6px;",
          
          # COL 1: Grades + Spray Charts
          div(
            # Punishes/Struggles
            div(style = "font-size: 9px; margin-bottom: 4px;",
                span(style = "color: #1A9850; font-weight: bold;", "Punish: "), span(punishes_text),
                br(),
                span(style = "color: #D73027; font-weight: bold;", "Struggle: "), span(struggles_text)
            ),
            # Grades - Now with 5 columns: Raw PWR, Game PWR, CON, AvK, DEC
            div(
              style = "display: grid; grid-template-columns: 30px repeat(5, 1fr); gap: 2px; font-size: 7px; margin-bottom: 6px;",
              div(), div(style = "text-align: center;", "Raw"), div(style = "text-align: center;", "Game"), 
              div(style = "text-align: center;", "CON"), div(style = "text-align: center;", "AvK"), div(style = "text-align: center;", "DEC"),
              div(style = "font-weight: bold;", "RHP"),
              div(style = paste0("background: ", grade_color(profile$rhp_raw_power), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$rhp_raw_power),
              div(style = paste0("background: ", grade_color(profile$rhp_power), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$rhp_power),
              div(style = paste0("background: ", grade_color(profile$rhp_contact), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$rhp_contact),
              div(style = paste0("background: ", grade_color(profile$rhp_avoid_k), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$rhp_avoid_k),
              div(style = paste0("background: ", grade_color(profile$rhp_swing_dec), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$rhp_swing_dec),
              div(style = "font-weight: bold;", "LHP"),
              div(style = paste0("background: ", grade_color(profile$lhp_raw_power), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$lhp_raw_power),
              div(style = paste0("background: ", grade_color(profile$lhp_power), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$lhp_power),
              div(style = paste0("background: ", grade_color(profile$lhp_contact), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$lhp_contact),
              div(style = paste0("background: ", grade_color(profile$lhp_avoid_k), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$lhp_avoid_k),
              div(style = paste0("background: ", grade_color(profile$lhp_swing_dec), "; text-align: center; padding: 2px; border-radius: 2px; font-weight: bold;"), profile$lhp_swing_dec)
            ),
            # Spray Charts - 2x2 grid
            div(style = "font-size: 8px; font-weight: bold; text-align: center; margin-bottom: 2px;", "Spray Charts"),
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 2px;",
              div(style = "text-align: center;",
                  div(style = "font-size: 7px; color: #666;", "vs RHP"),
                  plotOutput(paste0("spray_rhp_", safe_name), height = "60px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 7px; color: #666;", "vs LHP"),
                  plotOutput(paste0("spray_lhp_", safe_name), height = "60px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 7px; color: #666;", "0-1 K"),
                  plotOutput(paste0("spray_01k_", safe_name), height = "60px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 7px; color: #666;", "2 K"),
                  plotOutput(paste0("spray_2k_", safe_name), height = "60px"))
            ),
            # Pitch Family Stats
            div(style = "font-size: 7px; margin-top: 4px; border-top: 1px solid #ddd; padding-top: 3px;",
                div(style = "font-weight: bold; text-align: center; margin-bottom: 2px;", "vs Pitch Type (SLG | Whiff%)"),
                div(style = "display: grid; grid-template-columns: 25px 1fr 1fr; gap: 1px; font-size: 6px;",
                    div(style = "color: #FA8072; font-weight: bold;", "FB"),
                    div(style = "text-align: center;", {
                      fb_rhp <- profile$splits[["RHP_FB"]]$stats
                      slg <- if (!is.null(fb_rhp$SLG) && !is.na(fb_rhp$SLG)) sprintf(".%03d", round(fb_rhp$SLG * 1000)) else "-"
                      whiff <- if (!is.null(fb_rhp$Whiff_pct) && !is.na(fb_rhp$Whiff_pct)) sprintf("%.0f%%", fb_rhp$Whiff_pct) else "-"
                      paste0("R: ", slg, "|", whiff)
                    }),
                    div(style = "text-align: center;", {
                      fb_lhp <- profile$splits[["LHP_FB"]]$stats
                      slg <- if (!is.null(fb_lhp$SLG) && !is.na(fb_lhp$SLG)) sprintf(".%03d", round(fb_lhp$SLG * 1000)) else "-"
                      whiff <- if (!is.null(fb_lhp$Whiff_pct) && !is.na(fb_lhp$Whiff_pct)) sprintf("%.0f%%", fb_lhp$Whiff_pct) else "-"
                      paste0("L: ", slg, "|", whiff)
                    }),
                    div(style = "color: #A020F0; font-weight: bold;", "BB"),
                    div(style = "text-align: center;", {
                      bb_rhp <- profile$splits[["RHP_BB"]]$stats
                      slg <- if (!is.null(bb_rhp$SLG) && !is.na(bb_rhp$SLG)) sprintf(".%03d", round(bb_rhp$SLG * 1000)) else "-"
                      whiff <- if (!is.null(bb_rhp$Whiff_pct) && !is.na(bb_rhp$Whiff_pct)) sprintf("%.0f%%", bb_rhp$Whiff_pct) else "-"
                      paste0("R: ", slg, "|", whiff)
                    }),
                    div(style = "text-align: center;", {
                      bb_lhp <- profile$splits[["LHP_BB"]]$stats
                      slg <- if (!is.null(bb_lhp$SLG) && !is.na(bb_lhp$SLG)) sprintf(".%03d", round(bb_lhp$SLG * 1000)) else "-"
                      whiff <- if (!is.null(bb_lhp$Whiff_pct) && !is.na(bb_lhp$Whiff_pct)) sprintf("%.0f%%", bb_lhp$Whiff_pct) else "-"
                      paste0("L: ", slg, "|", whiff)
                    }),
                    div(style = "color: #2E8B57; font-weight: bold;", "OS"),
                    div(style = "text-align: center;", {
                      os_rhp <- profile$splits[["RHP_OS"]]$stats
                      slg <- if (!is.null(os_rhp$SLG) && !is.na(os_rhp$SLG)) sprintf(".%03d", round(os_rhp$SLG * 1000)) else "-"
                      whiff <- if (!is.null(os_rhp$Whiff_pct) && !is.na(os_rhp$Whiff_pct)) sprintf("%.0f%%", os_rhp$Whiff_pct) else "-"
                      paste0("R: ", slg, "|", whiff)
                    }),
                    div(style = "text-align: center;", {
                      os_lhp <- profile$splits[["LHP_OS"]]$stats
                      slg <- if (!is.null(os_lhp$SLG) && !is.na(os_lhp$SLG)) sprintf(".%03d", round(os_lhp$SLG * 1000)) else "-"
                      whiff <- if (!is.null(os_lhp$Whiff_pct) && !is.na(os_lhp$Whiff_pct)) sprintf("%.0f%%", os_lhp$Whiff_pct) else "-"
                      paste0("L: ", slg, "|", whiff)
                    })
                )
            )
          ),
          
          # COL 2: vs RHP - Pies + SLG + Hard Hit + Whiff
          div(
            style = "background: white; padding: 4px; border-radius: 4px;",
            div(style = "font-weight: bold; font-size: 9px; text-align: center; margin-bottom: 2px;", "vs RHP"),
            # Pies
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 3px; margin-bottom: 3px;",
              div(style = "text-align: center;", 
                  div(style = "font-size: 8px;", "Usage"),
                  plotOutput(paste0("pie_rhp_overall_", safe_name), height = "45px")),
              div(style = "text-align: center;", 
                  div(style = "font-size: 8px;", "1P"),
                  plotOutput(paste0("pie_rhp_1p_", safe_name), height = "45px")),
              div(style = "text-align: center;", 
                  div(style = "font-size: 8px;", "2K"),
                  plotOutput(paste0("pie_rhp_putaway_", safe_name), height = "45px"))
            ),
            # Swing% Heatmaps row (where they swing)
            div(style = "font-size: 7px; font-weight: bold; color: #666; margin: 2px 0 1px 0;", "Whiff% (blue=whiff)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #FA8072; font-weight: bold;", "FB"),
                  plotOutput(paste0("hm_rhp_whiff_fb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #A020F0; font-weight: bold;", "BB"),
                  plotOutput(paste0("hm_rhp_whiff_bb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #2E8B57; font-weight: bold;", "OS"),
                  plotOutput(paste0("hm_rhp_whiff_os_", safe_name), height = "55px"))
            ),
            # xValue Heatmaps row (combined pitch value)
            div(style = "font-size: 7px; font-weight: bold; color: #666; margin: 2px 0 1px 0;", "xValue (blue=pitch here)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_rhp_xval_fb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_rhp_xval_bb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_rhp_xval_os_", safe_name), height = "55px"))
            ),
            # xDamage Heatmaps row (expected damage if contact)
            div(style = "font-size: 7px; font-weight: bold; color: #666; margin: 2px 0 1px 0;", "xDamage (red=danger)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_rhp_xdmg_fb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_rhp_xdmg_bb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_rhp_xdmg_os_", safe_name), height = "55px"))
            )
          ),
          
          # COL 3: vs LHP - Pies + SLG + Hard Hit + Whiff
          div(
            style = "background: white; padding: 4px; border-radius: 4px;",
            div(style = "font-weight: bold; font-size: 9px; text-align: center; margin-bottom: 2px;", "vs LHP"),
            # Pies
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 3px; margin-bottom: 3px;",
              div(style = "text-align: center;", 
                  div(style = "font-size: 8px;", "Usage"),
                  plotOutput(paste0("pie_lhp_overall_", safe_name), height = "45px")),
              div(style = "text-align: center;", 
                  div(style = "font-size: 8px;", "1P"),
                  plotOutput(paste0("pie_lhp_1p_", safe_name), height = "45px")),
              div(style = "text-align: center;", 
                  div(style = "font-size: 8px;", "2K"),
                  plotOutput(paste0("pie_lhp_putaway_", safe_name), height = "45px"))
            ),
            # Swing% Heatmaps row (where they swing)
            div(style = "font-size: 7px; font-weight: bold; color: #666; margin: 2px 0 1px 0;", "Whiff% (blue=whiff)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #FA8072; font-weight: bold;", "FB"),
                  plotOutput(paste0("hm_lhp_whiff_fb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #A020F0; font-weight: bold;", "BB"),
                  plotOutput(paste0("hm_lhp_whiff_bb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #2E8B57; font-weight: bold;", "OS"),
                  plotOutput(paste0("hm_lhp_whiff_os_", safe_name), height = "55px"))
            ),
            # xValue Heatmaps row (combined pitch value)
            div(style = "font-size: 7px; font-weight: bold; color: #666; margin: 2px 0 1px 0;", "xValue (blue=pitch here)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_lhp_xval_fb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_lhp_xval_bb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_lhp_xval_os_", safe_name), height = "55px"))
            ),
            # xDamage Heatmaps row (expected damage if contact)
            div(style = "font-size: 7px; font-weight: bold; color: #666; margin: 2px 0 1px 0;", "xDamage (red=danger)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_lhp_xdmg_fb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_lhp_xdmg_bb_", safe_name), height = "55px")),
              div(style = "text-align: center;",
                  plotOutput(paste0("hm_lhp_xdmg_os_", safe_name), height = "55px"))
            )
          ),
          
          # COL 4: Movement Plots + Velo Splits + Notes
          div(
            style = "background: white; padding: 4px; border-radius: 4px;",
            # vs RHP Shapes
            div(style = "font-weight: bold; font-size: 8px; text-align: center; margin-bottom: 2px;", "vs RHP Movement (red=whiff, green=95+EV)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #FA8072; font-weight: bold;", "FB"),
                  plotOutput(paste0("mvmt_rhp_fb_", safe_name), height = "100px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #A020F0; font-weight: bold;", "BB"),
                  plotOutput(paste0("mvmt_rhp_bb_", safe_name), height = "100px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #2E8B57; font-weight: bold;", "OS"),
                  plotOutput(paste0("mvmt_rhp_os_", safe_name), height = "100px"))
            ),
            # RHP Velo Splits
            div(style = "font-size: 7px; font-weight: bold; margin: 2px 0; border-top: 1px solid #eee; padding-top: 2px;", "RHP Velo RV/100"),
            uiOutput(paste0("velo_splits_rhp_", safe_name)),
            
            # vs LHP Shapes  
            div(style = "font-weight: bold; font-size: 8px; text-align: center; margin: 4px 0 2px 0; border-top: 1px solid #ddd; padding-top: 4px;", "vs LHP Movement (red=whiff, green=95+EV)"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 2px;",
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #FA8072; font-weight: bold;", "FB"),
                  plotOutput(paste0("mvmt_lhp_fb_", safe_name), height = "100px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #A020F0; font-weight: bold;", "BB"),
                  plotOutput(paste0("mvmt_lhp_bb_", safe_name), height = "100px")),
              div(style = "text-align: center;",
                  div(style = "font-size: 6px; color: #2E8B57; font-weight: bold;", "OS"),
                  plotOutput(paste0("mvmt_lhp_os_", safe_name), height = "100px"))
            ),
            # LHP Velo Splits
            div(style = "font-size: 7px; font-weight: bold; margin: 2px 0; border-top: 1px solid #eee; padding-top: 2px;", "LHP Velo RV/100"),
            uiOutput(paste0("velo_splits_lhp_", safe_name))
          )
        ),
        
        # ROW 3: Hit/Out Charts (2 rows) + Stacked Comments + In-Game Notes
        div(
          style = "margin-top: 6px; padding-top: 6px; border-top: 2px solid #ddd;",
          div(
            style = "display: grid; grid-template-columns: 2fr 1fr 2fr; gap: 8px;",
            
            # LEFT: Hit/Out Location Charts in 2x2 grid
            div(
              div(style = "font-weight: bold; font-size: 9px; margin-bottom: 4px; color: #006F71;", "Hit & Out Locations (FB=red, BB=purple, OS=green)"),
              # Row 1: Overall Hits and Outs
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 4px; margin-bottom: 4px;",
                div(plotOutput(paste0("hitout_overall_hits_", safe_name), height = "100px")),
                div(plotOutput(paste0("hitout_overall_outs_", safe_name), height = "100px"))
              ),
              # Row 2: 2K Hits and Outs
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 4px;",
                div(plotOutput(paste0("hitout_2k_hits_", safe_name), height = "100px")),
                div(plotOutput(paste0("hitout_2k_outs_", safe_name), height = "100px"))
              )
            ),
            
            # MIDDLE: Stacked Comments
            div(
              div(style = "font-weight: bold; font-size: 9px; margin-bottom: 4px; color: #006F71;", "Pitch Plan"),
              div(
                div(style = "font-weight: bold; font-size: 7px; margin-bottom: 1px;", "Overall"),
                tags$textarea(
                  id = paste0("notes_overall_", safe_name),
                  style = "width: 100%; height: 70px; font-size: 8px; border: 1px solid #ddd; border-radius: 3px; padding: 3px; resize: none; margin-bottom: 4px;",
                  placeholder = "Overall approach..."
                )
              ),
              div(
                div(style = "font-weight: bold; font-size: 7px; margin-bottom: 1px;", "vs RHP"),
                tags$textarea(
                  id = paste0("notes_rhp_", safe_name),
                  style = "width: 100%; height: 70px; font-size: 8px; border: 1px solid #ddd; border-radius: 3px; padding: 3px; resize: none; margin-bottom: 4px;",
                  placeholder = "Pitch plan vs RHP..."
                )
              ),
              div(
                div(style = "font-weight: bold; font-size: 7px; margin-bottom: 1px;", "vs LHP"),
                tags$textarea(
                  id = paste0("notes_lhp_", safe_name),
                  style = "width: 100%; height: 70px; font-size: 8px; border: 1px solid #ddd; border-radius: 3px; padding: 3px; resize: none;",
                  placeholder = "Pitch plan vs LHP..."
                )
              )
            ),
            
            # RIGHT: In-Game Notes Template (5 AB boxes)
            div(
              div(style = "font-weight: bold; font-size: 9px; margin-bottom: 4px; color: #006F71;", "In Game Notes"),
              # AB number row
              div(
                style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 4px; text-align: center; font-weight: bold; font-size: 12px; margin-bottom: 2px;",
                div("1"), div("2"), div("3"), div("4"), div("5")
              ),
              # P: and I: labels stacked
              div(
                style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 4px; font-size: 8px; color: #666; margin-bottom: 2px;",
                div(style = "line-height: 1.2;", HTML("P:<br>I:")),
                div(style = "line-height: 1.2;", HTML("P:<br>I:")),
                div(style = "line-height: 1.2;", HTML("P:<br>I:")),
                div(style = "line-height: 1.2;", HTML("P:<br>I:")),
                div(style = "line-height: 1.2;", HTML("P:<br>I:"))
              ),
              # Diamond symbols (for runners on base) - BIGGER
              div(
                style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 4px; text-align: center; margin-bottom: 4px;",
                div(style = "font-size: 36px; color: #aaa; line-height: 1;", HTML("&#9671;")),
                div(style = "font-size: 36px; color: #aaa; line-height: 1;", HTML("&#9671;")),
                div(style = "font-size: 36px; color: #aaa; line-height: 1;", HTML("&#9671;")),
                div(style = "font-size: 36px; color: #aaa; line-height: 1;", HTML("&#9671;")),
                div(style = "font-size: 36px; color: #aaa; line-height: 1;", HTML("&#9671;"))
              ),
              # Note boxes for each AB - BIGGER
              div(
                style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 4px;",
                div(style = "border: 1px solid #bbb; height: 90px; border-radius: 3px;"),
                div(style = "border: 1px solid #bbb; height: 90px; border-radius: 3px;"),
                div(style = "border: 1px solid #bbb; height: 90px; border-radius: 3px;"),
                div(style = "border: 1px solid #bbb; height: 90px; border-radius: 3px;"),
                div(style = "border: 1px solid #bbb; height: 90px; border-radius: 3px;")
              )
            )
          )
        )
      )
    })
    
    do.call(tagList, card_list)
  })
  
  # Dynamically create outputs for each hitter
  observe({
    req(input$scouting_hitters)
    
    for (h_name in input$scouting_hitters) {
      local({
        hitter <- h_name
        safe_name <- gsub("[^A-Za-z0-9]", "", hitter)
        
        profile <- get_scouting_profile(hitter)
        if (is.null(profile)) return()
        
        border_col <- switch(as.character(profile$confidence),
                             "1" = "#D73027", "2" = "#FFD700", "3" = "#1A9850")
        
        # Pie charts - RHP
        output[[paste0("pie_rhp_overall_", safe_name)]] <- renderPlot({
          create_usage_pie(profile$rec_rhp_overall, border_col)
        }, bg = "transparent")
        
        output[[paste0("pie_rhp_1p_", safe_name)]] <- renderPlot({
          create_usage_pie(profile$rec_rhp_1p, border_col)
        }, bg = "transparent")
        
        output[[paste0("pie_rhp_putaway_", safe_name)]] <- renderPlot({
          create_usage_pie(profile$rec_rhp_putaway, border_col)
        }, bg = "transparent")
        
        # Pie charts - LHP
        output[[paste0("pie_lhp_overall_", safe_name)]] <- renderPlot({
          create_usage_pie(profile$rec_lhp_overall, border_col)
        }, bg = "transparent")
        
        output[[paste0("pie_lhp_1p_", safe_name)]] <- renderPlot({
          create_usage_pie(profile$rec_lhp_1p, border_col)
        }, bg = "transparent")
        
        output[[paste0("pie_lhp_putaway_", safe_name)]] <- renderPlot({
          create_usage_pie(profile$rec_lhp_putaway, border_col)
        }, bg = "transparent")
        
        # Heatmaps - RHP Swing%
        output[[paste0("hm_rhp_whiff_fb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "FB", "Right", "Whiff")
        }, bg = "transparent")
        
        output[[paste0("hm_rhp_whiff_bb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "BB", "Right", "Whiff")
        }, bg = "transparent")
        
        output[[paste0("hm_rhp_whiff_os_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "OS", "Right", "Whiff")
        }, bg = "transparent")
        
        # Heatmaps - RHP xValue
        output[[paste0("hm_rhp_xval_fb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "FB", "Right", "xValue")
        }, bg = "transparent")
        
        output[[paste0("hm_rhp_xval_bb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "BB", "Right", "xValue")
        }, bg = "transparent")
        
        output[[paste0("hm_rhp_xval_os_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "OS", "Right", "xValue")
        }, bg = "transparent")
        
        # Heatmaps - RHP xDamage
        output[[paste0("hm_rhp_xdmg_fb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "FB", "Right", "xDamage")
        }, bg = "transparent")
        
        output[[paste0("hm_rhp_xdmg_bb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "BB", "Right", "xDamage")
        }, bg = "transparent")
        
        output[[paste0("hm_rhp_xdmg_os_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "OS", "Right", "xDamage")
        }, bg = "transparent")
        
        # Heatmaps - LHP Swing%
        output[[paste0("hm_lhp_whiff_fb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "FB", "Left", "Whiff")
        }, bg = "transparent")
        
        output[[paste0("hm_lhp_whiff_bb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "BB", "Left", "Whiff")
        }, bg = "transparent")
        
        output[[paste0("hm_lhp_whiff_os_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "OS", "Left", "Whiff")
        }, bg = "transparent")
        
        # Heatmaps - LHP xValue
        output[[paste0("hm_lhp_xval_fb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "FB", "Left", "xValue")
        }, bg = "transparent")
        
        output[[paste0("hm_lhp_xval_bb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "BB", "Left", "xValue")
        }, bg = "transparent")
        
        output[[paste0("hm_lhp_xval_os_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "OS", "Left", "xValue")
        }, bg = "transparent")
        
        # Heatmaps - LHP xDamage
        output[[paste0("hm_lhp_xdmg_fb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "FB", "Left", "xDamage")
        }, bg = "transparent")
        
        output[[paste0("hm_lhp_xdmg_bb_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "BB", "Left", "xDamage")
        }, bg = "transparent")
        
        output[[paste0("hm_lhp_xdmg_os_", safe_name)]] <- renderPlot({
          create_metric_heatmap(tm_data, hitter, "OS", "Left", "xDamage")
        }, bg = "transparent")
        
        # Spray charts
        output[[paste0("spray_rhp_", safe_name)]] <- renderPlot({
          create_mini_spray(tm_data, hitter, "hand", "Right")
        }, bg = "transparent")
        
        output[[paste0("spray_lhp_", safe_name)]] <- renderPlot({
          create_mini_spray(tm_data, hitter, "hand", "Left")
        }, bg = "transparent")
        
        output[[paste0("spray_01k_", safe_name)]] <- renderPlot({
          create_mini_spray(tm_data, hitter, "strikes", "0-1")
        }, bg = "transparent")
        
        output[[paste0("spray_2k_", safe_name)]] <- renderPlot({
          create_mini_spray(tm_data, hitter, "strikes", "2")
        }, bg = "transparent")
        
        # Movement plots - vs RHP (by pitch family)
        output[[paste0("mvmt_rhp_fb_", safe_name)]] <- renderPlot({
          create_movement_plot(tm_data, hitter, "Right", "FB")
        }, bg = "transparent")
        
        output[[paste0("mvmt_rhp_bb_", safe_name)]] <- renderPlot({
          create_movement_plot(tm_data, hitter, "Right", "BB")
        }, bg = "transparent")
        
        output[[paste0("mvmt_rhp_os_", safe_name)]] <- renderPlot({
          create_movement_plot(tm_data, hitter, "Right", "OS")
        }, bg = "transparent")
        
        # Movement plots - vs LHP (by pitch family)
        output[[paste0("mvmt_lhp_fb_", safe_name)]] <- renderPlot({
          create_movement_plot(tm_data, hitter, "Left", "FB")
        }, bg = "transparent")
        
        output[[paste0("mvmt_lhp_bb_", safe_name)]] <- renderPlot({
          create_movement_plot(tm_data, hitter, "Left", "BB")
        }, bg = "transparent")
        
        output[[paste0("mvmt_lhp_os_", safe_name)]] <- renderPlot({
          create_movement_plot(tm_data, hitter, "Left", "OS")
        }, bg = "transparent")
        
        # Velo splits - vs RHP (using RV/100)
        output[[paste0("velo_splits_rhp_", safe_name)]] <- renderUI({
          pgs <- profile$pitch_group_stats
          
          lines <- list()
          
          # FB velo splits
          fb <- pgs[["RHP_FB"]]
          if (!is.null(fb$key_splits[["Hard"]])) {
            rv <- fb$key_splits[["Hard"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("FB92+:", rv_str))))
          }
          if (!is.null(fb$key_splits[["Soft"]])) {
            rv <- fb$key_splits[["Soft"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("FB<88:", rv_str))))
          }
          
          # BB velo splits
          bb <- pgs[["RHP_BB"]]
          if (!is.null(bb$key_splits[["HardBB"]])) {
            rv <- bb$key_splits[["HardBB"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("BB84+:", rv_str))))
          }
          if (!is.null(bb$key_splits[["SoftBB"]])) {
            rv <- bb$key_splits[["SoftBB"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("BB<76:", rv_str))))
          }
          
          # OS velo splits
          os <- pgs[["RHP_OS"]]
          if (!is.null(os$key_splits[["HardOS"]])) {
            rv <- os$key_splits[["HardOS"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("OS84+:", rv_str))))
          }
          if (!is.null(os$key_splits[["SoftOS"]])) {
            rv <- os$key_splits[["SoftOS"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; font-size: 7px;"), paste0("OS<76:", rv_str))))
          }
          
          if (length(lines) == 0) return(span(style = "color: #888; font-size: 7px;", "Low n"))
          
          div(style = "font-size: 7px; display: flex; flex-wrap: wrap;", lines)
        })
        
        # Velo splits - vs LHP (using RV/100)
        output[[paste0("velo_splits_lhp_", safe_name)]] <- renderUI({
          pgs <- profile$pitch_group_stats
          
          lines <- list()
          
          # FB velo splits
          fb <- pgs[["LHP_FB"]]
          if (!is.null(fb$key_splits[["Hard"]])) {
            rv <- fb$key_splits[["Hard"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("FB92+:", rv_str))))
          }
          if (!is.null(fb$key_splits[["Soft"]])) {
            rv <- fb$key_splits[["Soft"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("FB<88:", rv_str))))
          }
          
          # BB velo splits
          bb <- pgs[["LHP_BB"]]
          if (!is.null(bb$key_splits[["HardBB"]])) {
            rv <- bb$key_splits[["HardBB"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("BB84+:", rv_str))))
          }
          if (!is.null(bb$key_splits[["SoftBB"]])) {
            rv <- bb$key_splits[["SoftBB"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("BB<76:", rv_str))))
          }
          
          # OS velo splits
          os <- pgs[["LHP_OS"]]
          if (!is.null(os$key_splits[["HardOS"]])) {
            rv <- os$key_splits[["HardOS"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; margin-right: 5px; font-size: 7px;"), paste0("OS84+:", rv_str))))
          }
          if (!is.null(os$key_splits[["SoftOS"]])) {
            rv <- os$key_splits[["SoftOS"]]$rv100
            rv_str <- sprintf("%+.1f", rv)
            color <- if (rv >= 1) "#1A9850" else if (rv <= -1) "#D73027" else "#666"
            lines <- c(lines, list(span(style = paste0("color: ", color, "; font-size: 7px;"), paste0("OS<76:", rv_str))))
          }
          
          if (length(lines) == 0) return(span(style = "color: #888; font-size: 7px;", "Low n"))
          
          div(style = "font-size: 7px; display: flex; flex-wrap: wrap;", lines)
        })
        
        # Hit/Out Location Charts (replacing splits tables)
        output[[paste0("hitout_overall_hits_", safe_name)]] <- renderPlot({
          create_hit_out_chart(tm_data, hitter, "overall_hits")
        }, bg = "transparent")
        
        output[[paste0("hitout_overall_outs_", safe_name)]] <- renderPlot({
          create_hit_out_chart(tm_data, hitter, "overall_outs")
        }, bg = "transparent")
        
        output[[paste0("hitout_2k_hits_", safe_name)]] <- renderPlot({
          create_hit_out_chart(tm_data, hitter, "2k_hits")
        }, bg = "transparent")
        
        output[[paste0("hitout_2k_outs_", safe_name)]] <- renderPlot({
          create_hit_out_chart(tm_data, hitter, "2k_outs")
        }, bg = "transparent")
      })
    }
  })
  
  # --------------------------------------------------------
  # MATCHUP MATRIX TAB
  # --------------------------------------------------------
  
  # Combine pitchers from SP and RP inputs
  all_matrix_pitchers <- reactive({
    sp <- input$matrix_sp
    rp <- input$matrix_rp
    pitchers <- c()
    if (!is.null(sp) && sp != "") pitchers <- c(pitchers, sp)
    if (!is.null(rp) && length(rp) > 0) pitchers <- c(pitchers, rp)
    pitchers
  })
  
  # Combine hitters from lineup and bench inputs  
  all_matrix_hitters <- reactive({
    lineup <- input$matrix_lineup
    bench <- input$matrix_bench
    hitters <- c()
    if (!is.null(lineup) && length(lineup) > 0) hitters <- c(hitters, lineup)
    if (!is.null(bench) && length(bench) > 0) hitters <- c(hitters, bench)
    hitters
  })
  
  # Update detail dropdowns when matrix selections change
  observe({
    pitchers <- all_matrix_pitchers()
    if (length(pitchers) > 0) {
      updateSelectInput(session, "detail_matchup_pitcher", 
                        choices = pitchers,
                        selected = pitchers[1])
    }
  })
  
  observe({
    hitters <- all_matrix_hitters()
    if (length(hitters) > 0) {
      updateSelectInput(session, "detail_matchup_hitter", 
                        choices = hitters,
                        selected = hitters[1])
    }
  })
  
  # Dynamic legend text based on color perspective
  output$matrix_legend_text <- renderUI({
    perspective <- input$matrix_color_perspective
    show_rv <- input$matrix_show_rv
    if (is.null(perspective)) perspective <- "hitter"
    if (is.null(show_rv)) show_rv <- FALSE
    
    if (show_rv) {
      p(style = "font-size: 12px; color: #666; margin: 0;",
        "Showing RV/100 (batter perspective). ",
        tags$span(style = "color: #1A9850; font-weight: bold;", "Positive"), " = Hitter advantage, ",
        tags$span(style = "color: #D73027; font-weight: bold;", "Negative"), " = Pitcher advantage"
      )
    } else if (perspective == "hitter") {
      # CORRECTED: 100 = pitcher advantage, 0 = hitter advantage
      # From hitter perspective: green = low score = good for hitter
      p(style = "font-size: 12px; color: #666; margin: 0;",
        "Scores: 0-100 scale. ",
        tags$span(style = "color: #1A9850; font-weight: bold;", "Green (0-40)"), " = Hitter advantage, ",
        tags$span(style = "color: #B8860B; font-weight: bold;", "Yellow (41-59)"), " = Neutral, ",
        tags$span(style = "color: #D73027; font-weight: bold;", "Red (60-100)"), " = Pitcher advantage"
      )
    } else {
      # Pitcher perspective: green = high score = good for pitcher
      p(style = "font-size: 12px; color: #666; margin: 0;",
        "Scores: 0-100 scale. ",
        tags$span(style = "color: #D73027; font-weight: bold;", "Red (0-40)"), " = Hitter advantage, ",
        tags$span(style = "color: #B8860B; font-weight: bold;", "Yellow (41-59)"), " = Neutral, ",
        tags$span(style = "color: #1A9850; font-weight: bold;", "Green (60-100)"), " = Pitcher advantage"
      )
    }
  })
  
  matchup_df <- reactive({
    pitchers <- all_matrix_pitchers()
    hitters <- all_matrix_hitters()
    req(length(pitchers) > 0, length(hitters) > 0)
    build_matchup_matrix(pitchers, hitters)
  })
  
  output$matchup_matrix_table <- render_gt({
    df <- matchup_df()
    req(nrow(df) > 0)
    
    perspective <- input$matrix_color_perspective
    show_rv <- input$matrix_show_rv
    if (is.null(perspective)) perspective <- "hitter"
    if (is.null(show_rv)) show_rv <- FALSE
    
    # Get lineup info to mark starters vs bench
    lineup <- input$matrix_lineup
    bench <- input$matrix_bench
    sp <- input$matrix_sp
    rp <- input$matrix_rp
    
    # Create lookup vectors for positions
    rp_positions <- if (!is.null(rp) && length(rp) > 0) setNames(seq_along(rp), rp) else character(0)
    lineup_positions <- if (!is.null(lineup) && length(lineup) > 0) setNames(seq_along(lineup), lineup) else character(0)
    bench_positions <- if (!is.null(bench) && length(bench) > 0) setNames(seq_along(bench), bench) else character(0)
    
    # Safe SP value (scalar, can be empty string)
    sp_pitcher <- if (!is.null(sp) && length(sp) > 0 && sp != "") sp else ""
    
    df <- df %>%
      left_join(pitcher_hands, by = "Pitcher") %>%
      left_join(hitter_hands, by = c("Hitter" = "Batter")) %>%
      mutate(
        # Create pitcher labels with SP/RP designation
        PitcherRole = case_when(
          sp_pitcher != "" & Pitcher == sp_pitcher ~ "SP",
          Pitcher %in% names(rp_positions) ~ paste0("RP", match(Pitcher, names(rp_positions))),
          TRUE ~ ""
        ),
        PitcherLabel = paste0(
          ifelse(PitcherRole != "", paste0(PitcherRole, ": "), ""),
          Pitcher, " (", substr(PitcherThrows, 1, 1), ")"
        ),
        # Create hitter labels with lineup position  
        HitterPos = case_when(
          Hitter %in% names(lineup_positions) ~ as.character(match(Hitter, names(lineup_positions))),
          Hitter %in% names(bench_positions) ~ "BE",
          TRUE ~ ""
        ),
        HitterLabel = paste0(
          ifelse(HitterPos != "", paste0(HitterPos, ". "), ""),
          Hitter, " (", substr(BatterSide, 1, 1), ")"
        ),
        # Order for sorting - use match() which handles NAs properly
        hitter_order = case_when(
          Hitter %in% names(lineup_positions) ~ match(Hitter, names(lineup_positions)),
          Hitter %in% names(bench_positions) ~ 10L + match(Hitter, names(bench_positions)),
          TRUE ~ 99L
        ),
        pitcher_order = case_when(
          sp_pitcher != "" & Pitcher == sp_pitcher ~ 1L,
          Pitcher %in% names(rp_positions) ~ 1L + match(Pitcher, names(rp_positions)),
          TRUE ~ 99L
        )
      )
    
    # Determine display value
    if (show_rv) {
      df <- df %>% mutate(display_value = rv100)
    } else {
      df <- df %>% mutate(display_value = score)
    }
    
    # Sort and pivot
    matrix_df <- df %>%
      arrange(hitter_order) %>%
      select(HitterLabel, PitcherLabel, display_value, pitcher_order) %>%
      arrange(pitcher_order) %>%
      select(-pitcher_order) %>%
      pivot_wider(names_from = PitcherLabel, values_from = display_value, values_fill = NA)
    
    # Get pitcher columns in correct order
    pitcher_order_df <- df %>% 
      select(PitcherLabel, pitcher_order) %>% 
      distinct() %>% 
      arrange(pitcher_order)
    pitcher_cols <- pitcher_order_df$PitcherLabel
    pitcher_cols <- pitcher_cols[pitcher_cols %in% names(matrix_df)]
    
    # Reorder columns
    matrix_df <- matrix_df %>% select(HitterLabel, all_of(pitcher_cols))
    
    # Color palette based on perspective and mode
    # SCORING: 0 = hitter advantage, 100 = pitcher advantage
    if (show_rv) {
      # RV mode: green = positive (hitter good), red = negative (pitcher good)
      gt_table <- matrix_df %>%
        gt(rowname_col = "HitterLabel") %>%
        data_color(
          columns = all_of(pitcher_cols),
          fn = scales::col_numeric(
            palette = c("#D73027", "#FC8D59", "#FEE08B", "#FFFFBF", "#D9EF8B", "#91CF60", "#1A9850"),
            domain = c(-3, 3),
            na.color = "grey90"
          )
        ) %>%
        fmt_number(columns = all_of(pitcher_cols), decimals = 2, force_sign = TRUE)
    } else if (perspective == "hitter") {
      # Score mode, hitter perspective: green = LOW score = hitter advantage
      # 0 = hitter advantage (green), 100 = pitcher advantage (red)
      gt_table <- matrix_df %>%
        gt(rowname_col = "HitterLabel") %>%
        data_color(
          columns = all_of(pitcher_cols),
          fn = scales::col_numeric(
            palette = c("#1A9850", "#91CF60", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FC8D59", "#D73027"),
            domain = c(0, 100),
            na.color = "grey90"
          )
        )
    } else {
      # Score mode, pitcher perspective: green = HIGH score = pitcher advantage
      # 0 = hitter advantage (red), 100 = pitcher advantage (green)
      gt_table <- matrix_df %>%
        gt(rowname_col = "HitterLabel") %>%
        data_color(
          columns = all_of(pitcher_cols),
          fn = scales::col_numeric(
            palette = c("#D73027", "#FC8D59", "#FEE08B", "#FFFFBF", "#D9EF8B", "#91CF60", "#1A9850"),
            domain = c(0, 100),
            na.color = "grey90"
          )
        )
    }
    
    # Determine which rows are bench players for styling
    bench_rows <- which(grepl("^BE\\.", matrix_df$HitterLabel))
    
    # Build final table
    gt_table <- gt_table %>%
      tab_stubhead(label = "Batter") %>%
      tab_spanner(label = "Pitchers", columns = all_of(pitcher_cols)) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_style(style = cell_text(weight = "bold", size = px(11)), locations = cells_stub()) %>%
      tab_style(style = cell_text(weight = "bold", size = px(10)), locations = cells_column_labels()) %>%
      tab_style(style = cell_text(size = px(13), weight = "bold"), locations = cells_body()) %>%
      tab_options(table.font.size = px(11), data_row.padding = px(6), table.width = pct(100))
    
    # Add gray background to bench rows
    if (length(bench_rows) > 0) {
      gt_table <- gt_table %>%
        tab_style(
          style = cell_fill(color = "#f5f5f5"),
          locations = cells_stub(rows = bench_rows)
        )
    }
    
    # Add header
    subtitle_text <- if (show_rv) {
      "RV/100 (batter perspective) based on similar pitch profiles"
    } else if (perspective == "hitter") {
      "<span style='color:#1A9850'>■</span> Hitter Advantage | <span style='color:#FFFFBF'>■</span> Neutral | <span style='color:#D73027'>■</span> Pitcher Advantage"
    } else {
      "<span style='color:#D73027'>■</span> Hitter Advantage | <span style='color:#FFFFBF'>■</span> Neutral | <span style='color:#1A9850'>■</span> Pitcher Advantage"
    }
    
    gt_table %>%
      tab_header(
        title = md("**MAC Matchup Matrix**"), 
        subtitle = md(subtitle_text)
      )
  })
  
  # Detailed breakdown for selected matchup
  output$matrix_detail_ui <- renderUI({
    req(input$show_matchup_detail)
    
    isolate({
      p_name <- input$detail_matchup_pitcher
      h_name <- input$detail_matchup_hitter
      
      req(p_name, h_name)
      
      result <- calculate_matchup(p_name, h_name)
      
      perspective <- input$matrix_color_perspective
      if (is.null(perspective)) perspective <- "hitter"
      
      # Determine background color - 100 = pitcher advantage, 0 = hitter advantage
      bg_color <- if(result$score >= 60) "#FDAE61" else if(result$score <= 40) "#D9EF8B" else "#FFFFBF"
      
      # Get hitter and pitcher stats
      h_stats <- result$hitter_stats
      p_stats <- result$pitcher_stats
      
      div(
        style = "background: white; padding: 20px; border-radius: 8px; border: 1px solid #ddd; margin-top: 15px;",
        
        # Header
        h4(paste0(h_name, " vs ", p_name), style = "color: #006F71; margin-bottom: 20px; border-bottom: 2px solid #006F71; padding-bottom: 10px;"),
        
        # Score summary row
        fluidRow(
          column(3,
                 div(
                   style = paste0("padding: 20px; border-radius: 8px; background: ", bg_color, "; text-align: center;"),
                   div(style = "font-size: 40px; font-weight: bold;", result$score),
                   div(style = "font-size: 12px; color: #666; margin-top: 5px;", "Matchup Score"),
                   div(style = "font-size: 10px; color: #888; margin-top: 3px;", 
                       if(result$score >= 60) "Pitcher Advantage" 
                       else if(result$score <= 40) "Hitter Advantage"
                       else "Neutral")
                 )
          ),
          column(3,
                 div(
                   style = "padding: 15px; text-align: center; background: #f8f9fa; border-radius: 8px;",
                   div(style = "font-size: 24px; font-weight: bold;", sprintf("%+.2f", result$rv100)),
                   div(style = "font-size: 11px; color: #666;", "Batter RV/100"),
                   div(style = "font-size: 10px; color: #888; margin-top: 5px;",
                       paste0("vs Similar Pitches"))
                 )
          ),
          column(3,
                 div(
                   style = "padding: 15px; text-align: center; background: #f8f9fa; border-radius: 8px;",
                   div(style = "font-size: 24px; font-weight: bold;", result$n_similar),
                   div(style = "font-size: 11px; color: #666;", "Similar Pitches"),
                   div(style = "font-size: 10px; color: #888; margin-top: 5px;",
                       if(result$n_similar >= 150) "High confidence" 
                       else if(result$n_similar >= 50) "Medium confidence"
                       else "Low confidence")
                 )
          ),
          column(3,
                 div(
                   style = "padding: 15px; text-align: center; background: #f8f9fa; border-radius: 8px;",
                   div(style = "font-size: 16px; font-weight: bold;", 
                       if(!is.null(result$k_bb_adj) && result$k_bb_adj != 0) sprintf("%+d", result$k_bb_adj) else "0"),
                   div(style = "font-size: 11px; color: #666;", "K%/BB% Adj"),
                   div(style = "font-size: 10px; color: #888; margin-top: 5px;",
                       "Based on tendencies")
                 )
          )
        ),
        
        hr(style = "margin: 20px 0;"),
        
        # Player stats comparison
        fluidRow(
          column(6,
                 div(style = "background: #fff3e0; padding: 12px; border-radius: 8px;",
                     h6(paste0("Hitter: ", h_name), style = "color: #e65100; margin-bottom: 10px;"),
                     if (!is.null(h_stats) && nrow(h_stats) > 0) {
                       div(style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 10px; font-size: 12px;",
                           div(div(style = "font-weight: bold;", sprintf(".%03d", round(h_stats$BA[1] * 1000))), div(style = "color: #666;", "BA")),
                           div(div(style = "font-weight: bold;", sprintf(".%03d", round(h_stats$SLG[1] * 1000))), div(style = "color: #666;", "SLG")),
                           div(div(style = "font-weight: bold;", sprintf("%.1f%%", h_stats$K_pct[1])), div(style = "color: #666;", "K%")),
                           div(div(style = "font-weight: bold;", sprintf("%.1f%%", h_stats$BB_pct[1])), div(style = "color: #666;", "BB%"))
                       )
                     } else {
                       p("No stats available", style = "color: #666;")
                     }
                 )
          ),
          column(6,
                 div(style = "background: #e8f5e9; padding: 12px; border-radius: 8px;",
                     h6(paste0("Pitcher: ", p_name), style = "color: #2e7d32; margin-bottom: 10px;"),
                     if (!is.null(p_stats) && nrow(p_stats) > 0) {
                       div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; font-size: 12px;",
                           div(div(style = "font-weight: bold;", sprintf(".%03d", round(p_stats$BA_against[1] * 1000))), div(style = "color: #666;", "BAA")),
                           div(div(style = "font-weight: bold;", sprintf("%.1f%%", p_stats$K_pct[1])), div(style = "color: #666;", "K%")),
                           div(div(style = "font-weight: bold;", sprintf("%.1f%%", p_stats$BB_pct[1])), div(style = "color: #666;", "BB%"))
                       )
                     } else {
                       p("No stats available", style = "color: #666;")
                     }
                 )
          )
        ),
        
        hr(style = "margin: 20px 0;"),
        
        # Pitch type breakdown table
        if (!is.null(result$details) && nrow(result$details) > 0) {
          div(
            h6("Performance by Pitch Type", style = "color: #006F71; margin-bottom: 10px;"),
            tableOutput("matchup_pitch_breakdown")
          )
        },
        
        hr(style = "margin: 20px 0;"),
        
        # Additional metrics row (wOBA, wOBAcon, Stuff+)
        fluidRow(
          column(4,
                 div(style = "background: #e3f2fd; padding: 12px; border-radius: 8px; text-align: center;",
                     div(style = "font-size: 20px; font-weight: bold;", 
                         if(!is.null(result$woba) && !is.na(result$woba)) sprintf(".%03d", round(result$woba * 1000)) else "-"),
                     div(style = "font-size: 11px; color: #666;", "wOBA vs Similar")
                 )
          ),
          column(4,
                 div(style = "background: #e3f2fd; padding: 12px; border-radius: 8px; text-align: center;",
                     div(style = "font-size: 20px; font-weight: bold;", 
                         if(!is.null(result$wobacon) && !is.na(result$wobacon)) sprintf(".%03d", round(result$wobacon * 1000)) else "-"),
                     div(style = "font-size: 11px; color: #666;", "wOBAcon vs Similar")
                 )
          ),
          column(4,
                 div(style = "background: #e3f2fd; padding: 12px; border-radius: 8px; text-align: center;",
                     div(style = "font-size: 20px; font-weight: bold;", 
                         if(!is.null(result$stuff_plus) && !is.na(result$stuff_plus)) sprintf("%.0f", result$stuff_plus) else "-"),
                     div(style = "font-size: 11px; color: #666;", "Pitcher Stuff+")
                 )
          )
        ),
        
        hr(style = "margin: 20px 0;"),
        
        # Visualizations with controls
        h6("Similar Pitch Analysis", style = "color: #006F71; margin-bottom: 15px;"),
        
        # Visualization control row
        fluidRow(
          column(4,
                 selectInput("detail_heatmap_color", "Location Color By:", 
                             choices = c("Outcome" = "outcome", "Run Value" = "rv", "Whiff" = "whiff", "BA" = "ba"),
                             selected = "outcome", width = "100%")
          ),
          column(4,
                 selectInput("detail_movement_color", "Movement Color By:",
                             choices = c("Pitch Type" = "pitch_type", "Run Value" = "rv", "Outcome" = "outcome"),
                             selected = "pitch_type", width = "100%")
          ),
          column(4,
                 selectInput("detail_heatmap_type", "Heatmap Type:",
                             choices = c("Points" = "points", "RV Density" = "rv", "Whiff Density" = "whiff", "Damage Density" = "damage"),
                             selected = "points", width = "100%")
          )
        ),
        
        # Visualizations row
        fluidRow(
          column(4, plotOutput("matchup_detail_heatmap", height = "280px")),
          column(4, plotOutput("matchup_detail_movement", height = "280px")),
          column(4, plotOutput("matchup_detail_spray", height = "280px"))
        ),
        
        # Second row of heatmaps (density)
        hr(style = "margin: 15px 0;"),
        h6("Zone Heatmaps", style = "color: #006F71; margin-bottom: 10px;"),
        fluidRow(
          column(4, plotOutput("matchup_detail_rv_heatmap", height = "250px")),
          column(4, plotOutput("matchup_detail_whiff_heatmap", height = "250px")),
          column(4, plotOutput("matchup_detail_damage_heatmap", height = "250px"))
        )
      )
    })
  })
  
  # Store current matchup result for plots
  current_matchup_result <- reactiveVal(NULL)
  
  observeEvent(input$show_matchup_detail, {
    p_name <- input$detail_matchup_pitcher
    h_name <- input$detail_matchup_hitter
    if (!is.null(p_name) && !is.null(h_name)) {
      result <- calculate_mac_matchup(p_name, h_name)
      current_matchup_result(result)
    }
  })
  
  # Render the pitch breakdown table
  output$matchup_pitch_breakdown <- renderTable({
    req(input$show_matchup_detail)
    
    p_name <- input$detail_matchup_pitcher
    h_name <- input$detail_matchup_hitter
    
    req(p_name, h_name)
    
    result <- calculate_matchup(p_name, h_name)
    
    if (!is.null(result$details) && nrow(result$details) > 0) {
      result$details
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Heatmap of similar pitches (points with user-selected coloring)
  output$matchup_detail_heatmap <- renderPlot({
    result <- current_matchup_result()
    req(result)
    color_by <- input$detail_heatmap_color
    if (is.null(color_by)) color_by <- "outcome"
    create_similar_pitches_heatmap(result$similar_pitches_data, color_by = color_by)
  }, bg = "transparent")
  
  # Movement chart with user-selected coloring
  output$matchup_detail_movement <- renderPlot({
    result <- current_matchup_result()
    req(result)
    color_by <- input$detail_movement_color
    if (is.null(color_by)) color_by <- "pitch_type"
    create_similar_pitches_movement(result$similar_pitches_data, color_by = color_by)
  }, bg = "transparent")
  
  # Spray chart
  output$matchup_detail_spray <- renderPlot({
    result <- current_matchup_result()
    req(result)
    create_similar_pitches_spray(result$similar_pitches_data)
  }, bg = "transparent")
  
  # RV Density heatmap
  output$matchup_detail_rv_heatmap <- renderPlot({
    result <- current_matchup_result()
    req(result)
    create_similar_pitches_density_heatmap(result$similar_pitches_data, metric = "rv")
  }, bg = "transparent")
  
  # Whiff Density heatmap
  output$matchup_detail_whiff_heatmap <- renderPlot({
    result <- current_matchup_result()
    req(result)
    create_similar_pitches_density_heatmap(result$similar_pitches_data, metric = "whiff")
  }, bg = "transparent")
  
  # Damage Density heatmap
  output$matchup_detail_damage_heatmap <- renderPlot({
    result <- current_matchup_result()
    req(result)
    create_similar_pitches_density_heatmap(result$similar_pitches_data, metric = "damage")
  }, bg = "transparent")
  
  # --------------------------------------------------------
  # HITTER PROFILE TAB
  # --------------------------------------------------------
  
  output$hitter_overall_grades <- render_gt({
    req(input$profile_hitter)
    
    grades <- hitter_grades %>%
      filter(Batter == input$profile_hitter, split == "overall") %>%
      select(swing_decisions, contact, game_power, raw_power, avoid_k, overall) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Grade") %>%
      mutate(
        Metric = case_when(
          Metric == "swing_decisions" ~ "Swing Decisions",
          Metric == "contact" ~ "Contact",
          Metric == "game_power" ~ "Game Power",
          Metric == "raw_power" ~ "Raw Power",
          Metric == "avoid_k" ~ "Avoid K",
          Metric == "overall" ~ "OVERALL",
          TRUE ~ Metric
        )
      )
    
    grades %>%
      gt() %>%
      gt_theme_guardian() %>%
      data_color(
        columns = Grade,
        fn = scales::col_numeric(palette = c("#D73027", "#FFFFBF", "#1A9850"), domain = c(20, 80))
      ) %>%
      tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = Metric == "OVERALL")) %>%
      tab_header(title = paste("Grades:", input$profile_hitter))
  })
  
  output$hitter_split_grades <- render_gt({
    req(input$profile_hitter)
    
    grades <- hitter_grades %>%
      filter(Batter == input$profile_hitter, split != "overall") %>%
      select(split, n, overall, contact, game_power, swing_decisions) %>%
      arrange(desc(n)) %>%
      mutate(
        Split = case_when(
          split == "vs_RHP" ~ "vs RHP",
          split == "vs_LHP" ~ "vs LHP",
          split == "vs_FB" ~ "vs Fastball",
          split == "vs_BRK" ~ "vs Breaking",
          split == "vs_OS" ~ "vs Offspeed",
          split == "vs_hard" ~ "vs Hard (94+)",
          split == "vs_med" ~ "vs Med (89-93)",
          split == "vs_soft" ~ "vs Soft (<89)",
          split == "elevated" ~ "Elevated",
          split == "low" ~ "Low",
          split == "ahead" ~ "Ahead",
          split == "behind" ~ "Behind",
          split == "two_strike" ~ "2 Strikes",
          TRUE ~ split
        )
      ) %>%
      select(Split, N = n, Overall = overall, Contact = contact, Power = game_power, `Swing Dec` = swing_decisions)
    
    grades %>%
      gt() %>%
      gt_theme_guardian() %>%
      data_color(
        columns = c(Overall, Contact, Power, `Swing Dec`),
        fn = scales::col_numeric(palette = c("#D73027", "#FFFFBF", "#1A9850"), domain = c(20, 80))
      ) %>%
      tab_header(title = "Performance by Split")
  })
  
  # --------------------------------------------------------
  # PITCHER PROFILE TAB
  # --------------------------------------------------------
  
  output$pitcher_arsenal <- render_gt({
    req(input$profile_pitcher)
    
    prof <- pitcher_profiles %>% filter(Pitcher == input$profile_pitcher)
    if (nrow(prof) == 0) return(NULL)
    
    tibble(
      Metric = c("Pitches", "Throws", "Avg Velo", "FB%", "Breaking%", "Offspeed%",
                 "Hard%", "Med%", "Soft%", "Elevated%", "Low%"),
      Value = c(prof$pitches, prof$PitcherThrows, round(prof$avg_velo, 1),
                round(prof$fb_pct, 1), round(prof$brk_pct, 1), round(prof$os_pct, 1),
                round(prof$hard_pct, 1), round(prof$med_pct, 1), round(prof$soft_pct, 1),
                round(prof$elevated_pct, 1), round(prof$low_pct, 1))
    ) %>%
      gt() %>%
      gt_theme_guardian() %>%
      tab_header(title = paste("Profile:", input$profile_pitcher))
  })
  
  output$pitcher_tendencies <- renderPlot({
    req(input$profile_pitcher)
    
    prof <- pitcher_profiles %>% filter(Pitcher == input$profile_pitcher)
    if (nrow(prof) == 0) return(NULL)
    
    mix_df <- tibble(
      Type = c("Fastball", "Breaking", "Offspeed"),
      Pct = c(prof$fb_pct, prof$brk_pct, prof$os_pct)
    ) %>%
      mutate(Type = factor(Type, levels = c("Fastball", "Breaking", "Offspeed")))
    
    ggplot(mix_df, aes(x = Type, y = Pct, fill = Type)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(round(Pct), "%")), vjust = -0.5, fontface = "bold") +
      scale_fill_manual(values = c("Fastball" = "#FA8072", "Breaking" = "#A020F0", "Offspeed" = "#2E8B57")) +
      scale_y_continuous(limits = c(0, max(mix_df$Pct) * 1.15)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
      labs(x = NULL, y = "Usage %", title = "Pitch Mix")
  })
  
  # --------------------------------------------------------
  # MATCHUP DETAIL TAB
  # --------------------------------------------------------
  
  output$matchup_detail_table <- render_gt({
    req(input$detail_pitcher, input$detail_hitter)
    
    result <- calculate_matchup(input$detail_pitcher, input$detail_hitter)
    if (is.null(result$details)) return(NULL)
    
    result$details %>%
      gt() %>%
      gt_theme_guardian() %>%
      data_color(
        columns = `Hitter Grade`,
        fn = scales::col_numeric(palette = c("#1A9850", "#FFFFBF", "#D73027"), domain = c(20, 80))
      ) %>%
      tab_header(title = paste(input$detail_pitcher, "vs", input$detail_hitter))
  })
  
  output$matchup_summary <- renderUI({
    req(input$detail_pitcher, input$detail_hitter)
    
    result <- calculate_matchup(input$detail_pitcher, input$detail_hitter)
    
    score_color <- if (result$score >= 60) "#1A9850" else if (result$score <= 40) "#D73027" else "#666"
    advantage <- if (result$score >= 60) "Pitcher Advantage" else if (result$score <= 40) "Hitter Advantage" else "Even"
    
    div(
      style = "text-align: center; padding: 20px; background: #f5f5f5; border-radius: 8px;",
      h3(paste("Matchup Score:", result$score), style = paste("color:", score_color)),
      h4(paste("Hitter Grade:", result$hitter_grade)),
      p(strong(advantage), style = paste("color:", score_color, "; font-size: 1.2em;"))
    )
  })
  
  # --------------------------------------------------------
  # PDF EXPORT
  # --------------------------------------------------------
  
  # PDF creation function for scouting cards
  create_scouting_pdf <- function(tm_data, hitter_name, profile, output_file) {
    if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
    
    # Create PDF - landscape 11x17
    pdf(output_file, width = 17, height = 11, paper = "USr")
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    
    grid::grid.newpage()
    
    # Title section
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.97, width = 1, height = 0.05, just = c("center", "top")))
    grid::grid.text("Opponent Scouting Report",
                    gp = grid::gpar(fontface = "bold", cex = 1.3, col = "#006F71"))
    grid::popViewport()
    
    # Player name
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.93, width = 1, height = 0.04, just = c("center", "top")))
    grid::grid.text(paste0(hitter_name, " (", profile$batter_side, ")"),
                    gp = grid::gpar(fontface = "bold", cex = 1.6, col = "black"))
    grid::popViewport()
    
    # Stats table
    rs <- profile$raw_stats
    headers <- c("n", "RV/100", "EV", "Whiff%", "K%", "BB%", "Chase%", "Zone%")
    values <- c(profile$n, sprintf("%+.2f", profile$rv100), 
                round(rs$ev, 1), round(rs$whiff, 1),
                round(rs$k_pct, 1), round(rs$bb_pct, 1),
                round(rs$chase, 1), round(rs$zone_contact, 1))
    
    col_w <- 0.09
    x0 <- 0.5 - (length(headers) * col_w) / 2
    yh <- 0.87
    yv <- 0.85
    
    for (i in seq_along(headers)) {
      xi <- x0 + (i - 1) * col_w
      
      # Header with teal background
      grid::grid.rect(x = xi, y = yh, width = col_w * 0.985, height = 0.018,
                      just = c("left", "top"),
                      gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5))
      grid::grid.text(headers[i],
                      x = xi + col_w * 0.49, y = yh - 0.009,
                      gp = grid::gpar(col = "white", cex = 0.70, fontface = "bold"))
      
      # Value cell
      val <- values[i]
      grid::grid.rect(x = xi, y = yv, width = col_w * 0.985, height = 0.018,
                      just = c("left", "top"),
                      gp = grid::gpar(fill = "white", col = "black", lwd = 0.4))
      grid::grid.text(ifelse(is.finite(as.numeric(val)) | grepl("^[+-]", val), as.character(val), "-"),
                      x = xi + col_w * 0.49, y = yv - 0.009,
                      gp = grid::gpar(cex = 0.70))
    }
    
    # Punish/Struggle text
    grid::grid.text(paste0("Punish: ", profile$punishes),
                    x = 0.05, y = 0.80, just = "left",
                    gp = grid::gpar(cex = 0.8, col = "#1A9850", fontface = "bold"))
    grid::grid.text(paste0("Struggle: ", profile$struggles),
                    x = 0.05, y = 0.77, just = "left",
                    gp = grid::gpar(cex = 0.8, col = "#D73027", fontface = "bold"))
    
    # Create plots
    # Spray charts (left side)
    spray_rhp <- create_mini_spray(tm_data, hitter_name, "hand", "Right")
    spray_lhp <- create_mini_spray(tm_data, hitter_name, "hand", "Left")
    
    grid::pushViewport(grid::viewport(x = 0.12, y = 0.60, width = 0.18, height = 0.25, just = c("center", "top")))
    grid::grid.text("vs RHP", y = 1.02, gp = grid::gpar(cex = 0.7, fontface = "bold"))
    print(spray_rhp, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.12, y = 0.32, width = 0.18, height = 0.25, just = c("center", "top")))
    grid::grid.text("vs LHP", y = 1.02, gp = grid::gpar(cex = 0.7, fontface = "bold"))
    print(spray_lhp, newpage = FALSE)
    grid::popViewport()
    
    # Heatmaps (middle section)
    # vs RHP heatmaps
    grid::grid.text("vs RHP Heatmaps", x = 0.42, y = 0.74, gp = grid::gpar(cex = 0.8, fontface = "bold"))
    
    hm_width <- 0.12
    hm_height <- 0.18
    
    # SLG heatmaps
    slg_rhp_fb <- create_metric_heatmap(tm_data, hitter_name, "FB", "Right", "SLG")
    slg_rhp_bb <- create_metric_heatmap(tm_data, hitter_name, "BB", "Right", "SLG")
    slg_rhp_os <- create_metric_heatmap(tm_data, hitter_name, "OS", "Right", "SLG")
    
    grid::pushViewport(grid::viewport(x = 0.30, y = 0.55, width = hm_width, height = hm_height, just = c("center", "top")))
    grid::grid.text("FB SLG", y = 1.05, gp = grid::gpar(cex = 0.5))
    print(slg_rhp_fb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.42, y = 0.55, width = hm_width, height = hm_height, just = c("center", "top")))
    grid::grid.text("BB SLG", y = 1.05, gp = grid::gpar(cex = 0.5))
    print(slg_rhp_bb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.54, y = 0.55, width = hm_width, height = hm_height, just = c("center", "top")))
    grid::grid.text("OS SLG", y = 1.05, gp = grid::gpar(cex = 0.5))
    print(slg_rhp_os, newpage = FALSE)
    grid::popViewport()
    
    # Whiff heatmaps
    whiff_rhp_fb <- create_metric_heatmap(tm_data, hitter_name, "FB", "Right", "Whiff")
    whiff_rhp_bb <- create_metric_heatmap(tm_data, hitter_name, "BB", "Right", "Whiff")
    whiff_rhp_os <- create_metric_heatmap(tm_data, hitter_name, "OS", "Right", "Whiff")
    
    grid::pushViewport(grid::viewport(x = 0.30, y = 0.35, width = hm_width, height = hm_height, just = c("center", "top")))
    grid::grid.text("FB Whiff", y = 1.05, gp = grid::gpar(cex = 0.5))
    print(whiff_rhp_fb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.42, y = 0.35, width = hm_width, height = hm_height, just = c("center", "top")))
    grid::grid.text("BB Whiff", y = 1.05, gp = grid::gpar(cex = 0.5))
    print(whiff_rhp_bb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.54, y = 0.35, width = hm_width, height = hm_height, just = c("center", "top")))
    grid::grid.text("OS Whiff", y = 1.05, gp = grid::gpar(cex = 0.5))
    print(whiff_rhp_os, newpage = FALSE)
    grid::popViewport()
    
    # Movement plots (right section)
    grid::grid.text("Pitch Movement (red = whiff)", x = 0.80, y = 0.74, gp = grid::gpar(cex = 0.8, fontface = "bold"))
    
    mvmt_width <- 0.14
    mvmt_height <- 0.22
    
    # vs RHP movement
    mvmt_rhp_fb <- create_movement_plot(tm_data, hitter_name, "Right", "FB")
    mvmt_rhp_bb <- create_movement_plot(tm_data, hitter_name, "Right", "BB")
    mvmt_rhp_os <- create_movement_plot(tm_data, hitter_name, "Right", "OS")
    
    grid::pushViewport(grid::viewport(x = 0.70, y = 0.52, width = mvmt_width, height = mvmt_height, just = c("center", "top")))
    grid::grid.text("RHP FB", y = 1.03, gp = grid::gpar(cex = 0.5, col = "#FA8072"))
    print(mvmt_rhp_fb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.82, y = 0.52, width = mvmt_width, height = mvmt_height, just = c("center", "top")))
    grid::grid.text("RHP BB", y = 1.03, gp = grid::gpar(cex = 0.5, col = "#A020F0"))
    print(mvmt_rhp_bb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.94, y = 0.52, width = mvmt_width, height = mvmt_height, just = c("center", "top")))
    grid::grid.text("RHP OS", y = 1.03, gp = grid::gpar(cex = 0.5, col = "#228B22"))
    print(mvmt_rhp_os, newpage = FALSE)
    grid::popViewport()
    
    # vs LHP movement
    mvmt_lhp_fb <- create_movement_plot(tm_data, hitter_name, "Left", "FB")
    mvmt_lhp_bb <- create_movement_plot(tm_data, hitter_name, "Left", "BB")
    mvmt_lhp_os <- create_movement_plot(tm_data, hitter_name, "Left", "OS")
    
    grid::pushViewport(grid::viewport(x = 0.70, y = 0.28, width = mvmt_width, height = mvmt_height, just = c("center", "top")))
    grid::grid.text("LHP FB", y = 1.03, gp = grid::gpar(cex = 0.5, col = "#FA8072"))
    print(mvmt_lhp_fb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.82, y = 0.28, width = mvmt_width, height = mvmt_height, just = c("center", "top")))
    grid::grid.text("LHP BB", y = 1.03, gp = grid::gpar(cex = 0.5, col = "#A020F0"))
    print(mvmt_lhp_bb, newpage = FALSE)
    grid::popViewport()
    
    grid::pushViewport(grid::viewport(x = 0.94, y = 0.28, width = mvmt_width, height = mvmt_height, just = c("center", "top")))
    grid::grid.text("LHP OS", y = 1.03, gp = grid::gpar(cex = 0.5, col = "#228B22"))
    print(mvmt_lhp_os, newpage = FALSE)
    grid::popViewport()
    
    # Footer with date
    grid::grid.text(format(Sys.Date(), "%B %d, %Y"), x = 0.95, y = 0.02, just = "right",
                    gp = grid::gpar(cex = 0.6, col = "gray50"))
    
    invisible(output_file)
  }
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$pdf_title), "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Get selected hitters
      hitters <- input$scouting_hitters
      if (length(hitters) == 0) {
        showNotification("Please select at least one hitter", type = "error")
        return(NULL)
      }
      
      # Check if data is available
      if (is.null(tm_data)) {
        showNotification("No TrackMan data loaded", type = "error")
        return(NULL)
      }
      
      # Get title and logo
      report_title <- input$pdf_title
      report_subtitle <- input$pdf_subtitle
      
      # Load logo if provided
      logo_path <- NULL
      if (!is.null(input$pdf_logo)) {
        logo_path <- input$pdf_logo$datapath
      }
      
      if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
      
      # Create PDF - landscape 11x17
      pdf(file, width = 17, height = 11, paper = "USr")
      
      # Title page
      grid::grid.newpage()
      
      # Add logo if provided
      if (!is.null(logo_path)) {
        tryCatch({
          logo <- png::readPNG(logo_path)
          grid::grid.raster(logo, y = 0.75, height = 0.15)
        }, error = function(e) {
          tryCatch({
            logo <- jpeg::readJPEG(logo_path)
            grid::grid.raster(logo, y = 0.75, height = 0.15)
          }, error = function(e) NULL)
        })
      }
      
      grid::grid.text(report_title, y = 0.55, gp = grid::gpar(fontsize = 42, fontface = "bold", col = "#006F71"))
      if (report_subtitle != "") {
        grid::grid.text(report_subtitle, y = 0.45, gp = grid::gpar(fontsize = 28))
      }
      grid::grid.text(format(Sys.Date(), "%B %d, %Y"), y = 0.35, gp = grid::gpar(fontsize = 20, col = "gray50"))
      grid::grid.text(paste0(length(hitters), " Hitters"), y = 0.28, gp = grid::gpar(fontsize = 16, col = "gray60"))
      
      # Generate page for each hitter
      for (hitter in hitters) {
        profile <- get_scouting_profile(hitter)
        if (is.null(profile)) next
        
        grid::grid.newpage()
        
        # Header
        grid::grid.rect(x = 0.5, y = 0.97, width = 1, height = 0.06, just = c("center", "top"),
                        gp = grid::gpar(fill = "#006F71", col = NA))
        grid::grid.text(paste0(hitter, " (", profile$batter_side, ")"),
                        x = 0.5, y = 0.945,
                        gp = grid::gpar(fontsize = 20, fontface = "bold", col = "white"))
        
        # Stats row
        rs <- profile$raw_stats
        stats_text <- paste0(
          "n=", profile$n, "  |  RV/100: ", sprintf("%+.2f", profile$rv100),
          "  |  EV: ", round(rs$ev, 1), "  |  Whiff%: ", round(rs$whiff, 1),
          "  |  K%: ", round(rs$k_pct, 1), "  |  BB%: ", round(rs$bb_pct, 1)
        )
        grid::grid.text(stats_text, x = 0.5, y = 0.885,
                        gp = grid::gpar(fontsize = 12, col = "gray30"))
        
        # Punish/Struggle
        grid::grid.text(paste0("PUNISH: ", profile$punishes),
                        x = 0.03, y = 0.84, just = "left",
                        gp = grid::gpar(fontsize = 11, col = "#1A9850", fontface = "bold"))
        grid::grid.text(paste0("STRUGGLE: ", profile$struggles),
                        x = 0.03, y = 0.80, just = "left",
                        gp = grid::gpar(fontsize = 11, col = "#D73027", fontface = "bold"))
        
        # Spray charts
        spray_rhp <- create_mini_spray(tm_data, hitter, "hand", "Right")
        spray_lhp <- create_mini_spray(tm_data, hitter, "hand", "Left")
        
        grid::pushViewport(grid::viewport(x = 0.10, y = 0.58, width = 0.16, height = 0.22, just = c("center", "top")))
        grid::grid.text("vs RHP Spray", y = 1.05, gp = grid::gpar(cex = 0.7, fontface = "bold"))
        print(spray_rhp, newpage = FALSE)
        grid::popViewport()
        
        grid::pushViewport(grid::viewport(x = 0.10, y = 0.34, width = 0.16, height = 0.22, just = c("center", "top")))
        grid::grid.text("vs LHP Spray", y = 1.05, gp = grid::gpar(cex = 0.7, fontface = "bold"))
        print(spray_lhp, newpage = FALSE)
        grid::popViewport()
        
        # Heatmaps section
        grid::grid.text("vs RHP", x = 0.38, y = 0.76, gp = grid::gpar(cex = 0.9, fontface = "bold"))
        grid::grid.text("vs LHP", x = 0.58, y = 0.76, gp = grid::gpar(cex = 0.9, fontface = "bold"))
        
        hm_w <- 0.10
        hm_h <- 0.16
        
        # RHP SLG heatmaps
        for (j in 1:3) {
          pf <- c("FB", "BB", "OS")[j]
          hm <- create_metric_heatmap(tm_data, hitter, pf, "Right", "SLG")
          grid::pushViewport(grid::viewport(x = 0.28 + (j-1)*0.10, y = 0.58, width = hm_w, height = hm_h, just = c("center", "top")))
          grid::grid.text(paste0(pf, " SLG"), y = 1.06, gp = grid::gpar(cex = 0.5))
          print(hm, newpage = FALSE)
          grid::popViewport()
        }
        
        # RHP Whiff heatmaps
        for (j in 1:3) {
          pf <- c("FB", "BB", "OS")[j]
          hm <- create_metric_heatmap(tm_data, hitter, pf, "Right", "Whiff")
          grid::pushViewport(grid::viewport(x = 0.28 + (j-1)*0.10, y = 0.40, width = hm_w, height = hm_h, just = c("center", "top")))
          grid::grid.text(paste0(pf, " Whiff"), y = 1.06, gp = grid::gpar(cex = 0.5))
          print(hm, newpage = FALSE)
          grid::popViewport()
        }
        
        # LHP SLG heatmaps
        for (j in 1:3) {
          pf <- c("FB", "BB", "OS")[j]
          hm <- create_metric_heatmap(tm_data, hitter, pf, "Left", "SLG")
          grid::pushViewport(grid::viewport(x = 0.48 + (j-1)*0.10, y = 0.58, width = hm_w, height = hm_h, just = c("center", "top")))
          grid::grid.text(paste0(pf, " SLG"), y = 1.06, gp = grid::gpar(cex = 0.5))
          print(hm, newpage = FALSE)
          grid::popViewport()
        }
        
        # LHP Whiff heatmaps
        for (j in 1:3) {
          pf <- c("FB", "BB", "OS")[j]
          hm <- create_metric_heatmap(tm_data, hitter, pf, "Left", "Whiff")
          grid::pushViewport(grid::viewport(x = 0.48 + (j-1)*0.10, y = 0.40, width = hm_w, height = hm_h, just = c("center", "top")))
          grid::grid.text(paste0(pf, " Whiff"), y = 1.06, gp = grid::gpar(cex = 0.5))
          print(hm, newpage = FALSE)
          grid::popViewport()
        }
        
        # Movement plots
        grid::grid.text("Pitch Movement (red=whiff, green=95+EV)", x = 0.85, y = 0.76, gp = grid::gpar(cex = 0.9, fontface = "bold"))
        
        mvmt_w <- 0.12
        mvmt_h <- 0.18
        
        # vs RHP movement
        for (j in 1:3) {
          pf <- c("FB", "BB", "OS")[j]
          col <- c("#FA8072", "#A020F0", "#228B22")[j]
          mvmt <- create_movement_plot(tm_data, hitter, "Right", pf)
          grid::pushViewport(grid::viewport(x = 0.75 + (j-1)*0.10, y = 0.58, width = mvmt_w, height = mvmt_h, just = c("center", "top")))
          grid::grid.text(paste0("R-", pf), y = 1.06, gp = grid::gpar(cex = 0.5, col = col, fontface = "bold"))
          print(mvmt, newpage = FALSE)
          grid::popViewport()
        }
        
        # vs LHP movement
        for (j in 1:3) {
          pf <- c("FB", "BB", "OS")[j]
          col <- c("#FA8072", "#A020F0", "#228B22")[j]
          mvmt <- create_movement_plot(tm_data, hitter, "Left", pf)
          grid::pushViewport(grid::viewport(x = 0.75 + (j-1)*0.10, y = 0.38, width = mvmt_w, height = mvmt_h, just = c("center", "top")))
          grid::grid.text(paste0("L-", pf), y = 1.06, gp = grid::gpar(cex = 0.5, col = col, fontface = "bold"))
          print(mvmt, newpage = FALSE)
          grid::popViewport()
        }
        
        # Footer
        grid::grid.text(format(Sys.Date(), "%B %d, %Y"), x = 0.97, y = 0.02, just = "right",
                        gp = grid::gpar(cex = 0.6, col = "gray50"))
      }
      
      dev.off()
      
      showNotification("PDF generated successfully!", type = "message")
    }
  )
}

# ============================================================
# 8. RUN APP
# ============================================================

shinyApp(ui, server)