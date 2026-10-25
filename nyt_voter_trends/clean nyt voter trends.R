# NYT "Voter trends" estimates of the 2020 and 2024 presidential vote by group
#
# source: https://www.nytimes.com/interactive/2024/elections/voter-trends-national.html
# (plus its per-state pages), downloaded by hand as six CSVs into raw/. The
# three COMPLETE files hold everything; the three states_2024 files are an
# earlier pull of the same 2024 state numbers and are kept only for the record
# (see the note on shift_by_race below).
#
# outputs, all long, one row per year x state x group:
#
#   nyt_vote_by_group.csv
#     year, state, state_abb ("US" = national), category, group
#     share_voters    group's % of that state's voters
#     dem_pct / rep_pct   Biden (2020) or Harris (2024) / Trump % within group
#     margin          Dem minus Rep, points (NYT's, from unrounded numbers, so
#                     it can be 1 off dem_pct - rep_pct)
#     shift_from_2020 change in margin since 2020, points (2024 rows only;
#                     negative = toward Trump)
#
#   nyt_vote_by_group_and_race.csv  (the page's group x race grids)
#     year, state, state_abb, row_category, row_group ("All" = everyone),
#     race ("All" = all races), margin, shift_from_2020
#
#   nyt_electorate_vs_eligible.csv
#     year, state, state_abb, category, group
#     share_voters    group's % of voters
#     share_eligible  group's % of the voting-eligible population
#     overrep         share_voters - share_eligible (+ = turns out above its size)
#
# NA in margin / shift means NYT printed "No estimate" (group too small to
# estimate) or left the cell blank. NYT's "Even" and "No change" are coded 0.
#
# cleaning done here:
#   - labels like "D+4" / "R+13" parsed to signed numbers (D positive)
#   - "College grad." and "College degree" (same group, named differently on
#     different parts of the page) both become "College grad"
#   - curly apostrophe in "Didn’t vote" straightened
#   - "U.S. (national)" becomes "National"
#   - candidate-name columns dropped (implied by year), as are the subgroup
#     file's col_share_of_voters_pct (the race's share of ALL voters, repeated
#     on every row, not its share within the row group) and row shares
#     (already in nyt_vote_by_group.csv)
#
# quirks worth knowing:
#   - small-population states (IA, ME, MN, NH, ND, VT, WV, WY) are split only
#     White / Nonwhite; elsewhere race is White, Black, Hispanic, Asian, Other.
#     "Nonwhite" is a real group in those states, not a sum we made.
#   - the group x race shift grid also collapses AK, HI, MT, SD to White /
#     Nonwhite even though their margin grids show all five races, so for
#     those four states the 2024 shift sits on a Nonwhite row with no margin.
#   - raw/nyt_voter_trends_states_2024_shift_by_race.csv labels that Nonwhite
#     column "Black" in all 12 of those states. It's mislabeled: its values
#     match the COMPLETE file's Nonwhite column and the overview's Nonwhite
#     shift exactly. Don't use that file.
#   - "Region by race" is national only. "Voted in the last presidential /
#     midterm election" has no 2020-24 shift (NYT gives none).
#   - shares within a category sum to 99-102 from rounding.

library(tidyverse)
library(magrittr)

proj_dir <- "C:/Dropbox/OSU teaching/ps 2150/Fall 2026 (ps 2150)"
nyt_dir <- str_c(proj_dir, "/new data/nyt voter trends")
raw_dir <- str_c(nyt_dir, "/raw")

read_raw <- \(f)
  str_c(raw_dir, "/", f) %>%
    read_csv(
        col_types = cols(.default = "c")
    )

# "D+4" -> 4, "R+13" -> -13, "Even" / "No change" -> 0, "No estimate" -> NA
parse_margin <- \(lab)
  case_when(
      lab %in% c("Even", "No change") ~ 0,
      lab %>% str_detect("^D\\+\\d+$") ~ 1,
      lab %>% str_detect("^R\\+\\d+$") ~ -1
  ) %>%
    multiply_by(
        lab %>% str_extract("\\d+") %>% as.numeric %>% coalesce(0)
    )

clean_labels <- \(x)
  x %>%
    str_replace("^College (grad\\.|degree)$", "College grad") %>%
    str_replace("’", "'") %>%
    str_replace("^U\\.S\\. \\(national\\)$", "National")

state_abbs <- tibble(
    state = c(state.name, "Washington, D.C.", "National"),
    state_abb = c(state.abb, "DC", "US")
    )

# ---- vote by group ----

vote_by_group <- "nyt_voter_trends_COMPLETE_2020_2024_overview.csv" %>%
  read_raw %>%
  mutate(
      across(
          c(state, group),
          clean_labels
      ),
      year = year %>% as.integer,
      share_voters = share_of_voters_pct %>% as.numeric,
      dem_pct = dem_candidate_pct %>% as.numeric,
      rep_pct = rep_candidate_pct %>% as.numeric,
      margin = margin_label %>% parse_margin,
      shift_from_2020 = shift_from_2020_label %>% parse_margin
  ) %>%
  left_join(state_abbs, by = "state") %>%
  select(year, state, state_abb, category, group, share_voters, dem_pct, rep_pct, margin, shift_from_2020)

# ---- vote by group x race ----

grid <- "nyt_voter_trends_COMPLETE_2020_2024_subgroups.csv" %>%
  read_raw %>%
  mutate(
      across(
          c(state, row_group, col_group),
          clean_labels
      ),
      year = year %>% as.integer,
      value = value_label %>% parse_margin
  ) %>%
  select(year, state, measure, row_category, row_group, race = col_group, value)

vote_by_group_and_race <- grid %>%
  filter(
      measure %>% str_detect("^margin")
  ) %>%
  select(-measure) %>%
  rename(margin = value) %>%
  full_join(
      grid %>%
        filter(measure == "shift_2020_to_2024") %>%
        select(-measure) %>%
        rename(shift_from_2020 = value),
      by = c("year", "state", "row_category", "row_group", "race")
  ) %>%
  left_join(state_abbs, by = "state") %>%
  select(year, state, state_abb, row_category, row_group, race, margin, shift_from_2020)

# ---- electorate vs eligible population ----

electorate <- "nyt_voter_trends_COMPLETE_2020_2024_electorate.csv" %>%
  read_raw %>%
  mutate(
      across(
          c(state, group),
          clean_labels
      ),
      year = year %>% as.integer,
      share_voters = share_of_voters_pct %>% as.numeric,
      share_eligible = share_of_voting_eligible_pop_pct %>% as.numeric,
      overrep = share_voters %>% subtract(share_eligible)
  ) %>%
  left_join(state_abbs, by = "state") %>%
  select(year, state, state_abb, category, group, share_voters, share_eligible, overrep)

# ---- checks ----

stopifnot(
    vote_by_group$state_abb %>% is.na %>% not %>% all,
    vote_by_group %>% count(year, state, category, group) %>% pull(n) %>% equals(1) %>% all,
    vote_by_group_and_race %>% count(year, state, row_group, race) %>% pull(n) %>% equals(1) %>% all,
    electorate %>% count(year, state, category, group) %>% pull(n) %>% equals(1) %>% all,
    # NYT's margin is within a point of the rounded shares
    vote_by_group %>% filter(!is.na(margin)) %$% abs(dem_pct - rep_pct - margin) %>% max %>% is_weakly_less_than(1)
)

# ---- write ----

vote_by_group %>%
  write_csv(str_c(nyt_dir, "/nyt_vote_by_group.csv"), na = "")

vote_by_group_and_race %>%
  write_csv(str_c(nyt_dir, "/nyt_vote_by_group_and_race.csv"), na = "")

electorate %>%
  write_csv(str_c(nyt_dir, "/nyt_electorate_vs_eligible.csv"), na = "")
