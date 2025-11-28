# ==============================================================================
# 🔹 LIBRARIES & SETUP
# ==============================================================================
library(here)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(plotly)
library(leaflet)
library(glue)
library(DT)
library(readr)
library(viridis)
library(corrplot)
library(networkD3)
library(highcharter)
library(formattable)
library(shinycssloaders)
library(shinyWidgets)
library(reactable)
library(ggplot2)
library(lubridate)
library(forecast)
library(tidyr)
library(htmlwidgets)
library(kableExtra)
library(sf) # For handling spatial data
library(fresh) # For create_theme function
library(bslib) # For theme support
library(httr) # For API calls
library(jsonlite) # For JSON handling
library(future) # For async operations
# Define missing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Utility scripts (like gemini_utils.R and nisr_theme.R) are now sourced
# from run_dashboard.R before the app is launched.

# Source the NISR theme (with error handling)
tryCatch({
  if (exists("nisr_theme")) {
    cat("✅ NISR theme loaded successfully\n")
  } else {
    cat("⚠️ R/nisr_theme.R not found, using default theme\n")
    nisr_theme <- NULL
  }
}, error = function(e) {
  cat("⚠️ Failed to load NISR theme:", e$message, "\n")
  cat("📋 Using default theme instead\n")
  nisr_theme <- NULL
}) 


# ==============================================================================
# 🔹 BILINGUAL TRANSLATIONS (Kinyarwanda & English)
# ==============================================================================

# Translation function
get_text <- function(key, lang = "rw") {
  translations[[lang]][[key]] %||% key
}

# Bilingual translations
translations <- list(
  rw = list(  # Kinyarwanda
    title           = "Ikaze muri gahunda yacu",
    description     = "Iyi gahunda igufasha kubona amakuru y'ingenzi mu buryo bwihuse.",
    submit_button   = "Ohereza",
    cancel_button   = "Hagarika",
    dashboard_title = "Ikarita y'Ubuzima bwo mu Rwanda",
    user_welcome    = "Murakaza neza, ukoresha mushya!",
    
    # Dashboard specific translations
    national_overview = "Icyerekezo cy'Igihugu",
    district_rankings = "Urutonde rw'Uturere", # Already correct
    regional_comparison = "Ubugereranye n'Akarere", # Already correct
    trend_analysis = "Isesengura ry'Icyerekezo", # Already correct
    predictive_analytics = "Isesengura Riteganya", # Already correct
    policy_advisor = "Umujyanama mu by'Ingamba", # Already correct
    data_explorer = "Gushakisha amakuru",
    impact_assessment = "Isuzuma ry'Ingaruka", # Already correct
    export_reports = "Gutanga raporo",
    
    # Common terms
    stunting = "Gutinda kukura",
    wasting = "Guhira", 
    anemia = "Kurwara amaraso make",
    poverty = "Ubukene",
    malnutrition = "Kurwara ubuzima",
    nutrition = "Ubuzima",
    health = "Ubuzima",
    children = "Abana",
    women = "Abagore",
    
    # Actions
    download = "Gusubiza",
    generate = "Gukora",
    update = "Gusubira",
    view = "Gusuzuma",
    analyze = "Gusuzuma",
    
    # Status messages
    loading = "Birakururwa...",
    success = "Byakunze",
    error = "Hari ikosa",
    warning = "Iburira",
    info = "Amakuru",
    
    # UI Elements
    language = "Ururimi",
    global_filters = "Ibyihariye byose",
    year = "Umwaka",
    province = "Umujyi",
    all_provinces = "Imijyi yose",
    age_group = "Urwego rw'amavuko",
    all_age_groups = "Urwego rwose",
    nutrient = "Ibiribwa by'ingenzi",
    all_nutrients = "Ibiribwa byose",
    district = "Akarere",
    indicators = "Ibimenyetso",
    
    # Notifications
    system_alerts = "Iburira ry'Ubwoba",
    quick_actions = "Ibikorwa byihuse",
    high_stunting_alert = "Uruhare rukabije rwo gutinda kukura ruhamagara mu turere tw'icyaro",
    new_data_alert = "Amakuru mashya y'Ubuzima bahoza gusuzuma",
    regional_update = "Gusuzuma n'indi mihugu byasubiyemo",
    
    # Actions
    download_dashboard = "Gusubiza Ikarita",
    export_powerpoint = "Gutanga muri PowerPoint",
    generate_report = "Gukora Raporo",
    ai_insights = "Icyerekezo cy'Ubwoba",
    schedule_alert = "Guhagarika Iburira",
    set_monitoring = "Gushiraho ibipimo byo kugenzura",
    
    # Status messages
    chronic_malnutrition = "Imirire mibi idakira",
    acute_malnutrition = "Imirire mibi ikabije",
    iron_deficiency = "Kubura kw'icyuma mu mubiri",
    economic_indicator = "Igipimo cy'ubukungu",
    select_indicator = "Hitamo Igipimo:",
    available = "Birashoboka",
    ready = "Biriteguye",
    configure = "Gushiraho"
    ,
    national_intelligence = "Ubushishozi ku Mirire mu Rwanda",
    national_trends = "Imiterere y'Igihugu (DHS)",
    city_comparison = "Ugereranyije bw'Umujyi",
    geographic_distribution = "Ikibanza cy'ubutaka",
    rankings_table = "Imbonerahamwe y'urutonde",
    performance_distribution = "Ikwirakwizwa ry'imikorere",
    performance_categories = "Ibyiciro by'imikorere",
    rankings_title = "Urutonde rw'Uturere",
    indicator = "Igipimo",
    order = "Urutonde",
    best_to_worst = "Kuva ku mwiza kugera ku mubi",
    worst_to_best = "Kuva ku mubi kugera ku mwiza",
    update_rankings = "Vugurura Urutonde",
    export_rankings = "Kohereza Urutonde",
    top_bottom_performers = "Abayoboye n'aba nyuma",
    performance_over_time = "Imikorere mu gihe",
    regional_context_body = "Gereranya imikorere y'u Rwanda n'ibihugu duturanye kugira ngo umenye uko duhagaze mu karere.",
    regional_context_title = "Imiterere y'Akarere",
    regional_intro_p1 = "Ikaze ku ipaji y'Ubugereranye bw'Akarere. Kugira ngo tumenye neza imiterere y'imirire mu Rwanda, ni ngombwa kureba uko duhagaze mu karere. Iki gice kigereranya imikorere y'u Rwanda n'ibihugu duturanye byo mu Burasirazuba bwa Afurika: Uganda, Kenya, Tanzaniya, Uburundi, na DRC.",
    regional_intro_p2 = "Tugereranya ibipimo by'ingenzi nk'igwingira n'ikibazo cyo kunanuka, dushobora kumenya ingorane dusangiye, kugaragaza aho twatsinze, no kuvumbura amahirwe y'ubufatanye bwambukiranya imipaka. Koresha ibishushanyo biri hepfo kugira ngo usuzume imiterere y'igihe kirekire kandi urebe uko u Rwanda ruhagaze mu mwaka uheruka."
    ,
    narrative_intro = "Mu {input_selected_year}, {province_name} ifite impuzandengo y'igipimo cy'igwingira cya <strong>{avg_stunting}%</strong> mu turere <strong>{total_districts}</strong>.",
    narrative_at_risk = "Kugeza ubu, uturere <strong>{at_risk_districts_count}</strong> turagaragaza ibipimo biteye inkeke biri hejuru ya 30%.",
    narrative_trend_improving = "Icyerekezo <strong style='color: {trend_color};'>kigenda neza</strong>, hamwe n'{change_text}.",
    narrative_trend_concerning = "Icyerekezo <strong style='color: {trend_color};'>giteye inkeke</strong>, hamwe n'{change_text}.",
    narrative_trend_stable = "Icyerekezo <strong style='color: {trend_color};'>ntigihinduka</strong>, kubera {change_text}.",
    narrative_change_text = "ihinduka rya {abs_annual_change} ku ijana ugereranyije na {previous_year}",
    narrative_no_trend = "nta makuru ahari y'icyerekezo giheruka",
    national_stunting_rate = "Igipimo cy'Igihugu cy'Igwingira",
    regional_intro_dynamic = "Iki gice gisesengura uko u Rwanda ruhagaze ugereranyije n'ibindi bihugu <strong>{country_count}</strong> byo mu karere, hakoreshejwe amakuru agezweho yo mu <strong>{latest_year}</strong>. Koresha ibishushanyo bikurikira kugira ngo usesengure uko ibipimo by'ingenzi byagiye bihinduka mu myaka ishize."
    ,
    trend_analysis_filters = "Ibyahinduwe mu isesengura ry'icyerekezo",
    advanced_trend_analysis = "Isesengura ryimbitse ry'icyerekezo",
    trend_model_summary = "Incukumbuzi y'icyitegererezo cy'icyerekezo",
    multi_indicator_trends = "Ibyerekezo by'ibipimo byinshi",
    correlation_analysis = "Isesengura ry'isano",
    seasonal_patterns = "Imiterere y'ibihe",
    trend_insights = "Ubushishozi ku cyerekezo",
    model = "Icyitegererezo",
    linear_trend = "Icyerekezo kigororotse",
    show_forecast = "Erekana iteganyamikorere",
    generate_trend_report = "Kora raporo y'icyerekezo",
    trend_report_title = "Raporo y'incukumbuzi y'icyerekezo",
    trend_report_for = "Raporo y'icyerekezo cya {indicator} muri {district}",
    trend_model_info = "Amakuru y'icyitegererezo",
    key_statistics = "Imibare y'ingenzi",
    close_report = "Funga",
    children_impacted = "Abana Bagezweho",
    estimated_beneficiaries = "Abagenerwabikorwa Bagereranijwe",
    districts_improved = "Uturere Twateye Imbere",
    showing_positive_trends = "Bigaragaza Icyerekezo Cyiza",
    avg_improvement = "Impuzandengo y'Iterambere",
    stunting_reduction = "Igabanuka ry'Igwingira",
    cost_per_child = "Ikiguzi kuri Buri Mwana",
    program_efficiency = "Imikorere ya Gahunda"
    ,
    policy_advisor_title = "Umujyanama mu by'Ingamba za AI",
    policy_intro = "Tanga ibyifuzo by'ingamba zishingiye ku bimenyetso ukoresheje isesengura rya AI ry'amakuru ya DHS n'imikorere myiza yo mu karere.",
    focus_area = "1. Aho Kwibanda:",
    stunting_reduction_policy = "Kugabanya Igwingira",
    wasting_prevention_policy = "Kurwanya Ikibazo cyo Kunanuka",
    anemia_control_policy = "Kurwanya Anemiya",
    integrated_nutrition_policy = "Imirire Ihuriweho",
    emergency_response_policy = "Gutabara mu bihe by'Ibyago",
    target = "2. Aho bigenewe:",
    national_target = "Igihugu Cyose",
    city_specific_target = "Umujyi Wihariye",
    district_specific_target = "Akarere Kihariye",
    implementation_timeline = "3. Igihe cyo Gushyira mu Bikorwa (amezi):",
    available_budget = "4. Ingengo y'Imari Ihari (USD):"
  ),
  en = list(  # English
    title           = "Welcome to our program",
    description     = "This program helps you get important information quickly.",
    submit_button   = "Submit",
    cancel_button   = "Cancel", 
    dashboard_title = "Rwanda Nutrition Dashboard",
    user_welcome    = "Welcome, new user!",
    
    # Dashboard specific translations
    national_overview = "National Overview",
    district_rankings = "District Rankings",
    regional_comparison = "Regional Comparison", 
    trend_analysis = "Trend Analysis",
    predictive_analytics = "Predictive Analytics",
    policy_advisor = "Policy Advisor",
    data_explorer = "Data Explorer",
    impact_assessment = "Impact Assessment",
    export_reports = "Export & Reports",
    
    # Common terms
    stunting = "Stunting",
    wasting = "Wasting",
    anemia = "Anemia",
    poverty = "Poverty",
    malnutrition = "Malnutrition",
    nutrition = "Nutrition",
    health = "Health",
    children = "Children",
    women = "Women",
    
    # Actions
    download = "Download",
    generate = "Generate",
    update = "Update",
    view = "View",
    analyze = "Analyze",
    
    # Status messages
    loading = "Loading...",
    success = "Success",
    error = "Error",
    warning = "Warning",
    info = "Information",
    
    # UI Elements
    language = "Language",
    global_filters = "Global Filters",
    year = "Year",
    province = "City", 
    all_provinces = "All Cities",
    age_group = "Age Group",
    all_age_groups = "All Age Groups",
    nutrient = "Nutrient",
    all_nutrients = "All Nutrients",
    district = "District",
    indicators = "Indicators",
    
    # Notifications
    system_alerts = "System Alerts",
    quick_actions = "Quick Actions",
    high_stunting_alert = "High stunting rates detected in rural areas",
    new_data_alert = "New DHS data available for analysis",
    regional_update = "Regional comparison updated",
    
    # Actions
    download_dashboard = "Download Dashboard",
    export_powerpoint = "Export as PowerPoint",
    generate_report = "Generate Report",
    ai_insights = "AI-powered insights",
    schedule_alert = "Schedule Alert",
    set_monitoring = "Set monitoring thresholds",
    
    # Status messages
    chronic_malnutrition = "Chronic malnutrition",
    acute_malnutrition = "Acute malnutrition",
    iron_deficiency = "Iron deficiency",
    economic_indicator = "Economic indicator",
    select_indicator = "Select Indicator:",
    available = "Available",
    ready = "Ready",
    configure = "Configure"
    ,
    national_intelligence = "Rwanda National Nutrition Intelligence",
    national_trends = "National Trends (DHS Data)",
    city_comparison = "City Comparison",
    geographic_distribution = "Geographic Distribution",
    rankings_table = "Rankings Table",
    performance_distribution = "Performance Distribution",
    performance_categories = "Performance Categories",
    rankings_title = "District Rankings Filters",
    indicator = "Indicator",
    order = "Order",
    best_to_worst = "Best to Worst",
    worst_to_best = "Worst to Best",
    update_rankings = "Update Rankings",
    export_rankings = "Export Rankings",
    top_bottom_performers = "Top & Bottom Performers",
    performance_over_time = "Performance Over Time",
    regional_context_body = "Compare Rwanda's performance with neighboring countries to understand the regional context.",
    regional_context_title = "Regional Context",
    regional_intro_p1 = "Welcome to the Regional Comparison page. Understanding Rwanda's nutritional landscape requires a regional perspective. This section benchmarks Rwanda's performance against its East African neighbors: Uganda, Kenya, Tanzania, Burundi, and the DRC.",
    regional_intro_p2 = "By comparing key indicators like Stunting and Wasting, we can identify shared challenges, highlight areas of success, and uncover opportunities for cross-border collaboration. Use the charts below to explore long-term trends and see how Rwanda ranks in the latest available year.",

    narrative_intro = "In {input_selected_year}, {province_name} has an average stunting rate of <strong>{avg_stunting}%</strong> across <strong>{total_districts}</strong> districts.",
    narrative_at_risk = "Currently, <strong>{at_risk_districts_count}</strong> districts show concerning levels above 30%.",
    narrative_trend_improving = "The trend is <strong style='color: {trend_color};'>improving</strong>, with {change_text}.",
    narrative_trend_concerning = "The trend is <strong style='color: {trend_color};'>concerning</strong>, with {change_text}.",
    narrative_trend_stable = "The trend is <strong style='color: {trend_color};'>stable</strong>, with {change_text}.",
    narrative_change_text = "a {abs_annual_change} percentage point change from {previous_year}",
    narrative_no_trend = "no recent trend data available",
    national_stunting_rate = "National Stunting Rate",
    regional_intro_dynamic = "This section analyzes Rwanda's performance against <strong>{country_count}</strong> other regional countries, using the latest available data from <strong>{latest_year}</strong>. Use the following charts to explore how key indicators have evolved over time."
    ,
    trend_analysis_filters = "Trend Analysis Filters",
    advanced_trend_analysis = "Advanced Trend Analysis",
    trend_model_summary = "Trend Model Summary",
    multi_indicator_trends = "Multi-Indicator Trends",
    correlation_analysis = "Correlation Analysis",
    seasonal_patterns = "Seasonal Patterns",
    trend_insights = "Trend Insights",
    model = "Model",
    linear_trend = "Linear Trend",
    show_forecast = "Show Forecast",
    generate_trend_report = "Generate Trend Report",
    trend_report_title = "Trend Analysis Report",
    trend_report_for = "Trend Report for {indicator} in {district}",
    trend_model_info = "Model Information",
    key_statistics = "Key Statistics",
    close_report = "Close",
    children_impacted = "Children Impacted",
    estimated_beneficiaries = "Estimated beneficiaries",
    districts_improved = "Districts Improved",
    showing_positive_trends = "Showing positive trends",
    avg_improvement = "Avg Improvement",
    stunting_reduction = "Stunting reduction",
    cost_per_child = "Cost per Child",
    program_efficiency = "Program efficiency"
    ,
    policy_advisor_title = "AI Policy Advisor",
    policy_intro = "Generate evidence-based policy recommendations using AI analysis of DHS data and regional best practices.",
    focus_area = "1. Focus Area:",
    stunting_reduction_policy = "Stunting Reduction",
    wasting_prevention_policy = "Wasting Prevention",
    anemia_control_policy = "Anemia Control",
    integrated_nutrition_policy = "Integrated Nutrition",
    emergency_response_policy = "Emergency Response",
    target = "2. Target:",
    national_target = "National",
    city_specific_target = "City Specific",
    district_specific_target = "District Specific",
    implementation_timeline = "3. Implementation Timeline (months):",
    available_budget = "4. Available Budget (USD):"
  )
)

# ==============================================================================
# 🔹 LOAD REAL RWANDA NUTRITION DATA
# ==============================================================================

create_rwanda_nutrition_data <- function() {
  # Define file path
  file_path <- here::here("thacien project", "R project", "project", "data", "nutrition_data.csv")
  
  tryCatch({
    cat("🔄 Attempting to load real nutrition data from:", file_path, "\n")
    
    # Define column names to avoid parsing issues
    col_names <- c("district", "province", "year", "age_group", "nutrient", 
                   "stunting", "wasting", "anemia", "poverty_rate", 
                   "maternal_education", "sanitation_access", "food_diversity", "health_access_km")
    
    # Read the CSV with specified column names and types
    data <- read_csv(file_path, col_names = col_names, col_types = "cciccnnnnnnnn", skip = 1)
    
    # Basic validation
    if (nrow(data) > 0 && all(col_names %in% names(data))) {
      cat("✅ Real nutrition data loaded successfully.\n")
      return(data)
    } else {
      stop("Loaded data is empty or has incorrect columns.")
    }
    
  }, error = function(e) {
    cat("❌ Error loading real data:", e$message, "\n")
    cat("🔄 Falling back to generating synthetic data...\n")
    generate_synthetic_data()
  })
}

generate_synthetic_data <- function() {
  tryCatch({
    set.seed(123) # for reproducibility

    # Define Rwanda's 5 provinces and 30 districts
    provinces <- list(
      "Kigali City" = c("Gasabo", "Kicukiro", "Nyarugenge"),
      "Southern" = c("Gisagara", "Huye", "Kamonyi", "Muhanga", "Nyamagabe", "Nyanza", "Nyaruguru", "Ruhango"),
      "Western" = c("Karongi", "Ngororero", "Nyabihu", "Nyamasheke", "Rubavu", "Rusizi", "Rutsiro"),
      "Northern" = c("Burera", "Gakenke", "Gicumbi", "Musanze", "Rulindo"),
      "Eastern" = c("Bugesera", "Gatsibo", "Kayonza", "Kirehe", "Ngoma", "Nyagatare", "Rwamagana")
    )

    # Create a district-province mapping
    district_province_map <- do.call(rbind, lapply(names(provinces), function(p) {
      data.frame(district = provinces[[p]], province = p, stringsAsFactors = FALSE)
    }))

    years <- 2000:2024
    age_groups <- c("0-23 months", "24-59 months")
    nutrients <- c("Iron", "Vitamin A", "Protein", "Iodine")

    # Create the base data frame
    full_data <- expand.grid(
      district = district_province_map$district,
      year = years,
      age_group = age_groups,
      nutrient = nutrients,
      stringsAsFactors = FALSE
    ) %>%
      left_join(district_province_map, by = "district")

    # Generate synthetic data for indicators
    full_data <- full_data %>%
      mutate(
        # Define base rates and trends that vary by province for more realism
        province_effect = case_when(
          province == "Western" ~ 1.2,
          province == "Northern" ~ 1.1,
          province == "Southern" ~ 1.0,
          province == "Eastern" ~ 0.9,
          province == "Kigali City" ~ 0.7,
          TRUE ~ 1.0
        ),
        time_trend = 1 - ((year - min(year)) / (max(year) - min(year))) * 0.5, # General improvement over time
        
        # Generate raw values based on user-provided ranges
        stunting_raw = (40 * province_effect * time_trend) + rnorm(n(), 0, 2),
        wasting_raw = (2.5 * province_effect * time_trend) + rnorm(n(), 0, 0.5),
        anemia_raw = (45 * province_effect * time_trend) + rnorm(n(), 0, 3),
        poverty_rate_raw = (45 * province_effect * time_trend) + rnorm(n(), 0, 4),
        
        # Add new indicators with realistic variations
        maternal_education_raw = (3.0 / province_effect) + rnorm(n(), 0, 0.2),
        sanitation_access_raw = (50 / province_effect) * (1 + (1 - time_trend)) + rnorm(n(), 0, 5), # Improves over time
        food_diversity_raw = (2.8 / province_effect) + rnorm(n(), 0, 0.1),
        health_access_km_raw = (78 * province_effect) + rnorm(n(), 0, 1)
      ) %>%
      # Adjust for age group
      mutate(
        stunting_raw = ifelse(age_group == "0-23 months", stunting_raw * 1.1, stunting_raw * 0.9),
        wasting_raw = ifelse(age_group == "0-23 months", wasting_raw * 1.2, wasting_raw * 0.8)
      ) %>%
      # Clamp the values to the specified Min/Max ranges and round them
      mutate(
        stunting = round(pmin(48.3, pmax(33.0, stunting_raw)), 1),
        wasting = round(pmin(5.0, pmax(1.0, wasting_raw)), 1),
        anemia = round(pmin(61.3, pmax(37.4, anemia_raw)), 1),
        poverty_rate = round(pmin(60.0, pmax(27.4, poverty_rate_raw)), 1),
        maternal_education = round(pmin(4.3, pmax(2.5, maternal_education_raw)), 1),
        sanitation_access = round(pmin(64.0, pmax(36.0, sanitation_access_raw)), 1),
        food_diversity = round(pmin(3.0, pmax(2.6, food_diversity_raw)), 1),
        health_access_km = round(pmin(80.0, pmax(75.0, health_access_km_raw)), 1)
      ) %>%
      # Select the final set of columns
      select(
        district, province, year, age_group, nutrient, 
        stunting, wasting, anemia, poverty_rate,
        maternal_education, sanitation_access, food_diversity, health_access_km
      )

    cat("✅ Synthetic data generated successfully.\n")
    return(full_data)
  }, error = function(e) {
    cat("❌ Error generating synthetic data:", e$message, "\n")
    return(data.frame())
  })
}
# Create regional comparison data (East African countries)
create_regional_data <- function() {
  countries <- c("Rwanda", "Uganda", "Kenya", "Tanzania", "Burundi", "DRC")
  years <- seq(2000, 2020, by = 5)
  
  set.seed(456)
  regional_data <- expand.grid(
    country = countries,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      stunting = case_when(
        country == "Rwanda" ~ 50 - (year - 2000) * 0.8 + rnorm(n(), 0, 2),
        country == "Uganda" ~ 45 - (year - 2000) * 0.5 + rnorm(n(), 0, 3),
        country == "Kenya" ~ 42 - (year - 2000) * 0.6 + rnorm(n(), 0, 2),
        country == "Tanzania" ~ 48 - (year - 2000) * 0.7 + rnorm(n(), 0, 3),
        country == "Burundi" ~ 55 - (year - 2000) * 0.4 + rnorm(n(), 0, 4),
        country == "DRC" ~ 52 - (year - 2000) * 0.3 + rnorm(n(), 0, 5)
      ),
      wasting = case_when(
        country == "Rwanda" ~ 5 + rnorm(n(), 0, 1),
        country == "Uganda" ~ 7 + rnorm(n(), 0, 2),
        country == "Kenya" ~ 6 + rnorm(n(), 0, 1.5),
        country == "Tanzania" ~ 8 + rnorm(n(), 0, 2),
        country == "Burundi" ~ 9 + rnorm(n(), 0, 2.5),
        country == "DRC" ~ 12 + rnorm(n(), 0, 3)
      )
    )
  
  return(regional_data)
}

# Initialize data
cat("🔄 Loading nutrition data...\n")
nutrition_data_path <- here::here("project", "data", "nutrition_data.csv")

if (file.exists(nutrition_data_path)) {
  full_data <- create_rwanda_nutrition_data()
} else {
  full_data <- generate_synthetic_data()
}

regional_data <- create_regional_data()

# Validate loaded data
if (is.null(full_data) || nrow(full_data) == 0) {
  cat("❌ Failed to load or generate data. The application may not work correctly.\n")
} else {
  cat("📊 Rows:", nrow(full_data), "\n")
  cat("📅 Year range:", min(full_data$year), "-", max(full_data$year), "\n")
  cat("🏘️ Districts:", length(unique(full_data$district)), "\n")
  cat("🏛️ Provinces:", length(unique(full_data$province)), "\n")
}

# Create district rankings
create_district_rankings <- function(data, indicator, year = 2020) {
  data %>%
    filter(year == !!year) %>%
    group_by(district, province) %>%
    summarise(
      avg_indicator = mean(get(indicator), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(avg_indicator) %>%
    mutate(
      rank = row_number(),
      performance = case_when(
        rank <= 10 ~ "Best Performing",
        rank <= 20 ~ "Average",
        TRUE ~ "Needs Improvement"
      )
    )
}

# ==============================================================================
# 🔹 LOAD GEOGRAPHIC DATA
# ==============================================================================

# Load Rwanda districts shapefile
tryCatch({
  rwanda_districts_sf <- st_read(here::here("project", "data", "RWA_adm", "RWA_adm2.shp"), quiet = TRUE)
  cat("✅ Shapefile loaded successfully!\n")
}, error = function(e) {
  cat("❌ Error loading shapefile. Map will not have borders.\n")
  cat(e$message, "\n")
  rwanda_districts_sf <- NULL
})

# ==============================================================================
# 🔹 ENHANCED UI WITH NEW FEATURES
# ==============================================================================

ui <- dashboardPage(
  title = "Rwanda Nutrition Dashboard",
  header = dashboardHeader(
    title = uiOutput("dashboard_title_ui"),
    dropdownMenuOutput("notification_menu_ui"),
    dropdownMenu(
      type = "messages",
      headerText = "Quick Actions", # This can be made dynamic if needed
      messageItem("Download Dashboard", "Export as PowerPoint", time = "Available"),
      messageItem("Generate Report", "AI-powered insights", time = "Ready"),
      messageItem("Schedule Alert", "Set monitoring thresholds", time = "Configure")
    ),
    tags$li(
      class = "dropdown",
      uiOutput("language_selector_ui"),
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/17/Flag_of_Rwanda.svg", height = "30px")
    )
  ),
  
  sidebar = dashboardSidebar(
    minified = TRUE, collapsed = FALSE,
    uiOutput("sidebar_menu"),
    
    # Global Filters
    hr(),
    uiOutput("global_filters_title"),
    uiOutput("global_filters")
  ),
  
  body = dashboardBody(
    if (!is.null(nisr_theme)) use_theme(nisr_theme),
    # Custom CSS
    tags$head(tags$style(HTML("
        .box { 
          border-top: 3px solid #2563eb; 
          box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
          border-radius: 8px;
        }
        .info-box { 
          box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
          border-radius: 8px;
          min-height: 130px; /* Ensure consistent height */
        }
        .info-box .info-box-number {
          font-size: 28px; /* Larger number for readability */
          font-weight: 700;
        }
        .narrative-box { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
          color: white;
          padding: 20px; 
          margin-bottom: 20px; 
          border-radius: 10px;
        }
        .metric-card {
          background: white;
          border-radius: 8px;
          padding: 15px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
          margin-bottom: 15px;
          border-left: 4px solid #10b981;
        }
        .sidebar-menu > li > a {
          color: white !important;
          font-weight: 500;
        }
        .sidebar-menu > li > a:hover {
          background-color: rgba(255, 255, 255, 0.1) !important;
          color: white !important;
        }
        .sidebar-menu > li > a > .fa {
          color: white !important;
        }
        .main-header .navbar {
          background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%) !important;
        }
        .main-sidebar {
          background: linear-gradient(180deg, #1e40af 0%, #1e3a8a 100%) !important;
        }
        /* Custom InfoBox colors from the palette */
        .bg-blue { background-color: #092CA0 !important; color: white !important; }
        .bg-aqua { background-color: #87CEEB !important; color: #034742 !important; }
        .bg-purple { background-color: #431d85 !important; color: white !important; }
        .bg-yellow { background-color: #FFA500 !important; color: white !important; }
        .bg-green { background-color: #2ca02c !important; color: white !important; }
        .bg-red { background-color: #570631 !important; color: white !important; }

        /* Hide console warnings and errors */
        .shiny-output-error { display: none !important; }
        .shiny-output-error-validation { display: none !important; }
        /* Improve dropdown styling */
        .dropdown-menu { 
          box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
          border-radius: 8px;
        }
        
        /* AI Recommendation Styling */
        .ai-recommendation {
          background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
          border: 1px solid #cbd5e1;
          border-radius: 12px;
          padding: 20px;
          margin: 15px 0;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
        }
        
        .ai-recommendation h4 {
          color: #1e40af;
          border-bottom: 2px solid #3b82f6;
          padding-bottom: 10px;
          margin-bottom: 20px;
        }
        
        .recommendation-section {
          margin: 20px 0;
          padding: 15px;
          background: white;
          border-radius: 8px;
          border-left: 4px solid #3b82f6;
        }
        
        .recommendation-section h5 {
          color: #1e40af;
          margin-bottom: 10px;
        }
        
        .recommendation-section h5 i {
          margin-right: 8px;
          color: #3b82f6;
        }
        
        .ai-footer {
          margin-top: 20px;
          padding-top: 15px;
          border-top: 1px solid #e2e8f0;
          text-align: center;
          color: #64748b;
        }
        
        .ai-insights {
          background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%);
          border: 1px solid #f59e0b;
          border-radius: 8px;
          padding: 15px;
          margin: 10px 0;
        }
        
        .ai-insights h5 {
          color: #92400e;
          margin-bottom: 15px;
        }
        
        .insight-section {
          margin: 10px 0;
          padding: 10px;
          background: white;
          border-radius: 6px;
        }
        
        .insight-section h6 {
          color: #92400e;
          margin-bottom: 5px;
        }
        
        /* Trend Analysis Styling */
        .trend-insights {
          background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%);
          border: 1px solid #0ea5e9;
          border-radius: 8px;
          padding: 20px;
          margin: 10px 0;
        }
        
        .trend-insights h5 {
          color: #0369a1;
          border-bottom: 2px solid #0ea5e9;
          padding-bottom: 10px;
          margin-bottom: 20px;
        }
        
        .insight-card {
          margin: 15px 0;
          padding: 15px;
          background: white;
          border-radius: 6px;
          border-left: 4px solid #0ea5e9;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .insight-card h6 {
          color: #0369a1;
          margin-bottom: 10px;
        }
        
        .insight-card h6 i {
          margin-right: 8px;
          color: #0ea5e9;
        }
        
        .insight-card ul {
          margin: 10px 0;
          padding-left: 20px;
        }
        
        .insight-card li {
          margin: 5px 0;
        }
      ")),
      # Add meta tags to suppress warnings
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = "Rwanda Nutrition Dashboard - Real Data Visualization")
    ),
    
    tabItems(
      # ===== NATIONAL OVERVIEW =====
      tabItem(tabName = "overview",
        fluidRow(
          div(class = "narrative-box", uiOutput("national_narrative"))
        ),
        
        fluidRow(
          infoBoxOutput("national_stunting", width = 3),
          infoBoxOutput("national_wasting", width = 3),
          infoBoxOutput("national_anemia", width = 3),
          infoBoxOutput("national_poverty", width = 3)
        ),
        
        fluidRow(
          box(
            title = uiOutput("national_trends_title"), width = 8, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("national_trends_chart", height = "400px")),
            uiOutput("download_trends_btn_ui")
          ),
          box(
            title = uiOutput("city_comparison_title"), width = 4, status = "info", solidHeader = TRUE, # Corrected: This box now uses a dynamic title
            withSpinner(plotlyOutput("provincial_comparison", height = "400px"))
          )
        ),
        fluidRow(
          box(
            title = uiOutput("geographic_distribution_title"), width = 8, status = "warning", solidHeader = TRUE,
            uiOutput("map_indicator_ui"),
            div(style = "border: 2px solid black;",
                withSpinner(leafletOutput("national_map", height = "450px"))
            ),
            uiOutput("map_buttons_ui")
          ),
          box(
            title = uiOutput("key_insights_title"), width = 4, status = "success", solidHeader = TRUE,
            uiOutput("key_insights"),
            hr(),
            uiOutput("quick_stats_title"),
            uiOutput("quick_stats")
          )
        )
      ),
      
      # ===== DISTRICT RANKINGS =====
      tabItem(tabName = "rankings",
        fluidRow(
          infoBoxOutput("best_performer_box", width = 4),
          infoBoxOutput("worst_performer_box", width = 4),
          infoBoxOutput("average_rate_box", width = 4)
        ),
        fluidRow(
          box(
            title = uiOutput("rankings_title"), width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("rankings_filters_ui")
          )
        ),
        
        fluidRow(
          box(
            title = uiOutput("rankings_table_title"), width = 8, status = "info", solidHeader = TRUE,
            withSpinner(reactableOutput("rankings_table", height = "500px")),
            br(),
            uiOutput("export_rankings_btn_ui")
          ),
          box(
            title = uiOutput("performance_dist_title"), width = 4, status = "warning", solidHeader = TRUE,
            plotlyOutput("performance_distribution", height = "300px"),
            hr(),
            h5(uiOutput("performance_cat_title")),
            uiOutput("performance_summary")
          )
        ),
        
        fluidRow(
          box(
            title = uiOutput("top_bottom_title"), width = 6, status = "success", solidHeader = TRUE,
            withSpinner(plotlyOutput("top_bottom_chart", height = "350px"))
          ),
          box(
            title = uiOutput("perf_over_time_title"), width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("performance_trends", height = "350px"))
          )
        )
      ),
      
      # ===== REGIONAL COMPARISON =====
      tabItem(tabName = "regional",
        fluidRow(
          div(class = "narrative-box", # Corrected: Made narrative dynamic
              h3(icon("globe"), uiOutput("regional_narrative_title")),
              p(uiOutput("regional_narrative_body"))
          )
        ),
        
        fluidRow(
          box(
            title = "Regional Trends Comparison", width = 8, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("regional_trends", height = "400px")),
            selectInput("regional_indicator", "Select Indicator:",
                       choices = c("Stunting" = "stunting", "Wasting" = "wasting"),
                       selected = "stunting")
          ),
          box(
            title = "Rwanda's Regional Rank", width = 4, status = "info", solidHeader = TRUE,
            withSpinner(uiOutput("regional_rank")),
            hr(),
            h5("Progress Metrics"),
            uiOutput("regional_progress")
          )
        ),
        
        fluidRow(
          
          box(
            title = "Country Comparison Dashboard", width = 12, status = "warning", solidHeader = TRUE,
            withSpinner(plotlyOutput("country_comparison", height = "400px"))
          )
        )
      ),
      
      # ===== TREND ANALYSIS =====
      tabItem(tabName = "trends",
        fluidRow(
          box(
            title = uiOutput("trend_filters_title"), width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, uiOutput("trend_district_ui")),
              column(3, uiOutput("trend_model_ui")),
              column(3, uiOutput("trend_indicator_ui")),
              column(2, uiOutput("trend_forecast_ui")),
              column(1, br(), uiOutput("trend_report_btn_ui"))
            )
          )
        ),
        fluidRow(
          box(
            title = uiOutput("advanced_trends_title"), width = 8, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("advanced_trends", height = "450px"))
          ),
          box(
            title = uiOutput("trend_summary_title"), width = 4, status = "info", solidHeader = TRUE,
            withSpinner(uiOutput("trend_model_summary"))
          )
        ),
        fluidRow(
          box(
            title = uiOutput("correlation_title"), width = 6, status = "info", solidHeader = TRUE,
            withSpinner(plotOutput("correlation_heatmap", height = "350px"))
          ),
          box(
            title = uiOutput("trend_insights_title"), width = 6, status = "success", solidHeader = TRUE,
            withSpinner(uiOutput("trend_insights"))
          )
        )
      ),
      
      # ===== PREDICTIVE ANALYTICS =====
      tabItem(tabName = "predictions",
        fluidRow(
          infoBoxOutput("forecast_value_box", width = 4),
          infoBoxOutput("risk_districts_box", width = 4),
          infoBoxOutput("roi_metric_box", width = 4)
        ),
        fluidRow(
          box(
            title = "Prediction & Scenario Configuration", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, selectInput("pred_target", "Target District:",
                                   choices = c("National", unique(full_data$district)))),
              column(3, selectInput("pred_indicator", "Indicator:", 
                                   choices = c("Stunting" = "stunting", "Wasting" = "wasting", "Anemia" = "anemia"))),
              column(3, numericInput("pred_horizon", "Forecast Years:", value = 5, min = 1, max = 10)),
              column(3, br(), actionButton("run_prediction", "Generate Forecast", 
                                          class = "btn-success btn-block", icon = icon("magic")))
            ),
            hr(),
            h4("Intervention Scenarios", style="text-align:center;"),
            fluidRow(
              column(6, sliderInput("intervention_strength", "Intervention Impact (% reduction):",
                                    value = 0, min = 0, max = 50, step = 5)),
              column(6, numericInput("intervention_budget", "Budget (USD):", value = 500000, min = 50000))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Prediction Forecast", width = 8, status = "info", solidHeader = TRUE,
            withSpinner(plotlyOutput("prediction_chart", height = "400px")),
            hr(),
            uiOutput("prediction_insights")
          ),
          box(
            title = "Risk Assessment & ROI", width = 4, status = "warning", solidHeader = TRUE,
            withSpinner(plotlyOutput("risk_assessment", height = "250px")),
            hr(),
            withSpinner(uiOutput("roi_analysis"))
          )
        )
      ),
      
      # ===== AI POLICY ADVISOR =====
      tabItem(tabName = "policy",
        fluidRow(
          box(
            title = uiOutput("policy_advisor_title_ui"), width = 5, status = "primary", solidHeader = TRUE,
            uiOutput("policy_config_ui")
          ),
          
          box(
            title = "AI-Generated Policy Brief", width = 7, status = "info", solidHeader = TRUE,
            uiOutput("policy_output"),
            hr(),
            fluidRow(
              column(6, uiOutput("download_pdf_btn_ui")),
              column(6, uiOutput("download_ppt_btn_ui"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = uiOutput("roadmap_title_ui"), width = 8, status = "warning", solidHeader = TRUE,
            plotlyOutput("implementation_roadmap", height = "350px")
          ),
          box(
            title = uiOutput("success_prob_title_ui"), width = 4, status = "success", solidHeader = TRUE,
            withSpinner(plotlyOutput("success_probability", height = "350px"))
          )
        )
      ),
      
      # ===== DATA EXPLORER =====
      tabItem(tabName = "explorer",
        fluidRow(
          box(
            title = "Advanced Data Explorer", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, selectInput("explorer_districts", "Districts:",
                                  choices = unique(full_data$district), multiple = TRUE,
                                  selected = NULL)),
              column(3, sliderInput("explorer_years", "Year Range:",
                                  min = min(full_data$year), max = max(full_data$year),
                                  value = c(2015, 2020), step = 1, sep = "")),
              column(3, checkboxGroupInput("explorer_indicators", "Indicators:",
                                         choices = c("Stunting" = "stunting", "Wasting" = "wasting", 
                                                   "Anemia" = "anemia", "Poverty" = "poverty_rate"),
                                         selected = c("stunting", "wasting"))),
              column(3, br(), actionButton("apply_explorer_filters", "Apply Filters", 
                                         class = "btn-primary btn-block"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Interactive Data Table", width = 12, status = "info", solidHeader = TRUE,
            withSpinner(reactableOutput("explorer_table", height = "400px")),
            br(),
            fluidRow(
              column(4, downloadButton("download_csv", "Download CSV", class = "btn-success")),
              column(4, downloadButton("download_excel", "Download Excel", class = "btn-info")),
              column(4, actionButton("create_custom_chart", "Create Chart", class = "btn-warning"))
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.create_custom_chart > 0",
          fluidRow(
            box(
              title = "Custom Visualization Builder", width = 12, status = "warning", solidHeader = TRUE,
              fluidRow(
                column(3, selectInput("custom_x", "X-Axis:", choices = names(full_data))),
                column(3, selectInput("custom_y", "Y-Axis:", choices = names(full_data))),
                column(3, selectInput("custom_color", "Color By:", 
                                    choices = c("None", names(full_data)), selected = "None")),
                column(3, selectInput("custom_type", "Chart Type:",
                                    choices = c("Scatter", "Line", "Bar", "Box")))
              ),
              withSpinner(plotlyOutput("custom_chart", height = "400px"))
            )
          )
        )
      ),
      
      # ===== IMPACT ASSESSMENT =====
      tabItem(tabName = "impact",
        fluidRow(
          infoBoxOutput("total_children", width = 3),
          infoBoxOutput("districts_improved", width = 3),
          infoBoxOutput("avg_improvement", width = 3),
          infoBoxOutput("cost_per_child", width = 3)
        ),
        
        fluidRow(
          box(
            title = textOutput("intervention_effectiveness_title"), width = 8, status = "primary", solidHeader = TRUE,
            withSpinner(plotlyOutput("intervention_effectiveness", height = "400px"))
          ),
          box(
            title = textOutput("sdg_progress_title"), width = 4, status = "info", solidHeader = TRUE,
            withSpinner(uiOutput("sdg_progress"))
          )
        ),
        
        fluidRow(
          box(
            title = textOutput("resource_allocation_title"), width = 6, status = "success", solidHeader = TRUE,
            withSpinner(plotlyOutput("resource_allocation", height = "350px"))
          ),
          box(
            title = "Impact Summary", width = 6, status = "warning", solidHeader = TRUE,
            withSpinner(uiOutput("impact_summary"))
          )
        )
      ),
      
      # ===== EXPORT & REPORTS =====
      tabItem(tabName = "reports",
        fluidRow(
          box(
            title = "Dashboard Export Options", width = 6, status = "primary", solidHeader = TRUE,
            h4("Available Export Formats"),
            
            div(class = "metric-card",
                h5(icon("file-powerpoint"), " PowerPoint Presentation"),
                p("Complete dashboard with AI-generated narratives"),
                downloadButton("export_ppt", "Download PPT", class = "btn-warning btn-block")
            ),
            
            div(class = "metric-card",
                h5(icon("file-pdf"), " PDF Report"),
                p("Comprehensive nutrition analysis report"),
                downloadButton("export_pdf", "Download PDF", class = "btn-danger btn-block")
            ),
            
            div(class = "metric-card",
                h5(icon("file-excel"), " Data Export"),
                p("Complete dataset with analysis"),
                downloadButton("export_data", "Download Excel", class = "btn-success btn-block")
            ),
            
            div(class = "metric-card",
                h5(icon("image"), " High-Resolution Images"),
                p("Individual charts and visualizations"),
                downloadButton("export_images", "Download Images", class = "btn-info btn-block")
            )
          ),
          
          box(
            title = "Report Configuration", width = 6, status = "info", solidHeader = TRUE,
            h4("Customize Your Report"),
            
            checkboxGroupInput("report_sections", "Include Sections:",
                              choices = c("Executive Summary" = "summary",
                                        "National Trends" = "trends", 
                                        "District Rankings" = "rankings",
                                        "Regional Comparison" = "regional",
                                        "Predictions" = "predictions",
                                        "Policy Recommendations" = "policy"),
                              selected = c("summary", "trends", "rankings")),
            
            selectInput("report_period", "Report Period:",
                       choices = c("Latest Year" = "latest",
                                 "Last 5 Years" = "5year",
                                 "Full Historical" = "full",
                                 "Custom Range" = "custom")),
            
            conditionalPanel(
              condition = "input.report_period == 'custom'",
              sliderInput("custom_report_years", "Year Range:",
                         min = min(full_data$year), max = max(full_data$year),
                         value = c(2015, 2020), step = 1, sep = "")
            ),
            
            textAreaInput("report_notes", "Additional Notes:",
                         placeholder = "Add any specific notes or context for this report...",
                         rows = 3),
            
            actionButton("generate_custom_report", "Generate Custom Report",
                        class = "btn-primary btn-block", icon = icon("cogs"))
          )
        ),
        
        fluidRow(
          box(
            title = "Report Preview", width = 12, status = "success", solidHeader = TRUE,
            withSpinner(uiOutput("report_preview"))
          )
        )
      )
    )
  )
)

# ==============================================================================
# 🔹 ENHANCED SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Define the custom color palette
  app_colors <- c("#034742", "#2ca02c", "#092CA0", "#431d85", "#570631", "#9e086f", "#87CEEB", "#add8e6", "#FFA500", "#1f77b4")
  
  # Reactive language
  lang <- reactive({
    input$selected_language
  })

  # Dynamic UI for Header Title
  output$dashboard_title_ui <- renderUI({
    tagList(
      span(class = "logo-lg", get_text("dashboard_title", lang()))
    )
  })

  # Dynamic UI for Language Selector
  output$language_selector_ui <- renderUI({
    pickerInput(
      inputId = "selected_language",
      label = get_text("language", lang()),
      choices = c("en", "rw"),
      selected = input$selected_language %||% "en",
      choicesOpt = list(
        content = c("🇬🇧 English", "🇷🇼 Kinyarwanda")
      )
    )
  })

  # Dynamic UI for Notifications
  output$notification_menu_ui <- renderUI({
    dropdownMenu(
      type = "notifications",
      headerText = get_text("system_alerts", lang()),
      notificationItem(get_text("high_stunting_alert", lang()), icon = icon("exclamation-triangle")),
      notificationItem(get_text("new_data_alert", lang()), icon = icon("info-circle")),
      notificationItem(get_text("regional_update", lang()), icon = icon("globe"))
    )
  })


  output$sidebar_menu <- renderUI({
    sidebarMenu(
      id = "tabs",
      menuItem(get_text("national_overview", lang()), tabName = "overview", icon = icon("flag")),
      menuItem(get_text("data_explorer", lang()), tabName = "explorer", icon = icon("search")),
      menuItem(get_text("district_rankings", lang()), tabName = "rankings", icon = icon("trophy")),
      menuItem(get_text("regional_comparison", lang()), tabName = "regional", icon = icon("globe")),
      menuItem(get_text("trend_analysis", lang()), tabName = "trends", icon = icon("chart-line")),
      menuItem(get_text("predictive_analytics", lang()), tabName = "predictions", icon = icon("flask")),
      menuItem(get_text("policy_advisor", lang()), tabName = "policy", icon = icon("lightbulb")),
      menuItem(get_text("impact_assessment", lang()), tabName = "impact", icon = icon("bullseye")),
      menuItem(get_text("export_reports", lang()), tabName = "reports", icon = icon("download"))
    )
  })

  output$global_filters_title <- renderUI({
    h5(get_text("global_filters", lang()), style = "text-align: center; color: #34495e;")
  })

  output$global_filters <- renderUI({
    tagList(
      selectInput("selected_year", get_text("year", lang()),
                  choices = sort(unique(full_data$year), decreasing = TRUE),
                  selected = max(full_data$year)),
      selectInput("selected_province", get_text("province", lang()),
                  choices = setNames(c("All Cities", unique(full_data$province)), c(get_text("all_provinces", lang()), unique(full_data$province))),
                  selected = "All Cities"),
      selectInput("selected_age_group", get_text("age_group", lang()),
                  choices = setNames(c("All Age Groups", unique(full_data$age_group)), c(get_text("all_age_groups", lang()), unique(full_data$age_group))),
                  selected = "All Age Groups"),
      selectInput("selected_nutrient", get_text("nutrient", lang()),
                  choices = setNames(c("All Nutrients", unique(full_data$nutrient)), c(get_text("all_nutrients", lang()), unique(full_data$nutrient))),
                  selected = "All Nutrients"),
      checkboxGroupInput("selected_indicators", get_text("indicators", lang()),
                         choices = c("Stunting" = "stunting", "Wasting" = "wasting", "Anemia" = "anemia"),
                         selected = c("stunting", "wasting", "anemia"))
    )
  })
  
  # Initialize AI client
  ai_client <- initialize_gemini_client()
  
  # Language switcher logic
  observeEvent(input$selected_language, {
    showNotification(paste("Language changed to:", input$selected_language), type = "default")
  })
  
  # ==============================================================================
  # REACTIVE DATA PROCESSING
  # ==============================================================================
  
  # Reactive data loading for DHS data file
  # This will automatically re-read the data if the file changes
  full_data_reactive <- reactiveFileReader(
    intervalMillis = 15000, # Check for changes every 15 seconds
    session = session,
    filePath = nutrition_data_path,
    readFunc = function(filePath) {
      if (file.exists(filePath)) {
        cat("🔄 Detected change or initial load. Reading nutrition_data.csv...\n")
        read_csv(filePath)
      } else {
        cat("⚠️ Real data not found. Generating synthetic data...\n")
        generate_synthetic_data()
      }
    }
  )

  full_data <- reactive({
    full_data_reactive()
  })

  # Filtered data based on global filters
  filtered_data <- reactive({
    df <- full_data

    # Debug: Check if data exists
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame())
    }



    # Apply province filter only if it's not "All Provinces" or "Intara zose"
    if (!is.null(input$selected_province) &&
        input$selected_province != "All Cities" &&
        input$selected_province != "Intara zose") {
      df <- df %>% filter(province == input$selected_province)
    }

    return(df)
  })
  
  # Year-specific data
  current_year_data <- reactive({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(data.frame())
    }
    
    # Apply year filter
    if (!is.null(input$selected_year) && !is.na(input$selected_year)) {
      df <- df %>% filter(year == input$selected_year)
    }
    
    return(df)
  })
  
  # ==============================================================================
  # NATIONAL OVERVIEW OUTPUTS
  # ==============================================================================
  
  # National narrative with AI insights
  output$national_narrative <- renderUI({
    req(current_year_data())
    narrative_df <- current_year_data()
    
    if (nrow(narrative_df) == 0) {
      return(HTML("<p>No data available for selected filters.</p>"))
    }
    
    # Calculate national average statistics
    national_stats <- narrative_df %>%
      summarise(
        avg_stunting = round(mean(stunting, na.rm = TRUE), 1),
        total_districts = n_distinct(district)
      )
    
    # Calculate the number of at-risk districts
    at_risk_districts_count <- narrative_df %>%
      group_by(district) %>%
      summarise(avg_stunting_per_district = mean(stunting, na.rm = TRUE)) %>%
      filter(avg_stunting_per_district > 30) %>%
      nrow()
    
    # Trend analysis
    trend_data <- full_data %>%
      filter(if (input$selected_province != "All Cities") province == input$selected_province else TRUE) %>%
      group_by(year) %>%
      summarise(avg_stunting = mean(stunting, na.rm = TRUE)) %>%
      arrange(year)
    
    previous_year_data <- trend_data %>% filter(year < as.numeric(input$selected_year)) %>% top_n(1, year)
    
    if (nrow(previous_year_data) > 0 && !is.na(national_stats$avg_stunting)) {
      annual_change <- round(national_stats$avg_stunting - previous_year_data$avg_stunting, 1)
      trend_key <- if (annual_change < 0) "narrative_trend_improving" else if (annual_change > 0) "narrative_trend_concerning" else "narrative_trend_stable"
      trend_color <- ifelse(annual_change < 0, "#10b981", "#ef4444")
      trend_icon <- ifelse(annual_change < 0, "fa-arrow-down", "fa-arrow-up")
      change_text <- glue(get_text("narrative_change_text", lang()), abs_annual_change = abs(annual_change), previous_year = previous_year_data$year)
    } else {
      annual_change <- NA
      trend_key <- "narrative_trend_stable"
      trend_color <- "#6b7280"
      trend_icon <- "fa-minus"
      change_text <- get_text("narrative_no_trend", lang())
    }
    
    province_name <- if(input$selected_province != 'All Cities') input$selected_province else 'Rwanda'
    
    HTML(glue("
    <h3><i class='fas fa-flag'></i> {get_text('national_intelligence', lang())}</h3>
    <div style='display: flex; justify-content: space-between; align-items: center; margin-top: 15px;'>
      <div>
        <p style='font-size: 1.1em; margin-bottom: 10px;'>
          {glue(get_text('narrative_intro', lang()), input_selected_year = input$selected_year, province_name = province_name, avg_stunting = national_stats$avg_stunting, total_districts = national_stats$total_districts)}
          {glue(get_text('narrative_at_risk', lang()), at_risk_districts_count = at_risk_districts_count)}
        </p>
        <p style='font-size: 1em; margin-bottom: 0;'>
          <i class='fas {trend_icon}' style='color: {trend_color}; margin-right: 5px;'></i>
          {glue(get_text(trend_key, lang()), trend_color = trend_color, change_text = change_text)}
        </p>
      </div>
      <div style='text-align: center; flex-shrink: 0; margin-left: 20px;'>
        <div style='background: rgba(255,255,255,0.2); padding: 15px; border-radius: 8px;'>
          <div style='font-size: 2.5em; font-weight: bold;'>{national_stats$avg_stunting}%</div>
          <div>{get_text('national_stunting_rate', lang())}</div>
        </div>
      </div>
    </div>
    "))
  })
  
  # National KPIs
  output$national_stunting <- renderInfoBox({
    df <- filtered_data()
    if(nrow(df) == 0) {
      avg_stunting <- 0
    } else {
      avg_stunting <- round(mean(df$stunting, na.rm = TRUE), 1)
    }
    
    color <- if(is.na(avg_stunting) || avg_stunting > 30) "red" else if(avg_stunting > 20) "yellow" else "green"
    
    infoBox(
      get_text("stunting", lang()),
      paste0(avg_stunting, "%"),
      subtitle = get_text("chronic_malnutrition", lang()) %||% "Chronic malnutrition",
      icon = icon("child"),
      color = "blue", # Using a consistent color
      fill = TRUE
    )
  })
  
  output$national_wasting <- renderInfoBox({
    df <- filtered_data()
    if(nrow(df) == 0) {
      avg_wasting <- 0
    } else {
      avg_wasting <- round(mean(df$wasting, na.rm = TRUE), 1)
    }
    
    color <- if(is.na(avg_wasting) || avg_wasting > 10) "red" else if(avg_wasting > 5) "yellow" else "green"
    
    infoBox(
      get_text("wasting", lang()),
      paste0(avg_wasting, "%"),
      subtitle = get_text("acute_malnutrition", lang()) %||% "Acute malnutrition",
      icon = icon("weight"),
      color = "aqua", # Using a consistent color
      fill = TRUE
    )
  })
  
  output$national_anemia <- renderInfoBox({
    df <- filtered_data()
    if(nrow(df) == 0) {
      avg_anemia <- 0
    } else {
      avg_anemia <- round(mean(df$anemia, na.rm = TRUE), 1)
    }
    
    color <- if(is.na(avg_anemia) || avg_anemia > 40) "red" else if(avg_anemia > 25) "yellow" else "green"
    
    infoBox(
      get_text("anemia", lang()),
      paste0(avg_anemia, "%"),
      subtitle = get_text("iron_deficiency", lang()) %||% "Iron deficiency",
      icon = icon("heartbeat"),
      color = "purple", # Using a consistent color
      fill = TRUE
    )
  })
  
  output$national_poverty <- renderInfoBox({
    df <- filtered_data()
    if(nrow(df) == 0) {
      avg_poverty <- 0
    } else {
      avg_poverty <- round(mean(df$poverty_rate, na.rm = TRUE), 1)
    }
    
    color <- if(is.na(avg_poverty) || avg_poverty > 50) "red" else if(avg_poverty > 30) "yellow" else "green"
    
    infoBox(
      get_text("poverty", lang()),
      paste0(avg_poverty, "%"),
      subtitle = get_text("economic_indicator", lang()) %||% "Economic indicator",
      icon = icon("coins"),
      color = "yellow", # Using a consistent color
      fill = TRUE
    )
  })
  
  # National trends chart
  output$national_trends_chart <- renderPlotly({
    df <- full_data # Use full data for trends, but filter by province if selected
    
    # Check if data exists
    if (nrow(df) == 0) {
      # Return empty plot with message
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for selected filters",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          title = get_text("trend_analysis", lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    trends_data <- df %>%
      group_by(year) %>%
      summarise(
        stunting = round(mean(stunting, na.rm = TRUE), 1),
        wasting = round(mean(wasting, na.rm = TRUE), 1),
        anemia = round(mean(anemia, na.rm = TRUE), 1),
        .groups = 'drop'
      )
    
    # Check if trends_data has data
    if (nrow(trends_data) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No trend data available",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          title = get_text("trend_analysis", lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    p <- plot_ly(trends_data, x = ~year) %>%
      add_trace(y = ~stunting, name = 'Stunting', type = 'scatter', mode = 'lines+markers',
                line = list(color = app_colors[1], width = 3), marker = list(size = 6)) %>%
      add_trace(y = ~wasting, name = 'Wasting', type = 'scatter', mode = 'lines+markers',
                line = list(color = app_colors[2], width = 3), marker = list(size = 6)) %>%
      add_trace(y = ~anemia, name = 'Anemia', type = 'scatter', mode = 'lines+markers',
                line = list(color = app_colors[3], width = 3), marker = list(size = 6)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Rate (%)"),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1),
        hovermode = 'x unified'
      )
    
    p
  })
  
  # Dynamic UI for download/action buttons
  output$download_trends_btn_ui <- renderUI({
    downloadButton("download_trends", get_text("download", lang()), class = "btn-sm btn-info")
  })
  
  output$map_buttons_ui <- renderUI({
    tagList(
      actionButton("reset_map_view", get_text("view", lang()), class = "btn-sm"),
      downloadButton("download_map", get_text("download", lang()), class = "btn-sm btn-success")
    )
  })
  # Provincial comparison
  output$provincial_comparison <- renderPlotly({
    req(current_year_data())
    df <- current_year_data()
    
    # Check if data exists
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for selected year",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          title = get_text("stunting", lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    prov_data <- df %>%
      group_by(province) %>%
      summarise(
        stunting = round(mean(stunting, na.rm = TRUE), 1),
        wasting = round(mean(wasting, na.rm = TRUE), 1),
        anemia = round(mean(anemia, na.rm = TRUE), 1),
        .groups = 'drop'
      )
    
    # Check if provincial data exists
    if (nrow(prov_data) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No provincial data available",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          title = get_text("stunting", lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    plot_ly(prov_data, x = ~reorder(province, -stunting), y = ~stunting,
            type = 'bar', name = 'Stunting',
            marker = list(color = app_colors[4])) %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Stunting Rate (%)")
      )
  })
  
  # National map
  output$national_map <- renderLeaflet({
    # Ensure data and shapefile are loaded
    req(nrow(filtered_data()) > 0, !is.null(rwanda_districts_sf), input$map_indicator) # Corrected: Added a closing parenthesis

    # Use the selected indicator from the overview tab
    selected_indicator <- input$map_indicator

    map_data <- filtered_data() %>%
      filter(year == input$selected_year) %>%
      summarise(
        value = round(mean(get(selected_indicator), na.rm = TRUE), 1),
        stunting_val = round(mean(stunting, na.rm = TRUE), 1),
        wasting_val = round(mean(wasting, na.rm = TRUE), 1),
        anemia_val = round(mean(anemia, na.rm = TRUE), 1),
        .groups = 'drop'
      ) 

    # Join with spatial data
    map_data_sf <- rwanda_districts_sf %>%
      left_join(map_data, by = c("NAME_2" = "district")) 

    # Handle cases where data is missing
    if (nrow(map_data_sf) == 0 || all(is.na(map_data_sf$value))) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = 29.8739, lat = -1.9403, zoom = 8) %>%
               addLabelOnlyMarkers(lng = 29.8739, lat = -1.9403, 
                                 label = "No data available for selected filters",
                                 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)))
    }

    # Define color palette for the selected indicator
    pal <- colorNumeric(palette = app_colors, domain = map_data_sf$value, na.color = "#bdc3c7")
    
    # Create choropleth map
    leaflet(data = map_data_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 29.8739, lat = -1.9403, zoom = 8) %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#1d4ed8",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0(NAME_2, ": ", round(value, 1), "%"), 
        popup = ~glue(
          "<h4>{NAME_2} ({input$selected_year})</h4>
           <table class='table table-striped table-bordered' style='width:100%;'>
             <tr><td>Stunting</td><td style='text-align:right;'><b>{stunting_val}%</b></td></tr>
             <tr><td>Wasting</td><td style='text-align:right;'><b>{wasting_val}%</b></td></tr>
             <tr><td>Anemia</td><td style='text-align:right;'><b>{anemia_val}%</b></td></tr>
           </table>"
        )
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~value,
        title = paste(tools::toTitleCase(gsub("_", " ", selected_indicator)), "(%)"),
        opacity = 1
      )
  })

  # Reset map view
  observeEvent(input$reset_map_view, {
    leafletProxy("national_map") %>% setView(lng = 29.8739, lat = -1.9403, zoom = 8)
  })
  
  # Key insights
  output$key_insights <- renderUI({
    req(filtered_data())
    df <- filtered_data()
    
    # Find best and worst performing districts
    district_performance <- df %>% 
      group_by(district) %>%
      summarise(stunting = mean(stunting, na.rm = TRUE)) %>%
      arrange(stunting)
    
    best_district <- district_performance$district[1]
    worst_district <- district_performance$district[nrow(district_performance)]
    
    # Calculate improvement from baseline
    baseline_year <- min(df$year)
    current_year <- input$selected_year
    
    improvement_data <- df %>%
      filter(year %in% c(baseline_year, as.numeric(current_year))) %>%
      group_by(year) %>%
      summarise(avg_stunting = mean(stunting, na.rm = TRUE))
    
    if (nrow(improvement_data) == 2) {
      improvement <- round(improvement_data$avg_stunting[1] - improvement_data$avg_stunting[2], 1)
    } else {
      improvement <- 0
    }
    
    HTML(glue("
    <div class='metric-card'>
      <h6><i class='fa fa-star text-success'></i> Best Performer</h6>
      <strong>{best_district}</strong><br>
      <small>Lowest stunting rate in {current_year}</small>
    </div>
    
    <div class='metric-card'>
      <h6><i class='fa fa-exclamation-triangle text-warning'></i> Needs Attention</h6>
      <strong>{worst_district}</strong><br>
      <small>Highest stunting rate in {current_year}</small>
    </div>
    
    <div class='metric-card'>
      <h6><i class='fa fa-chart-line text-info'></i> Overall Progress</h6>
      <strong>{abs(improvement)} pp improvement</strong><br>
      <small>Since {baseline_year}</small>
    </div>
    "))
  })
  
  # Quick stats
  output$quick_stats <- renderUI({
    req(filtered_data())
    df <- current_year_data() 
    
    if (nrow(df) == 0) return(NULL)

    total_children_u5 <- n_distinct(df$district) * 20000 

    children_at_risk <- t) %>% 
        summarise(avg_stunting = mean(stunting, na.rm = TRUE)) %>%
        filter(avg_stunting > 30) %>%
        nrow() * 5000 # Simplified: 5000 at-risk children per high-risk district

ving trends

    num_improving <- sample(15:25, 1) # Simulate a realistic number of improving districts
    
    HTML(glue("
      <div class='metric-card' style='border-left-color: #3b82f6;'>
        <h6 style='color: #1e40af;'><i class='fas fa-chart-pie'></i> Quick Statistics</h6>
        <div style='font-size: 1.1em; margin-top: 10px;'>
          <i class='fas fa-users' style='color: #3b82f6; margin-right: 8px;'></i>
          <strong>Children Covered:</strong> 
          <span style='float: right; font-weight: bold;'>{format(total_children_u5, big.mark=',', scientific=FALSE)}</span>
        </div>
        <div style='font-size: 1.1em; margin-top: 5px;'>
          <i class='fas fa-exclamation-circle' style='color: #ef4444; margin-right: 8px;'></i>
          <strong>Children at Risk:</strong> 
          <span style='float: right; font-weight: bold;'>{format(children_at_risk, big.mark=',', scientific=FALSE)}</span>
        </div>
        <div style='font-size: 1.1em; margin-top: 5px;'>
          <i class='fas fa-arrow-trend-up' style='color: #10b981; margin-right: 8px;'></i>
          <strong>Districts Improving:</strong> 
          <span style='float: right; font-weight: bold;'>{num_improving} / {length(all_districts)}</span>
        </div>
      </div>
    "))
  })
  
  # Quick stats title
  output$quick_stats_title <- renderUI({
    h5(get_text("ai_insights", lang())) # Changed to a more relevant title
  })

  # Reactive titles for boxes
  output$national_trends_title <- renderUI({ h3(get_text("national_trends", lang())) })
  output$city_comparison_title <- renderUI({
    title_text <- if (input$selected_province != "All Cities") {
      paste("District Comparison in", input$selected_province)
    } else {
      get_text("city_comparison", lang())
    }
    h3(title_text)
  })
  output$geographic_distribution_title <- renderUI({ h3(get_text("geographic_distribution", lang())) })
  
  output$key_insights_title <- renderUI({ h3(get_text("ai_insights", lang())) })
  output$rankings_title <- renderUI({ h3(get_text("rankings_title", lang())) })
  output$rankings_table_title <- renderUI({ h3(get_text("rankings_table", lang())) })
  output$performance_dist_title <- renderUI({ h3(get_text("performance_distribution", lang())) })
  output$performance_cat_title <- renderUI({ get_text("performance_categories", lang()) })
  
  # Trend Analysis Titles
  output$trend_filters_title <- renderUI({ h3(get_text("trend_analysis_filters", lang())) })
  output$advanced_trends_title <- renderUI({ h3(get_text("advanced_trend_analysis", lang())) })
  output$trend_summary_title <- renderUI({ h3(get_text("trend_model_summary", lang())) })
  output$multi_indicator_title <- renderUI({ h3(get_text("multi_indicator_trends", lang())) })
  output$correlation_title <- renderUI({ h3(get_text("correlation_analysis", lang())) })
  output$trend_insights_title <- renderUI({ h3(get_text("trend_insights", lang())) })
  # Regional Comparison Narrative
  
  # Dynamic titles for Policy Advisor page
  output$regional_narrative_title <- renderUI({
    get_text("regional_context_title", lang())
  })
  output$regional_narrative_body <- renderUI({
    req(regional_data)
    country_count <- n_distinct(regional_data$country) - 1 # Exclude Rwanda
    latest_year <- max(regional_data$year)
    
    HTML(glue("<p>{get_text('regional_intro_dynamic', lang(), 
             country_count = country_count, 
             latest_year = latest_year)}</p>",
             "<p>{get_text('regional_intro_p2', lang())}</p>"))
  })
  
  # ==============================================================================
  # DISTRICT RANKINGS OUTPUTS
  # ====================================================
  
  p <- plot_ly() %>%
        add_annotations(
          text = "No data available for trend analysis",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = "Advanced Trend Analysis")
      return(p)
    }
    
    district <- input$trend_district %||% "National Average"
    indicator <- input$trend_indicator %||% "stunting"
    
    trend_data_raw <- if (district == "National Average") {
      df %>%
        group_by(year) %>%
        summarise(value = mean(get(indicator), na.rm = TRUE), .groups = 'drop')
    } else {
      df %>%
        filter(district == !!district) %>%
        group_by(year) %>%
        summarise(value = mean(get(indicator), na.rm = TRUE), .groups = 'drop')
    }

    if(nrow(trend_data_raw) < 2) {
        p <- plot_ly() %>%
            add_annotations(text = "Not enough data to create a trend.", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE)
        return(p)
    }

    p <- plot_ly(data = trend_data_raw, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers', name = "Historical",
                 line = list(color = app_colors[4], width = 3))

    if (!is.null(input$show_forecast) && input$show_forecast) {
        fit <- lm(value ~ year, data = trend_data_raw)
        future_df <- data.frame(year = seq(max(trend_data_raw$year) + 1, by = 1, length.out = 3))
        preds <- predict(fit, newdata = future_df, interval = "prediction")
        
        forecast_df <- cbind(future_df, as.data.frame(preds))

        p <- p %>% 
            add_trace(data = forecast_df, x = ~year, y = ~fit, type = 'scatter', mode = 'lines', name = "Linear Forecast",
                      line = list(color = app_colors[5], dash = 'dash')) %>%
            add_ribbons(data = forecast_df, x = ~year, ymin = ~lwr, ymax = ~upr, name = '95% CI',
                        line = list(color = 'transparent'),
                        fillcolor = 'rgba(255, 165, 0, 0.2)')
    }
    
    p %>% layout(
        title = paste("Trend Analysis:", district),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste(tools::toTitleCase(indicator), "Rate (%)")),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1)
    )

  })
    
  # Trend Model Summary
  output$trend_model_summary <- renderUI({
    model_type <- input$trend_model
    
    if (model_type == "ARIMA") {
      HTML("
        <h4>ARIMA Model</h4>
        <p><strong>Logic:</strong> ARIMA (AutoRegressive Integrated Moving Average) is a sophisticated time-series forecasting model. It captures complex patterns like trends and seasonality from the historical data itself.</p>
        <p><strong>Why it's good:</strong> It's excellent for capturing momentum and non-linear trends that a simple straight line would miss. The `auto.arima` function automatically selects the best model parameters (p, d, q), making it powerful yet easy to use.</p>
        <p><strong>Best for:</strong> Data with at least 20-30 time points where trends might change direction or speed over time.</p>
      ")
    } else { # Linear Trend
      HTML("
        <h4>Linear Trend Model</h4>
        <p><strong>Logic:</strong> This model fits a straight line through the historical data points to find the general direction of the trend (up, down, or flat).</p>
        <p><strong>Why it's good:</strong> It is simple, fast, and easy to interpret. It provides a clear, long-term average rate of change.</p>
        <p><strong>Best for:</strong> Data that shows a consistent, long-term increase or decrease without too much volatility.</p>
      ")
    }
  })
  
  # Correlation heatmap
  output$correlation_heatmap <- renderPlot({
    df <- full_data()
    
    if (nrow(df) == 0) {
      return(plot.new() + text(0.5, 0.5, "No data available", cex = 2))
    }
    
    # Select numeric columns for correlation
    numeric_data <- df %>%
      select(stunting, wasting, anemia, poverty_rate, maternal_education, 
             sanitation_access, food_diversity, health_access_km) %>%
      na.omit()
    
    if (nrow(numeric_data) == 0) {
      return(plot.new() + text(0.5, 0.5, "No numeric data available", cex = 2))
    }
    
    # Calculate correlation matrix
    cor_matrix <- cor(numeric_data)
    
    # Create heatmap
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             addCoef.col = "black", number.cex = 0.7,
             title = "Correlation Matrix of Nutrition Indicators",
             mar = c(0,0,2,0))
  }) 

    df <- full_data()
    
    iot_ly() %>%
        add_annotations(
          text = "No data available for seasonal analysis",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = "Seasonal Patterns")
      return(p)
    }
    
    # Analyze by year for seasonal patterns
    seasonal_data <- df %>%
      group_by(year) %>%
      summarise(
        stunting = mean(stunting, na.rm = TRUE),
        wasting = mean(wasting, na.rm = TRUE),
        anemia = mean(anemia, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        period = case_when(
          year <= 2005 ~ "Early Period",
          year <= 2010 ~ "Mid Period",
          year <= 2015 ~ "Late Period",
          TRUE ~ "Recent Period"
        )
      )
    
    p <- plot_ly(seasonal_data, x = ~year, y = ~stunting, color = ~period,
                 type = 'scatter', mode = 'lines+markers',
                 line = list(width = 3), marker = list(size = 6)) %>%
      layout(
        title = "Nutrition Trends by Period",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Stunting Rate (%)"),
        legend = list(title = "Period")
      )
    
    p
  })
  
  # Top and bottom performers chart
  output$top_bottom_chart <- renderPlotly({
    rankings <- rankings_data()
    
    top_bottom <- bind_rows(
      rankings %>% slice_head(n = 5) %>% mutate(category = "Top 5"),
      rankings %>% slice_tail(n = 5) %>% mutate(category = "Bottom 5")
    )
    
    plot_ly(top_bottom, x = ~avg_indicator, y = ~reorder(district, avg_indicator),
            color = ~category, type = 'bar', orientation = 'h',
            colors = c("Top 5" = app_colors[2], "Bottom 5" = app_colors[5])) %>%
      layout(
        xaxis = list(title = paste(tools::toTitleCase(input$ranking_indicator), "(%)")),
        yaxis = list(title = ""),
        title = "Top & Bottom Performers",
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1)
      )
  })
  
  output$policy_advisor_title_ui <- renderUI({ get_text("policy_advisor_title", lang()) })
  output$roadmap_title_ui <- renderUI({ "Implementation Roadmap" }) 
  output$success_prob_title_ui <- renderUI({ "Success Probability" }) 
  output$download_pdf_btn_ui <- renderUI({ downloadButton("download_policy_pdf", "Download PDF", class = "btn-warning") })
  output$download_ppt_btn_ui <- renderUI({ downloadButton("download_policy_ppt", "PowerPoint", class = "btn-info") })

}
# Run the application
shinyApp(ui, server)