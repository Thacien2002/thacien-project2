# ==============================================================================
#LIBRARIES & SETUP # nolint
# ==============================================================================
library(here)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(plotly)
library(leaflet)
library(glue)
library(data.table)
library(DT)
library(rlang)
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
library(sf)
library(fresh)
library(bslib)
library(httr)
library(jsonlite)
library(future)

# Define missing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
# Utility scripts (like gemini_utils.R and nisr_theme.R) are now sourced
# from run_dashboard.R before the app is launched.

# ==============================================================================
# 🔹 BILINGUAL TRANSLATIONS (Kinyarwanda & English)
# ==============================================================================

# Translation function (supports interpolation via named ... args)
get_text <- function(key, lang = "rw", ...) {
  txt <- translations[[lang]][[key]] %||% key
  dots <- list(...)
  if (length(dots) > 0) {
    dot_names <- names(dots)
    named_idx <- which(!is.null(dot_names) & nzchar(dot_names))
    if (length(named_idx) > 0) {
      safe_dots <- dots[named_idx]
      env <- list2env(safe_dots, parent = emptyenv())
      tryCatch({
        return(as.character(glue::glue(txt, .envir = env)))
      }, error = function(e) {
        return(txt)
      })
    }
  }
  return(txt)
}
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
    district_rankings = "Urutonde rw'Uturere",
    regional_comparison = "Ubugereranye n'Akarere",
    trend_analysis = "Isesengura ry'Icyerekezo",
    predictive_analytics = "Isesengura Riteganya",
    root_cause_analysis = "Isesengura ry'Imizi y'Ibibazo",
    policy_advisor = "Umujyanama mu by'Ingamba",
    data_explorer = "Gushakisha amakuru",
    solutions_hub = "Igisubizo cy'Ibitekerezo",
    impact_assessment = "Isuzuma ry'Ingaruka",
    data_quality = "Ubuziranenge bw'Amakuru",
    export_reports = "Gutanga raporo",
    
    # Common terms
    stunting = "Gutinda kukura",
    wasting = "Guhira", 
    anemia = "Kugira amaraso make",
    poverty = "Ubukene",
    malnutrition = "imirire mibi",
    nutrition = "Imirire/Indyo",
    health = "Ubuzima",
    children = "Abana",
    women = "Abagore",
    
    # Actions
    download = "Gukurura",
    generate = "Gukora",
    update = "Kuvugurura",
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
    age_group = "Urwego   rw'amavuko",
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
    available = "Birashoboka",
    ready = "Biriteguye",
    configure = "Gushiraho"
    ,
    rankings_title = "District Rankings Filters",
    indicator = "Indicator",
    order = "Order",
    best_to_worst = "Best to Worst",
    worst_to_best = "Worst to Best",
    update_rankings = "Update Rankings",
    export_rankings = "Export Rankings",
    top_bottom_performers = "Top & Bottom Performers",
    national_intelligence = "Ubushishozi ku Mirire mu Rwanda",
    national_trends = "Imiterere y'Igihugu (DHS)",
    city_comparison = "Ugereranyije bw'Umujyi",
    geographic_distribution = "Ikibanza cy'ubutaka",
    rankings_table = "Imbonerahamwe y'urutonde",
    performance_distribution = "Ikwirakwizwa ry'imikorere",
    performance_categories = "Ibyiciro by'imikorere",
    narrative_intro = "Mu {input_selected_year}, {province_name} ifite impuzandengo y'igipimo cy'igwingira cya <strong>{avg_stunting}%</strong> mu turere <strong>{total_districts}</strong>.",
    narrative_at_risk = "Kugeza ubu, uturere <strong>{at_risk_districts_count}</strong> turagaragaza ibipimo biteye inkeke biri hejuru ya 30%.",
    narrative_trend_improving = "Icyerekezo <strong style='color: {trend_color};'>kigenda neza</strong>, hamwe n'{change_text}.",
    narrative_trend_concerning = "Icyerekezo <strong style='color: {trend_color};'>giteye inkeke</strong>, hamwe n'{change_text}.",
    narrative_trend_stable = "Icyerekezo <strong style='color: {trend_color};'>ntigihinduka</strong>, kubera {change_text}.",
    narrative_change_text = "ihinduka rya {abs_annual_change} ku ijana ugereranyije na {previous_year}",
    narrative_no_trend = "nta makuru ahari y'icyerekezo giheruka",
    national_stunting_rate = "Igipimo cy'Igihugu cy'Igwingira",
    regional_context_title = "Imiterere y'Akarere",
    regional_intro_p1 = "Ikaze ku ipaji y'Ubugereranye bw'Akarere. Kugira ngo tumenye neza imiterere y'imirire mu Rwanda, ni ngombwa kureba uko duhagaze mu karere. Iki gice kigereranya imikorere y'u Rwanda n'ibihugu duturanye byo mu Burasirazuba bwa Afurika: Uganda, Kenya, Tanzaniya, Uburundi, na DRC.",
    regional_context_body = "Gereranya imikorere y'u Rwanda n'ibihugu duturanye kugira ngo umenye uko duhagaze mu karere.",
    regional_intro_dynamic = "Iki gice gisesengura uko u Rwanda ruhagaze ugereranyije n'ibindi bihugu <strong>{country_count}</strong> byo mu karere, hakoreshejwe amakuru agezweho yo mu <strong>{latest_year}</strong>. Koresha ibishushanyo bikurikira kugira ngo usesengure uko ibipimo by'ingenzi byagiye bihinduka mu myaka ishize.",
    root_cause_analysis = "Isesengura ry'Imizi y'Ibibazo",
    regional_intro_p2 = "Tugereranya ibipimo by'ingenzi nk'igwingira n'ikibazo cyo kunanuka, dushobora kumenya ingorane dusangiye, kugaragaza aho twatsinze, no kuvumbura amahirwe y'ubufatanye bwambukiranya imipaka. Koresha ibishushanyo biri hepfo kugira ngo usuzume imiterere y'igihe kirekire kandi urebe uko u Rwanda ruhagaze mu mwaka uheruka.",
    root_cause_title = "Isesengura ry'Imizi y'Ibibazo",
    root_cause_intro = "Iki gice kigufasha gusobanukirwa imizi y'ibibazo by'imirire mibi ukoresheje ibishushanyo by'isano. Hitamo igipimo cy'ibanze n'ibindi bintu kugira ngo ubone isano riri hagati yabyo."
    ,
    # District Rankings
    best_performer = "Uwitwaye neza kurusha abandi",
    needs_attention = "Bisaba kwitabwaho",
    national_average = "Impuzandengo y'Igihugu",
    performance_over_time = "Imikorere uko ihinduka",
    # Regional Comparison
    regional_trends_comparison = "Ubugereranye bw'Ibyerekezo mu Karere",
    rwanda_regional_rank = "Umwanya w'u Rwanda mu Karere",
    progress_metrics = "Ibipimo by'Iterambere",
    country_comparison_dashboard = "Ikarita yo kugereranya Ibihugu",
    select_indicator = "Hitamo Igipimo:",
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
    chronic_malnutrition = "Imirire mibi idakira",
    acute_malnutrition = "Imirire mibi ikabije",
    iron_deficiency = "Kubura ubutare",
    economic_indicator = "Igipimo cy'ubukungu",
    impact_narrative_goal = "Intego y'iyi karita si ukugaragaza amakuru gusa, ahubwo ni ugutera imbaraga ibikorwa bigamije kurandura inzara yihishe mu Rwanda. Ipaji y'Isuzuma ry'Ingaruka niho dupimira ingaruka z'ingamba zacu z'imirire.",
    impact_narrative_how_title = "Uko Bifasha Kurandura Inzara Yihishe",
    impact_narrative_decision_making = "Gufata Ibyemezo Bishingiye ku Bimenyetso: Bitanga ibimenyetso bifatika ku byerekeye porogaramu zitanga umusaruro mwiza, byemeza ko umutungo ushorwa mu ngamba zikora.",
    impact_narrative_accountability = "Kugenzura no Gusobanuza: Mu gukurikirana iterambere ugereranyije n'intego (nk'iza SDGs), bituma ababigiramo uruhare babazwa ibyo bakora kandi bikagaragaza inyungu zifatika z'ishoramari mu mirire ku baturage n'abafatanyabikorwa.",
    impact_narrative_optimization = "Kunoza Porogaramu: Bifasha kumenya impamvu ibikorwa bimwe na bimwe bigerwaho cyangwa bigatsindwa, bigatuma habaho kunoza no guhindura ingamba kugira ngo ingaruka zabyo ku igwingira, kunanuka, na anemiya zibe nyinshi.",
    interpreting_phases_title = "Gusobanukirwa Ibyiciro",
    phase1_desc = "Icyiciro cya 1 (Imyaka y'Ibanze): Cyibanze ku gushyiraho serivisi z'ibanze z'ubuzima n'imirire nyuma ya jenoside.",
    phase2_desc = "Icyiciro cya 2 (Igihe cy'Iterambere): Cyaranzwe na porogaramu z'igihugu zongerewe ubushobozi, zihuriweho, n'ubwitange bukomeye bwa leta, biganisha ku ntera ishimishije.",
    phase3_desc = "Icyiciro cya 3 (Ingamba zigezweho): Gihagarariye igihe cya none cy'ibikorwa bishingiye ku makuru, byibanda ku gukemura ibibazo by'imirire mibi bikigaragara."
    ,
    quick_stats = "Imibare y'ingenzi",
    impact_summary_title = "Incukumbuzi y'Ingaruka",
    intervention_effectiveness = "Ingaruka z'ibyakozwe",
    sdg_progress = "Iterambere ry'intego z'ikinyagihumbi",
    resource_allocation = "Ikwirakwizwa ry'umutungo",
    predictive_analytics_config = "Gushyiraho Iteganyamikorere",
    forecast_horizon = "Imyaka yo guteganya",
    run_forecast = "Kora iteganyamikorere",
    intervention_scenarios = "Ibikorwa biteganyijwe",
    intervention_impact = "Ingaruka z'igikorwa (% igabanuka)",
    forecast_chart_title = "Igishushanyo cy'Iteganyamikorere",
    risk_assessment_title = "Isesengura ry'ibyago",
    roi_analysis_title = "Isesengura ry'inyungu",
    forecasted_value = "Agaciro gateganyijwe",
    risk_districts = "Uturere turi mu byago",
    roi_metric = "Igipimo cy'inyungu",
    data_explorer_title = "Isesengura ryimbitse ry'amakuru",
    apply_filters = "Shyiraho ibyahinduwe"
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
    available_budget = "4. Ingengo y'Imari Ihari (USD):",
    
    # Solutions Hub
    solutions_hub_title = "Igisubizo cy'Ibitekerezo: Kurandura Inzara Yihishe mu Rwanda",
    solutions_hub_intro = "Iki gicumbi gitanga ibisubizo bishya, bishingiye ku makuru, byateguriwe u Rwanda mu kurwanya imirire mibi n'inzara yihishe. Ibi bitekerezo bishobora kuvamo gahunda zuzuye.",
    solution1_title = "Igitekerezo cya 1: Urubuga rwa mHealth rw'Imirire",
    solution1_concept = "Igitekerezo: Porogaramu ya telefoni igendanwa y'Abajyanama b'Ubuzima (CHW) n'ababyeyi.",
    solution1_chw = "Ku Bajyanama b'Ubuzima: Gukurikirana imikurire mu buryo bwa elegitoroniki, kwinjiza amakuru mu gihe nyacyo, n'ubutumwa bwikora ku bana bari mu kaga.",
    solution1_mothers = "Ku Babyeyi: Inama zihariye z'imirire (mu Kinyarwanda), ubutumwa bwibutsa igihe cyo kugaburira, no kubona amakuru y'ubuzima.",
    solution1_backend = "Icyuma cy'Amakuru: Ikarita nk'iyi ku bafata ibyemezo kugira ngo babone amakuru nyayo ava mu baturage.",
    solution1_impact_title = "Ingaruka:",
    solution1_impact_text = "Byongera cyane ubushobozi bwo kumenya hakiri kare, bitanga ubufasha bwihuse ku miryango, kandi biha abafata ibyemezo ishusho nyayo y'imirire mu gihugu.",
    solution2_title = "Igitekerezo cya 2: Sisitemu y'Uruhererekane rw'Ibiribwa n'Imirire",
    solution2_concept = "Igitekerezo: Sisitemu ihuza abahinzi b'ibihingwa byongerewe intungamubiri n'amasoko y'ibanze n'amashuri.",
    solution2_farmer = "Guhuza Abahinzi: Sisitemu ishingiye kuri SMS yo kwandika abahinzi b'ibishyimbo byongerewe ubutare, ibijumba bya Vitamine A, n'ibindi.",
    solution2_market = "Guhuza n'Isoko: Ihuza abahinzi banditse n'abacuruzi b'ibanze, gahunda zo kugaburira ku mashuri, n'amavuriro.",
    solution2_trace = "Gukurikirana: Gukoresha QR code mu gukurikirana umusaruro kuva ku murima kugera ku muguzi, byemeza ubuziranenge no gutanga amakuru ku ntungamubiri.",
    solution2_impact_text = "Bizamura ubukungu bw'ibanze, byongera kubona ibiribwa by'intungamubiri, kandi bigashyiraho sisitemu y'ibiribwa irambye ishingiye ku makuru mu kurwanya kubura intungamubiri z'ingenzi.",
    solution3_title = "Igitekerezo cya 3: Ikarita Iteguza Ahari Ibyago Ikoresheje Amakuru y'Ikirere",
    solution3_concept = "Igitekerezo: Sisitemu yo gutanga umuburo hakiri kare iteguza ahazaba ibyago by'imirire mibi.",
    solution3_integration = "Iyi sisitemu yakomatanya amakuru y'imirire (nk'ari iyi karita) n'andi masoko:",
    solution3_climate = "Amakuru y'Ikirere: Amakuru ya satelite y'imvura n'ibimera mu guteguza amapfa cyangwa umusaruro muke.",
    solution3_market = "Amakuru y'Isoko: Ibiciro by'ibiribwa by'ibanze mu kumenya ahari ukubura kw'ibiribwa.",
    solution3_ai = "Icyitegererezo cya AI: Icyitegererezo cy'imashini yiga mu guteguza uturere tuzaba mu kaga kenshi k'ibura ry'ibiribwa mu mezi 3-6 mbere.",
    solution3_impact_text = "Biha leta n'imiryango itegamiye kuri leta ubushobozi bwo gutanga ubufasha mu buryo buteganijwe, gutangiza gahunda z'ubufasha zihariye, no gukumira ibibazo by'imirire mibi bitaraba.",
    policy_brief_title = "Incamake y'Ingamba Zatewe n'Ubwenge bw'ubukorano",
    roadmap_title = "Icyerekezo cyo Gushyira mu Bikorwa",
    success_prob_title = "Amahirwe yo gutsinda",
    download_pdf = "Manura PDF",
    download_ppt = "Manura PowerPoint",

    # Engaging Docs Page
    engaging_docs_title = "Inyandiko Zishishikaje & Ziteguye",
    docs_evidence_title = "Inkomoko y'Amakuru & Ibimenyetso",
    docs_evidence_body = "Iyi karita yerekana amakuru y'imirire mu Rwanda ikoresheje amakuru ava mu bigo byizewe by'igihugu n'amahanga.",
    docs_source_nisr = "NISR (Ikigo cy'Igihugu gishinzwe Ibarurishamibare) gitanga amakuru y'ibanze ku mirire y'abana.",
    docs_source_unicef = "UNICEF Rwanda itanga ubushishozi ku mirire mibi n'ingaruka zayo mu turere.",
    docs_source_gnr = "Global Nutrition Report itanga intego z'igihugu zo kugabanya imirire mibi.",
    docs_source_media = "Amashusho atanga ibisobanuro byoroshye ku bikorwa by'imirire.",
    key_figures_title = "Imibare y'Ingenzi Muri Iyi Karita",
    key_figure_stunting = "~33% by'abana bari munsi y'imyaka itanu bafite igwingira mu Rwanda.",
    key_figure_anemia = "~37% by'abana bafite amezi 6-59 bafite anemiya (bitandukana n'imyaka).",
    key_figure_diet = "Gusa ~22% by'abana bafite amezi 6-23 nibo babona indyo yuzuye.",
    limitations_title = "Ibintu byo Kuzirikana",
    limitation_refresh = "Amakuru avugururwa buri myaka mike; DHS iheruka ni iya 2019-20.",
    limitation_hidden_hunger = "Inzara yihishe (kubura intungamubiri) iragoye kuyipima kandi ishobora kuba idahagarariwe neza mu makuru.",
    way_forward_title = "Icyerekezo cy'Ahazaza",
    way_forward_real_time = "Guhuza amakuru y'igihe nyacyo (urugero: amakuru ya serivisi z'imirire ya buri gihembwe) kugira ngo karita ivugururwe vuba.",
    way_forward_predictive = "Gukoresha isesengura riteganya mu kumenya uturere turi mu kaga kugira ngo ibikorwa byihutirwe.",
    way_forward_equity = "Gushimangira ishusho y'ubusumbane (mu bukungu, mu cyaro/mu mujyi, igitsina) kugira ngo ibikorwa bigenwe neza.",
    multimedia_title = "Amashusho & Raporo"
    ,
    footer_text = "Iyi karita yakozwe na Thacien. Amakuru yakusanyijwe ava mu bigo bikurikira:"
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
    root_cause_analysis = "Root Cause Analysis",
    policy_advisor = "Policy Advisor",
    data_explorer = "Data Explorer",
    solutions_hub = "Solutions Hub",
    impact_assessment = "Impact Assessment",
    data_quality = "Data Quality",
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
    province = "Province", 
    all_provinces = "All Provinces",
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
    available = "Available",
    ready = "Ready",
    configure = "Configure"
    ,
    rankings_title = "District Rankings Filters",
    indicator = "Indicator",
    order = "Order",
    best_to_worst = "Best to Worst",
    worst_to_best = "Worst to Best",
    update_rankings = "Update Rankings",
    export_rankings = "Export Rankings",
    top_bottom_performers = "Top & Bottom Performers",
    national_intelligence = "Rwanda National Nutrition Intelligence",
    national_trends = "National Trends (DHS Data)",
    city_comparison = "Province Comparison",
    geographic_distribution = "Geographic Distribution",
    rankings_table = "Rankings Table",
    performance_distribution = "Performance Distribution",
    performance_categories = "Performance Categories",
    narrative_intro = "In {input_selected_year}, {province_name} has an average stunting rate of <strong>{avg_stunting}%</strong> across <strong>{total_districts}</strong> districts.",
    narrative_at_risk = "Currently, <strong>{at_risk_districts_count}</strong> districts show concerning levels above 30%.",
    narrative_trend_improving = "The trend is <strong style='color: {trend_color};'>improving</strong>, with {change_text}.",
    narrative_trend_concerning = "The trend is <strong style='color: {trend_color};'>concerning</strong>, with {change_text}.",
    narrative_trend_stable = "The trend is <strong style='color: {trend_color};'>stable</strong>, with {change_text}.",
    narrative_change_text = "a {abs_annual_change} percentage point change from {previous_year}",
    narrative_no_trend = "no recent trend data available",
    national_stunting_rate = "National Stunting Rate",
    root_cause_analysis = "Root Cause Analysis",
    regional_context_title = "Regional Context",
    regional_intro_p1 = "Welcome to the Regional Comparison page. Understanding Rwanda's nutritional landscape requires a regional perspective. This section benchmarks Rwanda's performance against its East African neighbors: Uganda, Kenya, Tanzania, Burundi, and the DRC.",
    regional_intro_p2 = "By comparing key indicators like Stunting and Wasting, we can identify shared challenges, highlight areas of success, and uncover opportunities for cross-border collaboration. Use the charts below to explore long-term trends and see how Rwanda ranks in the latest available year.",
    root_cause_title = "Root Cause Analysis",
    root_cause_intro = "This section helps you understand the underlying drivers of malnutrition through relationship analysis. Select a primary indicator and other factors to explore their correlation."
    ,
    # District Rankings
    best_performer = "Best Performer",
    needs_attention = "Needs Attention",
    national_average = "National Average",
    performance_over_time = "Performance Over Time",
    # Regional Comparison
    regional_trends_comparison = "Regional Trends Comparison",
    rwanda_regional_rank = "Rwanda's Regional Rank",
    progress_metrics = "Progress Metrics",
    country_comparison_dashboard = "Country Comparison Dashboard",
    select_indicator = "Select Indicator:",
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
    program_efficiency = "Program efficiency",
    chronic_malnutrition = "Chronic Malnutrition",
    acute_malnutrition = "Acute Malnutrition",
    iron_deficiency = "Iron Deficiency",
    economic_indicator = "Economic Indicator"
    ,
    impact_narrative_goal = "The goal of this dashboard is not just to present data, but to drive action towards ending hidden hunger in Rwanda. The Impact Assessment page is where we measure the effectiveness of our nutrition strategies and interventions.",
    impact_narrative_how_title = "How It Helps End Hidden Hunger",
    impact_narrative_decision_making = "Evidence-Based Decision Making: It provides concrete evidence on which programs deliver the best results, ensuring that resources are invested in strategies that work.",
    impact_narrative_accountability = "Accountability and Transparency: By tracking progress against targets (like the SDGs), it holds stakeholders accountable and demonstrates the tangible benefits of nutrition investments to the public and partners.",
    impact_narrative_optimization = "Program Optimization: It helps identify why certain interventions succeed or fail, allowing for continuous improvement and adaptation of strategies to maximize their impact on stunting, wasting, and anemia.",
    interpreting_phases_title = "Interpreting the Phases",
    phase1_desc = "Phase 1 (Foundational Years): Focused on establishing basic health and nutrition services post-conflict.",
    phase2_desc = "Phase 2 (Acceleration Period): Marked by scaled-up, integrated national programs and strong government commitment, leading to significant gains.",
    phase3_desc = "Phase 3 (Modern Strategy): Represents the current era of data-driven, targeted interventions aimed at tackling persistent pockets of malnutrition.",
    quick_stats = "Quick Stats",
    impact_summary_title = "Impact Summary",
    intervention_effectiveness = "Intervention Effectiveness",
    sdg_progress = "SDG Progress",
    resource_allocation = "Resource Allocation",
    predictive_analytics_config = "Prediction & Scenario Configuration",
    forecast_horizon = "Forecast Horizon (Years)",
    run_forecast = "Generate Forecast",
    intervention_scenarios = "Intervention Scenarios",
    intervention_impact = "Intervention Impact (% reduction)",
    forecast_chart_title = "Forecast & Intervention Scenario Analysis",
    risk_assessment_title = "Scenario Impact & Cost-Benefit Analysis",
    roi_analysis_title = "ROI Analysis",
    forecasted_value = "Forecasted Value",
    risk_districts = "High-Risk Districts",
    roi_metric = "ROI Metric",
    data_explorer_title = "Advanced Data Explorer",
    apply_filters = "Apply Filters",

    policy_advisor_title = "AI Policy Advisor",
    policy_intro = "Generate evidence-based policy recommendations using AI analysis of DHS data and regional best practices.",
    focus_area = "1. Focus Area:",
    stunting_reduction_policy = "Stunting Reduction",
    wasting_prevention_policy = "Wasting Prevention",
    anemia_control_policy = "Anemia Control",
    integrated_nutrition_policy = "Integrated Nutrition",
    emergency_response_policy = "Emergency Response",
    target = "2. Target:",
    national_target = "National", # Corrected: Made this dynamic
    city_specific_target = "Province Specific",
    district_specific_target = "District Specific",
    implementation_timeline = "3. Implementation Timeline (months):",
    available_budget = "4. Available Budget (USD):",
    
    # Solutions Hub
    solutions_hub_title = "Solutions Hub: Ending Hidden Hunger in Rwanda",
    solutions_hub_intro = "This hub proposes innovative, data-driven solutions tailored to Rwanda's context to combat malnutrition and hidden hunger. These ideas can be developed into full-fledged systems.",
    solution1_title = "Idea 1: Mobile Health (mHealth) Nutrition Platform",
    solution1_concept = "Concept: A mobile app for Community Health Workers (CHWs) and mothers.",
    solution1_chw = "For CHWs: Digital growth monitoring, real-time data entry, and automated alerts for at-risk children.",
    solution1_mothers = "For Mothers: Personalized nutrition advice (in Kinyarwanda), feeding reminders, and access to health information.",
    solution1_backend = "Data Backend: Centralized dashboard (like this one) for policymakers to see real-time, granular data from the field.",
    solution1_impact_title = "Impact:",
    solution1_impact_text = "Dramatically improves early detection, provides timely support to families, and gives policymakers a live view of nutrition status across the country.",
    solution2_title = "Idea 2: Agri-Nutrition Supply Chain System",
    solution2_concept = "Concept: A system linking farmers of bio-fortified crops to local markets and schools.",
    solution2_farmer = "Farmer Link: SMS-based system to register farmers growing iron-rich beans, vitamin-A-rich sweet potatoes, etc.",
    solution2_market = "Market Link: Connects registered farmers to local vendors, school feeding programs, and clinics.",
    solution2_trace = "Traceability: QR codes to track produce from farm to consumer, ensuring quality and providing nutritional information.",
    solution2_impact_text = "Boosts local economies, increases access to nutritious food, and creates a sustainable, data-backed food system to fight micronutrient deficiencies.",
    solution3_title = "Idea 3: Predictive Hotspot Mapping with Climate Data",
    solution3_concept = "Concept: An early-warning system that predicts malnutrition hotspots.",
    solution3_integration = "This system would integrate nutritional data (like in this dashboard) with other sources:",
    solution3_climate = "Climate Data: Satellite rainfall and vegetation data to predict droughts or poor harvests.",
    solution3_market = "Market Data: Local food prices to identify areas with low food access.",
    solution3_ai = "AI Model: A machine learning model to forecast which districts will be at highest risk of food insecurity 3-6 months in advance.",
    solution3_impact_text = "Allows the government and NGOs to proactively allocate resources, launch targeted support programs, and prevent malnutrition crises before they happen.",
    policy_brief_title = "AI-Generated Policy Brief",
    roadmap_title = "Implementation Roadmap",
    success_prob_title = "Success Probability",
    download_pdf = "Download PDF",
    download_ppt = "Download PowerPoint",

    # Engaging Docs Page
    engaging_docs_title = "Engaging & Action Orientated",
    docs_evidence_title = "Data Sources & Evidence",
    docs_evidence_body = "This dashboard visualizes Rwanda's nutrition landscape using data from trusted national and international sources.",
    docs_source_nisr = "NISR (National Institute of Statistics of Rwanda) provides the core data on child nutrition.",
    docs_source_unicef = "UNICEF Rwanda offers key insights into chronic malnutrition and its impact at the sub-national level.",
    docs_source_gnr = "The Global Nutrition Report provides national targets for malnutrition reduction.",
    docs_source_media = "Multimedia resources offer accessible explanations of key nutrition interventions.",
    key_figures_title = "Key Figures in This Dashboard",
    key_figure_stunting = "~33% of children under five are stunted (short for age) in Rwanda.",
    key_figure_anemia = "~37% of children aged 6â€“59 months have anemia (varying by age group).",
    key_figure_diet = "Only about 22% of children aged 6â€“23 months receive a minimum acceptable diet in terms of diversity and frequency.",
    limitations_title = "Limitations to Keep in Mind",
    limitation_refresh = "Data are refreshed every few years; the most recent full DHS is 2019-20. Some sub-district data may have higher uncertainty.",
    limitation_hidden_hunger = "Hidden hunger (micronutrient deficiencies) is less visible and harder to measure; it may be under-represented in data.",
    way_forward_title = "Way Forward â€“ Future Development",
    way_forward_real_time = "Integrate more real-time or frequent monitoring (e.g., quarterly nutrition service data) to enable more timely dashboard updates.",
    way_forward_predictive = "Introduce predictive analytics to proactively target interventions toward at-risk districts.",
    way_forward_equity = "Strengthen visualisation of equity gaps (by wealth quintile, rural vs urban, gender) so that interventions can be more precisely directed.",
    multimedia_title = "Multimedia & Reports",
    full_docs_links_title = "Link to Full Documentation and Videos",

    footer_text = "This dashboard was developed by Thacien. Data sourced from:"
  )
)

# ==============================================================================
# 🔹 LOAD REAL RWANDA NUTRITION DATA
# ==============================================================================
# Load Rwanda districts shapefile
tryCatch({
  rwanda_districts_sf <- st_read(here::here("thacien project", "R project", "project", "data", "RWA_adm", "RWA_adm2.shp"), quiet = TRUE)
  cat("✅ Shapefile loaded successfully!\n")
}, error = function(e) {
  cat("❌ Error loading shapefile. Map will not have borders. Error: ", e$message, "\n")
  rwanda_districts_sf <- NULL
})

# Create regional comparison data (East African countries)
create_regional_data <- function() {
  countries <- c("Rwanda", "Uganda", "Kenya", "Tanzania", "Burundi", "DRC")
  years <- seq(2000, 2020, by = 5)
  
  set.seed(456) # for reproducibility
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

load_real_data <- function() {
  # Define file path using here() for robustness
  file_path <- here::here("thacien project", "R project", "project", "data", "nutrition_data.csv")
  
  tryCatch({
    cat("🔄 Attempting to load real nutrition data from:", file_path, "\n")
    
    # Read the CSV
    data <- read_csv(file_path, show_col_types = FALSE)
    
    if (nrow(data) > 0) {
      cat("✅ Real nutrition data loaded successfully.\n")
      return(data)
    } else {
      stop("Loaded data file is empty.")
    }
  }, error = function(e) {
    cat("❌ Error loading real data:", e$message, "\n")
    return(NULL) # Return NULL on failure
  })
}

generate_synthetic_data <- function() {
  tryCatch({
    cat("🔄 Generating 6000-row synthetic data...\n")
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
      left_join(district_province_map, by = "district") %>%
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

# --- DATA LOADING LOGIC ---
# Force the use of the 6000-row synthetic dataset as requested.
full_data <- generate_synthetic_data()

regional_data <- create_regional_data()

# Validate loaded data
if (is.null(full_data) || nrow(full_data) == 0) {
  stop("❌ FATAL ERROR: Data loading failed. The 'full_data' object is empty.")
} else {
  cat("📊 Data Validation Passed:\n")
  cat("   - Rows:", nrow(full_data), "\n")
  cat("   - Year range:", min(full_data$year), "-", max(full_data$year), "\n")
  cat("   - Districts:", length(unique(full_data$district)), "\n")
  cat("   - Provinces:", length(unique(full_data$province)), "\n")
}


# ==============================================================================
# 🔹 ENHANCED UI WITH NEW FEATURES
# ==============================================================================
library(shinydashboardPlus)
ui <- shinydashboardPlus::dashboardPage(
    title = "Rwanda Nutrition Inteligence Dashboard",
    header = uiOutput("dynamic_header"),
    
    sidebar = dashboardSidebar(
      minified = TRUE, collapsed = FALSE,
      uiOutput("sidebar_menu"),
      
      # Global Filters
      hr(),
      uiOutput("global_filters_title"),
      uiOutput("global_filters")
    ),
   footer = shinydashboardPlus::dashboardFooter(
    left = uiOutput("dashboard_footer_left")
  
   ),
    skin = "blue",
    body = dashboardBody(
      tags$head(tags$style(HTML(
        "body {
          background-image: linear-gradient(rgba(245, 245, 245, 0.92), rgba(245, 245, 245, 0.97)), url('https://images.unsplash.com/photo-1594822880522-520555903b30?q=80&w=2070&auto=format&fit=crop');
          background-size: cover;
          background-position: center center;
          background-repeat: no-repeat;
        }
      .box { 
        border-top: 3px solid #2563eb; 
        box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
        border-radius: 8px;
      }
        .info-box .info-box-number { 
          font-size: 24px; /* Increased for better readability */
          font-weight: 700;
        }
        .info-box .info-box-text { 
            font-size: 12px; /* Increased title size */
            font-weight: 600; /* Bolder title */
            text-transform: uppercase;
        }
        .info-box .progress-description, 
        .info-box .info-box-text + .info-box-number + .progress + .progress-description {
            font-size: 10px; /* Increased from 4px for readability */
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
          min-height: 115px; /* Ensure consistent height for metric cards */
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
        
        /* Professional Footer Styling */
        .main-footer {
            background: #f8f9fa;
            padding: 20px;
            color: #495057;
            border-top: 1px solid #dee2e6;
            display: flex;
            justify-content: space-between;
            align-items: center;
            flex-wrap: wrap;
        }
        .footer-section {
            text-align: center;
            margin: 10px;
        }
        .footer-developer-info {
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .footer-developer-info img {
            width: 50px;
            height: 50px;
            border-radius: 50%;
            margin-right: 15px;
            border: 2px solid #005CAB;
        }
      "))),
      # Add meta tags to suppress warnings
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = "Rwanda Nutrition Dashboard - Real Data Visualization")
    ),
    
    tabItems(
      # =====NATIONAL OVERVIEW =====
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
            title = uiOutput("national_trends_title"), width = 8, status = "primary", solidHeader = TRUE, # This was already correct
            withSpinner(plotlyOutput("national_trends_chart", height = "400px")),
            uiOutput("download_trends_btn_ui")
          ),
          box(
            title = uiOutput("city_comparison_title"), width = 4, status = "info", solidHeader = TRUE, # This was already correct
            withSpinner(plotlyOutput("provincial_comparison", height = "400px")),
            footer = "Click on a bar to drill down into districts."
          )
        ),
        fluidRow(
          box(
            title = uiOutput("geographic_distribution_title"), width = 8, status = "warning", solidHeader = TRUE,
            # Add the indicator selection dropdown directly above the map
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
          div(class = "narrative-box", uiOutput("rankings_narrative")) # This is now more detailed
        ),
        fluidRow(
          infoBoxOutput("best_performer_box", width = 4),
          infoBoxOutput("worst_performer_box", width = 4),
          infoBoxOutput("average_rate_box", width = 4)
        ),
        fluidRow( # Main control and visualization box
          box(
            title = uiOutput("rankings_title"), width = 12, status = "primary", solidHeader = TRUE,
            column(4, uiOutput("rankings_filters_ui")), # Indicator
            column(4, uiOutput("rankings_province_filter_ui")), # Province Filter
            column(4, uiOutput("rankings_district_filter_ui")), # District Filter
            hr(),
            fluidRow(
              # Card for Top & Bottom Performers Chart
              box(title = uiOutput("top_bottom_title"),
                  status = "primary", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("top_bottom_chart", height = "400px")),
                  footer = "This chart displays the 5 best and 5 worst-performing districts for the selected indicator."),
              # Card for Performance Gap Analysis
              box(title = uiOutput("perf_gap_title_ui"),
                  status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(uiOutput("top_bottom_narrative_output", height = "400px")))
            )
          )
        ),
        fluidRow(
          box( # New map for rankings page
            title = uiOutput("rankings_map_title"), 
            width = 12, 
            status = "warning", 
            solidHeader = TRUE,
            withSpinner(leafletOutput("rankings_map", height = "500px"))
          )
        ),
        fluidRow(
          box(
            title = uiOutput("rankings_table_title"), width = 12, status = "info", solidHeader = TRUE, # Table now has sparklines
            withSpinner(reactableOutput("rankings_table")),
            br(), uiOutput("export_rankings_btn_ui")
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
            withSpinner(plotlyOutput("advanced_trends", height = "450px")),
            hr(),
            uiOutput("multi_indicator_trends_ui") # Added for multi-indicator trends
          ),
          box(
            title = uiOutput("trend_summary_title"), width = 4, status = "info", solidHeader = TRUE,
            withSpinner(uiOutput("trend_model_summary")),
            hr(),
            uiOutput("trend_insights_title"),
            withSpinner(uiOutput("trend_insights"))
          )
        ),
        fluidRow(
          box(
            title = uiOutput("correlation_title"), width = 6, status = "info", solidHeader = TRUE,
            withSpinner(plotOutput("correlation_heatmap", height = "350px"))
          ),
          box(
            title = uiOutput("seasonal_analysis_title_ui"), width = 6, status = "success", solidHeader = TRUE,
            withSpinner(plotlyOutput("seasonal_analysis", height = "350px"))
          )
        )
      ),
      
      # ===== ROOT CAUSE ANALYSIS =====
      tabItem(tabName = "root_cause",
        fluidRow(
          box(
            title = uiOutput("root_cause_title_ui"), width = 12, status = "primary", solidHeader = TRUE,
            p(uiOutput("root_cause_intro_ui")),
            hr(),
            h4("Explore Relationships Between Indicators")
          )
        ),
        fluidRow(
          box(
            title = uiOutput("rca_config_title_ui"), width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4,
                     uiOutput("rca_indicator_ui")
              ),
              column(6,
                     checkboxGroupInput("rca_drivers", "2. Select Potential Drivers to Analyze:",
                                        choices = c("Poverty Rate" = "poverty_rate", "Maternal Education" = "maternal_education",
                                                    "Sanitation Access" = "sanitation_access", "Food Diversity" = "food_diversity",
                                                    "Health Access (km)" = "health_access_km"),
                                        selected = c("poverty_rate", "maternal_education", "sanitation_access"), inline = TRUE)
              ),
              column(2, br(), uiOutput("rca_button_ui"))
            )
          )
        ),
        fluidRow(
          box(title = "Driver Importance Analysis", width = 7, status = "warning", solidHeader = TRUE,
              withSpinner(plotlyOutput("rca_driver_plot", height = "400px"))),
          box(title = "Key Findings & Insights", width = 5, status = "info", solidHeader = TRUE,
              withSpinner(uiOutput("rca_insights", height = "400px"))))
      ),
      # ===== PREDICTIVE ANALYTICS =====
      tabItem(tabName = "predictions",
        fluidRow(
          infoBoxOutput("forecast_value_box", width = 4),
          infoBoxOutput("risk_districts_box", width = 4),
          infoBoxOutput("roi_metric_box", width = 4)
        ),
        fluidRow(
          box( # Corrected: Made title dynamic
            title = uiOutput("pred_config_title_ui"), width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, uiOutput("pred_target_ui")),
              column(3, uiOutput("pred_indicator_ui")),
              column(3, uiOutput("pred_horizon_ui")),
              column(3, br(), uiOutput("run_prediction_ui"))
            ),
            hr(),
            h4(uiOutput("intervention_scenarios_title_ui"), style = "text-align:center;"),
            fluidRow(
              column(6, uiOutput("intervention_impact_ui")),
              column(6, uiOutput("intervention_budget_ui"))
            )
          )
        ),
        
        fluidRow(
          box( # Corrected: Made title dynamic
            title = uiOutput("forecast_chart_title_ui"), width = 8, status = "info", solidHeader = TRUE,
            withSpinner(plotlyOutput("prediction_chart", height = "400px")),
            hr(),
            withSpinner(uiOutput("prediction_insights"))
          ),
          box( # Corrected: Made title dynamic
            title = uiOutput("risk_assessment_title_ui"), width = 4, status = "warning", solidHeader = TRUE,
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
            title = uiOutput("policy_brief_title_ui"), width = 7, status = "info", solidHeader = TRUE, # Corrected: Made title dynamic
            uiOutput("policy_output"),
            hr(),
            fluidRow(
              column(6, uiOutput("download_pdf_btn_ui")),
              column(6, uiOutput("download_ppt_btn_ui")),
              column(12, uiOutput("download_policy_pdf"), uiOutput("download_policy_ppt")) # Added download buttons here
            )
          )
        ),
        
        fluidRow(
          box(
            title = uiOutput("roadmap_title_ui"), width = 8, status = "warning", solidHeader = TRUE, # Corrected: Made title dynamic
            plotlyOutput("implementation_roadmap", height = "350px")
          ),
          box(
            title = uiOutput("success_prob_title_ui"), width = 4, status = "success", solidHeader = TRUE,
            withSpinner(plotlyOutput("success_probability", height = "350px")),
            hr(),
            uiOutput("success_prob_interpretation_title_ui"),
            uiOutput("success_prob_interpretation_p1_ui")
          ),
          box(
            title = uiOutput("success_prob_interp_title_ui"), width = 12, status = "primary", solidHeader = TRUE,
            p(uiOutput("success_prob_interp_p1_ui")),
            tags$ul(
              tags$li(strong("Budget Adequacy:"), " Measures if the budget is sufficient for the scale of the problem. A higher score means the budget is more adequate."),
              tags$li(strong("Timeline Feasibility:"), " Assesses if the implementation timeline is realistic. Longer timelines for complex issues score higher."),
              tags$li(strong("Data Quality:"), " Reflects the amount of available data to inform the policy. More data leads to better-informed strategies and a higher score.")
            )
          )
        )
      ),
      # ===== DATA QUALITY =====
      tabItem(tabName = "data_quality",
        fluidRow(
          box(title = uiOutput("data_quality_title_ui"), width = 12, status = "primary", solidHeader = TRUE,
              p(uiOutput("data_quality_intro_ui")),
              hr(),
              h4("Key Data Quality Metrics")
          )
        ),
        fluidRow(
          infoBoxOutput("completeness_box", width = 4),
          infoBoxOutput("consistency_box", width = 4),
          infoBoxOutput("timeliness_box", width = 4)
        ),
        fluidRow(
          box(
            title = uiOutput("missing_values_title_ui"), width = 12, status = "info", solidHeader = TRUE,
            p(uiOutput("missing_values_intro_ui")),
            withSpinner(reactableOutput("missing_values_table")),
            br(),
            p("Note: Missing values can impact the accuracy of analysis. Efforts are made to impute or account for missing data where appropriate.")
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
            title = uiOutput("intervention_effectiveness_title"), width = 8, status = "primary", solidHeader = TRUE,
            p(uiOutput("impact_narrative_goal_ui")),
            h4(uiOutput("impact_narrative_how_title_ui")),
            tags$ul(
              tags$li(uiOutput("impact_narrative_decision_making_ui")),
              tags$li(uiOutput("impact_narrative_accountability_ui")),
              tags$li(uiOutput("impact_narrative_optimization_ui")),
              tags$li(uiOutput("interpreting_phases_title_ui"))
            ),
            hr(),
            withSpinner(plotlyOutput("intervention_effectiveness_chart", height = "250px")),
            br(),
            div(class = "ai-insights", style="background: #e0f2fe; border-color: #0ea5e9;",
              h5(style="color: #0369a1;", uiOutput("interpreting_phases_title_ui")),
              p(uiOutput("phase1_desc_ui")),
              p(uiOutput("phase2_desc_ui")),
              p(uiOutput("phase3_desc_ui"))
            )
          ),
          box(
            title = uiOutput("sdg_progress_title"), width = 4, status = "info", solidHeader = TRUE,
            withSpinner(uiOutput("sdg_progress")),
            hr(),
            h5("Key Takeaways"),
            p("Focus on reducing stunting and poverty to meet SDG targets.")
          )
        ),
        
        fluidRow(
          box(
            title = uiOutput("resource_allocation_title_ui"), width = 6, status = "success", solidHeader = TRUE,
            withSpinner(plotlyOutput("resource_allocation", height = "350px")),
            br(),
            p("This chart highlights districts requiring prioritized resource allocation based on a composite score of key malnutrition and poverty indicators.")
          ),
          box(
            title = uiOutput("impact_summary_title_ui"), width = 6, status = "warning", solidHeader = TRUE,
            withSpinner(uiOutput("impact_summary")),
            br(),
            p("Summary of current nutrition status and overall impact trends.")
          )
        )
      ),
      # ===== SOLUTIONS HUB =====
      tabItem(tabName = "solutions",
        fluidRow(
          div(class = "narrative-box", # Corrected: Made narrative dynamic
              h3(icon("hands-helping"), uiOutput("solutions_hub_title_ui")),
              p(uiOutput("solutions_hub_intro_ui"))
          )
        ),
        fluidRow(
          box(
            title = uiOutput("solution1_title_ui"), status = "primary", solidHeader = TRUE, width = 4,
            h4(uiOutput("solution1_concept_ui")),
            uiOutput("solution1_details_ui"),
            h5(uiOutput("solution1_impact_title_ui"), style = "color: #1e40af;"),
            p(uiOutput("solution1_impact_text_ui"))
          ),
          box(
            title = uiOutput("solution2_title_ui"), status = "success", solidHeader = TRUE, width = 4,
            h4(uiOutput("solution2_concept_ui")),
            uiOutput("solution2_details_ui"),
            h5(uiOutput("solution1_impact_title_ui"), style = "color: #047857;"),
            p(uiOutput("solution2_impact_text_ui")),
            hr(),
            h5("Traceability"),
            p("QR code integration for farm-to-consumer tracking.")
          ),
          box(
            title = uiOutput("solution3_title_ui"), status = "warning", solidHeader = TRUE, width = 4,
            h4(uiOutput("solution3_concept_ui")),
            p(uiOutput("solution3_integration_ui")),
            uiOutput("solution3_details_ui"),
            h5(uiOutput("solution1_impact_title_ui"), style = "color: #a16207;"),
            p(uiOutput("solution3_impact_text_ui")),
            hr(),
            h5("AI Integration"),
            p("Predictive modeling for early warning systems.")
          )
        ),
        fluidRow(
          
        )
      ),
      
      # ===== ENGAGING DOCUMENTATION =====
      tabItem(tabName = "documentation",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = uiOutput("engaging_docs_title_ui"),
            uiOutput("documentation_image_ui"),
            
            fluidRow(
              column(7, 
                h3(uiOutput("docs_evidence_title_ui")),
                p(uiOutput("docs_evidence_body_ui")),
                tags$ul(
                  tags$li(uiOutput("docs_source_nisr_ui")),
                  tags$li(uiOutput("docs_source_unicef_ui")),
                  tags$li(uiOutput("docs_source_gnr_ui")),
                  tags$li(uiOutput("docs_source_media_ui"))
                ),
                hr(),
                h4(uiOutput("limitations_title_ui")),
                tags$ul(
                  tags$li(uiOutput("limitation_refresh_ui")),
                  tags$li(uiOutput("limitation_hidden_hunger_ui")),
                  tags$li("Geographic coverage may vary for specific indicators.")
                ),
                hr(),
                h3(uiOutput("way_forward_title_ui")),
                p(uiOutput("way_forward_intro_ui")),
                tags$ul(
                  tags$li(uiOutput("way_forward_real_time_ui")),
                  tags$li(uiOutput("way_forward_predictive_ui")),
                  tags$li(uiOutput("way_forward_equity_ui")),
                  tags$li("Integration with real-time climate and market data for enhanced prediction.")
                )
              ),
              column(5, uiOutput("multimedia_section_ui"))
            ) # end inner fluidRow
          ) # end box
        ) # end outer fluidRow
      ) # end documentation tabItem
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage
# ==============================================================================
# 🔹 ENHANCED SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  # ==============================================================================
  # HELPER FUNCTIONS
  # ==============================================================================
  
  # Reusable function to get trend data for a specific area and indicator
  get_trend_data <- function(data, area, ind) {
    if (area == "National Average") {
      data[, .(value = mean(get(ind), na.rm = TRUE)), by = year]
    } else {
      data[district == area, .(value = mean(get(ind), na.rm = TRUE)), by = year]
    }
  }
  
  # Convert full_data to a data.table for performance
  full_data <- as.data.table(full_data)
  
  
  # ==============================================================================
  # DYNAMIC HEADER UI
  # ==============================================================================
  output$dynamic_header <- renderUI({
    req(current_lang())
    dashboardHeader(
      title = tagList(
        span(class = "logo-lg", get_text("dashboard_title", current_lang())),
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/17/Flag_of_Rwanda.svg", height = "30px")
      ),
      dropdownMenu(
        type = "notifications",
        headerText = get_text("system_alerts", current_lang()),
        notificationItem(get_text("high_stunting_alert", current_lang()), icon = icon("exclamation-triangle")),
        notificationItem(get_text("new_data_alert", current_lang()), icon = icon("info-circle")),
        notificationItem(get_text("regional_update", current_lang()), icon = icon("globe"))
      ),
      dropdownMenu(
        type = "messages",
        headerText = get_text("quick_actions", current_lang()),
        messageItem(get_text("download_dashboard", current_lang()), get_text("export_powerpoint", current_lang()), time = get_text("available", current_lang())),
        messageItem(get_text("generate_report", current_lang()), get_text("ai_insights", current_lang()), time = get_text("ready", current_lang())),
        messageItem(get_text("schedule_alert", current_lang()), get_text("set_monitoring", current_lang()), time = get_text("configure", current_lang()))
      ),
      tags$li(
        class = "dropdown",
        pickerInput(
          inputId = "selected_language",
          label = NULL,
          choices = c("en", "rw"), 
          selected = isolate(input$selected_language) %||% "en",
          choicesOpt = list(content = c("🇬🇧 English", "🇷🇼 Kinyarwanda")) # Corrected: Use actual emojis
        )
      )
    )
  })
  output$sidebar_menu <- renderUI({
    sidebarMenu(
      id = "tabs",
      menuItem(get_text("national_overview", current_lang()), tabName = "overview", icon = icon("flag")),
      menuItem(get_text("district_rankings", current_lang()), tabName = "rankings", icon = icon("trophy")),
      menuItem(get_text("trend_analysis", current_lang()), tabName = "trends", icon = icon("chart-line")),
      menuItem(get_text("root_cause_analysis", current_lang()), tabName = "root_cause", icon = icon("sitemap")),
      menuItem(get_text("predictive_analytics", current_lang()), tabName = "predictions", icon = icon("flask")),
      menuItem(get_text("policy_advisor", current_lang()), tabName = "policy", icon = icon("lightbulb")),
      menuItem(get_text("impact_assessment", current_lang()), tabName = "impact", icon = icon("bullseye")),
      menuItem(get_text("data_quality", current_lang()), tabName = "data_quality", icon = icon("check-circle")),
      menuItem(get_text("solutions_hub", current_lang()), tabName = "solutions", icon = icon("hands-helping")),
      menuItem(get_text("engaging_docs_title", current_lang()), tabName = "documentation", icon = icon("book-open"))
    )
  })

  output$global_filters_title <- renderUI({
    h5(get_text("global_filters", current_lang()), style = "text-align: center; color: #34495e;")
  })

  output$global_filters <- renderUI({
    tagList(
      selectInput("selected_year", get_text("year", current_lang()),
                  choices = sort(unique(full_data$year), decreasing = TRUE),
                  selected = max(full_data$year)),
      selectInput("selected_province", get_text("province", current_lang()),
                  choices = setNames(c("All Cities", unique(full_data$province)), c(get_text("all_provinces", current_lang()), unique(full_data$province))),
                  selected = "All Cities"),
      selectInput("selected_age_group", get_text("age_group", current_lang()),
                  choices = setNames(c("All Age Groups", unique(full_data$age_group)), c(get_text("all_age_groups", current_lang()), unique(full_data$age_group))),
                  selected = "All Age Groups"),
      selectInput("selected_nutrient", get_text("nutrient", current_lang()),
                  choices = setNames(c("All Nutrients", unique(full_data$nutrient)), c(get_text("all_nutrients", current_lang()), unique(full_data$nutrient))),
                  selected = "All Nutrients"),
      checkboxGroupInput("selected_indicators", get_text("indicators", current_lang()),
                         choices = c("Stunting" = "stunting", "Wasting" = "wasting", "Anemia" = "anemia"),
                         selected = c("stunting", "wasting", "anemia"))
    )
  })
  
  # ==============================================================================
  # DYNAMIC FOOTER UI
  # ==============================================================================
  output$dashboard_footer_left <- renderUI({
    div(style = "width: 100%; display: flex; justify-content: space-around; align-items: center; flex-wrap: wrap;",
      # Data Sources Section
      div(class = "footer-section",
        strong("Data Sources"),
        tags$ul(style = "list-style: none; padding: 0; margin-top: 10px;",
          tags$li(tags$a(href = "https://www.statistics.gov.rw/", target = "_blank", "National Institute of Statistics of Rwanda (NISR)")),
          tags$li(tags$a(href = "https://data.worldbank.org/", target = "_blank", "World Bank Open Data")),
          tags$li(tags$a(href = "https://www.wfp.org/countries/rwanda", target = "_blank", "Rwanda CFSVA Reports (WFP)")),
          tags$li(tags$a(href = "https://www.unicef.org/rwanda/", target = "_blank", "UNICEF Rwanda"))
        )
      ),
      # Developer Info Section
      div(class = "footer-section",
        strong("Developed by"),
        div(class = "footer-developer-info", style = "margin-top: 10px;",
            # Developer Photo
            tags$img(src = "https://th.bing.com/th/id/OIP.FkMjveXYcv0JmXHPs3a_UAAAAA?o=7&cb=12rm=3&rs=1&pid=ImgDetMain&o=7&rm=3", alt = "Developer Photo"),
            div(style = "text-align: left;",
              "Thacien HARAGIRIMANA",
              br(),
              tags$a(href = "https://thacien-haragirimana-portfolio.b12sites.com/index", target = "_blank", "Portfolio"), " | ",
              # LinkedIn Profile
              tags$a(href = "https://www.linkedin.com/in/haragirimana-thacien-7a9b59282", target = "_blank", "LinkedIn")
            )
        )
      ),
      # Institution Logo Section
      div(class = "footer-section",
        tags$a(href = "https://www.statistics.gov.rw/", target = "_blank",
          tags$img(src = "https://www.bing.com/th/id/OIP.411OLhYNlmZCSXuWDVrgZAAAAA?w=207&h=211&c=8&rs=1&qlt=90&o=6&cb=12&dpr=1.5&pid=3.1&rm=2", height = "60px", alt = "NISR Logo")
        ),
        p("Kigali, Rwanda © 2025", style = "margin-top: 5px;") # Corrected: Use actual copyright symbol
      )
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
  
  # Reactive data for global filters
  filtered_data <- reactive({
    df <- copy(full_data) # Use a copy to avoid modifying the original data.table
    req(df, input$selected_year, current_lang())

        # Year filter
        df <- df %>% filter(year == as.numeric(input$selected_year))

        # Province filter (use translated "All Provinces" if present)
        if (!is.null(input$selected_province) &&
            input$selected_province != "All Cities" &&
            input$selected_province != get_text("all_provinces", current_lang())) {
          df <- df %>% filter(province == input$selected_province)
        }

        # Age Group filter
        if (!is.null(input$selected_age_group) &&
            input$selected_age_group != "All Age Groups" &&
            input$selected_age_group != get_text("all_age_groups", current_lang())) {
          df <- df %>% filter(age_group == input$selected_age_group)
        }

        # Nutrient filter
        if (!is.null(input$selected_nutrient) &&
            input$selected_nutrient != "All Nutrients" &&
            input$selected_nutrient != get_text("all_nutrients", current_lang())) {
          df <- df %>% filter(nutrient == input$selected_nutrient)
        }

    return(df)
  })
  # Reactive data for trend charts that respects the province filter
  trend_filtered_data <- reactive({
    df <- copy(full_data)
    
    # Apply province filter if a specific province is selected
    if (!is.null(input$selected_province) && input$selected_province != "All Cities" && input$selected_province != get_text("all_provinces", current_lang())) { # Corrected: Added closing parenthesis
      df <- df[province == input$selected_province]
    }
    
    return(df)
  })
  
  # Helper function to get data for the selected year
  current_year_data <- reactive({
    df <- filtered_data()
    req(df)

    if (nrow(df) == 0) {
      return(data.frame())
    }

    df %>% distinct(district, .keep_all = TRUE)
    df
  })
  # ==============================================================================
  # NATIONAL OVERVIEW OUTPUTS
  # ==============================================================================
  
  # Rankings Narrative
  output$rankings_narrative <- renderUI({
    req(rankings_data(), input$ranking_indicator, input$selected_year, current_lang())
    
    df_rankings <- rankings_data()
    indicator_name <- tools::toTitleCase(gsub(" ", "_", input$ranking_indicator))
    
    if (nrow(df_rankings) < 2) {
      return(HTML("<p>No data available for the selected filters to generate rankings narrative.</p>"))
    }

    # --- Get best and worst performers ---
    best_performer <- df_rankings %>% arrange(rank) %>% head(1)
    worst_performer <- df_rankings %>% arrange(desc(rank)) %>% head(1)
    performance_gap <- round(worst_performer$avg_indicator - best_performer$avg_indicator, 1)

    # --- Prepare for year-over-year comparison ---
    current_year_val <- as.numeric(input$selected_year)
    previous_year_val <- current_year_val - 1

    # Use full_data to get the overall average for the selected scope for trend calculation
    trend_comparison_df <- full_data %>%
      filter(year %in% c(current_year_val, previous_year_val)) %>%
      # Apply the same filters as the rankings_data reactive
      filter(if (input$selected_province != "All Cities") province == input$selected_province else TRUE) %>%
      group_by(year) %>%
      summarise(avg_val = mean(get(input$ranking_indicator), na.rm = TRUE), .groups = 'drop') %>%
      arrange(year)

    annual_change <- NA
    trend_narrative <- ""

    if (nrow(trend_comparison_df) == 2) {
      current_avg <- trend_comparison_df$avg_val[trend_comparison_df$year == current_year_val]
      previous_avg <- trend_comparison_df$avg_val[trend_comparison_df$year == previous_year_val]
      
      if (!is.na(current_avg) && !is.na(previous_avg)) {
        annual_change <- round(current_avg - previous_avg, 1) # Note: This is current - previous
        
        # For indicators where lower is better (like stunting, poverty), a positive change is "concerning"
        trend_direction <- if (annual_change > 0) "concerning" else "improving"
        trend_color <- if (trend_direction == "improving") "#10b981" else "#ef4444"
        
        trend_narrative <- glue(
          "The trend is <strong style='color:{trend_color};'>{trend_direction}</strong>, with a {abs(annual_change)} percentage point change from {previous_year_val}."
        )
      }
    }
    
    # Determine the geographic scope for the narrative
    geographic_scope <- if (input$rankings_province != "All Provinces") {
      paste("districts in the", input$rankings_province, "province")
    } else {
      "all districts of Rwanda"
    }
    
    HTML(glue(
      "<p>For {input$selected_year}, the analysis of <strong>{indicator_name}</strong> across {geographic_scope} shows that 
      <strong>{best_performer$district}</strong> is the best performer ({round(best_performer$avg_indicator, 1)}%), while 
      <strong>{worst_performer$district}</strong> requires the most attention ({round(worst_performer$avg_indicator, 1)}%).</p> 
      
      <p>The performance gap between the highest and lowest-ranked districts is <strong>{performance_gap} percentage points</strong>. 
      Rankings are determined by the selected indicator, with poverty rate used as a tie-breaker.</p> 
      
      <p>{trend_narrative}</p>
    "))
  })
  
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
      change_text <- glue(get_text("narrative_change_text", current_lang()), abs_annual_change = abs(annual_change), previous_year = previous_year_data$year)
    } else {
      annual_change <- NA
      trend_key <- "narrative_trend_stable"
      trend_color <- "#6b7280"
      trend_icon <- "fa-minus"
      change_text <- get_text("narrative_no_trend", current_lang())
    }
    
    province_name <- if(input$selected_province != 'All Cities') input$selected_province else 'Rwanda'
    
    HTML(glue(
    "<h3><i class='fas fa-flag'></i> {get_text('national_intelligence', current_lang())}</h3>
    <div style='display: flex; justify-content: space-between; align-items: center; margin-top: 15px;'>
      <div>
        <p style='font-size: 1.1em; margin-bottom: 10px;'>
          {glue(get_text('narrative_intro', current_lang()), input_selected_year = input$selected_year, province_name = province_name, avg_stunting = national_stats$avg_stunting, total_districts = national_stats$total_districts)}
          {glue(get_text('narrative_at_risk', current_lang()), at_risk_districts_count = at_risk_districts_count)}
        </p>
        <p style='font-size: 1em; margin-bottom: 0;'>
          <i class='fas {trend_icon}' style='color: {trend_color}; margin-right: 5px;'></i>
          {glue(get_text(trend_key, current_lang()), trend_color = trend_color, change_text = change_text)}
        </p>
      </div>
      <div style='text-align: center; flex-shrink: 0; margin-left: 20px;'>
        <div style='background: rgba(255,255,255,0.2); padding: 15px; border-radius: 8px;'>
          <div style='font-size: 2.5em; font-weight: bold;'>{national_stats$avg_stunting}%</div>
          <div>{get_text('national_stunting_rate', current_lang())}</div>
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
      get_text("stunting", current_lang()),
      paste0(avg_stunting, "%"),
      subtitle = get_text("chronic_malnutrition", current_lang()),
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
      get_text("wasting", current_lang()),
      paste0(avg_wasting, "%"),
      subtitle = get_text("acute_malnutrition", current_lang()),
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
      get_text("anemia", current_lang()),
      paste0(avg_anemia, "%"),
      subtitle = get_text("iron_deficiency", current_lang()),
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
      get_text("poverty", current_lang()),
      paste0(avg_poverty, "%"),
      subtitle = get_text("economic_indicator", current_lang()),
      icon = icon("coins"),
      color = "yellow", # Using a consistent color
      fill = TRUE
    )
  })
  
  # National trends chart
  output$national_trends_chart <- renderPlotly({
    df <- trend_filtered_data() # This reactive already calls full_data()
    
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
          title = get_text("trend_analysis", current_lang()),
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
          title = get_text("trend_analysis", current_lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    p <- plot_ly(trends_data, x = ~year) %>%
      add_trace(y = ~stunting, name = get_text("stunting", current_lang()), type = 'scatter', mode = 'lines+markers',
                line = list(color = app_colors[1], width = 2), marker = list(size = 6)) %>%
      add_trace(y = ~wasting, name = get_text("wasting", current_lang()), type = 'scatter', mode = 'lines+markers',
                line = list(color = app_colors[2], width = 2), marker = list(size = 6)) %>%
      add_trace(y = ~anemia, name = get_text("anemia", current_lang()), type = 'scatter', mode = 'lines+markers',
                line = list(color = app_colors[3], width = 2), marker = list(size = 6)) %>%
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
    downloadButton("download_trends", get_text("download", current_lang()), class = "btn-sm btn-info")
  })
  
  output$map_buttons_ui <- renderUI({
    tagList(
      actionButton("reset_map_view", get_text("view", current_lang()), class = "btn-sm"),
      downloadButton("download_map", get_text("download", current_lang()), class = "btn-sm btn-success")
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
          title = get_text("stunting", current_lang()),
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
          title = get_text("stunting", current_lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    plot_ly(prov_data, x = ~reorder(province, -stunting), y = ~stunting, 
            type = 'bar', name = 'Stunting',
            marker = list(color = app_colors[4]),
            source = "drilldown") %>% # Add source for drilldown
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Stunting Rate (%)")
      ) %>%
      event_register("plotly_click") # Register click events
  })
  
  # National map
  output$national_map <- renderLeaflet({
    # Ensure data and shapefile are loaded
    req(nrow(filtered_data()) > 0, !is.null(rwanda_districts_sf), 
        !is.null(input$map_indicator))

    # Use the indicator from the new dropdown above the map
    selected_indicator <- input$map_indicator

    map_data <- filtered_data() %>%
      filter(year == input$selected_year) %>%
      group_by(district) %>%
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
        title = paste(tools::toTitleCase(gsub(" ", "_", selected_indicator)), "(%)"),
        opacity = 1
      )
  })

  # Reset map view
  observeEvent(input$reset_map_view, {
    leafletProxy("national_map") %>% setView(lng = 29.8739, lat = -1.9403, zoom = 8)
  })
  
  # Key insights
  output$key_insights <- renderUI({
    req(filtered_data(), nrow(filtered_data()) > 0) # Ensure data is not empty
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
    
    improvement_data <- full_data %>% # Use full_data for historical comparison
      filter(year %in% c(baseline_year, as.numeric(current_year))) %>%
      group_by(year) %>%
      summarise(avg_stunting = mean(stunting, na.rm = TRUE))
    
    if (nrow(improvement_data) == 2) {
      improvement <- round(improvement_data$avg_stunting[1] - improvement_data$avg_stunting[2], 1)
    } else {
      improvement <- 0
    }
    
    HTML(glue(
    "<div class='metric-card'>
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
    df <- filtered_data()
    req(nrow(df) > 0)
    
    current_year <- as.numeric(input$selected_year)
    previous_year <- current_year - 1

    districts_in_view <- n_distinct(df$district, na.rm = TRUE)
    
    # Districts with stunting over 30%
    high_risk_districts <- df %>%
      group_by(district) %>%
      summarise(avg_stunting = mean(stunting, na.rm = TRUE), .groups = 'drop') %>%
      filter(avg_stunting > 30) %>%
      nrow()

    # Districts improving vs previous year
    comparison_df <- full_data[year %in% c(current_year, previous_year) & district %in% unique(df$district)]
    
    trend_comparison <- comparison_df[, .(value = mean(stunting, na.rm = TRUE)), by = .(district, year)] %>%
      dcast(district ~ year, value.var = "value")
    
    setnames(trend_comparison, as.character(c(current_year, previous_year)), c("current_val", "previous_val"), skip_absent=TRUE)
    
    num_improving <- trend_comparison[!is.na(current_val) & !is.na(previous_val) & current_val < previous_val, .N]

    HTML(glue(
      "<div class='metric-card' style='border-left-color: #6366f1;'>
        <h6 style='color: #4338ca;'><i class='fas fa-map-marker-alt'></i> Districts in View</h6>
        <strong>{districts_in_view}</strong><br><small>Based on filters</small>
      </div>
      <div class='metric-card' style='border-left-color: #ef4444;'>
        <h6 style='color: #be123c;'><i class='fas fa-exclamation-triangle'></i> High-Risk Districts</h6>
        <strong>{high_risk_districts}</strong><br><small>Stunting > 30%</small>
      </div>
      <div class='metric-card' style='border-left-color: #10b981;'>
        <h6 style='color: #047857;'><i class='fas fa-arrow-up'></i> Districts Improving</h6>
        <strong>{num_improving}</strong><br><small>vs. {previous_year}</small>
      </div>"
    ))
  })
  
  # Quick stats title
  output$quick_stats_title <- renderUI({
    h5(get_text("quick_stats", current_lang()))
  })

  # Reactive titles for boxes
  output$national_trends_title <- renderUI({ h3(get_text("national_trends", current_lang())) })
  output$city_comparison_title <- renderUI({
    title_text <- if (!is.null(input$selected_province) && input$selected_province != "All Cities") {
      paste("District Comparison in", input$selected_province)
    } else {
      get_text("city_comparison", current_lang())
    }
    h3(title_text)
  })
  output$geographic_distribution_title <- renderUI({ h3(get_text("geographic_distribution", current_lang())) })
  
  output$key_insights_title <- renderUI({ h3(get_text("ai_insights", current_lang())) })
  output$rankings_table_title <- renderUI({ h3(get_text("rankings_table", current_lang())) })
  output$performance_dist_title <- renderUI({ h3(get_text("performance_distribution", current_lang())) })
  output$performance_cat_title <- renderUI({ h4(get_text("performance_categories", current_lang())) })
  output$rankings_map_title <- renderUI({ h3(get_text("geographic_distribution", current_lang())) })
  output$top_bottom_title <- renderUI({ h5(get_text("top_bottom_performers", current_lang())) }) # Corrected: Changed to h5 for consistency
  output$perf_over_time_title <- renderUI({ get_text("performance_over_time", current_lang()) })

  output$resource_allocation_title_ui <- renderUI({ get_text("resource_allocation", current_lang()) })
  
  # New UI Outputs for dynamic titles
  output$map_indicator_ui <- renderUI({
    selectInput("map_indicator", 
                label = get_text("select_indicator", current_lang()),
                choices = setNames(c("stunting", "wasting", "anemia", "poverty_rate"), 
                                   c(get_text("stunting", current_lang()), get_text("wasting", current_lang()), get_text("anemia", current_lang()), get_text("poverty", current_lang()))),
                selected = "stunting")
  })
  output$impact_summary_title_ui <- renderUI({ get_text("impact_summary_title", current_lang()) })
  
  # Regional Comparison Narrative & Titles
  output$regional_narrative_title <- renderUI({
    get_text("regional_context_title", current_lang())
  })
  output$regional_narrative_body <- renderUI({ # Corrected: Made narrative dynamic
    req(regional_data, current_lang())
    country_count <- n_distinct(regional_data$country) - 1 # Exclude Rwanda
    latest_year <- max(regional_data$year)
    
    HTML(glue("<p>{get_text('regional_intro_p1', current_lang())}</p><p>{get_text('regional_intro_dynamic', current_lang(), country_count = country_count, latest_year = latest_year)}</p><p>{get_text('regional_intro_p2', current_lang())}</p>")) # Corrected: Added dynamic intro
  })
  
  # Root Cause Analysis Titles
  output$root_cause_title_ui <- renderUI({
    h3(get_text("root_cause_title", current_lang()))
  })
  output$root_cause_intro_ui <- renderUI({
    p(get_text("root_cause_intro", current_lang()))
  })
  output$rca_config_title_ui <- renderUI({ "Driver Analysis Configuration" }) # Assuming this is static or add to translations
  output$rca_indicator_ui <- renderUI({
    selectInput("rca_primary_indicator", paste("1.", get_text("select_indicator", current_lang())),
                choices = setNames(c("stunting", "wasting", "anemia"), c(get_text("stunting", current_lang()), get_text("wasting", current_lang()), get_text("anemia", current_lang()))))
  })
  output$rca_button_ui <- renderUI({
    actionButton("run_rca", get_text("analyze", current_lang()), icon = icon("cogs"), class = "btn-success btn-block")
  })
  
  # Dynamic UI for Policy Advisor
  output$policy_config_ui <- renderUI({
    tagList(
      p(get_text("policy_intro", current_lang())),
      selectInput("policy_focus_area", get_text("focus_area", current_lang()),
                  choices = setNames(c("Stunting Reduction", "Wasting Prevention", "Anemia Control"), c(get_text("stunting_reduction_policy", current_lang()), get_text("wasting_prevention_policy", current_lang()), get_text("anemia_control_policy", current_lang())))),
      selectInput("policy_target", get_text("target", current_lang()),
                  choices = setNames(c("National", "City Specific", "District Specific"), c(get_text("national_target", current_lang()), get_text("city_specific_target", current_lang()), get_text("district_specific_target", current_lang())))),
      conditionalPanel(
        condition = "input.policy_target == 'District Specific'",
        selectInput("policy_district", get_text("district", current_lang()), choices = unique(full_data$district))
      ),
      sliderInput("policy_timeline", get_text("implementation_timeline", current_lang()), min = 6, max = 60, value = 24, step = 6),
      numericInput("policy_budget", get_text("available_budget", current_lang()), value = 1000000, min = 100000, step = 100000),
      actionButton("generate_policy", get_text("generate_report", current_lang()), class = "btn-primary btn-block", icon = icon("brain"))
    )
  })
  
  output$success_prob_interp_title_ui <- renderUI({ "Interpreting the Success Probability" }) # Or add to translations
  output$success_prob_interp_p1_ui <- renderUI({ "The 'Success Probability' chart visualizes key factors that influence the potential success of a policy. A larger, more balanced shape indicates a higher likelihood of success." }) # Or add to translations
  
  # Data Quality Titles
  output$data_quality_title_ui <- renderUI({ get_text("data_quality", current_lang()) })
  output$data_quality_intro_ui <- renderUI({ "This section provides an overview of the quality of the underlying dataset, ensuring transparency and confidence in the analysis." })
  output$missing_values_title_ui <- renderUI({ "Missing Values Analysis" })
  output$missing_values_intro_ui <- renderUI({ "Count of missing (NA) values for key nutrition and socio-economic indicators." })
  
  # Impact Assessment Titles
  output$intervention_effectiveness_title <- renderUI({ get_text("intervention_effectiveness", current_lang()) })
  output$sdg_progress_title <- renderUI({ get_text("sdg_progress", current_lang()) })
  
  # Rankings Page
  output$perf_gap_title_ui <- renderUI({ "Performance Gap Analysis" })
  
  # ==============================================================================
  # DISTRICT RANKINGS OUTPUTS
  # ==============================================================================
  
  # Reactive for rankings data
  rankings_data <- reactive({
    # Start with global filters
    df <- filtered_data()
    req(df, input$ranking_indicator, nrow(df) > 0, input$rankings_province, input$rankings_district)
    
    # Apply tab-specific filters
    if (input$rankings_province != "All Provinces") {
      df <- df %>% filter(province == input$rankings_province) # New province filter
    }
    if (input$rankings_district != "All Districts") {
      df <- df %>% filter(district == input$rankings_district) # New district filter
    }

    # Ensure we have data after filtering
    req(nrow(df) > 0)
    
    data_for_ranking <- df
    indicator <- input$ranking_indicator
    
    # Get 5-year trend data for sparklines
    trend_data <- full_data[
      year >= (as.numeric(input$selected_year) - 4) & year <= as.numeric(input$selected_year) & district %in% unique(data_for_ranking$district),
      .(trend_value = mean(get(indicator), na.rm = TRUE)),
      by = .(district, year)
    ][, .(trend_list = list(trend_value)), by = district]
    
    data_for_ranking %>%
      group_by(district, province) %>%
      summarise(
        avg_indicator = mean(get(input$ranking_indicator), na.rm = TRUE),
        # Add a secondary metric for tie-breaking
        poverty_rate = mean(poverty_rate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(trend_data, by = "district") %>%
      arrange(avg_indicator, poverty_rate) %>%
      mutate(
        rank = row_number(), # Rank is now based on the new arrangement
        performance = case_when(
          rank <= 10 ~ "Best Performing",
          rank <= 20 ~ "Average",
          TRUE ~ "Needs Improvement"
        )
      )
  })

  # KPI Box: Best Performer
  output$best_performer_box <- renderInfoBox({
    df <- rankings_data()
    best <- df %>% arrange(rank) %>% head(1)
    infoBox(
      get_text("best_performer", current_lang()),
      best$district,
      subtitle = paste0(round(best$avg_indicator, 1), "% ", tools::toTitleCase(input$ranking_indicator)),
      icon = icon("trophy"),
      color = "green"
    )
  })

  # KPI Box: Worst Performer
  output$worst_performer_box <- renderInfoBox({
    df <- rankings_data()
    worst <- df %>% arrange(desc(rank)) %>% head(1)
    infoBox(
      get_text("needs_attention", current_lang()),
      worst$district,
      subtitle = paste0(round(worst$avg_indicator, 1), "% ", tools::toTitleCase(input$ranking_indicator)),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  # KPI Box: Average Rate
  output$average_rate_box <- renderInfoBox({
    df <- rankings_data()
    avg_val <- round(mean(df$avg_indicator, na.rm = TRUE), 1)
    infoBox(
      get_text("national_average", current_lang()),
      paste0(avg_val, "%"),
      subtitle = paste("for", input$selected_year),
      icon = icon("balance-scale"),
      color = "blue"
    )
  })

  # Dynamic UI for ranking filters (Indicator)
  output$rankings_filters_ui <- renderUI({
    # The choices for the indicator dropdown now come from the global filter
    indicator_choices <- c("Stunting" = "stunting", "Wasting" = "wasting",
                           "Anemia" = "anemia", "Poverty Rate" = "poverty_rate"
    )

    selectInput("ranking_indicator", get_text("indicator", current_lang()), 
                choices = indicator_choices,
                selected = indicator_choices[1],
                width = "100%") # Use full width
  })

  # NEW: Province filter for rankings tab, as requested
  output$rankings_province_filter_ui <- renderUI({
    selectInput("rankings_province", "Filter by Province:",
                choices = c("All Provinces", unique(full_data$province)),
                selected = "All Provinces")
  })

  # NEW: District filter for rankings tab, as requested (choices update based on province)
  output$rankings_district_filter_ui <- renderUI({
    req(input$rankings_province)
    
    district_choices <- if (input$rankings_province == "All Provinces") {
      unique(full_data$district)
    } else {
      unique(full_data$district[full_data$province == input$rankings_province])
    }
    selectInput("rankings_district", "Filter by District:", choices = c("All Districts", sort(district_choices)), selected = "All Districts")
  })

  output$export_rankings_btn_ui <- renderUI({
    downloadButton("download_rankings", get_text("export_rankings", current_lang()), class = "btn-success")
  })

  # Rankings Table with Sparklines
  output$rankings_table <- renderReactable({
    rankings <- rankings_data()
  req(rankings)
  
  reactable(
    rankings,
    columns = list(
      rank = colDef(name = "Rank", width = 70, align = "center",
                   cell = function(value) {
                     if (value <= 3) {
                       div(class = "badge badge-success", style="font-size:14px; padding: 5px 8px;", paste("🏆", value))
                     } else if (value <= 10) {
                       div(class = "badge badge-info", style="font-size:12px;", paste("⭐", value))
                     } else {
                       as.character(value)
                     }
                   }),
      district = colDef(name = "District", minWidth = 140),
      province = colDef(name = "Province", minWidth = 110),
      avg_indicator = colDef(
        name = paste(tools::toTitleCase(input$ranking_indicator), "(%)"), 
        minWidth = 150,
        align = "left",
        cell = function(value) {
          normalized_value <- value / max(rankings$avg_indicator, na.rm = TRUE) * 100
          bar_color <- if (value > 30) "#ef4444" else if (value > 20) "#f97316" else "#10b981"
          
          div(
            div(style = list(fontWeight = 600, color = "#333"), paste0(round(value, 1), "%")),
            div(
              style = list(background = "#e2e8f0", height = "8px", borderRadius = "4px"),
              div(style = list(background = bar_color, width = paste0(normalized_value, "% "), height = "100%", borderRadius = "4px"))
            )
          )
        }
      ),
      trend_list = colDef(
        name = "5-Year Trend",
        minWidth = 150,
        cell = function(values) {
          if (length(values) > 1) {
            sparkline(values, type = "line", 
                      lineColor = app_colors[4],
                      fillColor = "rgba(9, 44, 160, 0.1)",
                      chartRangeMin = min(unlist(rankings$trend_list), na.rm = TRUE),
                      chartRangeMax = max(unlist(rankings$trend_list), na.rm = TRUE),
                      height = "40px")
          } else {
            "N/A"
          }
        }
      ),
      performance = colDef(
        name = "Category", 
        minWidth = 120,
        cell = function(value) {
          color <- switch(value,
                         "Best Performing" = "primary",
                         "Average" = "warning",
                         "Needs Improvement" = "danger")
          span(class = paste0("badge badge-", color), style="font-size:12px; padding: 4px 8px;", value)
        }
      )
    ),
    striped = TRUE,
    highlight = TRUE,
    searchable = TRUE,
    showPageSizeOptions = TRUE,
    defaultPageSize = 15,
    theme = reactableTheme(
      headerStyle = list(background = "#f8fafc", borderBottom = "2px solid #e2e8f0"),
      borderColor = "#e2e8f0",
      highlightColor = "#e0f2fe"
    )
  )
})
  
  # ENHANCED: Narrative for Performance Gap Analysis
  output$top_bottom_narrative_output <- renderUI({
    req(rankings_data(), nrow(rankings_data()) > 1)
    df <- rankings_data()
    indicator_name <- tools::toTitleCase(gsub(" ", " ", input$ranking_indicator))
    
    best <- df %>% arrange(rank) %>% head(1)
    worst <- df %>% arrange(desc(rank)) %>% head(1)
    gap <- round(worst$avg_indicator - best$avg_indicator, 1)
    
    # --- NEW: Year-over-year comparison ---
    current_year <- as.numeric(input$selected_year) # This is fine
    previous_year <- current_year - 1

    prev_year_data <- full_data[
      year == previous_year & district %in% c(best$district, worst$district),
      .(prev_indicator = mean(get(input$ranking_indicator), na.rm = TRUE)),
      by = district]
      
    # Get previous year value for best district
    best_prev_val <- prev_year_data %>% filter(district == best$district) %>% pull(prev_indicator)
    best_change <- if(length(best_prev_val) > 0) round(best$avg_indicator - best_prev_val, 1) else NA
    best_trend_text <- if(!is.na(best_change)) {
      if(best_change < 0) "which is an improvement" else if(best_change > 0) "which is a decline" else "which is stable"
    } else {"(no prior year data)"}

    # Get previous year value for worst district
    worst_prev_val <- prev_year_data %>% filter(district == worst$district) %>% pull(prev_indicator)
    worst_change <- if(length(worst_prev_val) > 0) round(worst$avg_indicator - worst_prev_val, 1) else NA
    worst_trend_text <- if(!is.na(worst_change)) {
      if(worst_change < 0) "showing improvement" else if(worst_change > 0) "showing a decline" else "remaining stable"
    } else {"(no prior year data)"}
    
    # --- Build Narratives ---
    interpretation_narrative <- glue(
      "The performance gap for {tolower(indicator_name)} is <strong>{gap} percentage points</strong>. ",
      "<strong>{best$district}</strong> leads with a rate of <strong>{round(best$avg_indicator, 1)}%</strong>, {best_trend_text} from last year. ",
      "Conversely, <strong>{worst$district}</strong> has the highest rate at <strong>{round(worst$avg_indicator, 1)}%</strong>, {worst_trend_text} compared to last year. ",
      "This highlights not just the current disparity, but also the direction of change for the best and worst performers."
    )
    
    recommendation_narrative <- if (!is.na(worst_change) && worst_change < 0) {
      # Recommendation if the worst district is improving
      glue("Although <strong>{worst$district}</strong> has the highest rate, it is showing positive momentum. The recommendation is to <strong>analyze and reinforce</strong> the interventions that are currently driving this improvement. ",
           "Simultaneously, study the long-term strategies of <strong>{best$district}</strong> to identify best practices that can be scaled.")
    } else {
      # Recommendation if the worst district is stable or worsening
      glue("The lack of improvement in <strong>{worst$district}</strong> requires urgent attention. The recommendation is to <strong>conduct a root cause analysis</strong> in this district to understand barriers to progress. ",
           "Lessons from consistently high-performing districts like <strong>{best$district}</strong> should be adapted and implemented as a priority.")
    }
    
    div(class = "ai-insights", style = "background: #f0f9ff; border-color: #0ea5e9; padding: 20px; height: 400px;",
        div(class = "insight-section",
            h6(style = "color: #0369a1;", icon("lightbulb"), " Interpretation"),
            p(HTML(interpretation_narrative))
        ),
        div(class = "insight-section", style="background: #eef2ff; border-left-color: #4f46e5;",
            h6(style = "color: #4338ca;", icon("cogs"), " Recommendation"),
            p(HTML(recommendation_narrative))
        )
    )
  })


  # NEW: Interactive Map for District Rankings Tab, as requested
  output$rankings_map <- renderLeaflet({
    req(rankings_data(), !is.null(rwanda_districts_sf))
    map_data <- rankings_data()
    indicator <- input$ranking_indicator

    # Join with spatial data
    map_data_sf <- rwanda_districts_sf %>%
      left_join(map_data, by = c("NAME_2" = "district"))

    # Define color palette for the selected indicator
    pal <- colorNumeric(palette = "YlOrRd", domain = map_data_sf$avg_indicator, na.color = "#bdc3c7", reverse = TRUE)

    leaflet(data = map_data_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 29.8739, lat = -1.9403, zoom = 8) %>%
      addPolygons(
        fillColor = ~pal(avg_indicator),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 3, color = "#1d4ed8", dashArray = "", fillOpacity = 0.8, bringToFront = TRUE
        ),
        label = ~paste0(NAME_2, ": ", round(avg_indicator, 1), "%"),
        popup = ~glue("<h4>{NAME_2}</h4><b>Rank:</b> {rank}<br><b>{tools::toTitleCase(indicator)}:</b> {round(avg_indicator, 1)}%")
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~avg_indicator,
        title = paste(tools::toTitleCase(gsub(" ", " ", indicator)), "(%)"),
        opacity = 1
      )
  })

  # ENHANCED: Top & Bottom Performers Chart, as requested
  output$top_bottom_chart <- renderPlotly({
    rankings <- rankings_data()
    req(rankings)

    top_bottom_data <- bind_rows(
      rankings %>% arrange(rank) %>% head(5) %>% mutate(Category = "Top 5 Performers"),
      rankings %>% arrange(desc(rank)) %>% head(5) %>% mutate(Category = "Bottom 5 Performers")
    )

    plot_ly(top_bottom_data, x = ~avg_indicator, y = ~reorder(district, avg_indicator), 
            color = ~Category, type = 'bar', orientation = 'h',
            colors = c("Top 5 Performers" = "#10b981", "Bottom 5 Performers" = "#ef4444")) %>%
      layout( # Improved layout
        xaxis = list(title = paste(tools::toTitleCase(input$ranking_indicator), "(%)")),
        yaxis = list(title = ""),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.3)
      )
  })

  # Performance over time chart
  output$performance_trends <- renderPlotly({
    req(input$ranking_indicator, rankings_data())
    
    # Use full data to show historical trends for top/bottom performers
    trend_data <- full_data[, .(avg_indicator = mean(get(input$ranking_indicator), na.rm = TRUE)), by = .(year, district)]
    
    # Get top and bottom performers from the current year ranking
    top_district <- rankings_data() %>% slice_head(n = 1) %>% pull(district)
    bottom_district <- rankings_data() %>% slice_tail(n = 1) %>% pull(district)
    
    plot_data <- trend_data %>%
      filter(district %in% c(top_district, bottom_district))

    plot_ly(plot_data, x = ~year, y = ~avg_indicator, color = ~district,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = paste(get_text("performance_over_time", current_lang()), "(Top vs. Bottom)"),
             xaxis = list(title = "Year"),
             yaxis = list(title = paste(tools::toTitleCase(input$ranking_indicator), "(%)")))
  })

  # ==============================================================================
  # ROOT CAUSE ANALYSIS OUTPUTS
  # ==============================================================================
  


  rca_results <- eventReactive(input$run_rca, {
    req(input$rca_primary_indicator, input$rca_drivers)

    # Use data filtered by global controls, but across all years for robust correlation
    df <- trend_filtered_data() %>%
      select(all_of(c(input$rca_primary_indicator, input$rca_drivers))) %>%
      na.omit()

    if(nrow(df) < 10) return(NULL)

    correlations <- cor(df)[, input$rca_primary_indicator]
    cor_df <- data.frame(
      driver = names(correlations),
      correlation = correlations
    ) %>%
      filter(driver != input$rca_primary_indicator) %>%
      mutate(
        direction = ifelse(correlation > 0, "Positive", "Negative"),
        abs_corr = abs(correlation)
      ) %>%
      arrange(desc(abs_corr))

    return(cor_df)
  })

  output$rca_driver_plot <- renderPlotly({
    cor_df <- rca_results()
    req(cor_df)

    plot_ly(cor_df,
            x = ~correlation,
            y = ~reorder(tools::toTitleCase(gsub("_", " ", driver)), correlation),
            type = 'bar',
            orientation = 'h',
            marker = list(color = ~ifelse(direction == "Positive", "#ef4444", "#10b981"))) %>%
      layout(
        title = paste("Factors Correlated with", tools::toTitleCase(input$rca_primary_indicator)),
        xaxis = list(title = "Correlation Coefficient (Pearson's r)"),
        yaxis = list(title = ""),
        annotations = list(
          x = 0, y = -0.2, text = "Positive correlation means as the driver increases, the outcome worsens (e.g., more poverty, more stunting).<br>Negative correlation means as the driver increases, the outcome improves (e.g., more education, less stunting).",
          showarrow = F, xref='paper', yref='paper', xanchor='left', yanchor='auto', xshift=0, yshift=0,
          font=list(size=10, color="grey")
        )
      )
  })

  output$rca_insights <- renderUI({
    cor_df <- rca_results()
    req(cor_df)

    # Generate insights using the AI utility function
    generate_rca_insights(outcome = input$rca_primary_indicator, correlations = cor_df)
  })
  # ==============================================================================
  # TREND ANALYSIS OUTPUTS
  # ==============================================================================
  
  # Advanced trends chart
  output$advanced_trends <- renderPlotly({
    req(input$trend_indicator, nrow(full_data) > 0)
    
    # Get trend data
    district <- input$trend_district %||% "National Average"
    
    # Use the new helper function
    trend_data_raw <- get_trend_data(
      data = full_data,
      area = district,
      ind = input$trend_indicator
    )
    setorder(trend_data_raw, year) # Ensure data is sorted by year

    # Ensure we have a full time series for forecasting
    if(nrow(trend_data_raw) < 2) {
        p <- plot_ly() %>%
            add_annotations(text = "Not enough data to create a trend.", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE)
        return(p)
    }

    # Create a ts object for forecasting
    trend_ts <- ts(trend_data_raw$value, start = min(trend_data_raw$year), frequency = 1)

    # Main plot with historical data
    p <- plot_ly(x = ~trend_data_raw$year, y = ~trend_data_raw$value, type = 'scatter', mode = 'lines+markers', name = "Historical",
                 line = list(color = app_colors[4], width = 3)) # Corrected: Use app_colors[4]
    
    # Add forecast if requested
    if (!is.null(input$show_forecast) && input$show_forecast) {
        forecast_horizon <- 3

        if (input$trend_model == "ARIMA" && length(trend_ts) > 3) {
            fit <- auto.arima(trend_ts)
            fore <- forecast(fit, h = forecast_horizon)
            
            forecast_df <- data.frame(
                year = seq(max(trend_data_raw$year) + 1, by = 1, length.out = forecast_horizon),
                forecast = fore$mean,
                lower = fore$lower[,2], # 95% confidence
                upper = fore$upper[,2]
            )

            p <- p %>%
                add_trace(data = forecast_df, x = ~year, y = ~forecast, type = 'scatter', mode = 'lines', name = "ARIMA Forecast",
                          line = list(color = app_colors[5], dash = 'dash')) %>%
                add_ribbons(data = forecast_df, x = ~year, ymin = ~lower, ymax = ~upper, name = '95% CI',
                            line = list(color = 'transparent'),
                            fillcolor = 'rgba(255, 165, 0, 0.2)')

        } else { # Default to Linear Model
            fit <- lm(value ~ year, data = trend_data_raw)
            future_df <- data.frame(year = seq(max(trend_data_raw$year) + 1, by = 1, length.out = forecast_horizon))
            preds <- predict(fit, newdata = future_df, interval = "prediction")
            
            forecast_df <- cbind(future_df, as.data.frame(preds))

            p <- p %>%
                add_trace(data = forecast_df, x = ~year, y = ~fit, type = 'scatter', mode = 'lines', name = "Linear Forecast",
                          line = list(color = app_colors[5], dash = 'dash')) %>%
                add_ribbons(data = forecast_df, x = ~year, ymin = ~lwr, ymax = ~upr, name = '95% CI',
                            line = list(color = 'transparent'),
                            fillcolor = 'rgba(255, 165, 0, 0.2)')
        }
    }
    
    p %>%
      layout(
        title = paste("Trend Analysis:", district),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste(tools::toTitleCase(input$trend_indicator), "Rate (%)")),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1)
    )
  })
  
  # Dynamic UI for trend analysis tab
  output$trend_district_ui <- renderUI({
    selectInput("trend_district", get_text("district", current_lang()),
                choices = c("National Average", unique(full_data$district)),
                selected = "National Average")
  })
  
  output$trend_model_ui <- renderUI({
    selectInput("trend_model", get_text("model", current_lang()),
                choices = setNames(c("Linear", "ARIMA"),
                                   c(get_text("linear_trend", current_lang()), "ARIMA")),
                selected = "Linear")
  })
  
  output$trend_indicator_ui <- renderUI({
    selectInput("trend_indicator", get_text("indicator", current_lang()),
                choices = setNames(c("stunting", "wasting", "anemia", "poverty_rate"),
                                   c(get_text("stunting", current_lang()),
                                     get_text("wasting", current_lang()),
                                     get_text("anemia", current_lang()),
                                     get_text("poverty", current_lang()))),
                selected = "stunting")
  })
  
  
  # Dynamic UI for trend analysis tab
  output$trend_forecast_ui <- renderUI({
    checkboxInput("show_forecast", get_text("show_forecast", current_lang()), value = FALSE)
  })
  
  output$trend_report_btn_ui <- renderUI({
    actionButton("generate_trend_report", get_text("generate_report", current_lang()),
                 class = "btn-warning", icon = icon("chart-line"))
  })
  
  # Trend Model Summary
  output$trend_model_summary <- renderUI({
    req(input$trend_indicator, input$trend_district, input$trend_model)
    
    district <- input$trend_district
    indicator <- input$trend_indicator
    model_type <- input$trend_model
    
    # Use the new helper function
    trend_data_raw <- get_trend_data(
      data = full_data,
      area = district,
      ind = indicator
    )
    
    if(nrow(trend_data_raw) < 2) return(p("Not enough data for model summary."))
    
    if (model_type == "ARIMA") {
      if (nrow(trend_data_raw) > 3) {
        fit <- auto.arima(ts(trend_data_raw$value))
        model_name <- fit$arma
        aic_val <- round(fit$aic, 2)
        
        HTML(glue(
          "<h4><i class='fas fa-cogs'></i> ARIMA Model Details</h4>
          <p>ARIMA is ideal for capturing complex, non-linear patterns in time-series data.</p>
          <div class='metric-card'>
            <h6><i class='fas fa-info-circle'></i> Model Selected</h6>
            <strong>ARIMA({model_name[1]},{model_name[6]},{model_name[2]})</strong>
          </div>
          <div class='metric-card'>
            <h6><i class='fas fa-check-circle'></i> Model Fit (AIC)</h6>
            <strong>{aic_val}</strong><br><small>Lower is better</small>
          </div>
        "))
      } else {
        HTML("<p>Not enough data for ARIMA model. Select 'Linear Trend' instead.</p>")
      }
    } else { # Linear Trend
      fit <- lm(value ~ year, data = trend_data_raw)
      slope <- round(coef(fit)[2], 2)
      r_squared <- round(summary(fit)$r.squared, 3)
      
      HTML(glue(
        "<h4><i class='fas fa-ruler-horizontal'></i> Linear Model Details</h4>
        <p>A linear model provides a simple, straight-line trend showing the average rate of change.</p>
        <div class='metric-card'>
          <h6><i class='fas fa-chart-line'></i> Avg. Annual Change</h6>
          <strong>{slope} points/year</strong>
        </div>
        <div class='metric-card'>
          <h6><i class='fas fa-check-circle'></i> Model Fit (RÂ²)</h6>
          <strong>{r_squared}</strong><br><small>Closer to 1 is better</small>
        </div>
      ))
    }
  })
  
  # Event for generating trend report
  observeEvent(input$generate_trend_report, {
    req(nrow(full_data) > 0)

    district <- input$trend_district %||% "National Average"
    indicator <- input$trend_indicator %||% "stunting"
    
    trend_data_raw <- get_trend_data(
      data = full_data,
      area = district,
      ind = indicator)
    
    if (nrow(trend_data_raw) < 2) {
      showModal(modalDialog(title = get_text("error", current_lang()), "Not enough data to generate a report.", footer = modalButton(get_text("close_report", current_lang()))))
      return()
    }
    
    # Calculate statistics
    fit <- lm(value ~ year, data = trend_data_raw)
    slope <- round(coef(fit)[2], 2)
    r_squared <- round(summary(fit)$r.squared, 3)
    total_change <- round(tail(trend_data_raw$value, 1) - head(trend_data_raw$value, 1), 1)
    trend_direction <- if(slope < -0.1) "a clear improving trend" else if (slope > 0.1) "a concerning worsening trend" else "a relatively stable trend"
    narrative <- glue("The analysis for {tools::toTitleCase(indicator)} in {district} from {min(trend_data_raw$year)} to {max(trend_data_raw$year)} shows {trend_direction}. 
                      On average, the rate has changed by {slope} percentage points per year. The linear model explains approximately {round(r_squared*100)}% of the variability in the data.")
    
    showModal(modalDialog(
      title = get_text("trend_report_title", current_lang()),
      h4(glue(get_text("trend_report_for", current_lang()), indicator = tools::toTitleCase(indicator), district = district)),
      hr(),
      fluidRow(
        column(6,
               h5(get_text("trend_model_info", current_lang())),
               p(strong(get_text("model", current_lang()), ":"), input$trend_model)
        ),
        column(6,
               h5(get_text("key_statistics", current_lang())),
               p(strong("Avg. Annual Change: "), slope, "%"),
               p(strong("Total Change: "), total_change, "%"),
               p(strong("R-squared: "), r_squared)
        )
      ),
      hr(),
      h5("Interpretation"),
      p(narrative),
      hr(),
      h5("Trend Plot"),
      renderPlotly({
        plot_ly(trend_data_raw, x = ~year, y = ~value, type = 'scatter', mode = 'lines+markers', name = "Historical") %>%
          add_lines(y = ~fitted(fit), name = "Trend Line", line = list(dash = 'dash')) %>%
          layout(
            xaxis = list(title = get_text("year", current_lang())),
            yaxis = list(title = paste(tools::toTitleCase(indicator), "(%)"))
          )
      }),
      footer = tagList(
        modalButton(get_text("close_report", current_lang()))
      )
    ))
  })
  
  # Correlation heatmap
  output$correlation_heatmap <- renderPlot({
    # Use trend_filtered_data() to get a more robust correlation over time
    df <- trend_filtered_data()

    req(nrow(df) > 10) # Need sufficient data for meaningful correlation

    # Select relevant numeric columns for the heatmap
    numeric_data <- df %>%
      select(
        Stunting = stunting,
        Wasting = wasting,
        Anemia = anemia,
        Poverty = poverty_rate,
        `Maternal Edu` = maternal_education,
        Sanitation = sanitation_access,
        `Food Diversity` = food_diversity,
        `Health Access` = health_access_km
      ) %>%
      na.omit()

    req(ncol(numeric_data) > 1)
    cor_matrix <- cor(numeric_data)
    
    # Create heatmap
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             addCoef.col = "black", number.cex = 0.7,
             title = "Correlation Matrix of Nutrition Indicators",
             mar = c(0,0,1,0)) # Adjusted margins
  })
  
  # Seasonal analysis
  output$seasonal_analysis <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
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
  
  # ==============================================================================
  # DATA QUALITY OUTPUTS
  # ==============================================================================
  
  output$completeness_box <- renderInfoBox({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      completeness_val <- 0
    } else {
      completeness_val <- df %>%
        select(stunting, wasting, anemia, poverty_rate) %>%
        summarise(across(everything(), ~ 1 - (sum(is.na(.)) / n()))) %>%
        rowMeans() * 100
    }
    # Calculate overall completeness (non-NA values)
      
    infoBox(
      "Overall Completeness",
      paste0(round(completeness_val, 1), "%"),
      subtitle = "Non-missing values",
      icon = icon("check-double"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$consistency_box <- renderInfoBox({
    df <- filtered_data()
    
    # Example: Check for inconsistent stunting values (e.g., > 100)
    inconsistent_count <- if(nrow(df) > 0) sum(df$stunting > 100 | df$stunting < 0, na.rm = TRUE) else 0

    
    infoBox(
      "Consistency Checks",
      inconsistent_count,
      subtitle = "Values out of range",
      icon = icon("shield-alt"),
      color = "yellow",
      fill = TRUE
    )
  })
  
  output$timeliness_box <- renderInfoBox({
    selected_year <- input$selected_year %||% max(full_data$year, na.rm = TRUE)
    
    infoBox(
      "Data Year",
      selected_year,
      subtitle = "Year selected for analysis",
      icon = icon("calendar-check"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$missing_values_table <- renderReactable({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(reactable(data.frame(Message = "No data for the selected year.")))
    }
    
    missing_counts <- df %>%
      select(stunting, wasting, anemia, poverty_rate, maternal_education, sanitation_access, food_diversity) %>%
      summarise(across(everything(), ~ sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "Indicator", values_to = "Missing_Count") %>%
      reactable()
  })
  
  # ==============================================================================
  # PREDICTIVE ANALYTICS OUTPUTS
  # ==============================================================================
  
  # Reactive for prediction results
  prediction_results <- eventReactive(input$run_prediction, {
    indicator <- input$pred_indicator %||% "stunting"
    target_area <- input$pred_target %||% "National"
    horizon <- input$pred_horizon %||% 5
    
    historical_data <- if (target_area != "National") {
      full_data %>% filter(district == target_area)
    } else {
      full_data
    }
    
    historical_ts_df <- historical_data %>%
      group_by(year) %>%
      summarise(value = mean(base::get(indicator), na.rm = TRUE), .groups = 'drop') %>%
      arrange(year)
    
    if(nrow(historical_ts_df) < 4) return(NULL)
    
    ts_data <- ts(historical_ts_df$value, start = min(historical_ts_df$year), frequency = 1)
    fit <- auto.arima(ts_data)
    fore <- forecast(fit, h = horizon)
    
    return(list(
      historical = historical_ts_df,
      forecast = fore,
      fit = fit
    ))
  }, ignoreNULL = FALSE)

  # KPI Box: Forecast Value
  output$forecast_value_box <- renderInfoBox({
    results <- prediction_results() # This can be NULL initially
    
    forecast_val <- "N/A"
    if (!is.null(results)) {
      final_forecast_val <- tail(results$forecast$mean, 1)
      # Ensure intervention_strength is not NULL
      intervention_strength <- input$intervention_strength %||% 0
      forecast_val <- round(final_forecast_val * (1 - (intervention_strength / 100)), 1)
      forecast_val <- paste(forecast_val, "% ")
    }
    
    horizon <- input$pred_horizon %||% 5
    
    infoBox(
      "Forecasted Value",
      forecast_val,
      subtitle = paste("in", horizon, "years"),
      icon = icon("chart-line"),
      color = "purple"
    )
  })

  # KPI Box: High-Risk Districts
  output$risk_districts_box <- renderInfoBox({
    risk_count <- 0 # Default value
    if (!is.null(full_data) && nrow(full_data) > 0) {
      risk_count <- full_data %>%
        filter(year == max(year)) %>%
        group_by(district) %>%
        summarise(avg_stunting = mean(stunting, na.rm = TRUE)) %>%
        filter(avg_stunting > 30) %>%
        nrow()
    }
    infoBox(
      "High-Risk Districts",
      risk_count,
      subtitle = "Stunting > 30%",
      icon = icon("shield-alt"),
      color = "yellow"
    )
  })

  # KPI Box: ROI Metric
  output$roi_metric_box <- renderInfoBox({
    # Simplified ROI for KPI
    roi <- round((input$intervention_strength * 10000) / (input$intervention_budget / 100000), 2)
    infoBox("Potential ROI", paste0(roi, "x"), subtitle = "Simplified Metric", icon = icon("dollar-sign"), color = "green")
  })

  # Prediction chart
  output$prediction_chart <- renderPlotly({
    results <- prediction_results()
    indicator <- input$pred_indicator %||% "stunting"

    # If results are NULL (button not clicked, or not enough data), show a message.
    if (is.null(results)) {
      p <- plot_ly() %>%
        add_annotations(
          text = "Click 'Generate Forecast' to see predictions.<br><i>(Requires at least 4 years of historical data for the selected target)</i>",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "grey")
        ) %>%
        layout(
          title = get_text("forecast_chart_title", current_lang()),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    
    historical <- results$historical
    forecast_obj <- results$forecast
    
    # Simple linear trend forecast
    intervention_impact <- (input$intervention_strength %||% 0) / 100
    
    # Apply intervention to the forecast
    forecast_values <- as.numeric(forecast_obj$mean) * (1 - intervention_impact)
    
    # Create a combined data frame for plotting, ensuring forecast years are correct
    forecast_years <- time(forecast_obj$mean)
    plot_data <- data.frame(
      year = c(historical$year, forecast_years),
      value = c(historical$value, forecast_values),
      type = c(rep("Historical", nrow(historical)), rep("Forecast", length(forecast_obj$mean)))
    )
    
    plot_ly(plot_data, x = ~year, y = ~value, color = ~type,
            colors = c("Historical" = app_colors[4], "Forecast" = app_colors[5]),
            type = 'scatter', mode = 'lines+markers',
            line = list(width = 2.5), marker = list(size = 6)) %>%
      layout(
        title = paste("Nutrition Forecast:", tools::toTitleCase(indicator)),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste(tools::toTitleCase(indicator), "Rate (%)")),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1)
      )
  })
  
  # Risk assessment chart
  output$risk_assessment <- renderPlotly({
    df <- current_year_data()
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for risk assessment",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE, font = list(size = 16)
        ) %>%
        layout(title = "District Risk Assessment")
      return(p)
    }
    
    # Calculate risk scores based on multiple indicators
    risk_data <- df %>%
      group_by(district, province) %>%
      summarise(
        avg_stunting = mean(stunting, na.rm = TRUE),
        avg_wasting = mean(wasting, na.rm = TRUE),
        avg_anemia = mean(anemia, na.rm = TRUE),
        avg_poverty = mean(poverty_rate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        risk_score = (avg_stunting * 0.4 + avg_wasting * 0.3 + avg_anemia * 0.2 + avg_poverty * 0.1),
        risk_category = case_when(
          risk_score > 40 ~ "High Risk",
          risk_score > 25 ~ "Medium Risk", 
          TRUE ~ "Low Risk"
        )
      ) %>%
      arrange(desc(risk_score)) %>%
      slice_head(n = 10) # Limit to top 10 for clarity
    
    # Create scatter plot
    p <- plot_ly(risk_data, x = ~avg_stunting, y = ~avg_wasting, 
                 color = ~risk_category, colors = c("High Risk" = app_colors[5], "Medium Risk" = app_colors[9], "Low Risk" = app_colors[2]),
                 size = ~avg_poverty, sizes = c(10, 50),
                 text = ~paste("District:", district, "<br>City:", province, 
                              "<br>Risk Score:", round(risk_score, 1),
                              "<br>Stunting:", round(avg_stunting, 1), "%",
                              "<br>Wasting:", round(avg_wasting, 1), "%"),
                 hoverinfo = 'text',
                 type = 'scatter', mode = 'markers') %>%
      layout(
        title = "District Risk Matrix",
        xaxis = list(title = "Stunting Rate (%)"),
        yaxis = list(title = "Wasting Rate (%)"),
        legend = list(title = "Risk Category")
      )
    
    p
  })
  
  # ROI Analysis
  output$roi_analysis <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No data available for ROI analysis</p>"))
    }
    
    # Calculate basic ROI metrics
    total_districts <- length(unique(df$district))
    total_cities <- length(unique(df$province))
    avg_stunting <- round(mean(df$stunting, na.rm = TRUE), 1)
    avg_wasting <- round(mean(df$wasting, na.rm = TRUE), 1)
    
    # Simulate ROI calculations
    budget <- input$intervention_budget %||% 500000
    intervention_impact <- (input$intervention_strength %||% 0)
    
    estimated_reduction <- (avg_stunting * intervention_impact / 100)
    cost_per_reduction <- if(estimated_reduction > 0) budget / estimated_reduction else 0
    
    HTML(glue(
    "<div class='metric-card'>
      <h6 style='text-align:center;'><i class='fa fa-calculator'></i> ROI Analysis Summary</h6>
      <div class='row'>
        <div class='col-6'>
          <strong>Total Budget:</strong> ${format(budget, big.mark = ',')}<br> 
          <strong>Districts Covered:</strong> {total_districts}<br>
          <strong>Cities Covered:</strong> {total_cities}<br>
        </div>
        <div class='col-6'>
          <strong>Current Stunting:</strong> {avg_stunting}%<br>
          <strong>Estimated Reduction:</strong> {round(estimated_reduction, 1)}%<br>
          <strong>Cost per Reduction:</strong> ${format(round(cost_per_reduction), big.mark = ',')}<br>
        </div>
      </div>
      <div class='alert alert-info mt-3'>
        <strong>Investment Priority:</strong> Focus on high-stunting countries first for maximum impact.
      </div>
    </div>
    "))
  })
  
  # Prediction insights
  output$prediction_insights <- renderUI({
    indicator <- input$pred_indicator %||% "stunting"
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No insights available</p>"))
    }
    
    # Calculate insights based on current data
    current_year <- max(df$year, na.rm = TRUE)
    current_data_for_insight <- df %>% filter(year == current_year)
    
    avg_stunting <- round(mean(current_data_for_insight$stunting, na.rm = TRUE), 1)
    high_stunting_districts <- current_data_for_insight %>%
      group_by(district) %>%
      summarise(avg_stunting_per_district = mean(stunting, na.rm = TRUE)) %>%
      filter(avg_stunting_per_district > 30) %>%
      nrow()
    
    HTML(glue(
    "<div class='alert alert-success'>
      <h6><i class='fa fa-lightbulb'></i> Key Insights</h6>
      <ul>
        <li>Current average stunting rate: <strong>{avg_stunting}%</strong></li>
        <li>Districts with high stunting (>30%): <strong>{high_stunting_districts}</strong></li>
        <li>Trend: {ifelse(avg_stunting < 30, 'Improving', 'Needs attention')}</li>
        <li>Recommended intervention: {ifelse(avg_stunting < 25, 'Maintain current programs', 'Scale up nutrition programs')}</li>
      </ul>
    </div>
    "))
  })
  
  # ==============================================================================
  # POLICY & IMPACT ASSESSMENT OUTPUTS
  # ==============================================================================
  
  # Intervention effectiveness chart
  output$intervention_effectiveness_chart <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data for analysis."))
    }
    
    # Define phases and calculate effectiveness
    phase_summary <- df %>%
      mutate(
        phase = case_when(
          year <= 2008 ~ "Phase 1 (2000-08)",
          year <= 2016 ~ "Phase 2 (2009-16)",
          TRUE ~ "Phase 3 (2017-24)"
        )
      ) %>%
      group_by(phase) %>%
      summarise(
        start_rate = mean(stunting[year == min(year)], na.rm = TRUE),
        end_rate = mean(stunting[year == max(year)], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        reduction = round(start_rate - end_rate, 1)
      ) %>%
      filter(!is.na(reduction))

    plot_ly(phase_summary, x = ~phase, y = ~reduction, type = 'bar',
            marker = list(color = app_colors[2]),
            text = ~paste0(reduction, " p.p."),
            textposition = 'auto',
            hoverinfo = 'text',
            hovertemplate = '<b>%{x}</b><br>Reduction: %{y} p.p.<extra></extra>') %>%
      layout(
        title = list(text = "Stunting Reduction by Intervention Phase", x = 0.5),
        xaxis = list(title = ""),
        yaxis = list(title = "Reduction (Percentage Points)")
      )
  })
  
  # Resource allocation chart
  output$resource_allocation <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for resource allocation",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = "Resource Allocation Analysis")
      return(p)
    }
    
    # Calculate resource allocation priorities
    allocation_data <- df %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      group_by(district) %>%
      summarise(
        stunting = mean(stunting, na.rm = TRUE),
        wasting = mean(wasting, na.rm = TRUE),
        anemia = mean(anemia, na.rm = TRUE),
        poverty = mean(poverty_rate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        priority_score = (stunting * 0.4 + wasting * 0.3 + anemia * 0.2 + poverty * 0.1),
        priority_level = case_when(
          priority_score > 35 ~ "High Priority",
          priority_score > 25 ~ "Medium Priority",
          TRUE ~ "Low Priority"
        )
      ) %>%
      arrange(desc(priority_score)) %>%
      slice_head(n = 10)
    
    # Correctly create the bar chart with colors mapped to priority levels
    p <- plot_ly(allocation_data, 
                 x = ~reorder(district, priority_score), 
                 y = ~priority_score,
                 color = ~priority_level, 
                 colors = c("High Priority" = app_colors[5], "Medium Priority" = app_colors[9], "Low Priority" = app_colors[2]), 
                 type = 'bar') %>%
      layout(
        title = "Resource Allocation Priorities (Top 10 Districts)",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Priority Score"),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.3)
      )
    
    p
  })
  
  # SDG Progress tracking
  output$sdg_progress <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No data available for SDG progress</p>"))
    }
    
    # Calculate SDG targets progress
    current_year <- max(df$year, na.rm = TRUE)
    current_data <- df %>% filter(year == current_year)
    
    avg_stunting <- mean(current_data$stunting, na.rm = TRUE)
    avg_wasting <- mean(current_data$wasting, na.rm = TRUE)
    
    # SDG Target 2.2: End all forms of malnutrition by 2030
    # Using a more realistic baseline for progress calculation
    stunting_start <- 45 # From 2000
    stunting_target <- 19 # Rwanda's 2024 target
    stunting_progress <- min(100, max(0, ((stunting_start - avg_stunting) / (stunting_start - stunting_target)) * 100))
    
    poverty_start <- 60 # From 2000
    poverty_target <- 20 # Hypothetical target
    avg_poverty <- mean(current_data$poverty_rate, na.rm = TRUE)
    poverty_progress <- min(100, max(0, ((poverty_start - avg_poverty) / (poverty_start - poverty_target)) * 100))
    
    HTML(glue(
    "<div class='metric-card'>
      <h5><i class='fa fa-globe'></i> SDG Progress Tracking</h6>
      <div class='row'>
        <div class='col-6'>
          <h6>SDG 2: Zero Hunger</h6>
          <div class='progress mb-2'>
            <div class='progress-bar' style='width: {round(stunting_progress)}%'>
              Progress: {round(stunting_progress)}%
            </div>
          </div>
          <h6>SDG 1: No Poverty</h6>
          <div class='progress'>
            <div class='progress-bar bg-success' style='width: {round(poverty_progress)}%'>
              Progress: {round(poverty_progress)}%
            </div>
          </div>
        </div>
        <div class='col-6'>
          <h6>Current Status</h6>
          <p><strong>Stunting:</strong> {round(avg_stunting, 1)}% (Target: <5%)</p>
          <p><strong>Wasting:</strong> {round(avg_wasting, 1)}% (Target: <3%)</p>
          <p><strong>Years to 2030:</strong> {2030 - current_year}</p>
        </div>
      </div>
      <div class='alert alert-info'>
        <strong>Recommendation:</strong> {ifelse(avg_stunting > 20, 'Accelerate interventions', 'Maintain current progress')}
      </div>
    </div>
    "))
  })
  
  # ==============================================================================
  # IMPACT ASSESSMENT OUTPUTS
  # ==============================================================================
  
  # Total children impacted
  output$total_children <- renderInfoBox({ # Corrected: Made number readable
    df <- filtered_data()
    if(nrow(df) == 0) {
      total_children_u5 <- 0
    } else {
      # Simplified but more stable estimation based on number of districts in selection
      total_children_u5 <- n_distinct(df$district) * 20000 # Approx. 20k under 5 per district
    }
    
    infoBox(
      get_text("children_impacted", current_lang()),
      format(total_children_u5, big.mark = ",", scientific = FALSE),
      subtitle = get_text("estimated_beneficiaries", current_lang()),
      icon = icon("child"), 
      color = "blue",
      fill = TRUE
    )
  })
  
  # Districts improved
  output$districts_improved <- renderInfoBox({
    req(input$selected_year)
    current_year <- as.numeric(input$selected_year)
    previous_year <- current_year - 1
    
    # Use a broader dataset that respects filters but includes two years for comparison
    comparison_df <- full_data %>%
      filter(year %in% c(current_year, previous_year)) %>%
      filter(if (input$selected_province != "All Cities") province == input$selected_province else TRUE) %>%
      filter(if (input$selected_age_group != "All Age Groups") age_group == input$selected_age_group else TRUE) %>%
      filter(if (input$selected_nutrient != "All Nutrients") nutrient == input$selected_nutrient else TRUE)
      
    if(nrow(comparison_df) == 0) {
      improved <- 0
    } else {
      # Calculate the number of districts with improved stunting
      trend_comparison <- comparison_df %>%
        group_by(district) %>%
        summarise(
          current_val = mean(stunting[year == current_year], na.rm = TRUE),
          previous_val = mean(stunting[year == previous_year], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(!is.na(current_val) & !is.na(previous_val))
      
      improved <- sum(trend_comparison$current_val < trend_comparison$previous_val, na.rm = TRUE)
    }
    
    infoBox(
      get_text("districts_improved", current_lang()), 
      format(improved, big.mark = ","), 
      subtitle = paste("vs.", previous_year),
      icon = icon("arrow-up"), 
      color = "green",
      fill = TRUE
    )
  })
  
  # Average improvement
  output$avg_improvement <- renderInfoBox({
    req(input$selected_year)
    current_year <- as.numeric(input$selected_year)
    previous_year <- current_year - 1
    
    # Use a broader dataset that respects filters but includes two years for comparison
    comparison_df <- full_data %>%
      filter(year %in% c(current_year, previous_year)) %>%
      filter(if (input$selected_province != "All Cities") province == input$selected_province else TRUE) %>%
      filter(if (input$selected_age_group != "All Age Groups") age_group == input$selected_age_group else TRUE) %>%
      filter(if (input$selected_nutrient != "All Nutrients") nutrient == input$selected_nutrient else TRUE)
      
    if(nrow(comparison_df) < 2) {
      improvement <- 0
    } else {
      avg_trend <- comparison_df %>%
        group_by(year) %>%
        summarise(avg_stunting = mean(stunting, na.rm = TRUE), .groups = 'drop') %>%
        pivot_wider(names_from = year, values_from = avg_stunting)
      
      improvement <- if(as.character(previous_year) %in% names(avg_trend) && as.character(current_year) %in% names(avg_trend)) {
        round(avg_trend[[as.character(previous_year)]] - avg_trend[[as.character(current_year)]], 1)
      } else { 0 }
    }
    infoBox(
      get_text("avg_improvement", current_lang()),
      paste0(improvement, "%"),
      subtitle = get_text("stunting_reduction", current_lang()),
      icon = icon("chart-line"), 
      color = "yellow",
      fill = TRUE
    )
  })
  
  # Cost per child
  output$cost_per_child <- renderInfoBox({
    total_budget <- 50000000 # Example total budget for nutrition programs
    df <- filtered_data()
    total_children_u5 <- n_distinct(df$district) * 20000 # Approx. 20k under 5 per district

    if(nrow(df) == 0 || total_children_u5 == 0) {
      cost <- 0
    } else {
      cost <- round(total_budget / total_children_u5, 0)
    }
    
    infoBox(
      get_text("cost_per_child", current_lang()), 
      paste0("$", format(cost, big.mark = ",")),
      subtitle = "Estimated cost per child",
      icon = icon("dollar-sign"), 
      color = "red",
      fill = TRUE
    )
  })
  
  # Impact summary
  # Reactive value for average improvement to be shared between cards
  avg_improvement_reactive <- reactive({
    req(input$selected_year)
    current_year <- as.numeric(input$selected_year)
    previous_year <- current_year - 1
    
    comparison_df <- full_data %>%
      filter(year %in% c(current_year, previous_year)) %>%
      filter(if (input$selected_province != "All Cities") province == input$selected_province else TRUE) %>%
      filter(if (input$selected_age_group != "All Age Groups") age_group == input$selected_age_group else TRUE) %>%
      filter(if (input$selected_nutrient != "All Nutrients") nutrient == input$selected_nutrient else TRUE)
      
    if(nrow(comparison_df) < 2) return(0)
    
    avg_trend <- comparison_df %>%
      group_by(year) %>%
      summarise(avg_stunting = mean(stunting, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = year, values_from = avg_stunting)
      
    if(as.character(previous_year) %in% names(avg_trend) && as.character(current_year) %in% names(avg_trend)) round(avg_trend[[as.character(previous_year)]] - avg_trend[[as.character(current_year)]], 1) else 0
  })
  output$impact_summary <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No impact data available</p>"))
    }
    
    # Calculate impact metrics
    total_countries <- length(unique(df$district))
    total_cities <- length(unique(df$province))
    current_year <- max(df$year, na.rm = TRUE)
    current_data <- df %>% filter(year == current_year)
    
    avg_stunting <- round(mean(current_data$stunting, na.rm = TRUE), 1)
    avg_wasting <- round(mean(current_data$wasting, na.rm = TRUE), 1)
    
    HTML(glue(
    "<div class='metric-card'>
      <h5><i class='fa fa-chart-bar'></i> Impact Summary</h5>
      <div class='row' style='min-height: 120px;'>
        <div class='col-12'>
          <p><strong>Districts Covered:</strong> {total_countries}</p>
          <p><strong>Provinces Covered:</strong> {total_cities}</p>
          <p><strong>Current Stunting:</strong> {avg_stunting}%</p>
          <p><strong>Current Wasting:</strong> {avg_wasting}%</p>
        </div>
      </div>
      <div class='alert alert-success mt-3'>
        <strong>Impact Status:</strong> {ifelse(avg_stunting < 30, 'Positive trends observed', 'Intervention needed')}
      </div>
    </div>
    "))
  })

  # ==============================================================================
  # SOLUTIONS HUB UI OUTPUTS
  # ==============================================================================
  
  # ==============================================================================
  # REGIONAL & RANKINGS UI OUTPUTS
  # ==============================================================================
  
  output$regional_trends_title_ui <- renderUI({ get_text("regional_trends_comparison", current_lang()) })
  output$regional_rank_title_ui <- renderUI({ get_text("rwanda_regional_rank", current_lang()) })
  output$progress_metrics_title_ui <- renderUI({ get_text("progress_metrics", current_lang()) })
  output$country_comparison_title_ui <- renderUI({ get_text("country_comparison_dashboard", current_lang()) })
  output$top_bottom_title <- renderUI({ get_text("top_bottom_performers", current_lang()) })
  output$perf_over_time_title <- renderUI({ get_text("performance_over_time", current_lang()) })
  
  # ==============================================================================
  # PREDICTIVE & DATA EXPLORER UI OUTPUTS
  # ==============================================================================
  
  output$pred_config_title_ui <- renderUI({ get_text("predictive_analytics_config", current_lang()) })
  output$intervention_scenarios_title_ui <- renderUI({ get_text("intervention_scenarios", current_lang()) })
  output$forecast_chart_title_ui <- renderUI({ get_text("forecast_chart_title", current_lang()) })
  output$risk_assessment_title_ui <- renderUI({ get_text("risk_assessment_title", current_lang()) })
  
  output$pred_target_ui <- renderUI({
    selectInput("pred_target", get_text("target", current_lang()), choices = c("National", unique(full_data$district)))
  }) # Corrected: Call reactive full_data()
  output$pred_indicator_ui <- renderUI({
    selectInput("pred_indicator", get_text("indicator", current_lang()), 
                choices = setNames(c("stunting", "wasting", "anemia"), c(get_text("stunting", current_lang()), get_text("wasting", current_lang()), get_text("anemia", current_lang()))))
  })
  output$pred_horizon_ui <- renderUI({
    numericInput("pred_horizon", get_text("forecast_horizon", current_lang()), value = 5, min = 1, max = 10)
  })
  output$intervention_impact_ui <- renderUI({
    sliderInput("intervention_strength", get_text("intervention_impact", current_lang()),
                value = 0, min = 0, max = 50, step = 5)
  })
  output$intervention_budget_ui <- renderUI({
    numericInput("intervention_budget", get_text("available_budget", current_lang()), value = 500000, min = 50000)
  })
  
  
  
  output$run_prediction_ui <- renderUI({
    actionButton("run_prediction", get_text("run_forecast", current_lang()), class = "btn-success btn-block", icon = icon("magic"))
  })
  
  output$data_explorer_title_ui <- renderUI({ get_text("data_explorer_title", current_lang()) })
  output$apply_explorer_filters_ui <- renderUI({
    actionButton("apply_explorer_filters", get_text("apply_filters", current_lang()), class = "btn-primary btn-block")
  })
  
  # ==============================================================================
  # DOWNLOAD HANDLERS
  # ==============================================================================
  
  # Download trends chart
  output$download_trends <- downloadHandler( # Corrected: Made download work
    filename = function() {
      paste0("rwanda_nutrition_trends_", Sys.Date(), ".csv")
    },
    content = function(file) {
      trends_data <- trend_filtered_data() %>%
        group_by(year) %>%
        summarise(
          stunting = round(mean(stunting, na.rm = TRUE), 1),
          wasting = round(mean(wasting, na.rm = TRUE), 1),
          anemia = round(mean(anemia, na.rm = TRUE), 1),
          .groups = 'drop'
        )
      write.csv(trends_data, file, row.names = FALSE)
      showNotification("Trends data downloaded as CSV.", type = "message")
    }
  )
  
  # Download rankings
  output$download_rankings <- downloadHandler(
    filename = function() {
      paste0("district_rankings_", input$ranking_indicator, "_", input$selected_year, ".csv")
    },
    content = function(file) {
      rankings <- rankings_data()
      write.csv(rankings, file, row.names = FALSE)
    }
  )
  
  # ==============================================================================
  # AI POLICY ADVISOR OUTPUTS
  # ==============================================================================
  
  output$policy_brief_title_ui <- renderUI({ get_text("policy_brief_title", input$selected_language) })
  # Dynamic titles for Policy Advisor page
  output$policy_advisor_title_ui <- renderUI({ get_text("policy_advisor_title", input$selected_language) })
  output$roadmap_title_ui <- renderUI({ get_text("roadmap_title", input$selected_language) })
  output$success_prob_title_ui <- renderUI({ get_text("success_prob_title", input$selected_language) })
  output$download_pdf_btn_ui <- renderUI({ downloadButton("download_policy_pdf", get_text("download_pdf", input$selected_language), class = "btn-warning") })
  output$download_ppt_btn_ui <- renderUI({ downloadButton("download_policy_ppt", get_text("download_ppt", input$selected_language), class = "btn-info") })
  
  # AI Policy Recommendation
  observeEvent(input$generate_policy, { # Corrected: Made notification dynamic
    # Show loading notification
    showNotification("ðŸ¤– AI is analyzing your data and generating policy recommendations...",
                     type = "default", duration = 5)
    
    # Get current data context
    df <- current_year_data()
    
    # Prepare context for AI
    context <- list(
      district = if (input$policy_target == "District Specific") input$policy_district else "National", # Corrected: Use "District Specific"
      year = input$selected_year %||% max(full_data$year),
      focus_area = input$policy_focus_area,
      target = input$policy_target,
      timeline = input$policy_timeline,
      budget = input$policy_budget,
      lang = current_lang()
    )
    
    # Add nutrition indicators if data is available
    if (nrow(df) > 0) { # df is already filtered_data()
      context$stunting_rate <- round(mean(df$stunting, na.rm = TRUE), 1)
      context$wasting_rate <- round(mean(df$wasting, na.rm = TRUE), 1)
      context$anemia_rate <- round(mean(df$anemia, na.rm = TRUE), 1)
      context$poverty_rate <- round(mean(df$poverty_rate, na.rm = TRUE), 1)
    }
    
    # Generate AI recommendation
    ai_recommendation <- tryCatch({
      generate_policy_recommendation(ai_client, context)
    }, error = function(e) {
      cat("AI Policy Generation Error: ", e$message, "\n")
      # Return a user-friendly error message
      HTML(paste("<div class='alert alert-danger'><h4>Error Generating Policy</h4>",
                 "<p>Could not connect to the AI service. Please check the API key and network connection.</p>",
                 "<p>Details: ", e$message, "</p></div>"))
    })
    
    # Store in reactive value for output
    policy_recommendation(ai_recommendation)
  })
  
  # Initialize reactive value for policy recommendation
  policy_recommendation <- reactiveVal("Click 'Generate Report' to create evidence-based recommendations based on your current data filters. Note: This feature requires a Gemini API key. Please set the GEMINI_API_KEY environment variable.")
  
  # Render policy output
  output$policy_output <- renderUI({
    HTML(policy_recommendation() %||% "Click 'Generate' to see recommendations.")
  })
  
  # AI Dashboard Insights
  output$ai_insights <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No data available for AI analysis</p>"))
    }
    
    # Generate AI insights
    insights <- generate_dashboard_insights(df, ai_client)
    
    HTML(paste0(
      "<div class='ai-insights'>",
      "<h5><i class='fa fa-brain'></i> AI Insights</h5>",
      "<div class='insight-section'>",
      "<h6>Summary</h6>",
      "<p>", insights$summary, "</p>",
      "</div>",
      "<div class='insight-section'>",
      "<h6>Trends</h6>",
      "<p>", insights$trends, "</p>",
      "</div>",
      "<div class='insight-section'>",
      "<h6>Recommendations</h6>",
      "<p>", insights$recommendations, "</p>",
      "</div>",
      "</div>"
    ))
  })
  
  # Download policy PDF
  output$download_policy_pdf <- downloadHandler(
    filename = function() {
      paste0("AI_Policy_Brief_", input$policy_focus_area, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Create HTML file with policy recommendation
      html_content <- policy_recommendation() %||% "<h3>No policy generated.</h3>"
      full_html <- paste0(
        "<!DOCTYPE html>",
        "<html><head><title>AI Policy Brief</title>",
        "<style>body{font-family:Arial,sans-serif;margin:40px;}",
        ".ai-recommendation{border:1px solid #ddd;padding:20px;border-radius:8px;}",
        ".recommendation-section{margin:15px 0;}",
        "</style></head><body>",
        html_content,
        "</body></html>"
      )
      writeLines(full_html, file)
    }
  )
  
  # Download policy PowerPoint
  output$download_policy_ppt <- downloadHandler(
    filename = function() {
      paste0("AI_Policy_Brief_", input$policy_focus_area, "_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      showNotification("PowerPoint export requires additional setup with officer package", 
                       type = "info", duration = 5)
    }
  )
  
  # ==============================================================================
  # ENGAGING DOCS UI OUTPUTS
  # ==============================================================================
  output$engaging_docs_title_ui <- renderUI({ get_text("engaging_docs_title", current_lang()) })
  output$docs_evidence_title_ui <- renderUI({ get_text("docs_evidence_title", current_lang()) })
  output$docs_evidence_body_ui <- renderUI({ get_text("docs_evidence_body", current_lang()) })
  output$docs_source_nisr_ui <- renderUI({ HTML(get_text("docs_source_nisr", current_lang())) })
  output$docs_source_unicef_ui <- renderUI({ HTML(get_text("docs_source_unicef", current_lang())) })
  output$docs_source_gnr_ui <- renderUI({ HTML(get_text("docs_source_gnr", current_lang())) })
  output$docs_source_media_ui <- renderUI({ HTML(get_text("docs_source_media", current_lang())) })
  output$key_figures_title_ui <- renderUI({ get_text("key_figures_title", current_lang()) })
  output$key_figure_stunting_ui <- renderUI({ get_text("key_figure_stunting", current_lang()) })
  output$key_figure_anemia_ui <- renderUI({ get_text("key_figure_anemia", current_lang()) })
  output$key_figure_diet_ui <- renderUI({ get_text("key_figure_diet", current_lang()) })
  output$limitations_title_ui <- renderUI({ get_text("limitations_title", current_lang()) })
  output$limitation_refresh_ui <- renderUI({ get_text("limitation_refresh", current_lang()) })
  output$limitation_hidden_hunger_ui <- renderUI({ get_text("limitation_hidden_hunger", current_lang()) })
  output$way_forward_title_ui <- renderUI({ get_text("way_forward_title", current_lang()) })
  output$multimedia_title_ui <- renderUI({ get_text("multimedia_title", current_lang()) })
  output$way_forward_real_time_ui <- renderUI({ get_text("way_forward_real_time", current_lang()) })
  output$way_forward_predictive_ui <- renderUI({ get_text("way_forward_predictive", current_lang()) })
  output$way_forward_equity_ui <- renderUI({ get_text("way_forward_equity", current_lang()) })
  output$way_forward_intro_ui <- renderUI({ "This dashboard is the first bridge: by making nutrition data visible, accessible, and actionable, it supports Rwanda’s transition from data → insight → policy → results. Future enhancements will include:" })
  
  output$documentation_image_ui <- renderUI({
    tags$img(src = "https://tse2.mm.bing.net/th/id/OIP.zh2_NHICcbLkM6AL9gMiVgHaEK?rs=1&pid=ImgDetMain&o=7&rm=3",
             alt = "Children in a Rwandan community",
             style = "width: 100%; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
  })
  
  output$multimedia_section_ui <- renderUI({
    tagList(
      h3(uiOutput("multimedia_title_ui")),
      h5("Key Statistical Reports"),
      tags$a(href="https://www.statistics.gov.rw/datasource/demographic-and-health-survey-2019-20", target="_blank", class="btn btn-primary btn-block", "DHS 2019-20 Full Report", style="margin-bottom:10px;"),
      tags$a(href="https://www.unicef.org/rwanda/reports/rwanda-global-nutrition-report-2021", target="_blank", class="btn btn-info btn-block", "Global Nutrition Report 2021", style="margin-bottom:15px;"),
      
      # YouTube Video 1
      h5("Optimizing Early Life Nutrition"),
      tags$iframe(width="100%", height="240", src="https://www.youtube.com/embed/MJSRb9DBqT4", frameborder="0", style="border-radius: 8px; margin-bottom: 15px;"),
      # YouTube Video 2
      h5("Fighting Malnutrition with Micronutrient Powders"),
      tags$iframe(width="100%", height="240", src="https://www.youtube.com/embed/i5XtUptLqPU", frameborder="0", style="border-radius: 8px;")
    )
  })
  
  
  
  # Download policy PowerPoint
  output$download_policy_ppt <- downloadHandler(
    filename = function() {
      paste0("AI_Policy_Brief_", input$policy_focus_area, "_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      showNotification("PowerPoint export requires additional setup with officer package", 
                       type = "info", duration = 5)
    }
  )
  
  # Implementation Roadmap Chart
  output$implementation_roadmap <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for implementation roadmap",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = "Implementation Roadmap")
      return(p)
    }
    
    # Create implementation phases based on timeline
    timeline <- input$policy_timeline %||% 12
    phases <- data.frame( 
      phase = c("Phase 1: Planning", "Phase 2: Implementation", "Phase 3: Monitoring", "Phase 4: Evaluation"),
      start_month = c(1, timeline * 0.25, timeline * 0.5, timeline * 0.75),
      end_month = c(timeline * 0.25, timeline * 0.5, timeline * 0.75, timeline),
      progress = c(100, 75, 50, 25),
      color = c(app_colors[2], app_colors[4], app_colors[9], app_colors[5])
    )
    
    p <- plot_ly(phases, x = ~start_month, y = ~progress, color = ~phase, 
                 colors = ~color, type = 'bar', orientation = 'v',
                 text = ~paste("Phase:", phase, "<br>Duration:", round(end_month - start_month, 1), "months"), hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = "Policy Implementation Roadmap",
        xaxis = list(title = "Timeline (Months)", range = c(0, timeline)),
        yaxis = list(title = "Progress (%)", range = c(0, 100)),
        showlegend = FALSE,
        bargap = 0.1
      )
    
    p
  })
  
  # Success Probability Chart
  output$success_probability <- renderPlotly({
    # Calculate success factors based on data
    budget_factor <- min(100, max(20, (input$policy_budget %||% 1000000) / 50000))
    timeline_factor <- min(100, max(30, (input$policy_timeline %||% 12) * 5))
    data_quality <- min(100, max(40, nrow(filtered_data()) / 100))
    
    factors <- data.frame( # This is fine
      factor = c("Budget Adequacy", "Timeline Feasibility", "Data Quality"),
      score = c(budget_factor, timeline_factor, data_quality)
    )
    
    plot_ly(
      data = factors,
      type = 'scatterpolar',
      r = ~score,
      theta = ~factor,
      fill = 'toself',
      mode = 'lines+markers',
      fillcolor = 'rgba(67, 29, 133, 0.4)', # A nice purple from the palette
      marker = list(color = app_colors[4], size = 8),
      line = list(color = app_colors[4], width = 2)
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            showline = FALSE,
            showticklabels = FALSE
          )
        ),
        showlegend = FALSE,
        title = list(text = "Success Factor Analysis", x = 0.5)
      )
  })
  
  # Success Probability Interpretation
  output$success_prob_interpretation <- renderUI({
    budget_factor <- min(100, max(20, (input$policy_budget %||% 1000000) / 50000))
    timeline_factor <- min(100, max(30, (input$policy_timeline %||% 12) * 5))
    data_quality <- min(100, max(40, nrow(filtered_data()) / 100))
    
    tags$ul(
      tags$li(
        strong("Budget Adequacy:"), 
        glue(" Your score is {round(budget_factor)}. This measures if the budget is sufficient for the scale of the problem.")
      ),
      tags$li(
        strong("Timeline Feasibility:"), 
        glue(" Your score is {round(timeline_factor)}. This assesses if the implementation timeline is realistic.")
      ),
      tags$li(
        strong("Data Quality:"), 
        glue(" Your score is {round(data_quality)}. This reflects the amount of available data to inform the policy.")
      )
    )
  })
  # Multi-Indicator Trends
  output$multi_indicator_trends <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for multi-indicator trends",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = "Multi-Indicator Trends")
      return(p)
    }
    
    # Get selected indicators
    selected_indicators <- input$trend_indicators %||% c("stunting", "wasting", "anemia")
    if ("All Indicators" %in% selected_indicators) {
      selected_indicators <- c("stunting", "wasting", "anemia", "poverty_rate")
    }
    
    # Calculate trends for each indicator
    trend_data <- df %>%
      group_by(year) %>%
      summarise(
        across(all_of(selected_indicators), ~mean(.x, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = -year, names_to = "indicator", values_to = "value")
    
    # Create color palette
    colors <- c("stunting" = app_colors[5], "wasting" = app_colors[9], "anemia" = app_colors[3], "poverty_rate" = app_colors[2])
    
    p <- plot_ly(trend_data, x = ~year, y = ~value, color = ~indicator,
                 colors = colors, type = 'scatter', mode = 'lines+markers',
                 line = list(width = 2.5), marker = list(size = 6),
                 text = ~paste("Indicator:", indicator, "<br>Year:", year, "<br>Value:", round(value, 1), "%"),
                 hoverinfo = 'text') %>%
      layout(
        title = "Multi-Indicator Trends Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1)
      )
    
    p
  })
  
  # Trend Comparison
  output$trend_comparison <- renderPlotly({
    df <- filtered_data() # This reactive already calls full_data()
    
    if (nrow(df) == 0) {
      p <- plot_ly() %>%
        add_annotations(
          text = "No data available for trend comparison",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = "Trend Comparison")
      return(p)
    }
    
    # Compare top 5 countries by latest year stunting rate
    latest_year <- max(df$year, na.rm = TRUE)
    top_countries <- df %>%
      filter(year == latest_year) %>%
      group_by(district) %>%
      summarise(stunting = mean(stunting, na.rm = TRUE), .groups = 'drop') %>%
      arrange(stunting) %>%
      slice_head(n = 5) %>%
      pull(district)
    
    comparison_data <- df %>%
      filter(district %in% top_countries) %>%
      group_by(year, district) %>%
      summarise(stunting = mean(stunting, na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(comparison_.data, x = ~year, y = ~stunting, color = ~district,
                 type = 'scatter', mode = 'lines+markers',
                 line = list(width = 2), marker = list(size = 4),
                 text = ~paste("Country:", district, "<br>Year:", year, "<br>Stunting:", round(stunting, 1), "%"),
                 hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        title = "Stunting Trends: Top 5 Districts",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Stunting Rate (%)"),
        legend = list(title = "Countries")
      )
    
    p
  })
  
  # Trend Insights
  output$trend_insights <- renderUI({
    req(input$trend_indicator, input$trend_district)
    
    district <- input$trend_district
    indicator <- input$trend_indicator
    
    trend_data_raw <- if (district == "National Average") {
      full_data %>%
        group_by(year) %>%
        summarise(value = mean(get(indicator), na.rm = TRUE), .groups = 'drop')
    } else {
      full_data %>%
        filter(district == !!district) %>%
        group_by(year) %>%
        summarise(value = mean(get(indicator), na.rm = TRUE), .groups = 'drop')
    }
    
    if(nrow(trend_data_raw) < 2) return(p("Not enough data for insights."))
    
    # Calculations
    start_year <- min(trend_data_raw$year)
    end_year <- max(trend_data_raw$year)
    start_val <- round(trend_data_raw$value[trend_data_raw$year == start_year], 1)
    end_val <- round(trend_data_raw$value[trend_data_raw$year == end_year], 1)
    total_change <- round(end_val - start_val, 1)
    
    fit <- lm(value ~ year, data = trend_data_raw)
    slope <- round(coef(fit)[2], 2)
    
    trend_direction <- if(slope < -0.1) "Improving" else if (slope > 0.1) "Worsening" else "Stable"
    trend_color <- if(trend_direction == "Improving") "#10b981" else if (trend_direction == "Worsening") "#ef4444" else "#6b7280"
    trend_icon <- if(trend_direction == "Improving") "fa-arrow-down" else if (trend_direction == "Worsening") "fa-arrow-up" else "fa-minus"
    
    HTML(glue(
      "<div class='trend-insights'>
        <h5><i class='fa fa-search-chart'></i> Analysis for {tools::toTitleCase(indicator)} in {district}</h5>
        
        <div class='insight-card'>
          <h6><i class='fas fa-calendar-alt'></i> Overall Change ({start_year} to {end_year})</h6>
          <p>The rate changed from <strong>{start_val}%</strong> to <strong>{end_val}%</strong>, a total change of <strong>{total_change}</strong> percentage points.</p>
        </div>
        
        <div class='insight-card'>
          <h6 style='color: {trend_color};'><i class='fas {trend_icon}'></i> Trend Assessment</h6>
          <p>The average annual change is <strong>{slope}</strong> points per year, indicating a <strong style='color:{trend_color};'>{trend_direction}</strong> trend.</p>
        </div>
      </div>
    "))
  })
  
}
# ==============================================================================
# 🔹 TRANSLATED UI TITLES
# ==============================================================================


# ==============================================================================
# 🔹 ADDITIONAL HELPER FUNCTIONS
# ==============================================================================

# Additional helper functions for enhanced features
generate_ai_policy_brief <- function(focus_area, target, budget, timeline) {
  # Function body goes here
  # AI-powered policy recommendation generator
  # This would integrate with actual AI services in production
  
  base_recommendations <- list(
    "Stunting Reduction" = glue(
      "<div class='alert alert-info'>
        <h4><i class='fa fa-lightbulb'></i> AI-Generated Policy Brief: Stunting Reduction</h4>
        
        <h5>Executive Summary</h5>
        <p>Based on analysis of Rwanda's DHS data and regional best practices, a comprehensive 
        stunting reduction strategy targeting {target} is recommended with a {timeline}-month implementation timeline.</p>
        
        <h5>Priority Interventions (Budget: ${format(budget, big.mark = ',')})</h5>
        <ol>
          <li><strong>First 1000 Days Program</strong> (40% of budget)
            <ul>
              <li>Prenatal nutrition counseling and supplements</li>
              <li>Exclusive breastfeeding promotion</li>
              <li>Complementary feeding education</li>
            </ul>
          </li>
          <li><strong>Community Health Worker Training</strong> (25% of budget)
            <ul>
              <li>Growth monitoring and promotion</li>
              <li>Nutrition counseling skills</li>
              <li>Early detection of malnutrition</li>
            </ul>
          </li>
          <li><strong>Food Security and Agriculture</strong> (35% of budget)
            <ul>
              <li>Kitchen gardens and nutrition-sensitive agriculture</li>
              <li>Food fortification programs</li>
              <li>Seasonal food security interventions</li>
            </ul>
          </li>
        </ol>
        
        <h5>Expected Outcomes</h5>
        <ul>
          <li>8-12 percentage point reduction in stunting over {timeline} months</li>
          <li>Improved dietary diversity scores by 25%</li>
          <li>Enhanced community knowledge and practices</li>
          <li>Strengthened health system capacity</li>
        </ul>
        
        <div class='alert alert-warning'>
          <strong>Critical Success Factors:</strong> Political commitment, inter-sectoral coordination,
          community engagement, regular monitoring and evaluation, and sustainable financing mechanisms.
        </div>
      </div>
    "),
    
    "Wasting Prevention" = glue(
      "<div class='alert alert-info'>
        <h4><i class='fa fa-lightbulb'></i> AI-Generated Policy Brief: Wasting Prevention</h4>
        
        <h5>Executive Summary</h5>
        <p>A targeted wasting prevention strategy for {target} with immediate action protocols within {timeline} months.</p>
        
        <h5>Priority Interventions (Budget: ${format(budget, big.mark = ',')})</h5>
        <ol>
          <li><strong>Emergency Response & Screening</strong> (30% of budget)
            <ul>
              <li>Rapid nutritional assessment systems</li>
              <li>Early warning mechanisms for acute malnutrition</li>
              <li>Mobile screening clinics</li>
            </ul>
          </li>
          <li><strong>Therapeutic Feeding Programs</strong> (40% of budget)
            <ul>
              <li>Supplementary feeding centers</li>
              <li>Fortified food distribution</li>
              <li>Micronutrient supplementation</li>
            </ul>
          </li>
          <li><strong>WASH and Hygiene Interventions</strong> (30% of budget)
            <ul>
              <li>Water, Sanitation and Hygiene promotion</li>
              <li>Disease prevention programs</li>
              <li>Community health education</li>
            </ul>
          </li>
        </ol>
        
        <h5>Expected Outcomes</h5>
        <ul>
          <li>50-70% reduction in acute malnutrition cases over {timeline} months</li>
          <li>Improved treatment success rates above 80%</li>
          <li>Reduced mortality in vulnerable populations</li>
          <li>Strengthened health system emergency capacity</li>
        </ul>
        
        <div class='alert alert-warning'>
          <strong>Critical Success Factors:</strong> Rapid response capability, adequate supply chains,
          trained personnel, community awareness, and continuous monitoring.
        </div>
      </div>
    "),
    
    "Anemia Control" = glue(
      "<div class='alert alert-info'>
        <h4><i class='fa fa-lightbulb'></i> AI-Generated Policy Brief: Anemia Control</h4>
        
        <h5>Executive Summary</h5>
        <p>A comprehensive anemia control strategy targeting women and children in {target} 
        implemented over {timeline} months to reduce iron deficiency and improve health outcomes.</p>
        
        <h5>Priority Interventions (Budget: ${format(budget, big.mark = ',')})</h5>
        <ol>
          <li><strong>Iron Supplementation Programs</strong> (35% of budget)
            <ul>
              <li>Iron fortified food distribution</li>
              <li>Iron supplement provision to pregnant women</li>
              <li>Adolescent girl supplementation programs</li>
            </ul>
          </li>
          <li><strong>Food Fortification & Dietary Improvement</strong> (40% of budget)
            <ul>
              <li>Staple food fortification (flour, oil, salt)</li>
              <li>Promotion of iron-rich foods</li>
              <li>Nutrition education programs</li>
            </ul>
          </li>
          <li><strong>Disease Control & Screening</strong> (25% of budget)
            <ul>
              <li>Malaria prevention and treatment</li>
              <li>Deworming programs</li>
              <li>Anemia screening and diagnosis</li>
            </ul>
          </li>
        </ol>
        
        <h5>Expected Outcomes</h5>
        <ul>
          <li>15-20 percentage point reduction in anemia prevalence over {timeline} months</li>
          <li>Improved maternal and child health indicators</li>
          <li>Increased school performance in children</li>
          <li>Enhanced productivity in working-age populations</li>
        </ul>
        
        <div class='alert alert-warning'>
          <strong>Critical Success Factors:</strong> Supply chain reliability, community engagement,
          health worker training, monitoring of compliance, and sustainable funding.
        </div>
      </div>
    "),
    
    "Integrated Nutrition" = glue(
      "<div class='alert alert-info'>
        <h4><i class='fa fa-lightbulb'></i> AI-Generated Policy Brief: Integrated Nutrition Strategy</h4>
        
        <h5>Executive Summary</h5>
        <p>A holistic, multi-sectoral nutrition improvement strategy for {target} addressing stunting, 
        wasting, and anemia simultaneously over {timeline} months through coordinated interventions.</p>
        
        <h5>Priority Interventions (Budget: ${format(budget, big.mark = ',')})</h5>
        <ol>
          <li><strong>Health System Strengthening</strong> (30% of budget)
            <ul>
              <li>Nutrition training for health workers</li>
              <li>Growth monitoring systems</li>
              <li>Maternal and child health services</li>
            </ul>
          </li>
          <li><strong>Food Security & Agriculture</strong> (35% of budget)
            <ul>
              <li>Nutrition-sensitive agriculture</li>
              <li>Food fortification programs</li>
              <li>School feeding programs</li>
            </ul>
          </li>
          <li><strong>WASH & Social Protection</strong> (20% of budget)
            <ul>
              <li>Water and sanitation improvements</li>
              <li>Conditional cash transfers</li>
              <li>Community engagement programs</li>
            </ul>
          </li>
          <li><strong>Monitoring & Evaluation</strong> (15% of budget)
            <ul>
              <li>Data collection systems</li>
              <li>Impact assessments</li>
              <li>Program adjustments based on evidence</li>
            </ul>
          </li>
        </ol>
        
        <h5>Expected Outcomes</h5>
        <ul>
          <li>10-15 percentage point reduction in stunting over {timeline} months</li>
          <li>Significant improvements across all nutrition indicators</li>
          <li>Enhanced food security and dietary diversity</li>
          <li>Strengthened institutional capacity for nutrition programming</li>
        </ul>
        
        <div class='alert alert-warning'>
          <strong>Critical Success Factors:</strong> Inter-ministerial coordination, sustained political will,
          adequate financing, community participation, and evidence-based adaptation.
        </div>
      </div>
    "),
    
    "Emergency Response" = glue(
      "<div class='alert alert-info'>
        <h4><i class='fa fa-lightbulb'></i> AI-Generated Policy Brief: Emergency Nutrition Response</h4>
        
        <h5>Executive Summary</h5>
        <p>An emergency nutrition response protocol for {target} to address acute malnutrition crises 
        with rapid deployment and immediate support within {timeline} months.</p>
        
        <h5>Priority Interventions (Budget: ${format(budget, big.mark = ',')})</h5>
        <ol>
          <li><strong>Rapid Assessment & Response</strong> (25% of budget)
            <ul>
              <li>Emergency nutrition assessments</li>
              <li>Situation analysis and mapping</li>
              <li>Rapid intervention deployment</li>
            </ul>
          </li>
          <li><strong>Emergency Food & Supplies</strong> (45% of budget)
            <ul>
              <li>Ready-to-use therapeutic foods (RUTF)</li>
              <li>Emergency food baskets</li>
              <li>Medical supplies and equipment</li>
            </ul>
          </li>
          <li><strong>Treatment & Support Services</strong> (20% of budget)
            <ul>
              <li>Stabilization centers</li>
              <li>Outpatient therapeutic programs</li>
              <li>Psychosocial support</li>
            </ul>
          </li>
          <li><strong>Coordination & Logistics</strong> (10% of budget)
            <ul>
              <li>Emergency coordination mechanisms</li>
              <li>Supply chain management</li>
              <li>Communication systems</li>
            </ul>
          </li>
        </ol>
        
        <h5>Expected Outcomes</h5>
        <ul>
          <li>Rapid stabilization of acute malnutrition cases</li>
          <li>Mortality reduction in affected populations</li>
          <li>Improved treatment success rates above 75%</li>
          <li>Prevention of chronic malnutrition transition</li>
        </ul>
        
        <div class='alert alert-warning'>
          <strong>Critical Success Factors:</strong> Rapid decision-making, pre-positioned supplies,
          trained emergency teams, effective coordination, and donor support.
        </div>
      </div>
    ")
  )
  
  return(base_recommendations[[focus_area]] %||% base_recommendations[["Integrated Nutrition"]])
}

# ==============================================================================
# 🔹 RUN THE ENHANCED APPLICATION
# ==============================================================================
# Run the application
shinyApp(ui, server)
