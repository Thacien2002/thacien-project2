# Rwanda Nutrition Dashboard 🇷🇼📊

## Prerequisites
- R (version 4.1.0 or higher)
- RStudio (recommended)
- Internet connection for package installation

## Installation Steps

### 1. Install Required Packages
Run the package installation script:
```R
source("install_packages.R")
```

### 2. Set Up Gemini API (Optional)
To use AI features, set your Gemini API key:
```R
Sys.setenv(GEMINI_API_KEY = "AIzaSyCfwS4A6pgQzzMh-SJsHhQ3O5SrwC2AlU8")
```

### 3. Launch Dashboard
```R
library(shiny)
runApp('app.r')
## or run the file colled run_dashboard.R directly
```

## Troubleshooting
- Ensure all dependencies are installed
- Check R and package versions
- Verify API key if using AI features

## Features
- 🌐 Real-time nutrition data visualization at national and district levels

- 🤖 AI-powered policy recommendations based on nutrition indicators

- 📊 Interactive insights for regional and district-level comparisons
- solution hub and data quality information

- 🎨 Professional and accessible design, optimized for readability and clarity

- 📂 Dataset integration for Rwanda (2000–2024) nutrition indicators
## Data source
- Rwanda Demographic and Health Surveys (DHS)
- world bank data
- Ministry of Health Nutrition Reports

- FAO and WHO nutrition datasets
## Contributing
1. Fork the repository
2. Create a feature branch
3. Commit changes
4. Push to branch
5. Create pull request

## License
MIT License

## Contact
Dashboard Team - haragirimanathacien@gmail.com 

