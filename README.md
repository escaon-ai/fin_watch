# R-Based AI Financial Analyst Agent

This project implements an automated financial analysis agent that runs weekly to fetch financial data, analyze market trends using AI, and email a summary report.

## Features

- **Automated Data Fetching**: Uses existing `fetch_data.R` script to retrieve financial data
- **Smart Analysis**: Calculates weekly variations with different windows for different asset types:
  - 5-day window (Mon-Fri) for stock indices/ETFs (DCAM, PCEU) and ESTER
  - 7-day window (Mon-Sun) for Crypto (BTC)
- **AI-Powered Insights**: Uses Google Gemini to analyze market movements and provide contextual explanations
- **Automated Reporting**: Emails a formatted report every Monday morning
- **GitHub Actions Integration**: Fully automated workflow that runs on schedule

## Files

- `fetch_data.R`: Existing script that fetches financial data (DO NOT MODIFY)
- `monday_agent.R`: Main orchestration script that performs analysis and sends reports
- `install_packages.R`: Script to install required R packages
- `.github/workflows/monday_report.yml`: GitHub Actions workflow definition
- `setup_instructions.md`: Additional setup instructions

## Setup Instructions

1. **GitHub Secrets Configuration**:
   - `GEMINI_API_KEY`: Your Google Gemini API key
   - `EMAIL_USER`: Your Gmail address for sending reports
   - `EMAIL_PASSWORD`: Your Gmail app password (not your regular password)

2. **Package Installation**:
   Run `Rscript install_packages.R` to install required packages

3. **renv Setup**:
   The workflow uses renv to manage package versions. Make sure your `renv.lock` file includes all necessary packages.

## How It Works

1. Every Monday at 07:00 UTC (08:00 Paris Time), the GitHub Action triggers
2. The workflow sets up R environment and installs dependencies
3. `monday_agent.R` sources `fetch_data.R` to get the latest financial data
4. Calculates percentage variations based on your investment strategy rules
5. Sends data to Google Gemini for AI analysis with contextual prompts
6. Formats and emails a comprehensive report

## Investment Strategy Context

The AI analysis is tailored to your specific investment approach:
- **DCAM & PCEU**: Long-term holds, focusing on weekly dynamics
- **BTC**: "Buy the dip" monitoring, focusing on daily lows and volatility reasons
- **€STER**: Cash parking, focusing on rate stability/risk of drop

## Customization

You can modify the analysis prompts in `monday_agent.R` to better suit your needs or add additional assets to track.