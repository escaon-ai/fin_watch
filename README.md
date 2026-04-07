# fin_watch — Rapport financier hebdomadaire automatisé

Analyse automatique des marchés chaque lundi matin : collecte de données, insights IA, et envoi d'un rapport PDF par email.

📄 **[Voir un exemple de rapport PDF](./examples/financial_report_week14.pdf)**

---

## Ce que fait le projet

1. Récupère les données de clôture de la semaine précédente (lundi → vendredi)
2. Calcule les variations hebdomadaires en % et génère des sparklines
3. Lance une analyse IA via **Google Gemini** (ellmer), qui interroge une API news en temps réel avant de rédiger
4. Génère un rapport PDF (Quarto + Typst) avec tableau gt et analyse narrative
5. Envoie le rapport par email chaque lundi à 9h00 (heure de Paris)

---

## Indices suivis

| Indice | Source | Stratégie |
|--------|--------|-----------|
| **DCAM** — Amundi PEA MSCI World | Yahoo Finance | Long terme, diversifié |
| **PCEU** — Amundi PEA MSCI Europe | Yahoo Finance | Long terme, diversifié |
| **BTC** — Bitcoin en EUR | Yahoo Finance | Buy the Dip, surveillance volatilité |
| **€STER** — Euro Short-Term Rate | BCE (ECB API) | Trésorerie en attente d'investissement |

---

## Architecture

```
main.R              # Orchestration : fetch → wrangle → AI → email
fun.R               # Fonctions : fetch_fin_data, wrangle_fin_data,
                    #             fetch_market_news (tool IA), perform_ai_analysis,
                    #             send_email_report
email_report.qmd    # Template Quarto/Typst pour le PDF
financial_report.pdf # Exemple de rapport généré
renv.lock           # Environnement R reproductible
.github/workflows/
  monday_report.yml # GitHub Actions — déclenchement lundi 9h Paris
```

---

## Analyse IA

L'analyse utilise **Gemini** via le package R `ellmer`. Avant de rédiger, le modèle dispose d'un **tool** (`fetch_market_news`) qui interroge [NewsAPI](https://newsapi.org) pour récupérer les actualités financières récentes. Gemini effectue plusieurs appels ciblés (BCE, marchés actions, crypto, macro) puis produit une analyse en français de 3 à 5 paragraphes.

---

## Secrets GitHub requis

| Secret | Description |
|--------|-------------|
| `GEMINI_API_KEY` | Clé API Google AI Studio |
| `NEWS_API_KEY` | Clé NewsAPI.org (plan gratuit suffisant) |
| `EMAIL_USER` | Adresse Gmail expéditrice |
| `EMAIL_PASSWORD` | Mot de passe d'application Gmail |

---

## Lancer localement

```r
# Définir les variables d'environnement dans .Renviron, puis :
renv::restore()
source("main.R")
```
