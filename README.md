# F1 Betting Pool 2026

An R Shiny application to run a parallel Formula 1 predictions championship during the 2026 season.

## Features
- **Dynamic Calendar:** Queries the official OpenF1 API to fetch the 2026 Grand Prix schedule.
- **Exact and Partial Predictions:** Automated scoring system based on real grid and finishing positions.
- **Extras:** Fastest lap and Mazepin Award (first retirement).
- **Global Ranking:** View player scores in real-time for each session alongside the global championship leaderboards.

## Installation & Configuration
The project uses a local SQLite database for user authentication and Google Sheets as the main backend to store predictions. It is **critical** not to push real credentials to GitHub.

1. Clone the repository:
   ```bash
   git clone git@github.com:MarcosMarinM/porra-F1.git
   ```
2. Install the required R dependencies:
   ```R
   install.packages(c(
     "shiny", "shinymanager", "googlesheets4", "dplyr", "httr2",
     "jsonlite", "cachem", "bslib", "ggplot2", "tidyr", "plotly",
     "DBI", "RSQLite"
   ))
   ```
3. Create the users database (only the first time):
   ```R
   shinymanager::create_db(
     data.frame(
       user = "admin", password = "admin", admin = TRUE,
       stringsAsFactors = FALSE
     ),
     sqlite_path = "usuarios.sqlite"
   )
   ```
   Change the password before deploying.

4. Create your local `.Renviron` file in the project directory using `.Renviron.example` as a template:
   ```env
   F1_SHEET_ID="your-google-sheet-id-here"
   F1_DB_PATH="usuarios.sqlite"
   F1_JSON_PATH="f1-service-account.json"
   F1_YEAR="2026"
   F1_REMEMBER_DAYS="90"
   OPENF1_USERNAME=""
   OPENF1_PASSWORD=""
   ```
   *Note: Never commit your actual `.Renviron`, `.sqlite`, or `.json` files.*

   OpenF1 restricts global API access while a live session is in progress.
   Set `OPENF1_USERNAME` and `OPENF1_PASSWORD` (the app requests and caches the
   access token automatically) or set `OPENF1_TOKEN` directly. Without
   credentials the app still works outside live sessions.

5. Run the application:
   ```R
   shiny::runApp('app.r')
   ```

## Session persistence

"Remember me" tokens are stored in the `remember_tokens` table of the SQLite
database (in addition to the browser cookie). This means the SQLite file must
persist across application restarts (avoid ephemeral filesystems). If the
hosting platform restarts the app or wipes the filesystem, users will need to
log in again.

## API Provider
Live session results and driver grids are powered by the excellent [OpenF1 API](https://openf1.org/docs).
