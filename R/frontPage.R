#' Module UI for the home page.
#' 
#' @param id ID of the module
#' @return Div for the home page
frontPageUI <- function(id) {
  div(
    h2(
      "MetaPairwise V1.1.0",
      tags$sup("Beta", style = "color:#6CC0ED"), 
      align = "left"
    ),
    fluidRow(
      column(
        width = 4,
        style = "display: flex;",
        img(
          src = "images/MetaPairwiseLogo.png",
          style = "vertical-align: middle; width: -webkit-fill-available; max-height: 300px; max-width: 300px; margin: auto;"
        )
      ),
      column(
        width = 8,
        h4("About"),
        p("This app allows researchers to conduct pairwise meta-analysis for binary and continuous outcome data without the need for programming knowledge."),
        p("The app contains two main sections:"),
        p(strong("Data"), " - Upload your data or use an example dataset."),
        p(
          strong("Calculator"), " - Meta-analyse the dataset in a frequentist or bayesian framework, generate statistical results and display downloadable figures."
        ),
        hr(),
        p(tags$strong("Latest Updates:")),
        p(tags$strong("Minor update (2nd September 2024 v1.1.0-beta):")),
        tags$ul(
          tags$li("Added reproducible script downloads."),
          tags$li("Added JSON download of the results.")
        ),
        p(tags$strong("Major update (11th March 2024 v1.0.0-beta):")),
        p(
          tags$ul(
            tags$li(
              "Initial release."
            )
          )
        )
      ),
      p(
        "The code for MetaPairwise is open-source and available on the ",
        tags$a(href = "https://github.com/CRSU-Apps/MetaPairwise", "CRSU GitHub Page.", target="_blank")
      ),
      h4("Authors"),
      p("Will Robinson, Clareece Nevill, Janion Nevill, Nicola Cooper, Alex Sutton"),
      p("For feedback/questions about this app please email the CRSU team at apps@crsu.org.uk."),
      p(
        "App powered by R and Shiny. All frequentist statistical calculations are performed using R package metafor (Wolfgang Viechtbauer 2010).",
        tags$a("metafor: Meta-Analysis Package for R. R package version 4.4-0.", href = "https://cran.r-project.org/web/packages/metafor/index.html", target = "_blank"),
        "All Bayesian statistical calculations are performed using R package RStan (Stan Development Team 2016)",
        tags$a("rstan: R Interface to Stan. R package version 2.32.3.", href = "https://cran.r-project.org/web/packages/rstan/index.html", target = "_blank"),
        "and R package MetaStan (Burak Kuersad Guenhan, Christian Roever 2022)",
        tags$a("MetaStan: Bayesian Meta-Analysis via 'Stan'. R package version 1.0.0.", href = "https://cran.r-project.org/web/packages/MetaStan/index.html", target = "_blank")
      ),
      br(),
      p(
        "THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT",
        "NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.",
        "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,",
        "WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE",
        "OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
      ),
      wellPanel(
        div(
          style = "display: inline;",
          img(src = 'funded-by-nihr-logo.png', width = "55%")
        ),
        div(
          style = "display: inline;",
          img(src = 'CRSU_logo.png', width = "40%")
        ),
        div(
          tags$strong("Funding and Support Acknowledgement:"),
          tags$p(
            "MetaPairwise is part of the Complex Reviews Synthesis Unit (CRSU) suite of evidence synthesis apps.",
            "The development of these apps is currently funded (majority) and overseen by the Evidence Synthesis Group @ CRSU (NIHR153934).",
            "Further details of other funders and support, current and past, can be found ",
            tags$a("on our GitHub page", href = "https://github.com/CRSU-Apps/.github/wiki/Detailed-Funding-Statement", target = "_blank"),
            ". The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."
          ),
          tags$p(
            "More information about the UK NIHR Complex Reviews Synthesis Unit (CRSU) can be found ",
            tags$a("on our website.", href = "https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/", target = "_blank"),
          )
        )
      )
    )
  )
}

#' Module server for the home page.
#' 
#' @param id ID of the module
frontPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Do nothing
  })
}
