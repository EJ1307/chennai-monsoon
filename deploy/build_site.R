# Builds the static website that Vercel serves: the Shiny app exported with Shinylive,
# so R runs inside the visitor's browser (WebAssembly) and no R server is needed.
#
# Run from the project folder, then commit site/ (Vercel publishes it as it is):
#   Rscript prepare/precompute.R    # only if the data or the analysis changed
#   Rscript deploy/build_site.R

unlink("site", recursive = TRUE)
shinylive::export("app", "site", quiet = TRUE)

# The editor redirect page is not needed on the public site
unlink("site/edit", recursive = TRUE)

# Page title, icon and a short note while R starts (the first visit takes a while)
file.copy("app/www/favicon.svg", "site/favicon.svg", overwrite = TRUE)
index <- readLines("site/index.html", warn = FALSE)
head_extra <- c(
  '    <title>Chennai monsoon spells</title>',
  '    <meta name="description" content="Wet and dry spells of Chennai\'s northeast monsoon, 1991 to 2023: a Markov chain analysis with extreme-rainfall risk, built in R Shiny." />',
  '    <link rel="icon" type="image/svg+xml" href="./favicon.svg" />',
  '    <style>',
  '      html, body { background: #f6f5f1; }',
  '      #starting { position: fixed; left: 0; right: 0; top: calc(50% + 64px); z-index: 10;',
  '        text-align: center; font: 400 15px/1.5 system-ui, -apple-system, "Segoe UI", sans-serif;',
  '        color: #52514e; padding: 0 24px; transition: opacity .3s ease; }',
  '      #starting b { display: block; color: #0b0b0b; font-weight: 500; font-size: 17px; margin-bottom: 2px; }',
  '      #starting.done { opacity: 0; pointer-events: none; }',
  '    </style>'
)
index <- sub("^\\s*<title>.*</title>\\s*$", paste(head_extra, collapse = "\n"), index)
note <- c(
  '    <div id="starting"><b>Chennai monsoon spells</b>Starting R in your browser. The first visit takes about 20 seconds; after that it opens faster.</div>',
  '    <script>',
  '      // Hide the note once the Shiny app inside the frame is connected',
  '      const startedCheck = setInterval(() => {',
  '        const frame = document.querySelector("#root iframe");',
  '        let ready = false;',
  '        try { ready = !!(frame && frame.contentWindow.Shiny && frame.contentWindow.Shiny.shinyapp',
  '          && frame.contentWindow.Shiny.shinyapp.isConnected()); } catch (e) {}',
  '        if (ready) { document.getElementById("starting").classList.add("done"); clearInterval(startedCheck); }',
  '      }, 400);',
  '    </script>'
)
root_line <- grep('id="root"', index)
index <- append(index, note, after = root_line)
writeLines(index, "site/index.html")

writeLines(c("User-agent: *", "Allow: /"), "site/robots.txt")

size <- sum(file.size(list.files("site", recursive = TRUE, full.names = TRUE)))
cat(sprintf("site/ ready: %d files, %.1f MB\n",
            length(list.files("site", recursive = TRUE)), size / 1e6))
